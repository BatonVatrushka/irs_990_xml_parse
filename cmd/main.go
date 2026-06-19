package main

import (
	"context"
	"fmt"
	"log"
	"log/slog"
	"os"
	"strconv"
	"sync/atomic"
	"time"

	"github.com/jackc/pgx/v5/pgxpool"
	"github.com/robfig/cron/v3"

	"github.com/BatonVatrushka/irs_990_xml_parse/internal/bmf"
	"github.com/BatonVatrushka/irs_990_xml_parse/internal/db"
	"github.com/BatonVatrushka/irs_990_xml_parse/internal/fetcher"
	"github.com/BatonVatrushka/irs_990_xml_parse/internal/models"
	"github.com/BatonVatrushka/irs_990_xml_parse/internal/parser"
)

const (
	defaultStartYear = 2019 // IRS new host starts at 2019
	defaultWorkers   = 20
)

func main() {
	dbURL := mustEnv("DATABASE_URL")
	mode := getEnv("MODE", "backfill") // backfill | sync | daemon
	workers := getEnvInt("WORKERS", defaultWorkers)
	startYear := getEnvInt("START_YEAR", defaultStartYear)
	endYear := getEnvInt("END_YEAR", time.Now().Year()-1)

	ctx := context.Background()

	pool, err := db.Connect(ctx, dbURL)
	if err != nil {
		log.Fatalf("db connect: %v", err)
	}
	defer pool.Close()

	if err := runMigrations(ctx, pool); err != nil {
		log.Fatalf("migrations: %v", err)
	}

	slog.Info("loading IRS BMF — this may take a minute")
	bl := bmf.New()
	orgs, err := bl.Load(ctx)
	if err != nil {
		log.Fatalf("bmf load: %v", err)
	}
	slog.Info("BMF ready", "housing_homeless_orgs", len(orgs))

	// Seed the organizations table with all BMF-matched orgs upfront.
	// This ensures every org is represented even if we never find their XML.
	slog.Info("upserting organizations from BMF")
	if err := db.BulkUpsertOrgs(ctx, pool, orgs); err != nil {
		log.Fatalf("bulk upsert orgs: %v", err)
	}
	slog.Info("organizations seeded", "count", len(orgs))

	f := fetcher.New()

	switch mode {
	case "backfill":
		slog.Info("starting backfill", "start_year", startYear, "end_year", endYear)
		for year := startYear; year <= endYear; year++ {
			if err := processYear(ctx, pool, f, orgs, year, workers); err != nil {
				slog.Error("year failed", "year", year, "err", err)
			}
		}
		slog.Info("backfill complete")

	case "sync":
		year := time.Now().Year()
		slog.Info("syncing year", "year", year)
		if err := processYear(ctx, pool, f, orgs, year, workers); err != nil {
			log.Fatalf("sync failed: %v", err)
		}

	case "daemon":
		go func() {
			if err := processYear(ctx, pool, f, orgs, time.Now().Year(), workers); err != nil {
				slog.Error("initial sync failed", "err", err)
			}
		}()

		c := cron.New()
		c.AddFunc("@weekly", func() {
			fresh, err := bl.Load(ctx)
			if err != nil {
				slog.Error("bmf reload failed", "err", err)
				fresh = orgs
			}
			// Re-seed orgs from freshly-downloaded BMF.
			if err := db.BulkUpsertOrgs(ctx, pool, fresh); err != nil {
				slog.Error("bulk upsert orgs failed", "err", err)
			}
			year := time.Now().Year()
			slog.Info("scheduled sync", "year", year)
			if err := processYear(ctx, pool, f, fresh, year, workers); err != nil {
				slog.Error("scheduled sync failed", "err", err)
			}
		})
		c.Start()
		defer c.Stop()
		select {}

	default:
		log.Fatalf("unknown MODE %q — valid values: backfill, sync, daemon", mode)
	}
}

// processYear fetches the CSV index for one year, identifies filings from
// BMF-matched orgs, then streams through ZIP archives processing each match.
func processYear(
	ctx context.Context,
	pool *pgxpool.Pool,
	f *fetcher.Fetcher,
	orgs map[string]bmf.OrgInfo,
	year, workers int,
) error {
	slog.Info("fetching index", "year", year)
	entries, err := f.FetchIndex(ctx, year)
	if err != nil {
		return fmt.Errorf("fetch index %d: %w", year, err)
	}
	slog.Info("index ready", "year", year, "total_filings", len(entries))

	// Build a map of ObjectID → IndexEntry for filings we care about.
	targetIDs := make(map[string]struct{})
	entryMap := make(map[string]models.IndexEntry)
	for _, e := range entries {
		if e.FormType != "990" && e.FormType != "990EZ" {
			continue
		}
		if _, ok := orgs[e.EIN]; !ok {
			continue
		}
		targetIDs[e.ObjectID] = struct{}{}
		entryMap[e.ObjectID] = e
	}
	slog.Info("targets identified", "year", year, "target_count", len(targetIDs))

	// Bulk-skip filings that already have has_full_data = TRUE.
	done, err := db.GetProcessedIDs(ctx, pool, targetIDs)
	if err != nil {
		return fmt.Errorf("get processed ids: %w", err)
	}
	for id := range done {
		delete(targetIDs, id)
	}
	slog.Info("after skip check",
		"year", year,
		"already_done", len(done),
		"to_process", len(targetIDs),
	)

	if len(targetIDs) == 0 {
		slog.Info("nothing new to process", "year", year)
		return nil
	}

	var processed, withJ atomic.Int64

	// Stream through each ZIP candidate for this year.
	// ProcessZIP silently skips URLs that 404.
	for _, zipURL := range f.ZIPURLsForYear(year) {
		err := f.ProcessZIP(ctx, zipURL, targetIDs, func(objectID string, data []byte) error {
			entry := entryMap[objectID]
			org := orgs[entry.EIN]
			return processXML(ctx, pool, entry, org, year, zipURL, data, &processed, &withJ)
		})
		if err != nil {
			slog.Error("zip processing failed", "url", zipURL, "err", err)
			// Continue to next ZIP rather than aborting the whole year.
		}
	}

	slog.Info("year complete",
		"year", year,
		"processed", processed.Load(),
		"skipped_already_done", len(done),
		"with_schedule_j", withJ.Load(),
	)
	return nil
}

func processXML(
	ctx context.Context,
	pool *pgxpool.Pool,
	entry models.IndexEntry,
	org bmf.OrgInfo,
	filingYear int,
	zipURL string,
	data []byte,
	processed, withJ *atomic.Int64,
) error {
	result, err := parser.Parse(data, entry.ObjectID, entry.CompanyName, org.City, org.State, org.NTEECode)
	if err != nil {
		slog.Warn("parse failed", "object_id", entry.ObjectID, "err", err)
		_ = db.MarkProcessed(ctx, pool, entry, zipURL, filingYear,
			org.City, org.State, org.NTEECode, false, false, err.Error())
		return nil
	}

	hasJ := len(result.ScheduleJRecords) > 0

	// 1. Ensure the filing_index row exists FIRST — child tables (revenue, etc.)
	//    have a FK reference to it, so this must precede any detail inserts.
	//    has_full_data stays FALSE until all inserts succeed.
	if err := db.MarkProcessed(ctx, pool, entry, zipURL, filingYear,
		org.City, org.State, org.NTEECode, hasJ, false, ""); err != nil {
		return err
	}

	// 2. Clean up stale rows from any previous partial run.
	if err := db.DeleteFilingData(ctx, pool, entry.ObjectID); err != nil {
		return fmt.Errorf("delete filing data %s: %w", entry.ObjectID, err)
	}

	// 3. Enrich the organizations table with XML-sourced fields.
	if err := db.UpsertOrg(ctx, pool, &models.OrgRecord{
		EIN:               org.EIN,
		Name:              org.Name,
		City:              org.City,
		State:             org.State,
		NTEECode:          org.NTEECode,
		IRSSubsectionCode: org.IRSSubsectionCode,
		RulingDate:        org.RulingDate,
		TaxPeriod:         entry.TaxPeriod,
		WebsiteURL:        result.WebsiteURL,
		GrossReceipts:     result.GrossReceipts,
	}); err != nil {
		slog.Warn("upsert org failed", "object_id", entry.ObjectID, "err", err)
		// Non-fatal — continue with other inserts.
	}

	// 4. Insert all financial detail tables.
	if insertErr := insertFilingData(ctx, pool, result, hasJ); insertErr != nil {
		slog.Warn("insert filing data failed", "object_id", entry.ObjectID, "err", insertErr)
		_ = db.SetHasFullData(ctx, pool, entry.ObjectID, false, insertErr.Error())
		return nil
	}

	// 5. All inserts succeeded — flip the flag so this filing is skipped next run.
	if err := db.SetHasFullData(ctx, pool, entry.ObjectID, true, ""); err != nil {
		return err
	}

	if hasJ {
		withJ.Add(1)
	}

	n := processed.Add(1)
	if n%100 == 0 {
		slog.Info("progress", "year", filingYear, "processed", n, "with_schedule_j", withJ.Load())
	}
	return nil
}

// insertFilingData inserts all financial detail records for a single filing.
// Returns the first error encountered (all inserts use CopyFrom / single Exec).
func insertFilingData(ctx context.Context, pool *pgxpool.Pool, result *parser.Result, hasJ bool) error {
	if hasJ {
		if err := db.InsertCompensation(ctx, pool, result.ScheduleJRecords); err != nil {
			return err
		}
	}
	if err := db.InsertRevenue(ctx, pool, result.Revenue); err != nil {
		return err
	}
	if err := db.InsertExpenses(ctx, pool, result.Expenses); err != nil {
		return err
	}
	if err := db.InsertBalanceSheet(ctx, pool, result.BalanceSheet); err != nil {
		return err
	}
	if err := db.InsertEmployeeComp(ctx, pool, result.Employees); err != nil {
		return err
	}
	if err := db.InsertContractors(ctx, pool, result.Contractors); err != nil {
		return err
	}
	if err := db.InsertGrantsPaid(ctx, pool, result.Grants); err != nil {
		return err
	}
	return nil
}

func runMigrations(ctx context.Context, pool *pgxpool.Pool) error {
	migrations := []string{
		"migrations/001_create_tables.sql",
		"migrations/002_add_geo_ntee.sql",
		"migrations/003_full_schema.sql",
	}
	for _, path := range migrations {
		sql, err := os.ReadFile(path)
		if err != nil {
			return fmt.Errorf("read %s: %w", path, err)
		}
		if _, err := pool.Exec(ctx, string(sql)); err != nil {
			return fmt.Errorf("exec %s: %w", path, err)
		}
	}
	return nil
}

func mustEnv(key string) string {
	v := os.Getenv(key)
	if v == "" {
		log.Fatalf("required env var %s is not set", key)
	}
	return v
}

func getEnv(key, fallback string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return fallback
}

func getEnvInt(key string, fallback int) int {
	if v := os.Getenv(key); v != "" {
		if n, err := strconv.Atoi(v); err == nil {
			return n
		}
	}
	return fallback
}
