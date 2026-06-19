// Integration tests for the db package.
//
// These tests require a real PostgreSQL instance.
// Set TEST_DATABASE_URL to run them; they are skipped otherwise.
//
// Example (matches docker-compose defaults):
//
//	TEST_DATABASE_URL=postgres://irs990:irs990@localhost:5433/irs990 go test ./internal/db/...
package db_test

import (
	"context"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/jackc/pgx/v5/pgxpool"

	"github.com/BatonVatrushka/irs_990_xml_parse/internal/bmf"
	"github.com/BatonVatrushka/irs_990_xml_parse/internal/db"
	"github.com/BatonVatrushka/irs_990_xml_parse/internal/models"
)

// repoRoot is resolved once at init time relative to this file's location.
var repoRoot string

func init() {
	_, filename, _, _ := runtime.Caller(0)
	// filename: .../internal/db/db_test.go  →  repo root is ../../
	repoRoot = filepath.Join(filepath.Dir(filename), "..", "..")
}

// testPool connects to the test database and applies all migrations.
// The test is skipped when TEST_DATABASE_URL is not set.
func testPool(t *testing.T) *pgxpool.Pool {
	t.Helper()
	dsn := os.Getenv("TEST_DATABASE_URL")
	if dsn == "" {
		t.Skip("set TEST_DATABASE_URL to run integration tests")
	}
	ctx := context.Background()
	pool, err := db.Connect(ctx, dsn)
	if err != nil {
		t.Fatalf("db connect: %v", err)
	}
	t.Cleanup(pool.Close) // registered first → runs last (LIFO)
	applyMigrations(t, ctx, pool)
	return pool
}

func applyMigrations(t *testing.T, ctx context.Context, pool *pgxpool.Pool) {
	t.Helper()
	for _, name := range []string{
		"migrations/001_create_tables.sql",
		"migrations/002_add_geo_ntee.sql",
		"migrations/003_full_schema.sql",
	} {
		sql, err := os.ReadFile(filepath.Join(repoRoot, name))
		if err != nil {
			t.Fatalf("read migration %s: %v", name, err)
		}
		if _, err := pool.Exec(ctx, string(sql)); err != nil {
			t.Fatalf("exec migration %s: %v", name, err)
		}
	}
}

// cleanupFiling deletes all test data for object_ids with the DBTEST_ prefix.
// Called both at the start (stale data guard) and via t.Cleanup.
func cleanupFiling(ctx context.Context, pool *pgxpool.Pool) {
	for _, tbl := range []string{
		"schedule_j_compensation",
		"employee_compensation",
		"contractors",
		"revenue",
		"expenses",
		"balance_sheet",
		"grants_paid",
		"filing_index",
	} {
		pool.Exec(ctx, "DELETE FROM "+tbl+" WHERE object_id LIKE 'DBTEST_%'") //nolint:errcheck
	}
	pool.Exec(ctx, "DELETE FROM organizations WHERE ein LIKE 'DBTEST_%'") //nolint:errcheck
}

func insertTestFiling(t *testing.T, ctx context.Context, pool *pgxpool.Pool, objectID, ein string) {
	t.Helper()
	entry := models.IndexEntry{
		ObjectID:    objectID,
		EIN:         ein,
		TaxPeriod:   "202112",
		FormType:    "990",
		CompanyName: "DB TEST ORG",
	}
	err := db.MarkProcessed(ctx, pool, entry, "https://example.com/test.zip", 2021,
		"Denver", "CO", "L41", false, false, "")
	if err != nil {
		t.Fatalf("insertTestFiling: %v", err)
	}
}

// ─── GetProcessedIDs ─────────────────────────────────────────────────────────

func TestGetProcessedIDs(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()
	cleanupFiling(ctx, pool)
	t.Cleanup(func() { cleanupFiling(ctx, pool) })

	// Insert one filing with has_full_data=true and one with false.
	doneID := "DBTEST_DONE_001"
	pendingID := "DBTEST_PENDING_001"
	insertTestFiling(t, ctx, pool, doneID, "DBTEST_EIN_001")
	insertTestFiling(t, ctx, pool, pendingID, "DBTEST_EIN_002")
	if err := db.SetHasFullData(ctx, pool, doneID, true, ""); err != nil {
		t.Fatalf("SetHasFullData: %v", err)
	}

	target := map[string]struct{}{
		doneID:    {},
		pendingID: {},
		"DBTEST_UNKNOWN": {}, // not in DB at all — should be absent from result
	}

	done, err := db.GetProcessedIDs(ctx, pool, target)
	if err != nil {
		t.Fatalf("GetProcessedIDs: %v", err)
	}

	if _, ok := done[doneID]; !ok {
		t.Errorf("GetProcessedIDs: %q (has_full_data=true) must be in result", doneID)
	}
	if _, ok := done[pendingID]; ok {
		t.Errorf("GetProcessedIDs: %q (has_full_data=false) must NOT be in result", pendingID)
	}
	if _, ok := done["DBTEST_UNKNOWN"]; ok {
		t.Error("GetProcessedIDs: unknown ID must not appear in result")
	}
}

func TestGetProcessedIDs_EmptyInput(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()

	done, err := db.GetProcessedIDs(ctx, pool, map[string]struct{}{})
	if err != nil {
		t.Fatalf("GetProcessedIDs(empty): %v", err)
	}
	if len(done) != 0 {
		t.Errorf("expected empty result for empty input, got %d entries", len(done))
	}
}

// ─── MarkProcessed / SetHasFullData ──────────────────────────────────────────

func TestMarkProcessed_InsertsAndUpserts(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()
	cleanupFiling(ctx, pool)
	t.Cleanup(func() { cleanupFiling(ctx, pool) })

	entry := models.IndexEntry{
		ObjectID:    "DBTEST_MARK_001",
		EIN:         "DBTEST_EIN_010",
		TaxPeriod:   "202112",
		FormType:    "990",
		CompanyName: "MARK TEST ORG",
	}

	// Initial insert.
	if err := db.MarkProcessed(ctx, pool, entry, "https://example.com/a.zip", 2021,
		"Denver", "CO", "L41", false, false, ""); err != nil {
		t.Fatalf("MarkProcessed (insert): %v", err)
	}

	// Upsert with schedule_j flag flipped — should not error.
	if err := db.MarkProcessed(ctx, pool, entry, "https://example.com/a.zip", 2021,
		"Denver", "CO", "L41", true, false, ""); err != nil {
		t.Fatalf("MarkProcessed (upsert): %v", err)
	}

	// Verify: GetProcessedIDs returns false (has_full_data still false).
	done, err := db.GetProcessedIDs(ctx, pool, map[string]struct{}{"DBTEST_MARK_001": {}})
	if err != nil {
		t.Fatalf("GetProcessedIDs: %v", err)
	}
	if _, ok := done["DBTEST_MARK_001"]; ok {
		t.Error("has_full_data should still be false after MarkProcessed without hasFullData=true")
	}
}

func TestSetHasFullData(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()
	cleanupFiling(ctx, pool)
	t.Cleanup(func() { cleanupFiling(ctx, pool) })

	objectID := "DBTEST_SETFULL_001"
	insertTestFiling(t, ctx, pool, objectID, "DBTEST_EIN_020")

	// Flip to true.
	if err := db.SetHasFullData(ctx, pool, objectID, true, ""); err != nil {
		t.Fatalf("SetHasFullData(true): %v", err)
	}

	done, err := db.GetProcessedIDs(ctx, pool, map[string]struct{}{objectID: {}})
	if err != nil {
		t.Fatalf("GetProcessedIDs: %v", err)
	}
	if _, ok := done[objectID]; !ok {
		t.Error("expected object_id to appear in done set after SetHasFullData(true)")
	}

	// Flip back to false.
	if err := db.SetHasFullData(ctx, pool, objectID, false, "some error"); err != nil {
		t.Fatalf("SetHasFullData(false): %v", err)
	}
	done, _ = db.GetProcessedIDs(ctx, pool, map[string]struct{}{objectID: {}})
	if _, ok := done[objectID]; ok {
		t.Error("object_id must not appear in done set after SetHasFullData(false)")
	}
}

// ─── DeleteFilingData ─────────────────────────────────────────────────────────

func TestDeleteFilingData(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()
	cleanupFiling(ctx, pool)
	t.Cleanup(func() { cleanupFiling(ctx, pool) })

	objectID := "DBTEST_DEL_001"
	insertTestFiling(t, ctx, pool, objectID, "DBTEST_EIN_030")

	// Insert one revenue row to represent "partial data".
	rev := &models.RevenueRecord{
		ObjectID:        objectID,
		EIN:             "DBTEST_EIN_030",
		TaxYear:         2021,
		TotalRevenueAmt: 1_000_000,
	}
	if err := db.InsertRevenue(ctx, pool, rev); err != nil {
		t.Fatalf("InsertRevenue: %v", err)
	}

	// Verify revenue row exists.
	var count int
	pool.QueryRow(ctx, `SELECT COUNT(*) FROM revenue WHERE object_id = $1`, objectID).Scan(&count)
	if count != 1 {
		t.Fatalf("expected 1 revenue row before delete, got %d", count)
	}

	// Delete filing data.
	if err := db.DeleteFilingData(ctx, pool, objectID); err != nil {
		t.Fatalf("DeleteFilingData: %v", err)
	}

	// Revenue row must be gone.
	pool.QueryRow(ctx, `SELECT COUNT(*) FROM revenue WHERE object_id = $1`, objectID).Scan(&count)
	if count != 0 {
		t.Errorf("expected 0 revenue rows after DeleteFilingData, got %d", count)
	}

	// filing_index row must still exist (DeleteFilingData only removes child rows).
	pool.QueryRow(ctx, `SELECT COUNT(*) FROM filing_index WHERE object_id = $1`, objectID).Scan(&count)
	if count != 1 {
		t.Errorf("filing_index row must survive DeleteFilingData, got count=%d", count)
	}
}

// ─── BulkUpsertOrgs ───────────────────────────────────────────────────────────

func TestBulkUpsertOrgs(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()
	cleanupFiling(ctx, pool)
	t.Cleanup(func() { cleanupFiling(ctx, pool) })

	orgs := map[string]bmf.OrgInfo{
		"DBTEST_EIN_100": {
			EIN:               "DBTEST_EIN_100",
			Name:              "TEST HOUSING ORG",
			City:              "Denver",
			State:             "CO",
			NTEECode:          "L41",
			IRSSubsectionCode: "03",
			RulingDate:        "202001",
		},
		"DBTEST_EIN_101": {
			EIN:      "DBTEST_EIN_101",
			Name:     "TEST SHELTER",
			City:     "Seattle",
			State:    "WA",
			NTEECode: "P85",
		},
	}

	if err := db.BulkUpsertOrgs(ctx, pool, orgs); err != nil {
		t.Fatalf("BulkUpsertOrgs: %v", err)
	}

	var count int
	pool.QueryRow(ctx, `SELECT COUNT(*) FROM organizations WHERE ein LIKE 'DBTEST_%'`).Scan(&count)
	if count != 2 {
		t.Fatalf("expected 2 org rows, got %d", count)
	}

	// Upsert again with a name change — should update, not insert duplicate.
	orgs["DBTEST_EIN_100"] = bmf.OrgInfo{
		EIN:  "DBTEST_EIN_100",
		Name: "TEST HOUSING ORG RENAMED",
	}
	if err := db.BulkUpsertOrgs(ctx, pool, orgs); err != nil {
		t.Fatalf("BulkUpsertOrgs (upsert): %v", err)
	}

	pool.QueryRow(ctx, `SELECT COUNT(*) FROM organizations WHERE ein LIKE 'DBTEST_%'`).Scan(&count)
	if count != 2 {
		t.Errorf("upsert must not create duplicate rows: got %d, want 2", count)
	}

	var name string
	pool.QueryRow(ctx, `SELECT name FROM organizations WHERE ein = 'DBTEST_EIN_100'`).Scan(&name)
	if name != "TEST HOUSING ORG RENAMED" {
		t.Errorf("upsert must update name: got %q", name)
	}
}

// ─── InsertCompensation ───────────────────────────────────────────────────────

func TestInsertCompensation_DeduplicatesOnNameAndTitle(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()
	cleanupFiling(ctx, pool)
	t.Cleanup(func() { cleanupFiling(ctx, pool) })

	objectID := "DBTEST_COMP_001"
	insertTestFiling(t, ctx, pool, objectID, "DBTEST_EIN_200")

	records := []*models.CompensationRecord{
		{
			ObjectID:              objectID,
			EIN:                   "DBTEST_EIN_200",
			TaxYear:               2021,
			CompanyName:           "TEST ORG",
			PersonName:            "ALICE SMITH",
			Title:                 "CEO",
			ReportableCompFromOrg: 200_000,
			TotalCompFilingOrg:    210_000,
		},
		// Duplicate: same PersonName + Title — should be deduped in Go before CopyFrom.
		{
			ObjectID:              objectID,
			EIN:                   "DBTEST_EIN_200",
			TaxYear:               2021,
			CompanyName:           "TEST ORG",
			PersonName:            "ALICE SMITH",
			Title:                 "CEO",
			ReportableCompFromOrg: 199_000, // different value but same key
			TotalCompFilingOrg:    209_000,
		},
		// Different title — should be kept as a separate row.
		{
			ObjectID:              objectID,
			EIN:                   "DBTEST_EIN_200",
			TaxYear:               2021,
			CompanyName:           "TEST ORG",
			PersonName:            "ALICE SMITH",
			Title:                 "Board Chair", // same person, different title
			ReportableCompFromOrg: 0,
		},
	}

	if err := db.InsertCompensation(ctx, pool, records); err != nil {
		t.Fatalf("InsertCompensation: %v", err)
	}

	var count int
	pool.QueryRow(ctx,
		`SELECT COUNT(*) FROM schedule_j_compensation WHERE object_id = $1`, objectID,
	).Scan(&count)
	// CEO duplicate is deduped → 1 CEO row + 1 Board Chair row = 2 total.
	if count != 2 {
		t.Errorf("expected 2 rows after dedup, got %d", count)
	}
}

// ─── InsertEmployeeComp ───────────────────────────────────────────────────────

func TestInsertEmployeeComp_DeduplicatesOnNameAndTitle(t *testing.T) {
	pool := testPool(t)
	ctx := context.Background()
	cleanupFiling(ctx, pool)
	t.Cleanup(func() { cleanupFiling(ctx, pool) })

	objectID := "DBTEST_EMP_001"
	insertTestFiling(t, ctx, pool, objectID, "DBTEST_EIN_300")

	records := []*models.EmployeeCompRecord{
		{
			ObjectID:   objectID,
			EIN:        "DBTEST_EIN_300",
			TaxYear:    2021,
			PersonName: "BOB JONES",
			Title:      "CFO",
			IsOfficer:  true,
		},
		// Duplicate: same PersonName + Title.
		{
			ObjectID:   objectID,
			EIN:        "DBTEST_EIN_300",
			TaxYear:    2021,
			PersonName: "BOB JONES",
			Title:      "CFO",
			IsOfficer:  true,
		},
	}

	if err := db.InsertEmployeeComp(ctx, pool, records); err != nil {
		t.Fatalf("InsertEmployeeComp: %v", err)
	}

	var count int
	pool.QueryRow(ctx,
		`SELECT COUNT(*) FROM employee_compensation WHERE object_id = $1`, objectID,
	).Scan(&count)
	if count != 1 {
		t.Errorf("expected 1 row after dedup, got %d", count)
	}
}
