// Package bmf loads the IRS Exempt Organization Business Master File (BMF)
// and filters it to organizations with NTEE codes related to housing and
// homeless services. The result is used as an EIN allowlist so the main
// pipeline never downloads XMLs for unrelated organizations.
//
// IRS BMF reference:
// https://www.irs.gov/charities-non-profits/exempt-organizations-business-master-file-extract-eo-bmf
package bmf

import (
	"context"
	"encoding/csv"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"strings"
	"time"
)

// The IRS publishes the full BMF across 4 regional CSV files.
var bmfURLs = []string{
	"https://www.irs.gov/pub/irs-soi/eo1.csv",
	"https://www.irs.gov/pub/irs-soi/eo2.csv",
	"https://www.irs.gov/pub/irs-soi/eo3.csv",
	"https://www.irs.gov/pub/irs-soi/eo4.csv",
}

// homelessNTEEPrefixes covers NTEE codes for housing and homeless services.
// L  = Housing & Shelter (all subtypes, e.g. L41 Temporary Housing for Homeless)
// P60 = Emergency Assistance (food, clothing, cash)
// P65 = Residential Care & Adult Day Programs
// P80 = Centers to Support the Rights of Those with Disabilities (some overlap)
// P85 = Homeless Centers
// P86 = Homeless Shelters
//
// Adjust this list to widen or narrow the dataset.
var homelessNTEEPrefixes = []string{
	"L",   // All Housing & Shelter subtypes
	"P60", // Emergency Assistance
	"P65", // Residential Care
	"P85", // Homeless Centers
	"P86", // Homeless Shelters
}

// OrgInfo holds the BMF metadata we care about for each matched organization.
type OrgInfo struct {
	EIN               string
	Name              string
	City              string
	State             string
	NTEECode          string
	IRSSubsectionCode string // e.g. "03" for 501(c)(3)
	RulingDate        string // YYYYMM from BMF RULING column
}

// Loader fetches and parses BMF files.
type Loader struct {
	client *http.Client
}

func New() *Loader {
	return &Loader{
		client: &http.Client{Timeout: 120 * time.Second},
	}
}

// Load downloads all 4 BMF regional files and returns a map of EIN → OrgInfo
// for every organization whose NTEE code matches a homeless/housing prefix.
func (l *Loader) Load(ctx context.Context) (map[string]OrgInfo, error) {
	orgs := make(map[string]OrgInfo)
	for _, url := range bmfURLs {
		slog.Info("loading BMF file", "url", url)
		n, err := l.loadFile(ctx, url, orgs)
		if err != nil {
			return nil, fmt.Errorf("bmf %s: %w", url, err)
		}
		slog.Info("BMF file loaded", "url", url, "matched", n)
	}
	slog.Info("BMF load complete", "total_orgs", len(orgs))
	return orgs, nil
}

func (l *Loader) loadFile(ctx context.Context, url string, out map[string]OrgInfo) (int, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return 0, err
	}

	resp, err := l.client.Do(req)
	if err != nil {
		return 0, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return 0, fmt.Errorf("HTTP %d", resp.StatusCode)
	}

	r := csv.NewReader(resp.Body)
	r.LazyQuotes = true
	r.TrimLeadingSpace = true

	// Build a column → index map from the header row so we're not
	// dependent on column order.
	headers, err := r.Read()
	if err != nil {
		return 0, fmt.Errorf("read header: %w", err)
	}
	idx := make(map[string]int, len(headers))
	for i, h := range headers {
		idx[strings.ToUpper(strings.TrimSpace(h))] = i
	}

	matched := 0
	for {
		row, err := r.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			continue // skip malformed rows
		}

		ntee := strings.TrimSpace(col(row, idx, "NTEE_CD"))
		if !matchesNTEE(ntee) {
			continue
		}

		ein := strings.TrimSpace(col(row, idx, "EIN"))
		if ein == "" {
			continue
		}

		out[ein] = OrgInfo{
			EIN:               ein,
			Name:              strings.TrimSpace(col(row, idx, "NAME")),
			City:              strings.ToTitle(strings.ToLower(strings.TrimSpace(col(row, idx, "CITY")))),
			State:             strings.TrimSpace(col(row, idx, "STATE")),
			NTEECode:          ntee,
			IRSSubsectionCode: strings.TrimSpace(col(row, idx, "SUBSECTION")),
			RulingDate:        strings.TrimSpace(col(row, idx, "RULING")),
		}
		matched++
	}
	return matched, nil
}

func col(row []string, idx map[string]int, name string) string {
	i, ok := idx[name]
	if !ok || i >= len(row) {
		return ""
	}
	return row[i]
}

func matchesNTEE(code string) bool {
	if code == "" {
		return false
	}
	for _, prefix := range homelessNTEEPrefixes {
		if strings.HasPrefix(code, prefix) {
			return true
		}
	}
	return false
}
