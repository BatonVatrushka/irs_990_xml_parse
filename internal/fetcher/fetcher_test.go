package fetcher

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// ─── ZIPURLsForYear ───────────────────────────────────────────────────────────

func TestZIPURLsForYear_OldFormat(t *testing.T) {
	// 2016–2020 use numbered download990xml_YEAR_N.zip files (up to 10).
	for _, year := range []int{2016, 2019, 2020} {
		urls := newForTest("https://base").ZIPURLsForYear(year)
		if len(urls) == 0 {
			t.Errorf("year %d: got 0 URLs", year)
		}
		for _, u := range urls {
			if !strings.Contains(u, fmt.Sprintf("download990xml_%d_", year)) {
				t.Errorf("year %d: URL %q missing expected pattern", year, u)
			}
		}
	}
}

func TestZIPURLsForYear_2021SingleFile(t *testing.T) {
	// 2021 has exactly one TEOS ZIP64 file; old download990xml URLs 404.
	urls := newForTest("https://base").ZIPURLsForYear(2021)
	if len(urls) != 1 {
		t.Fatalf("2021: want exactly 1 URL, got %d: %v", len(urls), urls)
	}
	if !strings.Contains(urls[0], "2021_TEOS_XML_01A.zip") {
		t.Errorf("2021: unexpected URL %q", urls[0])
	}
}

func TestZIPURLsForYear_2022TwoFiles(t *testing.T) {
	urls := newForTest("https://base").ZIPURLsForYear(2022)
	if len(urls) != 2 {
		t.Fatalf("2022: want exactly 2 URLs, got %d: %v", len(urls), urls)
	}
	want := []string{"2022_TEOS_XML_01A.zip", "2022_TEOS_XML_02A.zip"}
	for i, w := range want {
		if !strings.Contains(urls[i], w) {
			t.Errorf("2022 URL[%d]: got %q, want to contain %q", i, urls[i], w)
		}
	}
}

func TestZIPURLsForYear_2023MonthlyAB(t *testing.T) {
	// 2023+ generate A and B variant for each of the 12 months = 24 URLs.
	urls := newForTest("https://base").ZIPURLsForYear(2023)
	if len(urls) != 24 {
		t.Fatalf("2023: want 24 URLs (12 months × A+B), got %d", len(urls))
	}
	// Spot-check January A and B.
	if !strings.Contains(urls[0], "2023_TEOS_XML_01A.zip") {
		t.Errorf("2023 URL[0]: got %q", urls[0])
	}
	if !strings.Contains(urls[1], "2023_TEOS_XML_01B.zip") {
		t.Errorf("2023 URL[1]: got %q", urls[1])
	}
	// Spot-check December.
	if !strings.Contains(urls[22], "2023_TEOS_XML_12A.zip") {
		t.Errorf("2023 URL[22]: got %q", urls[22])
	}
}

func TestZIPURLsForYear_UsesBaseURL(t *testing.T) {
	// Verify that all generated URLs start with the injected base URL.
	base := "https://custom.example.com/irs"
	for _, year := range []int{2019, 2021, 2022, 2023} {
		urls := newForTest(base).ZIPURLsForYear(year)
		for _, u := range urls {
			if !strings.HasPrefix(u, base) {
				t.Errorf("year %d: URL %q does not start with base %q", year, u, base)
			}
		}
	}
}

// ─── FetchIndex ───────────────────────────────────────────────────────────────

func TestFetchIndex_ParsesCSV(t *testing.T) {
	const csvData = `RETURN_ID,FILING_TYPE,EIN,TAX_PERIOD,SUB_DATE,TAXPAYER_NAME,RETURN_TYPE,DLN,OBJECT_ID
1001,F,123456789,202112,2022-03-15,HOUSING HELP INC,990,,200100001
1002,F,987654321,202112,2022-04-01,SHELTER ORG,990EZ,,200200002
`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// FetchIndex builds: {baseURL}/{year}/index_{year}.csv
		// With base = ts.URL, the path will be /2022/index_2022.csv.
		fmt.Fprint(w, csvData)
	}))
	defer ts.Close()

	f := newForTest(ts.URL)
	entries, err := f.FetchIndex(context.Background(), 2022)
	if err != nil {
		t.Fatalf("FetchIndex: %v", err)
	}
	if len(entries) != 2 {
		t.Fatalf("entries: got %d, want 2", len(entries))
	}

	e0 := entries[0]
	if e0.EIN != "123456789" {
		t.Errorf("entries[0].EIN: got %q", e0.EIN)
	}
	if e0.CompanyName != "HOUSING HELP INC" {
		t.Errorf("entries[0].CompanyName: got %q", e0.CompanyName)
	}
	if e0.FormType != "990" {
		t.Errorf("entries[0].FormType: got %q", e0.FormType)
	}
	if e0.ObjectID != "200100001" {
		t.Errorf("entries[0].ObjectID: got %q", e0.ObjectID)
	}
	if e0.TaxPeriod != "202112" {
		t.Errorf("entries[0].TaxPeriod: got %q", e0.TaxPeriod)
	}

	e1 := entries[1]
	if e1.FormType != "990EZ" {
		t.Errorf("entries[1].FormType: got %q", e1.FormType)
	}
	if e1.ObjectID != "200200002" {
		t.Errorf("entries[1].ObjectID: got %q", e1.ObjectID)
	}
}

func TestFetchIndex_DoesNotCrashOnShortRows(t *testing.T) {
	// A row with fewer columns than the header is still parseable by csv.Reader;
	// the col() helper returns "" for missing columns. FetchIndex must not crash.
	const csvData = `RETURN_ID,FILING_TYPE,EIN,TAX_PERIOD,SUB_DATE,TAXPAYER_NAME,RETURN_TYPE,DLN,OBJECT_ID
1001,F,123456789,202112,2022-03-15,HOUSING HELP INC,990,,200100001
this,is,short
`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, csvData)
	}))
	defer ts.Close()

	f := newForTest(ts.URL)
	entries, err := f.FetchIndex(context.Background(), 2022)
	if err != nil {
		t.Fatalf("FetchIndex must not error on short rows: %v", err)
	}
	// The valid row must be present.
	found := false
	for _, e := range entries {
		if e.EIN == "123456789" {
			found = true
		}
	}
	if !found {
		t.Error("valid entry (EIN 123456789) must be present in result")
	}
}

func TestFetchIndex_HTTP404(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	}))
	defer ts.Close()

	f := newForTest(ts.URL)
	_, err := f.FetchIndex(context.Background(), 2022)
	if err == nil {
		t.Error("expected error for HTTP 404, got nil")
	}
}
