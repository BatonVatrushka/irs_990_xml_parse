package bmf

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestMatchesNTEE(t *testing.T) {
	tests := []struct {
		code  string
		match bool
	}{
		// Housing & Shelter (prefix "L") — all subtypes match
		{"L", true},
		{"L41", true},
		{"L80", true},
		{"L99", true},
		// Emergency Assistance (prefix "P60")
		{"P60", true},
		{"P601", true},
		// Residential Care (prefix "P65")
		{"P65", true},
		{"P650", true},
		// Homeless Centers (prefix "P85")
		{"P85", true},
		{"P85A", true},
		// Homeless Shelters (prefix "P86")
		{"P86", true},
		// Should NOT match
		{"P", false},   // bare P — not a listed prefix
		{"P80", false}, // P80 is not in the list
		{"P8", false},  // too short
		{"A10", false}, // Arts
		{"B", false},   // Education
		{"", false},    // empty
		{"p85", false}, // case-sensitive — lowercase does not match
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("code=%q", tt.code), func(t *testing.T) {
			got := matchesNTEE(tt.code)
			if got != tt.match {
				t.Errorf("matchesNTEE(%q) = %v, want %v", tt.code, got, tt.match)
			}
		})
	}
}

func TestLoadFile_FiltersByNTEE(t *testing.T) {
	// CSV with three rows: L41 (match), A10 (no match), P85 (match).
	// CITY column is all-caps; the code does ToLower then ToTitle, which
	// for all-ASCII input round-trips back to all-caps.
	const csvData = `EIN,NAME,CITY,STATE,NTEE_CD,SUBSECTION,RULING
123456789,HOUSING HELP INC,DENVER,CO,L41,03,202001
987654321,GENERAL CHARITY,CHICAGO,IL,A10,03,201901
111222333,HOMELESS CENTER,SEATTLE,WA,P85,03,201801
`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, csvData)
	}))
	defer ts.Close()

	l := New()
	orgs := make(map[string]OrgInfo)
	n, err := l.loadFile(context.Background(), ts.URL, orgs)
	if err != nil {
		t.Fatalf("loadFile: %v", err)
	}

	if n != 2 {
		t.Errorf("matched count: got %d, want 2", n)
	}
	if len(orgs) != 2 {
		t.Errorf("orgs map size: got %d, want 2", len(orgs))
	}

	// L41 org should be present with correct fields.
	o, ok := orgs["123456789"]
	if !ok {
		t.Fatal("EIN 123456789 not found")
	}
	if o.EIN != "123456789" {
		t.Errorf("EIN: got %q", o.EIN)
	}
	if o.NTEECode != "L41" {
		t.Errorf("NTEECode: got %q, want L41", o.NTEECode)
	}
	if o.State != "CO" {
		t.Errorf("State: got %q, want CO", o.State)
	}
	if o.IRSSubsectionCode != "03" {
		t.Errorf("IRSSubsectionCode: got %q, want 03", o.IRSSubsectionCode)
	}
	if o.RulingDate != "202001" {
		t.Errorf("RulingDate: got %q, want 202001", o.RulingDate)
	}

	// A10 must be excluded.
	if _, ok := orgs["987654321"]; ok {
		t.Error("A10 org (EIN 987654321) must not be in result")
	}

	// P85 must be included.
	if _, ok := orgs["111222333"]; !ok {
		t.Error("P85 org (EIN 111222333) must be in result")
	}
}

func TestLoadFile_SkipsRowsWithMissingEIN(t *testing.T) {
	const csvData = `EIN,NAME,CITY,STATE,NTEE_CD,SUBSECTION,RULING
,NO EIN ORG,BOSTON,MA,L41,03,202001
999000001,VALID ORG,PHOENIX,AZ,L80,03,202101
`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, csvData)
	}))
	defer ts.Close()

	l := New()
	orgs := make(map[string]OrgInfo)
	n, err := l.loadFile(context.Background(), ts.URL, orgs)
	if err != nil {
		t.Fatalf("loadFile: %v", err)
	}
	if n != 1 {
		t.Errorf("matched count: got %d, want 1 (row with empty EIN skipped)", n)
	}
	if _, ok := orgs["999000001"]; !ok {
		t.Error("EIN 999000001 must be present")
	}
}

func TestLoadFile_HTTP404(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	}))
	defer ts.Close()

	l := New()
	orgs := make(map[string]OrgInfo)
	_, err := l.loadFile(context.Background(), ts.URL, orgs)
	if err == nil {
		t.Error("expected error for HTTP 404, got nil")
	}
}

func TestLoadFile_ColumnOrderIndependent(t *testing.T) {
	// Columns in a different order than bmf.go expects — the header-map
	// approach should handle any ordering.
	const csvData = `STATE,EIN,NTEE_CD,NAME,RULING,CITY,SUBSECTION
TX,777888999,P86,DALLAS SHELTER,201501,DALLAS,03
`
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, csvData)
	}))
	defer ts.Close()

	l := New()
	orgs := make(map[string]OrgInfo)
	_, err := l.loadFile(context.Background(), ts.URL, orgs)
	if err != nil {
		t.Fatalf("loadFile: %v", err)
	}
	o, ok := orgs["777888999"]
	if !ok {
		t.Fatal("EIN 777888999 not found")
	}
	if o.State != "TX" {
		t.Errorf("State: got %q, want TX", o.State)
	}
	if o.NTEECode != "P86" {
		t.Errorf("NTEECode: got %q", o.NTEECode)
	}
}
