package parser

import (
	"os"
	"testing"
)

func readFixture(t *testing.T, name string) []byte {
	t.Helper()
	data, err := os.ReadFile("testdata/" + name)
	if err != nil {
		t.Fatalf("read fixture %s: %v", name, err)
	}
	return data
}

// TestParse_Full990 verifies that a complete modern 990 XML is parsed correctly
// across all sections: header, Schedule J, Part VII-A/B, Part VIII, Part IX,
// Part X, and Schedule I.
func TestParse_Full990(t *testing.T) {
	data := readFixture(t, "full_990.xml")
	result, err := Parse(data, "OBJ001", "HOUSING HELP INC", "Denver", "CO", "L41")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}

	// ── Org-level fields ──────────────────────────────────────────────────────

	if result.GrossReceipts != 5_000_000 {
		t.Errorf("GrossReceipts: got %d, want 5000000", result.GrossReceipts)
	}
	if result.WebsiteURL != "https://example.org" {
		t.Errorf("WebsiteURL: got %q", result.WebsiteURL)
	}
	// BMF city/state take precedence over XML header values.
	if result.City != "Denver" {
		t.Errorf("City: got %q, want Denver", result.City)
	}
	if result.State != "CO" {
		t.Errorf("State: got %q, want CO", result.State)
	}

	// ── Schedule J ────────────────────────────────────────────────────────────

	if len(result.ScheduleJRecords) != 1 {
		t.Fatalf("ScheduleJRecords: got %d, want 1", len(result.ScheduleJRecords))
	}
	j := result.ScheduleJRecords[0]
	if j.PersonName != "JANE DOE" {
		t.Errorf("ScheduleJ PersonName: got %q", j.PersonName)
	}
	if j.Title != "Executive Director" {
		t.Errorf("ScheduleJ Title: got %q", j.Title)
	}
	if j.BaseCompensation != 110_000 {
		t.Errorf("ScheduleJ BaseCompensation: got %d, want 110000", j.BaseCompensation)
	}
	if j.Bonus != 10_000 {
		t.Errorf("ScheduleJ Bonus: got %d, want 10000", j.Bonus)
	}
	if j.DeferredComp != 5_000 {
		t.Errorf("ScheduleJ DeferredComp: got %d, want 5000", j.DeferredComp)
	}
	if j.TotalCompFilingOrg != 132_000 {
		t.Errorf("ScheduleJ TotalCompFilingOrg: got %d, want 132000", j.TotalCompFilingOrg)
	}
	// Passthrough fields from Parse parameters.
	if j.ObjectID != "OBJ001" {
		t.Errorf("ScheduleJ ObjectID: got %q", j.ObjectID)
	}
	if j.NTEECode != "L41" {
		t.Errorf("ScheduleJ NTEECode: got %q", j.NTEECode)
	}
	if j.CompanyName != "HOUSING HELP INC" {
		t.Errorf("ScheduleJ CompanyName: got %q", j.CompanyName)
	}

	// ── Part VII-A: Officers / Employees ──────────────────────────────────────

	if len(result.Employees) != 2 {
		t.Fatalf("Employees: got %d, want 2", len(result.Employees))
	}
	e0 := result.Employees[0] // Jane Doe — officer
	if e0.PersonName != "JANE DOE" {
		t.Errorf("Employees[0] PersonName: got %q", e0.PersonName)
	}
	if !e0.IsOfficer {
		t.Error("Employees[0] IsOfficer: want true")
	}
	if e0.IsTrusteeOrDirector {
		t.Error("Employees[0] IsTrusteeOrDirector: want false")
	}
	if e0.ReportableCompFromOrg != 120_000 {
		t.Errorf("Employees[0] ReportableCompFromOrg: got %d, want 120000", e0.ReportableCompFromOrg)
	}

	e1 := result.Employees[1] // Bob Board — director
	if e1.PersonName != "BOB BOARD" {
		t.Errorf("Employees[1] PersonName: got %q", e1.PersonName)
	}
	if !e1.IsTrusteeOrDirector {
		t.Error("Employees[1] IsTrusteeOrDirector: want true")
	}
	if e1.IsOfficer {
		t.Error("Employees[1] IsOfficer: want false")
	}

	// ── Part VII-B: Contractors (modern schema) ────────────────────────────────

	if len(result.Contractors) != 1 {
		t.Fatalf("Contractors: got %d, want 1", len(result.Contractors))
	}
	c := result.Contractors[0]
	if c.ContractorName != "ACME CONSULTING LLC" {
		t.Errorf("Contractor name: got %q", c.ContractorName)
	}
	if c.CompensationAmt != 75_000 {
		t.Errorf("Contractor comp: got %d, want 75000", c.CompensationAmt)
	}
	if c.City != "Boulder" {
		t.Errorf("Contractor city: got %q", c.City)
	}
	if c.ServicesDesc != "IT Services" {
		t.Errorf("Contractor services: got %q", c.ServicesDesc)
	}

	// ── Part VIII: Revenue ────────────────────────────────────────────────────

	if result.Revenue == nil {
		t.Fatal("Revenue is nil")
	}
	rev := result.Revenue
	if rev.GovernmentGrantsAmt != 3_000_000 {
		t.Errorf("Revenue.GovernmentGrantsAmt: got %d", rev.GovernmentGrantsAmt)
	}
	if rev.TotalContributionsAmt != 3_500_000 {
		t.Errorf("Revenue.TotalContributionsAmt: got %d", rev.TotalContributionsAmt)
	}
	if rev.TotalProgramServiceRevAmt != 900_000 {
		t.Errorf("Revenue.TotalProgramServiceRevAmt: got %d", rev.TotalProgramServiceRevAmt)
	}
	// CYTotalRevenueAmt is the primary source.
	if rev.TotalRevenueAmt != 4_500_000 {
		t.Errorf("Revenue.TotalRevenueAmt: got %d, want 4500000", rev.TotalRevenueAmt)
	}
	// InvestmentIncomeGrp wins when non-zero.
	if rev.InvestmentIncomeAmt != 50_000 {
		t.Errorf("Revenue.InvestmentIncomeAmt: got %d, want 50000", rev.InvestmentIncomeAmt)
	}
	if rev.NetGainLossInvestmentsAmt != 25_000 {
		t.Errorf("Revenue.NetGainLossInvestmentsAmt: got %d, want 25000", rev.NetGainLossInvestmentsAmt)
	}
	if rev.EIN != "123456789" {
		t.Errorf("Revenue.EIN: got %q", rev.EIN)
	}
	if rev.TaxYear != 2021 {
		t.Errorf("Revenue.TaxYear: got %d, want 2021", rev.TaxYear)
	}

	// ── Part IX: Expenses ─────────────────────────────────────────────────────

	if result.Expenses == nil {
		t.Fatal("Expenses is nil")
	}
	exp := result.Expenses
	if exp.GrantsToOrgsAmt != 200_000 {
		t.Errorf("Expenses.GrantsToOrgsAmt: got %d", exp.GrantsToOrgsAmt)
	}
	if exp.OtherSalariesWagesAmt != 1_500_000 {
		t.Errorf("Expenses.OtherSalariesWagesAmt: got %d", exp.OtherSalariesWagesAmt)
	}
	if exp.OccupancyAmt != 300_000 {
		t.Errorf("Expenses.OccupancyAmt: got %d", exp.OccupancyAmt)
	}
	if exp.TotalFunctionalExpensesAmt != 4_200_000 {
		t.Errorf("Expenses.TotalFunctionalExpensesAmt: got %d", exp.TotalFunctionalExpensesAmt)
	}

	// ── Part X: Balance Sheet ─────────────────────────────────────────────────

	if result.BalanceSheet == nil {
		t.Fatal("BalanceSheet is nil")
	}
	bs := result.BalanceSheet
	if bs.CashNonInterestBearingAmt != 100_000 {
		t.Errorf("BalanceSheet.CashNonInterestBearingAmt: got %d", bs.CashNonInterestBearingAmt)
	}
	if bs.TotalAssetsAmt != 2_000_000 {
		t.Errorf("BalanceSheet.TotalAssetsAmt: got %d", bs.TotalAssetsAmt)
	}
	if bs.TotalLiabilitiesAmt != 500_000 {
		t.Errorf("BalanceSheet.TotalLiabilitiesAmt: got %d", bs.TotalLiabilitiesAmt)
	}
	if bs.TotalNetAssetsAmt != 1_500_000 {
		t.Errorf("BalanceSheet.TotalNetAssetsAmt: got %d", bs.TotalNetAssetsAmt)
	}

	// ── Schedule I: Grants Paid ───────────────────────────────────────────────

	if len(result.Grants) != 1 {
		t.Fatalf("Grants: got %d, want 1", len(result.Grants))
	}
	g := result.Grants[0]
	if g.RecipientName != "SHELTER FIRST INC" {
		t.Errorf("Grant RecipientName: got %q", g.RecipientName)
	}
	if g.RecipientEIN != "987654321" {
		t.Errorf("Grant RecipientEIN: got %q", g.RecipientEIN)
	}
	// RecipientUSAddress field used in this fixture.
	if g.RecipientCity != "Aurora" {
		t.Errorf("Grant RecipientCity: got %q, want Aurora", g.RecipientCity)
	}
	if g.RecipientState != "CO" {
		t.Errorf("Grant RecipientState: got %q", g.RecipientState)
	}
	if g.CashGrantAmt != 50_000 {
		t.Errorf("Grant CashGrantAmt: got %d", g.CashGrantAmt)
	}
	if g.PurposeOfGrantTxt != "Emergency shelter services" {
		t.Errorf("Grant PurposeOfGrantTxt: got %q", g.PurposeOfGrantTxt)
	}
}

// TestParse_BMFCityOverridesXML verifies that a non-empty bmfCity/bmfState
// wins over the city/state in the XML return header.
func TestParse_BMFCityOverridesXML(t *testing.T) {
	data := readFixture(t, "full_990.xml")
	// XML header has Portland/OR; BMF has Denver/CO — BMF must win.
	result, err := Parse(data, "OBJ001", "TEST ORG", "Denver", "CO", "L41")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if result.City != "Denver" {
		t.Errorf("City: got %q, want Denver (BMF must override XML)", result.City)
	}
	if result.State != "CO" {
		t.Errorf("State: got %q, want CO", result.State)
	}
}

// TestParse_990EZ verifies that when there is no IRS990 element (990EZ filing),
// the parser returns Schedule J records but nil for all financial sections.
func TestParse_990EZ(t *testing.T) {
	data := readFixture(t, "990ez.xml")
	result, err := Parse(data, "OBJ002", "SMALL SHELTER ORG", "Chicago", "IL", "P85")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}

	if len(result.ScheduleJRecords) != 1 {
		t.Fatalf("ScheduleJRecords: got %d, want 1", len(result.ScheduleJRecords))
	}
	j := result.ScheduleJRecords[0]
	if j.PersonName != "JOHN SMITH" {
		t.Errorf("PersonName: got %q", j.PersonName)
	}
	if j.ReportableCompFromOrg != 85_000 {
		t.Errorf("ReportableCompFromOrg: got %d", j.ReportableCompFromOrg)
	}
	if j.BaseCompensation != 80_000 {
		t.Errorf("BaseCompensation: got %d", j.BaseCompensation)
	}
	if j.TotalCompFilingOrg != 92_000 {
		t.Errorf("TotalCompFilingOrg: got %d", j.TotalCompFilingOrg)
	}

	// No IRS990 element — all financial sections must be nil/empty.
	if result.Revenue != nil {
		t.Error("Revenue must be nil for 990EZ")
	}
	if result.Expenses != nil {
		t.Error("Expenses must be nil for 990EZ")
	}
	if result.BalanceSheet != nil {
		t.Error("BalanceSheet must be nil for 990EZ")
	}
	if len(result.Employees) != 0 {
		t.Errorf("Employees must be empty for 990EZ, got %d", len(result.Employees))
	}
	if len(result.Contractors) != 0 {
		t.Errorf("Contractors must be empty for 990EZ, got %d", len(result.Contractors))
	}
	if len(result.Grants) != 0 {
		t.Errorf("Grants must be empty for 990EZ, got %d", len(result.Grants))
	}
}

// TestParse_OldContractorSchema covers three edge cases in a single filing:
//  1. Old contractor name schema (flat BusinessNameLine1Txt, no BusinessName wrapper)
//  2. Revenue fallback: CYTotalRevenueAmt absent → TotalRevenueGrp used
//  3. Schedule I address in USAddress field (not RecipientUSAddress)
//  4. Empty bmfCity → city falls back to XML header
func TestParse_OldContractorSchema(t *testing.T) {
	data := readFixture(t, "old_contractor.xml")
	// Empty bmfCity/bmfState — parser must fall back to XML header.
	result, err := Parse(data, "OBJ003", "NORTHWEST HELP", "", "", "L50")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}

	// City/state from XML header.
	if result.City != "Seattle" {
		t.Errorf("City: got %q, want Seattle", result.City)
	}
	if result.State != "WA" {
		t.Errorf("State: got %q, want WA", result.State)
	}

	// Old contractor schema: name at ContractorName/BusinessNameLine1Txt.
	if len(result.Contractors) != 1 {
		t.Fatalf("Contractors: got %d, want 1", len(result.Contractors))
	}
	c := result.Contractors[0]
	if c.ContractorName != "OLD FORMAT CORP" {
		t.Errorf("ContractorName: got %q, want OLD FORMAT CORP", c.ContractorName)
	}
	if c.CompensationAmt != 40_000 {
		t.Errorf("CompensationAmt: got %d, want 40000", c.CompensationAmt)
	}

	// Revenue fallback: no CYTotalRevenueAmt → TotalRevenueGrp/TotalRevenueColumnAmt.
	if result.Revenue == nil {
		t.Fatal("Revenue is nil")
	}
	if result.Revenue.TotalRevenueAmt != 950_000 {
		t.Errorf("TotalRevenueAmt: got %d, want 950000 (fallback from TotalRevenueGrp)", result.Revenue.TotalRevenueAmt)
	}

	// Schedule I with USAddress (not RecipientUSAddress).
	if len(result.Grants) != 1 {
		t.Fatalf("Grants: got %d, want 1", len(result.Grants))
	}
	g := result.Grants[0]
	if g.RecipientName != "FOOD BANK WEST" {
		t.Errorf("Grant RecipientName: got %q", g.RecipientName)
	}
	if g.RecipientCity != "Renton" {
		t.Errorf("Grant RecipientCity: got %q, want Renton (from USAddress fallback)", g.RecipientCity)
	}
	if g.RecipientState != "WA" {
		t.Errorf("Grant RecipientState: got %q", g.RecipientState)
	}
	if g.CashGrantAmt != 25_000 {
		t.Errorf("Grant CashGrantAmt: got %d", g.CashGrantAmt)
	}
}

// TestParse_MalformedXML verifies that malformed input returns a non-nil error.
func TestParse_MalformedXML(t *testing.T) {
	_, err := Parse([]byte("<not valid xml <<<"), "OBJ999", "BAD ORG", "", "", "")
	if err == nil {
		t.Error("expected error for malformed XML, got nil")
	}
}

// TestParse_EmptyAmountsDefaultToZero verifies that missing dollar-amount
// elements parse to 0 rather than producing an error.
func TestParse_EmptyAmountsDefaultToZero(t *testing.T) {
	const xml = `<?xml version="1.0" encoding="UTF-8"?>
<Return xmlns="http://www.irs.gov/efile">
  <ReturnHeader>
    <TaxYr>2021</TaxYr>
    <Filer><EIN>999000001</EIN></Filer>
  </ReturnHeader>
  <ReturnData>
    <IRS990>
      <!-- Intentionally omit all amount fields -->
    </IRS990>
  </ReturnData>
</Return>`
	result, err := Parse([]byte(xml), "OBJ_EMPTY", "EMPTY ORG", "Austin", "TX", "L41")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if result.GrossReceipts != 0 {
		t.Errorf("GrossReceipts: got %d, want 0", result.GrossReceipts)
	}
	if result.Revenue == nil {
		t.Fatal("Revenue is nil")
	}
	if result.Revenue.TotalRevenueAmt != 0 {
		t.Errorf("TotalRevenueAmt: got %d, want 0", result.Revenue.TotalRevenueAmt)
	}
	if result.BalanceSheet == nil {
		t.Fatal("BalanceSheet is nil")
	}
	if result.BalanceSheet.TotalAssetsAmt != 0 {
		t.Errorf("TotalAssetsAmt: got %d, want 0", result.BalanceSheet.TotalAssetsAmt)
	}
	if len(result.ScheduleJRecords) != 0 {
		t.Errorf("ScheduleJRecords: got %d, want 0", len(result.ScheduleJRecords))
	}
}
