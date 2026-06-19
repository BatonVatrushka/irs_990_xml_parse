// Package parser extracts structured data from IRS 990 XML filings.
//
// The IRS e-file schema uses a default namespace (http://www.irs.gov/efile).
// Go's encoding/xml matches on local name when struct tags omit a namespace,
// so all struct tags here use bare element names.
//
// Key schema conventions observed in IRS 990 XML (2017v2.x – 2022v5.x):
//
//   Part I summary fields:
//     CYTotalRevenueAmt, CYInvestmentIncomeAmt, etc.
//     These flat fields appear in every 990 XML and are the most reliable
//     source for Part VIII totals.
//
//   Part VIII: Contribution/grants lines are flat fields (GovernmentGrantsAmt,
//     TotalContributionsAmt, etc.).  Other revenue lines use a *Grp wrapper
//     with a TotalRevenueColumnAmt child.
//
//   Part IX: Every expense line uses a *Grp wrapper with a TotalAmt child.
//     The group element names closely mirror the line descriptions.
//
//   Part X: Each balance sheet line uses a *Grp wrapper with BOYAmt / EOYAmt.
//     We capture only EOY (end-of-year) values.
//
//   Part VII-A: Form990PartVIISectionAGrp (officers/directors/employees)
//   Part VII-B: ContractorCompensationGrp
//               Contractor name is nested: ContractorName/BusinessName/BusinessNameLine1Txt
//
//   Schedule J: RltdOrgOfficerTrstKeyEmplGrp (high-earner detail)
//   Schedule I: GrantsOtherAsstToOrgsInUSGrp (grants to US orgs; recipient-level detail)
package parser

import (
	"encoding/xml"
	"fmt"
	"strconv"
	"strings"

	"github.com/BatonVatrushka/irs_990_xml_parse/internal/models"
)

// ─── helper group types ───────────────────────────────────────────────────────

// amtGrp is used for Part X balance sheet lines (BOY / EOY columns).
type amtGrp struct {
	EOYAmt string `xml:"EOYAmt"`
}

// revGrp is used for Part VIII revenue groups that have multiple columns.
// We capture the Total Revenue column amount.
type revGrp struct {
	TotalRevenueColumnAmt string `xml:"TotalRevenueColumnAmt"`
}

// expGrp is used for Part IX expense groups. Each line has 4 column amounts
// (total / program / mgmt / fundraising). We capture the total column.
type expGrp struct {
	TotalAmt string `xml:"TotalAmt"`
}

// ─── XML struct tree ──────────────────────────────────────────────────────────

type return990 struct {
	XMLName      xml.Name     `xml:"Return"`
	ReturnHeader returnHeader `xml:"ReturnHeader"`
	ReturnData   returnData   `xml:"ReturnData"`
}

type returnHeader struct {
	TaxYear string `xml:"TaxYr"`
	Filer   filer  `xml:"Filer"`
}

type filer struct {
	EIN       string    `xml:"EIN"`
	USAddress usAddress `xml:"USAddress"`
}

type usAddress struct {
	City  string `xml:"CityNm"`
	State string `xml:"StateAbbreviationCd"`
}

type returnData struct {
	IRS990    *irs990    `xml:"IRS990"`
	ScheduleJ *scheduleJ `xml:"IRS990ScheduleJ"`
	ScheduleI *scheduleI `xml:"IRS990ScheduleI"`
}

// ─── IRS990 main form ─────────────────────────────────────────────────────────

type irs990 struct {
	// ── Org header ────────────────────────────────────────────────────────
	GrossReceiptsAmt  string `xml:"GrossReceiptsAmt"`
	WebsiteAddressTxt string `xml:"WebsiteAddressTxt"`

	// ── Part I summary: CY (current year) flat fields ─────────────────────
	// Present in all 990 XML schema versions. Used as reliable totals for
	// Part VIII revenue fields whose group names vary by schema version.
	CYTotalRevenueAmt     string `xml:"CYTotalRevenueAmt"`
	CYInvestmentIncomeAmt string `xml:"CYInvestmentIncomeAmt"`
	CYOtherRevenueAmt     string `xml:"CYOtherRevenueAmt"` // lines 5-11 total

	// ── Part VII-A: Officers, Directors, Trustees, Key Employees ──────────
	Officers []officerEmployee `xml:"Form990PartVIISectionAGrp"`

	// ── Part VII-B: Independent Contractors ──────────────────────────────
	Contractors []contractorComp `xml:"ContractorCompensationGrp"`

	// ── Part VIII: Revenue ────────────────────────────────────────────────
	// Contributions and grants (lines 1a–1g) — flat fields across all years.
	FederatedCampaignsAmt    string `xml:"FederatedCampaignsAmt"`
	MembershipDuesAmt        string `xml:"MembershipDuesAmt"`
	FundraisingAmt           string `xml:"FundraisingAmt"` // line 1c: contributions from fundraising events
	RelatedOrganizationsAmt  string `xml:"RelatedOrganizationsAmt"`
	GovernmentGrantsAmt      string `xml:"GovernmentGrantsAmt"`
	AllOtherContributionsAmt string `xml:"AllOtherContributionsAmt"`
	NoncashContributionsAmt  string `xml:"NoncashContributionsAmt"`
	TotalContributionsAmt    string `xml:"TotalContributionsAmt"`
	// Program service revenue (line 2g)
	TotalProgramServiceRevenueAmt string `xml:"TotalProgramServiceRevenueAmt"` // flat — confirmed correct name
	// Bond proceeds (line 5) — usually flat
	TaxExemptBondProceedsAmt string `xml:"TaxExemptBondProceedsAmt"`
	// Other revenue lines — group pattern
	InvestmentIncomeGrp          revGrp `xml:"InvestmentIncomeGrp"`           // line 3
	NetRentalIncomeOrLossGrp     revGrp `xml:"NetRentalIncomeOrLossGrp"`      // line 7d
	NetGainLossInvestmentsGrp    revGrp `xml:"NetGainLossInvestmentsGrp"`     // line 8d
	NetIncmFromFundraisingEvtGrp revGrp `xml:"NetIncmFromFundraisingEvtGrp"` // line 9d
	NetIncomeFromGamingGrp       revGrp `xml:"NetIncomeFromGamingGrp"`        // line 10d
	OtherRevenueTotalAmt         string `xml:"OtherRevenueTotalAmt"`           // line 11d misc
	// Total revenue (line 12) — group in XML but CYTotalRevenueAmt is the reliable source
	TotalRevenueGrp revGrp `xml:"TotalRevenueGrp"` // fallback when CY field is absent

	// ── Part IX: Expenses ────────────────────────────────────────────────
	// Every line uses a *Grp wrapper; we capture TotalAmt (column A).
	GrantsAndOtherAssistToOrgsGrp   expGrp `xml:"GrantsAndOtherAssistToOrgsGrp"`
	GrantsAndOtherAssistToIndivGrp  expGrp `xml:"GrantsAndOtherAssistToIndivGrp"`
	ForeignGrantsGrp                expGrp `xml:"ForeignGrantsGrp"`
	BenefitsToMembersGrp            expGrp `xml:"BenefitsToMembersGrp"`
	CompCurrentOfcrDirectorsGrp     expGrp `xml:"CompCurrentOfcrDirectorsGrp"`
	CompDisqualifiedPersonsGrp      expGrp `xml:"CompDisqualifiedPersonsGrp"`
	OtherSalariesAndWagesGrp        expGrp `xml:"OtherSalariesAndWagesGrp"`
	PensionPlanContributionsGrp     expGrp `xml:"PensionPlanContributionsGrp"`
	OtherEmployeeBenefitsGrp        expGrp `xml:"OtherEmployeeBenefitsGrp"`
	PayrollTaxesGrp                 expGrp `xml:"PayrollTaxesGrp"`
	FeesForServicesManagementGrp    expGrp `xml:"FeesForServicesManagementGrp"`
	FeesForServicesLegalGrp         expGrp `xml:"FeesForServicesLegalGrp"`
	FeesForServicesAccountingGrp    expGrp `xml:"FeesForServicesAccountingGrp"`
	FeesForServicesLobbyingGrp      expGrp `xml:"FeesForServicesLobbyingGrp"`
	FeesForServicesProfFundraisGrp  expGrp `xml:"FeesForServicesProfFundraisGrp"`
	FeesForServicesInvestMgmtGrp    expGrp `xml:"FeesForServicesInvestMgmtGrp"`
	FeesForServicesOtherGrp         expGrp `xml:"FeesForServicesOtherGrp"`
	AdvertisingGrp                  expGrp `xml:"AdvertisingGrp"`
	OfficeExpensesGrp               expGrp `xml:"OfficeExpensesGrp"`
	InformationTechnologyGrp        expGrp `xml:"InformationTechnologyGrp"`
	OccupancyGrp                    expGrp `xml:"OccupancyGrp"`
	TravelGrp                       expGrp `xml:"TravelGrp"`
	ConferencesMeetingsGrp          expGrp `xml:"ConferencesMeetingsGrp"`
	InterestGrp                     expGrp `xml:"InterestGrp"`
	DepreciationDepletionGrp        expGrp `xml:"DepreciationDepletionGrp"`
	InsuranceGrp                    expGrp `xml:"InsuranceGrp"`
	AllOtherExpensesGrp             expGrp `xml:"AllOtherExpensesGrp"`
	TotalFunctionalExpensesGrp      expGrp `xml:"TotalFunctionalExpensesGrp"`

	// ── Part X: Balance Sheet (End-of-Year) ──────────────────────────────
	// Assets
	CashNonInterestBearingGrp       amtGrp `xml:"CashNonInterestBearingGrp"`
	SavingsAndTempCashInvstGrp      amtGrp `xml:"SavingsAndTempCashInvstGrp"`
	PledgesAndGrantsReceivableGrp   amtGrp `xml:"PledgesAndGrantsReceivableGrp"`
	AccountsReceivableGrp           amtGrp `xml:"AccountsReceivableGrp"`
	ReceivablesFromOfficersGrp      amtGrp `xml:"ReceivablesFromOfficersGrp"`
	OtherNotesLoansReceivableNetGrp amtGrp `xml:"OtherNotesLoansReceivableNetGrp"`
	InventoriesForSaleOrUseGrp      amtGrp `xml:"InventoriesForSaleOrUseGrp"`
	PrepaidExpensesGrp              amtGrp `xml:"PrepaidExpensesGrp"`
	LandBldgEquipNetBookValueGrp    amtGrp `xml:"LandBldgEquipNetBookValueGrp"`
	InvestmentsPubTradedSecGrp      amtGrp `xml:"InvestmentsPubTradedSecGrp"`
	InvestmentsOtherSecuritiesGrp   amtGrp `xml:"InvestmentsOtherSecuritiesGrp"`
	InvestmentsProgramRelatedGrp    amtGrp `xml:"InvestmentsProgramRelatedGrp"`
	IntangibleAssetsNetGrp          amtGrp `xml:"IntangibleAssetsNetGrp"`
	OtherAssetsTotalGrp             amtGrp `xml:"OtherAssetsTotalGrp"`
	TotalAssetsGrp                  amtGrp `xml:"TotalAssetsGrp"`
	// Liabilities
	AccountsPayableAccruedExpGrp amtGrp `xml:"AccountsPayableAccruedExpGrp"`
	GrantsPayableGrp             amtGrp `xml:"GrantsPayableGrp"`
	DeferredRevenueGrp           amtGrp `xml:"DeferredRevenueGrp"`
	TaxExemptBondLiabilitiesGrp  amtGrp `xml:"TaxExemptBondLiabilitiesGrp"`
	LoansFromOfficersGrp         amtGrp `xml:"LoansFromOfficersGrp"`
	MortgagesNotesPayableLongGrp amtGrp `xml:"MortgagesNotesPayableLongGrp"`
	OtherLiabilitiesGrp          amtGrp `xml:"OtherLiabilitiesGrp"`
	TotalLiabilitiesGrp          amtGrp `xml:"TotalLiabilitiesGrp"`
	// Net assets — pre-ASU 2016
	UnrestrictedNetAssetsGrp    amtGrp `xml:"UnrestrictedNetAssetsGrp"`
	TemporarilyRstrNetAssetsGrp amtGrp `xml:"TemporarilyRstrNetAssetsGrp"`
	PermanentlyRstrNetAssetsGrp amtGrp `xml:"PermanentlyRstrNetAssetsGrp"`
	// Net assets — post-ASU 2016 (2018+ tax years)
	NetAssetsWODonorRestrictionsGrp   amtGrp `xml:"NetAssetsWODonorRestrictionsGrp"`
	NetAssetsWithDonorRestrictionsGrp amtGrp `xml:"NetAssetsWithDonorRestrictionsGrp"`
	// Total net assets — element name confirmed in 2017v2.x schema
	TotalNetAssetsFundBalanceGrp amtGrp `xml:"TotalNetAssetsFundBalanceGrp"`
}

// ── Part VII-A ────────────────────────────────────────────────────────────────

type officerEmployee struct {
	PersonNm                       string `xml:"PersonNm"`
	TitleTxt                       string `xml:"TitleTxt"`
	AverageHoursPerWeekRt          string `xml:"AverageHoursPerWeekRt"`
	IndividualTrusteeOrDirectorInd string `xml:"IndividualTrusteeOrDirectorInd"`
	InstitutionalTrusteeInd        string `xml:"InstitutionalTrusteeInd"`
	OfficerInd                     string `xml:"OfficerInd"`
	KeyEmployeeInd                 string `xml:"KeyEmployeeInd"`
	HighestCompensatedEmployeeInd  string `xml:"HighestCompensatedEmployeeInd"`
	FormerOfcrDirectorTrusteeInd   string `xml:"FormerOfcrDirectorTrusteeInd"`
	ReportableCompFromOrgAmt       string `xml:"ReportableCompFromOrgAmt"`
	ReportableCompFromRltdOrgAmt   string `xml:"ReportableCompFromRltdOrgAmt"`
	OtherCompensationAmt           string `xml:"OtherCompensationAmt"`
}

// ── Part VII-B ────────────────────────────────────────────────────────────────

type contractorComp struct {
	ContractorName    contractorName    `xml:"ContractorName"`
	ContractorAddress contractorAddress `xml:"ContractorAddress"`
	ServicesDesc      string            `xml:"ServicesDesc"`
	CompensationAmt   string            `xml:"CompensationAmt"`
}

// contractorName handles two schema variants:
//
//	Modern (2017v2+): ContractorName/BusinessName/BusinessNameLine1Txt
//	Older:            ContractorName/BusinessNameLine1Txt
type contractorName struct {
	BusinessName         contractorBusinessName `xml:"BusinessName"`
	BusinessNameLine1Txt string                 `xml:"BusinessNameLine1Txt"` // older fallback
}

type contractorBusinessName struct {
	BusinessNameLine1Txt string `xml:"BusinessNameLine1Txt"`
}

type contractorAddress struct {
	USAddress usAddress `xml:"USAddress"`
}

// ── Schedule J ────────────────────────────────────────────────────────────────

type scheduleJ struct {
	Persons []personComp `xml:"RltdOrgOfficerTrstKeyEmplGrp"`
}

type personComp struct {
	PersonNm                         string `xml:"PersonNm"`
	TitleTxt                         string `xml:"TitleTxt"`
	AverageHoursPerWeekRt            string `xml:"AverageHoursPerWeekRt"`
	ReportableCompFromOrgAmt         string `xml:"ReportableCompFromOrgAmt"`
	ReportableCompFromRltdOrgAmt     string `xml:"ReportableCompFromRltdOrgAmt"`
	OtherCompensationAmt             string `xml:"OtherCompensationAmt"`
	BaseCompensationFilingOrgAmt     string `xml:"BaseCompensationFilingOrgAmt"`
	BonusFilingOrganizationAmount    string `xml:"BonusFilingOrganizationAmount"`
	OtherCompensationFilingOrgAmt    string `xml:"OtherCompensationFilingOrgAmt"`
	DeferredCompensationFilingOrgAmt string `xml:"DeferredCompensationFilingOrgAmt"`
	NontaxableBenefitsFilingOrgAmt   string `xml:"NontaxableBenefitsFilingOrgAmt"`
	TotalCompensationFilingOrgAmt    string `xml:"TotalCompensationFilingOrgAmt"`
	CompReportedOtherOrgAmt          string `xml:"CompReportedOtherOrgAmt"`
}

// ── Schedule I ────────────────────────────────────────────────────────────────

type scheduleI struct {
	// Modern schema name for US org grants (Part II of Schedule I)
	GrantsOrgUS []grantRecipient `xml:"GrantsOtherAsstToOrgsInUSGrp"`
	// Alternate/older element names
	RecipientTable []grantRecipient `xml:"RecipientTable"`
}

type grantRecipient struct {
	RecipientBusinessName recipientBusinessName `xml:"RecipientBusinessName"`
	RecipientPersonNm     string                `xml:"RecipientPersonNm"` // individual recipient
	RecipientEIN          string                `xml:"RecipientEIN"`
	// Address field name varies by schema version
	RecipientUSAddress usAddress `xml:"RecipientUSAddress"`
	USAddress          usAddress `xml:"USAddress"`
	AddressUS          usAddress `xml:"AddressUS"`
	IRCSectionDesc     string    `xml:"IRCSectionDesc"`
	CashGrantAmt       string    `xml:"CashGrantAmt"`
	NonCashAssistanceAmt string  `xml:"NonCashAssistanceAmt"`
	PurposeOfGrantTxt  string    `xml:"PurposeOfGrantTxt"`
}

type recipientBusinessName struct {
	BusinessNameLine1Txt string `xml:"BusinessNameLine1Txt"`
}

// ─── Result ───────────────────────────────────────────────────────────────────

// Result is the full output of parsing one 990 XML file.
type Result struct {
	City          string
	State         string
	GrossReceipts int64
	WebsiteURL    string
	// nil slices / nil pointers mean the section was absent in the filing.
	ScheduleJRecords []*models.CompensationRecord
	Revenue          *models.RevenueRecord
	Expenses         *models.ExpensesRecord
	BalanceSheet     *models.BalanceSheetRecord
	Employees        []*models.EmployeeCompRecord
	Contractors      []*models.ContractorRecord
	Grants           []*models.GrantPaidRecord
}

// ─── Parse ────────────────────────────────────────────────────────────────────

// Parse extracts all available financial data from raw IRS 990 XML.
func Parse(data []byte, objectID, companyName, bmfCity, bmfState, nteeCode string) (*Result, error) {
	var doc return990
	if err := xml.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("xml unmarshal: %w", err)
	}

	city := bmfCity
	state := bmfState
	if city == "" {
		city = strings.TrimSpace(doc.ReturnHeader.Filer.USAddress.City)
	}
	if state == "" {
		state = strings.TrimSpace(doc.ReturnHeader.Filer.USAddress.State)
	}

	taxYear, _ := strconv.Atoi(strings.TrimSpace(doc.ReturnHeader.TaxYear))
	ein := strings.TrimSpace(doc.ReturnHeader.Filer.EIN)

	result := &Result{City: city, State: state}

	// ── Schedule J ────────────────────────────────────────────────────────
	if doc.ReturnData.ScheduleJ != nil {
		for _, p := range doc.ReturnData.ScheduleJ.Persons {
			name := strings.TrimSpace(p.PersonNm)
			if name == "" {
				continue
			}
			result.ScheduleJRecords = append(result.ScheduleJRecords, &models.CompensationRecord{
				ObjectID:                     objectID,
				EIN:                          ein,
				TaxYear:                      taxYear,
				CompanyName:                  companyName,
				City:                         city,
				State:                        state,
				NTEECode:                     nteeCode,
				PersonName:                   name,
				Title:                        strings.TrimSpace(p.TitleTxt),
				AvgHoursPerWeek:              parseFloat(p.AverageHoursPerWeekRt),
				ReportableCompFromOrg:        parseInt(p.ReportableCompFromOrgAmt),
				ReportableCompFromRelatedOrg: parseInt(p.ReportableCompFromRltdOrgAmt),
				OtherCompensation:            parseInt(p.OtherCompensationAmt),
				BaseCompensation:             parseInt(p.BaseCompensationFilingOrgAmt),
				Bonus:                        parseInt(p.BonusFilingOrganizationAmount),
				OtherCompFilingOrg:           parseInt(p.OtherCompensationFilingOrgAmt),
				DeferredComp:                 parseInt(p.DeferredCompensationFilingOrgAmt),
				NontaxableBenefits:           parseInt(p.NontaxableBenefitsFilingOrgAmt),
				TotalCompFilingOrg:           parseInt(p.TotalCompensationFilingOrgAmt),
				CompReportedOtherOrg:         parseInt(p.CompReportedOtherOrgAmt),
			})
		}
	}

	f := doc.ReturnData.IRS990
	if f == nil {
		// 990EZ or other variant — no IRS990 element
		return result, nil
	}

	result.GrossReceipts = parseInt(f.GrossReceiptsAmt)
	result.WebsiteURL = strings.TrimSpace(f.WebsiteAddressTxt)

	// ── Part VII-A: Officers / Employees ──────────────────────────────────
	for _, o := range f.Officers {
		name := strings.TrimSpace(o.PersonNm)
		if name == "" {
			continue
		}
		result.Employees = append(result.Employees, &models.EmployeeCompRecord{
			ObjectID:                  objectID,
			EIN:                       ein,
			TaxYear:                   taxYear,
			PersonName:                name,
			Title:                     strings.TrimSpace(o.TitleTxt),
			AvgHoursPerWeek:           parseFloat(o.AverageHoursPerWeekRt),
			IsTrusteeOrDirector:       o.IndividualTrusteeOrDirectorInd != "",
			IsInstitutionalTrustee:    o.InstitutionalTrusteeInd != "",
			IsOfficer:                 o.OfficerInd != "",
			IsKeyEmployee:             o.KeyEmployeeInd != "",
			IsHighestCompensated:      o.HighestCompensatedEmployeeInd != "",
			IsFormer:                  o.FormerOfcrDirectorTrusteeInd != "",
			ReportableCompFromOrg:     parseInt(o.ReportableCompFromOrgAmt),
			ReportableCompFromRelated: parseInt(o.ReportableCompFromRltdOrgAmt),
			OtherCompensation:         parseInt(o.OtherCompensationAmt),
		})
	}

	// ── Part VII-B: Contractors ───────────────────────────────────────────
	for _, c := range f.Contractors {
		// Modern schema: ContractorName/BusinessName/BusinessNameLine1Txt
		name := strings.TrimSpace(c.ContractorName.BusinessName.BusinessNameLine1Txt)
		// Older schema fallback: ContractorName/BusinessNameLine1Txt
		if name == "" {
			name = strings.TrimSpace(c.ContractorName.BusinessNameLine1Txt)
		}
		if name == "" {
			continue
		}
		addr := c.ContractorAddress.USAddress
		result.Contractors = append(result.Contractors, &models.ContractorRecord{
			ObjectID:        objectID,
			EIN:             ein,
			TaxYear:         taxYear,
			ContractorName:  name,
			City:            strings.TrimSpace(addr.City),
			State:           strings.TrimSpace(addr.State),
			ServicesDesc:    strings.TrimSpace(c.ServicesDesc),
			CompensationAmt: parseInt(c.CompensationAmt),
		})
	}

	// ── Part VIII: Revenue ────────────────────────────────────────────────
	// Total revenue: use CYTotalRevenueAmt (Part I summary, reliable across versions).
	// Fall back to TotalRevenueGrp/TotalRevenueColumnAmt if absent.
	totalRev := parseInt(f.CYTotalRevenueAmt)
	if totalRev == 0 {
		totalRev = parseInt(f.TotalRevenueGrp.TotalRevenueColumnAmt)
	}

	// Investment income: use group value, fall back to CY summary field.
	invInc := parseInt(f.InvestmentIncomeGrp.TotalRevenueColumnAmt)
	if invInc == 0 {
		invInc = parseInt(f.CYInvestmentIncomeAmt)
	}

	// Program service revenue: TotalProgramServiceRevenueAmt is the correct flat tag.
	result.Revenue = &models.RevenueRecord{
		ObjectID:                  objectID,
		EIN:                       ein,
		TaxYear:                   taxYear,
		FederatedCampaignsAmt:     parseInt(f.FederatedCampaignsAmt),
		MembershipDuesAmt:         parseInt(f.MembershipDuesAmt),
		FundraisingAmt:            parseInt(f.FundraisingAmt),
		RelatedOrganizationsAmt:   parseInt(f.RelatedOrganizationsAmt),
		GovernmentGrantsAmt:       parseInt(f.GovernmentGrantsAmt),
		AllOtherContributionsAmt:  parseInt(f.AllOtherContributionsAmt),
		NoncashContributionsAmt:   parseInt(f.NoncashContributionsAmt),
		TotalContributionsAmt:     parseInt(f.TotalContributionsAmt),
		TotalProgramServiceRevAmt: parseInt(f.TotalProgramServiceRevenueAmt),
		InvestmentIncomeAmt:       invInc,
		BondProceedsAmt:           parseInt(f.TaxExemptBondProceedsAmt),
		NetRentalIncomeLossAmt:    parseInt(f.NetRentalIncomeOrLossGrp.TotalRevenueColumnAmt),
		NetGainLossInvestmentsAmt: parseInt(f.NetGainLossInvestmentsGrp.TotalRevenueColumnAmt),
		FundraisingNetIncomeAmt:   parseInt(f.NetIncmFromFundraisingEvtGrp.TotalRevenueColumnAmt),
		GamingNetIncomeAmt:        parseInt(f.NetIncomeFromGamingGrp.TotalRevenueColumnAmt),
		NetIncomeFromInventoryAmt: 0, // rarely relevant for homeless services orgs
		TotalOtherRevenueAmt:      coalesceInt(parseInt(f.CYOtherRevenueAmt), parseInt(f.OtherRevenueTotalAmt)),
		TotalRevenueAmt:           totalRev,
	}

	// ── Part IX: Expenses ─────────────────────────────────────────────────
	result.Expenses = &models.ExpensesRecord{
		ObjectID:                    objectID,
		EIN:                         ein,
		TaxYear:                     taxYear,
		GrantsToOrgsAmt:             parseInt(f.GrantsAndOtherAssistToOrgsGrp.TotalAmt),
		GrantsToIndividualsAmt:      parseInt(f.GrantsAndOtherAssistToIndivGrp.TotalAmt),
		ForeignGrantsAmt:            parseInt(f.ForeignGrantsGrp.TotalAmt),
		BenefitsToMembersAmt:        parseInt(f.BenefitsToMembersGrp.TotalAmt),
		CompCurrentOfficersAmt:      parseInt(f.CompCurrentOfcrDirectorsGrp.TotalAmt),
		CompDisqualifiedPersonsAmt:  parseInt(f.CompDisqualifiedPersonsGrp.TotalAmt),
		OtherSalariesWagesAmt:       parseInt(f.OtherSalariesAndWagesGrp.TotalAmt),
		PensionPlanContribsAmt:      parseInt(f.PensionPlanContributionsGrp.TotalAmt),
		OtherEmployeeBenefitsAmt:    parseInt(f.OtherEmployeeBenefitsGrp.TotalAmt),
		PayrollTaxesAmt:             parseInt(f.PayrollTaxesGrp.TotalAmt),
		FeesManagementAmt:           parseInt(f.FeesForServicesManagementGrp.TotalAmt),
		FeesLegalAmt:                parseInt(f.FeesForServicesLegalGrp.TotalAmt),
		FeesAccountingAmt:           parseInt(f.FeesForServicesAccountingGrp.TotalAmt),
		FeesLobbyingAmt:             parseInt(f.FeesForServicesLobbyingGrp.TotalAmt),
		FeesProfessionalFundraising: parseInt(f.FeesForServicesProfFundraisGrp.TotalAmt),
		FeesInvestmentMgmtAmt:       parseInt(f.FeesForServicesInvestMgmtGrp.TotalAmt),
		FeesOtherAmt:                parseInt(f.FeesForServicesOtherGrp.TotalAmt),
		AdvertisingAmt:              parseInt(f.AdvertisingGrp.TotalAmt),
		OfficeExpensesAmt:           parseInt(f.OfficeExpensesGrp.TotalAmt),
		InformationTechnologyAmt:    parseInt(f.InformationTechnologyGrp.TotalAmt),
		OccupancyAmt:                parseInt(f.OccupancyGrp.TotalAmt),
		TravelAmt:                   parseInt(f.TravelGrp.TotalAmt),
		ConferencesMeetingsAmt:      parseInt(f.ConferencesMeetingsGrp.TotalAmt),
		InterestAmt:                 parseInt(f.InterestGrp.TotalAmt),
		DepreciationDepletionAmt:    parseInt(f.DepreciationDepletionGrp.TotalAmt),
		InsuranceAmt:                parseInt(f.InsuranceGrp.TotalAmt),
		AllOtherExpensesAmt:         parseInt(f.AllOtherExpensesGrp.TotalAmt),
		TotalFunctionalExpensesAmt:  parseInt(f.TotalFunctionalExpensesGrp.TotalAmt),
	}

	// ── Part X: Balance Sheet ─────────────────────────────────────────────
	result.BalanceSheet = &models.BalanceSheetRecord{
		ObjectID:                      objectID,
		EIN:                           ein,
		TaxYear:                       taxYear,
		CashNonInterestBearingAmt:     eoyAmt(f.CashNonInterestBearingGrp),
		SavingsTempCashInvestAmt:      eoyAmt(f.SavingsAndTempCashInvstGrp),
		PledgesGrantsReceivableAmt:    eoyAmt(f.PledgesAndGrantsReceivableGrp),
		AccountsReceivableAmt:         eoyAmt(f.AccountsReceivableGrp),
		ReceivablesOfficersAmt:        eoyAmt(f.ReceivablesFromOfficersGrp),
		OtherNotesLoansRecvAmt:        eoyAmt(f.OtherNotesLoansReceivableNetGrp),
		InventoriesAmt:                eoyAmt(f.InventoriesForSaleOrUseGrp),
		PrepaidExpensesAmt:            eoyAmt(f.PrepaidExpensesGrp),
		LandBldgEquipNetAmt:           eoyAmt(f.LandBldgEquipNetBookValueGrp),
		InvestmentsPubliclyTradedAmt:  eoyAmt(f.InvestmentsPubTradedSecGrp),
		InvestmentsOtherSecuritiesAmt: eoyAmt(f.InvestmentsOtherSecuritiesGrp),
		InvestmentsProgramRelatedAmt:  eoyAmt(f.InvestmentsProgramRelatedGrp),
		IntangibleAssetsNetAmt:        eoyAmt(f.IntangibleAssetsNetGrp),
		OtherAssetsTotalAmt:           eoyAmt(f.OtherAssetsTotalGrp),
		TotalAssetsAmt:                eoyAmt(f.TotalAssetsGrp),
		AccountsPayableAccruedAmt:     eoyAmt(f.AccountsPayableAccruedExpGrp),
		GrantsPayableAmt:              eoyAmt(f.GrantsPayableGrp),
		DeferredRevenueAmt:            eoyAmt(f.DeferredRevenueGrp),
		TaxExemptBondLiabAmt:          eoyAmt(f.TaxExemptBondLiabilitiesGrp),
		LoansFromOfficersAmt:          eoyAmt(f.LoansFromOfficersGrp),
		MortgagesNotesPayableAmt:      eoyAmt(f.MortgagesNotesPayableLongGrp),
		OtherLiabilitiesAmt:           eoyAmt(f.OtherLiabilitiesGrp),
		TotalLiabilitiesAmt:           eoyAmt(f.TotalLiabilitiesGrp),
		UnrestrictedNetAssetsAmt:      eoyAmt(f.UnrestrictedNetAssetsGrp),
		TempRestrictedNetAssetsAmt:    eoyAmt(f.TemporarilyRstrNetAssetsGrp),
		PermRestrictedNetAssetsAmt:    eoyAmt(f.PermanentlyRstrNetAssetsGrp),
		NetAssetsWithoutDonorRestr:    eoyAmt(f.NetAssetsWODonorRestrictionsGrp),
		NetAssetsWithDonorRestr:       eoyAmt(f.NetAssetsWithDonorRestrictionsGrp),
		TotalNetAssetsAmt:             eoyAmt(f.TotalNetAssetsFundBalanceGrp),
	}

	// ── Schedule I: Grants Paid ───────────────────────────────────────────
	if doc.ReturnData.ScheduleI != nil {
		allGrants := append(doc.ReturnData.ScheduleI.GrantsOrgUS,
			doc.ReturnData.ScheduleI.RecipientTable...)

		for _, g := range allGrants {
			name := strings.TrimSpace(g.RecipientBusinessName.BusinessNameLine1Txt)
			if name == "" {
				name = strings.TrimSpace(g.RecipientPersonNm)
			}
			if name == "" {
				continue
			}
			// Resolve address from whichever field the schema version uses.
			addr := g.RecipientUSAddress
			if addr.City == "" {
				addr = g.USAddress
			}
			if addr.City == "" {
				addr = g.AddressUS
			}
			result.Grants = append(result.Grants, &models.GrantPaidRecord{
				ObjectID:          objectID,
				EIN:               ein,
				TaxYear:           taxYear,
				RecipientName:     name,
				RecipientEIN:      strings.TrimSpace(g.RecipientEIN),
				RecipientCity:     strings.TrimSpace(addr.City),
				RecipientState:    strings.TrimSpace(addr.State),
				IRCSectionDesc:    strings.TrimSpace(g.IRCSectionDesc),
				CashGrantAmt:      parseInt(g.CashGrantAmt),
				NoncashAssistAmt:  parseInt(g.NonCashAssistanceAmt),
				PurposeOfGrantTxt: strings.TrimSpace(g.PurposeOfGrantTxt),
			})
		}
	}

	return result, nil
}

// ─── helpers ─────────────────────────────────────────────────────────────────

func eoyAmt(g amtGrp) int64     { return parseInt(g.EOYAmt) }

// coalesceInt returns the first non-zero value.
func coalesceInt(vals ...int64) int64 {
	for _, v := range vals {
		if v != 0 {
			return v
		}
	}
	return 0
}

func parseInt(s string) int64 {
	v, _ := strconv.ParseInt(strings.TrimSpace(s), 10, 64)
	return v
}

func parseFloat(s string) float64 {
	v, _ := strconv.ParseFloat(strings.TrimSpace(s), 64)
	return v
}
