package models

// IndexEntry is one row from the IRS annual filing index CSV.
// Column names match the new apps.irs.gov format (post-2021).
//
//	RETURN_ID, FILING_TYPE, EIN, TAX_PERIOD, SUB_DATE,
//	TAXPAYER_NAME, RETURN_TYPE, DLN, OBJECT_ID
type IndexEntry struct {
	ReturnID    string // RETURN_ID
	FilingType  string // FILING_TYPE
	EIN         string // EIN
	TaxPeriod   string // TAX_PERIOD
	SubmittedOn string // SUB_DATE
	CompanyName string // TAXPAYER_NAME
	FormType    string // RETURN_TYPE  (990, 990EZ, 990PF, etc.)
	DLN         string // DLN
	ObjectID    string // OBJECT_ID — used to match files inside ZIP archives
}

// OrgRecord is the canonical record for one EIN, seeded from BMF and
// enriched with XML data during filing processing.
type OrgRecord struct {
	EIN                string
	Name               string
	City               string
	State              string
	NTEECode           string
	IRSSubsectionCode  string
	RulingDate         string // YYYYMM
	TaxPeriod          string // most recent tax_period
	WebsiteURL         string
	GrossReceipts      int64
}

// CompensationRecord holds one person's Schedule J data from a 990 filing,
// enriched with geographic and NTEE metadata from the BMF.
type CompensationRecord struct {
	ObjectID                     string
	EIN                          string
	TaxYear                      int
	CompanyName                  string
	City                         string
	State                        string
	NTEECode                     string
	PersonName                   string
	Title                        string
	AvgHoursPerWeek              float64
	ReportableCompFromOrg        int64
	ReportableCompFromRelatedOrg int64
	OtherCompensation            int64
	BaseCompensation             int64
	Bonus                        int64
	OtherCompFilingOrg           int64
	DeferredComp                 int64
	NontaxableBenefits           int64
	TotalCompFilingOrg           int64
	CompReportedOtherOrg         int64
}

// RevenueRecord holds Part VIII (Statement of Revenue) data for one filing.
type RevenueRecord struct {
	ObjectID                   string
	EIN                        string
	TaxYear                    int
	FederatedCampaignsAmt      int64
	MembershipDuesAmt          int64
	FundraisingAmt             int64
	RelatedOrganizationsAmt    int64
	GovernmentGrantsAmt        int64
	AllOtherContributionsAmt   int64
	NoncashContributionsAmt    int64
	TotalContributionsAmt      int64
	TotalProgramServiceRevAmt  int64
	InvestmentIncomeAmt        int64
	BondProceedsAmt            int64
	RoyaltiesRevenueAmt        int64
	NetRentalIncomeLossAmt     int64
	NetGainLossInvestmentsAmt  int64
	FundraisingNetIncomeAmt    int64
	GamingNetIncomeAmt         int64
	NetIncomeFromInventoryAmt  int64
	TotalOtherRevenueAmt       int64
	TotalRevenueAmt            int64
}

// ExpensesRecord holds Part IX (Statement of Functional Expenses) for one filing.
type ExpensesRecord struct {
	ObjectID                     string
	EIN                          string
	TaxYear                      int
	GrantsToOrgsAmt              int64
	GrantsToIndividualsAmt       int64
	ForeignGrantsAmt             int64
	BenefitsToMembersAmt         int64
	CompCurrentOfficersAmt       int64
	CompDisqualifiedPersonsAmt   int64
	OtherSalariesWagesAmt        int64
	PensionPlanContribsAmt       int64
	OtherEmployeeBenefitsAmt     int64
	PayrollTaxesAmt              int64
	FeesManagementAmt            int64
	FeesLegalAmt                 int64
	FeesAccountingAmt            int64
	FeesLobbyingAmt              int64
	FeesProfessionalFundraising  int64
	FeesInvestmentMgmtAmt        int64
	FeesOtherAmt                 int64
	AdvertisingAmt               int64
	OfficeExpensesAmt            int64
	InformationTechnologyAmt     int64
	RoyaltiesAmt                 int64
	OccupancyAmt                 int64
	TravelAmt                    int64
	PaymentsTravelEntertainAmt   int64
	ConferencesMeetingsAmt       int64
	InterestAmt                  int64
	PaymentsToAffiliatesAmt      int64
	DepreciationDepletionAmt     int64
	InsuranceAmt                 int64
	AllOtherExpensesAmt          int64
	TotalFunctionalExpensesAmt   int64
}

// BalanceSheetRecord holds Part X (Balance Sheet) end-of-year amounts for one filing.
type BalanceSheetRecord struct {
	ObjectID                      string
	EIN                           string
	TaxYear                       int
	// Assets
	CashNonInterestBearingAmt     int64
	SavingsTempCashInvestAmt      int64
	PledgesGrantsReceivableAmt    int64
	AccountsReceivableAmt         int64
	ReceivablesOfficersAmt        int64
	OtherNotesLoansRecvAmt        int64
	InventoriesAmt                int64
	PrepaidExpensesAmt            int64
	LandBldgEquipNetAmt           int64
	InvestmentsPubliclyTradedAmt  int64
	InvestmentsOtherSecuritiesAmt int64
	InvestmentsProgramRelatedAmt  int64
	IntangibleAssetsNetAmt        int64
	OtherAssetsTotalAmt           int64
	TotalAssetsAmt                int64
	// Liabilities
	AccountsPayableAccruedAmt     int64
	GrantsPayableAmt              int64
	DeferredRevenueAmt            int64
	TaxExemptBondLiabAmt          int64
	LoansFromOfficersAmt          int64
	MortgagesNotesPayableAmt      int64
	OtherLiabilitiesAmt           int64
	TotalLiabilitiesAmt           int64
	// Net assets — pre-ASU 2016 (older filings)
	UnrestrictedNetAssetsAmt      int64
	TempRestrictedNetAssetsAmt    int64
	PermRestrictedNetAssetsAmt    int64
	// Net assets — post-ASU 2016 (2018+ tax years)
	NetAssetsWithoutDonorRestr    int64
	NetAssetsWithDonorRestr       int64
	TotalNetAssetsAmt             int64
}

// EmployeeCompRecord is one row from Part VII-A (Officers, Directors, Trustees,
// Key Employees) for a single filing.
type EmployeeCompRecord struct {
	ObjectID                  string
	EIN                       string
	TaxYear                   int
	PersonName                string
	Title                     string
	AvgHoursPerWeek           float64
	IsTrusteeOrDirector       bool
	IsInstitutionalTrustee    bool
	IsOfficer                 bool
	IsKeyEmployee             bool
	IsHighestCompensated      bool
	IsFormer                  bool
	ReportableCompFromOrg     int64
	ReportableCompFromRelated int64
	OtherCompensation         int64
}

// ContractorRecord is one row from Part VII-B (Independent Contractors)
// for a single filing.
type ContractorRecord struct {
	ObjectID        string
	EIN             string
	TaxYear         int
	ContractorName  string
	City            string
	State           string
	ServicesDesc    string
	CompensationAmt int64
}

// GrantPaidRecord is one row from Schedule I (Grants and Other Assistance)
// for a single filing.
type GrantPaidRecord struct {
	ObjectID            string
	EIN                 string
	TaxYear             int
	RecipientName       string
	RecipientEIN        string
	RecipientCity       string
	RecipientState      string
	IRCSectionDesc      string
	CashGrantAmt        int64
	NoncashAssistAmt    int64
	PurposeOfGrantTxt   string
}
