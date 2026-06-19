package db

import (
	"context"
	"fmt"

	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"

	"github.com/BatonVatrushka/irs_990_xml_parse/internal/bmf"
	"github.com/BatonVatrushka/irs_990_xml_parse/internal/models"
)

// ─── filing_index helpers ─────────────────────────────────────────────────────

// GetProcessedIDs returns the subset of objectIDs that have already been fully
// processed (has_full_data = TRUE). Used to bulk-skip filings before ZIP downloads.
func GetProcessedIDs(ctx context.Context, pool *pgxpool.Pool, objectIDs map[string]struct{}) (map[string]struct{}, error) {
	if len(objectIDs) == 0 {
		return map[string]struct{}{}, nil
	}

	ids := make([]string, 0, len(objectIDs))
	for id := range objectIDs {
		ids = append(ids, id)
	}

	rows, err := pool.Query(ctx,
		`SELECT object_id FROM filing_index
		 WHERE object_id = ANY($1) AND has_full_data = TRUE`,
		ids,
	)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	done := make(map[string]struct{})
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, err
		}
		done[id] = struct{}{}
	}
	return done, rows.Err()
}

// MarkProcessed records a filing as handled.
// hasFullData = true means all financial tables were populated successfully.
// On error, hasFullData should be false so the filing is retried next run.
func MarkProcessed(
	ctx context.Context,
	pool *pgxpool.Pool,
	entry models.IndexEntry,
	zipURL string,
	filingYear int,
	city, state, nteeCode string,
	hasScheduleJ bool,
	hasFullData bool,
	errMsg string,
) error {
	_, err := pool.Exec(ctx, `
		INSERT INTO filing_index
			(object_id, ein, tax_period, form_type, company_name, filing_year, url,
			 city, state, ntee_code, processed_at, has_schedule_j, has_full_data, error_msg)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, NOW(), $11, $12, NULLIF($13, ''))
		ON CONFLICT (object_id) DO UPDATE
			SET processed_at   = NOW(),
			    city           = EXCLUDED.city,
			    state          = EXCLUDED.state,
			    ntee_code      = EXCLUDED.ntee_code,
			    has_schedule_j = EXCLUDED.has_schedule_j,
			    has_full_data  = EXCLUDED.has_full_data,
			    error_msg      = EXCLUDED.error_msg
	`, entry.ObjectID, entry.EIN, entry.TaxPeriod, entry.FormType,
		entry.CompanyName, filingYear, zipURL,
		city, state, nteeCode,
		hasScheduleJ, hasFullData, errMsg)
	return err
}

// DeleteFilingData removes all previously-inserted rows for objectID from every
// financial detail table. Call this before re-inserting to keep the pipeline
// idempotent when reprocessing filings that had has_full_data = FALSE.
func DeleteFilingData(ctx context.Context, pool *pgxpool.Pool, objectID string) error {
	tables := []string{
		"schedule_j_compensation",
		"revenue",
		"expenses",
		"balance_sheet",
		"employee_compensation",
		"contractors",
		"grants_paid",
	}
	for _, t := range tables {
		if _, err := pool.Exec(ctx,
			fmt.Sprintf(`DELETE FROM %s WHERE object_id = $1`, t),
			objectID,
		); err != nil {
			return fmt.Errorf("delete from %s: %w", t, err)
		}
	}
	return nil
}

// ─── organizations ────────────────────────────────────────────────────────────

// BulkUpsertOrgs seeds the organizations table from the BMF allowlist.
// Uses pgx.Batch so we stay within the pool (no raw connection needed).
// Runs in chunks of 500 to stay under pgx's pipeline limits.
func BulkUpsertOrgs(ctx context.Context, pool *pgxpool.Pool, orgs map[string]bmf.OrgInfo) error {
	const chunkSize = 500

	orgSlice := make([]bmf.OrgInfo, 0, len(orgs))
	for _, o := range orgs {
		orgSlice = append(orgSlice, o)
	}

	const q = `
		INSERT INTO organizations
			(ein, name, city, state, ntee_code, irs_subsection_code, ruling_date)
		VALUES ($1, $2, $3, $4, $5, $6, $7)
		ON CONFLICT (ein) DO UPDATE SET
			name                = EXCLUDED.name,
			city                = EXCLUDED.city,
			state               = EXCLUDED.state,
			ntee_code           = EXCLUDED.ntee_code,
			irs_subsection_code = EXCLUDED.irs_subsection_code,
			ruling_date         = EXCLUDED.ruling_date,
			updated_at          = NOW()`

	for i := 0; i < len(orgSlice); i += chunkSize {
		end := i + chunkSize
		if end > len(orgSlice) {
			end = len(orgSlice)
		}
		chunk := orgSlice[i:end]

		batch := &pgx.Batch{}
		for _, org := range chunk {
			batch.Queue(q,
				org.EIN, org.Name, org.City, org.State,
				org.NTEECode, org.IRSSubsectionCode, org.RulingDate,
			)
		}
		br := pool.SendBatch(ctx, batch)
		for range chunk {
			if _, err := br.Exec(); err != nil {
				br.Close()
				return fmt.Errorf("bulk upsert orgs: %w", err)
			}
		}
		if err := br.Close(); err != nil {
			return err
		}
	}
	return nil
}

// UpsertOrg enriches one organization record with fields extracted from the XML
// (gross receipts, website URL, most recent tax period).
func UpsertOrg(ctx context.Context, pool *pgxpool.Pool, rec *models.OrgRecord) error {
	_, err := pool.Exec(ctx, `
		INSERT INTO organizations
			(ein, name, city, state, ntee_code, irs_subsection_code, ruling_date,
			 tax_period, website_url, gross_receipts)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, NULLIF($9,''), NULLIF($10, 0))
		ON CONFLICT (ein) DO UPDATE SET
			tax_period     = EXCLUDED.tax_period,
			website_url    = COALESCE(NULLIF(EXCLUDED.website_url, ''), organizations.website_url),
			gross_receipts = COALESCE(NULLIF(EXCLUDED.gross_receipts, 0), organizations.gross_receipts),
			updated_at     = NOW()
	`, rec.EIN, rec.Name, rec.City, rec.State, rec.NTEECode,
		rec.IRSSubsectionCode, rec.RulingDate,
		rec.TaxPeriod, rec.WebsiteURL, rec.GrossReceipts,
	)
	return err
}

// ─── schedule_j_compensation ─────────────────────────────────────────────────

// InsertCompensation batch-inserts Schedule J records using COPY for speed.
func InsertCompensation(ctx context.Context, pool *pgxpool.Pool, records []*models.CompensationRecord) error {
	if len(records) == 0 {
		return nil
	}

	// Deduplicate by (person_name, title) to match the UNIQUE constraint.
	seen := make(map[string]struct{}, len(records))
	deduped := make([]*models.CompensationRecord, 0, len(records))
	for _, r := range records {
		key := r.PersonName + "\x00" + r.Title
		if _, ok := seen[key]; !ok {
			seen[key] = struct{}{}
			deduped = append(deduped, r)
		}
	}
	records = deduped

	rows := make([][]any, 0, len(records))
	for _, r := range records {
		rows = append(rows, []any{
			r.ObjectID, r.EIN, r.TaxYear, r.CompanyName,
			r.City, r.State, r.NTEECode,
			r.PersonName, r.Title, r.AvgHoursPerWeek,
			r.ReportableCompFromOrg, r.ReportableCompFromRelatedOrg,
			r.OtherCompensation, r.BaseCompensation, r.Bonus,
			r.OtherCompFilingOrg, r.DeferredComp, r.NontaxableBenefits,
			r.TotalCompFilingOrg, r.CompReportedOtherOrg,
		})
	}

	cols := []string{
		"object_id", "ein", "tax_year", "company_name",
		"city", "state", "ntee_code",
		"person_name", "title", "avg_hours_per_week",
		"reportable_comp_from_org", "reportable_comp_from_related_org",
		"other_compensation", "base_compensation", "bonus",
		"other_comp_filing_org", "deferred_comp", "nontaxable_benefits",
		"total_comp_filing_org", "comp_reported_other_org",
	}

	_, err := pool.CopyFrom(ctx,
		pgx.Identifier{"schedule_j_compensation"},
		cols,
		pgx.CopyFromRows(rows),
	)
	if err != nil {
		return fmt.Errorf("copy schedule_j: %w", err)
	}
	return nil
}

// ─── revenue ─────────────────────────────────────────────────────────────────

func InsertRevenue(ctx context.Context, pool *pgxpool.Pool, r *models.RevenueRecord) error {
	if r == nil {
		return nil
	}
	_, err := pool.Exec(ctx, `
		INSERT INTO revenue (
			object_id, ein, tax_year,
			federated_campaigns_amt, membership_dues_amt, fundraising_amt,
			related_organizations_amt, government_grants_amt,
			all_other_contributions_amt, noncash_contributions_amt,
			total_contributions_amt, total_program_service_rev_amt,
			investment_income_amt, bond_proceeds_amt, royalties_revenue_amt,
			net_rental_income_loss_amt, net_gain_loss_investments_amt,
			fundraising_net_income_amt, gaming_net_income_amt,
			net_income_from_inventory_amt, total_other_revenue_amt, total_revenue_amt
		) VALUES (
			$1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$20,$21,$22
		)`,
		r.ObjectID, r.EIN, r.TaxYear,
		r.FederatedCampaignsAmt, r.MembershipDuesAmt, r.FundraisingAmt,
		r.RelatedOrganizationsAmt, r.GovernmentGrantsAmt,
		r.AllOtherContributionsAmt, r.NoncashContributionsAmt,
		r.TotalContributionsAmt, r.TotalProgramServiceRevAmt,
		r.InvestmentIncomeAmt, r.BondProceedsAmt, r.RoyaltiesRevenueAmt,
		r.NetRentalIncomeLossAmt, r.NetGainLossInvestmentsAmt,
		r.FundraisingNetIncomeAmt, r.GamingNetIncomeAmt,
		r.NetIncomeFromInventoryAmt, r.TotalOtherRevenueAmt, r.TotalRevenueAmt,
	)
	return err
}

// ─── expenses ─────────────────────────────────────────────────────────────────

func InsertExpenses(ctx context.Context, pool *pgxpool.Pool, e *models.ExpensesRecord) error {
	if e == nil {
		return nil
	}
	_, err := pool.Exec(ctx, `
		INSERT INTO expenses (
			object_id, ein, tax_year,
			grants_to_orgs_amt, grants_to_individuals_amt, foreign_grants_amt,
			benefits_to_members_amt, comp_current_officers_amt,
			comp_disqualified_persons_amt, other_salaries_wages_amt,
			pension_plan_contribs_amt, other_employee_benefits_amt,
			payroll_taxes_amt, fees_management_amt, fees_legal_amt,
			fees_accounting_amt, fees_lobbying_amt, fees_professional_fundraising,
			fees_investment_mgmt_amt, fees_other_amt, advertising_amt,
			office_expenses_amt, information_technology_amt, royalties_amt,
			occupancy_amt, travel_amt, payments_travel_entertain_amt,
			conferences_meetings_amt, interest_amt, payments_to_affiliates_amt,
			depreciation_depletion_amt, insurance_amt, all_other_expenses_amt,
			total_functional_expenses_amt
		) VALUES (
			$1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,
			$19,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$30,$31,$32,$33,$34
		)`,
		e.ObjectID, e.EIN, e.TaxYear,
		e.GrantsToOrgsAmt, e.GrantsToIndividualsAmt, e.ForeignGrantsAmt,
		e.BenefitsToMembersAmt, e.CompCurrentOfficersAmt,
		e.CompDisqualifiedPersonsAmt, e.OtherSalariesWagesAmt,
		e.PensionPlanContribsAmt, e.OtherEmployeeBenefitsAmt,
		e.PayrollTaxesAmt, e.FeesManagementAmt, e.FeesLegalAmt,
		e.FeesAccountingAmt, e.FeesLobbyingAmt, e.FeesProfessionalFundraising,
		e.FeesInvestmentMgmtAmt, e.FeesOtherAmt, e.AdvertisingAmt,
		e.OfficeExpensesAmt, e.InformationTechnologyAmt, e.RoyaltiesAmt,
		e.OccupancyAmt, e.TravelAmt, e.PaymentsTravelEntertainAmt,
		e.ConferencesMeetingsAmt, e.InterestAmt, e.PaymentsToAffiliatesAmt,
		e.DepreciationDepletionAmt, e.InsuranceAmt, e.AllOtherExpensesAmt,
		e.TotalFunctionalExpensesAmt,
	)
	return err
}

// ─── balance_sheet ────────────────────────────────────────────────────────────

func InsertBalanceSheet(ctx context.Context, pool *pgxpool.Pool, b *models.BalanceSheetRecord) error {
	if b == nil {
		return nil
	}
	_, err := pool.Exec(ctx, `
		INSERT INTO balance_sheet (
			object_id, ein, tax_year,
			cash_non_interest_bearing_amt, savings_temp_cash_invest_amt,
			pledges_grants_receivable_amt, accounts_receivable_amt,
			receivables_officers_amt, other_notes_loans_receiv_amt,
			inventories_amt, prepaid_expenses_amt, land_bldg_equip_net_amt,
			investments_publicly_traded_amt, investments_other_securities_amt,
			investments_program_related_amt, intangible_assets_net_amt,
			other_assets_total_amt, total_assets_amt,
			accounts_payable_accrued_amt, grants_payable_amt,
			deferred_revenue_amt, tax_exempt_bond_liab_amt,
			loans_from_officers_amt, mortgages_notes_payable_amt,
			other_liabilities_amt, total_liabilities_amt,
			unrestricted_net_assets_amt, temp_restricted_net_assets_amt,
			perm_restricted_net_assets_amt,
			net_assets_without_donor_restr, net_assets_with_donor_restr,
			total_net_assets_amt
		) VALUES (
			$1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,
			$19,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$30,$31,$32
		)`,
		b.ObjectID, b.EIN, b.TaxYear,
		b.CashNonInterestBearingAmt, b.SavingsTempCashInvestAmt,
		b.PledgesGrantsReceivableAmt, b.AccountsReceivableAmt,
		b.ReceivablesOfficersAmt, b.OtherNotesLoansRecvAmt,
		b.InventoriesAmt, b.PrepaidExpensesAmt, b.LandBldgEquipNetAmt,
		b.InvestmentsPubliclyTradedAmt, b.InvestmentsOtherSecuritiesAmt,
		b.InvestmentsProgramRelatedAmt, b.IntangibleAssetsNetAmt,
		b.OtherAssetsTotalAmt, b.TotalAssetsAmt,
		b.AccountsPayableAccruedAmt, b.GrantsPayableAmt,
		b.DeferredRevenueAmt, b.TaxExemptBondLiabAmt,
		b.LoansFromOfficersAmt, b.MortgagesNotesPayableAmt,
		b.OtherLiabilitiesAmt, b.TotalLiabilitiesAmt,
		b.UnrestrictedNetAssetsAmt, b.TempRestrictedNetAssetsAmt,
		b.PermRestrictedNetAssetsAmt,
		b.NetAssetsWithoutDonorRestr, b.NetAssetsWithDonorRestr,
		b.TotalNetAssetsAmt,
	)
	return err
}

// ─── employee_compensation ────────────────────────────────────────────────────

func InsertEmployeeComp(ctx context.Context, pool *pgxpool.Pool, records []*models.EmployeeCompRecord) error {
	if len(records) == 0 {
		return nil
	}

	// Deduplicate by (person_name, title) — some 990 XMLs list the same person
	// twice (role transition, amended filing artifact). The UNIQUE constraint
	// enforces this at the DB level; we deduplicate here so CopyFrom doesn't fail.
	seen := make(map[string]struct{}, len(records))
	deduped := make([]*models.EmployeeCompRecord, 0, len(records))
	for _, r := range records {
		key := r.PersonName + "\x00" + r.Title
		if _, ok := seen[key]; !ok {
			seen[key] = struct{}{}
			deduped = append(deduped, r)
		}
	}
	records = deduped

	rows := make([][]any, 0, len(records))
	for _, r := range records {
		rows = append(rows, []any{
			r.ObjectID, r.EIN, r.TaxYear,
			r.PersonName, r.Title, r.AvgHoursPerWeek,
			r.IsTrusteeOrDirector, r.IsInstitutionalTrustee,
			r.IsOfficer, r.IsKeyEmployee, r.IsHighestCompensated, r.IsFormer,
			r.ReportableCompFromOrg, r.ReportableCompFromRelated, r.OtherCompensation,
		})
	}

	_, err := pool.CopyFrom(ctx,
		pgx.Identifier{"employee_compensation"},
		[]string{
			"object_id", "ein", "tax_year",
			"person_name", "title", "avg_hours_per_week",
			"is_trustee_or_director", "is_institutional_trustee",
			"is_officer", "is_key_employee", "is_highest_compensated", "is_former",
			"reportable_comp_from_org", "reportable_comp_from_related", "other_compensation",
		},
		pgx.CopyFromRows(rows),
	)
	if err != nil {
		return fmt.Errorf("copy employee_comp: %w", err)
	}
	return nil
}

// ─── contractors ─────────────────────────────────────────────────────────────

func InsertContractors(ctx context.Context, pool *pgxpool.Pool, records []*models.ContractorRecord) error {
	if len(records) == 0 {
		return nil
	}

	rows := make([][]any, 0, len(records))
	for _, r := range records {
		rows = append(rows, []any{
			r.ObjectID, r.EIN, r.TaxYear,
			r.ContractorName, r.City, r.State,
			r.ServicesDesc, r.CompensationAmt,
		})
	}

	_, err := pool.CopyFrom(ctx,
		pgx.Identifier{"contractors"},
		[]string{
			"object_id", "ein", "tax_year",
			"contractor_name", "city", "state",
			"services_desc", "compensation_amt",
		},
		pgx.CopyFromRows(rows),
	)
	if err != nil {
		return fmt.Errorf("copy contractors: %w", err)
	}
	return nil
}

// ─── grants_paid ─────────────────────────────────────────────────────────────

func InsertGrantsPaid(ctx context.Context, pool *pgxpool.Pool, records []*models.GrantPaidRecord) error {
	if len(records) == 0 {
		return nil
	}

	rows := make([][]any, 0, len(records))
	for _, r := range records {
		recipEIN := r.RecipientEIN
		if recipEIN == "" {
			recipEIN = "" // CopyFrom will send NULL for empty string via pgx text protocol
		}
		rows = append(rows, []any{
			r.ObjectID, r.EIN, r.TaxYear,
			r.RecipientName, nullIfEmpty(r.RecipientEIN),
			r.RecipientCity, r.RecipientState,
			r.IRCSectionDesc, r.CashGrantAmt, r.NoncashAssistAmt,
			r.PurposeOfGrantTxt,
		})
	}

	_, err := pool.CopyFrom(ctx,
		pgx.Identifier{"grants_paid"},
		[]string{
			"object_id", "ein", "tax_year",
			"recipient_name", "recipient_ein",
			"recipient_city", "recipient_state",
			"irc_section_desc", "cash_grant_amt", "noncash_assistance_amt",
			"purpose_of_grant_txt",
		},
		pgx.CopyFromRows(rows),
	)
	if err != nil {
		return fmt.Errorf("copy grants_paid: %w", err)
	}
	return nil
}

// SetHasFullData updates the has_full_data flag on a filing_index row after
// all child-table inserts have completed (or failed). Keeping this separate
// from MarkProcessed lets us insert the parent row first (satisfying FK
// constraints on child tables) and then flip the flag only on success.
func SetHasFullData(ctx context.Context, pool *pgxpool.Pool, objectID string, success bool, errMsg string) error {
	_, err := pool.Exec(ctx, `
		UPDATE filing_index
		SET has_full_data = $2,
		    error_msg     = NULLIF($3, '')
		WHERE object_id = $1`,
		objectID, success, errMsg,
	)
	return err
}

// nullIfEmpty returns nil when s is empty so pgx sends NULL to Postgres.
func nullIfEmpty(s string) any {
	if s == "" {
		return nil
	}
	return s
}
