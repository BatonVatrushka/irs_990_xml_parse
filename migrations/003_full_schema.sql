-- Migration 003: Full 990 financial schema
--
-- Adds:
--   • has_full_data to filing_index (replaces processed_at as the skip guard)
--   • Fix UNIQUE on schedule_j_compensation to include title (prevents false
--     conflicts when the same person holds two roles in one filing)
--   • organizations  — one row per EIN, canonical BMF data + XML enrichment
--   • revenue        — Part VIII  (one row per filing)
--   • expenses       — Part IX    (one row per filing)
--   • balance_sheet  — Part X EOY (one row per filing)
--   • employee_compensation — Part VII-A  (all listed officers/employees)
--   • contractors    — Part VII-B (independent contractors)
--   • grants_paid    — Schedule I  (grants & assistance paid out)

-- ─── filing_index ────────────────────────────────────────────────────────────

ALTER TABLE filing_index
    ADD COLUMN IF NOT EXISTS has_full_data BOOLEAN DEFAULT FALSE;

-- ─── schedule_j_compensation ─────────────────────────────────────────────────
-- Widen the uniqueness key so a person who appears twice with different titles
-- (e.g. CEO and Board Chair in one transition year) doesn't collide.

ALTER TABLE schedule_j_compensation
    DROP CONSTRAINT IF EXISTS schedule_j_compensation_object_id_person_name_key;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'uniq_sched_j_person_title'
    ) THEN
        ALTER TABLE schedule_j_compensation
            ADD CONSTRAINT uniq_sched_j_person_title
            UNIQUE (object_id, person_name, title);
    END IF;
END
$$;

-- ─── organizations ────────────────────────────────────────────────────────────
-- Seeded from BMF at startup; enriched with XML fields during filing processing.

CREATE TABLE IF NOT EXISTS organizations (
    ein                  VARCHAR(20) PRIMARY KEY,
    name                 TEXT        NOT NULL,
    city                 TEXT,
    state                VARCHAR(2),
    ntee_code            VARCHAR(10),
    irs_subsection_code  VARCHAR(5),  -- 03 = 501(c)(3), etc.
    ruling_date          VARCHAR(6),  -- YYYYMM from BMF
    tax_period           VARCHAR(10), -- most recent tax_period from filing
    website_url          TEXT,
    gross_receipts       BIGINT,
    created_at           TIMESTAMPTZ DEFAULT NOW(),
    updated_at           TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_org_state      ON organizations(state);
CREATE INDEX IF NOT EXISTS idx_org_city_state ON organizations(city, state);
CREATE INDEX IF NOT EXISTS idx_org_ntee       ON organizations(ntee_code);

-- ─── revenue (Part VIII) ──────────────────────────────────────────────────────

CREATE TABLE IF NOT EXISTS revenue (
    id                              BIGSERIAL   PRIMARY KEY,
    object_id                       VARCHAR(50) NOT NULL REFERENCES filing_index(object_id),
    ein                             VARCHAR(20) NOT NULL,
    tax_year                        INT         NOT NULL,
    -- Contributions & grants (lines 1a–1g)
    federated_campaigns_amt         BIGINT      DEFAULT 0,
    membership_dues_amt             BIGINT      DEFAULT 0,
    fundraising_amt                 BIGINT      DEFAULT 0,
    related_organizations_amt       BIGINT      DEFAULT 0,
    government_grants_amt           BIGINT      DEFAULT 0,  -- ← public funding
    all_other_contributions_amt     BIGINT      DEFAULT 0,
    noncash_contributions_amt       BIGINT      DEFAULT 0,
    total_contributions_amt         BIGINT      DEFAULT 0,
    -- Program service revenue (line 2g)
    total_program_service_rev_amt   BIGINT      DEFAULT 0,
    -- Other revenue
    investment_income_amt           BIGINT      DEFAULT 0,
    bond_proceeds_amt               BIGINT      DEFAULT 0,
    royalties_revenue_amt           BIGINT      DEFAULT 0,
    net_rental_income_loss_amt      BIGINT      DEFAULT 0,
    net_gain_loss_investments_amt   BIGINT      DEFAULT 0,
    fundraising_net_income_amt      BIGINT      DEFAULT 0,
    gaming_net_income_amt           BIGINT      DEFAULT 0,
    net_income_from_inventory_amt   BIGINT      DEFAULT 0,
    total_other_revenue_amt         BIGINT      DEFAULT 0,
    total_revenue_amt               BIGINT      DEFAULT 0,
    created_at                      TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE (object_id)
);

CREATE INDEX IF NOT EXISTS idx_revenue_ein_year    ON revenue(ein, tax_year);
CREATE INDEX IF NOT EXISTS idx_revenue_govt_grants ON revenue(government_grants_amt DESC);
CREATE INDEX IF NOT EXISTS idx_revenue_total       ON revenue(total_revenue_amt DESC);

-- ─── expenses (Part IX) ───────────────────────────────────────────────────────

CREATE TABLE IF NOT EXISTS expenses (
    id                              BIGSERIAL   PRIMARY KEY,
    object_id                       VARCHAR(50) NOT NULL REFERENCES filing_index(object_id),
    ein                             VARCHAR(20) NOT NULL,
    tax_year                        INT         NOT NULL,
    grants_to_orgs_amt              BIGINT      DEFAULT 0,
    grants_to_individuals_amt       BIGINT      DEFAULT 0,
    foreign_grants_amt              BIGINT      DEFAULT 0,
    benefits_to_members_amt         BIGINT      DEFAULT 0,
    comp_current_officers_amt       BIGINT      DEFAULT 0,
    comp_disqualified_persons_amt   BIGINT      DEFAULT 0,
    other_salaries_wages_amt        BIGINT      DEFAULT 0,
    pension_plan_contribs_amt       BIGINT      DEFAULT 0,
    other_employee_benefits_amt     BIGINT      DEFAULT 0,
    payroll_taxes_amt               BIGINT      DEFAULT 0,
    fees_management_amt             BIGINT      DEFAULT 0,
    fees_legal_amt                  BIGINT      DEFAULT 0,
    fees_accounting_amt             BIGINT      DEFAULT 0,
    fees_lobbying_amt               BIGINT      DEFAULT 0,
    fees_professional_fundraising   BIGINT      DEFAULT 0,
    fees_investment_mgmt_amt        BIGINT      DEFAULT 0,
    fees_other_amt                  BIGINT      DEFAULT 0,
    advertising_amt                 BIGINT      DEFAULT 0,
    office_expenses_amt             BIGINT      DEFAULT 0,
    information_technology_amt      BIGINT      DEFAULT 0,
    royalties_amt                   BIGINT      DEFAULT 0,
    occupancy_amt                   BIGINT      DEFAULT 0,
    travel_amt                      BIGINT      DEFAULT 0,
    payments_travel_entertain_amt   BIGINT      DEFAULT 0,
    conferences_meetings_amt        BIGINT      DEFAULT 0,
    interest_amt                    BIGINT      DEFAULT 0,
    payments_to_affiliates_amt      BIGINT      DEFAULT 0,
    depreciation_depletion_amt      BIGINT      DEFAULT 0,
    insurance_amt                   BIGINT      DEFAULT 0,
    all_other_expenses_amt          BIGINT      DEFAULT 0,
    total_functional_expenses_amt   BIGINT      DEFAULT 0,
    created_at                      TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE (object_id)
);

CREATE INDEX IF NOT EXISTS idx_expenses_ein_year ON expenses(ein, tax_year);

-- ─── balance_sheet (Part X — end-of-year column) ─────────────────────────────

CREATE TABLE IF NOT EXISTS balance_sheet (
    id                               BIGSERIAL   PRIMARY KEY,
    object_id                        VARCHAR(50) NOT NULL REFERENCES filing_index(object_id),
    ein                              VARCHAR(20) NOT NULL,
    tax_year                         INT         NOT NULL,
    -- Assets
    cash_non_interest_bearing_amt    BIGINT      DEFAULT 0,
    savings_temp_cash_invest_amt     BIGINT      DEFAULT 0,
    pledges_grants_receivable_amt    BIGINT      DEFAULT 0,
    accounts_receivable_amt          BIGINT      DEFAULT 0,
    receivables_officers_amt         BIGINT      DEFAULT 0,
    other_notes_loans_receiv_amt     BIGINT      DEFAULT 0,
    inventories_amt                  BIGINT      DEFAULT 0,
    prepaid_expenses_amt             BIGINT      DEFAULT 0,
    land_bldg_equip_net_amt          BIGINT      DEFAULT 0,
    investments_publicly_traded_amt  BIGINT      DEFAULT 0,
    investments_other_securities_amt BIGINT      DEFAULT 0,
    investments_program_related_amt  BIGINT      DEFAULT 0,
    intangible_assets_net_amt        BIGINT      DEFAULT 0,
    other_assets_total_amt           BIGINT      DEFAULT 0,
    total_assets_amt                 BIGINT      DEFAULT 0,
    -- Liabilities
    accounts_payable_accrued_amt     BIGINT      DEFAULT 0,
    grants_payable_amt               BIGINT      DEFAULT 0,
    deferred_revenue_amt             BIGINT      DEFAULT 0,
    tax_exempt_bond_liab_amt         BIGINT      DEFAULT 0,
    loans_from_officers_amt          BIGINT      DEFAULT 0,
    mortgages_notes_payable_amt      BIGINT      DEFAULT 0,
    other_liabilities_amt            BIGINT      DEFAULT 0,
    total_liabilities_amt            BIGINT      DEFAULT 0,
    -- Net assets (pre-ASU-2016 fields for older filings)
    unrestricted_net_assets_amt      BIGINT      DEFAULT 0,
    temp_restricted_net_assets_amt   BIGINT      DEFAULT 0,
    perm_restricted_net_assets_amt   BIGINT      DEFAULT 0,
    -- Net assets (post-ASU-2016 fields, 2018+ tax years)
    net_assets_without_donor_restr   BIGINT      DEFAULT 0,
    net_assets_with_donor_restr      BIGINT      DEFAULT 0,
    total_net_assets_amt             BIGINT      DEFAULT 0,
    created_at                       TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE (object_id)
);

CREATE INDEX IF NOT EXISTS idx_balance_ein_year    ON balance_sheet(ein, tax_year);
CREATE INDEX IF NOT EXISTS idx_balance_total_assets ON balance_sheet(total_assets_amt DESC);

-- ─── employee_compensation (Part VII-A) ──────────────────────────────────────
-- All officers, directors, trustees, and key employees listed on Part VII-A.
-- This is broader than Schedule J — includes anyone required to be listed.

CREATE TABLE IF NOT EXISTS employee_compensation (
    id                           BIGSERIAL   PRIMARY KEY,
    object_id                    VARCHAR(50) NOT NULL REFERENCES filing_index(object_id),
    ein                          VARCHAR(20) NOT NULL,
    tax_year                     INT         NOT NULL,
    person_name                  TEXT        NOT NULL,
    title                        TEXT,
    avg_hours_per_week           NUMERIC(8,2),
    is_trustee_or_director       BOOLEAN     DEFAULT FALSE,
    is_institutional_trustee     BOOLEAN     DEFAULT FALSE,
    is_officer                   BOOLEAN     DEFAULT FALSE,
    is_key_employee              BOOLEAN     DEFAULT FALSE,
    is_highest_compensated       BOOLEAN     DEFAULT FALSE,
    is_former                    BOOLEAN     DEFAULT FALSE,
    reportable_comp_from_org     BIGINT      DEFAULT 0,
    reportable_comp_from_related BIGINT      DEFAULT 0,
    other_compensation           BIGINT      DEFAULT 0,
    created_at                   TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE (object_id, person_name, title)
);

CREATE INDEX IF NOT EXISTS idx_emp_comp_ein_year ON employee_compensation(ein, tax_year);
CREATE INDEX IF NOT EXISTS idx_emp_comp_person   ON employee_compensation(person_name);
CREATE INDEX IF NOT EXISTS idx_emp_comp_total    ON employee_compensation(reportable_comp_from_org DESC);

-- ─── contractors (Part VII-B) ─────────────────────────────────────────────────

CREATE TABLE IF NOT EXISTS contractors (
    id               BIGSERIAL   PRIMARY KEY,
    object_id        VARCHAR(50) NOT NULL REFERENCES filing_index(object_id),
    ein              VARCHAR(20) NOT NULL,
    tax_year         INT         NOT NULL,
    contractor_name  TEXT        NOT NULL,
    city             TEXT,
    state            VARCHAR(2),
    services_desc    TEXT,
    compensation_amt BIGINT      DEFAULT 0,
    created_at       TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_contractors_ein_year ON contractors(ein, tax_year);
CREATE INDEX IF NOT EXISTS idx_contractors_name     ON contractors(contractor_name);
CREATE INDEX IF NOT EXISTS idx_contractors_comp     ON contractors(compensation_amt DESC);

-- ─── grants_paid (Schedule I) ─────────────────────────────────────────────────
-- No UNIQUE constraint — the same recipient can receive multiple grants from
-- one org in one filing year (e.g. multiple programs).

CREATE TABLE IF NOT EXISTS grants_paid (
    id                    BIGSERIAL   PRIMARY KEY,
    object_id             VARCHAR(50) NOT NULL REFERENCES filing_index(object_id),
    ein                   VARCHAR(20) NOT NULL,
    tax_year              INT         NOT NULL,
    recipient_name        TEXT        NOT NULL,
    recipient_ein         VARCHAR(20),
    recipient_city        TEXT,
    recipient_state       VARCHAR(2),
    irc_section_desc      TEXT,
    cash_grant_amt        BIGINT      DEFAULT 0,
    noncash_assistance_amt BIGINT     DEFAULT 0,
    purpose_of_grant_txt  TEXT,
    created_at            TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_grants_ein_year      ON grants_paid(ein, tax_year);
CREATE INDEX IF NOT EXISTS idx_grants_recipient_name ON grants_paid(recipient_name);
CREATE INDEX IF NOT EXISTS idx_grants_recipient_ein  ON grants_paid(recipient_ein) WHERE recipient_ein IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_grants_cash_amt       ON grants_paid(cash_grant_amt DESC);
