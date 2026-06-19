-- Migration 004: Trigram full-text search indexes
--
-- Enables pg_trgm and adds GIN indexes on all high-value name columns so that
-- fuzzy ILIKE / similarity (%) queries are fast without a seq scan.
--
-- Covered columns:
--   organizations.name                     — org name search
--   schedule_j_compensation.person_name    — exec name across filings
--   schedule_j_compensation.company_name   — filing org name (denormalized)
--   employee_compensation.person_name      — Part VII-A officers/employees
--   contractors.contractor_name            — independent contractors
--   grants_paid.recipient_name             — grant recipients

CREATE EXTENSION IF NOT EXISTS pg_trgm;

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_orgs_name_trgm
    ON organizations USING GIN (name gin_trgm_ops);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_sched_j_person_trgm
    ON schedule_j_compensation USING GIN (person_name gin_trgm_ops);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_sched_j_company_trgm
    ON schedule_j_compensation USING GIN (company_name gin_trgm_ops);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_emp_comp_person_trgm
    ON employee_compensation USING GIN (person_name gin_trgm_ops);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_contractors_name_trgm
    ON contractors USING GIN (contractor_name gin_trgm_ops);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_grants_recipient_trgm
    ON grants_paid USING GIN (recipient_name gin_trgm_ops);
