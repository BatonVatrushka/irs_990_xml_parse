-- Tracks every filing in the IRS index and whether we've processed it.
CREATE TABLE IF NOT EXISTS filing_index (
    object_id      VARCHAR(50)  PRIMARY KEY,
    ein            VARCHAR(20)  NOT NULL,
    tax_period     VARCHAR(10),
    form_type      VARCHAR(20),
    company_name   TEXT,
    filing_year    INT          NOT NULL,
    url            TEXT         NOT NULL,
    processed_at   TIMESTAMPTZ,
    has_schedule_j BOOLEAN      DEFAULT FALSE,
    error_msg      TEXT
);

-- One row per person per filing for Schedule J compensation data.
CREATE TABLE IF NOT EXISTS schedule_j_compensation (
    id                               BIGSERIAL   PRIMARY KEY,
    object_id                        VARCHAR(50) NOT NULL REFERENCES filing_index(object_id),
    ein                              VARCHAR(20) NOT NULL,
    tax_year                         INT         NOT NULL,
    company_name                     TEXT,
    person_name                      TEXT,
    title                            TEXT,
    avg_hours_per_week               NUMERIC(8,2),
    reportable_comp_from_org         BIGINT      DEFAULT 0,
    reportable_comp_from_related_org BIGINT      DEFAULT 0,
    other_compensation               BIGINT      DEFAULT 0,
    base_compensation                BIGINT      DEFAULT 0,
    bonus                            BIGINT      DEFAULT 0,
    other_comp_filing_org            BIGINT      DEFAULT 0,
    deferred_comp                    BIGINT      DEFAULT 0,
    nontaxable_benefits              BIGINT      DEFAULT 0,
    total_comp_filing_org            BIGINT      DEFAULT 0,
    comp_reported_other_org          BIGINT      DEFAULT 0,
    created_at                       TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE (object_id, person_name)
);

CREATE INDEX IF NOT EXISTS idx_comp_ein        ON schedule_j_compensation(ein);
CREATE INDEX IF NOT EXISTS idx_comp_tax_year   ON schedule_j_compensation(tax_year);
CREATE INDEX IF NOT EXISTS idx_comp_company    ON schedule_j_compensation(company_name);
CREATE INDEX IF NOT EXISTS idx_comp_person     ON schedule_j_compensation(person_name);
CREATE INDEX IF NOT EXISTS idx_comp_total_comp ON schedule_j_compensation(total_comp_filing_org DESC);
