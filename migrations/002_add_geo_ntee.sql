ALTER TABLE schedule_j_compensation
    ADD COLUMN IF NOT EXISTS city      TEXT,
    ADD COLUMN IF NOT EXISTS state     VARCHAR(2),
    ADD COLUMN IF NOT EXISTS ntee_code VARCHAR(10);

-- Also store org-level geo/NTEE on the filing index for quick lookups.
ALTER TABLE filing_index
    ADD COLUMN IF NOT EXISTS city      TEXT,
    ADD COLUMN IF NOT EXISTS state     VARCHAR(2),
    ADD COLUMN IF NOT EXISTS ntee_code VARCHAR(10);

CREATE INDEX IF NOT EXISTS idx_comp_state      ON schedule_j_compensation(state);
CREATE INDEX IF NOT EXISTS idx_comp_city_state ON schedule_j_compensation(city, state);
CREATE INDEX IF NOT EXISTS idx_comp_ntee       ON schedule_j_compensation(ntee_code);
