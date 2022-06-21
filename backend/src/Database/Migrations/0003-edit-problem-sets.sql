ALTER TABLE problem_sets DROP COLUMN topic_id;
ALTER TABLE problem_set_problems ADD COLUMN position INTEGER NOT NULL UNIQUE;