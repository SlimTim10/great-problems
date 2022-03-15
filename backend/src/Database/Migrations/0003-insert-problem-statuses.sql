-- Must reflect order and naming in Common.Api.ProblemStatus
INSERT INTO problem_statuses(id, name)
VALUES (0, 'Draft'), (1, 'Published')
ON CONFLICT DO NOTHING;
