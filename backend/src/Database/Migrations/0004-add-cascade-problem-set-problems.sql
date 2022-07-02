ALTER TABLE problem_set_problems
DROP CONSTRAINT problem_set_problems_problem_id_fkey,
ADD CONSTRAINT problem_set_problems_problem_id_fkey
  FOREIGN KEY(problem_id) REFERENCES problems(id) ON DELETE CASCADE;

ALTER TABLE problem_set_problems
DROP CONSTRAINT problem_set_problems_problem_set_id_fkey,
ADD CONSTRAINT problem_set_problems_problem_set_id_fkey
  FOREIGN KEY(problem_set_id) REFERENCES problem_sets(id) ON DELETE CASCADE;