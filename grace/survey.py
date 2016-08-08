from grace import Solver, get_all_available_problem_ids, get_problem_path
import compare
from fractions import Fraction
import os

PROBLEM_DIR = "/home/icfpc/shared/problems"

solver = Solver()
extend_count, shrink_count = 0, 0
for problem_id in get_all_available_problem_ids():
    if solver.is_already_solved(problem_id):
        continue
    try:
        with open(get_problem_path(problem_id)) as problem_file:
            problem_input = compare.ProblemInput(problem_file)
        transform_tuple = solver.is_solvable_with_scale(problem_input)
        print("id: {}".format(problem_id))
        if transform_tuple is None:
            continue
        scaling = transform_tuple[0]
        if scaling != Fraction(1):
            print("id: {} is solvable if scaling is available (scaling ratio: {})".format(problem_id, scaling))
            if scaling > 1:
                extend_count += 1
            else:
                shrink_count += 1
    except Exception as e:
        print(e)
        print("failed to load problem id = {}".format(problem_id))
print("{} problems can be solbed with scaling.".format(extend_count + shrink_count))
print("extending {} is super difficult, shrinking {} is relatively difficult".format(extend_count, shrink_count))