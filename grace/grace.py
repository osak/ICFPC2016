# duplicate detector which is rotate and shift tolerant.
# the name Grace is taken from the character of SOUND VOLTEX III GRAVITY WARS, an arcade rhythm game from KONAMI.

import pymongo
import os
import sys
import compare
import argparse
import subprocess
from fractions import Fraction


SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
PROBLEM_DIR = "/home/icfpc/shared/problems"
GIDEON_EXE = "/home/icfpc/shared/mkut/bin/gideon"
SHUFFLE_EXE = "/home/icfpc/shared/kawatea/shuffle"


def fetch_perfect_solutions():
    client = pymongo.MongoClient('localhost', 27017)
    origami = client['origami']
    return origami.solutions.find({"resemblance": {"$eq" : 1.0}, "output": {"$exists" : True}})


def get_problem_path(problem_id):
    return os.path.join(PROBLEM_DIR, "{}.in".format(problem_id))


def get_our_problem_ids():
    client = pymongo.MongoClient('localhost', 27017)
    origami = client['origami']
    ids = []
    for problems in origami.problems.find({"owner_id": {"$eq" : 58}}):
        ids.append(int(problems["problem_id"]))
    return ids


def get_all_available_problem_ids():
    ids = []
    for i in range(30000):
        if os.path.exists(get_problem_path(i)):
            ids.append(i)
    return ids


def shuffle(solution):
    proc = subprocess.Popen([SHUFFLE_EXE], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    if isinstance(solution, str):
        solution = bytes(solution, "ascii")
    out, err = proc.communicate(solution)
    return out


def gideon(command_args, solution):
    proc = subprocess.Popen([GIDEON_EXE] + command_args, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    if isinstance(solution, str):
        solution = bytes(solution, "ascii")
    out, err = proc.communicate(solution)
    return out


def generate_new_solution(compare_result, solved_solution):
    do_reflect, a_source, b_source, rotator = compare_result
    reflected = gideon(["mirror", str(compare.Point(Fraction(0), Fraction(0))), str(compare.Point(Fraction(1), Fraction(0)))], solved_solution) if do_reflect else solved_solution
    originated = gideon(["mov", str(-a_source)], reflected)
    rotated = gideon(["rot", str(rotator.vecmul(compare.Point(Fraction(1), Fraction(0))))], originated)
    new_solution_long = gideon(["mov", str(b_source)], rotated)
    return shuffle(new_solution_long)


class Solver(object):
    def __init__(self):
        solution_objects = fetch_perfect_solutions()
        self.solutions = dict((int(solution['problem_id']), solution["output"]) for solution in solution_objects)
        self.problems = dict()
        for problem_id in self.solutions:
            try:
                with open(get_problem_path(problem_id)) as problem_file:
                    self.problems[problem_id] = compare.ProblemInput(problem_file)
            except Exception as e:
                print(e, file=sys.stderr)
                print("failed to load problem id = {}".format(problem_id), file=sys.stderr)

    def solve(self, problem_input):
        for problem_id, solved_input in self.problems.items():
            compare_result = compare.get_transform_body(solved_input, problem_input)
            if compare_result is not None:
                print("solved by {}".format(problem_id), file=sys.stderr)
                return generate_new_solution(compare_result, self.solutions[problem_id])
        return None

    def is_solvable_with_scale(self, problem_input):
        for problem_id, solved_input in self.problems.items():
            compare_result = compare.get_transform_with_scaling(solved_input, problem_input)
            if compare_result is not None:
                return compare_result
        return None

    def is_already_solved(self, problem_id):
        return problem_id in self.solutions


def main(destination_directory):
    if not os.path.exists(destination_directory):
        os.mkdir(destination_directory)
    solver = Solver()
    count = 0
    our_ids = set(get_our_problem_ids())
    for problem_id in get_all_available_problem_ids():
        if problem_id in our_ids:
            print("id: " + str(problem_id) + " is our problem. skipping.", file=sys.stderr)
            continue
        if solver.is_already_solved(problem_id):
            print("id: " + str(problem_id) + " is already solved. skipping.", file=sys.stderr)
            continue
        try:
            with open(get_problem_path(problem_id)) as problem_file:
                problem_input = compare.ProblemInput(problem_file)
            new_solution = solver.solve(problem_input)
            if new_solution is None:
                print("id: " + str(problem_id) + " is not solvable now.", file=sys.stderr)
                continue
            print("id: " + str(problem_id) + " can be solved by GRACE.", file=sys.stderr)
            with open(os.path.join(destination_directory, "{}.sol".format(problem_id)), "w") as solution_file:
                solution_file.write(str(new_solution, "ascii"))
                count += 1

        except Exception as e:
            print(e)
            print("failed to load problem id = {}".format(problem_id))
    print("detected {} solutions".format(count), file=sys.stderr)
    subprocess.run(["/home/icfpc/shared/rasis/rasis_say", "-channel", "general", "グレイスが{}個の解を見つけマシタ！".format(count)])


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("destination", default="solutions")
    args = parser.parse_args()
    main(args.destination)
