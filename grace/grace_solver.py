import grace
import sys
from compare import ProblemInput


def solve():
    problem_input = ProblemInput(sys.stdin)
    solver = grace.Solver()
    sys.stdout.write(str(solver.solve(problem_input), "ascii"))


if __name__ == '__main__':
    solve()