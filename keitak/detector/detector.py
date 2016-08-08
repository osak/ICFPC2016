#!/usr/bin/env python

from clusteredProblems import getClusteredProblems
from completeSolutions import getCompleteSolutions


def getDuplicated():
    clusteredProblems = getClusteredProblems()
    completeSolutions = getCompleteSolutions()

    already = completeSolutions['already']
    id2Sol = completeSolutions['outputs']

    throw = {}

    for clustered in clusteredProblems:
        solution = None
        for id in clustered:
            if id in id2Sol: #found solution
                candidate = id2Sol[id]
                if solution is None or len(candidate) < len(solution):
                    solution = candidate

        if solution is None:
            continue

        shouldSolve = []
        for id in clustered:
            if id not in already:
                shouldSolve.append(id)

        for id in shouldSolve:
            throw[id] = solution

    return throw

def main():
    throw = getDuplicated()
    print(len(throw))
    for id, output in throw.items():
        print(id)
        print(output)

if __name__ == '__main__':
    main()