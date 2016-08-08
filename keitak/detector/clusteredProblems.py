#!/usr/bin/env

import os
import sameChecker

def trimIn(str):
    return str[:-len('.in')]


def getClusteredProblems():
    path = '/home/icfpc/shared/problems/'
    files = os.listdir(path)

    canonical = []

    merged = set()

    for A in files:
        a = None
        try:
            a = int(trimIn(A))
        except:
            continue

        if A in merged:
            continue
        merged.add(A)
        friends = set()
        friends.add(a)
        for B in files:
            b = None
            try:
                b = int(trimIn(B))
            except:
                continue

            if A == B:
                continue
            if sameChecker.sameCheck(path + A, path + B):
                merged.add(B)
                friends.add(b)

        canonical.append(friends)

    return canonical


def main():
    clusteredProblems = getClusteredProblems()
    for problems in clusteredProblems:
        print(problems)

if __name__ == '__main__':
    main()