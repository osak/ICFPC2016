#!/usr/bin/env python

import pymongo
import time
import sys

def seekProblems(id = None, name = None):
    client = pymongo.MongoClient('localhost', 27017)
    origami = client['origami']
    problems = origami.problems

    results = None
    if name is not None:
        print(name)
        results = problems.find({
            "owner_name": {"$eq": name}
        })
    elif id is not None:
        results = problems.find({
            "owner_id": {"$eq": id}
        })

    problems = origami.solutions

    pids = []
    for problem in results:
        pids.append(problem['problem_id'])

    return pids

def showTargetLinks(id = None, name = None):
    base = 'http://2016sv.icfpcontest.org/problem/view/'
    pids = seekProblems(id, name)
    for pid in pids:
        print(base + str(pid))


def main(id=None, name=None):
    showTargetLinks(id, name)

if __name__ == '__main__':
    param = sys.argv

    if len(param) > 2 and param[1] == '-n':
        main(name=str(param[2]))
    elif len(param) > 2 and param[1] == '-i':
        main(id=int(param[2]))
    else:
        print("usage")
        print("python seeker.py -n <name>")
        print("python seeker.py -i <id>")
