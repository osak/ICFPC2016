import json
import os
import subprocess

from compare import ProblemInput, get_transform_body
from grace import fetch_perfect_solutions, get_problem_path

PROBLEM_DIR = "/home/icfpc/shared/problems"
RASIS_SAY = "/home/icfpc/shared/rasis/rasis_say"


class UnionFind(object):
    def __init__(self, size):
        self.table = [-1 for _ in range(size)]

    def find(self, x):
        if self.table[x] < 0:
            return x
        else:
            self.table[x] = self.find(self.table[x])
            return self.table[x]

    def union(self, x, y):
        s1 = self.find(x)
        s2 = self.find(y)
        if s1 != s2:
            if self.table[s1] <= self.table[s2]:
                self.table[s1] += self.table[s2]
                self.table[s2] = s1
            else:
                self.table[s2] += self.table[s1]
                self.table[s1] = s2
            return True
        return False


problems = dict()
for filename in os.listdir(PROBLEM_DIR):
    if not filename.endswith(".in"):
        continue
    problem_id = int(filename[:-3])
    path = get_problem_path(problem_id)
    try:
        with open(path) as problem_file:
            problem = ProblemInput(problem_file)
        problems[problem_id] = problem
    except Exception as e:
        print(e)

pid2uid = dict()
for pid in problems:
    next_id = len(pid2uid)
    pid2uid[pid] = next_id

uf = UnionFind(len(problems))
for id1 in problems:
    for id2 in problems:
        if uf.find(pid2uid[id1]) == uf.find(pid2uid[id2]):
            continue
        if get_transform_body(problems[id1], problems[id2]) is not None:
            uf.union(pid2uid[id1], pid2uid[id2])

grouped = dict()
for pid in problems:
    uid = pid2uid[pid]
    group = uf.find(uid)
    if group not in grouped:
        grouped[group] = []
    grouped[group].append(pid)

with open("/home/icfpc/shared/grace/clusters.json", "w") as clusters_file:
    json.dump(grouped, clusters_file)

solved_ids = set(int(solution["problem_id"]) for solution in fetch_perfect_solutions())

clusters = [cluster for cluster in grouped.values() if len(cluster) >= 2 and len(solved_ids.intersection(cluster)) == 0]
clusters.sort(key=lambda cls: len(cls), reverse=True)
comment = '\n'.join(map(str, clusters))
proc = subprocess.Popen([RASIS_SAY, "-"], stdin=subprocess.PIPE)
proc.communicate(bytes(comment, "ascii"))
subprocess.run([RASIS_SAY, "グレイスが解かれていない問題をまとめてくれマシタ！"])
