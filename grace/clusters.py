with open("clusters") as clusters_file:
    clusters = eval(clusters_file.read())

for problem_ids in clusters.values():
    if len(problem_ids) > 1:
        print(problem_ids)
