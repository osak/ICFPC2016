import argparse
import subprocess
import os


def main(grace_output):
    for file_name in os.listdir(grace_output):
        if file_name.endswith(".sol"):
            problem_id = file_name[:-4]
            subprocess.run(["/home/icfpc/shared/osak/submit_solution.rb", "-e", "grace", "-p", str(problem_id), "-f", os.path.join(grace_output, file_name)])
            print(file_name + " submitted.")


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("grace_output", default="solutions")
    args = parser.parse_args()
    main(args.grace_output)
