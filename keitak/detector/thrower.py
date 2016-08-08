#!/usr/bin/env python

import hashlib
from detector import getDuplicated
import os
import subprocess

def killDupplicate():
    throw = getDuplicated()
    with open("./shoushouri.sh", "w") as daisyouri:
        for id, output in throw.items():
            hash = hashlib.sha1(output).hexdigest()
            path = os.path.abspath("outputs/" + hash + ".txt")
            with open(path, "w") as file:
                file.write(output)

            args = []
            args.append("~/shared/osak/submit_solution.rb")
            args.append("-e")
            args.append("Daishouri")
            args.append("-p")
            args.append(str(id))
            args.append("-f")
            args.append(path)

            daisyouri.write(' '.join(args) + '\n')
            # subprocess.call(args, shell=True)
        daisyouri.write("echo Daishouried " + str(len(throw)) + "duplicated problems \n")

    print("Run shoushouri.sh to daishouri")
def main():
    killDupplicate()


if __name__ == '__main__':
    main()