#!/usr/bin/env python

import pymongo


def getCompleteSolutions():
    client = pymongo.MongoClient('localhost', 27017)
    origami = client['origami']
    solutions = origami.solutions
    results = solutions.find({"resemblance":{"$eq" : 1.0}, "output":{"$exists":True}})

    outputs = {}
    already = set()
    for result in results:
        id = int(result['problem_id'])
        already.add(id)
        output = result['output']
        if id not in outputs or len(output) < len(outputs[id]):
            outputs[id] = output

    return {
        'already': already,
        'outputs': outputs
    }


def main():
    result = getCompleteSolutions()

    print(result['already'])

    for id, output in result['outputs'].items():
        print(id)
        print(output)

if __name__ == '__main__':
    main()