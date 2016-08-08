#!/usr/bin/env python

import pymongo
import time
import sys

def getHowManySubmitted():
    client = pymongo.MongoClient('localhost', 27017)
    origami = client['origami']
    submit_queue = origami.submit_queue
    timestamp = int(time.time())
    timestamp -= timestamp % 3600

    results = submit_queue.find({
        "status": {"$eq": "complete"},
        "submission_time" : {"$gt": timestamp}
    })

    return results.count()


def getZanki():
    count = getHowManySubmitted()
    return 1000 - count

def main(zanki):
    if zanki:
        print(getZanki())
    else:
        print(getHowManySubmitted())


if __name__ == '__main__':
    param = sys.argv

    if len(param) == 1:
        main(True)
    elif len(param) > 1 and param[1] == '-r':
        main(False)
    else:
        print("usage")
        print("python zanki.py: How many times you can submit")
        print("python zanki.py - r: How many times you submitted during this term")
