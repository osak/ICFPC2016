#!/usr/bin/env python

import filecmp
import sys


def sameCheck(A, B):
    return filecmp.cmp(A, B)


def main(A, B):
    result = sameCheck(A, B)
    print(result)


if __name__ == '__main__':
    param = sys.argv
    main(param[1], param[2])