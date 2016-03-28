#!/usr/bin/python
# Here you will be able to combine the values that come from 2 sources
# Value that starts with A will be the user data
# Values that start with B will be forum node data

import sys


def reducer():

    user = None

    for line in sys.stdin:

        # YOUR CODE HERE

        data = line.split("\t")
        if data[1] == "A":
            user = data
            continue

        output = data[2:] + user[2:]

        print("\t".join(output))
