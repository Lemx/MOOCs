#!/usr/bin/python
import sys

oldStore = None
maxAmount = 0.0

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    store, amount = data

    if oldStore and oldStore != store:
        print("{}\t{}".format(oldStore, maxAmount))

        maxAmount = 0

    oldStore = store
    maxAmount = max(maxAmount, float(amount))

if oldStore:
    print("{}\t{}".format(oldStore, maxAmount))
