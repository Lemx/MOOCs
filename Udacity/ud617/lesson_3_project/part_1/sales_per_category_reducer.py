#!/usr/bin/python
import sys

oldCategory = None
total = 0.0

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    category, amount = data

    if oldCategory and oldCategory != category:
        print("{}\t{}".format(oldCategory, total))

        total = 0

    oldCategory = category
    total += float(amount)

if oldCategory:
    print("{}\t{}".format(oldCategory, total))
