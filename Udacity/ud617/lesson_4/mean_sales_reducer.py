#!/usr/bin/python
import sys

dayCount = 0
oldDay = None
total = 0.0

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    day, amount = data

    if oldDay and oldDay != day:
        print("{}\t{}".format(oldDay, total / dayCount))

        total = 0.0
        dayCount = 0

    oldDay = day
    total += float(amount)
    dayCount += 1

if oldDay:
    print("{}\t{}".format(oldDay, total / dayCount))
