#!/usr/bin/python
import sys

oldPage = None
totalHits = 0

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    page, hit = data

    if oldPage and oldPage != page:
        print("{}\t{}".format(oldPage, totalHits))

        total = 0

    oldPage = page
    totalHits += int(hit)

if oldPage:
    print("{}\t{}".format(oldPage, totalHits))
