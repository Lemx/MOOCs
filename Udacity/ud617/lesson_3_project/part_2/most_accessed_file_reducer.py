#!/usr/bin/python
import sys

oldFile = None
totalHits = 0
maxHits = 0
mostAccessedFile = None

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    file, hit = data

    if oldFile and oldFile != file:
        if totalHits > maxHits:
            maxHits = totalHits
            mostAccessedFile = oldFile

        totalHits = 0

    oldFile = file
    totalHits += int(hit)

if mostAccessedFile:
    print("{}\t{}".format(mostAccessedFile, maxHits))
