#!/usr/bin/python
import sys

oldIp = None
totalHits = 0

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    ip, hit = data

    if oldIp and oldIp != ip:
        print("{}\t{}".format(oldIp, totalHits))

        totalHits = 0

    oldIp = ip
    totalHits += int(hit)

if oldIp:
    print("{}\t{}".format(oldIp, totalHits))
