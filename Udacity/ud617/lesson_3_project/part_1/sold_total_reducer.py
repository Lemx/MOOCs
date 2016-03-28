#!/usr/bin/python
import sys

totalSales = 0
totalRevenue = 0.0

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    sold, amount = data

    totalRevenue += float(amount)
    totalSales += int(sold)

print("{}\t{}".format(totalSales, totalRevenue))
