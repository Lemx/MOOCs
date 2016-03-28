#!/usr/bin/python
import sys

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 6:
        continue

    date, time, store, category, amount, payment_method = data
    print("{}\t{}".format(category, amount))

