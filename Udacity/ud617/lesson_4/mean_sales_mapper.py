#!/usr/bin/python
import sys
from datetime import datetime

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 6:
        continue

    date, time, store, category, amount, payment_method = data
    weekday = datetime.strptime(date, "%Y-%m-%d").weekday()
    print("{}\t{}".format(weekday, amount))
