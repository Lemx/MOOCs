#!/usr/bin/python
import sys
import re

regex = re.compile(r'([(\d\.)]+) (.*) (.*) \[(.*?)\] "(\w*?) (.*) (.*)" (.*) (.*)')

for line in sys.stdin:
    matches = regex.match(line.strip())

    if matches:
        data = matches.groups()
    else:
        continue

    if len(data) != 9:
        continue

    page = data[5]
    print("{}\t{}".format(page, 1))
