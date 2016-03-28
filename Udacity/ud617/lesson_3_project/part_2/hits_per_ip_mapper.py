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

    ip = data[0]
    print("{}\t{}".format(ip, 1))
