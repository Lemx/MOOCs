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

    path = data[5]
    if "http://www.the-associates.co.uk" in path:
        path = path.replace("http://www.the-associates.co.uk", "")
    print("{}\t{}".format(path, 1))
