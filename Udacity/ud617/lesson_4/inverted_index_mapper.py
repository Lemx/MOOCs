#!/usr/bin/python
import sys
import re

delimiters = " ", ".", ",", "!", "?", ":", ";", "\"", "(", ")", "<", ">", "[", "]", "#", "$", "=", "-", "/"
pattern = '|'.join(map(re.escape, delimiters))
regex = re.compile(pattern)

temp = ""

for line in sys.stdin:
    data = line.strip().split("\t")
    if data[0] == "\"id\"":
        continue

    if len(data) != 19:
        temp += line

        data = temp.strip().split("\t")
        if len(data) != 19:
            continue

    temp = ""
    for word in regex.split(data[4]):
        if word:
            print("{}\t{}".format(word.lower(), data[0].strip("\"")))
