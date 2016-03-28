#!/usr/bin/python
import sys

oldWord = None
ids = []

for line in sys.stdin:
    data = line.strip().split("\t")

    if len(data) != 2:
        continue

    word, id = data

    if oldWord and oldWord != word:
        print("{}\t{};{}".format(oldWord, len(ids), ",".join(map(str, sorted(ids, key=int)))))

        ids = []

    oldWord = word
    ids.append(id)

if oldWord:
    print("{}\t{};{}".format(oldWord, len(ids), ",".join(map(str, sorted(ids, key=int)))))
