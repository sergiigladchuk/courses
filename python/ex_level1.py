#!/usr/bin/python3

import sys
import re

print("\n**************************\nQ.11\n-----------------------")

fileName = sys.argv[1]
countLines = 0

with open(fileName, 'r') as f:
    if re.search('.+\.fastq$', fileName) != None:
        #fastq processing
        for line in f:
            if re.match('^@.+', line) != None:
                countLines += 1
    else:
        #fasta processing
        for line in f:
            if re.match('^\>.+', line) != None:
                countLines += 1
                
                
print(countLines)
