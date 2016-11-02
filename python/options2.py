#!/usr/bin/python3

import argparse
import sys

usage = '''This program counts the total number of characters in a file
(including newlines) or in a number of lines in the files'''

parser = argparse.ArgumentParser(description=usage)

parser.add_argument('-v','--version',
		    action='version',
		    version='%(prog)s 1.0')

parser.add_argument('-n',
		    type=int,
		    metavar='LINES',
		    dest='lines',
		    help='number of lines to analyze')

parser.add_argument('-i',
		    dest='infile',
		    metavar='INFILE',
		    type=argparse.FileType('r'),
		    required=True)

parser.add_argument('-o',
		    dest='outfile',
		    metavar='OUTFILE',
		    type=argparse.FileType('w'),
		    default=sys.stdout)

args = parser.parse_args()
parser.parse_args()

count = 0
countLines = 0
for line in args.infile:
	
	if countLines == args.lines:
		break
	countLines += 1
	count += len(line)

print (count, file=args.outfile)