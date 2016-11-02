#!/usr/bin/python3

import argparse
import sys

usage = '''This program removes barcode and then primer based on the input sample and number of missmatches'''

parser = argparse.ArgumentParser(description=usage)

parser.add_argument('-v','--version',
					action='version',
					version='%(prog)s 1.0')

parser.add_argument('-p','--primer',
					type=str,
					metavar='PRIMER',
					dest='primerSeq',
					help='primer to be removed',
					default='')

parser.add_argument('-m','--missmatches',
					type=int,
					metavar='MISSMATCHES',
					dest='missMatches',
					help='number of mismatches in primer',
					default=0)

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

lineCount = 0
totalMatchCount = 0
totalSeqCount = 0
for line in args.infile:
	lineCount+= 1
	
	if lineCount % 4 == 1 or lineCount % 4 == 3:
		#ID line or + line
		print(line, end='', file=args.outfile)
	elif lineCount % 4 == 2:
		#Seq line
		totalSeqCount += 1
		#remove barcode
		seq = line[8:-1:]
		
		#check for primer
		currentMissMatches = 0
		isPrimerMatch = False
		for char1, char2 in zip(seq[0:len(args.primerSeq)],args.primerSeq):
			if char1 != char2:
				currentMissMatches += 1
		
		if currentMissMatches <= args.missMatches:
			isPrimerMatch = True
			seq = seq[len(args.primerSeq): ]
			totalMatchCount += 1
		
		print(seq, file=args.outfile)
	else:
		#quality line trim
		trimLen = 8
		if isPrimerMatch:
			trimLen += len(args.primerSeq)
		print(line[trimLen : ], end='', file=args.outfile)
	
#infor output
print('Total seq lines: {}\nMatched with primer: {}'.format(totalSeqCount,totalMatchCount),file=sys.stdout)
	
	