#!/usr/bin/python3

import argparse
import sys

usage = '''This program extracts palindromic sequences from given fasta file and two parameters'''

parser = argparse.ArgumentParser(description=usage)

parser.add_argument('-v','--version',
					action='version',
					version='%(prog)s 1.0')

parser.add_argument('-s','--stem-length',
					type=int,
					metavar='STEM',
					dest='stemLen',
					help='min length of the stem',
					required=True)

parser.add_argument('-l','--loop-length',
					type=int,
					metavar='LOOP',
					dest='loopLen',
					help='exact length of the loop',
					required=True)

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

seq = ''
for line in args.infile:
	if line[0] != '>':
		#build total sequences
		seq += line.rstrip()

palindromLen = args.stemLen * 2 + args.loopLen
for pos in range(len(seq) - palindromLen):
	#compare two stems for compartibility
	stem1 = seq[pos : pos + args.stemLen]
	stem2 = seq[pos + args.stemLen + args.loopLen : pos + palindromLen]
	revCompatibleStem2 = stem2[::-1].translate(str.maketrans('ACGT','TGCA'))
	if stem1 == revCompatibleStem2:
		print(seq[pos : pos + palindromLen])
	
	
