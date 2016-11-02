#!/usr/bin/python3

import sys
import my_bio_module

in_fh = open(sys.argv[1], 'r')

prev_id = None

for line in in_fh:
	if line[0] == '>':
		prev_id = line[1:]
	elif prev_id is not None:
		decimals = 1
		gc_perc = my_bio_module.getGCPercentage(line, decimals)
		print('Sequence {} has CG content {}%'.format(prev_id, gc_perc))
		prev_id = None
	else:
		print('Error: Input not in single-lined fasta format')
		exit(1)