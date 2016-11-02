#!/usr/bin/python3


'''
  Title: 1_1.py
  Date: 2016-10-11
  Author: Sergii Gladchuk

  Description:
    This program will print absolute abundances for each amino acid in the input file
    
    
  List of functions:
    no functions are used in program
  
  
  Procedure:
    1. Create dictionary for all amino acids single letters and X as any amino acid, with 0
    2. Open the input file
    3. Iterate through each line in the file
    4. Skip id lines and get only sequences
    5. Loop through each leter in seq 
    6. Check if letter is in dictionary
    7. Update counts in dictionary
    8. Create sorted list of amino acids
    9. Output of the dictionary elements based on sorted list
  
  Usage:
    ./1_1.py inputFile
    
'''
    
import sys
import re

#1
aminoAcids = {'A' : 0, 'C' : 0, 'D' : 0, 'E' : 0, 'F' : 0, 'G' : 0, 
	      'H' : 0, 'I' : 0, 'K' : 0, 'L' : 0, 'M' : 0, 'N' : 0, 
	      'P' : 0, 'Q' : 0, 'R' : 0, 'S' : 0, 'T' : 0, 'T' : 0, 
	      'V' : 0, 'W' : 0, 'Y' : 0, 'X' : 0}

inputFile = sys.argv[1]
#2
with open(inputFile, 'r') as fIn:
  
  #3
  for line in fIn:
    
    #4
    if line[0] != '>':
      #removing new lines and changing to upper case
      seq = line.replace('\n','').upper()
      
      #5
      for char in seq:
        
        #6
        if char in aminoAcids:
          #7
          aminoAcids[char] += 1
        else:
          #7
          aminoAcids['X'] += 1

#8
sortedAminoAcids = ['A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'M', 
                    'N', 'P', 'Q', 'R', 'S', 'T', 'T', 'V', 'W', 'Y', 'X']
#9
for acid in sortedAminoAcids:
  print('{}\t{}'.format(acid, aminoAcids[acid]))
