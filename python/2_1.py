#!/usr/bin/python3


'''
  Title: 2_1.py
  Date: 2016-10-12
  Author: Sergii Gladchuk

  Description:
    
    
  List of functions:
    No functions used
  
  Procedure:
    1. Open blastx file
    2. Itarate through each line and access id line
    3. Split line into list in order to easier access columns
    4. Build dictionary id (col 1) as keys and protein name (col 10) as value
    5. Do not create value if 'null' in blast file
    6. Open input and output fies 
    7. Loop through lines in input fasta file and access ID line
    8. Check that id is in the dictionary and append ID line with protein name
    9. Print changed id line and all other lines to output
    
  Usage:
    ./2_1.py fastaFile blastxFile
    
'''
    
import sys
import re

proteinNames = {}

#1
with open(sys.argv[2], 'r') as blastxF:
  
  #2
  for blastLine in blastxF:
    
    #3
    listLine = blastLine.split('\t')
    
    #4
    blastID = listLine[0]
    hitName = listLine[9]
    
    #5
    if hitName != 'null':
      proteinNames[blastID] = hitName


#6
with open(sys.argv[1], 'r') as fastaIn, open('2_1_output.fna', 'w') as fastaOut:
    
    #7
    for fastaLine in fastaIn:
      
      if fastaLine[0] == '>':
        fastaId = fastaLine[1:fastaLine.find('\t')]
        
        #8
        if fastaId in proteinNames:
          
          #9
          print(fastaLine[:-1:] + '\tprotein=' + proteinNames[fastaId], file=fastaOut)
          
        else:
          print(fastaLine, end='', file=fastaOut)
      
      else:
        print(fastaLine, end='', file=fastaOut)