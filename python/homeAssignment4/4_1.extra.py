#!/usr/bin/python3

'''
  Title: 1_4.py
  Date: 2016-10-21
  Author: Sergii Gladchuk

  Description:
    This program will calculate pairwise alignment score for all the pairs of provided aligned sequences in input fasta file
    
  List of functions:
    No functions used
  
  Procedure:
    1. Assign inputs to variables, and build transition&transversion score dictionary
    2. Parse input file into dictionary and sorted list
    3. Build header of output
    4. Enter first-loop to start building 2D-table of results
    5. Start to build new line with ID name (id)
    6. Add identation
    7. Enter second-loop for second IDs
    8. Loop simultaneously through each sequence and calculate score
    9. Add score to line
    10. Output of line
    
    
  Usage:
    ./1_4.extra.py transitionCost transversionCost gapOpening gapExtention matchScore input_fasta
    
'''
    
import sys
import re

#1

transitionCost = int(sys.argv[1])
transversionCost = int(sys.argv[2])
gapOpening = int(sys.argv[3])
gapExtention = int(sys.argv[4])
matchScore = int(sys.argv[5])
inFasta = sys.argv[6]

transScores = {'AG' : transitionCost,
               'GA' : transitionCost,
               'CT' : transitionCost,
               'TC' : transitionCost,
               'AC' : transversionCost,
               'CA' : transversionCost,
               'AT' : transversionCost,
               'TA' : transversionCost,
               'GC' : transversionCost,
               'CG' : transversionCost,
               'GT' : transversionCost,
               'TG' : transversionCost}

#2
seqDict = {}
seqList = []

with open(inFasta, 'r') as fIn:
    
    for line in fIn:
        if line[0] == '>':
            currentId = line[1:].rstrip()
            seqList.append(currentId)
            seqDict[currentId] = ''
        else:
            seqDict[currentId] += line.rstrip().upper()

#3
print('\t' + ('\t'.join(seqList[1:])))

#4
for seqID1 in seqList[0:-1]:
    #5
    outLine = seqID1
    
    #6
    for i in seqList[1 : seqList.index(seqID1) + 1 ]:
        outLine += '\t'
    
    #7
    for seqID2 in seqList[seqList.index(seqID1) + 1 : ]:
      #8
        score = 0
        isGapOpen = False
        for char1, char2 in zip(seqDict[seqID1],seqDict[seqID2]):
            if char1 == char2 and char1 != '-':
                score += matchScore
                isGapOpen = False
            elif char1 != '-' and char2 != '-':
                score += transScores[char1 + char2]
                isGapOpen = False
            else:
                #gap
                if isGapOpen:
                    #extention
                    score += gapExtention
                else:
                    #gapOpen
                    score += gapOpening
                    isGapOpen = True
            
            
        #9
        outLine += '\t{}'.format(score)
      
    #10
    print(outLine)
      


