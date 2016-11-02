#!/usr/bin/python3


'''
  Title: 1_2.py
  Date: 2016-10-12
  Author: Sergii Gladchuk

  Description:
    This program will separate one fastq output file to different samles files
    based on barcodes.
    
  List of functions:
    No functions used
  
  Procedure:
    1. Open input file and output files
    2. Hardcode all barcodes and output files into dictionary
    3. Declare currentOutput file holder, idLine and line number variables before loop
    4. Loop through each line in input files and itarte line number
    5. Save ID line in variable and remove any current output
    6. Access sequence line and check if barcode is present in the beggining
    7. Output ID line and sequence line without barcode to selected output file
    8. If no burcode assign output to undetermined
    9. Output other two lines (before new ID line to same file)
    
    
  Usage:
    ./1_2.py inputFile
    
'''
    
import sys
import re

#1
with open(sys.argv[1], 'r') as fIn, open('sample1.fastq', 'w') as sample1, \
open('sample2.fastq', 'w') as sample2, open('sample3.fastq', 'w') as sample3, \
open('undetermined.fastq', 'w') as undetermined:
  
  #2
  sampleFiles = {'TATCCTCT' : sample1, 'GTAAGGAG' : sample2, 'TCTCTCCG' : sample3}
  
  #3
  currentOut = None
  idLine = ''
  lineNum = 0
  
  #4
  for line in fIn:
    lineNum += 1
    
    #5
    if lineNum % 4 == 1:
      #id line
      idLine = line
      #reset current output
      currentOut = None
    
    #6
    elif lineNum % 4 == 2:
      
      #sequence line
      barCode = line[0:8]
      if barCode in sampleFiles:
        currentOut = sampleFiles[barCode]
        
        #7
        print(idLine,file=currentOut, end='')
        print(line[8:],file=currentOut, end='')
      
      else:
        #8
        currentOut = undetermined
        print(idLine,file=currentOut, end='')
        print(line,file=currentOut, end='')
    
    else:
      #9
      print(line,file=currentOut, end='')
        
    