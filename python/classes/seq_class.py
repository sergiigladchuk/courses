#!/usr/bin/python3
import re

class Sequence:
	
	'''
	Super class for sequences
	Contains basic sequence information
	'''
	
	def __init__(self, sequence):
		self.seq = sequence
	
	def getSubSequenceCount(self, sequence, sub_sequence):
		
		'''
		Takes a sequence and a sub sequence, and returns the number of sub sequences occurences in the sequence
		'''
		
		return len(re.findall(sub_sequence, self.seq, re.I))
	
	def __str__(self):
		return self.seq
	
class NuclSequence(Sequence):
	
	'''
	Sub class to the sequence class, representing a nucleotide sequence
	'''
	
	def __init__(self, sequence):
		super().__init__(sequence)
		
	def getRevComp(self):
		
		'''
		Retrive the reverse complement for the sequence
		'''
		
		return self.seq[::-1].translate(str.maketrans('atgc','tgca'))
	
	def getOligoCount(self, oligo):
		
		'''
		Counts the number of oligo occurences in the sequence 
		Counts both for forward and reverse strand
		'''
		
		fw_count = super().getSubSequenceCount(self.seq,oligo)
		rv_seq = self.getRevComp()
		rv_count = super().getSubSequenceCount(rv_seq,oligo)
		
		return fw_count + rv_count
	
class AminoSequence(Sequence):
	
	'''
	Sub class to sequence class representing an amino sequence
	'''
	
	def __init__(self, sequence):
		super().__init__(sequence)
		
	def getPeptideCount(self, peptide):
		
		'''
		Counts number of target peptide occurences in the sequence
		'''
		
		return super().getSubSequenceCount(self.seq, peptide)
	
