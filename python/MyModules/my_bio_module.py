def getGCPercentage(sequence, decimals):
	
	'''
		Calculates and returns the percentage of CG-letters
		in the targetstring. Allos the user to specify the number of decimal in the returned number
	'''
	
	lower_seq = sequence.lower()
	gc_count = lower_seq.count('c') + lower_seq.count('g')
	
	return round(100 * gc_count / len(sequence), decimals)

def getReverseComplement(sequence):
	revCompliment = sequence[::-1].translate(str.maketrans('aAcCgGtT.','tTgGcCaAn'))
	return revCompliment