#!/usr/bin/python3

class FirstClass:
	
	def __init__(self, attribute):
		self.instance_var = attribute
		
	def get_variable(self):
		return self.instance_var

my_first_instance = FirstClass('I\'m first instance')
my_second_instance = FirstClass('I\'m second instance')


print(my_first_instance.get_variable())
print(my_second_instance.get_variable())