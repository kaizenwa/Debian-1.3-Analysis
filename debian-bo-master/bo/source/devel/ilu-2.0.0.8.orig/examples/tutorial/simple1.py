# simple1.py, a simple program that demonstrates the use of the
#  Tutorial true module as a library.
#
# run this with the command "python simple1.py NUMBER [NUMBER...]"
#

import Tutorial, CalculatorImpl, string, sys

# A simple program:
#  1)  make an instance of Tutorial.Calculator
#  2)  add all the arguments by invoking the Add method
#  3)  print the resultant value.

def main (argv):

	c = CalculatorImpl.Calculator()
	if not c:
		error("Couldn't create calculator")

	# clear the calculator before using it

	c.SetValue (0.0)

	# now loop over the arguments, adding each in turn */

	for arg in argv[1:]:
		v = string.atof(arg)
		c.Add (v)

	# and print the result

	print "the sum is", c.GetValue()
	sys.exit(0)

main(sys.argv)

