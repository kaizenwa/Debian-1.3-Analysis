# simple2.py, a simple program that demonstrates the use of the
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

	if (len(sys.argv) < 2):
		c.SetValue (0.0)
	else:
		c.SetValue (string.atof(argv[1]))

	# now loop over the arguments, Dividing by each in turn */

	try:
		for arg in argv[2:]:
			v = string.atof(arg)
			c.Divide (v)
	except:
		print 'exception signalled:  ' + str(sys.exc_type)
		sys.exit(1)

	# and print the result

	print "the sum is", c.GetValue()
	sys.exit(0)

main(sys.argv)

