# simple3.py -- a simple client program that finds the Calculator Factory,
#   creates a calculator, and adds up its arguments as before
#
# to run:  python simple4.py ARG [ARG...]

import Tutorial, ilu, sys, string

# We define a new routine, "Get_Tutorial_Calculator", which 
# finds the tutorial factory, then creates a new Calculator
# object for us.

def Get_Tutorial_Calculator (sid, ih):

	# We have to call ilu.LookupObject() with the object ID of
	# the factory object, and the ``type'' of the object we're looking
	# for, which is always available as MODULE.TYPENAME

	f = ilu.LookupObject (sid, ih, Tutorial.Factory)
	if not f:
		print "Can't find Tutorial.Factory instance " + factoryObjectID
		sys.exit(1)
	c = f.CreateCalculator()
	return (c)

def main (argv):

	# A simple program:
	#  1)  make an instance of Tutorial.Calculator
	#  2)  add all the arguments by invoking the Add method
	#  3)  print the resultant value.

	if (len(argv) < 3):
		print "Usage:  python simple3.py FACTORY-OBJECT-SID FACTORY-OBJECT-IH NUMBER [NUMBER...]\n",
		sys.exit(1)

	c = Get_Tutorial_Calculator(argv[1], argv[2])
	if not c:
		print "Couldn't create calculator"
		sys.exit(1)

	# clear the calculator before using it

	c.SetValue (0.0)

	# now loop over the arguments, adding each in turn

	for arg in argv[3:]:
		v = string.atof (arg)
		c.Add (v)

	# and print the result

	print "the sum is " + str(c.GetValue())

	sys.exit (0);  


main(sys.argv)
