# server2.py -- a program that runs a Tutorial.Calculator server,
#  which actually returns instances of Tutorial2.TapeCalculator.
#

import ilu, FactoryImpl2, sys

def main(argv):

	if (len(argv) < 2):
		print "Usage:  python server2.py SERVER-ID"
		sys.exit(1)

	theServer = ilu.CreateServer (argv[1])
	theFactory = FactoryImpl2.Factory ("theFactory", theServer)

	# Now make the Factory object "well-known" by publishing it.

	theFactory.IluPublish()

	# Now we print the string binding handle (the object's name plus
	# its location) of the new instance.

	print "Factory2 instance published."
	print "Its SBH is '" + theFactory.IluSBH() + "'"

	loophandle = ilu.CreateLoopHandle()
	ilu.RunMainLoop (loophandle)


main(sys.argv)
