# server.py -- a program that runs a Tutorial.Calculator server
#

import ilu, FactoryImpl, sys

def main(argv):

	if (len(argv) < 2):
		print "Usage:  python server.py SERVER-ID"
		sys.exit(1)

        # Create a kernel server with appropriate server ID, which
        #  is passed in as the first argument

	theServer = ilu.CreateServer (argv[1])

        # Now create an instance of a Factory object on that server,
        #  with the instance handle "theFactory"

	theFactory = FactoryImpl.Factory ("theFactory", theServer)

	# Now make the Factory object "well-known" by publishing it.

	theFactory.IluPublish()

	# Now we print the string binding handle (the object's name plus
	# its location) of the new instance.

	print "Factory instance published."
	print "Its SBH is '" + theFactory.IluSBH() + "'"

	handle = ilu.CreateLoopHandle()
	ilu.RunMainLoop (handle)


main(sys.argv)
