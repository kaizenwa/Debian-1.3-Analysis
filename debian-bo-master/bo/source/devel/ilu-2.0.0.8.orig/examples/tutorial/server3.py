# server3.py -- a program that runs a Tutorial.Calculator server
#  Puts up a Tk button to kill it with.

# load ilu_tk first, so that the Tk main loop gets set up with ILU
import ilu_tk
# then load the other ilu-dependent modules
import FactoryImpl2, sys, Tkinter, ilu

def main(argv):

	def quit():
		sys.exit(0)

	if (len(argv) < 2):
		print "Usage:  python server3.py SERVER-ID"
		sys.exit(1)

	theServer = ilu.CreateServer (argv[1])
	theFactory = FactoryImpl2.Factory ("theFactory", theServer)
	theFactory.IluPublish()

	# Now we put up a Tk button so that the user can kill the
	#  server by pressing the button

	f = Tkinter.Frame() ; Tkinter.Pack.config(f)
	b = Tkinter.Button (f, {'text' : theFactory.IluObjectID(),\
				'command': quit})
	b.pack ({'side': 'left', 'fill': 'both'})

	# Then we wait in the ilu_tk mainloop, instead of either
	#  the ILU mainloop or the Tkinter mainloop

	ilu_tk.RunMainLoop()


main(sys.argv)
