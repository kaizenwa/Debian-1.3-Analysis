import multlang, multlang__skel, ilu, sys

# here's the implementation of Squarer

multiplier = None

class Squarer (multlang__skel.Squarer):

	def __init__(self, ih, srvr):
		self.IluInstHandle = ih
		self.IluServer = srvr

	def ObtainSquare (self, val):
		global multiplier

		if ((long(val) * long(val)) > 0xFFFFFFFFL):
			raise multlang.TooBig

		if (not multiplier):
			multiplier = ilu.LookupObject("Server1", "theMultiplierObject", multlang.Multiplier)
		if (not multiplier):
			print "Can't find multiplier object!"
			sys.exit(1)
		try:
			result = multiplier.Multiply(val, val)
		except multlang.TooBig:
			raise multlang.TooBig
		return result



# on loading of this module, create an instance
# of Squarer, and Publish it

s = ilu.CreateServer("Server2");
o = Squarer("theSquarerObject", s);
o.IluPublish()
print "created Squarer object <%s>" % o.IluSBH()
