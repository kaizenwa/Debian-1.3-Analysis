import ilu, cubit__skel

mainloopvar = ilu.CreateLoopHandle()
False = 0
True = 1

def trunc(x, n=32, s=1):
	# this function was written by Guido van Rossum...
	# it attempts to truncate a value the same way C does
	bit = 1L<<n
	x = x%bit
	if s and x & (bit>>1):
		x = x-bit
	return int(x)

class realCubit (cubit__skel.Cubit):

	def __init__(self, ih, srvr=None):
		self.IluInstHandle = ih
		self.IluServer = srvr

	def cube_short (self, s):
		val = trunc(s * s * s, 16, True)
		print 'cube_short(%s) => %s' % (s, val)
		return val

	def cube_long (self, l):
		val = trunc(l * l * l, 32, True)
		print 'cube_long(%s) => %s' % (l, val)
		return val

	def cube_octet (self, o):
		val = trunc(o * o * o, 8, False)
		print 'cube_octet(%s) => %s' % (o, val)
		return val

	def cube_struct (self, s):
		val = {'o' : trunc((s['o'] * s['o'] * s['o']), 8, False),
		       'l' : trunc((s['l'] * s['l'] * s['l']), 32, True),
		       's' : trunc((s['s'] * s['s'] * s['s']), 16, True) }
		print 'cube_struct(%s) => %s' % (s, val)
		return val

	def please_exit (self):
		global mainloopvar
		print 'please_exit'
		ilu.ExitMainLoop(mainloopvar)

def main():
  instHandle = "cubit"
  serverID = "cubit-server"

  s = ilu.CreateServer(serverID, ("tcp_0_0",), "iiop_1_0_1")
  uc = realCubit(instHandle, s)

  uc.IluPublish()

  print ilu.IOROfObject(uc)

  ilu.RunMainLoop(mainloopvar)

main()
