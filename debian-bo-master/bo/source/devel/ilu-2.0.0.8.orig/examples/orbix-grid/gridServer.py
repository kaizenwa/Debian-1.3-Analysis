import ilu, sys, grid, grid__skel

mainloopvar = ilu.CreateLoopHandle()

class real_grid (grid__skel.grid):

	def __init__(self, width, height, name=None, srvr=None):
		self.IluInstHandle = name
		self.IluServer = srvr
		self.width = width;
		self.height = height;
		self.values = {}

	def _get_height (self):
		return self.height

	def _get_width (self):
		return self.height

	def set (self, row, column, val):
		if (row < 0 or row >= self.height or column < 0 or column >= self.width):
			return
		key = "%d,%d" % (row, column)
		self.values[key] = val

	def get (self, row, column):
		if (row < 0 or row >= self.height or column < 0 or column >= self.width):
			return 0
		key = "%d,%d" % (row, column)
		if (self.values.has_key(key)):
			return self.values[key]
		else:
			return 0

def main():

  instHandle = "i1"
  serverID = "gridexample.somemachine.somedept.somecompany.com"

  s = ilu.CreateServer(serverID, ("tcp_0_0",), "iiop_1_0_1")
  uc = real_grid (10, 10, instHandle, s)

  uc.IluPublish()

  print ilu.IOROfObject(uc)

  ilu.RunMainLoop(mainloopvar)

if __name__ == '__main__':
	main()
