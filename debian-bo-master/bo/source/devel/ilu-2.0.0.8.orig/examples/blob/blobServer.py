import sys, ilu, BlobExample, BlobExample__skel

class blobserv(BlobExample__skel.blobserv):
	def __init__(self):
		self.clientlist = []
		self.bloblist = []

	def RegisterClient(self, c):
		print 'adding client', c, 'to client list of', self
		self.clientlist.append(c)
		for bl in self.bloblist:
			bl.addvis(c)

	def RemoveClient(self, c):
		for bl in self.bloblist:
			bl.delvis(c)
		self.clientlist.remove(c)

	def NewBlobId(self, x, y):
		n = blobid(x, y)
		self.bloblist.append(n)
		for cl in self.clientlist:
			n.addvis(cl)

	def RemoveBlobId(self, n):
		for client in self.clientlist:
			n.delvis(client)
		self.bloblist.remove(n)

class blobid(BlobExample__skel.blobid):
	def __init__(self, x, y):
		self.my_blobvises = []
		self.c2bv = {}
		self.x = x
		self.y = y
		self.grabbed = None

	def addvis(self, client):
		nbv = client.NewBlobvis(self.x, self.y, self)
		self.my_blobvises.append(nbv)
		self.c2bv[client] = nbv
		if self.grabbed:
			nbv.Grab()

	def delvis(self, client):
		client.RemoveBlobvis(self)
		self.my_blobvises.remove(self.c2bv[client])
		del self.c2bv[client]

	def ReqMove(self, xdelta, ydelta):
		#if bv == self.grabbed:
			for lbv in self.my_blobvises:
				lbv.Move(xdelta, ydelta)
			self.x = self.x + xdelta
			self.y = self.y + ydelta

	def ReqGrab(self, bv):
		if self.grabbed:
			return 0
		else:
			self.grabbed = bv
			for lbv in self.my_blobvises:
				lbv.Grab()
			return 1

	def ReqRelease(self, bv):
		if bv == self.grabbed:
			for lbv in self.my_blobvises:
				lbv.Release()
			self.grabbed = None


loopvar = ilu.CreateLoopHandle()

def main(argv):

	if len(argv) < 2:
		server = ilu.CreateServer ();
	elif len(argv) < 3:
		server = ilu.CreateServer (argv[1])
	elif len(argv) < 4:
		server = ilu.CreateServer (argv[1], None, argv[2])
	else:
		server = ilu.CreateServer (argv[1], argv[3:], argv[2])

	serv = blobserv()
	sbh = serv.IluSBH()
	f = open('t', 'w')
	f.write(sbh)
	f.close()

	print sbh

	ilu.RunMainLoop(loopvar)

main(sys.argv)
