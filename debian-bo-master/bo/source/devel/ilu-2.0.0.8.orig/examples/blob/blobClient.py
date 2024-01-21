import sys, ilu, BlobExample, BlobExample__skel, Tkinter, ilu_tk

class blobvis(BlobExample__skel.blobvis):
	def __init__(self, canv, x, y, n, bb):
		self.me = canv.create_oval(x-10, y-10, x+10, y+10,
					   {"fill" : "green"})
		self.my_blobid = n
		self.my_blobboard = bb
		self.canv = canv
		self.grabbed = None
		self.setbindings()

	def setbindings(self):
		self.canv.tag_bind(self.me, "<Any-Enter>",
				   self.attempt_grab)
		self.canv.tag_bind(self.me, "<Any-Leave>",
				   self.attempt_release)

	def attempt_grab(self, event):
		if self.my_blobid.ReqGrab(self):
			self.grabbed = 1
			self.my_blobboard.grabbed_blobvis = self
			self.my_blobboard.reqmovecall = self.my_blobid.ReqMove

	def Grab(self):
		self.canv.itemconfig(self.me, {"fill" : "red"})

	def attempt_release(self, event):
		if self.grabbed:
			self.my_blobid.ReqRelease(self)
			self.grabbed = None
			self.my_blobboard.grabbed_blobvis = None

	def Release(self):
		self.canv.itemconfig(self.me, {"fill" : "blue"})

	#def mousedrag(self, xdelta, ydelta):
	#	#if self.grabbed:
	#		self.my_blobid.ReqMove(xdelta, ydelta, self)

	def Move(self, xdelta, ydelta):
		self.canv.move(self.me, xdelta, ydelta)
		self.canv.update_idletasks()

	def remove(self):
		self.canv.delete(self.me)

class privis:
	def __init__(self, canv, x, y, bb):
		self.me = canv.create_oval(x-10, y-10, x+10, y+10,
					   {"fill" : "green"})
		self.my_blobid = self
		self.x = 20
		self.y = 20
		self.my_blobboard = bb
		self.canv = canv
		self.grabbed = None
		self.setbindings()

	def setbindings(self):
		self.canv.tag_bind(self.me, "<Any-Enter>",
				   self.attempt_grab)
		self.canv.tag_bind(self.me, "<Any-Leave>",
				   self.attempt_release)

	def attempt_grab(self, event):
		self.Grab()
		self.grabbed = 1
		self.my_blobboard.grabbed_blobvis = self
		self.my_blobboard.reqmovecall = self.Move

	def Grab(self):
		self.canv.itemconfig(self.me, {"fill" : "red"})

	def ReqRelease(self, bv):
		self.Release()
		self.grabbed = None

	def attempt_release(self, event):
		if self.grabbed:
			self.my_blobid.ReqRelease(self)
			self.grabbed = None
			self.my_blobboard.grabbed_blobvis = None

	def Release(self):
		self.canv.itemconfig(self.me, {"fill" : "blue"})

	#def mousedrag(self, xdelta, ydelta):
	#	#if self.grabbed:
	#		self.my_blobid.ReqMove(xdelta, ydelta, self)

	def Move(self, xdelta, ydelta):
		self.canv.move(self.me, xdelta, ydelta)

	def remove(self):
		self.canv.delete(self.me)

class BlobBoard(BlobExample__skel.blobcli):
	def mouseDown(self, event):
		id = self.draw.find_withtag("current")
		if not id:
			self.my_server.NewBlobId(event.x, event.y)
		self.lastx = event.x
		self.lasty = event.y

	def mouse3down(self, event):
		if self.grabbed_blobvis:
			print 'removing', self.grabbed_blobvis.my_blobid
	       		self.my_server.RemoveBlobId(self.grabbed_blobvis.
						    my_blobid)

	def mouse2Down(self, event):
		self.my_blobvises[99] = privis(self.draw, event.x, event.y,
					       self)
					       
	def NewBlobvis(self, x, y, n):
		ob = blobvis(self.draw, x, y, n, self)
		self.my_blobvises[n]=ob
		return ob

	def RemoveBlobvis(self, n):
		self.my_blobvises[n].remove()
		del self.my_blobvises[n]
				
	#		self.grabbed_blobvis.mousedrag(event.x - self.lastx, 
	#					       event.y - self.lasty)

	def mouseDrag(self, event):
		if self.grabbed_blobvis:
			self.reqmovecall(event.x - self.lastx, 
					 event.y - self.lasty)
			self.draw.update_idletasks()
		self.lastx = event.x
		self.lasty = event.y

	def quit_routine(self):
		self.my_server.RemoveClient(self)
		ilu_tk.ExitMainLoop()
		self.me.quit()
		sys.exit(0)

	def createWidgets(self):
		self.QUIT = Tkinter.Button(self.me, {'text': 'QUIT', 
					       'fg': 'red', 
					       'command': self.quit_routine})
		self.QUIT.pack({'side': 'bottom', 'fill': 'both'})

		self.draw = Tkinter.Canvas(self.me, 
					   {"width" : "5i", "height" : "5i"})
		self.draw.pack({'side': 'top'})

		Tkinter.Widget.bind(self.draw, "<Button-1>", self.mouseDown)
		Tkinter.Widget.bind(self.draw, "<Button-3>", self.mouse3down)
		Tkinter.Widget.bind(self.draw, "<B1-Motion>", self.mouseDrag)
		Tkinter.Widget.bind(self.draw, "<Button-2>", self.mouse2Down)

	def __init__(self):
		self.me = Tkinter.Frame()
		Tkinter.Pack.config(self.me)
		self.grabbed_blobvis = None
		self.my_blobvises = {}
		# my_blobvisess is a mapping from blobid object to
		# blobvis object.   Used to request that the blobvis
		# object remove itself from the canvas upon blobid deletion.

		self.createWidgets()

		f = open('t', 'r')
		sbh = f.readline()
		print "sbh=", sbh
		f.close()

		self.my_server = ilu.ObjectOfSBH(BlobExample.blobserv, sbh)
		self.my_server.RegisterClient(self)


test = BlobBoard()

ilu_tk.RunMainLoop()
