#!/import/python-1.2/s41
#

import os, socket, sys, ilu, T__skel
import time

loophandle = ilu.CreateLoopHandle()

startTime = float(long(time.time()) / 100000 * 100000)

class Hello(T__skel.P):
  def __init__(self, server, ih):
	  self.IluInstHandle = ih
	  self.IluServer = server

  def m1(self):
	  print "m1"
	  ilu.ExitMainLoop(loophandle)

def main(argv):
  server = ilu.CreateServer("TestServer", ["sunrpcrm", "tcp_0_3000"])

  mach = Hello(server, "1")
  print mach.IluSBH()
  mach.IluPublish()
  
  ilu.RunMainLoop(loophandle)

main(sys.argv)
