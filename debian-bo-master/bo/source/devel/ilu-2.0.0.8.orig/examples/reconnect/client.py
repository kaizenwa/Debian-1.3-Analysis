#!/import/python-1.2/s41
#

import sys, os, ilu, T, time, traceback

loophandle = ilu.CreateLoopHandle()

alarm = ilu.CreateAlarm()

def call_m1(obj,f):
	try:
		print "calling m1"
		obj.m1()
		alarm.set(ilu.FineTime_Now() + ilu.FineTime(10), f, (obj,f,))
	except:
		traceback.print_exc()
		ilu.ExitMainLoop(loophandle)

inst = ilu.LookupObject("TestServer", "1", T.P)
if inst == None: 
	print "Could not find server."
	sys.exit(1)
alarm.set(ilu.FineTime_Now() + ilu.FineTime(10), call_m1, (inst,call_m1,))
ilu.RunMainLoop(loophandle)
