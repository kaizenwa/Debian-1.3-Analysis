import sys

if '-mt' in sys.argv:
	import thread
	import ilu
	ilu.ThreadedOperation()
else:
	import ilu

import Test1
import Test1__skel
import Test3__skel

singleO2 = None

class O1(Test1__skel.O1):
  def __init__(self, ih, srvr):
    self.IluInstHandle = ih
    self.IluServer = srvr
    self.one = 0

  def U_CSS_to_U(self, u, css):
    print "Test1.O1.U-CSS-to-U"
    return u

  def f_CSS_to_RO(self, css):
    print "Test1.O1.f-CSS-to-R0"
    return Test1.TheR(("one", "two", "three"), ["hi", "there"], 9)

  def R_ScS_to_F(self, r, s):
    print "Test1.O1.R-ScS-to-F"
    return 39.7

  def a_RO(self, ro):
    print "Test1.O1.a-RO"

  def get_O2(self):
    global singleO2
    print "Test1.O1.get-O2"
    if singleO2:
      uc = singleO2
    else:
      try:
        uc = O2()
      except:
        raise Test1.CantCreate
      singleO2 = uc
    return uc

  def get_O3(self, subclass):
    print "Test1.O1.get-O3"
    try:
      if subclass:
	uc = O()
      else:
	if self.one == 0:
	  self.one = 1
	  print "making O3..."
	  uc = O3()
	else:
	  self.one = 0
	  print "making O4..."
	  uc = O4()
    except:
      raise Test1.CantCreate
    return uc

class O2(Test1__skel.O2):
  def OO_A0_to_CSS(self, o, a):
    print "Test1.o2.OO-A0-to-CSS"
    if o == None:
      raise Test1.E2, 7
    return []

  def R_I_A1_to_I_A0(self, r, i, a):
    print "Test1.O2.R-I-A1-to-I-A0"
    ret = [1, 2, 3, 4, 5, 6, 7, 8]
    return ret, i

class O3(Test1__skel.O3):
  def RS_R_to_R_IS(self, r):
    print "Test1.O3.RS-R-to-R-IS"
    print "(Caller is " + str(ilu.CallerIdentity()) + ")"
    r2 = Test1.TheR(("a", "b", "c"), ["just", "a", "string"], -133567)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    print "Test1.O3.O1-U-to-U"
    u = (3, o)
    return u

  def BS_to_I(self, b):
    return len(b) * len(b)

class P(Test1__skel.P):
  def RS_R_to_R_IS(self, r):
    print "Test1.P.RS-R-to-R-IS"
    r2 = Test1.TheR(["from", "P", "string"], ["one", "two"], 25719)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    print "Test1.P.O1-U-to-U"
    u = (3, o)
    return u

  def BS_to_I(self, b):
    return len(b)

  def m2(self, j):
    return [j, j * j]

class O4(Test1__skel.O4):
  def RS_R_to_R_IS(self, r):
    print "Test1.O4.RS-R-to-R-IS"
    r2 = Test1.TheR(["from", "P", "string"], ["three", "four"], 0x7FFFFFF3)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    print "Test1.O4.O1-U-to-U"
    u = (3, o)
    return u

  def BS_to_I(self, b):
    bLen = len(b)
    print "Test1.O4.BS-to-I (%d:" % bLen,
    for i in range(0, 11):
      if i < bLen:
	val = b[i]
      else:
	val = 0
      print "%02x" % val
    print "...) => %d" % bLen
    return bLen

  def R_to_R(self, r):
    r2 = 1020304.05060708
    print "Test1.O4.R_to_R (%.10f) => %.10f" % (r, r2)
    return r2

class O(Test3__skel.O):
  def RS_R_to_R_IS(self, r):
    print "Test3.O.RS-R-to-R-IS"
    r2 = Test1.TheR(["from", "P", "string"], ["three", "four"], -1)
    ret = []
    return ret, r2

  def O1_U_to_U(self, o, u):
    print "Test3.O.O1-U-to-U (", o, ", {%d})" % u[0]
    u = (3, o)
    return u

  def BS_to_I(self, b):
    return len(b) * len(b)

  def SR_to_I(self, i):
    print "Test3.O.SR-to-I(%f)" % i
    return int(i)

  def I_to_Test1U(self, i):
    print "Test3.O.I-to-Test1U(%d)" % i
    return (5, ilu.TRUE)

loopvar = ilu.CreateLoopHandle();

def main():
  instHandle = "Test1_Initial_Object"
  serverId = "Test1-Server"

  s = ilu.CreateServer(serverId)
  uc = O1(instHandle, s)

  uc.IluPublish()
  uc2 = ilu.LookupObject(serverId, instHandle, Test1.O1)
  if uc2 != uc:
    print "*** Error, lookup returns wrong object"
  uc2.IluPublish()

  print "exported", uc.IluSBH()

  ilu.RunMainLoop(loopvar)

main()
