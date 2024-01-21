#
# Copyright (c) 1991, 1992, 1993, 1994 Xerox Corporation.  All Rights Reserved.
#
# Unlimited use, reproduction, and distribution of this software is
# permitted.  Any copy of this software must include both the above
# copyright notice of Xerox Corporation and this paragraph.  Any
# distribution of this software must comply with all applicable United
# States export control laws.  This software is made available AS IS,
# and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
# INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
# PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
# THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
# CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
# XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
#
# $Id: iluRt.py,v 1.13 1996/06/21 02:48:52 janssen Exp $
#

from iluPr import *

class IluObject:

    def IluSBH(self):
	return SBHOfObject(self)

    def IluObjectID(self):
	sbh = self.IluSBH()
	ih, sid, mstid, cinfo = ParseSBH(sbh)
	return (sid, ih,)

    def IluTypeName(self):
	return self._IluClass.name()

    def IluTypeID(self):
	return self._IluClass.id()

    def IluPublish(self):
	PublishObject(self)

    def IluWithdraw(self):
	WithdrawObject(self)

class IluObjSurr(IluObject):

    def IluTrueP(self):
        return 0

    def IluPing(self):
        return PingObject(self)

    def __str__(self):
	    oid = self.IluObjectID()
	    return "<%s:%s/%s>" % (self.IluTypeName(), oid[0], oid[1])
	    
    def __getstate__(self):
      cState = {}
      sbh = self.IluSBH()
      cState["sbh"] = sbh
      return cState

    def __setstate__(self, state):
      sbh = state["sbh"]
      inst = ObjectOfSBH(self.__class__, sbh)
      self._oldsurrinst = inst  ## to prevent against gc
      self._IluInstVars = inst._IluInstVars

    __repr__ = __str__

class IluObjTrue(IluObject):

    def IluTrueP(self):
        return 1

    def IluPing(self):
        return 1

    def __str__(self):
	    oid = self.IluObjectID()
	    return "<%s=%s/%s>" % (self.IluTypeName(), oid[0], oid[1])
	    
    __repr__ = __str__


class IluRecord:

    def __getitem__(self, key):
        return self.__dict__[key]

    def __setitem__(self, key, val):
        self.__dict__[key] = val

    def __str__(self):
        return '<%s:%s>' % (self.__ilu_type_name__, self.__dict__)
		
    __repr__ = __str__


# the following contributed by Scott Hassan and Guido van Rossum
def CaughtUnexpectedException(call):
  UnexpectedException(call)
