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
# $Id: ilu.py,v 1.14 1996/06/18 03:27:16 head Exp $
#

# Exceptions
from iluRt import		\
    IluGeneralError,		\
    IluProtocolError,		\
    IluUnimplementedMethodError

# Constants
from iluRt import		\
    FALSE,			\
    TRUE,			\
    FineTimeRate,		\
    Version

# Methods
from iluRt import		\
    SetDebugLevel,		\
    SetDebugLevelViaString,	\
    CreateServer,		\
    DefaultServer,		\
    ObjectOfSBH,		\
    LookupObject,		\
    ParseSBH,			\
    IOROfObject,		\
    CallerIdentity,		\
    RegisterInputHandler,	\
    CreateLoopHandle,		\
    RunMainLoop,		\
    ExitMainLoop,		\
    SetMainLoop,		\
    LongReal,			\
    FineTime,			\
    FineTime_Now,		\
    CreateAlarm,                \
    ThreadedOperation

def UnregisterInputHandler (fd):
    RegisterInputHandler (fd, None)

def TypeName(cl):
    return cl._IluClass.name()

def TypeID(cl):
    return cl._IluClass.id()

def FormSBH(objectID, contactInfo):
    return objectID + '@' + contactInfo
