#$Id: Make2.CC,v 11.22 96/02/18 11:41:21 al Exp $
VPATH = .:..

# starting point for cfront based "CC" compilers

CCC = CC
CCFLAGS = -DNDEBUG -DUNIX -O
LIBS = -lcomplex -lm
LDFLAGS =

.SUFFIXES : .o .cc
.cc.o:; $(CCC) $(CCFLAGS) -c $<
#------------------------------------------------------------
# If you are porting and using a cfront based compiler,
# you should use this file as a start.
# If you get errors or warnings, make a copy of it and modify that.
# Warnings are turned off here because many of the supplied headers
# make their own warnings.
# If the port is non-trivial, check with me first because someone else
# may have already done it.
# If it works without changes, send me a note so I can put it in the docs.
# Please send the changes to al@atd.rochester.ny.us
#------------------------------------------------------------
# SGI
# There will be warnings of function arguments not used and temporaries
# used for non-const CS& argument
#------------------------------------------------------------
#------------------------------------------------------------
