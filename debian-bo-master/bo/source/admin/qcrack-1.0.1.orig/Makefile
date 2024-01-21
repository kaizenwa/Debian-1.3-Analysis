# Use gcc 2.5.8 if possible.
COMPILER = gcc

# Blank this out if you aren't using a 386+ machine.
D486 = -m486

# **IMPORTANT **
# You must use proper endianness.  Failure to do so will break qinit.
#
# For big endian machines, use -DBIG_ENDIAN.  Make "endian" to find your 
# endianness if you don't know it.
#ENDIANNESS = -DBIG_ENDIAN
ENDIANNESS = -DLITTLE_ENDIAN

OPTS = $(D486) $(ENDIANNESS) -O6 -ffast-math -fstrength-reduce -funroll-loops -fomit-frame-pointer -finline-functions -fexpensive-optimizations -Wall

all:	qinit qcrack qext endian

###############################################################################
##                                                                           ##
##  Do not add any form of dependency compile; it will mess up the misc.h    ##
##  header's exclusion #define                                               ##
##                                                                           ##
###############################################################################

qinit:	qinit.c fcrypt.c misc.c
	$(COMPILER) $(OPTS) -o qinit qinit.c fcrypt.c misc.c

qcrack:	qcrack.c fcrypt.c misc.c
	$(COMPILER) $(OPTS) -o qcrack qcrack.c fcrypt.c misc.c

qext:	qext.c misc.c fcrypt.c
	$(COMPILER) $(OPTS) -o qext qext.c fcrypt.c misc.c

endian:	endian.c
	$(COMPILER) -o endian endian.c

clean:	
	rm -f qcrack qinit qext endian core
