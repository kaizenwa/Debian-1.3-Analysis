#    Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
# 
# This file is part of GNU Ghostscript.
# 
# GNU Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
# anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the GNU Ghostscript General Public License for full details.
# 

# Partial makefile, common to all Desqview/X configurations.

# This part of the makefile gets inserted after the compiler-specific part
# (xxx-head.mak) and before gs.mak and devs.mak.

# ----------------------------- Generic stuff ----------------------------- #

# Define the platform name.

PLATFORM=dvx_

# Define the syntax for command, object, and executable files.

CMD=.bat
O=-o ./
OBJ=o
XE=.exe

# Define the current directory prefix and command invocations.

CAT=type
D=\\
EXP=
SHELL=
SH=
SHP=

# Define the arguments for genconf.

CONFILES=-p -pl &-l%%s -ol ld.tr

# Define the compilation rules and flags.

CCFLAGS=$(GENOPT) $(CFLAGS)

.c.o: $(AK)
	$(CCC) $*.c

CCCF=$(CCC)
CCD=$(CCC)
CCINT=$(CCC)
