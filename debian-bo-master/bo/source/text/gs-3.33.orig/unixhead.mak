#    Copyright (C) 1990, 1991, 1993 Aladdin Enterprises.  All rights reserved.
# 
# This file is part of GNU Ghostscript.
# 
# GNU Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
# anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the GNU Ghostscript General Public License for full details.
# 

# Partial makefile common to all Unix configurations.

# This part of the makefile gets inserted after the compiler-specific part
# (xxx-head.mak) and before gs.mak and devs.mak.

# ----------------------------- Generic stuff ----------------------------- #

# Define the platform name.  For a "stock" System V platform,
# use sysv_ instead of unix_.

PLATFORM=unix_

# Define the syntax for command, object, and executable files.

CMD=
O=-o ./
OBJ=o
XE=

# Define the current directory prefix and command invocations.

CAT=cat
D=/
EXP=./
SHELL=/bin/sh
SH=$(SHELL)
SHP=$(SH) $(EXP)

# Define the arguments for genconf.

CONFILES=-p "%s&s&&" -pl "&-l%s&s&&" -pL "&-L%s&s&&" -ol ld.tr

# Build the VMS MODULES.LIS file on a Unix system.
# (Don't let this become the default target, though.)

unixdefault: default

modules.lis: $(MAKEFILE) genconf$(XE) devs.tr
	$(EXP)genconf @devs.tr -pue "%s" -o modules.lis

# Define the compilation rules and flags.

CCFLAGS=$(GENOPT) $(CFLAGS)

.c.o: $(AK)
	$(CCC) $*.c

CCAUX=$(CC)
CCCF=$(CCC)
CCD=$(CCC)
CCINT=$(CCC)
