#    Copyright (C) 1991, 1995 Aladdin Enterprises.  All rights reserved.
# 
# This file is part of GNU Ghostscript.
# 
# GNU Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
# anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the GNU Ghostscript General Public License for full details.
# 

# wccommon.mak
# Section of Watcom C/C++ makefile common to MS-DOS and MS Windows.
# We strongly recommend that you read the Watcom section of make.doc
# before attempting to build Ghostscript with the Watcom compiler.

# This file is used by watc.mak and watcwin.mak.
# Those files supply the following parameters:
#   Configuration, public:
#	GS_LIB_DEFAULT, GS_INIT, FEATURE_DEVS, DEVICE_DEVS*
#   Configuration, internal, generic:
#	PLATFORM, MAKEFILE, AK, CC*, DEBUG, NOPRIVATE
#   Configuration, internal, specific to DOS/Windows:
#	TDEBUG, USE_ASM, ASM,
#	COMPDIR, INCDIR, LIBPATHS,
#	CPU_TYPE, FPU_TYPE

# We want Unix-compatible behavior.  This is part of it.

.NOCHECK

# Define additional extensions to keep `make' happy

.EXTENSIONS: .be .z

# Define the ANSI-to-K&R dependency.  Watcom C accepts ANSI syntax.

AK=

# Define the extensions for command, object, and executable files.

CMD=.bat
O=-fo=
OBJ=obj
XE=.exe

# Define the current directory prefix and shell invocations.

D=\\

EXPP=dos4gw
SH=
# The following is needed to work around a problem in wmake.
SHP=command /c

# Define the arguments for genconf.

CONFILES=-p FILE&s%s -ol objw.tr

# Define the names of the Watcom C files.
# See the comments in watc.mak and watcwin.mak regarding WCVERSION.

!ifeq WCVERSION 10.0
COMP=$(%WATCOM)\binb\wcc386
LINK=$(%WATCOM)\bin\wlink
WRC=$(%WATCOM)\binb\wrc.exe
!else
!ifeq WCVERSION 9.5
COMP=$(%WATCOM)\bin\wcc386
LINK=$(%WATCOM)\bin\wlinkp
WRC=$(%WATCOM)\binb\wrc.exe
!else
COMP=$(%WATCOM)\bin\wcc386p
LINK=$(%WATCOM)\bin\wlinkp
WRC=$(%WATCOM)\binb\rc.exe
!endif
!endif
INCDIR=$(%WATCOM)\h
WBIND=$(%WATCOM)\binb\wbind.exe

# Define the generic compilation flags.

!ifeq CPU_TYPE 586
FPFLAGS=-fpi87
!else
!ifeq CPU_TYPE 486
FPFLAGS=-fpi87
!else
!ifeq FPU_TYPE 387
FPFLAGS=-fpi87
!else
!ifeq FPU_TYPE 287
FPFLAGS=-fpi287
!else
!ifeq FPU_TYPE -1
FPFLAGS=-fpc
!else
FPFLAGS=-fpi
!endif
!endif
!endif
!endif
!endif

INTASM=
PCFBASM=

# Define the generic compilation rules.

.asm.obj:
	$(ASM) $(ASMFLAGS) $<;

# Make sure we get the right default target for make.

dosdefault: $(GS)$(XE)
	%null

# -------------------------- Auxiliary programs --------------------------- #

echogs$(XE): echogs.$(OBJ)
	echo OPTION STUB=$(STUB) >_temp_.tr
	echo $LIBPATHS >>_temp_.tr
	$(LINK) @_temp_.tr FILE echogs

echogs.$(OBJ): echogs.c

genarch$(XE): genarch.$(OBJ)
	echo $LIBPATHS >_temp_.tr
	$(LINK) @_temp_.tr FILE genarch

genarch.$(OBJ): genarch.c

genconf$(XE): genconf.$(OBJ)
	echo OPTION STUB=$(STUB) >_temp_.tr
	echo OPTION STACK=8k >>_temp_.tr
	echo $LIBPATHS >>_temp_.tr
	$(LINK) @_temp_.tr FILE genconf

genconf.$(OBJ): genconf.c

# No special gconfig_.h is needed.
# Watcom `make' supports output redirection.
gconfig_.h: wccommon.mak
	echo /* This file deliberately left blank. */ >gconfig_.h

gconfigv.h: wccommon.mak $(MAKEFILE) echogs$(XE)
	$(EXP)echogs -w gconfigv.h -x 23 define USE_ASM -x 2028 -q $(USE_ASM)-0 -x 29
	$(EXP)echogs -a gconfigv.h -x 23 define USE_FPU -x 2028 -q $(FPU_TYPE)-0 -x 29

# Define the compilation flags.

!ifneq NOPRIVATE 0
CP=-dNOPRIVATE
!else
CP=
!endif

!ifneq DEBUG 0
CD=-dDEBUG
!else
CD=
!endif

!ifneq TDEBUG 0
CT=-d2
LCT=DEBUG ALL
!else
CT=-d1
LCT=DEBUG LINES
!endif

!ifneq DEBUG 0
CS=
!else
CS=-s
!endif

GENOPT=$(CP) $(CD) $(CT) $(CS)

CCFLAGS=$(GENOPT) $(PLATOPT) $(FPFLAGS) $(CFLAGS) $(XCFLAGS)
CC=$(COMP) -oi -i=$(INCDIR) $(CCFLAGS) -zq
CCC=$(CC)
CCD=$(CC)
CCCF=$(CC)
CCINT=$(COMP) -oit -i=$(INCDIR) $(CCFLAGS)

.c.obj:
	$(CCC) $<
