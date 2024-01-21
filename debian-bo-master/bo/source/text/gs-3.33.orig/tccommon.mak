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

# tccommon.mak
# Section of MS-DOS makefile common to Turbo C and Turbo/Borland C++,
# MS-DOS and MS Windows.

# This file is used by tc.mak, bc.mak, bcwin.mak, and bcwin32.mak.
# Those files supply the following parameters:
#   Configuration, public:
#	GS_DOCDIR, GS_LIB_DEFAULT, GS_INIT, FEATURE_DEVS, DEVICE_DEVS*
#   Configuration, internal, generic:
#	PLATFORM, MAKEFILE, AK, CC*, DEBUG, NOPRIVATE
#   Configuration, internal, specific to DOS/Windows:
#	TDEBUG, USE_ASM, ASM,
#	COMPDIR, COMP, COMPAUX, WINCOMP, (BGIDIR, BGIDIRSTR), INCDIR, LIBDIR,
#	CPU_TYPE, FPU_TYPE
#	F286, GENOPT, CAOPT

# Make sure we get the right default target for make.

dosdefault: default

# Define a rule for invoking just the preprocessor.

.c.i:
	$(COMPDIR)\cpp -I$(INCDIR) $(CAOPT) $(CCFLAGS) -P- $<

# Define the syntax for command, object, and executable files.

CMD=.bat
O=-o
OBJ=obj
XE=.exe

# Define the current directory prefix and shell invocations.

D=\\

EXP=
SH=
SHP=

# Define the arguments for genconf.

CONFILES=-p %s+ -o obj.tr -l lib.tr

# Define the memory model for Turbo C.  Don't change it!

MM=l

# Define the generic compilation flags.

!if $(CPU_TYPE) >= 400
ASMCPU=/DFOR80386 /DFOR80486
PLATOPT=$(F286) -DFOR80386 -DFOR80486
!elif $(CPU_TYPE) >= 300
ASMCPU=/DFOR80386
PLATOPT=$(F286) -DFOR80386
!elif $(CPU_TYPE) >= 200
ASMCPU=
PLATOPT=$(F286)
!elif $(CPU_TYPE) >= 100
ASMCPU=
PLATOPT=-1
!else
ASMCPU=
PLATOPT=
!endif

!if $(CPU_TYPE) >= 486 || $(FPU_TYPE) >= 287
ASMFPU=/DFORFPU
FPFLAGS=-f287
FPLIB=fp87
!elif $(FPU_TYPE) > 0
ASMFPU=/DFORFPU
FPFLAGS=-f87
FPLIB=fp87
!else
!if $(FPU_TYPE) < 0
ASMFPU=/DNOFPU
!else
ASMFPU=
!endif
FPFLAGS=
FPLIB=emu
!endif

!if $(TDEBUG)
ASMDEBUG=/DDEBUG
!else
ASMDEBUG=
!endif

!if $(USE_ASM)
INTASM=iutilasm.$(OBJ)
PCFBASM=gdevegaa.$(OBJ)
!else
INTASM=
PCFBASM=
!endif

# Define the generic compilation rules.

ASMFLAGS=$(ASMCPU) $(ASMFPU) $(ASMDEBUG)

.asm.obj:
	$(ASM) $(ASMFLAGS) $<;

# -------------------------- Auxiliary programs --------------------------- #

CCAUX=$(COMPDIR)\$(COMPAUX) -m$(MM) -I$(INCDIR) -L$(LIBDIR) -O

echogs$(XE): echogs.c
	$(CCAUX) echogs.c

# If we are running in a Windows environment with a different compiler
# for the DOS utilities, we have to invoke genarch by hand:
!if $(WINCOMP)
genarch$(XE): genarch.c
	$(COMPDIR)\$(COMP) -I$(INCDIR) -L$(LIBDIR) -O genarch.c
	echo Run "win genarch arch.h" then continue make
!else
genarch$(XE): genarch.c
	$(CCAUX) genarch.c
!endif

genconf$(XE): genconf.c
	$(CCAUX) genconf.c

# No special gconfig_.h is needed.
gconfig_.h: tccommon.mak echogs$(XE)
	echogs -w gconfig_.h /* This file deliberately left blank. */

gconfigv.h: tccommon.mak $(MAKEFILE) echogs$(XE)
	$(EXP)echogs -w gconfigv.h -x 23 define USE_ASM -x 2028 -q $(USE_ASM)-0 -x 29
	$(EXP)echogs -a gconfigv.h -x 23 define USE_FPU -x 2028 -q $(FPU_TYPE)-0 -x 29

# ---------------------- MS-DOS I/O debugging option ---------------------- #

dosio_=zdosio.$(OBJ)
dosio.dev: $(dosio_)
	$(SETMOD) dosio $(dosio_)
	$(ADDMOD) dosio -oper zdosio

zdosio.$(OBJ): zdosio.c $(OP) $(store_h)

# ----------------------------- Assembly code ----------------------------- #

iutilasm.$(OBJ): iutilasm.asm
