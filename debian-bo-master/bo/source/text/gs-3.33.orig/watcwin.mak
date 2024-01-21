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

# makefile for MS-DOS/Watcom C386 platform.
# We strongly recommend that you read the Watcom section of make.doc
# before attempting to build Ghostscript with the Watcom compiler.

# ------------------------------- Options ------------------------------- #

###### This section is the only part of the file you should need to edit.

# ------ Generic options ------ #

# Define the directory that will hold documentation at runtime.

GS_DOCDIR=c:/gs

# Define the default directory/ies for the runtime
# initialization and font files.  Separate multiple directories with \;.
# Use / to indicate directories, not a single \.

GS_LIB_DEFAULT=.;c:/gs\;c:/gs/fonts

# Define the name of the interpreter initialization file.
# (There is no reason to change this.)

GS_INIT=gs_init.ps

# Choose generic configuration options.

# Setting DEBUG=1 includes debugging features (-Z switch) in the code.
# Code runs substantially slower even if no debugging switches are set,
# and also takes about another 25K of memory.

DEBUG=0

# Setting TDEBUG=1 includes symbol table information for the Watcom debugger,
# and also enables stack checking.  Code is substantially slower and larger.

TDEBUG=0

# Setting NOPRIVATE=1 makes private (static) procedures and variables public,
# so they are visible to the debugger and profiler.
# No execution time or space penalty, just larger .OBJ and .EXE files.

NOPRIVATE=0

# Define the name of the executable file.

GS=gswin386

# Define the directory where the IJG JPEG library sources are stored.
# You may have to change this if the IJG library version changes.
# See jpeg.mak for more information.

JSRCDIR=jpeg-5a

# Define any other compilation flags.  Including -DA4 makes A4 paper size
# the default for most, but not, printer drivers.

CFLAGS=

# ------ Platform-specific options ------ #

# Define which version of Watcom C we are using.
# Possible values are 8.5, 9.0, 9.5, or 10.0.
# Unfortunately, wmake can only test identity, not compare magnitudes,
# so the version must be exactly one of those strings.
WCVERSION=10.0

# Define the locations of the libraries.
LIBPATHS=LIBPATH $(%WATCOM)\lib386 LIBPATH $(%WATCOM)\lib386\win

# Define the location of the DOS extender stub.
STUB=$(%WATCOM)\binb\wstub.exe

# Choose platform-specific options.

# Define the processor (CPU) type.  Options are 386,
# 485 (486SX or Cyrix 486SLC), 486 (486DX), or 586 (Pentium).
# Currently the only difference is that 486 and above assume
# the presence of a FPU, and the other processor types do not.

CPU_TYPE=386

# Define the math coprocessor (FPU) type.  Options are 0, 287, or 387.
# If CPU_TYPE is 486 or above, FPU_TYPE is implicitly set to 387,
# since 486DX and later processors include the equivalent of an 80387 on-chip.
# A non-zero option means that the executable will only run if a FPU
# of that type (or higher) is available: this is NOT currently checked
# at runtime.
#   Code is significantly faster.

FPU_TYPE=0

# ---------------------------- End of options ---------------------------- #

# Define the platform name.

PLATFORM=watcwin_

# Define the name of the makefile -- used in dependencies.

MAKEFILE=watcwin.mak

# Define additional platform compilation flags.

PLATOPT=-zW

!include wccommon.mak

# ------ Devices and features ------ #

# Choose the language feature(s) to include.  See gs.mak for details.
# Since we have a large address space, we include the optional features.

FEATURE_DEVS=level2.dev

# Choose the device(s) to include.  See devs.mak for details.

DEVICE_DEVS=
DEVICE_DEVS3=deskjet.dev djet500.dev laserjet.dev ljetplus.dev ljet2p.dev ljet3.dev ljet4.dev
DEVICE_DEVS4=cdeskjet.dev cdjcolor.dev cdjmono.dev cdj550.dev pj.dev pjxl.dev pjxl300.dev
DEVICE_DEVS6=epson.dev eps9high.dev ibmpro.dev bj10e.dev bj200.dev
DEVICE_DEVS8=pcxmono.dev pcxgray.dev pcx16.dev pcx256.dev pcx24b.dev
DEVICE_DEVS9=psmono.dev bit.dev bitrgb.dev bitcmyk.dev
DEVICE_DEVS10=tiffcrle.dev tiffg3.dev tiffg32d.dev tiffg4.dev tifflzw.dev tiffpack.dev

!include gs.mak
!include jpeg.mak
!include devs.mak

# -------------------------------- Library -------------------------------- #

# The Watcom C platform

watc__=gp_iwatc.$(OBJ) gp_msdos.$(OBJ) gp_nofb.$(OBJ) gp_dosfs.$(OBJ) gp_dosfe.$(OBJ)
watcwin_.dev: $(watc__)
	$(SETMOD) watcwin_ $(watc__)

gp_iwatc.$(OBJ): gp_iwatc.c $(stat__h) $(string__h) $(gx_h) $(gp_h)

# ----------------------------- Main program ------------------------------ #

BEGINFILES=*.err gs*.ico gs*.res
# The Watcom compiler doesn't recognize wildcards;
# we don't want any compilation to fail.
CCBEGIN=for %%f in (gs*.c gx*.c z*.c) do $(CCC) %%f

LIBDOS=$(LIB_ALL) gp_iwatc.$(OBJ) gp_msdos.$(OBJ) gp_nofb.$(OBJ) gp_dosfs.$(OBJ) objw.tr

# Interpreter main program

ICONS=gsgraph.ico gstext.ico

GS_ALL=gs.$(OBJ) $(INT_ALL) $(INTASM)\
  $(LIBDOS) objw.tr objwl.tr $(GS).res $(ICONS)

# Make the icons from their text form.

gsgraph.ico: gsgraph.icx echogs$(XE)
	echogs -wb gsgraph.ico -n -X -r gsgraph.icx

gstext.ico: gstext.icx echogs$(XE)
	echogs -wb gstext.ico -n -X -r gstext.icx

$(GS).res: $(GS).rc $(ICONS)
	$(WRC) -i$(INCDIR) -r $(GS)

objwl.tr: $(MAKEFILE)
	echo SYSTEM WIN386 >objwl.tr
	echo OPTION STACK=8k >>objwl.tr

$(GS)$(XE): $(GS_ALL) $(DEVS_ALL) objwl.tr
	$(LINK) $(LCT) NAME $(GS) OPTION MAP=$(GS) FILE gs @objw.tr @objwl.tr
	$(WBIND) $(GS) -R $(GS).res
	rm $(GS).rex $(GS).res
