#===============================================================
# V Make Configuration file - Version 1.15 - 12/6/96
#
# Copyright (C) 1995,1996  Bruce E. Wampler
#
# This file is part of the V C++ GUI Framework, and is covered
# under the terms of the GNU Library General Public License,
# Version 2. This library has NO WARRANTY. See the source file
# vapp.cxx for more complete information about license terms.
#===============================================================


#---------------------------------------------------------------------
# Version info
#---------------------------------------------------------------------
VV	=	1.16
VVW	=	116


#---------------------------------------------------------------------
# HOMEV info
#---------------------------------------------------------------------
HOMEV	=	$(HOME)/v


#---------------------------------------------------------------------
# Tools used in the makefile execution
#---------------------------------------------------------------------
CC	=	gcc
CXX	=	g++

#---------------------------------------------------------------------
# VPATH for dependencies on header files
#---------------------------------------------------------------------
VPATH=$(HOMEV)/includex/v


#---------------------------------------------------------------------
# Select the architecture of your system.
# These are the architectures that V has been extensively tested with:
# linujx, linuelf, sun4, mips, sgi
# For I486 or i586 choose: intel.
# User contributed definitions are available for:
# hpux, aix, solaris, bsd
#---------------------------------------------------------------------
ARCH	= linux
#ARCH	= linuxelf
#ARCH	= sun4
#ARCH	= mips
#ARCH	= sgi
#ARCH	= hpux
#ARCH	= aix
#ARCH	= solaris
#ARCH	= bsd
Arch = $(ARCH)


#---------------------------------------------------------------------
# Select Athena or Motif TOOLKIT option.
# NOTE: The Motif version is NOT complete!!! It really doesn't
# work yet! However, there are some ifeqs in this makefile
# for compiling Motif. You can try them, but the result does NOT work!
#---------------------------------------------------------------------
#TOOLKIT	= Motif
TOOLKIT	= Athena


#---------------------------------------------------------------------
# Use this define for NO 3D on Athena Widgets
# For Motif use NO 3D i.e. USE_3D=no.
#---------------------------------------------------------------------
ifeq ($(TOOLKIT),Motif)
USE_3D = no
else
USE_3D = yes
endif


#---------------------------------------------------------------------
# Select X11R-version
#---------------------------------------------------------------------
#X11RV	= X11R5
X11RV	= X11R6


#---------------------------------------------------------------------
# Select Debug or no Debug
#---------------------------------------------------------------------
#DEBUG	=	yes
DEBUG	= no


#---------------------------------------------------------------------
# Define filename extensions that are targeted by cleanup's
#---------------------------------------------------------------------
CLEANEXTS= *.bak *~ *.tmp


#---------------------------------------------------------------------
# Architecture dependent directory locations
#---------------------------------------------------------------------
VLibDir	=	$(HOMEV)/lib/$(Arch)
ifeq ($(TOOLKIT),Motif)
oDir	=	$(HOMEV)/objm/$(Arch)
endif
ifeq ($(TOOLKIT),Athena)
oDir	=	$(HOMEV)/obj/$(Arch)
endif
LibDir	=	$(HOMEV)/lib/$(Arch)
Bin	=	$(HOMEV)/bin/$(Arch)


#---------------------------------------------------------------------
# Architecture independent 
#---------------------------------------------------------------------
INCDIR	=	-I$(HOMEV)/includex
LIBDIR	=	-L$(LibDir)
ifeq ($(TOOLKIT),Athena)
	LIBNAME	= libV
	V1NAME 	= v1
	LIBS	= -lV -lXaw
endif
ifeq ($(TOOLKIT),Motif)
	LIBNAME	= libVm
	V1NAME 	= v1m
	LIBS	= -lVm /usr/lib/libXm.a
endif


#---------------------------------------------------------------------
# Linux
#---------------------------------------------------------------------
ifeq ($(Arch),linux)
INCDIR	+=	-I/usr/X11/include
LIBDIR	+=	-L/usr/$(X11RV)/lib
LIBS	+=	-lXmu -lXt -lXext -lX11
TARZ	=	z
RANLIB	=	ranlib
VGPATH	=	LINUX
endif


#---------------------------------------------------------------------
# Linux ELF shared library
#---------------------------------------------------------------------
ifeq ($(Arch),linuxelf)
INCDIR	+=	-I/usr/X11/include
LIBDIR	+=	-L/usr/$(X11RV)/lib
LIBS	+=	-lXmu -lXt -lXext -lX11 -lstdc++
TARZ	=	z
RANLIB	=	ranlib
VGPATH	=	LINUX
endif


#---------------------------------------------------------------------
# Sun4 - unm
#---------------------------------------------------------------------
ifeq ($(Arch),sun4)
INCDIR	+=	-I/usr/local/X11/include
LIBDIR	+=	-L/usr/local/X11/lib
LIBS	+=	-lXaw -lXmu -lXt -lXext -lX11
TARZ	=
RANLIB	=	ranlib
VGPATH	=	UNIX
endif


#---------------------------------------------------------------------
# mips - unm
#---------------------------------------------------------------------
ifeq ($(Arch),mips)
INCDIR	+=	-I/usr/local/X11/include
LIBDIR	+=	-L/usr/local/X11/lib
LIBS	+=	-lXaw -lXmu -lXt -lXext -lX11
TARZ	=
RANLIB	=	ranlib
VGPATH	=	UNIX
endif

#---------------------------------------------------------------------
# sgi
#	- the native compiler on IRIX 5.3 complains ad nauseam
#         about parameters that are declared but not referenced
#         in virtual functions; the -woff turns that warning off
#---------------------------------------------------------------------
ifeq ($(Arch),sgi)
INCDIR	+=	-I/usr/include
LIBDIR	+=	-L/usr/lib
LIBS	+=	-lXmu -lXt -lXext -lX11 -lm
TARZ	=
RANLIB	=	true
VGPATH	=	UNIX
ifneq ($(CXX),g++)
	CFLAGS += -woff 3262
endif
endif

#---------------------------------------------------------------------
# hpux:
# 	- /usr/include is included explicitly because on HP-UX 10.20
#	  mixing of GNU supplied and system supplied includes leads to
#	  problems
#       - X11 is available either in /usr/contrib or /usr/local
#         listing both can not hurt 
#----------------------------------------------------------------------
ifeq ($(Arch),hpux)
INCDIR	+=	-I/usr/include -I/usr/include/$(X11RV) \
		-I/usr/contrib/$(X11RV)/include \
		-I/usr/local/$(X11RV)/include
LIBDIR	+=	-L/usr/lib/$(X11RV) -L/usr/contrib/$(X11RV)/lib \
		-L/usr/local/$(X11RV)/lib
LIBS    +=	-lXmu -lXt -lSM -lICE -lXext -lX11
TARZ	=
RANLIB	=       true
VGPATH	=	UNIX
endif


#----------------------------------------------------------------------
# solaris - unm
#----------------------------------------------------------------------
ifeq ($(Arch),solaris)
INCDIR	+=	-I/usr/local/X11/include -I$(HOMEV)/includex
LIBDIR	+=	-L/usr/local/X11/lib
LIBS	+=	-lXmu -lXt -lXext -lX11 -lnsl -lsocket
TARZ	=
RANLIB	=	true
VGPATH	=	UNIX
endif


#----------------------------------------------------------------------
# FreeBSD with gcc
#----------------------------------------------------------------------
ifeq ($(Arch),bsd)
INCDIR  +=       -I/usr/$(X11RV)/include
LIBDIR  +=       -L/usr/$(X11RV)/lib
LIBS    +=       -lV -lXaw -lXmu -lXt -lXext -lX11
TARZ    =
RANLIB  =       ranlib
VGPATH	=	UNIX
endif


#---------------------------------------------------------------------
# C/C++ compile options
#---------------------------------------------------------------------
#CFLAGS	+= -v $(INCDIR) 
CFLAGS	+= $(INCDIR) 
ifeq ($(DEBUG),no)
CFLAGS	+=	-O
endif
ifeq ($(DEBUG),yes)
CFLAGS	+=	-g
endif
ifeq ($(TOOLKIT),Motif)
CFLAGS	+=	-DMotif
endif
ifeq ($(Arch),sgi)
endif


#---------------------------------------------------------------------
# LINK/LOAD options
#---------------------------------------------------------------------
LDFLAGS	=	$(LIBDIR) $(LIBS)
