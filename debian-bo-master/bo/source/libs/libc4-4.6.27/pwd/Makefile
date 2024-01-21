#
# Makefile for pwd functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS:=
ifeq ($(NYS),)
SRCS  = fgetpwent.c getpw.c getpwent.c getpwnam.c getpwuid.c \
	putpwent.c pwdopen.c pwdread.c #shadow.c
else
SRCS= putpwent.c getpw.c
endif
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
