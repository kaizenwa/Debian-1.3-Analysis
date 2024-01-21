#
# Makefile for grp functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I. -I$(TOPDIR)

DIRS:=
ifeq ($(NYS),)
SRCS= fgetgrent.c getgrent.c getgrgid.c getgrnam.c grpopen.c \
	grpread.c initgroups.c # gshadow.c
else
SRCS=
endif
SMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
