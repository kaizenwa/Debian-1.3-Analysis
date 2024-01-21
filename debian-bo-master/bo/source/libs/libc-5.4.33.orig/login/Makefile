#
# Makefile for login functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I.

DIRS:=

SRCS = getlogin.c getpass.c utmp2.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=
 
include $(TOPDIR)/Maketargets
