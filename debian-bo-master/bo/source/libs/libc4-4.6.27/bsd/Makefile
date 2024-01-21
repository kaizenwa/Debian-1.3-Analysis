#
# This is Makefile of bsd lib for Linux.
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I. -I$(TOPDIR)

DIRS:=

SRCS = setenv.c ftime.c psignal.c mkstemp.c sigint.c realpath.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets

foo:
	echo $(ELF_STATIC_DIR)
	echo $(STATIC_DIR)

