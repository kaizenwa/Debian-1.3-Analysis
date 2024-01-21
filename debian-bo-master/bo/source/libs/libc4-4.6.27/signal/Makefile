#
# Makefile for signal functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS:=
SRC1S=  sigaddset.c sigdelset.c sigempty.c sigfillset.c \
	sigismem.c
SRC2S= gsignal.c kill.c sigaction.c sigblock.c siggetmask.c \
	sigpause.c sigproc.c sigsetmask.c ssignal.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets  
