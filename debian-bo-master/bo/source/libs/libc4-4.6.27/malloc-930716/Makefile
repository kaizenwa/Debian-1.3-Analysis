#
# Makefile for malloc library functions
#

override DEBUG=false

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

LIBMCHECK=$(DEBUG_DIR)/libmcheck.a

INC_CFLAGS= -I. -I. -I.
VSCFLAGS=-D_default_morecore=__default_morecore \
	-D__MALLOC_0_RETURNS_NULL -D_morecore=__morecore
BASE_CFLAGS := $(BASE_CFLAGS) $(VSCFLAGS)

DIRS:=

SRC1S  = malloc.c free.c realloc.c calloc.c \
	valloc.c 
SRC2S = cfree.c

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRCS:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)


ifeq ($(DEBUG),true)
lib:: $(LIBMCHECK)

$(LIBMCHECK): mcheck-init.o
	$(RM) -f $(LIBMCHECK)
	$(AR) uvc $(LIBMCHECK) $?
	$(REALRANLIB) $(LIBMCHECK)

endif

include $(TOPDIR)/Maketargets
