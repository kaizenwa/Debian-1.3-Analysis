#
# Makefile for malloc library functions
#

override STATIC=false
override SHARED=false
override STATIC_SHARED=false
override PROFILE=false

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

LIBMCHECK=$(DEBUG_DIR)/libmcheck.a
LIBMCHECK_SRCS=mcheck-init.c
LIBMCHECK_OBJS=$(addprefix $(DEBUG_DIR)/$(SUBDIR)/, $(LIBMCHECK_SRCS:.c=.o))

INC_CFLAGS= -I. -I. -I.
VSCFLAGS=-D_default_morecore=__default_morecore \
	-D__MALLOC_0_RETURNS_NULL -D_morecore=__morecore
BASE_CFLAGS:= $(BASE_CFLAGS) $(VSCFLAGS)

DIRS:=

SRC1S  = malloc.c free.c realloc.c calloc.c \
	valloc.c memalign.c mstats.c mtrace.c mcheck.c
SRC2S = cfree.c

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRCS:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets

ifeq ($(DEBUG),true)

ifeq ($(SPEED),fast)

.PHONY: $(LIBMCHECK)

lib:: $(LIBMCHECK_OBJS) # $(LIBMCHECK)

$(LIBMCHECK): $(LIBMCHECK_OBJS)
	$(AR) uvc $(LIBMCHECK) $?
else
lib:: $(LIBMCHECK)($(LIBMCHECK_OBJS))
endif
endif
