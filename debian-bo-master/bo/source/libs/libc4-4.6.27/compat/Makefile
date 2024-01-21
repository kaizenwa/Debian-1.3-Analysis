#
# This is Makefile for some misc functions.
#

override SHARED=true
override STATIC=false
override DEBUG=false
override PROFILE=false
JUMP_LIB = libcompat

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

SHARED_LIB=$(SHARED_DIR)/libcompat/libcompat.a
.PHONY: $(SHARED_LIB)

INC_CFLAGS= -I. -I. -I$(TOPDIR)/misc

DIRS=$(TARGET_ARCH) math

SRC1S= __old_sel.c __old_glob.c __old_gdtbsz.c __undefined.c \
	__warning.c aliases.c __old_regex.c # __old__validuser.c
SRC2S=__old_libc.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
