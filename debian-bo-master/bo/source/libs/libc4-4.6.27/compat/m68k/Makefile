#
# This is Makefile for some misc functions.
#

override SHARED=true
override STATIC=false
override DEBUG=false
override PROFILE=false
JUMP_LIB = libcompat

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

SHARED_LIB=$(SHARED_DIR)/libcompat/libcompat.a
.PHONY: $(SHARED_LIB)

INC_CFLAGS=

DIRS:=

SRCS= __old_sigjmp.c __old__uname.c __old__ghn.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
