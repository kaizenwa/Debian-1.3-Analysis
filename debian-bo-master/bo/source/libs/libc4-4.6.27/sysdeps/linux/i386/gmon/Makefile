#
# This is Makefile of gmon for Linux
#
#

TOPDIR=../../../..

override STATIC=false
override SHARED=false
override DEBUG=false
override CHECKER=false

# That is fake for SUBDIR.
JUMP_LIB=libgmon

PROFILE_OPT_CFLAGS=-O6

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

PROFILE_LIB=$(PROFILE_DIR)/libgmon.a

DIRS=
SRCS=gmon.c profil.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
