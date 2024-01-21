#
# This is Makefile of gdbm 1.5 for Linux
#

override DEBUG=false
override PROFILE=false
#override CHECKER=false
JUMP_LIB=libgdbm

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

override STATIC_LIB=$(STATIC_DIR)/libgdbm.a
override SHARED_LIB=$(SHARED_DIR)/libgdbm.a
override CHECKER_LIB=$(CHECKER_DIR)/libgdbm.a

INC_CFLAGS= -I. -I$(TOPDIR)

DIRS:=

SRCS =	dbminit.c delete.c fetch.c store.c seq.c close.c \
	dbmopen.c dbmdelete.c dbmfetch.c dbmstore.c dbmseq.c \
	dbmclose.c dbmdirfno.c dbmpagfno.c dbmrdonly.c \
	gdbmopen.c gdbmdelete.c gdbmfetch.c  gdbmstore.c gdbmclose.c \
	gdbmreorg.c gdbmseq.c gdbmsync.c gdbmerrno.c gdbmexists.c \
	gdbmsetopt.c bucket.c falloc.c findkey.c global.c hash.c update.c \
	version.c

SMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
