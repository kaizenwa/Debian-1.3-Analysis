#
# This is Makefile of db for Linux
#

override DEBUG=false
override PROFILE=false
#override CHECKER=false

override SHLIB=libdb
JUMP_LIB=libdb

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

override STATIC_LIB=$(STATIC_DIR)/libdb.a
override SHARED_LIB=$(SHARED_DIR)/libdb.a
override CHECKER_LIB=$(CHECKER_DIR)/libdb.a

DB_CFLAGS= -include $(CROSS_INCDIR)/bsd/bsd.h -D__DBINTERFACE_PRIVATE
BASE_CFLAGS := $(BASE_CFLAGS) $(DB_CFLAGS)

DIRS:=

VPATH=	./hash ./btree ./db ./mpool ./recno ./clib

SRCS=   hash.c hash_bigkey.c hash_buf.c hash_func.c hash_log2.c \
	hash_page.c hsearch.c ndbm.c bt_close.c bt_conv.c bt_debug.c \
	bt_delete.c bt_get.c bt_open.c bt_overflow.c bt_page.c \
	bt_put.c bt_search.c bt_seq.c bt_split.c bt_utils.c \
	db.c mpool.c rec_close.c rec_delete.c rec_get.c rec_open.c \
	rec_put.c rec_search.c rec_seq.c rec_utils.c snprintf.c

SRCS=   hash.c hash_bigkey.c hash_buf.c hash_func.c hash_log2.c \
	hash_page.c hsearch.c ndbm.c bt_close.c bt_conv.c bt_debug.c \
	bt_delete.c bt_get.c bt_open.c bt_overflow.c bt_page.c \
	bt_put.c bt_search.c bt_seq.c bt_split.c bt_utils.c \
	db.c mpool.c rec_close.c rec_delete.c rec_get.c rec_open.c \
	rec_put.c rec_search.c rec_seq.c rec_utils.c

ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
