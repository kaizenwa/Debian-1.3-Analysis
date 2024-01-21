#
# Makefile for locale functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

VSCFLAGS= -DUSE_ISO_8859_1 -DUSE_KOI_8

INC_CLAGS= -I.
BASE_CFLAGS := $(BASE_CFLAGS) $(VSCFLAGS)

DIRS:=
SRCS = C-collate.c C-ctype.c C-ctype_ct.c C-ctype_mb.c \
	C-monetary.c C-numeric.c C-response.c C-time.c setlocale.c \
	localeconv.c ISO88591-ctype.c ISO88591_ct.c ISO88591_mb.c \
	KOI8-ct.c KOI8-ctype.c KOI8-mb.c

ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
