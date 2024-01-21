#
# Makefile for xstring functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I.

DIRS:=

SRCS= wcscat.c wcschr.c wcscmp.c wcscpy.c wcscspn.c wcsdup.c wcslen.c wcsncat.c \
      wcsncmp.c wcsncpy.c wcspbrk.c wcsrchr.c wcsspn.c wcstok.c wcsstr.c wmemchr.c \
      wmemcmp.c wmemcpy.c wmemmove.c wmemset.c \
      btowc.c wctob.c mbsinit.c \
      mbrlen.c mbrtowc.c wcrtomb.c mbsrtowcs.c wcsrtombs.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets  
