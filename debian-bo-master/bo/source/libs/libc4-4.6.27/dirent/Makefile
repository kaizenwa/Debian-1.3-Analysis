#
# This is Makefile of dirent for Linux
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I.

DIRS:=

SRCS = alphasort.c closedir.c opendir.c rewinddir.c scandir.c \
	seekdir.c telldir.c
ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=

include $(TOPDIR)/Maketargets
