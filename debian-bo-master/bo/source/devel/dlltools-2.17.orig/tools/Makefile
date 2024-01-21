#
# This Makefile is specially tailored for use under Linux.  Most people should
# use this unless one of the other Makefiles is specifically tailored for your
# machine.  This Makefile generally works under SVr4 on a i486 as well,
# although the BINDIR should be adjusted in such cases.
#
# Define DLL if you are building tools for dynamic linking libraries.
# Define LONG_FILENAMES if you do not use the Minix filesystem
#DEFS=-DDLL -DLONG_FILENAMES
BINDIR=/usr/bin
#LIBS=-B/usr/src/qmagic/ -Xlinker \-qmagic -v -lg #To debug NULL dereferences
LIBS=
MISC_OBJS=
CC=gcc
HOST_CC=gcc
#CFLAGS = -g -O  -Wall -I. $(DEFS)
CFLAGS = -g -O2 -Wall -I. $(DEFS)

include Makerules
