#
# This is Makefile for some misc functions.
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I.

DIRS:=

SRC1S= drand48.c getusersh.c insremque.c \
	syslog.c usleep.c obstack.c ftok.c __getline.c hostid.c \
	lfind.c lsearch.c tsearch.c hsearch.c init-misc.c progname.c \
	basename.c
# ldexp.c
SRC2S = brk.c getdtsz.c gethstnm.c getpgsz.c getrusage.c ioctl.c \
	mknod.c sbrk.c select.c setregid.c setreuid.c \
	utimes.c stdiocompat.c dtoa.c
SRC3S= __environ.S errno.S
SRCS= $(SRC1S) $(SRC2S) $(SRC3S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s) $(SRC3S:.S=.s)
OBJS= $(SRC1S:.c=.o) $(SRC3S:.S=.o)
ALIASES= $(SRC2S:.c=.o)
 
include $(TOPDIR)/Maketargets
