#
# This is Makefile of io for Linux
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I. -I$(TOPDIR)

DIRS:=
SRC1S= ftw.c getdirname.c getwd.c lockf.c
SRC2S= access.c chdir.c chmod.c chown.c close.c dup.c dup2.c \
	fchdir.c fchmod.c fchown.c fcntl.c flock.c fstat.c isatty.c link.c \
	lseek.c lstat.c mkdir.c open.c pipe.c read.c readlink.c \
	rmdir.c stat.c symlink.c umask.c unlink.c write.c
SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)
 
include $(TOPDIR)/Maketargets
