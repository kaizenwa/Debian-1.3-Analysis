#
# Makefile for POSIX functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

# If GETOPT_COMPAT is defined, `+' as well as `--' can introduce a
# long-named option.  Because this is not POSIX.2 compliant, it is
#  being phased out.
# COMPAT_FLAGS = -DGETOPT_COMPAT

BASE_CFLAGS:= $(BASE_CFLAGS) $(COMPAT_FLAGS)

DIRS:=

SRC1S= cfmakeraw.c cfsetget.c clock.c confstr.c ctermid.c cuserid.c \
	execl.c execle.c execlp.c execv.c execvp.c \
	fnmatch.c getenv.c getenvsec.c \
	getopt.c getopt1.c killpg.c mktemp.c \
	putenv.c raise.c sleep.c system.c tcdrain.c \
	tcflow.c tcflush.c tcgetpgrp.c tcsendbrk.c \
	tcsetattr.c tcsetpgrp.c #readv.c writev.c
SRC2S= environ.c execve.c fork.c fpathconf.c getegid.c geteuid.c \
	getgid.c getgrps.c getpid.c getppid.c getuid.c pathconf.c \
	setgid.c setsid.c setuid.c sysconf.c \
	tcgetattr.c times.c wait.c wait3.c wait4.c waitpid.c \
	setfsgid.c setfsuid.c

SRCS= $(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o)
ALIASES= $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
