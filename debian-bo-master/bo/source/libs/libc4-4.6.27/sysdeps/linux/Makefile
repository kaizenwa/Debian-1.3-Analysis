#
# Makefile for Linux specific functions
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I./$(TARGET_ARCH) -I$(TOPDIR)
BASE_CFLAGS:= $(BASE_CFLAGS) -DLDSO_ADDR=$(LDSO_ENTRY) \
	-DTARGET_MACHINE=\"$(TARGET_MACHINE)\"

ifeq ($(MATH),true)

lib all:
	($(MAKE) -C $(TARGET_ARCH) $@)

else	# the math library

ifneq ($(ELF),true)
OPT_SRC1S=__load.c 
endif

DIRS=$(TARGET_ARCH)
SRC1S= __bsd_sig.c __flock.c __fpathconf.c __getdtsz.c \
	__gethstnm.c __getpgsz.c __isatty.c __pathconf.c \
	__sigblock.c __siggtmsk.c __sigpause.c __sigstmsk.c \
	__sysconf.c __tcgetatr.c __utimes.c _errlist.c _siglist.c \
	getdnnm.c mkfifo.c morecore.c nlist.c setegid.c \
	seteuid.c signal.c \
	__adjtime.c __ntpgttm.c __wait.c __wait3.c \
	__waitpid.c accept.c bind.c connect.c getpeernam.c \
	getsocknam.c getsockopt.c listen.c mmap.c \
	shmat.c shmctl.c shmdt.c shmget.c \
	msgctl.c msgget.c msgrcv.c msgsnd.c \
	semctl.c semget.c semop.c \
	recv.c revcfrom.c send.c sendto.c setpgrp.c setsockopt.c \
	shutdown.c socket.c socketpair.c tell.c ulimit.c \
	set-init.c $(OPT_SRC1S)
SRC2S=	__access.S __chdir.S __chmod.S __chown.S __close.S __dup.S \
	__dup2.S __execve.S __fchmod.S __fchdir.S __fchown.S __fcntl.S \
	__fork.S __fstat.S __fstatfs.S __getegid.S __geteuid.S \
	__getgid.S __getgrps.S __getitmr.S __getpid.S __getppid.S \
	__getrusag.S __gettod.S __getuid.S __ioctl.S __kill.S \
	__link.S __lseek.S __lstat.S __mkdir.S __mknod.S __open.S \
	__pipe.S __read.S __readlink.S __rmdir.S __setgid.S \
	__setitmr.S __setregid.S __setreuid.S __setsid.S __settod.S \
	__setuid.S __sigproc.S __stat.S __statfs.S \
	__symlink.S __times.S __umask.S __uname.S __unlink.S \
	__wait4.S __write.S acct.S alarm.S chroot.S creat.S \
	fsync.S ftruncate.S getpgrp.S getrlimit.S idle.S ioperm.S \
	iopl.S mount.S munmap.S nice.S pause.S reboot.S \
	rename.S setdnnm.S setgroups.S sethstnm.S setpgid.S \
	setprio.S setrlimit.S sigpending.S stime.S swapoff.S \
	swapon.S sync.S time.S truncate.S umount.S uselib.S \
	ustat.S utime.S vhangup.S vm86.S mprotect.S \
	__adjtimex.S getpgid.S __ipc.S
SRC3S= fstatfs.c statfs.c uname.c __vfork.c vfork.c

SRCS=$(SRC1S) $(SRC2S) $(SRC3S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.S=.s) $(SRC3S:.c=.s)
OBJS= $(SRC1S:.c=.o) $(SRC2S:.S=.o)
ALIASES=$(SRC3S:.c=.o)

include $(TOPDIR)/Maketargets

endif	# the math library
