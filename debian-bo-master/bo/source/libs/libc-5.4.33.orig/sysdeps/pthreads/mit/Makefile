#
# Makefile for pthreads
#

TOPDIR=../../..

override STATIC=false
override DEBUG=false
override PROFILE=false
JUMP_LIB=libpthreads

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS= -I../$(TARGET_OS) -I$(TOPDIR)
BASE_CFLAGS:= $(BASE_CFLAGS) -DTARGET_MACHINE=\"$(TARGET_MACHINE)\" \
	-DPTHREAD_KERNEL $(PTHREAD_CFLAGS)
OPT_CFLAGS:= -g
PIC_OPT_CFLAGS:= -fPIC -g

override SHARED_LIB=$(SHARED_DIR)/libpthreads.a


DIRS:= linux

# libpthreads
SRC1S= __pthread_init.c _exit.c cleanup.c cond.c condattr.c dump_state.c \
	errno.c fd.c fd_kern.c fd_pipe.c globals.c info.c \
	mutex.c mutexattr.c prio_queue.c process.c pthread.c \
	pthread_attr.c pthread_detach.c pthread_join.c pthread_kill.c \
	pthread_once.c queue.c schedparam.c select.c sig.c signal.c \
	sleep.c specific.c wait.c mutex_malloc.c file.c pthread_init.c

SRCS=$(SRC1S) $(SRC2S)
ASMS= $(SRC1S:.c=.s) $(SRC2S:.c=.s)
OBJS= $(SRC1S:.c=.o) $(SRC2S:.c=.o)

include $(TOPDIR)/Maketargets
