#
# Makefile for Linux specific functions
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

LIB:=libc.a

DIRS=$(TARGET_ARCH)

INC_CFLAGS= -I$(TOPDIR)/sysdeps/linux/$(TARGET_ARCH) -I$(TOPDIR)

SRCS=__stat.c __lstat.c __symlink.c __link.c __main.c \
	__environ.c errno.c __unlink.c
OBJS= $(SRCS:.c=.o)
ASMS= $(SRCS:.c=.s)

lib:: $(LIB)

$(LIB): $(OBJS)
	$(AR) uvc $(LIB) $?
	-$(AR) -d $(LIB) __.SYMDEF
	$(REALRANLIB) $(LIB)

asm: $(ASMS)

obj: $(OBJS)

clean realclean::
	$(RM) -f core *.o *.a *.s *.i tmp_make foo

depend::
	$(CC) -M $(CFLAGS) $(SRCS) > .depend

ifneq ($(DIRS),)
lib clean realclean depend::
	for d in $(DIRS); do \
	  ($(MAKE) -C $$d $@); \
	done
endif

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
