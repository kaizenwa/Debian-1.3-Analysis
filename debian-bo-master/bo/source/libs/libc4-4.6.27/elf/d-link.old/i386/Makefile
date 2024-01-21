TOPDIR=../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

PIC_OPT_CFLAGS=-fPIC -O2 -D__PIC__ # -funroll-loops

../%.o: %.S
	$(CC) $(PIC_CFLAGS) -c $< -o $@

../%.o: %.c
	$(CC) $(PIC_CFLAGS) -c $< -o $@

INC_CFLAGS=-I.. -I. -DNO_UNDERSCORE -DVERBOSE_DLINKER

SRC1S = resolve.S
SRC2S = elfinterp.c boot1.c

SRCS	= $(SRC1S) $(SRC2S)
OBJ1S	= $(SRC1S:.S=.o)
OBJ2S	= $(SRC2S:.c=.o)
OBJS	= $(OBJ1S) $(OBJ2S)

DLINK_OBJS:= $(addprefix ../, $(OBJS))

lib:: $(DLINK_OBJS)

obj: $(DLINK_OBJS)

asm: $(ASMS)

realclean::
	$(RM) -f .depend core *.o *.a *.s *.i tmp_make

clean::
	$(RM) -f core *.o *.a *.s *.i tmp_make

depend::
	$(CC) $(CFLAGS) -M $(SRCS) | \
		sed -e 's,^[ 	]*\(.*.o\)[ 	]*:,$(STATIC_DIR)/$(SUBDIR)/\1 $(SHARED_DIR)/$(SUBDIR)/\1 $(SHARED_LITE_DIR)/$(SUBDIR)/\1 $(DEBUG_DIR)/$(SUBDIR)/\1 $(PROFILE_DIR)/$(SUBDIR)/\1 $(CHECKER_DIR)/$(SUBDIR)/\1:,' > .depend
#	$(MAKE) subdir TARGET=depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif

