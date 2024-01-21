TOPDIR=../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

INC_CFLAGS=-I.. -I../$(TARGET_ARCH) -DNO_UNDERSCORE -DVERBOSE_DLINKER

PIC_OPT_CFLAGS=-fPIC -O2 -D__PIC__  # -funroll-loops

SRC2S = dlib.c

SRCS	= $(SRC1S) $(SRC2S)
OBJ1S	= $(SRC1S:.S=.o)
OBJ2S	= $(SRC2S:.c=.o)
OBJS	= $(OBJ1S) $(OBJ2S)

DL_VERSION:=$(shell awk -F= ' { if ($$1 == "Version")  print $$2 }' \
  ../../libdl/jump.params)

DL_MAJOR:=$(shell awk -F= ' { \
  if ($$1 == "Version") { \
    for (i = 1; i <= length ($$2); i++) { \
      if (substr ($$2, i, 1) == ".") { \
	print substr ($$2, 1, i - 1); break; \
      } } } } '  ../../libdl/jump.params)

LIBDL=$(ELF_SHARED_DIR)/libdl.so.$(DL_VERSION)

ELF_LDFLAGS=--shared -nostdlib -nostartfiles # using GNU ld
#ELF_LDFLAGS=-G  # with SVr4 ld

lib:: $(OBJS)
	$(CC) $(ELF_LDFLAGS) -o $(LIBDL) -Wl,-soname -Wl,libdl.so.$(DL_MAJOR) *.o

obj: $(OBJS)

realclean::
	$(RM) -f .depend core *.o *.a *.s *.i tmp_make foo

clean::
	$(RM) -f core *.o *.a *.s *.i tmp_make foo

depend::
	$(CC) $(CFLAGS) -M $(SRCS) | \
		sed -e 's,^[ 	]*\(.*.o\)[ 	]*:,$(STATIC_DIR)/$(SUBDIR)/\1 $(SHARED_DIR)/$(SUBDIR)/\1 $(SHARED_LITE_DIR)/$(SUBDIR)/\1 $(DEBUG_DIR)/$(SUBDIR)/\1 $(PROFILE_DIR)/$(SUBDIR)/\1 $(CHECKER_DIR)/$(SUBDIR)/\1:,' > .depend
#	$(MAKE) subdir TARGET=depend

ifneq ($(DIRS),)
lib realclean clean depend::
	@for i in $(DIRS); do \
	  echo making $@ in $$i; \
	  ($(MAKE) -C $$i $@); \
	done;
endif

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif

