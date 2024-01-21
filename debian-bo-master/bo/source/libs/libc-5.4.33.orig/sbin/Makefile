#
# Makefile for Linux specific functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

.PHONY: lib

DIRS=lib
LIBS=$(TOPDIR)/libalias.a lib/libc.a
CRT0=./lib/$(TARGET_ARCH)/crt/crt0.o
PROGS=sln

all: lib $(PROGS)

sln: sln.o
	$(LD) -N -dll-verbose -static -o $@ $(CRT0) $? $(LIBS) \
		`gcc -b $(TARGET_MACHINE) --print`

install: $(PROGS)
	@if [ -d $(TARGET_SBIN_DIR) ]; then \
	  echo installing $(PROGS) to $(TARGET_SBIN_DIR)/; \
	  cp $(PROGS) $(TARGET_SBIN_DIR)/; \
	else \
	  echo \"$(TARGET_SBIN_DIR)\" is not a directory!; \
	  exit 1; \
	fi

asm: $(ASMS)

clean::
	$(RM) -f core *.i *.s *.o *.a tmp_make foo $(PROGS)

realclean::
	$(RM) -f core *.i *.s *.o *.a tmp_make foo $(PROGS) .depend

depend::
	$(CC) $(CFLAGS) -M *.c > .depend

ifneq ($(DIRS),)
lib realclean clean depend::
	@set -e; for i in $(DIRS); \
	do \
		echo making $@ in $$i; \
		$(MAKE) -C $$i $@; \
	done
else
lib::
	@true
endif

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
