#
# Makefile for Linux specific functions
#

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS=$(TARGET_OS)

ifneq ($(MATH),true)
DIRS := $(DIRS) $(TARGET_ARCH)
endif

lib depend realclean clean:
	@for i in $(DIRS); \
	do \
		echo making $@ in $$i; \
		($(MAKE) -C $$i $@); \
	done
