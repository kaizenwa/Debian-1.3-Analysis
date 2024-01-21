#
# Makefile for Linux specific functions
#

TOPDIR=../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS=crt

lib clean realclean depend::
ifneq ($(DIRS),)
	@set -e; for d in $(DIRS); do \
	  $(MAKE) -C $$d $@; \
	done
else
	@true
endif

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
