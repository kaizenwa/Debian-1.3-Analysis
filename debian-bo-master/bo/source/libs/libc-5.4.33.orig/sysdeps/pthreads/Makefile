#
# Makefile for pthreads
#

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

DIRS:= inet libio linux malloc mit

lib depend realclean clean:
	@for i in $(DIRS); \
	do \
		echo making $@ in $$i; \
		($(MAKE) -C $$i $@) || exit 1; \
	done
