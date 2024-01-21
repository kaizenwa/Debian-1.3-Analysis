#
# Makefile for crt0.o of Linux
#
#

LD=true
MV=true
override STATIC_SHARED=false
override SHARED=false
override DEBUG=false

TOPDIR=../../../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

CFLAGS=
DEBUG_CFLAGS=
PROFILE_CFLAGS=
CHECKER_CFLAGS=

ifeq ($CHECKER),true)
lib::: $(CHECKER_DIR)/chkrcrt0.o
endif

ifeq ($(PROFILE),true)
lib:: $(PROFILE_DIR)/gcrt0.o
endif

ifeq ($(STATIC),true)
lib:: $(STATIC_DIR)/crt0.o
endif

lib::
	@true

realclean clean:
	$(RM) -f core *.s *.o *.a tmp_make foo

depend:
	@echo No dependency in `pwd`.
