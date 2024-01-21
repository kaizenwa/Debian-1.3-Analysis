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
CC = $(REALCC)

ifeq ($(ELF),true)

lib:: $(ELF_SHARED_DIR)/crt1.o \
	$(ELF_SHARED_DIR)/crti.o $(ELF_SHARED_DIR)/crtn.o \
	$(ELF_PROFILE_DIR)/gcrt1.o

$(ELF_SHARED_DIR)/crt1.o: crt1.S crt0.S
$(ELF_PROFILE_DIR)/gcrt1.o: gcrt1.S gcrt0.S

else

ifeq ($(CHECKER),true)
lib:: $(CHECKER_DIR)/chkrcrt0.o
endif

ifeq ($(PROFILE),true)
lib:: $(PROFILE_DIR)/gcrt0.o
endif

ifeq ($(STATIC),true)
lib:: $(STATIC_DIR)/crt0.o
endif

endif


lib::
	@true

realclean clean:
	$(RM) -f core *.s *.o *.a tmp_make foo

depend:
	@echo No dependency in `pwd`.
