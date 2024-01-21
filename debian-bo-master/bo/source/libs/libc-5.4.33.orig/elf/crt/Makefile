#
# Makefile for crt0.o of Linux
#
#

LD=true
MV=true
override STATIC_SHARED=false
override SHARED=false
override DEBUG=false

TOPDIR=../..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

CFLAGS=
DEBUG_CFLAGS=

PROFILE_CFLAGS=
CHECKER_CFLAGS=
CC = $(REALCC)
SED=sed

CRT_CFLAGS=-DGENERATE_SHARED_ELF_CXX_CODE=1

ifeq ($(ELF),true)

crt:: tm.h

crt:: $(ELF_SHARED_DIR)/crtbegin.o $(ELF_SHARED_DIR)/crtend.o \
	$(ELF_SHARED_DIR)/crtbeginS.o $(ELF_SHARED_DIR)/crtendS.o

asm: crtbegin.s crtend.s crtbeginS.s crtendS.s

$(ELF_SHARED_DIR)/crtbeginS.o: crtstuff.c
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		-fpic $(CRT_CFLAGS) \
		-Iconfig -DCRT_BEGIN -finhibit-size-directive \
		-fno-inline-functions -g0 -c $< -o $@

$(ELF_SHARED_DIR)/crtendS.o: crtstuff.c
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		-fpic $(CRT_CFLAGS) \
		-Iconfig -DCRT_END -finhibit-size-directive \
		-fno-inline-functions -g0 -c $< -o $@

$(ELF_SHARED_DIR)/crtbegin.o: crtstuff.c
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		$(CRT_CFLAGS) \
		-Iconfig -DCRT_BEGIN -finhibit-size-directive \
		-fno-inline-functions -g0 -c $< -o $@

$(ELF_SHARED_DIR)/crtend.o: crtstuff.c
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		$(CRT_CFLAGS) \
		-Iconfig -DCRT_END -finhibit-size-directive \
		-fno-inline-functions -g0 -c $< -o $@

crtbeginS.s: crtstuff.c
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		-fpic $(CRT_CFLAGS) \
		-Iconfig -DCRT_BEGIN -finhibit-size-directive \
		-fno-inline-functions -g0 -S $< -o $@

crtendS.s: crtstuff.c
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		-fpic $(CRT_CFLAGS) \
		-Iconfig -DCRT_END -finhibit-size-directive \
		-fno-inline-functions -g0 -S $< -o $@

# That is an ugly hack to work around the gas bug.
crtbegin.s: crtstuff.c
	$(RM) -rf tmp.s
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		$(CRT_CFLAGS) \
		-Iconfig -DCRT_BEGIN -finhibit-size-directive \
		-fno-inline-functions -g0 -S $< -o tmp.s
	$(SED) -e 's/^[ 	]*\(call[ 	]*__do_global_dtors_aux\)[ 	]*$$/	\1@PLT/' tmp.s > $@
	$(RM) -rf tmp.s

# That is an ugly hack to work around the gas bug.
crtend.s: crtstuff.c
	$(RM) -rf tmp.s
	$(CC) -DIN_GCC -DPOSIX  -O2 -fomit-frame-pointer -I. \
		$(CRT_CFLAGS) \
		-Iconfig -DCRT_END -finhibit-size-directive \
		-fno-inline-functions -g0 -S $< -o tmp.s
	$(SED) -e 's/^[ 	]*\(call[ 	]*__do_global_ctors_aux\)[ 	]*$$/	\1@PLT/' tmp.s > $@
	$(RM) -rf tmp.s

else

lib::
	@true

endif

realclean clean:
	$(RM) -f core *.s *.o *.a tmp_make foo

ifeq ($(CANONICAL_TARGET_MACHINE),ix86-linux)
tm.h: config/i386/linuxelf.h
	ln -sf $? $@
endif

ifeq ($(CANONICAL_TARGET_MACHINE),m68k-linux)
tm.h: config/m68k/linux.h
	ln -sf $? $@
endif

depend:
	@echo No dependency in `pwd`.
