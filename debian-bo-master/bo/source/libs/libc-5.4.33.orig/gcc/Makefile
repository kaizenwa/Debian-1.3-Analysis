#
# This is Makefile for libgcc.a
#

override STATIC=false
override DEBUG=false
override PROFILE=false
#override CHECKER=false

JUMP_LIB=libgcc

TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

srcdir=.

ifeq ($(CHECKER),true)
LIBGCC=$(CHECKER_DIR)/libgcc/libgcc3.a
else
LIBGCCDIR=$(SHARED_DIR)/libgcc
endif

LIBGCC1=#libgcc1.a
LIBGCC2=libgcc2.a

CFLAGS = $(WFLAGS) $(OPT_CFLAGS) $(INC_CFLAGS) $(XCFLAGS)

INCLUDES =$(SYSTEM_INC_CFLAGS) -I. -I$(srcdir) -I$(srcdir)/config
INC_CFLAGS=$(INCLUDES)

CCLIBFLAGS= $(OPT_CFLAGS)
LIBGCC2_CFLAGS=$(OPT_CFLAGS) $(DEBUGFLAGS)

DIRS=

ifeq ($(STATIC_SHARED),true)
SHARED=true
endif

ifeq ($(SHARED),true)

SHARED=FALSE

include $(TOPDIR)/Maketargets

ifeq ($(ELF),true)

LIB2FUNCS = _udivdi3 _umoddi3 _udivmoddi4 \
	_op_new _op_vnew _new_handler _op_delete _op_vdel \
	_eh __gcc_bcmp _varargs _shtab _clear_cache \
	_trampoline _pure

LIBGCC2_CFLAGS=$(BASE_OPTFLAGS) $(PIC_OPT_CFLAGS)
else

LIB2FUNCS = _eprintf __main _exit _ctors _op_new _op_delete \
         _new_handler _udivdi3 _umoddi3 _udivmoddi4
# _muldi3 _udivdi3 _umoddi3 _udivmoddi4

LIBGCC2_CFLAGS=$(BASE_OPTFLAGS) $(DEBUGFLAGS) -B$(JUMPAS)

LIBGCCALIASES=$(SHARED_DIR)/$(SUBDIR)/aliases.o

endif

lib::
	@if [ -d $(SHARED_DIR)/$(SUBDIR) ]; then \
	  true; else \
	  $(RM) -f $(SHARED_DIR)/$(SUBDIR); \
	  $(MKDIR) $(SHARED_DIR)/$(SUBDIR); \
	fi
	$(RM) -f libgcc1.a	# safer (it can be the checkered one)

lib all:: libgcc

else

ifeq ($(CHECKER),true)

include $(TOPDIR)/Maketargets

LIB2FUNCS = _eprintf __main _exit _ctors _op_new _op_delete \
         _new_handler _muldi3 _udivdi3 _umoddi3 _udivmoddi4

LIBGCCALIASES=$(CHECKER_DIR)/$(SUBDIR)/aliases.o

LIBGCC2_CFLAGS=$(BASE_OPTFLAGS) $(DEBUGFLAGS) $(CHECKER_OPT_CFLAGS)

lib::
	@if [ -d $(CHECKER_DIR)/$(SUBDIR) ]; then \
	  true; else \
	  $(RM) -f $(CHECKER_DIR)/$(SUBDIR); \
	  $(MKDIR) $(CHECKER_DIR)/$(SUBDIR); \
	fi
	$(RM) -f libgcc1.a	# safer (it can be the static one)

lib all:: libgcc

else

lib all:
	@true

endif
endif

libgcc:: config.h  hconfig.h tconfig.h tm.h

ifeq ($(CANONICAL_TARGET_MACHINE),ix86-linux)

config.h: config/i386/xm-linux.h
	ln -s $? $@

hconfig.h: config/i386/xm-linux.h
	ln -s $? $@

tconfig.h: config/i386/xm-linux.h
	ln -s $? $@

tm.h: config/i386/linux.h
	ln -s $? $@

endif

ifeq ($(CANONICAL_TARGET_MACHINE),m68k-linux)

config.h: config/m68k/xm-linux.h
	ln -s $? $@

hconfig.h: config/m68k/xm-linux.h
	ln -s $? $@

tconfig.h: config/m68k/xm-linux.h
	ln -s $? $@

tm.h: config/m68k/linux.h
	ln -s $? $@

endif

libgcc1.a: libgcc1.c
	-$(RM) -fr tmplibgcc1.a tmpcopy
# Actually build it in tmplibgcc1.a, then rename at end,
# so that libgcc1.a itself remains nonexistent if compilation is aborted.
# -e causes any failing command to make this rule fail.
# -e doesn't work in certain shells, so we test $$? as well.
	-set -e; \
	mkdir tmpcopy; \
	for name in .. $(LIB1FUNCS); \
	do \
	  if [ x$${name} != x.. ]; then \
	    echo $${name}; \
	    $(CC) $(CCLIBFLAGS) $(INCLUDES) -c -DL$${name} \
	    	$(srcdir)/libgcc1.c -o ./tmpcopy/$${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	  else true; fi; \
	done
	(cd ./tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc1.a *.o)
	$(RM) -rf ./tmpcopy
# Some shells crash when a loop has no items.
# So make sure there is always at least one--`..'.
# Then ignore it.
# We don't use -e here because there are if statements
# that should not make the command give up when the if condition is false.
# Instead, we test for failure after each command where it matters.
	-for file in .. $(LIB1FUNCS_EXTRA); \
	do \
	  if [ x$${file} != x.. ]; then \
	    name=`echo $${file} | sed -e 's/[.]c$$//' -e 's/[.]asm$$//'`; \
	    echo $${name}; \
	    if [ $${name}.asm = $${file} ]; then \
	      cp $${file} $${name}.s || exit 1; file=$${name}.s; \
	    else true; fi; \
	    $(CC) $(CCLIBFLAGS) $(INCLUDES) -c $${file}; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	    $(AR) $(AR_FLAGS) tmplibgcc1.a $${name}.o; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	    rm -f $${name}.[so]; \
	  else true; \
	  fi; \
	done
	if [ -f tmplibgcc1.a ]; then \
	  mv tmplibgcc1.a libgcc1.a; \
	else true; fi

# Compiling libgcc2.a requires making sure that cc1, etc. have been compiled.
# But recompiling cc1 should not force recompilation of libgcc2.a.
# If you want to force recompilation, delete libgcc2.a.
# Depend on install-cross-tools to make sure we set up properly 
# to run the assembler before we try compiling anything.
# install-cross-tools does nothing if not cross compiling.
libgcc2.ready: $(GCC_PASSES) $(CROSS_TOOLS)
	-if [ -f libgcc2.ready ] ; then \
		true; \
	else \
		touch libgcc2.ready; \
	fi

libgcc2.a: libgcc2.c
#libgcc2.ready $(CONFIG_H) $(LIB2FUNCS_EXTRA) \
#   machmode.h longlong.h gbl-ctors.h config.status
# Actually build it in tmplibgcc2.a, then rename at end,
# so that libgcc2.a itself remains nonexistent if compilation is aborted.
	-$(RM) -fr tmplibgcc2.a tmpcopy
# -e causes any failing command to make this rule fail.
# -e doesn't work in certain shells, so we test $$? as well.
	set -e; \
	mkdir tmpcopy; \
	for name in $(LIB2FUNCS); \
	do \
	  echo $${name}; \
	  $(CC) $(LIBGCC2_CFLAGS) $(INCLUDES) -c -DL$${name} \
	      $(srcdir)/libgcc2.c -o ./tmpcopy/$${name}.o; \
	  if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	done
	(cd ./tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc2.a *.o)
	$(RM) -rf ./tmpcopy
# Some shells crash when a loop has no items.
# So make sure there is always at least one--`..'.
# Then ignore it.
# We don't use -e here because there are if statements
# that should not make the command give up when the if condition is false.
# Instead, we test for failure after each command where it matters.
	-for file in .. $(LIB2FUNCS_EXTRA); \
	do \
	  if [ x$${file} != x.. ]; then \
	    name=`echo $${file} | sed -e 's/[.]c$$//' -e 's/[.]asm$$//'`; \
	    echo $${name}; \
	    if [ $${name}.asm = $${file} ]; then \
	      cp $${file} $${name}.s || exit 1; file=$${name}.s; \
	    else true; fi; \
	    $(CC) $(LIBGCC2_CFLAGS) $(INCLUDES) -c $${file}; \
	    if [ $$? -eq 0 ] ; then true; else exit 1; fi; \
	    $(AR) $(AR_FLAGS) tmplibgcc2.a $${name}.o; \
	    rm -f $${name}.[so]; \
	  else true; \
	  fi; \
	done
	mv tmplibgcc2.a libgcc2.a
# These lines were deleted from above the mv command
# because ranlibing libgcc.a itself should suffice.
#	-if [ x${HPUX_GAS} = x ] ; then \
#	  if $(RANLIB_TEST) ; then $(RANLIB) tmplibgcc2.a; else true; fi \
#	else true; fi

# Combine the various libraries into a single library, libgcc.a.
libgcc:: $(LIBGCC1) $(LIBGCC2) $(LIBGCCALIASES)
	-$(RM) -rf tmplibgcc.a $(LIBGCC) tmpcopy
	mkdir tmpcopy
	-if [ x$(LIBGCC1) != x ];			\
	then (cd tmpcopy; $(AR) x ../$(LIBGCC1));	\
	else true;					\
	fi
	(cd tmpcopy; $(AR) x ../$(LIBGCC2))
	(cd tmpcopy; $(AR) $(AR_FLAGS) ../tmplibgcc.a *.o)
	$(RM) -rf tmpcopy $(LIBGCC2)
# Actually build it in tmplibgcc.a, then rename at end,
# so that libgcc.a itself remains nonexistent if compilation is aborted.
	$(AR) $(AR_FLAGS) tmplibgcc.a $(LIBGCCALIASES)
	$(REALRANLIB) tmplibgcc.a
	mv tmplibgcc.a $(LIBGCCDIR)/../libgcc.a
	(cd $(LIBGCCDIR); $(AR) -x ../libgcc.a)

clean::
	-rm -f config.h  hconfig.h tconfig.h tm.h

depend::
