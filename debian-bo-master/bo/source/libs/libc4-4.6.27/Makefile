#
# Makefile for the Linux C library
#

TOPDIR=.

include $(TOPDIR)/Makeconfig

ifeq ($(NYS),true)
NYS_DIR_SUFFIX=-nys
endif

DEPEND_DIRS= bsd cvt ctype curses-bsd4.4 curses-old db des dirent \
	gdbm  grp inet io libio locale login malloc math misc \
	mntent netgroup posix pwd regex rpc setjmp signal stdlib \
	string sysdeps termcap time ufc compat gcc libbsd \
	malloc-930716 $(YPDIR) $(NLSDIR) elf/d-link

CLEAN_DIRS= bsd cvt ctype curses-bsd4.4 curses-old db des dirent \
	gdbm grp inet io libio locale login malloc math misc \
	mntent netgroup posix pwd regex rpc setjmp signal stdlib \
	string sysdeps termcap time ufc compat gcc libbsd \
	malloc-930716 $(YPDIR) $(NLSDIR) sbin jump

ifeq ($(LITE),true)
# for light C library without curses, gdbm
DIRS = bsd cvt ctype des dirent grp inet io libio locale login \
	malloc-930716 misc mntent posix pwd regex rpc setjmp signal \
	stdlib string sysdeps termcap time ufc compat gcc # $(YPDIR)
else
ifeq ($(CHECKER),true)
DIRS= bsd cvt ctype des dirent grp inet io \
	libio locale login math misc mntent netgroup posix pwd regex \
	rpc setjmp signal stdlib string sysdeps termcap time ufc \
	gcc libbsd $(YPDIR) $(NLSDIR) gdbm curses-bsd4.4
else
ifeq ($(ELF),true)
DIRS= bsd cvt ctype des dirent grp inet io libbsd libio locale login \
	math misc mntent netgroup posix pwd regex rpc setjmp signal \
	stdlib string sysdeps time ufc malloc malloc-930716 \
	$(YPDIR) $(NLSDIR) gdbm curses-bsd4.4 termcap db elf
else
ifeq ($(MATH),true)
DIRS= math sysdeps
else
DIRS= bsd cvt ctype curses-bsd4.4 curses-old db des dirent gdbm grp \
	inet io \
	libio locale login malloc math misc mntent netgroup posix \
	pwd regex rpc setjmp signal stdlib string sysdeps termcap \
	time ufc compat gcc libbsd malloc-930716 $(YPDIR) $(NLSDIR)
endif
endif
endif
endif

ifeq ($(SHARED),true)
ifneq ($(ELF),true)
DIRS:= $(DIRS) jump
CLEAN_DIRS:= $(CLEAN_DIRS) jump
endif
endif

all: lib

lib:
	for i in $(DIRS); do \
		echo making $@ in $$i; \
		(cd $$i; $(MAKE) $@); \
	done

depend:
	for i in $(DEPEND_DIRS); do \
		echo making $@ in $$i; \
		(cd $$i; $(MAKE) $@); \
	done

clean realclean:
	$(RM) -f core *.o *.s *.sa *.so.* *.a verify.out
	$(RM) -rf $(OBJS_DIRS)
	for i in $(CLEAN_DIRS); do \
	   echo making $@ in $$i; \
	   (cd $$i; $(MAKE) $@); \
	done

install: install.shared install.static install.debug install.profile

install.lite:
	(cd jump; $(MAKE) install LITE=true)

install.shared:
	(cd jump; $(MAKE) install)

install.static:
	-$(AR) -d static/libc.a __.SYMDEF
	(cd static; $(RM) -rf tmpcopy; $(MKDIR) tmpcopy; \
	 cd tmpcopy; \
	 $(AR) -x ../../libalias.a; \
	 $(AR) $(AR_FLAGS) ../libc.a *.o; \
	 $(RM) *; \
	 $(AR) -x ../../libmalias.a; \
	 $(AR) $(AR_FLAGS) ../libm.a *.o; \
	 cd ..; $(RM) -rf tmpcopy; \
	 for l in *.a; do \
	   $(REALRANLIB) $$l; done; \
	 cp *.a crt0.o $(TARGET_LIB_DIR))
	(cd static; cd $(TARGET_LIB_DIR); \
	 rm -f libdbm.a; \
	 ln -sf libgdbm.a libdbm.a)

install.debug:
	-$(AR) -d debug/libg.a __.SYMDEF
	(cd debug; $(RM) -rf tmpcopy; $(MKDIR) tmpcopy; \
	 cd tmpcopy; \
	 $(AR) -x ../../libalias.a; \
	 $(AR) $(AR_FLAGS) ../libg.a *.o; \
	 cd ..; $(RM) -rf tmpcopy; \
	 for l in *.a; do \
	   $(REALRANLIB) $$l; done; \
	 cp *.a $(TARGET_LIBEXTRA_DIR); \
	 cp libc$(NYS_DIR_SUFFIX)/mcheck-init.o $(TARGET_LIBEXTRA_DIR)/libmcheck.a)

install.profile:
	-$(AR) -d profile/libc_p.a __.SYMDEF
	(cd profile; $(RM) -rf tmpcopy; $(MKDIR) tmpcopy; \
	 cd tmpcopy; \
	 $(AR) -x ../../libalias.a; \
	 $(AR) $(AR_FLAGS) ../libc_p.a *.o; \
	 cd ..; $(RM) -rf tmpcopy; \
	 for l in *.a; do \
	   $(REALRANLIB) $$l; done; \
	 cp *.a gcrt0.o $(TARGET_LIBEXTRA_DIR))

install.elf:
	$(MAKE) -C elf install ELF=true
	$(MAKE) install.elf.real ELF=true

install.elf.real:
	for l in $(ELF_DEBUG_DIR)/lib*.a $(ELF_PROFILE_DIR)/lib*.a \
		$(ELF_STATIC_DIR)/lib*.a; do \
	   $(REALRANLIB) $$l; \
	done
	if [ ! -d  $(TARGET_ELF_LIB_DIR) ]; then \
		$(RM) -f $(TARGET_ELF_LIB_DIR); \
		$(MKDIR) $(TARGET_ELF_LIB_DIR); \
	else true; fi
	if [ ! -d  $(TARGET_ELF_LIBEXTRA_DIR) ]; then \
		$(RM) -f $(TARGET_ELF_LIBEXTRA_DIR); \
		$(MKDIR) $(TARGET_ELF_LIBEXTRA_DIR); \
	else true; fi
	cp $(ELF_STATIC_DIR)/lib*.a $(TARGET_ELF_LIB_DIR)
	cp $(ELF_DEBUG_DIR)/lib*.a $(ELF_PROFILE_DIR)/lib*.a \
		$(ELF_PROFILE_DIR)/*.o $(TARGET_ELF_LIBEXTRA_DIR)
	$(AR) $(AR_FLAGS) $(TARGET_ELF_LIBEXTRA_DIR)/libmcheck.a \
		$(ELF_DEBUG_DIR)/libc/mcheck-init.o
	$(REALRANLIB) $(TARGET_ELF_LIBEXTRA_DIR)/libmcheck.a
	(cd $(TARGET_ELF_LIB_DIR); rm -f libdbm.a; \
	 ln -s libgdbm.a libdbm.a)

install.checker:
	(cd checker; $(MAKE) install)
