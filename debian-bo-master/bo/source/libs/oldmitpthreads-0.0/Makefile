#
# Makefile for the Linux C library
#

TOPDIR=.

include $(TOPDIR)/Makeconfig

ifeq ($(NYS),true)
NYS_DIR_SUFFIX=-nys
endif

#
ifeq ($(OLD_GCC),true)
LIBGCC=gcc
endif

DEPEND_DIRS= assert bsd cvt ctype des dirent \
	grp inet io libio locale login malloc math misc \
	mntent netgroup posix pwd regex rpc setjmp signal stdlib \
	string sysdeps time ufc libbsd xstring wctype \
	$(LIBGCC) $(MALLOC) $(YPDIR) $(NLSDIR)

CLEAN_DIRS= assert bsd cvt ctype des dirent \
	grp inet io libio locale login malloc math misc \
	mntent netgroup posix pwd regex rpc setjmp signal stdlib \
	string sysdeps time ufc libbsd xstring wctype \
	$(LIBGCC) $(MALLOC) $(YPDIR) $(NLSDIR) sbin

ifeq ($(CHECKER),true)
DIRS= assert bsd cvt ctype des dirent grp inet io \
	libio locale login math misc mntent netgroup posix pwd regex \
	rpc setjmp signal stdlib string sysdeps termcap time ufc \
	xstring wctype \
	libbsd $(LIBGCC) $(MALLOC) $(YPDIR) $(NLSDIR)
else
ifeq ($(ELF),true)
DIRS= assert bsd cvt ctype des dirent grp inet io libbsd libio \
	locale login xstring wctype \
	math misc mntent netgroup posix pwd regex rpc setjmp signal \
	stdlib string sysdeps time ufc malloc $(LIBGCC) $(MALLOC) \
	$(YPDIR) $(NLSDIR) elf
else
ifeq ($(MATH),true)
DIRS= math sysdeps
else
DIRS= assert bsd cvt ctype des dirent grp inet io \
	libio locale login malloc math misc mntent netgroup posix \
	pwd regex rpc setjmp signal stdlib string sysdeps \
	xstring wctype \
	time ufc libbsd $(LIBGCC) $(MALLOC)  $(YPDIR) $(NLSDIR)
endif
endif
endif
DIRS=sysdeps elf

all: lib

lib:
	set -e; for i in $(DIRS); do \
		echo making $@ in $$i; \
		$(MAKE) -C $$i $@; \
	done

depend:
	set -e; for i in $(DEPEND_DIRS); do \
		echo making $@ in $$i; \
		$(MAKE) -C $$i $@; \
	done

clean realclean:
	$(RM) -f core *.o *.s *.sa *.so.* *.a verify.out
	$(RM) -rf $(OBJS_DIRS)
	for i in $(CLEAN_DIRS); do \
	   echo making $@ in $$i; \
	   $(MAKE) -C $$i $@; \
	done

install: install.elf

install.elf:
	$(MAKE) -C elf install ELF=true
	$(MAKE) install.elf.real ELF=true

install.elf.real:
	if [ ! -d  $(TARGET_ELF_LIB_DIR) ]; then \
		$(RM) -f $(TARGET_ELF_LIB_DIR); \
		$(MKDIR) $(TARGET_ELF_LIB_DIR); \
	else true; fi
	if [ ! -d  $(TARGET_ELF_LIBEXTRA_DIR) ]; then \
		$(RM) -f $(TARGET_ELF_LIBEXTRA_DIR); \
		$(MKDIR) $(TARGET_ELF_LIBEXTRA_DIR); \
	else true; fi
	for l in $(ELF_STATIC_DIR)/lib*.a; do \
	   if [ x$(NYS_DIR_SUFFIX)x = "xx" ]; then \
	     name=`basename $$l`; \
	   else \
	     name=`basename $$l | sed s/$(NYS_DIR_SUFFIX)//` ; \
	   fi; \
	   cp $$l $(TARGET_ELF_LIB_DIR)/$$name ; \
	   $(REALRANLIB) $(TARGET_ELF_LIB_DIR)/$$name ; \
	done
	for l in $(ELF_DEBUG_DIR)/lib*.a $(ELF_PROFILE_DIR)/lib*.a; do \
	   if [ x$(NYS_DIR_SUFFIX)x = "xx" ]; then \
	     name=`basename $$l`; \
	   else \
	     name=`basename $$l | sed s/$(NYS_DIR_SUFFIX)//` ; \
	   fi; \
	   cp $$l $(TARGET_ELF_LIBEXTRA_DIR)/$$name ; \
	   $(REALRANLIB) $(TARGET_ELF_LIBEXTRA_DIR)/$$name ; \
	done
	cp $(ELF_PROFILE_DIR)/*.o $(TARGET_ELF_LIBEXTRA_DIR)
	cp $(ELF_DEBUG_DIR)/libc$(NYS_DIR_SUFFIX)/mcheck-init.o \
		$(TARGET_ELF_LIBEXTRA_DIR)/libmcheck.a
	exit 0 || (cd $(TARGET_ELF_LIB_DIR); \
	 rm -f libdbm.a libcurses.a libtermcap.a; \
	 ln -s libncurses.a libcurses.a; \
	 ln -s libncurses.a libtermcap.a; \
	 ln -s libgdbm.a libdbm.a)

install.checker:
	(cd checker; $(MAKE) install)
