#
# For ELF
#
TOPDIR=..

include $(TOPDIR)/Makeconfig

ELF_LDFLAGS=-shared # using GNU ld
#ELF_LDFLAGS=-G # with SVr4 ld

# Use the same version numbering as the jumptable a.out libraries.
ELFLIBS=libc libm libtermcap libcurses libgdbm libdb

CRTBEGIN=$(ELF_SHARED_DIR)/crtbeginS.o
CRTEND=$(ELF_SHARED_DIR)/crtendS.o

ifeq ($(ELF),true)
lib::
	  ($(MAKE) -C d-link lib);

lib::
	  ($(MAKE) -C crt crt);

lib::
	for l in $(ELFLIBS); do \
	  JUMP_PARAMS=$$l/jump.params; \
	  SHLIB_VERSION=`awk -F= ' { if ($$1 == "Version")  print $$2 }' $$JUMP_PARAMS`; \
	  MAJOR_VERSION=`echo $$SHLIB_VERSION | sed 's/\.[0-9]*\.[0-9]*//'`; \
	  if [ "1" = "1" ]; then \
	    $(CC) $(ELF_LDFLAGS) -B$(ELF_SHARED_DIR)/ \
		-o $(ELF_SHARED_DIR)/$$l.so.$$SHLIB_VERSION \
		-Wl,-soname,$$l.so.$$MAJOR_VERSION \
		$(ELF_SHARED_DIR)/$$l/*.o; \
	  else \
	    $(LD) $(ELF_LDFLAGS) -o $(ELF_SHARED_DIR)/$$l.so.$$SHLIB_VERSION \
		-soname $$l.so.$$MAJOR_VERSION $(CRTBIGIN) \
		$(ELF_SHARED_DIR)/$$l/*.o $(CRTEND); \
	  fi \
	done

# Used for testing purposes - not of any actual value as far as I can tell.
libc_pic.so:
	$(MKDIR) libalias ; cd libalias ;\
	$(AR) x ../../libalias.a ; cd .. ;\
	$(LD) -Bshareable -Bsymbolic -assert nosymbolic
		-o $@.1.1 libc/*.o libalias/*.o

else

lib::
	@true
endif

include $(TOPDIR)/Maketargets

ifeq ($(TARGET_ELF_SO_DIR),/lib/elf)

install:
	if [ ! -d $(TARGET_ELF_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_LIB_DIR); \
	else true; fi
	if [ ! -d $(TARGET_ELF_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_SO_DIR); \
	else true; fi
	cp $(ELF_SHARED_DIR)/*.o $(TARGET_ELF_LIB_DIR)
	for l in $(ELFLIBS) libdl; do \
	  JUMP_PARAMS=$$l/jump.params; \
	  SHLIB_VERSION=`awk -F= ' { if ($$1 == "Version")  print $$2 }' $$JUMP_PARAMS`; \
	  MAJOR_VERSION=`echo $$SHLIB_VERSION | sed 's/\.[0-9]*\.[0-9]*//'`; \
	  lib=$$l.so; \
	  mlib=$$lib.$$MAJOR_VERSION; \
	  flib=$$lib.$$SHLIB_VERSION; \
	  if [ $$l = "libdl" ]; then \
	    mdlinker=ld-linux.so.$$MAJOR_VERSION; \
	    fdlinker=ld-linux.so.$$SHLIB_VERSION; \
	    if [ -f $(TARGET_ELF_SO_DIR)/$$fdlinker ]; then \
	      (cd $(TARGET_ELF_SO_DIR); \
	       cp $$fdlinker /tmp; \
	       if [ $$? -eq 0 ]; then \
	       ln -sf /tmp/$$fdlinker $$mdlinker; \
	       else exit 1; fi; \
	       if [ ! -d backup ]; then \
	         $(MKDIR) backup; \
	       else true; fi; \
	       mv $$fdlinker backup/$$fdlinker.$$$$); \
	    else true; fi; \
	    cp $(ELF_SHARED_DIR)/$$fdlinker $(TARGET_ELF_SO_DIR); \
	    (cd $(TARGET_ELF_SO_DIR); ln -sf $$fdlinker $$mdlinker); \
	  fi; \
	  if [ -f $(TARGET_ELF_SO_DIR)/$$flib ]; then \
	    (cd $(TARGET_ELF_SO_DIR); \
	     cp $$flib /tmp; \
	     if [ $$? -eq 0 ]; then \
	       ln -sf /tmp/$$flib $$mlib; \
	     else exit 1; fi; \
	     if [ ! -d backup ]; then \
	       $(MKDIR) backup; \
	     else true; fi; \
	     mv $$flib backup/$$flib.$$$$); \
	  else true; fi; \
	  cp $(ELF_SHARED_DIR)/$$flib $(TARGET_ELF_SO_DIR); \
	  (cd $(TARGET_ELF_SO_DIR); ln -sf $$flib $$mlib; \
	   ln -sf $$mlib $$lib); \
	  ln -sf $(TARGET_ELF_SO_DIR)/$$lib \
		$(TARGET_ELF_LIB_DIR)/$$lib; \
	done
	(cd $(TARGET_ELF_LIB_DIR); rm -f libdbm.so; \
	 ln -s libgdbm.so libdbm.so)
	$(LDCONFIG) -v -n $(TARGET_ELF_SO_DIR)

else

install:
	if [ ! -d $(TARGET_ELF_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_LIB_DIR); \
	else true; fi
	if [ ! -d $(TARGET_ELF_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_SO_DIR); \
	else true; fi
	cp $(ELF_SHARED_DIR)/*.o $(TARGET_ELF_LIB_DIR)
	for l in $(ELFLIBS) libdl; do \
	  JUMP_PARAMS=$$l/jump.params; \
	  SHLIB_VERSION=`awk -F= ' { if ($$1 == "Version")  print $$2 }' $$JUMP_PARAMS`; \
	  MAJOR_VERSION=`echo $$SHLIB_VERSION | sed 's/\.[0-9]*\.[0-9]*//'`; \
	  lib=$$l.so; \
	  mlib=$$lib.$$MAJOR_VERSION; \
	  flib=$$lib.$$SHLIB_VERSION; \
	  if [ $$l = "libdl" ]; then \
	    mdlinker=ld-linux.so.$$MAJOR_VERSION; \
	    fdlinker=ld-linux.so.$$SHLIB_VERSION; \
	    cp $(ELF_SHARED_DIR)/$$fdlinker $(TARGET_ELF_SO_DIR); \
	    (cd $(TARGET_ELF_SO_DIR); ln -sf $$fdlinker $$mdlinker); \
	  fi; \
	  cp $(ELF_SHARED_DIR)/$$flib $(TARGET_ELF_SO_DIR); \
	  (cd $(TARGET_ELF_SO_DIR); $(RM) -f $$lib; \
	   ln -sf $$mlib $$lib; ln -sf $$flib $$mlib); \
	  (cd $(TARGET_ELF_LIB_DIR); $(RM) -f $$lib; \
	   ln -sf /lib/elf/$$lib $$lib); \
	done
	(cd $(TARGET_ELF_LIB_DIR); rm -f libdbm.so; \
	 ln -s libgdbm.so libdbm.so)

endif

ifeq (.depend,$(wildcard .depend))
include .depend
endif
