#
# For ELF
#
TOPDIR=..

include $(TOPDIR)/Makeconfig

ELF_LDFLAGS=-shared #-Bsymbolic # using GNU ld
#ELF_LDFLAGS=-G # with SVr4 ld

ifeq ($(NYS),true)
NYS_DIR_SUFFIX=-nys
endif

# Use the same version numbering as the jumptable a.out libraries.
BUILD_ELFLIBS=libc libm libpthreads
ELFLIBS=libc libm 

CRTBEGIN=$(ELF_SHARED_DIR)/crtbeginS.o
CRTEND=$(ELF_SHARED_DIR)/crtendS.o

ifeq ($(ELF),true)
#lib::
#	  $(MAKE) -C d-link lib;

ifeq ($(OLD_GCC),true)
LIBGCC=$(ELF_SHARED_DIR)/libgcc.a
lib::
	  $(MAKE) -C crt crt
endif

lib::
	for l in $(BUILD_ELFLIBS); do \
	  JUMP_PARAMS=$$l/jump.params; \
	  SHLIB_VERSION=`awk -F= ' { if ($$1 == "Version")  print $$2 }' $$JUMP_PARAMS`; \
	  MAJOR_VERSION=`echo $$SHLIB_VERSION | sed 's/\..*//'`; \
	  $(CC) $(ELF_LDFLAGS) -B$(ELF_SHARED_DIR)/ \
		-o $(ELF_SHARED_DIR)/$$l.so.$$SHLIB_VERSION \
		-Wl,-soname,$$l.so.$$MAJOR_VERSION \
		-Wl,-whole-archive \
		`case $$l in \
		libc) echo $(ELF_SHARED_DIR)/libc$(NYS_DIR_SUFFIX).a $(LIBGCC);;  \
		libcurses) echo $(ELF_SHARED_DIR)/$$l*.a $(ELF_SHARED_DIR)/libtermcap.so;; \
		*) echo $(ELF_SHARED_DIR)/$$l.a;; \
		esac` -Wl,-no-whole-archive; \
	  (cd $(ELF_SHARED_DIR); \
		rm -f $$l.so; \
		ln -s $$l.so.$$SHLIB_VERSION $$l.so); \
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

ifeq ($(TARGET_ELF_SO_DIR),/lib)

install:
	if [ ! -d $(TARGET_ELF_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_LIB_DIR); \
	else true; fi
	if [ ! -d $(TARGET_ELF_SO_EXTRA_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_SO_EXTRA_DIR); \
	else true; fi
	if [ ! -d $(TARGET_ELF_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_SO_DIR); \
	else true; fi
	cp $(ELF_SHARED_DIR)/*.o $(TARGET_ELF_LIB_DIR)
	for l in $(ELFLIBS); do \
	  JUMP_PARAMS=$$l/jump.params; \
	  SHLIB_VERSION=`awk -F= ' { if ($$1 == "Version")  print $$2 }' $$JUMP_PARAMS`; \
	  MAJOR_VERSION=`echo $$SHLIB_VERSION | sed 's/\..*//'`; \
	  lib=$$l.so; \
	  mlib=$$lib.$$MAJOR_VERSION; \
	  flib=$$lib.$$SHLIB_VERSION; \
	  case $$l in \
	  libdl|libc|libm|libcurses|libncurses|libtermcap) \
		so_dir=$(TARGET_ELF_SO_DIR);; \
	  *) so_dir=$(TARGET_ELF_SO_EXTRA_DIR);; \
	  esac; \
	  if [ $$l = "libdl" ]; then \
	    mdlinker=ld-linux.so.$$MAJOR_VERSION; \
	    fdlinker=ld-linux.so.$$SHLIB_VERSION; \
	    if [ -f $$so_dir/$$fdlinker ]; then \
	      (cd $$so_dir; \
	       cp $$fdlinker /tmp; \
	       if [ $$? -eq 0 ]; then \
	       ln -sf /tmp/$$fdlinker $$mdlinker; \
	       else exit 1; fi; \
	       if [ ! -d backup ]; then \
	         $(MKDIR) backup; \
	       else true; fi; \
	       mv $$fdlinker backup/$$fdlinker.$$$$); \
	    else true; fi; \
	    cp $(ELF_SHARED_DIR)/$$fdlinker $$so_dir; \
	    (cd $$so_dir; ln -sf $$fdlinker $$mdlinker); \
	  fi; \
	  if [ -f $$so_dir/$$flib ]; then \
	    (cd $$so_dir; \
	     cp $$flib /tmp; \
	     if [ $$? -eq 0 ]; then \
	       ln -sf /tmp/$$flib $$mlib; \
	     else exit 1; fi; \
	     if [ ! -d backup ]; then \
	       $(MKDIR) backup; \
	     else true; fi; \
	     mv $$flib backup/$$flib.$$$$); \
	  else true; fi; \
	  cp $(ELF_SHARED_DIR)/$$flib $$so_dir; \
	  chmod 755 $$so_dir/$$flib; \
	  (cd $(TARGET_ELF_SO_EXTRA_DIR); \
	   case $$l in \
	   libc|libm) \
	     ln -sf $$so_dir/$$flib $$l.so; \
	     ;; \
	   libncurses) \
	     ln -sf $$flib libcurses.so.5.0.0; \
	     ln -sf $$flib libtermcap.so.5.0.0; \
	     ;; \
	   libdb) \
	     ln -sf $$flib libdb.so.2.0.4; \
	     ln -sf libdb.so.2.0.4 libdb.so.2; \
	     ;; \
	   libgdbm) \
	     ln -sf $$flib libdbm.so.2.0.4; \
	     ln -sf $$flib libgdbm.so.2.0.4; \
	     ln -sf libdbm.so.2.0.4 libdbm.so.2; \
	     ln -sf libgdbm.so.2.0.4 libgdbm.so.2; \
	     ;; \
	   esac;); \
	done
	$(LDCONFIG) -v

else

install:
	if [ ! -d $(TARGET_ELF_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_LIB_DIR); \
	else true; fi
	if [ ! -d $(TARGET_ELF_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_SO_DIR); \
	else true; fi
	if [ ! -d $(TARGET_ELF_SO_EXTRA_DIR) ]; then \
	  $(MKDIR) $(TARGET_ELF_SO_EXTRA_DIR); \
	else true; fi
	cp $(ELF_SHARED_DIR)/*.o $(TARGET_ELF_LIB_DIR)
	for l in $(ELFLIBS); do \
	  JUMP_PARAMS=$$l/jump.params; \
	  SHLIB_VERSION=`awk -F= ' { if ($$1 == "Version")  print $$2 }' $$JUMP_PARAMS`; \
	  MAJOR_VERSION=`echo $$SHLIB_VERSION | sed 's/\..*//'`; \
	  lib=$$l.so; \
	  mlib=$$lib.$$MAJOR_VERSION; \
	  flib=$$lib.$$SHLIB_VERSION; \
	  case $$l in \
	  libdl|libc|libm|libcurses|libncurses|libtermcap) \
		so_dir=$(TARGET_ELF_SO_DIR);; \
	  *) so_dir=$(TARGET_ELF_SO_EXTRA_DIR);; \
	  esac; \
	  if [ $$l = "libdl" ]; then \
	    mdlinker=ld-linux.so.$$MAJOR_VERSION; \
	    fdlinker=ld-linux.so.$$SHLIB_VERSION; \
	    cp $(ELF_SHARED_DIR)/$$fdlinker $$so_dir; \
	    (cd $$so_dir; ln -sf $$fdlinker $$mdlinker); \
	  fi; \
	  cp $(ELF_SHARED_DIR)/$$flib $$so_dir; \
	  (cd $(TARGET_ELF_SO_EXTRA_DIR); \
	   case $$l in \
	   libc|libm) \
	     $(RM) -f $$l.so; \
	     ln -s $$so_dir/$$flib $$l.so; \
	     ;; \
	   libncurses) \
	     $(RM) -f libcurses.so.5.0.0 \
		libtermcap.so.5.0.0; \
	     ln -s $$flib libcurses.so.5.0.0; \
	     ln -s $$flib libtermcap.so.5.0.0; \
	     ;; \
	   libdb) \
	     $(RM) -f libdb.so.2.0.4 libdb.so.2; \
	     ln -s $$flib libdb.so.2.0.4; \
	     ln -s libdb.so.2.0.4 libdb.so.2; \
	     ;; \
	   libgdbm) \
	     $(RM) -f libdbm.so.2.0.4 libgdbm.so.2.0.4 \
		libdbm.so.2 libgdbm.so.2; \
	     ln -s $$flib libdbm.so.2.0.4; \
	     ln -s $$flib libgdbm.so.2.0.4; \
	     ln -s libdbm.so.2.0.4 libdbm.so.2; \
	     ln -s libgdbm.so.2.0.4 libgdbm.so.2; \
	     ;; \
	   esac;); \
	done

endif

ifeq (.depend,$(wildcard .depend))
include .depend
endif
