#

TOPDIR=../..

include $(TOPDIR)/Makeconfig

JUMP_DIR:=$(TOPDIR)/jump/libc-nys

JUMP_PARAMS=$(JUMP_DIR)/jump.params
SHLIB_NAME:= $(shell awk -F= ' { if ($$1 == "Name")  print $$2 }' $(JUMP_PARAMS))
SHLIB_TEXT:= $(shell awk -F= ' { if ($$1 == "Text")  print $$2 }' $(JUMP_PARAMS))
SHLIB_DATA:= $(shell awk -F= ' { if ($$1 == "Data")  print $$2 }' $(JUMP_PARAMS))
SHLIB_JUMP:= $(shell awk -F= ' { if ($$1 == "Jump")  print $$2 }' $(JUMP_PARAMS))
SHLIB_GOT:= $(shell awk -F= ' { if ($$1 == "GOT")  print $$2 }' $(JUMP_PARAMS))
SHLIB_VERSION:= $(shell awk -F= ' { if ($$1 == "Version")  print $$2 }' $(JUMP_PARAMS))

SHLIB_VERSION_MAJOR:=$(shell awk -F= ' { \
  if ($$1 == "Version") { \
    for (i = 1; i <= length ($$2); i++) { \
      if (substr ($$2, i, 1) == ".") { \
	print substr ($$2, 1, i - 1); break; \
      } } } } ' $(JUMP_PARAMS))

SHLIB_FILE:=$(basename $(SHLIB_NAME)).so.$(SHLIB_VERSION)

SHLIB_FILE_MAJOR:=$(basename $(SHLIB_NAME)).so.$(SHLIB_VERSION_MAJOR)

SYSLIBS:= $(SHARED_DIR)/libgcc/libgcc3.a \
	$(SHARED_DIR)/libcompat/libcompat.a $(TOPDIR)/libalias.a

STUBNAMES=libc libcurses-old libtermcap libgdbm
STUBLIBS:= $(foreach lib, $(STUBNAMES), $(lib).sa)
SHLIBS:= $(foreach lib, $(STUBNAMES), $(lib).a)

STATIC_OBJS=__fpu_control.o __load.o __setfpucw.o set-init.o

all lib:
	(cd $(TOPDIR); for l in *.a; do \
	  $(AR) -d $$l __.SYMDEF; \
	  $(REALRANLIB) $$l; done)
	($(AR) -d $(SHARED_DIR)/libcompat/libcompat.a __.SYMDEF; \
	 $(REALRANLIB) $(SHARED_DIR)/libcompat/libcompat.a; )
	(cd $(SHARED_DIR); for l in *.a; do \
	  $(AR) -d $$l __.SYMDEF; \
	  $(REALRANLIB) $$l; done)
	$(MKIMAGE) -l $(SHLIB_NAME) -v $(SHLIB_VERSION) -a $(SHLIB_TEXT) \
		-d $(SHLIB_DATA) -j $(SHLIB_JUMP)  -g $(SHLIB_GOT) \
		-- $(SHLIBS:%=$(SHARED_DIR)/%) $(SYSLIBS)
	$(MKSTUBS) -l $(SHLIB_NAME) -v $(SHLIB_VERSION) -a $(SHLIB_TEXT) \
		-d $(SHLIB_DATA) -j $(SHLIB_JUMP)  -g $(SHLIB_GOT) \
		-- libc libtermcap libgdbm # $(STUBNAMES)
	# $(VERIFY) -l $(SHLIB_FILE) $(STUBLIBS)
	mv $(SHLIB_FILE) lib.so # do this trick for stupid fs
	$(STRIP) lib.so
	mv lib.so $(SHLIB_FILE)
	$(RM) -rf ./tmpcopy; mkdir tmpcopy; \
	(cd ./tmpcopy ;\
	   $(AR) -x ../$(TOPDIR)/libalias.a; \
	   $(AR) -x ../$(SHARED_DIR)/libc.a  $(STATIC_OBJS); \
	   $(AR) $(AR_FLAGS) ../libc.sa *.o; \
	 cd ..; $(RM) -rf ./tmpcopy;)
	for l in *.sa; do \
	  $(AR) -d $$l __.SYMDEF; \
	  $(REALRANLIB) $$l; \
	done

realclean clean:
	$(RM) -f core *.o *.s *.sa *.so.* *.a verify.out *.log jump.funcs

ifeq ($(TARGET_SO_DIR),/lib)

install:
	if [ ! -d $(TARGET_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_LIB_DIR); \
	else true; fi
	cp *.sa $(TARGET_LIB_DIR)
	if [ ! -d $(TARGET_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_SO_DIR); \
	else true; fi
	if [ -f $(TARGET_SO_DIR)/$(SHLIB_FILE) ]; then \
	  (cd $(TARGET_SO_DIR); \
	   cp $(SHLIB_FILE) /tmp; \
	   if [ $$? -eq 0 ]; then \
	     ln -sf /tmp/$(SHLIB_FILE) $(SHLIB_FILE_MAJOR); \
	   else exit 1; fi; \
	   if [ ! -d backup ]; then \
	     $(MKDIR) backup; \
	   else true; fi; \
	   mv $(SHLIB_FILE) backup/$(SHLIB_FILE).$$$$); \
	else true; fi
	cp $(SHLIB_FILE) $(TARGET_SO_DIR)
	(cd $(TARGET_SO_DIR); \
	   ln -sf $(SHLIB_FILE) $(SHLIB_FILE_MAJOR))
	$(LDCONFIG) -v

else

install:
	if [ ! -d $(TARGET_LIB_DIR) ]; then \
	  $(MKDIR) $(TARGET_LIB_DIR); \
	else true; fi
	cp lib*.sa $(TARGET_LIB_DIR)
	if [ ! -d $(TARGET_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_SO_DIR); \
	else true; fi
	cp $(SHLIB_FILE) $(TARGET_SO_DIR)

endif
