#

TOPDIR=../..

include $(TOPDIR)/Makeconfig

JUMP_DIR:=$(TOPDIR)/jump/libc.lite

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

STUBNAMES=libc libtermcap
STUBLIBS:= $(foreach lib, $(STUBNAMES), $(lib).sa)
SHLIBS:= $(foreach lib, $(STUBNAMES), $(lib).a)

all lib:
	(cd $(TOPDIR); for l in *.a; do \
	 $(AR) -d $$l __.SYMDEF; \
	 $(REALRANLIB) $$l; done)
	($(AR) -d $(SHARED_DIR)/libcompat/libcompat.a __.SYMDEF; \
	 $(REALRANLIB) $(SHARED_DIR)/libcompat/libcompat.a; )
	(cd $(SHARED_DIR); for l in *.a; do \
	 $(AR) -d $$l __.SYMDEF; \
	 $(REALRANLIB)  $$l; done)
	$(MKIMAGE) -l $(SHLIB_NAME) -v $(SHLIB_VERSION) -a $(SHLIB_TEXT) \
		-d $(SHLIB_DATA) -j $(SHLIB_JUMP)  -g $(SHLIB_GOT) \
		-- $(SHLIBS:%=$(SHARED_DIR)/%) $(SYSLIBS)
	mv $(SHLIB_FILE) lib.so # do this trick for stupid fs
	$(STRIP) lib.so
	mv lib.so $(SHLIB_FILE)

realclean clean:
	$(RM) -f core *.o *.s *.sa *.so.* *.a verify.out *.log

install:
	if [ ! -d $(TARGET_SO_DIR) ]; then \
	  $(MKDIR) $(TARGET_SO_DIR); \
	else true; fi
	cp $(SHLIB_FILE) $(TARGET_SO_DIR)
