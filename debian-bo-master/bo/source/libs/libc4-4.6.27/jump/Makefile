#

TOPDIR=..

include $(TOPDIR)/Makeconfig

CLEAN_DIRS=libc libc.lite libm libc-nys libcurses libdb

ifeq ($(MATH),true)

DIRS=libm

else

ifeq ($(NYS),true)
NYS_DIR_SUFFIX=-nys
endif

ifeq ($(LITE),true)
DIRS=libc.lite
else
DIRS=libc$(NYS_DIR_SUFFIX) libm libcurses libdb
endif

endif

all lib install:
	@for d in $(DIRS); do \
	  echo makeing $@ in $$d; \
	  ($(MAKE) -C $$d $@); \
	done

realclean clean:
	@for d in $(CLEAN_DIRS); do \
	  echo makeing $@ in $$d; \
	  ($(MAKE) -C $$d $@); \
	done

depend:
	@true
