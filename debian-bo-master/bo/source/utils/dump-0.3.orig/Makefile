include ./MCONFIG

SUBDIRS=	compat/lib common dump restore

ifdef	BUILD_RMT
SUBDIRS+=	rmt
endif

all::
	for i in $(SUBDIRS); do $(MAKE) -C $$i || exit 1; done

clean::
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean || exit 1; done

install::
	for i in $(SUBDIRS); do $(MAKE) -C $$i install || exit 1; done

dep depend::
	for i in $(SUBDIRS); do $(MAKE) -C $$i depend || exit 1; done

realclean really-clean clobber spotless::
	for i in $(SUBDIRS); do $(MAKE) -C $$i realclean || exit 1; done

