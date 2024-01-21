PASSENV  = $(EXTRA_XINCLUDE_DEFINE) $(EXTRA_XLIB_DEFINE) $(FLEX_DEFINE) \
	CC='$(CC)' CFLAGS='$(CFLAGS)' FOIL='$(FOIL)'                    \
	INSTALL='$(INSTALL)' LDFLAGS='$(LDFLAGS)'                       \
	NO_X_FOR_MF='$(NO_X_FOR_MF)' USE_NCURSES='$(USE_NCURSES)'       \
	bindir='$(bindir)' platform='$(platform)' prefix='$(prefix)'    \
	CONFFLAGS='$(CONFFLAGS)' MAKE='$(MAKE)'

install all:
	(cd src; $(MAKE) $(PASSENV) $@)

distclean clean:
	(cd src; $(MAKE) $(PASSENV) $@)
	(cd test; $(MAKE) $(PASSENV) $@)
	(cd paper; $(MAKE) $(PASSENV) $@)
