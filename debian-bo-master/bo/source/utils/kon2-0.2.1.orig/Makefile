VERSION	= 0.2

SUBDIRS = lib src font tools

ifeq (.config,$(wildcard .config))
include .config
endif

all:	subdirs

version:
	@expr `cat .VERSION` + 1 > .VERSION
	@echo \#define	VERSION	\"ver.$(VERSION).`cat .VERSION` \(`date +%m/%d`\)\" > include/version.h

config:
	@sh tools/Configure < config.in

subdirs:
	@for i in $(SUBDIRS);\
	do (cd $$i && echo making all in $$i && $(MAKE)) || exit;\
	done

depend:
	@for i in $(SUBDIRS);\
	do (cd $$i && echo building dependencies in $$i && $(MAKE) depend) || exit;\
	done

clean:
	$(RM) -f *~ include/*~ doc/*~ .depend .config include/config.h config.out
	@if [ -f config.org ]; then mv config.org config.in; fi
	@for i in $(SUBDIRS);\
	do (cd $$i && echo cleaning all in $$i && $(MAKE) clean);\
	done

install: makedir instbin
	@if [ -f $(LIBDIR)/$(CONFIG_NAME) ]; then\
		mv $(LIBDIR)/$(CONFIG_NAME) $(LIBDIR)/$(CONFIG_NAME)'.org';\
		echo $(LIBDIR)/$(CONFIG_NAME)': file exists ... renamed !!';\
	fi
	@cp $(CONFIG_NAME) $(LIBDIR)
	install -c doc/kon.1 $(MANDIR)

makedir:
	@sh tools/makedir $(BINDIR)
	@sh tools/makedir $(LIBDIR)
	@sh tools/makedir $(MANDIR)

instbin:
	@for i in $(SUBDIRS);\
	do (cd $$i && echo installing all in $$i && $(MAKE) install);\
	done
