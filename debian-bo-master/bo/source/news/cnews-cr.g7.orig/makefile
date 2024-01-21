# things that just might have to be changed
MAKE=make

# Do not mess with the following; configuration is done by other means.
# There are some complex little dances done because this makefile must
# be operational before the syntax of makefile-include is known.  The
# makefile in conf, in particular, is heavily interwoven with this one.
LIBDIRS=libbig libc libcnews libdbz libfake libsmall libstdio
PDIRS=util batch conf ctl expire explode inject input maint \
	nov readnews relay
UIDIRS=inject maint
RDIRS=$(PDIRS)
SLOWRDIRS=libdbz
INALL=conf/inall
ROPT=

# System V bug fix
SHELL=/bin/sh

all:	needquiz patchchores msub sub mx libcnews.a pgms
	: everything built successfully

lib:	libcnews.a

libcnews.a:	needquiz msub sub mx conf/liblist
	$(INALL) "$(MAKE)" `cat conf/liblist`

freshlib:
	rm -f libcnews.a conf/liblist
	$(MAKE) lib

pgms:	needquiz msub sub mx libcnews.a
	$(INALL) "$(MAKE)" $(PDIRS)
	: programs built successfully

install:	needquiz
	$(INALL) "$(MAKE) install" $(PDIRS)
	: 'all programs installed!'
	: 'but input/newsspool needs its ownership and permissions changed'

setup:		needquiz
	$(INALL) "$(MAKE) setup" $(PDIRS)
	cd conf ; $(MAKE) versetup

ui:	needquiz
	$(INALL) "$(MAKE) ui" $(UIDIRS)
	: 'user interface installed'

readpostcheck:	needquiz
	$(INALL) "$(MAKE) readpostcheck" readnews
	: 'readnews, postnews, checknews installed'

cmp:	needquiz
	$(INALL) "$(MAKE) cmp" $(PDIRS)
	: 'no worrisome differences found'

cmpok:
	$(INALL) "$(MAKE) cmp CMPOPT=-i" $(PDIRS)

# conf/makeinc and conf/substitutions depend on quiz and conf/useanswers too,
# but including them here would defeat the don't-touch-if-unchanged trick in
# conf/useanswers which avoids spurious re-runs of subst.
include/config.make:	quiz conf/useanswers
	: "you must run (or re-run) quiz first -- see README.install"
	@false

needquiz:	include/config.make

msub:	conf/msubsts.done

conf/msubsts.done:	conf/makefilelist conf/makeinc conf/subst
	sh conf/subst -f conf/makeinc `cat conf/makefilelist`
	touch $@

sub:	conf/substs.done

conf/substs.done:	conf/subst.all conf/substitutions conf/subst
	sh conf/subst -f conf/substitutions `cat conf/subst.all`
	touch $@

mx:	conf/mx.done

conf/mx.done:	msub include/config.make
	: A mysterious fatal error here probably means you gave quiz the
	: wrong answer when it asked you how to do an include in a makefile.
	cd conf ; make mx
	touch $@

small:
	echo "include libc libcnews $(DBZ) libfake libsmall" >conf/liblist

big:
	echo "include libc libcnews $(DBZ) libfake libbig" >conf/liblist

stdio:
	cd libstdio ; $(MAKE) r $(ROPT) || ( $(MAKE) clean ; exit 1 )
	: and only if that succeeded...
	cd libstdio ; $(MAKE) rclean
	echo "libstdio" >>conf/liblist

ourstdio:
	: "Find out if our stdio speedups will work on your system..."
	-$(MAKE) stdio
	: "If that failed, don't panic -- the makefiles will cope.  It just"
	: "means that the stdio speedups don't work on this system."
	sleep 5

sysstdio:

conf/liblist:	include/config.make
	cd conf ; $(MAKE) liblist

patchchores:
	: if this fails, you have not installed all parts of a multi-part patch
	test " `cat conf/versionname | sed 's/^[^.]*\..//'`" = " " ;
	: delete files obsoleted by patches
	rm -f `cat conf/deadfiles`

r:
	$(INALL) "$(MAKE) r $(ROPT) && $(MAKE) rclean" $(RDIRS)
	: leave libdbz to last because its regression test is rather slow
	$(INALL) "$(MAKE) r $(ROPT) && $(MAKE) rclean" $(SLOWRDIRS)
	: 'all tests successful!'
	: 'either you or newsmaster should have mail from the "report" test'

tidy:
	: 'if this bombs messily, you probably did a "make spotless" already'
	$(INALL) "$(MAKE) clean" $(LIBDIRS) $(PDIRS)

clean:	tidy
	$(INALL) "$(MAKE) clean" include
	rm -f libcnews.a

veryclean:	clean
	rm -f conf/*.done conf/liblist 

spotless:
	-$(MAKE) veryclean
	rm -f include/config.make conf/makeinc conf/substitutions

sterile:	spotless
	rm -f conf/quiz.def

# this is strictly for development use
.DEFAULT:
	: if you see this, you are doing something wrong -- the following
	: is strictly for development use
	$(MAKE) -f mfile $@
