#=========================================================================
# NOTE: GNU make is a "must", gcc as well (you may have luck with another
# ansi compiler)
#=========================================================================

#=========================================================================
# Configurable section
#=========================================================================
VERSION = 0.4
PATCHLEVEL = 6
TETEXDIR=/usr/local/teTeX
# the following should be best for a pentium
# -O2 -s -m486 -malign-jumps=2 -malign-loops=2 -malign-functions=2
CFLAGS	= -O2

# For "real" 64bit OS, e.g. Digital Unix 4.0. Irix6?, use:
# CFLAGS += -DPOINTER_IS_NOT_INT

# For BSD4.4 based systems (e.g. FreeBSD2.x.x)
# CFLAGS += -DHAVE_EXTERN_SYS_ERRLIST

# -s makes problems on some platforms. We run strip after "make install",
# so we really do not need -s here.
LDFLAGS	=

# The shared libXmu from OpenWindows is buggy on some SunOS 4.1.3 systems.
# If you run into this problem (when linking xdvi), set BROKEN_SUNOS_LIBXMU
# to yes:
BROKEN_SUNOS_LIBXMU=no
#BROKEN_SUNOS_LIBXMU=yes

# On some platforms, this Makefile sets the include and lib directory
# for the X Window System to the vendor specific directories (e.g.
# /usr/openwin/{include,lib}). To turn off this "feature", set
# USE_VENDOR_X=no. You may also need to set XINC and XLIB (see below).
USE_VENDOR_X=yes
#USE_VENDOR_X=no

#=========================================================================
# Set these, if configure cannot find the location of your libs and
# includes for X, or if you want to overwrite the default value.
#=========================================================================
#override XINC    = --x-includes=/path/to/the/xincludes
#override XLIB    = --x-libraries=/path/to/the/xlibs

#=========================================================================
# CHANGES BELOW, ONLY FOR EXPERTS OR IN CASE OF TROUBLE!
#=========================================================================

# the default target:
default: all

# NOTE: rerun "make config" if you make changes in this section!

#=========================================================================
# Use this, if you use flex and libfl.a is installed in an unusual place
# and cannot be found by configure.
#=========================================================================
#override FLEX_LIB_DIR=/usr/lib

#=========================================================================
# Set these, to specify extra-directories to be be searched for includes
# and libraries when compiling xdvi. This may be necessary, if the athena
# widgets can not be found in the standard places.
#=========================================================================
#override EXTRA_XINCLUDE_DIR=/path/to/some/extra/includes
#override EXTRA_XLIB_DIR=/path/to/some/extra/libs

#=========================================================================
# set USE_DIALOG=false if compiling dialog causes trouble on your system
# texconfig will still run in command-mode
#=========================================================================
override USE_DIALOG=true

#=========================================================================
# set USE_NCURSES=false if you want to use your system curses instead of
# the ncurses library.
#=========================================================================
override USE_NCURSES=true

#=========================================================================
# set HAVE_NCURSES=true if you want to use an already installed version of
# the ncurses library.
#=========================================================================
override HAVE_NCURSES=false

#=========================================================================
# X stuff
#=========================================================================
NO_X_FOR_MF	= -DNO_X11WIN

# change the true/false definition of FOIL, if you get an error with wchar_t
FOIL		= false

#=========================================================================
# END OF CONFIGURABLE SECTION. PLEASE DO NOT CHANGE THE LINES BELOW.
# END OF CONFIGURABLE SECTION. PLEASE DO NOT CHANGE THE LINES BELOW.
#=========================================================================
ifneq (,$(findstring /,$(MAKE)))
config:		abort
abort:
	@echo '$(MAKE): make must not be called with an absolute path.'; exit 1
endif
KPSEDIR		= kpse-2.6
TEXIDIR		= texinfo-3.9
DIALOGDIR	= dialog-0.5
NCURSESDIR	= $(DIALOGDIR)/ncurses-1.9.9e
MKINDEXDIR	= makeindex-2.12
CC		= gcc
SHELL		= /bin/sh
platform	= $(shell ./config.guess | sed 's/-.*-/-/')
CWD		:= $(shell pwd)
INSTALL		= $(CWD)/$(KPSEDIR)/bin/install -c
override prefix	= $(TETEXDIR)
override bindir	= $(prefix)/bin/$(platform)
override TEXMF	= $(TETEXDIR)/texmf
override PATH	:= $(bindir):$(CWD)/bin-extra:$(CWD)/$(KPSEDIR)/bin:$(PATH)

CONFDIRS   = $(KPSEDIR) $(TEXIDIR)
ifeq ($(USE_DIALOG),true)
SUBDIRS    = $(TEXIDIR) $(MKINDEXDIR) $(KPSEDIR) $(DIALOGDIR) bin-extra
CONFDIRS   += $(DIALOGDIR)
else
SUBDIRS    = $(TEXIDIR) $(MKINDEXDIR) $(KPSEDIR) bin-extra
override USE_NCURSES = no
endif

PASSENV  = $(EXTRA_XINCLUDE_DEFINE) $(EXTRA_XLIB_DEFINE) $(FLEX_DEFINE) \
	CC='$(CC)' CFLAGS='$(CFLAGS)' FOIL='$(FOIL)'                    \
	INSTALL='$(INSTALL)' LDFLAGS='$(LDFLAGS)'                       \
	NO_X_FOR_MF='$(NO_X_FOR_MF)' USE_NCURSES='$(USE_NCURSES)'       \
	bindir='$(bindir)' platform='$(platform)' prefix='$(prefix)'    \
	CONFFLAGS='$(CONFFLAGS)' MAKE='$(MAKE)'                         \
	TETEXDIR='$(TETEXDIR)' package='$(package)'                     \
	opt='$(opt)' HAVE_NCURSES='$(HAVE_NCURSES)'                     \
	BROKEN_SUNOS_LIBXMU=$(BROKEN_SUNOS_LIBXMU)

MYVARS	= opt='$(opt)' package='$(package)'

CONFFLAGS = --prefix=$(prefix) $(XINC) $(XLIB) \
  --with-datadir=/usr/lib/terminfo --cache-file=$(CWD)/config.cache </dev/null

ifeq ($(distrib), true)
override package     = true
override NO_X_FOR_MF =
endif

ifeq (486,$(findstring 486, $(platform)))
CFLAGS += -m486
endif

ifeq ($(package),true)
TETEXDIR	= $(CWD)/tmp/teTeX
warn-texmfcnf: prepare-tmp
else
warn-texmfcnf: $(TETEXDIR)/texmf.cnf
endif

ifeq (yes,$(USE_VENDOR_X))
ifneq (,$(findstring rs6000-aix3,$(platform)))
XINC    = --x-includes=/usr/lpp/X11/include
XLIB    = --x-libraries=/usr/lpp/X11/lib
EXTRA_XLIB_DIR = /usr/lpp/X11/Xamples/lib/Xaw
endif

ifneq (,$(findstring hpux,$(platform)))
XINC    = --x-includes=/usr/include/X11R5
XLIB    = --x-libraries=/usr/include/X11R5
endif

ifneq (,$(findstring sparc,$(platform)))
XINC    = --x-includes=/usr/openwin/include
XLIB    = --x-libraries=/usr/openwin/lib
endif
endif

ifdef FLEX_LIB_DIR
FLEX_DEFINE = LEX=flex LEXLIB='-L$(FLEX_LIB_DIR) -lfl'
endif

ifdef EXTRA_XINCLUDE_DIR
EXTRA_XINCLUDE_DEFINE = EXTRA_XINCLUDE_FLAG=-I$(EXTRA_XINCLUDE_DIR)
endif

ifdef EXTRA_XLIB_DIR
EXTRA_XLIB_DEFINE = EXTRA_XLIB_FLAG=-L$(EXTRA_XLIB_DIR)
else
EXTRA_XLIB_DEFINE =
endif

default: all

config: prepare-tmp
	for i in $(CONFDIRS); do \
	   (set -x; cd $$i; rm -f config.cache; \
	   $(PASSENV) ./configure $(CONFFLAGS)); \
	done

all distclean clean:
	for i in $(SUBDIRS); do \
	  (set -x; cd $$i; $(MAKE) $(PASSENV) $@ || exit 1); \
	done

install:
	for i in $(SUBDIRS); do \
	  (set -x; cd $$i; $(MAKE) $(PASSENV) $@ || exit 1); \
	done
	@strip $(bindir)/* >/dev/null 2>&1 || true

rmcache:
	rm -f config.cache

manpages mostlyclean triptrap run-triptrap clean-triptrap:
	(cd $(KPSEDIR); $(MAKE) $(PASSENV) $@)

ini:
	cp $(KPSEDIR)/web2c/tex/tex.pool $(TEXMF)/web2c
	cp $(KPSEDIR)/web2c/mf/mf.pool $(TEXMF)/web2c
	cp $(KPSEDIR)/web2c/mp/mp.pool $(TEXMF)/web2c
	bin-extra/texconfig init

package:
	( cd tmp; tar cvf - teTeX/bin ) | gzip -9 > $(platform).tar.gz

prepare-tmp:
ifeq ($(package),true)
	rm -rf tmp
	bin-extra/MakeTeXmkdir $(TETEXDIR)
	cp $(KPSEDIR)/kpathsea/texmf.cnf.in $(TETEXDIR)/texmf.cnf
endif

# target distrib is primary for me :-)

distrib-pre:
	$(MAKE) $(MYVARS) distrib=true do-distrib-pre

distrib-all:
	$(MAKE) $(MYVARS) distrib=true do-distrib-all

distrib-install:
	$(MAKE) $(MYVARS) distrib=true do-distrib-install

do-distrib-install: install package

do-distrib-pre:
	$(MAKE) $(MYVARS) config clean

do-distrib-all:
	$(MAKE) $(MYVARS) all
	(cd $(KPSEDIR)/web2c/mf; mv virmf virmf.X)
	rm -f $(KPSEDIR)/web2c/mf/vextra.o
	(cd $(KPSEDIR)/web2c/mf; $(MAKE) $(PASSENV) \
	  NO_X_FOR_MF=-DNO_X11WIN all)

texhash:
	bin-extra/texhash

tmp-clean:
	rm -rf tmp

$(TETEXDIR)/texmf.cnf:
	@echo
	@echo "ERROR: The file $(TETEXDIR)/texmf.cnf does not exist."
	@echo
	@echo "Do not copy it from the source-tree to this location."
	@echo "Instead, install the inputs-tree for teTeX (form"
	@echo "teTeX-lib-$(VERSION).tar.gz or base/goodies series and "
	@echo "make sure that TETEXDIR in this Makefile points to the"
	@echo "directory where the files are installed."
	@echo
	@false

distrib: distrib-pre distrib-all distrib-install
config: warn-texmfcnf
config: rmcache warn-texmfcnf
world: config clean texhash all install ini
distclean: rmcache tmp-clean

export TETEXDIR TEXMF
