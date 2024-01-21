# Generated automatically from Makefile.in by configure.
# Makefile for web2c manual pages.
# 
# You need to change the variables and the pathnames only if this
# Makefile is invoked on its own, instead of from `../Makefile'.

SHELL = /bin/sh
srcdir = .

# This is so kpathsea will get remade automatically if you change
# something in it and recompile from the package directory.
kpathsea_parent = ../../..
kpathsea_dir = $(kpathsea_parent)/kpathsea
kpathsea = $(kpathsea_dir)/kpathsea.a

INSTALL = ../../
INSTALL_DATA = ${INSTALL} -m 644

DITROFF = ditroff
DITROFF = groff

# These pathnames are used to customize the manual pages with local
# directory names.  They should match the system directories so the
# documentation and the programs will be consistent. They are overridden
# by make from web2c/Makefile.
# The following make definitions (down to `end of installation
# directories') are repeated in several Makefiles, because different
# installations like to change them at different levels. If you edit the
# top-level Makefile, the changes should propagate, however.
# 
# The following define installation directories. The default paths are
# defined in kpathsea/paths.h, which is built from kpathsea/paths.h.in.
# See kpathsea/INSTALL for a description of how the various path-related
# files are used and created.

# Generic installation directories.
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
scriptdir = $(bindir)
libdir = $(exec_prefix)/lib
datadir = $(prefix)/lib
infodir = $(prefix)/info
includedir = $(prefix)/include
manext = 1
mandir = $(prefix)/man/man$(manext)
top_srcdir = $(srcdir)/../..


# Where the configuration files get installed.
dvips_prefix = $(texmf_prefix)/dvips
configdir = $(dvips_prefix)

# Where the PostScript prologues go.
headerdir = $(configdir)

# Where dvips' TeX macro files get installed.
psmacrodir = $(texinputdir)/dvips

# If a font can't be found close enough to its stated size, we look for
# each of these sizes in the order given.  This colon-separated list is
# overridden by the envvar TEXSIZES, and by a program-specific variable
# (e.g., XDVISIZES), and perhaps by a config file (e.g., in dvips).
default_texsizes = 300:600

# End of installation directory definitions.

localmodes = modes

# If you like, delete the man pages for programs you aren't installing.
# The programs are listed in alphabetical order.
# 
manfiles=		\
	mag.1		\
	pfb2pfa.1	\
	pk2bm.1		\
	ps2pk.1

# The edited file always has extension .1; we change it when we install.
.SUFFIXES: .man .1 .txt .ps .dvi
.man.1:
	sed -f sedscript $*.man >$@
.1.dvi:
	$(DITROFF) -Tdvi -man $*.1 >$@
.1.ps:
	$(DITROFF) -Tps -man $*.1 >$@
.1.txt:
	$(DITROFF) -Tascii -man $*.1 | col -b | expand >$@

all: $(manfiles)
.PHONY: dw

manfiles: $(manfiles)
dvi: $(manfiles:.1=.dvi)
ps: $(manfiles:.1=.ps)
txt: $(manfiles:.1=.txt)

# We do not depend on the top-level Makefile since the top-level
# Makefile can change for reasons that do not affect the man pages.

Makefile: Makefile.in ../config.status
	(cd ..; $(SHELL) config.status)

# Prevent GNU make 3 from overflowing arg limit on system V.
.NOEXPORT:


check: $(manfiles)
	for f in $(manfiles); do \
		echo =============== $$f =============== ; \
		checknr -c.BI.BR.IR.IB.RB.RI $$f ; \
	done
.PHONY: check

# Doubled word check.  You can find the dw utility on ftp.math.utah.edu in
# pub/tex/errata/dw.shar (an email message "send dw.shar from tex/errata"
# to tuglib@math.utah.edu will return a copy).
dw: $(manfiles:.1=.txt)
	for f in $(manfiles:.1=.txt); do \
		echo =============== $$f =============== ; \
		dw <$$f ; \
	done
.PHONY: dw

# manpages.sok is an additional custom spelling dictionary for these
# manual pages.  UNIX variants in the Berkeley tradition support the
# command-line syntax +dictfile to provide additional dictionaries.
# AT&T spell derivatives are not so friendly.
spell: $(manfiles) manpages.sok
	for f in $(manfiles) ; \
	do \
		echo =============== $$f =============== ; \
		deroff $$f | spell +manpages.sok ; \
	done
.PHONY: spell


install install-data: all
	$(srcdir)/../../mkdirchain $(mandir)
	for nameone in $(manfiles); do					\
          name=`basename $${nameone} .1`;				\
          $(INSTALL_DATA) $${name}.1 $(mandir)/$${name}.$(manext);	\
        done
install-exec:

mostlyclean:

clean: mostlyclean
	rm -f sedscript *.1

distclean: clean
	rm -f Makefile

realclean: distclean

extraclean:
	rm -f *~ *\#* patch* *.orig *.rej *.bak *.ckp *.i *.s
	rm -f *.dvi *.log *.aux *gf *pk *.tfm core a.out
