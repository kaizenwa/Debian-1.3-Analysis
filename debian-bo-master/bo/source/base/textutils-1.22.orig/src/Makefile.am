## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = ansi2knr

bin_PROGRAMS = cat cksum comm csplit cut expand fmt fold head join md5sum \
nl od paste pr sort split sum tac tail tr unexpand uniq wc

noinst_HEADERS = system.h
EXTRA_DIST =

datadir = $(prefix)/@DATADIRNAME@
localedir = $(datadir)/locale

INCLUDES = -I.. -I$(srcdir) -I$(top_srcdir)/lib -I../intl
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @INTLLIBS@ ../lib/libtu.a

$(PROGRAMS): $(LDADD)

## If necessary, add -lm to resolve use of pow in lib/strtod.c.
sort_LDADD = $(LDADD) @POW_LIBM@
sort_DEPENDENCIES = $(LDADD)
