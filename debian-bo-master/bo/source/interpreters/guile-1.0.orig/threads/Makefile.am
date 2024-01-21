## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(srcdir)/.. -I.. -I$(srcdir)/../libguile -I../libguile

lib_LIBRARIES = @THREADLIB@
EXTRA_LIBRARIES = libthreads.a

libthreads_a_SOURCES = threads.c threads.h

## This is kind of nasty... there are ".c" files that we don't want to
## compile, since they are #included in threads.c.  So instead we list
## them here.
noinst_HEADERS = coop-threads.c coop-threads.h coop.c fsu-pthreads.h \
mit-pthreads.c mit-pthreads.h

SUFFIXES = .x
.c.x:
	../libguile/guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

EXTRA_DIST = PLUGIN/OPT .cvsignore

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
