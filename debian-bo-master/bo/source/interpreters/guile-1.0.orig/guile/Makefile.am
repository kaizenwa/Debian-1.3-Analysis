## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

LOCALDEFS = -DGUILE_BOOT_CMD='"(primitive-load-path \"ice-9/boot-9.scm\")"'
INCLUDES = $(LOCALDEFS) $(xtra_cflags) -I$(srcdir)/../libguile -I$(srcdir)/.. -I..

bin_PROGRAMS = guile
guile_SOURCES = guile.c getopt.c getopt1.c getopt.h
guile_LDADD = $(xtra_cflags) $(plugin_libs) $(xtra_libs)
guile_DEPENDENCIES = $(xtra_dependencies)

EXTRA_DIST = .gdbinit PLUGIN/OPT PLUGIN/REQ \
PLUGIN/guile.inits PLUGIN/guile.libs
