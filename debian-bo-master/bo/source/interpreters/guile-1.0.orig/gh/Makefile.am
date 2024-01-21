## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

pkginclude_HEADERS = gh.h

lib_LIBRARIES = libgh.a
libgh_a_SOURCES = gh_eval.c gh_predicates.c gh_init.c gh_funcs.c \
			gh_data.c gh_list.c gh_io.c

check_PROGRAMS = gh_test_c gh_test_repl
gh_test_c_SOURCES = gh_test_c.c
gh_test_c_DEPENDENCIES = libgh.a
gh_test_repl_SOURCES = gh_test_repl.c
gh_test_repl_DEPENDENCIES = libgh.a
LDADD = -L. -lgh -L../libguile -lguile -lthreads -lqt

INCLUDES = -I$(srcdir)/../libguile -I$(srcdir)/.. -I../

EXTRA_DIST = PLUGIN/REQ PLUGIN/guile.config PLUGIN/guile.libs
