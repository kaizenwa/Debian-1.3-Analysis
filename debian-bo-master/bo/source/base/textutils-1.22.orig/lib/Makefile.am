## Process this file with automake to produce Makefile.in
noinst_LIBRARIES = libtu.a

INCLUDES = -I.. -I$(srcdir) -I../intl

libtu_a_SOURCES = full-write.c getopt.c getopt1.c \
linebuffer.c long-options.c md5.c memcasecmp.c memchr.c safe-read.c \
xmalloc.c xstrtod.c xstrtol.c xstrtoul.c

libtu_a_LIBADD = @LIBOBJS@ @ALLOCA@
libtu_a_DEPENDENCIES = $(libtu_a_LIBADD)

noinst_HEADERS = error.h getline.h getopt.h linebuffer.h \
long-options.h md5.h memcasecmp.h regex.h rx.h xstrtod.h xstrtol.h xstrtoul.h
