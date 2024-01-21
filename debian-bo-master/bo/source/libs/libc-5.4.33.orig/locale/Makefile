#
#	Makefile for locale
#
TOPDIR=..

include $(TOPDIR)/Makeconfig
include $(TOPDIR)/Makerules

localedir=/usr/share/locale
i18ndir=/usr/share/i18n

BASE_CFLAGS := -I. -DLOCALE_PATH='"$(localedir)"' \
	    -DCHARMAP_PATH='"$(i18ndir)/charmap"' \
	    -DLOCSRCDIR='"$(i18ndir)/locales"' -DHAVE_CONFIG_H \
	    -DLOCALE_ALIAS_PATH='"$(localedir):$(i18ndir)"' \
	    -Iprograms $(BASE_CFLAGS)

SRCS	= setlocale.c loadlocale.c localeconv.c nl_langinfo.c \
	  $(categories:%=lc-%.c) $(categories:%=C-%.c) \
	  explodename.c l10nflist.c findlocale.c C_name.c localealias.c
categories	= ctype messages monetary numeric time collate

vpath %.c programs
vpath %.h programs

LDEFSRCS	:= programs/localedef.c $(categories:%=ld-%.c) \
		   programs/charmap.c charset.c linereader.c locfile.c \
		   stringtrans.c
PROGLIBS	:= simple-hash.c xmalloc.c xstrdup.c programs/error.c \
		   euidaccess.c group_member.c
LOCSRCS		:= programs/locale.c


GPERF = gperf
GPERFFLAGS = -acCgopt -k1,2,5,$$

ASMS= $(SRCS:.c=.s)
OBJS= $(SRCS:.c=.o)
ALIASES=


include $(TOPDIR)/Maketargets

programs/%-kw.h: programs/%-kw.gperf
	$(GPERF) $(GPERFFLAGS) -N $(@F:-kw.h=_hash) $< > $@.new
	mv -f $@.new $@

programs: localedef locale

localedef: $(LDEFSRCS:.c=.o) $(PROGLIBS:.c=.o)
	$(CC) -o $@ $^

locale: $(LOCSRCS:.c=.o) $(PROGLIBS:.c=.o)
	$(CC) -o $@ $^

programs/error.o: programs/error.c
	$(CC) $(CFLAGS) -c programs/error.c -o programs/error.o

programs/charmap.o: programs/charmap.c
	$(CC) $(CFLAGS) -c programs/charmap.c -o programs/charmap.o

programs/localedef.o: programs/localedef.c
	$(CC) $(CFLAGS) -c programs/localedef.c -o programs/localedef.o

programs/locale.o: programs/locale.c
	$(CC) $(CFLAGS) -c programs/locale.c -o programs/locale.o

programsclean: 
	-rm -f localedef locale $(LDEFSRCS:.c=.o) $(PROGLIBS:.c=.o)
	-rm -f $(LOCSRCS:.c=.o)
