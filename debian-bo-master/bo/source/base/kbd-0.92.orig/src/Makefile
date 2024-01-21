# Note: keytables used to be in /usr/lib, here it is in /usr/lib/kbd
ifndef DATADIR
DATADIR = /usr/lib/kbd
endif

ifndef BINDIR
BINDIR = /usr/bin
endif

PROGS   = dumpkeys loadkeys showkey setfont showfont \
	  setleds setmetamode kbd_mode resizecons chvt disalloc \
	  getkeycodes setkeycodes \
	  psfaddtable psfgettable psfstriptable

OLDPROGS= mapscrn loadunimap

MISC    = screendump setvesablank spawn_console spawn_login \
	  getunimap clrunimap fgconsole

SHCMDS  = unicode_start unicode_stop

WARN	= -Wall
DEFS	= -DDATADIR=\"$(DATADIR)\"
CFLAGS  = -O2
LDFLAGS = -s

CC	= gcc
YACC	= bison -y
LEX	= flex -8

.c.o:
	$(CC) -c $(WARN) $(CFLAGS) $(DEFS) $<

all:	$(PROGS) $(OLDPROGS)

progs:	$(PROGS)

old:	$(OLDPROGS)

install:	all
	install $(PROGS) $(OLDPROGS) $(BINDIR)

# loadkeys.o: separate rule since the flex output does not permit -Wall
loadkeys.o:	loadkeys.c analyze.c
	$(CC) -c $(CFLAGS) $(DEFS) $<


# mapscrn and loadunimap are now part of setfont
# but can be compiled separately, if desired
main_mapscrn.o: mapscrn.c paths.h
	$(CC) $(CFLAGS) $(WARN) $(DEFS) -DMAIN -c $< -o $@

main_loadunimap.o: loadunimap.c paths.h
	$(CC) $(CFLAGS) $(WARN) $(DEFS) -DMAIN -c $< -o $@

$(OLDPROGS): %: main_%.o findfile.o
	$(CC) $(LDFLAGS) $^ -o $@


clean:
	rm -f core *.o analyze.c loadkeys.c

reallyclean spotless: clean
	rm -f $(PROGS) $(OLDPROGS) $(MISC) *~

$(PROGS): %: %.o

dumpkeys loadkeys: ksyms.o

loadkeys.o mapscrn.o setfont.o resizecons.o loadunimap.o: paths.h

psfaddtable.o psfgettable.o psfstriptable.o setfont.o: psf.h

loadkeys mapscrn setfont resizecons loadunimap: findfile.o

chvt clrunimap disalloc dumpkeys getkeycodes getunimap kbd_mode: getfd.o
loadkeys loadunimap mapscrn resizecons setkeycodes setfont: getfd.o
setvesablank showkey: getfd.o

setfont: mapscrn.o loadunimap.o
