# quick and dirty Makefile

# configuration parameters
# HASSTRDUP - whether system has strdup() call.  Ultrix, e.g., doesn't.
# HASSTRERROR - whether system has strerror() call
# LIBS - local libraries to link in (e.g. -lresolv)

     HASSTRDUP = # -DHAS_STRDUP
   HASSTRERROR = # -DHAS_STRERROR
          LIBS = # -lresolv

DEFINES = $(HASSTRDUP) $(HASSTRERROR) $(DEFAULT_DOMAIN)

CFLAGS = $(DEFINES)

FILES = bulk_mailer.README INSTALL bulk_mailer.c patchlevel.h Makefile

bulk_mailer: bulk_mailer.o
	$(CC) $(CFLAGS) -o bulk_mailer bulk_mailer.o $(LIBS)

bulk_mailer.o: bulk_mailer.c patchlevel.h
	$(CC) -c $(CFLAGS) bulk_mailer.c

install: bulk_mailer
	cp bulk_mailer /usr/local/bin

clean:
	rm -f bulk_mailer bulk_mailer.o

bulk_mailer.tar: 
	tar cf bulk_mailer.tar $(FILES)

#test: bulk_mailer
#	bulk_mailer -v -debug primate-talk-owner addrs </dev/null
