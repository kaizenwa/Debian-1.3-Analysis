CC = gcc
CFLAGS = -O2   -I. -I/usr/include/bsd   -DDEBIAN
LDFLAGS = -s
LDLIBS = -lbsd

INSTALL = install
LS = ls

usrbindir = /usr/bin
libdir = /usr/lib
mandir = /usr/man
man1dir = $(mandir)/man1
man6dir = $(mandir)/man6


# Programs that are installed to $(usrbindir).
USRBINPROGS = banner cal calendar col colcrt colrm column from hexdump \
              look lorder tsort ul vacation whois

# Programs installed to $(usrbindir), but setgid tty
SGIDTTYPROGS = write

.c.o:
	$(CC) $(CFLAGS) -c $<

.o:
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

all:  $(USRBINPROGS) $(SGIDTTYPROGS)

clean:
	rm -f $(USRBINPROGS) $(SGIDTTYPROGS)
	rm -f *.o core *~

install: all
	./mkinstalldirs $(usrbindir) $(libdir) $(man1dir) $(man6dir)
	for f in $(USRBINPROGS); \
	do \
	  $(INSTALL) $$f $(usrbindir)/$$f; \
	done
	for f in $(SGIDTTYPROGS); do \
	  $(INSTALL) -o root -g tty -m 2755 $$f $(usrbindir)/$$f; \
	done
	-$(INSTALL) -d $(libdir)/calendar
	for f in `$(LS) ./calendars`; \
	do \
	  $(INSTALL) -m 644 ./calendars/$$f $(libdir)/calendar/$$f; \
	done
	for f in *.1; \
	do \
	  $(INSTALL) -m 644 $$f $(man1dir)/$$f; \
	done
	for f in *.6; \
	do \
	  $(INSTALL) -m 644 $$f $(man6dir)/$$f; \
	done

banner:   banner.o err.o getopt.o
cal:      cal.o err.o getopt.o
calendar: calendar.o err.o getopt.o
col:      col.o err.o getopt.o
colcrt:   colcrt.o err.o getopt.o
colrm:    colrm.o getopt.o
column:   column.o err.o getopt.o
from:     err.o from.o getopt.o
hexdump:  conv.o display.o getopt.o hexdump.o hexsyntax.o odsyntax.o parse.o
look:     getopt.o look.o
lorder:   lorder.sh
script:   getopt.o script.o
tsort:    err.o getopt.o tsort.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS) -ldb
ul:       err.o getopt.o ul.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS) -lncurses
vacation: err.o getopt.o vacation.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS) -ldb
whois:    getopt.o whois.o
write:    err.o getopt.o write.o
