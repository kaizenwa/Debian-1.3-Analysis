CC = gcc
CFLAGS = -O2   -I. -I/usr/include/bsd   -DDEBIAN
LDFLAGS = -s
LDLIBS = -lbsd

INSTALL = install
LS = ls

bindir = /bin
usrbindir = /usr/bin
usrsbindir = /usr/sbin
mandir = /usr/man
man1dir = $(mandir)/man1
man8dir = $(mandir)/man8

# Programs that are installed to $(bindir).
BINPROGS = kill

# Programs that are installed to $(usrbindir).
USRBINPROGS = logger renice script

# Programs install,ed to $(usrbindir), but setgid tty
SGIDTTYPROGS = wall

# Programs that are installed to $(usrsbindir).
USRSBINPROGS = 


.c.o:
	$(CC) $(CFLAGS) -c $<

.o:
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

all: $(BINPROGS) $(USRBINPROGS) $(SGIDTTYPROGS) $(USRSBINPROGS)

clean:
	rm -f $(BINPROGS) $(USRBINPROGS) $(SGIDTTYPROGS) $(USRSBINPROGS)
	rm -f *.o core *~

install: all
	./mkinstalldirs $(bindir) $(usrbindir) $(usrsbindir) \
	  $(man1dir) $(man8dir)
	for f in $(BINPROGS); \
	do \
	  $(INSTALL) $$f $(bindir)/$$f; \
	done
	for f in $(USRBINPROGS); \
	do \
	  $(INSTALL) $$f $(usrbindir)/$$f; \
	done
	for f in $(SGIDTTYPROGS); do \
	  $(INSTALL) -o root -g tty -m 2755 $$f $(usrbindir)/$$f; \
	done
	for f in $(USRSBINPROGS); \
	do \
	  $(INSTALL) $$f $(usrsbindir)/$$f; \
	done
	for f in *.1; \
	do \
	  $(INSTALL) -m 644 $$f $(man1dir)/$$f; \
	done
	for f in *.8; \
	do \
	  $(INSTALL) -m 644 $$f $(man8dir)/$$f; \
	done

#chroot:   chroot.o err.o getopt.o
kill:     err.o getopt.o kill.o
logger:   err.o getopt.o logger.o
renice:   err.o getopt.o renice.o
script:   getopt.o script.o
wall:     err.o getopt.o ttymsg.o wall.o
