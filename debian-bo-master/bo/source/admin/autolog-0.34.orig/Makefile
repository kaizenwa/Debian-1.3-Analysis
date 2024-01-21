# *********************************************************
# Change this section as needed
# The -g flag is to include debugging information.  It gets
# stripped back out in the install command anyway.
CC	= gcc
CFLAGS	= -g
MANDIR  = /usr/man
BINDIR  = /usr/sbin
# *********************************************************

autolog:	autolog.c
	$(CC) $(CFLAGS) -o autolog autolog.c

install:	autolog
	install -s autolog $(BINDIR)
	install -m644 autolog.conf.5 $(MANDIR)/man5
	install -m644 autolog.8 $(MANDIR)/man8

clean:
	rm autolog
