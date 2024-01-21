
SHELL = /bin/sh

BINDIR = /usr/local/bin
MANDIR = /usr/local/man/man1

.PHONY: all

all: rexec install

rexec: rexec.c

rexec.1:

install: rexec rexec.1
	@ echo "Installation:  Press enter for defaults."; \
	echo -n "Enter the location to install executable "; \
	echo -n " (default: $(BINDIR)): "; \
	read bindir; \
	bindir=$${bindir:-$(BINDIR)}; \
	echo -n "Enter the manpage location (default $(MANDIR)): ";\
	read mandir; \
	mandir=$${mandir:-$(MANDIR)}; \
	cp rexec $$bindir; \
	chmod a+rx $${bindir}/rexec; \
	cp rexec.1 $$mandir; \
	chmod a+r $${mandir}/rexec.1
