#ident	"@(#)smail/contrib/execm:RELEASE-3_2:Makefile,v 1.4 1996/02/26 15:34:17 woods Exp"

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

ROOT=../..
EXECMSRC=execm.c
MISCSRC=Makefile README
SRC=${EXECMSRC} ${MISCSRC}

COPY    = cp
CFLAGS	= -O
LDFLAGS = -i
LIBMAIL = /usr/lib/mail
EXECMAIL= $(LIBMAIL)/execmail

all:    execm

execm:  execm.c
	$(CC) $(CFLAGS) $(LDFLAGS) execm.c -o execm

install: $(EXECMAIL)

$(EXECMAIL): execm
	@if [ ! -d $(LIBMAIL) ]; then \
		echo "I don't think this is a Xenix system."; \
		exit 1; \
	fi
	@if [ ! -f $(EXECMAIL).x ]; then \
		echo "Saving $(EXECMAIL) as $(EXECMAIL).x"; \
		(set -x; mv $(EXECMAIL) $(EXECMAIL).x); \
		exit $?; \
	fi
	$(COPY) execm $(EXECMAIL)
	chgrp bin $(EXECMAIL)
	chown bin $(EXECMAIL)
	chmod 755 $(EXECMAIL)

depend:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

clean:
	rm -f *.o

clobber: clean
	rm -f execm
