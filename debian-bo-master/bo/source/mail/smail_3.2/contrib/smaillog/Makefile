#ident	"@(#)smail/contrib/smaillog:RELEASE-3_2:Makefile,v 1.5 1996/02/26 15:35:14 woods Exp"

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

#CC           = gcc

# add -DSYSV for System V or -DPOSIX for POSIX P1003.1
#CFLAGS       = -O -DPOSIX
#CFLAGS	      = -O -DSYSV
CFLAGS	      = -O

# add -lc_s for SVR3 shared C library
LFLAGS        =
#LFLAGS	      = -lc_s

DEST	      = /usr/local/bin
MAN           = /usr/local/man
EXT           = 8

EXTHDRS	      = /usr/include/ctype.h \
		/usr/include/stdio.h \
		/usr/include/string.h \
		/usr/include/strings.h

HDRS	      =

PROGRAM	      = smaillog

LIBRARY	      = lib.a

MAKEFILE      = Makefile

OBJS	      = smaillog.o

PRINT	      = pr

SRCS	      = smaillog.c

SRC	      = ${MAKEFILE} ${SRCS} ${HDRS}  README smaillog.8

all:		$(PROGRAM)

$(PROGRAM):	$(OBJS)
		$(CC) -o $(PROGRAM) $(OBJS) $(LFLAGS)

$(LIBRARY):	$(OBJS)
		ar cru $(LIBRARY) $(OBJS)
		ranlib $(LIBRARY)

clean:;		-rm -f $(OBJS)
		-rm -f *~ \#*		

depend:;	mkmf -f $(MAKEFILE) LIBRARY=$(LIBRARY) DEST=$(DEST)

extract:;	ar xo $(DEST)/$(LIBRARY)
		@rm -f __.SYMDEF

index:;		ctags -wx $(HDRS) $(SRC)

install:	$(PROGRAM)
		-rm -f $(DEST)/$(PROGRAM)
		cp $(PROGRAM) $(DEST)/$(PROGRAM)
		-chgrp bin $(DEST)/$(PROGRAM)
		-chown bin $(DEST)/$(PROGRAM)
		-rm -f $(MAN)/man$(EXT)/$(PROGRAM).$(EXT)
		cp $(PROGRAM).8 $(MAN)/man$(EXT)/$(PROGRAM).$(EXT)

library:        $(LIBRARY)

print:;		$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
		@-ar xo $(DEST)/$(LIBRARY)
		@make -f $(MAKEFILE) DEST=$(DEST) install clean
###
smaillog.o: /usr/include/stdio.h /usr/include/ctype.h \
	/usr/include/string.h

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

clobber: clean
	rm -f ${PROGRAM}
