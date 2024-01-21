DESTDIR=
SHELL=	/bin/bash

# $(DESTDIR)/ is prepended to BINDIR and MANDIR.

# CONFIGURE: Set location of executable, its group and mode.
BINDIR=	usr/bin
BINGRP=	root
BINMOD=	0755

# CONFIGURE: Set location of man directory and man page suffix.
MANDIR=	usr/man/man1
MANSFX=	1

# CONFIGURE(Config): Select an OSTYPE for your machine from the list above.
OSTYPE=	linux

# CONFIGURE(Config): Add any OS-specific options here.
COPTS=-O3 -Wall -Wno-unused -pipe -I..

# CONFIGURE(Config): Add any special libraries your system needs.
LIBS=	-s -L../proc -lproc

SRCS=	main.c argparse.c getproc.c
OBJS=	main.o argparse.o getproc.o
HDRS=	conf.h

CFLAGS=	 ${COPTS}

all:	skill

.PHONY:	all install config clean lint FRC depend

skill:	${OBJS}
	${CC} ${CFLAGS} ${OBJS} -o skill ${LIBS}

getproc.c:
	rm -f getproc.c getproc.o
	ln -s machdep/${OSTYPE}.c getproc.c

${OBJS}: ${HDRS}

install: skill
#(SysV)	filepriv -d ${DESTDIR}/${BINDIR}/skill
	rm -f ${DESTDIR}/${BINDIR}/skill ${DESTDIR}/${MANDIR}/skill.${MANSFX}
	rm -f ${DESTDIR}/${BINDIR}/snice ${DESTDIR}/${MANDIR}/snice.${MANSFX}
#	install -c -g ${BINGRP} -m ${BINMOD} -s skill ${DESTDIR}/${BINDIR}
	cp skill ${DESTDIR}/${BINDIR}/skill
	strip ${DESTDIR}/${BINDIR}/skill
	chgrp ${BINGRP} ${DESTDIR}/${BINDIR}/skill
	chmod ${BINMOD} ${DESTDIR}/${BINDIR}/skill
	ln ${DESTDIR}/${BINDIR}/skill ${DESTDIR}/${BINDIR}/snice
#(SysV)	filepriv -f dacread ${DESTDIR}/${BINDIR}/skill
#	install -c -m 644 skill.1 ${DESTDIR}/${MANDIR}/skill.${MANSFX}
	cp skill.1 ${DESTDIR}/${MANDIR}/skill.${MANSFX}
	chmod 644 ${DESTDIR}/${MANDIR}/skill.${MANSFX}
	ln ${DESTDIR}/${MANDIR}/skill.${MANSFX}\
		${DESTDIR}/${MANDIR}/snice.${MANSFX}

config Config: FRC
	./Config

clean:
	rm -f skill snice *.o [Ee]rrs [Ii][Ee]rrs mklog a.out core getproc.c

lint:	getproc.c
	lint ${COPTS} ${SRCS} ${LIBS}

FRC:

depend: ${SRCS}
	for i in ${SRCS}; do \
	    cc -M ${COPTS} $$i | sed 's/\.o//' | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
