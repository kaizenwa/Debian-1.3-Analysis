# Last updated : Thu Apr 18 00:11:33 1996
# V0.51
#
# $Revision: 1.5 $
#
PROG=bootpc
#CFLAGS=-O6 -ansi -Wall -pedantic -m486
CFLAGS=-O6 -Wall -pedantic
# Apparently -ansi breaks with some headers shipped with some versions
# Remove -m486 since we may be on any Linux system...

# list the Cfiles here
CFILES=bootpc.c

# List the header files here
CHEADS=bootp.h bptypes.h

# Other files we want to have arround in distrib
MISC=Makefile MANIFEST LSM DHCP README WHEN-IT-FAILS VERSION \
	 ChangeLog rc.bootp VERSION

# the delete command
RM=/bin/rm
SED=/usr/bin/sed

# we generate this from the list of C files above
OBJS=$(CFILES:.c=.o)

%.o:	%.c $(CHEADS)
	$(CC) $(CFLAGS) -c $(*).c

all:	$(PROG)

clean:
	${RM} -f ${OBJS} ${PROG} *~ *.tar *.tar.gz *.tgz

${PROG}: ${OBJS}
	${CC} -o ${PROG} ${CFLAGS} ${OBJS}
	strip ${PROG}

# Make up a distribution file from the bits we want people to see
distrib:  $(CFILES) $(CHEADS) $(MISC)
	set `cat VERSION` ;\
	MinV=`echo $$1|sed 's/\.//g'`; \
	rdate=`date '+%Y-%h-%d'`; \
	distname=bootpc.v$$MinV ; \
	rm -fr $$distname; \
	mkdir $$distname; \
	chmod a+w $$distname; \
	for i in `cat MANIFEST` ; do \
	  echo $$i; sed -e "s/0.51/$$1/g;s/051/$$MinV/g;s/1996-Apr-18/$$rdate/g" $$i > $$distname/$$i;\
	done; \
	chmod -R a+rx $$distname; \
	tar -cvf $$distname.tar $$distname; \
	gzip -9v $$distname.tar &&mv $$distname.tar.gz $$distname.tgz ||true; \
	rm -fr $$distname
