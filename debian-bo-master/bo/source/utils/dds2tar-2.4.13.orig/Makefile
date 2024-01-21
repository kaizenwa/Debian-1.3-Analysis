# Makefile for dds2tar
#
#--------------------------------------------------------
# To compile everything:                     make
# To install the executables and man pages:  make install
#--------------------------------------------------------

# Here are some configurable options:
################################################################

# To enable the use of the log pages of an HPDAT,
# uncomment the following line :
HPDAT=-DHPDAT

# Default tape device is: /dev/nst0
# edit or uncomment the following line to change the default device
DEVICE=/dev/rmt0

# The tape blocks are read into a buffer, so the buffer of dds2tar
# should be in the same size as the one in /usr/src/linux/driver/scsi/st.c
# of the kernel code. Set ST_BUFFER_BLOCKS to zero, in order to take the
# size from the kernel source directly.
#
# The number is halved, if EOVERFLOW is detected.
# The value of the kernel is normally ST_BUFFER_BLOCKS=32.
#
# To configure the buffer size of dds2tar
# change or uncomment the following line :
# BUFFER=-DST_BUFFER_BLOCKS=32
BUFFER=-DST_BUFFER_BLOCKS=0

# To include some experimental stuff
# uncomment the following line :
# EXP_STUFF=-DEXP_STUFF

# change this to where you want to install:
BINDIR=/usr/bin
MANDIR=/usr/man
MANEXT=1

# place here your favorite C compiler and options
CC=cc
CCOPT=$(HPDAT) $(HPDAT4) $(EXP_STUFF) $(MATCH) $(BUFFER) \
      -DDEVICE=\"$(DEVICE)\" -O4 -Wall

#        -Wall \
#        -Wstrict-prototypes \
#        -Wshadow \
#        -Wwrite-strings \
#        -Wpointer-arith \
#        -Wcast-qual \
#        -Wenum-clash \
#        -Wcast-align \
#        -Wtraditional \
#        -Wmissing-prototypes \
#        -Wnested-externs \
#        -Winline \
#        -pedantic \
 

# nothing configurable below this line
################################################################

.SUFFIXES: .c .o .man .$(MANEXT) .ps .s

V=2.4.13
D=dds2tar-${V}
T=tar-1.11.8

CCFLAGS=-DVERSION=\"$V\"

TAR=tar
ARCHIVE=\
$(D)/Makefile \
$(D)/Changes \
$(D)/README \
$(D)/dds2tar.man \
$(D)/dds2index.man \
$(D)/mt-dds.man \
$(D)/dds-dd.man \
$(D)/dds2tar.ps \
$(D)/dds2index.ps \
$(D)/mt-dds.ps \
$(D)/dds-dd.ps \
$(D)/dds2tar.h \
$(D)/dds2tar.c \
$(D)/dds_extract.c \
$(D)/zf-cre-open.h \
$(D)/zf-cre-open.c \
$(D)/dds_index.c \
$(D)/dds_tape.h \
$(D)/dds_tape.c \
$(D)/dds_chkhdr.c \
$(D)/tar-1.11.2.patch \
$(D)/tar-1.11.8.patch \
$(D)/tar-1.11.2-sparse-files.patch \
$(D)/tar-1.11.2-sparse-files.README \
$(D)/dds2tar-test.sh \
$(D)/index-of-tar \
$(D)/index-of-tar-v \
$(D)/index-of-tar-2 \
$(D)/index-of-dds2index \
$(D)/.indent.pro \
$(D)/dds2tar.lsm \
$(D)/COPYING \
$(D)/ddstool \


OBJ= dds_index.o dds_extract.o dds_tape.o \
     zf-cre-open.o dds_chkhdr.o


all: dds2tar dds2tar.1 dds2index.1 mt-dds.1 dds-dd.1

dds2tar: dds2tar.o $(OBJ)
	$(CC) dds2tar.o $(OBJ)  -o dds2tar
	ln -f dds2tar dds2index
	ln -f dds2tar mt-dds
	ln -f dds2tar dds-dd

tar2tar: tar2tar.c
	$(CC) tar2tar.c -o tar2tar

dds2index: dds2index.o $(OBJ)
	$(CC) dds2index.o $(OBJ)  -o dds2index

mt-dds: mt-dds.o $(OBJ)
	$(CC) mt-dds.o $(OBJ)  -o mt-dds

dds-dd: dds-dd.o $(OBJ)
	$(CC) dds-dd.o $(OBJ)  -o dds-dd

install: dds2tar ddstool \
	dds2tar.$(MANEXT) mt-dds.$(MANEXT) dds2index.$(MANEXT) dds-dd.$(MANEXT)
	install -c -s dds2tar $(BINDIR)
	install -c ddstool $(BINDIR)
	ls -l $(BINDIR)/dds2tar $(BINDIR)/ddstool
	install -c dds2tar.$(MANEXT)   $(MANDIR)/man$(MANEXT)
	install -c dds2index.$(MANEXT) $(MANDIR)/man$(MANEXT)
	install -c mt-dds.$(MANEXT)    $(MANDIR)/man$(MANEXT)
	install -c dds-dd.$(MANEXT)    $(MANDIR)/man$(MANEXT)
	ln -f $(BINDIR)/dds2tar  $(BINDIR)/dds2index
	ln -f $(BINDIR)/dds2tar  $(BINDIR)/mt-dds
	ln -f $(BINDIR)/dds2tar  $(BINDIR)/dds-dd

tar: dds2tar.ps dds2index.ps mt-dds.ps dds-dd.ps tar-1.11.8.patch
	cd .. ; echo ' ' ; \
		$(TAR) -R -c -f $(D).tar -v $(ARCHIVE) ;
		echo ' '
	ls -l ../$(D).tar
	gzip -9 -f ../$(D).tar
	ls -l ../$(D).tar.gz | awk '{print $$5}' > archive_length
	sed s/LENGTH/`cat archive_length`/ <dds2tar.lsm |\
           sed s/VERSION/$(V)/ > ../$(D).lsm
	ls -l ../dds2tar*z ../dds2tar*.lsm
	-rm -f ../dds2tar archive_length
	cd .. ; ln -s $(D) dds2tar
	echo ' '
	cd .. ; tar zfc tar-1.11.8.1.tar.gz --totals tar-1.11.8.1
	cd .. ; tar cf archive.tar ${D}.lsm ${D}.tar.gz ${T}.1.tar.gz

T=tar-1.11.8
tar-patch tar-1.11.8.patch: 
	cd .. ;  diff -ru ${T} ${T}.1 > $(D)/${T}.patch ; true
	ls -l tar-*patch

.c.o:
	$(CC) -c $< $(CCOPT) $(CCFLAGS)

.c.s:
	$(CC) -S -c $< $(CCOPT) $(CCFLAGS)

.man.1:
	umask 022 ; sed -e 's?/dev/tape?$(DEVICE)?' $< >$@

.man.ps:
	umask 022 ; sed -e 's?/dev/tape?$(DEVICE)?' $< >$*.1
	umask 022 ; groff -man $*.$(MANEXT)   > $*.ps

clean:
	-rm -f $(OBJ) dds2tar.o dds2index.o mt-dds.o dds-dd.o *.BAK *.s
	-rm -f dds2tar dds2index mt-dds dds-dd fio_test 
	-rm -f *~ *.$(MANEXT) *.ps
	-rm -f tags *.bak
	chmod 644 *

doc:
	                a2ps -p -nP    [A-Z]* *.h *.c *.lsm > a.ps
	man dds2tar   | a2ps -p -nP -m -Hdds2tar            > b.ps
	man dds2index | a2ps -p -nP -m -Hdds2index          > c.ps
	man mt-dds    | a2ps -p -nP -m -Hmt-dds.man.ps      > d.ps
	man dds-dd    | a2ps -p -nP -m -Hdds-dd.man.ps      > d.ps

psman: dds2tar.$(MANEXT) mt-dds.$(MANEXT) dds2index.$(MANEXT) dds-dd.$(MANEXT)
	groff -man dds2tar.$(MANEXT)   > dds2tar.ps
	groff -man dds2index.$(MANEXT) > dds2index.ps
	groff -man mt-dds.$(MANEXT)    > mt-dds.ps
	groff -man dds-dd.$(MANEXT)    > dds-dd.ps

dds_extract.o:dds_extract.c dds2tar.h dds_tape.h
dds_index.o:dds_index.c dds2tar.h dds_tape.h
dds_tape.o:dds_tape.c dds2tar.h dds_tape.h
zf-cre-open.o:zf-cre-open.c zf-cre-open.h

INC=dds2tar.h dds_tape.h zf-cre-open.h
dds2tar.o:dds2tar.c $(INC)
	$(CC) -c dds2tar.c $(CCOPT) $(CCFLAGS) -DPROGRAM=DDS2TAR -o dds2tar.o
dds2index.o:dds2tar.c $(INC)
	$(CC) -c dds2tar.c $(CCOPT) $(CCFLAGS) -DPROGRAM=DDS2INDEX -o dds2index.o
mt-dds.o:dds2tar.c $(INC)
	$(CC) -c dds2tar.c $(CCOPT) $(CCFLAGS) -DPROGRAM=MTDDS -o mt-dds.o
dds-dd.o:dds2tar.c $(INC)
	$(CC) -c dds2tar.c $(CCOPT) $(CCFLAGS) -DPROGRAM=MTDDS -o dds-dd.o

fio_test:zf-cre-open.c zf-cre-open.h
	cc zf-cre-open.c -DTEST -o fio_test

tags:
	ctags -stv *.c

