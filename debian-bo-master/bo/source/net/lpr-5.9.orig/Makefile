.EXPORT_ALL_VARIABLES:

CC = gcc
CFLAGS = -O2 -fomit-frame-pointer -include /usr/include/bsd/bsd.h \
	-I/usr/include/bsd -I../common_source
LDFLAGS	= -N -s
LDLIBS = -lbsd -lcommon -L ../common_source


SUBDIRS	= common_source filters lpc lpd lpq lpr lprm lptest pac

all:
	for i in $(SUBDIRS); do make -C $$i all; done

install:
	install -o root -g root -m 755 -d ${DEBDIR}/var/spool
	install -o root -g lp -m 2775 -d ${DEBDIR}/var/spool/lpd
	install -o root -g lp -m 2775 -d ${DEBDIR}/var/spool/lpd/lp
	install -o root -g lp -m 2775 -d ${DEBDIR}/var/spool/lpd/remote
	install -o root -g root -m 0755 filter.pcl filter.ps ${DEBDIR}/etc
	install -o root -g root -m 0644 etc.printcap ${DEBDIR}/etc/printcap
	install -o root -g root -m 0644 printcap.5 ${DEBDIR}/usr/man/man5
	for i in $(SUBDIRS); do make -C $$i install; done

clean:
	for i in $(SUBDIRS); do make -C $$i clean; done

