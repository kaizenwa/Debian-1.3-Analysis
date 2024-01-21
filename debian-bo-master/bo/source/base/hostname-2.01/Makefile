CFLAGS=-O2 -Wall -Wstrict-prototypes -Wmissing-prototypes -DSTANDALONE
LDFLAGS=-s

# uncomment the following line if you want NLS
# NLS = 1

# uncomment the following line if you want to install to a different base dir.
# BASEDIR=/mnt/test

###############################################################################

ifneq (${NLS},)
CFLAGS += -DNLS=1
endif

OBJS=hostname.o

hostname: nlscat $(OBJS)
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $(OBJS)
	strip hostname
	ln -f hostname dnsdomainname

nlscat:
ifneq (${NLS},)
	(cd nls && make)
endif

install: hostname
ifneq (${NLS},)
	install -o root -g root -m 755 -d ${BASEDIR}/usr/lib/locale/en_US.88591
	install -o root -g root -m 755 -d ${BASEDIR}/usr/lib/locale/fr_FR.88591
	install -o root -g root -m 755 -d ${BASEDIR}/usr/lib/locale/de_DE.88591
	install -o root -g root -m 755 -d ${BASEDIR}/usr/man/en_US.88591/man1
	install -o root -g root -m 755 -d ${BASEDIR}/usr/man/fr_FR.88591/man1
#	install -o root -g root -m 755 -d ${BASEDIR}/usr/man/de_DE.88591/man1
	install -o root -g root -m 755 nls/en_US.88591/hostname.cat ${BASEDIR}/usr/lib/locale/en_US.88591
	install -o root -g root -m 755 nls/fr_FR.88591/hostname.cat ${BASEDIR}/usr/lib/locale/fr_FR.88591
	install -o root -g root -m 755 nls/de_DE.88591/hostname.cat ${BASEDIR}/usr/lib/locale/de_DE.88591
	install -o root -g root -m 755 man/en_US.88591/hostname.1 ${BASEDIR}/usr/man/en_US.88591/man1
	(cd ${BASEDIR}/usr/man/en_US.88591/man1 && ln -fs hostname.1 dnsdomainname.1)
	install -o root -g root -m 755 man/fr_FR.88591/hostname.1 ${BASEDIR}/usr/man/fr_FR.88591/man1
	(cd ${BASEDIR}/usr/man/fr_FR.88591/man1 && ln -fs hostname.1 dnsdomainname.1)
#	install -o root -g root -m 755 man/de_DE.88591/hostname.1 ${BASEDIR}/usr/man/de_DE.88591/man1
#	(cd ${BASEDIR}/usr/man/de_DE.88591/man1 && ln -fs hostname.1 dnsdomainname.1)
else
	install -o root -g root -m 0644 man/en_US.88591/hostname.1 ${BASEDIR}/usr/man/man1
	(cd ${BASEDIR}/usr/man/man1 && ln -fs hostname.1 dnsdomainname.1)
endif
	install -o root -g root -m 0755 hostname ${BASEDIR}/bin
	(cd ${BASEDIR}/bin && ln -f hostname dnsdomainname) || exit 1

clean:
ifneq (${NLS},)
	(cd nls && make clean)
endif
	rm -f $(OBJS) hostname dnsdomainname core

