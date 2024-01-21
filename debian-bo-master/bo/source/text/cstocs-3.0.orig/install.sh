#!/bin/sh

: ${PREFIX:=/usr/local}

: ${LIBDIR:=${PREFIX}/lib/cstocs}
: ${BINDIR:=${PREFIX}/bin}
: ${MANDIR:=${PREFIX}/man/man1}

echo -n "Installing cstocs ..."

umask 022

rm -rf ${LIBDIR}
mkdir -p ${LIBDIR}

# Let's see whether perl5 is installed ...
PERLBINARY="`which perl5`"
if [ "$?" -ne 0 ]
then
	PERLBINARY="`which perl`"
	if [ "$?" -ne 0 ]
	then
		DEFAULTCSTOCS="cstocs.awk"
		PERLBINARY="/usr/bin/perl"
	fi
fi
[ "$DEFAULTCSTOCS" = "" ] && DEFAULTCSTOCS="cstocs.pl"

sed "s,__DEFAULT_CSTOCSDIR__,${LIBDIR},g" <cstocs.awk.in >${LIBDIR}/cstocs.awk
sed "s,__DEFAULT_CSTOCSDIR__,${LIBDIR},g
	s,__PERL_BINARY__,${PERLBINARY},g" <cstocs.pl.in >${LIBDIR}/cstocs.pl

cp *.enc accent ${LIBDIR}

rm -f ${MANDIR}/cstocs.1 ${MANDIR}/cstocs.1.gz
sed "s,__DEFAULT_CSTOCSDIR__,${LIBDIR},g" <cstocs.1.in \
	| gzip -9 >${MANDIR}/cstocs.1.gz

chmod 644 ${LIBDIR}/* ${MANDIR}/cstocs.1.gz
chmod 755 ${LIBDIR}/cstocs.awk ${LIBDIR}/cstocs.pl ${LIBDIR}
ln -s ../lib/cstocs/${DEFAULTCSTOCS} /usr/bin/cstocs
echo "Done."
exit 0
