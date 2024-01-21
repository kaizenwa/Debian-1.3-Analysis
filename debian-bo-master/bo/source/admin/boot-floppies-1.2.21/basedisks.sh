#!	/bin/bash

# Abort on any non-zero return.
set -e

# Verbose shell execution.
# set -v
# set -x

umask 022

if [ $# -ne 2 ]; then
	echo "Usage:	"$0 "debian-archive-directory debian-version" 1>& 2
	cat 1>&2 << EOF

	Creates a new base archive.

	debian-archive-directory: the local copy of the Debian archive.
	debian-version: version of the Debian release

EOF
	exit -1;
fi

. common.sh

# Set this to the location of the package archive.
archive=$1

# 
version=`echo $2 | sed -e 's/\./_/'`

# To build a rootdisc for an other architecture we need the name
arch="`dpkg --print-architecture`"

# Temporary directories
B=/var/tmp/base-tmp-$$
P=/var/tmp/partial-tmp-$$

# The home of the scripts used by this script.
scripts=`pwd`/scripts/basedisks

exit=false

for i in $archive/Packages.gz $scripts/PACKAGES_all $scripts/PACKAGES_${arch} \
 $scripts/PARTIAL_PACKAGES_all $scripts/PARTIAL_PACKAGES_${arch}; do
	if [ ! -f $i ]; then
		echo "Can't open $i"
		exit=true
	fi
done
if $exit; then
	exit -1;
fi

# Expand the package names into full pathnames.
PACKAGE_PATHS=`make_paths \`cat $scripts/PACKAGES_all \
 $scripts/PACKAGES_${arch}\``
PARTIAL_PACKAGE_PATHS=`make_paths \`cat $scripts/PARTIAL_PACKAGES_all \
 $scripts/PARTIAL_PACKAGES_${arch}\``

if $exit; then
	exit -1;
fi

rm -f -r $B $P # Paranoia.

for i in $PACKAGE_PATHS; do
	# echo Extracting $i 1>&2
	dpkg-deb --extract $i $B
done

for i in $PARTIAL_PACKAGE_PATHS; do
	# echo Extracting $i 1>&2
	dpkg-deb --extract $i $P
done

(cd $P; tar -cf - `cat $scripts/PARTIAL_PACKAGE_FILES_all \
 $scripts/PARTIAL_PACKAGE_FILES_${arch}`) \
 |(cd $B;tar xvlpf -)

gzip -d < $archive/Packages.gz > $B/var/lib/dpkg/available

# Miscellaneous links normally done by the postinst scripts.
if [ ! -f $B/etc/ld.so.cache ]; then
	cp /etc/ld.so.cache $B/etc/ld.so.cache
	chmod 644 $B/etc/ld.so.cache
	chown root.root $B/etc/ld.so.cache
fi

if [ "$arch" = "m68k" ]; then
  #Until now linux-m68k has no fdisks in util-linux.deb
  #I've only a beta version for atari und amiga from the
  #programmer.
  cp /sbin/fdisk /sbin/cfdisk /sbin/atari-fdisk /sbin/afdisk $B/sbin
  #and some files are missing in syslinux.deb
  `cp /usr/lib/syslinux/img*Atari.gz $B/usr/lib/syslinux`
  (cd $B/dev; $B/dev/MAKEDEV -c -I ad atarimouse amigamouse amigamouse1 fb)
fi

if [ ! -f $B/lib/ld-linux.so.1 ]; then
	(cd $B/lib; ln -s ld-linux.so.1.* ld-linux.so.1)
fi

if [ ! -f $B/usr/bin/perl -a -f $B/usr/bin/perl.dist ]; then
	(cd $B/usr/bin; mv perl.dist perl)
fi
(cd $B/bin; ln -s ../usr/bin/perl perl)
echo -n > $B/var/lib/dpkg/status

mkdir -p $B/dev/inet
case "$arch" in
	i386)
		(cd $B/dev; /dev/MAKEDEV -I generic )
		;;
	m68k)
		(cd $B/dev; /dev/MAKEDEV -I generic-m68k )
		;;
esac

(cd $B; chroot $B sbin/ldconfig.new)
trap "umount $B/proc" 0
chroot $B mount -t proc proc /proc
(cd $B;dpkg --root=. --install --force-depends --force-auto-select `make_paths ldso base-files libc5 dpkg`)

chroot $B /sbin/ldconfig
(cd $B;dpkg --root=. --unpack --force-auto-select \
 --force-depends --force-overwrite $PACKAGE_PATHS)

# Bug: ppp insists that /var/run/syslogd.pid exist.
echo "1">$B/var/run/syslogd.pid

mv $B/usr/sbin/start-stop-daemon $B/usr/sbin/start-stop-daemon.REAL
cp $B/bin/true $B/usr/sbin/start-stop-daemon
(cd $B/etc; ln -s ../usr/lib/zoneinfo/Factory localtime)
echo America/Los_Angeles > $B/etc/timezone # Avoid post-inst question.
chmod 644 $B/etc/localtime $B/etc/timezone
(cd $B;yes N | dpkg --root=. --configure --pending --force-configure-any \
 --force-depends ; )
mv $B/usr/sbin/start-stop-daemon.REAL $B/usr/sbin/start-stop-daemon
echo Factory > $B/etc/timezone
chroot $B umount /proc
trap 0

# Bug: ppp insists that /var/run/syslogd.pid exist.
rm -f $B/var/run/syslogd.pid

# XXX #9189
touch $B/etc/init.d/xdm
chmod a+x $B/etc/init.d/xdm

# XXX
sed -e 's,/sbin/setup.sh,/root/setup.sh,' \
	<$B/etc/init.d/boot >$B/etc/init.d/boot.tmp
mv $B/etc/init.d/boot.tmp $B/etc/init.d/boot
chmod a+x $B/etc/init.d/boot

# Locale files make the base a bit larger. Un-comment this if it becomes
# a problem.
# rm -f -r $B/usr/share/locale $B/usr/share/gettext $B/usr/share/nls

# will be copied from or created by the root disk
rm -f $B/etc/{conf.modules,modules} $B/etc/init.d/network

(find $B -name '*.dpkg-old' -exec rm -f \{\} \; ; true)
rm -f $B/var/lib/dpkg/status-old $B/var/lib/dpkg/available-old
(cd $B; tar clf - .)| gzip --best > base${version}.tgz
rm -f -r $B $P &
if [ $arch = i386 ]; then
	./utilities/floppy_split base${version}.tgz base 1200
elif [ $arch = m68k ]; then
	./utilities/floppy_split base${version}.tgz base14 1440
	./utilities/floppy_split base${version}.tgz base72 720
fi
exit 0
