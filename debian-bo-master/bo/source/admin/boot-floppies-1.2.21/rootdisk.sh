#!	/bin/sh
# Root Disk maker. Bruce Perens, December 1995
# Dale Scheetz, Sven Rudolph 1996, 1997
# This is free software under the GNU General Public License.

# Dumb GNU tar is broken at the moment.
set -e
# set -v
# set -x

first () {
	echo $1
}

last () {
	eval 'echo $'$#
}

# Abort on any non-zero return.
umask 022

if [ $# != 5 ]; then
echo "Usage: "$0" revision-extension ftp-archive archive blocks debian-version" 1>&2
	cat 1>&2 << EOF

	revision-extension: the revision extension of the kernel.
	ftp-archive: the directory containing the complete Debian mirror
	archive: the directory containing the Debian binary packages.
	blocks: the size of the floppy in 1K blocks, use 720, 1200, or 1440.
	debian-version: version of the Debian release.
EOF

	exit -1
fi

revext=$1

# Set this to the location of the package archive.
ftp_archive=$2

# Set this to the location of the package archive.
archive=$3

# Set to the number of blocks per floppy.
blocks=$4

#Set this to the Debian version
debianversion=$5

# Set this to the block device for the floppy disk.
floppy=/var/tmp/floppy-image.$$

mnt=/var/tmp/mnt.rootdisk

if [ ! -d $archive/. ]; then
	echo "Error: "$archive": not a directory." 1>&2
	exit -1
fi

. common.sh

# To build a rootdisk for an other architecture we need the name
export arch="`dpkg --print-architecture`"

(cd utilities; make)

# Try to unmount anything that should not be mounted.
(umount $mnt; umount $floppy; true)2>/dev/null >/dev/null

mkdir -p $mnt

# Zero the entire disk, so that when I compress the raw disk image,
# unused blocks will compress well.
dd if=/dev/zero of=$floppy bs=1k count=${blocks}

# Make a Minix filesystem.
mkfs.minix -n30 -i 850 $floppy ${blocks}

# Mount the disk.
mount -t minix -o loop $floppy $mnt

# Temporary directories
E=/var/tmp/extract-tmp-$$
R=/var/tmp/root-tmp-$$

# The home of the scripts used by this script.
scripts=`pwd`/scripts/rootdisk

exit=false

for i in \
	$scripts/EXTRACT_LIST_all \
	$scripts/EXTRACT_LIST_${arch} \
	$scripts/SMALL_BASE_LIST_all \
	$scripts/SMALL_BASE_LIST_${arch} \
	$scripts/LINKS \
	;do
	if [ ! -f $i ]; then
		echo "Can't open $i"
		exit=true
	fi
done
if $exit; then
	exit -1
fi

# A list of all the hard links to make to the "busybox" program.
LINKS="`grep -v '^#' $scripts/LINKS`"

# Expand the package names into full pathnames.
PACKAGE_PATHS=`make_paths \`cat $scripts/EXTRACT_LIST_all $scripts/EXTRACT_LIST_${arch}\``

if $exit; then
	exit -1
fi

if [ -d $E ]; then
	mv -f $E $E.old
	rm -f -r $E.old &
fi

mkdir -p -m 755 $E

for i in $PACKAGE_PATHS; do
	# echo Extracting $i 1>&2
	dpkg-deb --extract $i $E
done

# XXX: makedev should accept alternate location of config files
# Bug:
# MAKEDEV-C doesn't take an argument to select the config file, so we are
# currently using the installed ones in /etc.

mkdir -p $R/dev/inet
case "$arch" in
	i386)
		(cd $R/dev; /dev/MAKEDEV -I boot-floppies )
		;;
	m68k)
		(cd $R/dev; /dev/MAKEDEV -I boot-floppies-m68k )
		;;
esac

#( cd $R/dev;  rm -f ippp* isdn* isdnctrl* vcs[2-6]? vcsa[2-6]? \
#	[pt]ty[qrs]* tty[2-6]? ttyI?? )

$scripts/strip_executables $E/bin/* $E/usr/bin/* $E/sbin/* $E/usr/sbin/* \
 $E/lib/* $E/usr/lib/*

(cd $scripts/prototype; tar clf - .)|(cd $R;tar xlpf -)

rm -f $scripts/new_root_home/*~ $scripts/new_root_home/.*~ 
(cd $scripts/new_root_home ; tar cf - .) \
	| gzip -9 > $R/etc/root.sh.tar.gz

gzip -9 $E/usr/share/keytables/*
#(cd $E/usr/share/keytables; tar zcvf keytables.tar.gz \
#    {dvorak,es,fi,fr,it,no,pl,se-latin1,us,uk}.map; rm *.map)
# Use tar to do the copying because it preserves hard links.
(cd $E; tar --files-from=$scripts/SMALL_BASE_LIST_all -cf -)|(cd $R;tar xlpf -)
(cd $E; tar --files-from=$scripts/SMALL_BASE_LIST_${arch} -cf -)|(cd $R;tar xlpf -)
if [ "$revext" != lowmem ]; then
	rm -f $R/bin/mount
	cp $E/bin/mount $R/bin/
	rm -f $R/bin/umount
	cp $E/bin/umount $R/bin/
else
# sbin/badblocks
	( cd $R; rm -f sbin/{badblocks,cfdisk,e2fsck,fsck,fsck.ext2};
		rm -f sbin/{ifconfig,isapnp,losetup,pnpdump,route};
		rm -rf usr/share/keytables;
		rm -rf etc/terminfo/a;
		rm -f bin/{cpio,loadkeys,mount,umount};
		rm -f usr/bin/superformat;
		)
fi

(cd $E/lib; cp \
	`last ld-linux.so.*` \
	`last libcom_err.so.*` \
	`last libe2p.so.*` \
	`last libext2fs.so.*` \
	`last libm.so.*` \
	`last libuuid.so.*` \
 $R/lib )

rm -f -r $E &

cp utilities/busybox/busybox $R/sbin/init
$scripts/strip_executables $R/sbin/init
(cd $R; for j in ${LINKS}; do \
	if [ ! -f $j ]; then \
		ln sbin/init $j ; \
	fi; \
done; )
if [ "$revext" != lowmem ]; then
	(cd $R/sbin; ln e2fsck fsck.ext2)
	(cd $R/sbin; ln e2fsck fsck)
else
	ln $R/sbin/init $R/bin/mount
	ln $R/sbin/init $R/bin/umount
fi

(cd $R/bin; ln ash sh)
(cd $R/bin; ln ae edit)
(cd $R/lib; ln `last ld-linux.*` ld-linux.so.1)

touch $R/etc/architecture.$arch
if [ -n "$revext" ]; then
	touch $R/etc/revision_ext.$revext
fi
#cp modcont$revext $R/usr/lib/module_help/modcont
cp modcont $R/usr/lib/module_help/

tty=`tty`
m4 $scripts/release_notes -D__debianversion__=$debianversion \
	-D__date__="`date '+%B %d, %Y'`" \
	-D__username__=`ls -l  $tty| awk '{ print $3 }'` \
	-D__developers__=`cat $ftp_archive/indices/Maintainers |
		awk '{ print $2 $3 }'|sort|uniq|tail +5 |wc -l| 
		awk '{ print $1 }' | sed -e 's/.$/0/'` >$R/release_notes

# Kludge - remove when this is fixed.
if [ "$arch" = "m68k" ]; then
#Until now linux-m68k has no fdisks in util-linux.deb
#I've only a beta version for atari und amiga from the
#programmer.
  cp /sbin/fdisk /sbin/cfdisk /sbin/atari-fdisk /sbin/afdisk $R/sbin/
fi

EXECUTABLES="$R/bin/* $R/sbin/* $R/usr/bin/* $R/lib/* $R/usr/lib/* /sbin/lilo"
$scripts/generate_library libncurses.so.3.0 /usr/lib/libncurses3.0_pic.a \
 $R/lib/libncurses.so.3.0 $EXECUTABLES

$scripts/generate_library libc.so.5 /usr/lib/libc5_pic.a $R/lib/libc.so.5 \
	$EXECUTABLES

chroot $R /sbin/ldconfig.new -v
rm -f $R/sbin/ldconfig.new &

  
# Replace symbolic links on the floppy with hard links! This saves one block
# per link.
(cd $R;find . -type l -printf '(cd `dirname %p`;if [ ! -d %l ]; then rm %f; if ln %l %f 2>/dev/null; then true; else ln -s %l %f ; fi; fi)\n') | (cd $R ;sh)

echo "root">$R/type.txt

#copy to floppy image

(cd $R; tar cf - .) | (cd $mnt; tar xpf -)

rm -f -r $R &

# Umount the floppy and copy it to a compressed raw disk image file.
df $mnt
umount $mnt
rmdir $mnt

(rm -f root$revext.bin ;true)
if [ "$revext" = lowmem ]; then
	cp $floppy root$revext.bin
else
	gzip -9f <$floppy >root$revext.bin
fi
ls -l root$revext.bin
rm -f $floppy
exit 0
