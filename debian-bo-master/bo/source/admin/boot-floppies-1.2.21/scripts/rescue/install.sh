#!	/bin/sh

# Install the kernel from this boot floppy onto the root filesystem.
# Don't run LILO, so the hard disk won't be bootable until more is done.
# The commands used by this script are limited to the ones available on
# the root floppy.

#set -x
set -e
umask 022

VERSION=__version__

extract="cpio --format=ustar --extract --unconditional"
if [ -x /bin/star -o -x /usr/bin/star ]; then
	extract="star"
fi

if [ ! install.sh ]; then
	echo 	"Error: Change directory to the boot floppy before running this script."
	1>&2
	exit -1
fi

FLOPPY=`pwd`

if [ $# -gt 0 ]; then
	DESTDIR=$1
fi

if [ ! -d ${DESTDIR}/boot ]; then
	mkdir -m 2775 ${DESTDIR}/boot
fi

cp linux ${DESTDIR}/boot/vmlinuz-${VERSION}
rdev ${DESTDIR}/boot/vmlinuz-${VERSION} ${DESTDIR}
rdev -r ${DESTDIR}/boot/vmlinuz-${VERSION} 0
rdev -R ${DESTDIR}/boot/vmlinuz-${VERSION} 1
rdev -v ${DESTDIR}/boot/vmlinuz-${VERSION} -1
if [ -f sys_map.gz ]; then
	zcat < sys_map.gz > ${DESTDIR}/boot/System.map-${VERSION}
fi
if [ -f ${DESTDIR}/vmlinuz ]; then 
	rm -f ${DESTDIR}/vmlinuz.old
	mv -f ${DESTDIR}/vmlinuz ${DESTDIR}/vmlinuz.old
	rm -f ${DESTDIR}/vmlinuz
fi
(cd ${DESTDIR}/; ln -s boot/vmlinuz-${VERSION} vmlinuz)
if [ -f ${DESTDIR}/System.map ]; then 
	rm -f ${DESTDIR}/System.old
	mv -f ${DESTDIR}/System.map ${DESTDIR}/System.old
	rm -f ${DESTDIR}/System.map
fi
(cd ${DESTDIR}/; ln -s -f boot/System.map-${VERSION} System.map)
sync
exit 0
