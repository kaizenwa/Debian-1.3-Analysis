#!	/bin/sh

# Install the kernel from this boot floppy onto the root filesystem.
# Don't run LILO, so the hard disk won't be bootable until more is done.
# The commands used by this script are limited to the ones available on
# the root floppy.

#set -x
set -e
umask 022
retcode=0

extract="cpio --format=ustar --extract --unconditional"
if [ -x /bin/star -o -x /usr/bin/star ]; then
	extract="star"
fi

if [ ! -f install.sh ]; then
	echo 	"Error: Change directory to the floppy before running this script."
	1>&2
	exit -1
fi

FLOPPY=`pwd`

if [ $# -gt 0 ]; then
	DESTDIR=$1
fi

(cd ${DESTDIR}/; zcat < $FLOPPY/modules.tgz|$extract)
#(cd ${DESTDIR}/; depmod -a)

#mv ${DESTDIR}/bin/mount ${DESTDIR}/bin/mount.old
#mv ${DESTDIR}/bin/umount ${DESTDIR}/bin/umount.old

#set +e 
#(cd ${DESTDIR}/; zcat < $FLOPPY/nettools.tgz|$extract)

#for i in mount umount; do
#	if [ ! -x ${DESTDIR}/bin/$i ]; then
#		mv ${DESTDIR}/bin/$i.old ${DESTDIR}/bin/$i
#		retcode=-1
#	else
#		rm -f ${DESTDIR}/bin/$i.old
#	fi
#done

sync
exit $retcode
