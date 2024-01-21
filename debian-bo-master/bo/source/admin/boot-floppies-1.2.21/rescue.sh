#!	/bin/sh
# Boot Disk maker. Bruce Perens, July-September 1995
# This is free software under the GNU General Public License.

# Abort on any non-zero return.
set -e

# Verbose shell execution.
# set -x
# set -v

# Print a usage message and exit if the argument count is wrong.
if [ $# != 5 ]; then
echo "Usage: "$0" revision-extension kernel-package root-image blocks debian-version" 1>&2
	cat 1>&2 << EOF

	revision-extension: the revision extension of the kernel.
	kernel-package: the Debian package containing the kernel.
	root-image: a compressed disk image to load in ramdisk and mount as root.
	blocks: the size of the floppy in 1K blocks, use 720, 1200, or 1440.
	debian-version: version of the Debian release.

EOF

	exit -1
fi

revext=$1

# Set this to the location of the package archive.
archive=$2

# Set this to the location of the root filesystem image
rootimage=$3

# Set to the number of blocks per floppy.
blocks=$4

#Set this to the Debian version
debianversion=$5

# Set this to the block device for the floppy disk.
floppy=/var/tmp/floppy-image.$$

mnt=/var/tmp/mnt.$$

loopdevice=/dev/loop3

# Make sure the kernel package is available.
if [ ! -f $archive ]; then
	echo "Can't find $archive" 1>&2
	exit -1
fi

# Try to unmount anything that should not be mounted. Aborted runs of this
# script may have left things mounted.
(umount /mnt; umount $mnt; umount $floppy; true)2>/dev/null >/dev/null

mkdir -p $mnt

# Zero the entire disk, so that when I compress the raw disk image,
# unused blocks will compress well.
dd if=/dev/zero of=$floppy bs=1k count=$blocks

# Extract the kernel from its package.
(rm -f -r /var/tmp/extract-tmp-$$;true)
mkdir /var/tmp/extract-tmp-$$
dpkg-deb --extract $archive /var/tmp/extract-tmp-$$

# Get the kernel version from its package name.
KERNEL_VERSION=`basename $archive | sed -e 's/kernel-image-//' -e 's/_.*//'`
ARCHITECTURE=`dpkg --print-architecture`
DEBIAN_KERNEL_IMAGE=`basename $archive .deb | sed -e s/_$ARCHITECTURE\$//`

# Do the actual work of making the disk bootable.
if [ $blocks -le 1400 ]; then
    rootimage=/var/tmp/root.$$
    echo -n >$rootimage
fi

#XXX find free loop device
losetup -d $loopdevice || true
losetup $loopdevice $floppy
mkrboot syslinux /var/tmp/extract-tmp-$$/boot/vmlinuz-*.*.* $rootimage $loopdevice $blocks
losetup -d $loopdevice

if [ $blocks -le 1400 ]; then
    rm -f $rootimage
fi


# mount the disk image to install additional files
mount -o loop $floppy $mnt

# Copy system.map to the floppy.
if [ "$revext" != lowmem ]; then
	cp sys_map${revext}.gz $mnt/sys_map.gz
fi

# Install the greeting and help messages
for i in debian.txt readme.txt f1.txt f10.txt f2.txt f3.txt \
		f4.txt f5.txt f6.txt f7.txt f8.txt f9.txt ; do
	TODOS="unix2dos"
#	case $i in
#	install.sh|rdev.sh)
#		TODOS=cat
#	esac

	if [ -n "$revext" ]; then
		revextdefine="-D__${revext}__"
	fi

	cat scripts/rescue/$i | \
		m4 -P -D__kernel_version__=$KERNEL_VERSION \
				-D__kernel_image__=$DEBIAN_KERNEL_IMAGE \
				-D__size${blocks}__ \
				-D__debianversion__=$debianversion \
			${revextdefine}| $TODOS > $mnt/$i
done

sed s/__version__/$KERNEL_VERSION/ <scripts/rescue/syslinux.cfg |\
        m4 -D__size${blocks}__ | unix2dos >$mnt/syslinux.cfg
sed s/__version__/$KERNEL_VERSION/ <scripts/rescue/install.sh >$mnt/install.sh
sed s/__version__/$KERNEL_VERSION/ <scripts/rescue/rdev.sh >$mnt/rdev.sh

chmod 777 $mnt/install.sh $mnt/rdev.sh
if [ "$revext" = lowmem ]; then
	echo lowmem >$mnt/type.txt
else
	echo rescue >$mnt/type.txt
#	cp rescmods${revext}.tgz $mnt/modules.tgz
fi

umount $mnt
rmdir $mnt

(rm -f -r /var/tmp/extract-tmp-$$ resc${blocks}${revext}.bin;true)
mv $floppy resc${blocks}${revext}.bin
ls -l resc${blocks}${revext}.bin
exit 0
