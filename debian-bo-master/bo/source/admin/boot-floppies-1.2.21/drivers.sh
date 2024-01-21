#!	/bin/sh
# Boot Disk maker. Bruce Perens, July-September 1995
# This is free software under the GNU General Public License.

# Abort on any non-zero return.
set -e

# Verbose shell execution.
# set -x
# set -v

# Print a usage message and exit if the argument count is wrong.
if [ $# -ne 3 ]; then
echo "Usage: $0 revision-extension blocks modules-archive" 1>&2
	cat 1>&2 << EOF

	revision-extension: the revision extension of the kernel.
	blocks: the size of the floppy in 1K blocks, use 720, 1200, or 1440
	modules-archive: tgz containing the modules

EOF

	exit -1
fi

floppy=/var/tmp/floppy-image.$$

mnt=/var/tmp/mnt.$$

revext="$1"

blocks=$2

archive=$3

# Make sure the modules archive is there.
if [ ! -f $archive ]; then
	echo "Can't find $archive" 1>&2
	exit -1
fi

# Try to unmount anything that should not be mounted. Aborted runs of this
# script may have left things mounted.
(umount $mnt; umount $floppy; true)2>/dev/null >/dev/null

mkdir -p $mnt

# Zero the entire disk, so that if I compress the raw disk image,
# unused blocks will compress well.
dd if=/dev/zero of=$floppy bs=1k count=$blocks

# Make the filesystem.
mkdosfs -r 16 -n drivers -m scripts/drivers/boot.txt $floppy $blocks

# mount the disk
mount -o loop $floppy $mnt

cp $archive $mnt/modules.tgz

cp scripts/drivers/install.sh $mnt/install.sh
chmod 777 $mnt/install.sh
#echo ${revext}"drivers">$mnt/type.txt
echo drivers>$mnt/type.txt

umount $mnt
rmdir $mnt

mv $floppy drv${blocks}${revext}.bin
ls -l drv${blocks}${revext}.bin
exit 0
