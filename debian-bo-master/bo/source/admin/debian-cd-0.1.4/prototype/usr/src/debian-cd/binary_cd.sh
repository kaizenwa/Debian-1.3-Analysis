#!	/bin/sh

. vars.sh
. cd_functions.sh

EXCLUDE="$ALWAYS_EXCLUDE \
	$RELEASE/source \
	$ARCHIVE/contrib/source \
	$ARCHIVE/non-free/source"

for i in $EXCLUDE; do
	EXCLUDE_FLAGS="$EXCLUDE_FLAGS -x $i"
done

# Make the /boot directory at the top of the archive.
make_boot_directory

todos < text/binary-cd/SOURCE.txt > $ARCHIVE/SOURCE.txt
todos < text/both/colophon.txt > $ARCHIVE/colophon.txt

# Write .mkisofsrc
VOLI="Debian 1.3 Beta Binary"
cd_info > .mkisofsrc

mkisofs -a -r -T -v \
 $EXCLUDE_FLAGS \
 -b boot/resc1440.bin \
 -c boot/boot.catalog \
 -o $TARGET/binary.iso \
 $ARCHIVE
