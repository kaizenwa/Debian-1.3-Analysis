#!	/bin/sh

. vars.sh
. cd_functions.sh

EXCLUDE=" \
    $ALWAYS_EXCLUDE \
	$ARCHIVE/doc \
	$ARCHIVE/tools \
	$ARCHIVE/boot \
	$ARCHIVE/frozen \
	$ARCHIVE/stable \
	$ARCHIVE/stable \
	$RELEASE/binary-i386 \
	$RELEASE/binary-all \
	$RELEASE/msdos-i386 \
	$RELEASE/msdos-all \
	$RELEASE/Packages \
	$RELEASE/Packages.gz \
	$ARCHIVE/contrib/binary-i386 \
	$ARCHIVE/contrib/msdos-i386 \
	$ARCHIVE/contrib/binary-all \
	$ARCHIVE/contrib/msdos-all \
	$ARCHIVE/contrib/binary \
	$ARCHIVE/contrib/Packages \
	$ARCHIVE/contrib/Packages.gz \
	$ARCHIVE/non-free/binary-i386 \
	$ARCHIVE/non-free/binary-all \
	$ARCHIVE/non-free/msdos-i386 \
	$ARCHIVE/non-free/msdos-all \
	$ARCHIVE/non-free/binary \
	$ARCHIVE/non-free/Packages \
	$ARCHIVE/non-free/Packages.gz \
	$RELEASE/binary-i386 \
	$RELEASE/binary-all \
	$RELEASE/disks-i386 \
	$RELEASE/msdos-i386 \
	$RELEASE/msdos-all \
	$RELEASE/binary \
	$RELEASE/Packages \
	$RELEASE/Packages.gz"

for i in $EXCLUDE; do
	EXCLUDE_FLAGS="$EXCLUDE_FLAGS -x $i"
done

# Write .mkisofsrc
VOLI="Debian 1.3 Beta Source"
cd_info > .mkisofsrc

todos < text/both/colophon.txt > $ARCHIVE/colophon.txt

mkisofs -a -r -T -v \
 $EXCLUDE_FLAGS \
 -o source.iso \
 $ARCHIVE
