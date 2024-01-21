cd_info ()
{
	if [ -n "$APPI" ]; then
		echo  APPI=\"$APPI\"
	fi
	if [ -n "$COPY" ]; then
		echo  COPY=\"$COPY\"
	fi
	if [ -n "$ABST" ]; then
		echo  ABST=\"$ABST\"
	fi
	if [ -n "$BIBL" ]; then
		echo  BIBL=\"$BIBL\"
	fi
	if [ -n "$PREP" ]; then
		echo  PREP=\"$PREP\"
	fi
	if [ -n "$PUBL" ]; then
		echo  PUBL=\"$PUBL\"
	fi
	if [ -n "$SYSI" ]; then
		echo  SYSI=\"$SYSI\"
	fi
	if [ -n "$VOLI" ]; then
		echo  VOLI=\"$VOLI\"
	fi
	if [ -n "$VOLS" ]; then
		echo  VOLS=\"$VOLS\"
	fi

	return 0
}

make_boot_directory ()
{
	tmp=/tmp/$$

	if [ ! -d $ARCHIVE/boot ]; then
		mkdir -m 755 $ARCHIVE/boot
	fi

	cp $RELEASE"/disks-"$ARCHITECTURE/current/linux \
	 $ARCHIVE/boot
	cp $RELEASE"/disks-"$ARCHITECTURE/current/lmemroot.bin \
	 $ARCHIVE/boot
	cp $RELEASE"/disks-"$ARCHITECTURE/current/root.bin \
	 $ARCHIVE/boot
	cp $RELEASE"/disks-"$ARCHITECTURE/current/resc1440.bin \
	 $ARCHIVE/boot
	(mkdir $tmp; cd $tmp; unzip $ARCHIVE/tools/lodlin*.zip)
	cp $tmp/*/*.EXE $ARCHIVE/boot

	for i in batch/*; do
		todos < $i > $ARCHIVE/boot/`echo $i | sed -e s!batch/!!`
	done

	return 0
}
