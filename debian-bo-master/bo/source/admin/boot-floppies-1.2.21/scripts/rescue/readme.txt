You can place any compressed Linux kernel on this disk, and it should boot.
To do so:
	Configure the kernel with the following facilities linked in: initrd,
	 ramdisk, msdos, fat, minix, elf, ext2fs, procfs.
    Make your kernel with "make bzImage".
    Copy it to "linux" on the floppy.
    Change directory to the floppy and run ./rdev.sh to configure the kernel.
    Optionally edit syslinux.cfg to add arguments to the "DEFAULT"
    line, or add an "APPEND" line with arguments to be appended to any
    user-typed command line as well as the default.

Documentation to read:
    /usr/lib/syslinux/readme*
    "man rdev"
    /usr/src/linux/documentation/ramdisk.txt

Source code:
    The scripts that create this disk and the other Debian bootstrap disks
    are installed in /usr/src/boot-floppies/ by the boot-floppies package.

- Bruce Perens, 12-March-1996
