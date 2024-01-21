#!/bin/sh
#
# /etc/rc.d/rc.S:  System initialization script.
#
# Mostly written by:  Patrick J. Volkerding, <volkerdi@ftp.cdrom.com>
#

PATH=/sbin:/usr/sbin:/bin:/usr/bin

# enable swapping
/sbin/swapon -a

# Start update.
/sbin/update &

# Test to see if the root partition is read-only, like it ought to be.
READWRITE=no
if echo -n >> "Testing filesystem status"; then
 rm -f "Testing filesystem status"
 READWRITE=yes
fi

# Check the integrity of all filesystems
if [ ! $READWRITE = yes ]; then
 /sbin/fsck -A -a
 # If there was a failure, drop into single-user mode.
 if [ $? -gt 1 ] ; then
  echo
  echo
  echo "**************************************"
  echo "fsck returned error code - REBOOT NOW!"
  echo "**************************************"
  echo
  echo
  /bin/login 
 fi
 # Remount the root filesystem in read-write mode
 echo "Remounting root device with read-write enabled."
 /sbin/mount -w -n -o remount /
 if [ $? -gt 0 ] ; then
  echo
  echo "Attempt to remount root device as read-write failed!  This is going to"
  echo "cause serious problems...  "
  echo 
  echo "If you're using the UMSDOS filesystem, you **MUST** mount the root partition"
  echo "read-write!  You can make sure the root filesystem is getting mounted "
  echo "read-write with the 'rw' flag to Loadlin:"
  echo
  echo "loadlin vmlinuz root=/dev/hda1 rw   (replace /dev/hda1 with your root device)"
  echo
  echo "Normal bootdisks can be made to mount a system read-write with the rdev command:"
  echo
  echo "rdev -R /dev/fd0 0"
  echo
  echo "You can also get into your system by using a bootkernel disk with a command"
  echo "like this on the LILO prompt line:  (change the root partition name as needed)"
  echo 
  echo "LILO: mount root=/dev/hda1 rw"
  echo
  echo "Please press ENTER to continue, then reboot and use one of the above methods to"
  echo -n "get into your machine and start looking for the problem. " 
  read junk; 
 fi
else
 echo "Testing filesystem status: read-write filesystem"
 if [ ! -d /DOS ]; then # no warn for UMSDOS (kind of a bad test, but...)
  cat << EOF

*** ERROR: Root partition has already been mounted read-write. Cannot check!

For filesystem checking to work properly, your system must initially mount
the root partition as read only. Please modify your kernel with 'rdev' so that
it does this. If you're booting with LILO, add a line:

   read-only

to the Linux section in your /etc/lilo.conf and type 'lilo' to reinstall it.

If you boot from a kernel on a floppy disk, put it in the drive and type:
  rdev -R /dev/fd0 1

If you boot from a bootkernel disk, or with Loadlin, you can add the 'ro' flag.

This will fix the problem *AND* eliminate this annoying message. :^)

EOF
  echo -n "Press ENTER to continue. "
  read junk;
 fi
fi

# remove /etc/mtab* so that mount will create it with a root entry
/bin/rm -f /etc/mtab* /etc/nologin /var/run/*

# mount file systems in fstab (and create an entry for /)
# but not NFS because TCP/IP is not yet configured
/sbin/mount -avt nonfs

# Clean up the utmp file.
( cd /var/run && find . ! -type d -exec rm -f -- {} \; )

# Looks like we have to create this.
cat /dev/null > /var/run/utmp

# Configure the system clock.
# This can be changed if your system keeps GMT.
if [ -x /sbin/clock ]; then
  /sbin/clock -s
fi

# Setup the /etc/issue and /etc/motd to reflect the current kernel level:
# THESE WIPE ANY CHANGES YOU MAKE TO /ETC/ISSUE AND /ETC/MOTD WITH EACH
# BOOT. COMMENT THEM OUT IF YOU WANT TO MAKE CUSTOM VERSIONS.
echo > /etc/issue
echo Welcome to Linux `/bin/uname -a | /bin/cut -d\  -f3`. >> /etc/issue
echo >> /etc/issue
echo "`/bin/uname -a | /bin/cut -d\  -f1,3`." > /etc/motd

# Run serial port setup script:
# (CAREFUL! This can make some systems hang if the rc.serial script isn't
# set up correctly. If this happens, you may have to edit the file from a
# boot disk)
#
# . /etc/rc.d/rc.serial

