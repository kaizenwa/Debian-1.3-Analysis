#!	/bin/sh
set -x
set -e
rdev -R /mnt/linux 1
rdev -r /mnt/linux 0
rdev -v /mnt/linux -1
rdev /mnt/linux /dev/ram0
