rem bootup Debian Installation disks with loadlin
echo "Booting Debian Installation System"
loadlin linux /dev/ram rw initrd=root.bin
