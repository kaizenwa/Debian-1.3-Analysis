#!/bin/sh
#
#
echo 
echo "This version of the setserial package comes with a new"
echo "/etc/rc.boot/0setserial file which attempts to ensure that"
echo "the serial module is loaded if the module is not compiled"
echo "into the kernel or loaded already. I would therefore recommend"
echo "that you answer Y after taking a copy of your config file, and"
echo "reconfigure the new file manually..."
echo 
exit 0
#
