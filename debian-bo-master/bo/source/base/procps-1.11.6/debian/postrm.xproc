#!/bin/sh
set -e

if [ "$1" = "remove" ]; then
        dpkg-divert --package xproc --remove --rename \
                --divert /usr/X11R6/bin/xload.xcontrib /usr/X11R6/bin/xload
        dpkg-divert --package xproc --remove --rename \
                --divert /usr/X11R6/bin/xmem.xcontrib /usr/X11R6/bin/xmem
fi

if [ -x /usr/bin/update-menus ] ; then
        update-menus 
fi
