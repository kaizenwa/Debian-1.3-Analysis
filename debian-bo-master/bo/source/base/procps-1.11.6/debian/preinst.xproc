#!/bin/sh
set -e

if [ "$1" = "install" ]; then
        dpkg-divert --package xproc --add --rename \
                --divert /usr/X11R6/bin/xload.xcontrib /usr/X11R6/bin/xload
        dpkg-divert --package xproc --add --rename \
                --divert /usr/X11R6/bin/xmem.xcontrib /usr/X11R6/bin/xmem
fi

