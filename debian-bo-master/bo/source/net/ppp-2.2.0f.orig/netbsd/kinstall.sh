#!/bin/sh

# This script modifies the kernel sources in /sys to install
# ppp-2.2.  It is intended to be run in the ppp-2.2 directory.
#
# Paul Mackerras	17-Mar-95

ARCH=$(uname -m)
CONF=$(uname -v | sed 's/.*(\(.*\)).*/\1/')
SYS=/sys
ARCHDIR=$SYS/arch/$ARCH
CFILE=$ARCHDIR/conf/$CONF
DOCONF=
DOMAKE=

# Work out whether to use config or config.new
if grep -q timezone $CFILE; then
  CONFIG=config
else
  CONFIG=config.new
fi

# Copy new versions of files into /sys/net

for f in net/if_ppp.h net/ppp-comp.h net/ppp_defs.h netbsd/bsd-comp.c \
	 netbsd/if_ppp.c netbsd/if_pppvar.h netbsd/ppp_tty.c \
	 netbsd/slcompress.c netbsd/slcompress.h; do
  dest=$SYS/net/$(basename $f)
  if [ -f $dest ]; then
    if ! cmp -s $f $dest; then
      echo "Copying $f to $dest"
      mv -f $dest $dest.orig
      echo " (old version saved in $dest.orig)"
      cp $f $dest
      DOMAKE=yes
    fi
  else
    echo "Copying $f to $dest"
    cp $f $dest
    DOMAKE=yes
  fi
done

# Add extra stuff to /sys/conf/files or /sys/conf/files.newconf

if [ -f $SYS/conf/files.oldconf ]; then
  OLDFILES=files.oldconf
  NEWFILES=files
  OLDCONFIG=config.old
  NEWCONFIG=config
else
  OLDFILES=files
  NEWFILES=files.newconf
  OLDCONFIG=config
  NEWCONFIG=config.new
fi

if [ -f $SYS/conf/$OLDFILES ]; then
  if ! grep -q ppp_tty $SYS/conf/$OLDFILES; then
    echo "Patching $SYS/conf/$OLDFILES"
    patch -N $SYS/conf/$OLDFILES <netbsd/files.patch
    if [ $CONFIG = $OLDCONFIG ]; then
      DOCONF=yes
    fi
  fi
fi
if [ -f $SYS/conf/$NEWFILES ]; then
  if ! grep -q ppp_tty $SYS/conf/$NEWFILES; then
    echo "Patching $SYS/conf/$NEWFILES"
    patch -N $SYS/conf/$NEWFILES <netbsd/files.newconf.patch
    if [ $CONFIG = $NEWCONFIG ]; then
      DOCONF=yes
    fi
  fi
fi

# Add in patch to call PPP software interrupt routine.

if ! grep -q NETISR_PPP $SYS/net/netisr.h; then
  echo "Patching $SYS/net/netisr.h"
  patch -p -N -d $SYS/net <netbsd/netisr.h.patch
  DOMAKE=yes
fi

d=$ARCH
case $ARCH in
  amiga)  p=machdep.c;;
  hp300)  p=machdep.c;;
  i386)   d=isa; p=icu.s;;
  mac68k) p=machdep.c;;
  pc532)  p=locore.s;;
  pmax)   p=trap.c;;
  sparc)  p=intr.c;;
  sun3)   p=isr.c;;
esac

if [ x$p != x -a -f $ARCHDIR/$d/$p ]; then
  if ! grep -q NETISR_PPP $ARCHDIR/$d/$p; then
    echo "Patching $ARCHDIR/$d/$p"
    patch -p -N -d $ARCHDIR/$d < netbsd/arch/$ARCH/$p.patch
    DOMAKE=yes
  fi
fi

# Tell the user to add a pseudo-device line to the configuration file
# and remake the kernel, if necessary.

if [ -f $CFILE ]; then
  if ! grep -q '^[ 	]*pseudo-device[ 	][ 	]*ppp' $CFILE; then
    echo
    echo "The currently-running kernel was built from configuration file"
    echo "$CFILE, which does not include PPP."
    echo "You need either to add a line like 'pseudo-device ppp 2' to"
    echo "this file, or use another configuration file which includes"
    echo "a line like this."
    DOCONF=yes
  fi
fi

if [ $DOCONF ]; then
  echo
  echo "You need to configure and build a new kernel."
  echo "The procedure for doing this involves the following commands."
  echo "(\"$CONF\" may be replaced by the name of another config file.)"
  echo
  echo "	cd $ARCHDIR/conf"
  echo "	/usr/sbin/$CONFIG $CONF"
  echo "	cd ../compile/$CONF"
  echo "	make depend"
  DOMAKE=yes
elif [ $DOMAKE ]; then
  echo "You need to build a new kernel."
  echo "The procedure for doing this involves the following commands."
  echo
  echo "	cd $ARCHDIR/compile/$CONF"
fi
if [ $DOMAKE ]; then
  echo "	make"
  echo
  echo "Then copy the new kernel ($ARCHDIR/compile/$CONF/netbsd)"
  echo "to /netbsd and reboot.  (Keep a copy of the old /netbsd,"
  echo "just in case.)"
fi
