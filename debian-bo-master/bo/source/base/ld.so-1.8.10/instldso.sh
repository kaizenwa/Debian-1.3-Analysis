#!/bin/sh

umask 022

echo 
echo 'This version of ld.so requires a Linux kernel version 2.0.0'
echo 'or later with ELF support compiled in.  Using the binfmt_elf'
echo 'module is not sufficient.  If your system does not meet these'
echo 'requirements, press Control-C now to abort the installation.'
echo 'Press Enter to continue.'
if [ -z "$AUTOINST" ]; then
       read
fi

echo 'Expect some cache has wrong version warnings if upgrading'
echo 'from a version prior to 1.7.0.'

. ./Version.mk

if [ -z "$PREFIX" ]; then
       PREFIX=
fi
STRIP=-s

install -d -m 755 $PREFIX/etc
install -d -m 755 $PREFIX/sbin
install -d -m 755 $PREFIX/lib
install -d -m 755 $PREFIX/usr/bin
install -d -m 755 $PREFIX/usr/lib
install -d -m 755 $PREFIX/usr/include
install -d -m 755 $PREFIX/usr/man/man1
install -d -m 755 $PREFIX/usr/man/man3
install -d -m 755 $PREFIX/usr/man/man8
install -d -m 755 $PREFIX/usr/info

#if [ -f /etc/ld.so.cache ] ; then
#	echo Deleting old /etc/ld.so.cache
#	rm -f /etc/ld.so.cache
#fi

if [ ! -f $PREFIX/etc/ld.so.conf ] ; then
	echo Creating new /etc/ld.so.conf
	for dir in /usr/local/lib /usr/X11R6/lib /usr/X386/lib /usr/openwin/lib /lib/elf ; do
		if [ -d $PREFIX$dir ] ; then
			echo $dir >> $PREFIX/etc/ld.so.conf
		fi
	done
fi

if [ -f ld-so/ld.so ] ; then
	echo Installing ld.so
	install $STRIP ld-so/ld.so $PREFIX/lib/ld.so.$VERSION
	mv -f $PREFIX/lib/ld.so.$VERSION $PREFIX/lib/ld.so
	ln -f $PREFIX/lib/ld.so $PREFIX/lib/ld.so.$VERSION
else
	echo Not installing a.out support
fi

echo Installing ld-linux.so
install d-link/ld-linux.so $PREFIX/lib/ld-linux.so.$VERSION
if [ -n "$STRIP" ] ; then
    strip -g -K _dl_debug_state $PREFIX/lib/ld-linux.so.$VERSION
fi
mv -f $PREFIX/lib/ld-linux.so.$VERSION $PREFIX/lib/ld-linux.so.$VMAJOR
ln -f $PREFIX/lib/ld-linux.so.$VMAJOR $PREFIX/lib/ld-linux.so.$VERSION

echo Installing libdl.so
install -m 644 d-link/libdl/dlfcn.h $PREFIX/usr/include/dlfcn.h
install $STRIP d-link/libdl/libdl.so $PREFIX/lib/libdl.so.$VERSION
ln -sf libdl.so.$VERSION $PREFIX/lib/libdl.so

echo Installing ldd
install $STRIP util/ldd $PREFIX/usr/bin
echo Installing lddstub
install $STRIP util/lddstub $PREFIX/usr/lib/lddstub

echo Installing and running ldconfig
install $STRIP util/ldconfig $PREFIX/sbin
$PREFIX/sbin/ldconfig

echo Installing manual and info pages
install -m 644 man/ldd.1 $PREFIX/usr/man/man1
install -m 644 man/ldconfig.8 man/ld.so.8 $PREFIX/usr/man/man8
install -m 644 man/dlopen.3 $PREFIX/usr/man/man3
ln -sf dlopen.3 $PREFIX/usr/man/man3/dlsym.3
ln -sf dlopen.3 $PREFIX/usr/man/man3/dlerror.3
ln -sf dlopen.3 $PREFIX/usr/man/man3/dlclose.3
install -m 644 man/ld.so.info $PREFIX/usr/info

echo Installation complete
