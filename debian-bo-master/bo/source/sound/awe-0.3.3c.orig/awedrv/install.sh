#!/bin/sh
#
# AWE32 sound driver installation script for Linux systems
#
# Run this script by sh, "sh install.sh [type [samples infos]]"
# type is VOXWARE (for Linux-1.2.x & 1.3.x)
#	  USS     (for Linux-2.0.1 or newer)
#	  OldOSS  (for Linux-2.1.0 to 2.1.8)
#	  OSS     (for Linux-2.1.9 or newer)
#	  OSS38	  (for Linux-2.1.25 or newer)
# if type is unspecified, examine it from kernel source.

#----------------------------------------------------------------
# user input
#----------------------------------------------------------------

check_sound_version () {
	echo "checking the version of installed sound driver.."
	cat > getver$$.c << __EOF__ 
#include <stdio.h>
#include <linux/soundcard.h>
#include <soundvers.h>
int main()
{
#ifdef SOUND_INTERNAL_VERSION
#if SOUND_INTERNAL_VERSION == 0x30500
	printf("OldUSS");
#elif SOUND_INTERNAL_VERSION >= 0x030501 && SOUND_INTERNAL_VERSION < 0x030700
	printf("USS");
#elif SOUND_INTERNAL_VERSION >= 0x030700 && SOUND_INTERNAL_VERSION < 0x030707
	printf("OldOSS");
#elif SOUND_INTERNAL_VERSION >= 0x030707 && SOUND_INTERNAL_VERSION < 0x030803
	printf("OSS");
#elif SOUND_INTERNAL_VERSION >= 0x030803
	printf("OSS38");
#else
	printf("unknown");
#endif
#elif SOUND_VERSION >= 300 && SOUND_VERSION < 350
	printf("VOXWARE");
#else
	printf("unknown");
#endif
	return 0;
}
__EOF__
	cc -I$linuxroot/include -I$linuxroot/drivers/sound -o getver$$ getver$$.c
	sound_version=`./getver$$`
	rm -f getver$$ getver$$.c
}

readln () {
	echo -n "$1"
	IFS='@' read ans </dev/tty || exit 1
	[ -z "$ans" ] && ans=$2
}

read_bool () {
	readln "$1? [$2] " "$2"
	[ $ans = yes ]
}

chk_dir () {
	[ ! -d $1 ]
}

# read_value message default-vaule variable-name check-command
read_value () {
	while [ 1 = 1 ]; do
		readln "$1 [$2] " "$2"
		eval "$3=\"$ans\""
		if [ $4 = "nocheck" ]; then
			break
		elif $4 "$ans"; then
			echo "entry $ans already exists: input again."
			echo
		else
			break
		fi
	done
}

setup_config () {
	tmpconfig=/tmp/awe_config.$$
	tmpconfig2=/tmp/awe_conf2.$$
	rm -f $tmpconfig $tmpconfig2
	cp awe_config.h $tmpconfig
}

define_condition () {
	sed -e "s@^#undef $1\$@#define $1@" $tmpconfig > $tmpconfig2
	mv -f $tmpconfig2 $tmpconfig
}

undef_condition () {
	sed -e "s@^#define $1\$@#undef $1@" $tmpconfig > $tmpconfig2
	mv -f $tmpconfig2 $tmpconfig
}

define_value () {
	sed -e "s@^#define $1 [0-9]*@#define $1 $2@" $tmpconfig > $tmpconfig2
	mv -f $tmpconfig2 $tmpconfig
}

#----------------------------------------------------------------
# main part
#----------------------------------------------------------------

sources="awe_wave.c awe_hw.h awe_version.h README.awe ChangeLog.awe"
headers="awe_voice.h"

setup_config

read_value "Linux source root directory" /usr/src/linux linuxroot chk_dir
if [ ! -L $linuxroot/include/asm ]; then
	echo "making symlink $linuxroot/include/asm"
	(cd $linuxroot;make symlinks)
fi

if [ -f $linuxroot/include/asm/uaccess.h ]; then
	define_condition AWE_NEW_KERNEL_INTERFACE
else
	undef_condition AWE_NEW_KERNEL_INTERFACE
fi
if [ -f $linuxroot/drivers/sound/lowlevel/lowlevel.h ]; then
	define_condition HAS_LOWLEVEL_H
else
	undef_condition HAS_LOWLEVEL_H
fi

if [ x$1 != x ]; then
	sound_version=$1
else
	check_sound_version
fi

if [ x$sound_version = xOldUSS ]; then
	echo
	echo "Looks like you're using linux-2.0.0..."
	echo "Unfortunately, the sound driver in linux-2.0.0 is obsolete."
	echo "Please update to the newer one (at least linux-2.0.1)."
	echo
	exit 1
elif [ x$sound_version = xUSS ]; then
	echo "sound driver is OSS/Free-3.5.0 (aka USS/Lite)"
	destdir=$linuxroot/drivers/sound/lowlevel
	undef_condition AWE_OBSOLETE_VOXWARE
	undef_condition AWE_NO_PATCHMGR
	if [ ! -f $destdir/awe_wave.c ]; then
		echo "applying a patch to lowlevel directory"
		patch -d $linuxroot < usslite-3.54-awe.diff
	fi
	
elif [ x$sound_version = xOSS ]; then
	echo "sound driver is OSS/Free-3.707 or newer"
	destdir=$linuxroot/drivers/sound/lowlevel
	undef_condition AWE_OBSOLETE_VOXWARE
	define_condition AWE_NO_PATCHMGR

elif [ x$sound_version = xOSS38 ]; then
	echo "sound driver is OSS/Free-3.8b5 or newer"
	destdir=$linuxroot/drivers/sound/lowlevel
	undef_condition AWE_OBSOLETE_VOXWARE
	define_condition AWE_NO_PATCHMGR
	define_condition AWE_OSS38

elif [ x$sound_version = xOldOSS ]; then
	echo "sound driver is old OSS/Free"
	destdir=$linuxroot/drivers/sound/lowlevel
	undef_condition AWE_OBSOLETE_VOXWARE
	undef_condition AWE_NO_PATCHMGR

elif [ x$sound_version = xVOXWARE ]; then
	echo "sound driver is OSS/Free-3.0.x (aka Voxware)"
	define_condition AWE_OBSOLETE_VOXWARE
	undef_condition AWE_NO_PATCHMGR
	destdir=$linuxroot/drivers/sound
	if [ ! -f $destdir/awe_wave.c ]; then
		echo "applying a patch to sound driver directory"
		patch -d $destdir < voxware-3.01-awe.diff
	fi
else
	echo "Unknown sound driver version. quit."
	exit 1
fi


if [ x$2 != x ]; then
	maxsamples=$2
else
	read_value "Maximum number of samples (more than 600 recommended if for 2MB GM/GS presets)" 600 maxsamples nocheck
fi
define_value AWE_MAX_SAMPLES $maxsamples

if [ x$3 != x ]; then
	maxinfos=$3
else
	read_value "Maximum number of instruments (more than 3100 recommended for 2MB GM/GS presets)" 3200 maxinfos nocheck
fi
define_value AWE_MAX_INFOS $maxinfos

echo -n "copying source files:"
for i in $sources; do
	echo -n " $i"
	rm -f $destdir/$i
	cp $i $destdir
done
echo

echo -n "copying header files:"
headdir=$linuxroot/include/linux
if [ x$sound_version = xVOXWARE ]; then
	headrel=../../include/linux
else
	headrel=../../../include/linux
fi
for i in $headers; do
	echo -n " $i"
	rm -f $headdir/$i $destdir/$i
	cp $i $headdir
	ln -s $headrel/$i $destdir/$i
done
echo

echo "copying config file"
rm -f $destdir/awe_config.h
mv $tmpconfig $destdir/awe_config.h

echo "done."
echo

if [ x$sound_version = xVOXWARE ]; then
	echo "Please configure and remaks linux kernel."
	echo "Answer YES to 'AWE32 synth support' in sound driver configuration."
	echo "Also, you should keep other SoundBlaster options, too."
else
	echo "Please configure and remake linux kernel (and modules)."
	echo "Answer YES to 'AWE32 synth support' in Sound menu"
	echo "when configuring your kernel."
	echo "Also, you should keep other SoundBlaster options, too."
fi

exit 0
