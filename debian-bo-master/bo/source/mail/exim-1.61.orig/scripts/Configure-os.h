#! /bin/sh

# Shell script to create a link to the appropriate OS-specific header file.

scripts=../scripts

# Get the OS type, and check that there is a make file for it.

os=`$scripts/os-type` || exit 1

if	test ! -r ../OS/Makefile-$os
then    echo ""
	echo "*** Sorry - operating system $os is not supported"
        echo "*** See OS/Makefile-* for supported systems" 1>&2
        echo ""
	exit 1;
fi

# Ensure there is an OS-specific header file, and link it to os.h. There should
# always be one if there is a make file for the OS, so its absence is somewhat
# disastrous.

if	test ! -r ../OS/os.h-$os
then    echo ""
	echo "*** Build error: OS/os.h-$os file is missing"
        echo ""
	exit 1;
fi
rm -f os.h
ln -s ../OS/os.h-$os os.h || exit 1

# End of Configure-os.h
