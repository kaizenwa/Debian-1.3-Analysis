#!/bin/tcsh -f
#			Build all WWW Code for this platform
#
#	Figure out what sort of unix this is
#	(NeXT machines don't have uname!)

# When BUILDing a SOCKSified httpd:
# Set this to the location of your ready-built SOCKS library
# setenv SOCKSLIB /xxxx/yyyy/libsocks.a

## Use this setting to enable SOCKS:
# setenv SOCKS_FLAGS -DSOCKS
## ..OR this setting to enable both SOCKS _and_ client access control:
# setenv SOCKS_FLAGS "-DSOCKS -DCLIENT_CONTROL"
## Note that cern_httpd's normal access control can be used instead.

set MAKE=gmake

set UNAME=""
if (-e /usr/bin/uname)		set UNAME=`/usr/bin/uname`
if (-e /bin/uname)		set UNAME=`/bin/uname`
if (-e /usr/apollo/bin)		set UNAME=`ver sys5.3 /bin/uname`
if (-e /usr/bin/ncrm)		set UNAME=ncr
if (-e /usr/bin/swconfig)	set UNAME=sco
if ( $UNAME == "" ) then
    if (-r /NextApps ) then
        hostinfo | grep I386
        if  ( $status == 0 ) then
            set UNAME=next-386
        else
            set UNAME=next
        endif
    endif
endif
#
setenv UNAME $UNAME

# For apollo, must use bsd mode. Also, WWW_MACH not inherited through $MAKE!
if ($UNAME == next)		setenv WWW_MACH	next
if ($UNAME == next-386)		setenv WWW_MACH	next-386
if ($UNAME == FreeBSD)		setenv WWW_MACH	freebsd
if ($UNAME == ncr)		setenv WWW_MACH	ncr
if ($UNAME == sco) then
	setenv WWW_MACH	sco
	setenv PATH "${PATH}:/usr/local/bin"
endif
if ($UNAME == "HP-UX")		setenv WWW_MACH	snake
if ($UNAME == "IRIX")		setenv WWW_MACH	sgi
if ($UNAME == "SunOS") then
	set arch=`arch`
	set revision=`uname -r`
	if ($revision =~ 5.* ) then
		setenv WWW_MACH		${arch}-sol2
	else
		setenv	WWW_MACH	${arch}
	endif
endif
if ($UNAME == "ULTRIX")		setenv WWW_MACH	ultrix
if ($UNAME == "AIX")    	setenv WWW_MACH	aix
if ($UNAME == "OSF1")   	setenv WWW_MACH	osf1
if ($UNAME == "BSD/386")	setenv WWW_MACH	unix
if ($UNAME == "BSD/OS")		setenv WWW_MACH	unix
if ($UNAME == "Linux")		setenv WWW_MACH	linux
if ($UNAME == "NetBSD")		setenv WWW_MACH	netbsd
if ($UNAME == "A/UX")		setenv WWW_MACH	apple
if ($UNAME == "SMP_DC.SOx")	setenv WWW_MACH	pyramid
if ($UNAME == "uts") then
	# differentiate between UTS 2 (SVR3/BSD) and UTS 4 (SVR4)
	set version=`uname -v`
	if ($version =~ 2.*) then
		setenv WWW_MACH		uts2
	else
		setenv WWW_MACH		uts4
	endif
endif
if (-e /etc/conf/kconfig.d) then
	grep _POSIX_VERION /usr/include/sys/unistd.h >/dev/null
	if ($status) then
		setenv WWW_MACH		isc3.0		# Lauren
	endif
endif

#
# DELL Unix (How can I tell I'm running on DELL????)
#
# setenv WWW_MACH dell

#
# Unisys Unix (How can I tell I'm running on Unisys????)
#
# setenv WWW_MACH unisys

if ($?WWW_MACH == 0) then
	echo
	echo "Please edit BUILD file to include your machine OS"
	echo "and mail differences back to libwww@w3.org"
	echo
	echo "If you are BUILDing for:"
	echo "	- ISC 3.0"
	echo "	- DELL Unix SVR4"
	echo "	- Unisys Unix SVR4"
	echo "just uncomment the corresponding lines in the BUILD script."
	echo
	exit -99
endif
echo "________________________________________________________________"
echo "WWW build for machine type:                            " $WWW_MACH

#	Now go do build

#	We don't want SHELL set to something funny to screw up $MAKE

echo "Building Image Directory"
mkdir Image/jpeg/$WWW_MACH >& /dev/null
mkdir Image/libpng/$WWW_MACH >& /dev/null
mkdir Image/zlib/$WWW_MACH >& /dev/null
if (-e Image/jpeg/$WWW_MACH/libjpeg.a) then
        echo "Libjpeg done..."
else
	echo "Compiling libjpeg.a for $WWW_MACH"
        cd Image/jpeg/Implementation; ./configure ; $MAKE libjpeg.a ;
        cp libjpeg.a ../$WWW_MACH/; $MAKE clean; cd ../../..
        ranlib Image/jpeg/$WWW_MACH/libjpeg.a
endif

if (-e Image/zlib/$WWW_MACH/libz.a) then
        echo "ZLib done..."
else
	echo "Compiling libz.a for $WWW_MACH"
        cd Image/zlib/Implementation; $MAKE libz.a;
        cp libz.a ../$WWW_MACH/; $MAKE clean; cd ../../..
        ranlib Image/zlib/$WWW_MACH/libz.a
endif

if (-e Image/libpng/$WWW_MACH/libpng.a) then
        echo "Libpng done..."
else
	echo "Compiling libpng.a for $WWW_MACH"
        cd Image/libpng/Implementation; $MAKE libpng.a;
        cp libpng.a ../$WWW_MACH/; $MAKE clean; cd ../../..
        ranlib Image/libpng/$WWW_MACH/libpng.a
endif

(cd All/Implementation; unsetenv SHELL; $MAKE $1)
set stat = $status
echo
echo "WWW build for " $WWW_MACH  " done. status = " $stat
exit $stat
