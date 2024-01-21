#!/bin/sh
# 
#  Universal NFS Server 2.2beta4.1 configuration file
#  Aug 18, 1995
#  Copyright (C) 1995 Alexander O. Yuriev   <alex@bach.cis.temple.edu>  
#                                CIS Laboratories, TEMPLE UNIVERSITY.
#  GNU General Public License 2 or above terms apply.
#
#  Modified by Olaf Kirch.
#
#  Feb 1997 - added batch configuration via command line options.
#	--batch		don't ask questions
#	--ugidd=yes/no	enable/disable support for ugidd
#	--hosts-access=yes/no
#			When using ugidd, use hosts.allow/hosts.deny
#			to limit access to the uid mapper to certain
#			hosts.
#	--libwrap-directory=pathname
#			Specify the directory where libwrap.a can
#			be found, if not in the default lib path.
#	--rquotad=yes/no
#			Use/no not use the rpc.rquotad that comes
#			with this package (discouraged).
#	--exports-uid=uid
#	--exports-gid=gid
#			Define uid and gid of the exports file owner.
#			Whenever mountd accesses /etc/exports, it
#			will make sure the file owner hasn't changed.
#	--log-mounts=yes/no
#			Enable/disable logging of all mount requests.
#

batch=false

while [ $# -ne 0 ]; do
	case $1 in
	--batch)	batch=true;;
	--*)		what=`expr $1 : '--\(.*\)=' | tr -s - _`
			value=`expr $1 : '.*=\(.*\)'`
			echo $what=$value
			eval $what=$value;;
	*)		echo "Invalid option $1" >&2;;
	esac
	shift
done

function read_yesno {
	ans=""
	echo >&2
	override=$2
	while test -z $ans; do
		echo -n "$1 [y/n] " >&2
		if [ -n "$override" ]; then
			ans=$override
			echo "$override" >&2
		else
			read ans
		fi
		case $ans in
		y*|Y*)	ans=Y;;
		n*|N*)	ans=N;;
		*)	echo "You must answer y or n" >&2
			ans="";;
		esac
		override=
	done
	echo $ans
}

function read_ugid {
	ans=""
	override=$5
	while test -z $ans; do
		echo -n "$2 [default $3 $4] " >&2
		if [ -n "$override" ]; then
			ans=$override
			echo "$ans" >&2
		else
			read ans
		fi
		if [ -z "$ans" ]; then
			ans=$3
		elif expr "$ans" : "^[0-9]*$" >/dev/null; then
			break;
		else
			ans=`id $1 $ans 2>/dev/null`
		fi
	done
	echo $ans
}

cat << EOF

***********************************************************
*     Universal NFS Server 2.2beta5 Autoconfiguration     *
***********************************************************

   This package is BETA software. Until the final 2.2
   is released, please make sure you are using the latest
   version that you can get from
     
         ftp://linux.nrao.edu/pub/people/okir/nfsd/
	 ftp://ftp.mathematik.th-darmstadt.de/pub/linux/okir

   Please also make sure you replace any older versions
   of unfsd you are running.  Versions of the Universal
   NFS Server prior to 2.2 had some security holes.

   Caveat: although the Universal NFS Server 2.0 was
   originally written to support a set of different
   platfroms, including AIX, SunOS and others, versions
   above 2.0 were developed on Linux only. Thus, there
   may be some portability problems on other OSes.

   If you have a bug report, please follow the instructions
   in the file BUGS.

EOF
# -------------- blurb excerpt removed -----------------
#  This version is based on Olaf's version 2.2beta1
#  with automatic exports access control and hooks for
#  the experimental FoxbatSARS by Alex Yuriev.

if ! $batch; then
	echo -n "Please press return to continue"
	read
fi

cat << EOF

***************************************************************
*  Universal NFS Server 2.2beta5 Site Specific Configuration  *
***************************************************************

    Please answer the following questions to make the system
    specific changes in configuration of the UNFS Server.

EOF

cat << EOF
+---------------------+
| ugidd configuration |
+---------------------+

This release of unfsd supports dynamic mapping of uids and gids between
hosts with different uid spaces. However, you should not use this feature
unless you absolutely must, because it can pose a security risk: When run
unprotected, the ugidd mapping daemon can be abused to obtain a complete
list of all login names on your NFS client machine. As a counter-measure,
ugidd can be protected with the hosts_access control mechanism used by
Wietse Venema's tcp_wrapper package. Note that this does not offer a
hundred percent protection, though, as it can still be spoofed by hosts
on the same network as your client machine.

EOF

USE_UGIDD=`read_yesno "Are you going to use ugidd? (not recommended)" $ugidd`

if [ "$USE_UGIDD" = "Y" ]; then
  echo " What can I say, its your system. I will use ugidd to map uid/gids."

  USE_HSTACS=`read_yesno hosts_access \
			"Do you want to protect ugidd with HOST ACCESS?"`
  if [ "$USE_HSTACS" = "N" ]; then
    echo " You like danger, I won't use wrappers to control access to ugidd."
  else
    echo " Okay, at least I will use wrappers to control access to ugidd."
  fi
else
  echo " Good, I never liked ugidd."
fi

if [ "$USE_HSTACS" = "Y" ]; then
	LIBDIR=
	echo
	echo "Looking for libwrap.a... "
	for libdir in /usr/lib /usr/local/lib $LIBWRAP_DIR; do
		if [ -f $libdir/libwrap.a ]; then
			echo " Okay, libwrap is in $libdir"
			LIBDIR=$libdir;
			break;
		fi
	done
	if [ -z "$LIBDIR" ]; then
		cat << EOF

To protect ugidd with host_access, you must have libwrap.a installed
somewhere.  This library is part of the tcp_wrappers package. If you
don't have it, please obtain the source from 

	ftp:/win.tue.nl:/pub/security/tcp_wrapper_X.Y.tar.gz

and compile it.

EOF
		haveit=`read_yesno "Do you have libwrap.a installed?"`
		if [ "$haveit" != "Y" ]; then
			echo " Too bad. Aborting configuration."
			exit 1;
		fi

		while [ -z "$LIBDIR" ]; do
			echo -n "Which directory is it installed in? "
			read libdir
			if [ -f $libdir/libwrap.a ]; then
				LIBDIR=$libdir
			else
				echo " Can't find $libdir/libwrap.a"
			fi
		done
	fi
	HSTACS_LIBDIR=$LIBDIR
fi

cat << EOF
+-----------------------+
| rquotad configuration |
+-----------------------+

This release of unfsd supports disk quota management. The rquotad can be
called by the client to check what quota has been assigned to the user.
Even if you haven't configured your kernel to support disk quotas, you
may still wish to run rquotad, because some rquota implementations will
hang when there's no rquotad running on the NFS server.

If the configure script detects that your kernel doesn't have quota
support, it will simply tell the client that no quota has been installed
for that user and directory.

Note that this rquotad is fairly crude; there is a far better
implementation contained in the quota patches.

EOF

USE_RQUOTAD=`read_yesno "Do you want to build rquotad?" $rquotad`

cat << EOF

+------------------------------+
| Access control configuration |
+------------------------------+

Unfsd makes sure the exports file is always owned by the same user, and
is not writable by anyone but that user. I will now ask you for the uid
and gid of that user. Please enter appropriate user and group ids or names.

EOF

EXPSOWNUID=`read_ugid --user "Which uid should own /etc/exports?" 0 "(root)" $exports_uid`
EXPSOWNGID=`read_ugid --group "Which gid should own /etc/exports?" 0 "(root)" $exports_gid`

cat << EOF

UNFS Server 2.2beta5 and later can log mount requests and their
success/failure to syslogd. This can be very useful for systems
that are not protected from internet by firewalls. (Actually, it should
by very useful in all cases).

EOF

LOG_MOUNTS=`read_yesno "Do you want to log all mount reqests into syslog? (recommended) " $log_mounts`
if [ "$LOG_MOUNTS" = "Y" ]; then
  echo "  Good, I'd log all mount requests and their status into syslog"
else
  echo "  Well, don't say I did not offer..."
fi 

echo
echo "Creating custom configuration ..."
echo "The following is your system specific configuration: "
echo
echo -n " *** User/Group ID Map Daemon is" 
test $USE_UGIDD = "N" && echo -n " not"
echo -n " used"
if [ "$USE_UGIDD" = "Y" ] ; then
  test $USE_HSTACS = "Y" && echo -n " but" || echo -n " but NOT"
  echo " protected with host access control"
else
  echo
fi

echo -n " *** Disk quota deamon is"
if [ $USE_RQUOTAD = "Y" ]; then
  echo " used"
else
  echo " not used"
fi

echo " *** Exports Control files should be owned by UID=$EXPSOWNUID GID=$EXPSOWNGID"
echo -n " *** Mount requests will "
test $LOG_MOUNTS = "N" && echo -n "not "
echo "be logged to syslogd(8)"
# echo -n " *** Support for export of DOS filesystems "
# test $USE_DOSFS && echo "enabled." || echo "disabled."

echo 
echo "Updating site.h..."

(
echo "/*"
echo " * Site-specific configuration options generated by BUILD."
echo " * Please do not edit."
echo " */"
echo
echo "/*"
echo " * If REAL_UGIDD is defined, the real rpc.ugidd is created,"
echo " * otherwise, a dummy program is created"
echo " */"
if [ "$USE_UGIDD" = "Y" ]; then
  echo "#define REAL_UGIDD"
else
  echo "#undef REAL_UGIDD"
fi
echo
echo "/*"
echo " * if HOSTS_ACCESS is defined, ugidd uses host access control"
echo " * provided by libwrap.a from tcp_wrappers"
echo " */"

if [  "$USE_HSTACS" = "Y" ]; then
  echo "#define HOSTS_ACCESS"
else
  echo "/* #undef HOSTS_ACCESS */"
fi  
echo 
echo "/*"
echo " * Define correct ownership of export control file"
echo " */"
echo "#define EXPORTSOWNERUID  ((uid_t) $EXPSOWNUID)"
echo "#define EXPORTSOWNERGID  ((gid_t) $EXPSOWNGID)"
echo
echo "/*"
echo " * If WANT_LOG_MOUNTS is defined, every mount request will be logged"
echo " * to syslogd with the name of source site and a path that was"
echo " * it requested"
echo " */"
if [ "$LOG_MOUNTS" = "Y" ]; then
  echo "#define WANT_LOG_MOUNTS"
else
  echo "#undef WANT_LOG_MOUNTS"
fi

) > site.h 

echo "Updating site.mk ..."
(
echo "#"
echo "# Site-specific make options generated by BUILD. Please do not edit."
echo "#"
echo
echo "# ugidd support"
if [ $USE_UGIDD = "Y" ]; then
  echo "UGIDD_PROG=\${rpcprefix}ugidd"
  echo "UGIDD_MAN=ugidd"
else
  echo "UGIDD_PROG="
  echo "UGIDD_MAN="
fi
echo "# rquotad support"
if [ $USE_RQUOTAD = "Y" ]; then
  echo "RQUOTAD_PROG=\${rpcprefix}rquotad"
  echo "RQUOTAD_MAN="
else
  echo "RQUOTAD_PROG="
  echo "RQUOTAD_MAN="
fi
echo "# Location of tcp_wrapper library"
if [  "$USE_HSTACS" = "Y" ]; then
  echo "LIBWRAP_DIR=-L$HSTACS_LIBDIR"
  echo "LIBWRAP_LIB=-lwrap"
else
  echo "LIBWRAP_DIR="
  echo "LIBWRAP_LIB="
fi
) > site.mk

cat << EOF

I'm now running GNU configure to determine some system-specific things.
This make take a while on your first attempt.

EOF

if ! $batch; then
	echo -n "Please press return to continue "; read foo
fi

sh configure
if [ $? -ne 0 ]; then
  echo
  echo
  echo " *** Warning: GNU configure exited with error code $?"
  echo " *** Aborting installation. Please check the output of"
  echo " *** configure."
  echo
  exit 2
fi

cat << "EOF"

Uphh... Done. Now you can run `make install' to build and install the
binaries and manpages.

EOF
exit 0
