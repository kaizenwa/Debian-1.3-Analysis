dnl Af-specific additions to Autoconf macros.
dnl Copyright (C) 1994, 1995, 1996 Malc Arnold.
dnl
dnl $Id: aclocal.m4,v 1.20 1997/03/05 21:13:02 malc Exp $
dnl
dnl Checks for system features
dnl
AC_DEFUN(AF_SYS_JOBCONTROL,
[AC_MSG_CHECKING(for job control)
AC_CACHE_VAL(ac_cv_sys_jobcontrol,
[AC_EGREP_CPP([Have jobcontrol],
[#include <signal.h>
#ifdef SIGTSTP
Have jobcontrol
#endif
], ac_cv_sys_jobcontrol=yes, ac_cv_sys_jobcontrol=no)])
AC_MSG_RESULT($ac_cv_sys_jobcontrol)
test $ac_cv_sys_jobcontrol = yes && AC_DEFINE(HAVE_JOBCONTROL)
])

AC_DEFUN(AF_SYS_RESIZING,
[AC_MSG_CHECKING(for window resizing)
AC_CACHE_VAL(ac_cv_sys_resizing,
[AC_EGREP_CPP([Have resizing],
[#include <signal.h>
#ifdef SIGWINCH
Have resizing
#endif
], ac_cv_sys_resizing=yes, ac_cv_sys_resizing=no)])
AC_MSG_RESULT($ac_cv_sys_resizing)
test $ac_cv_sys_resizing = yes && AC_DEFINE(HAVE_RESIZING)
])

AC_DEFUN(AF_SYS_NETWORKING,
[AF_MSG_ECHO([Checking for BSD networking...])
AC_CHECK_LIB(socket, socket, [LIBS="$LIBS -lsocket"])
AC_CHECK_LIB(nsl, gethostbyname, [LIBS="$LIBS -lnsl"])
AC_CHECK_FUNC(socket,
    AC_CHECK_FUNC(connect,
	AC_CHECK_FUNC(gethostbyname,
	    AC_CHECK_FUNC(getservbyname, ac_cv_sys_networking="yes"))),
    ac_cv_sys_networking = "no")
])

dnl Checks for header files

AC_DEFUN(AF_TM_GMTOFF,
[AC_REQUIRE([AC_STRUCT_TM])dnl
AC_MSG_CHECKING(if struct tm includes tm_gmtoff)
AC_CACHE_VAL(ac_cv_tm_gmtoff,
[AC_TRY_COMPILE([#include <sys/types.h>
#ifdef TM_IN_SYS_TIME
#include <sys/time.h>
#else
#include <time.h>
#endif
], [struct tm *tp; tp->tm_gmtoff;
], ac_cv_tm_gmtoff=yes, ac_cv_tm_gmtoff=no)])
AC_MSG_RESULT($ac_cv_tm_gmtoff)
test $ac_cv_tm_gmtoff = yes && AC_DEFINE(HAVE_TM_GMTOFF)
])

AC_DEFUN(AF_HEADER_TERMIO,
[AC_MSG_CHECKING(for termio.h)
AC_CACHE_VAL(ac_cv_header_termio,
[AC_EGREP_CPP([Have termio],
[#include <termio.h>
#ifdef TCGETA
#ifdef TCSETAW
Have termio
#endif
#endif
], ac_cv_header_termio=yes, ac_cv_header_termio=no)])
AC_MSG_RESULT($ac_cv_header_termio)
test $ac_cv_header_termio = yes && AC_DEFINE(HAVE_TERMIO)
AC_MSG_CHECKING(for termios.h)
AC_CACHE_VAL(ac_cv_header_termios,
[AC_EGREP_CPP([Have termios],
[#include <termios.h>
#ifdef TCGETA
#ifdef TCSETAW
Have termios
#endif
#endif
], ac_cv_header_termios=yes, ac_cv_header_termios=no)])
AC_MSG_RESULT($ac_cv_header_termios)
test $ac_cv_header_termios = yes && AC_DEFINE(HAVE_TERMIOS)
])

dnl Checks for library functions

AC_DEFUN(AF_FUNC_PGRP_ARGS,
[AC_MSG_CHECKING(if setpgrp/getpgrp need arguments)
AC_CACHE_VAL(ac_cv_func_pgrp_args,
[AC_TRY_RUN([#include <stdio.h>
/* Exit 1 if setpgrp needs arguments */
main() {
	/* Try for a failure if we need args */
	return((setpgrp(1, 0) != -1) ? 0 : 1);
}], ac_cv_func_pgrp_args=no, ac_cv_func_pgrp_args=yes,
ac_cv_func_pgrp_args=yes)])
AC_MSG_RESULT($ac_cv_func_pgrp_args)
test $ac_cv_func_pgrp_args = yes && AC_DEFINE(PGRP_NEEDS_ARGS)
])

AC_DEFUN(AF_FUNC_OSPEED_EXTERN,
[AC_MSG_CHECKING(if ospeed is defined)
AC_CACHE_VAL(ac_cv_func_ospeed_extern,
[AC_TRY_LINK(, [
/* Try setting an external ospeed */
extern short ospeed;
ospeed = 5;
], ac_cv_func_ospeed_extern=yes, ac_cv_func_ospeed_extern=no)])
AC_MSG_RESULT($ac_cv_func_ospeed_extern)
test $ac_cv_func_ospeed_extern = yes && AC_DEFINE(OSPEED_EXTERN)
])

dnl  This is a bit of a hack really.  It checks whether we'll need
dnl  psignal.o in the af library, and if so it also sets up the
dnl  definition it requires.  This is because DEFS seems a bit 
dnl  excessive when you only need one of the values.

AC_DEFUN(AF_SET_LIBDEFS,
[LIBDEFS=""
if test "$ac_cv_have_sigprocmask" = "no"; then
  LIBOBJS="$LIBOBJS psignal.o"
  LIBDEFS="-DRETSIGTYPE=$ac_cv_type_signal"
  test "$ac_cv_have_sigsetmask" = "yes" &&
	LIBDEFS="$LIBDEFS -DHAVE_SIGSETMASK"
fi
test "$ac_cv_func_pgrp_args" = yes &&
	LIBDEFS="$LIBDEFS -DPGRP_FUNCS_NEED_ARGS"
test "$ac_cv_header_string_h" = yes &&
	LIBDEFS="$LIBDEFS -DHAVE_STRING_H"
AC_SUBST(LIBDEFS)
])

dnl  How to print messages for the user

AC_DEFUN(AF_MSG_ECHO, [echo ["$1"]])
AC_DEFUN(AF_MSG_ECHO_N, [echo $ac_n ["$1"]$ac_c])

dnl  The following macros are far less general, and are intended
dnl  purely for use with af.  They require user interaction,
dnl  partly because there's no sensible way to determine some
dnl  of this information, and partly because that's the way we
dnl  prefer it.
dnl
dnl  Best thing to do here, is to include these modules at the
dnl  start of the configure.in file, so that the questions get
dnl  asked first, and then call AF_COFFEE which reports the end
dnl  of the questions.

AC_DEFUN(AF_MASTERMIND,
[cat <<ENDOFMESSAGE

To configure af sensibly, there are several options that can only be set
by asking the installer (that's you) what they want.  These questions will
be asked now, to get them out of the way.  In the following questions, any
default will be shown in square brackets, simply pressing return will take
the default.

ENDOFMESSAGE
])
AC_DEFUN(AF_CC_OPTIONS,
[dflt="$CC"
AF_MSG_ECHO_N([Which C compiler should be used? [$dflt] ])
read CC
test -z "$CC" && CC="$dflt"
dflt="$CFLAGS"
AF_MSG_ECHO_N([And what compile options should be passed to it? [$dflt] ])
read CFLAGS
test -z "$CFLAGS" && CFLAGS="$dflt"
AC_SUBST(CFLAGS)
AF_MSG_ECHO_N([And what options (not libraries) for linking? [] ])
read LDFLAGS
AC_SUBST(LDFLAGS)
dflt=""
AF_MSG_ECHO([ ])
])

AC_DEFUN(AF_HOSTNAME,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Checking how to get hostname...])
name=""
AC_CHECK_FUNC(gethostname, [name=gethostname])
if test -z "$name"; then
  AC_CHECK_FUNC(uname, [name=uname])
fi
case "$name" in
  gethostname)	AC_DEFINE(HAVE_GETHOSTNAME)
		test "$ac_cv_sys_networking" = "yes" &&
			AC_DEFINE(HAVE_GETHOSTBYNAME) ;;
  uname)	AC_DEFINE(HAVE_UNAME)
		test "$ac_cv_sys_networking" = "yes" &&
			AC_DEFINE(HAVE_GETHOSTBYNAME) ;;
  *)		AF_MSG_ECHO([ ])
		AF_MSG_ECHO([No function to get hostname available.])
		AF_MSG_ECHO_N([What is the full mail domain of this machine? ])
		read ac_cv_sys_domain
		AC_DEFINE_UNQUOTED(HOSTNAME, "$ac_cv_sys_domain") ;;
esac
name=""
])

AC_DEFUN(AF_SVR4_PASSWD,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Does your password file have gecos fields in SVr4 form, with])
AF_MSG_ECHO_N([the name between the first '-' and the first '('? [n] ])
read yesno
case "$yesno" in
y*) AC_DEFINE(SVR4_PASSWD) ;;
*)  ;;
esac
])

AC_DEFUN(AF_WHERE,
[AF_MSG_ECHO([ ])
AC_PREFIX_PROGRAM(af)
test "$prefix" = "NONE" && prefix="$ac_default_prefix"
dflt="$prefix/bin"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([Where do you want to put the executable files? [$dflt] ])
read BINDIR
test -z "$BINDIR" && BINDIR="$dflt"
AC_SUBST(BINDIR)
dflt="$prefix/lib/af"
AF_MSG_ECHO_N([Where do you want to put the reference files? [$dflt] ])
read LIBDIR
test -z "$LIBDIR" && LIBDIR="$dflt"
AC_SUBST(LIBDIR)
dflt="$LIBDIR"
AF_MSG_ECHO_N([Where do you want to put the configuration files? [$dflt] ])
read ETCDIR
test -z "$ETCDIR" && ETCDIR="$dflt"
AC_SUBST(ETCDIR)
AC_DEFINE_UNQUOTED(AFLIBDIR, "$LIBDIR")
AC_DEFINE_UNQUOTED(HELPFILE, "$LIBDIR/af.help")
AC_DEFINE_UNQUOTED(READMBOX, "$LIBDIR/readmbox")
AC_DEFINE_UNQUOTED(WRITEMBOX, "$LIBDIR/writembox")
AC_DEFINE_UNQUOTED(SYS_STARTUPFILE, "$ETCDIR/afrc")
AC_DEFINE_UNQUOTED(SYS_ALIASFILE, "$ETCDIR/afalias")
dflt="$prefix/man/man1"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([How about the manual pages for programs? [$dflt] ])
read MAN1DIR
test -z "$MAN1DIR" && MAN1DIR="$dflt"
AC_SUBST(MAN1DIR)
dflt="1"
AF_MSG_ECHO_N([With what suffix (eg. 1, 1l, 1, C)? [$dflt] ])
read MAN1EXT
test -z "$MAN1EXT" && MAN1EXT="$dflt"
changequote(<<,>>)
MAN1SEC=`echo $ac_n "$MAN1EXT"$ac_c | tr '[a-z]' '[A-Z]'`
changequote([,])
AC_SUBST(MAN1EXT)
AC_SUBST(MAN1SEC)
dflt="$prefix/man/man5"
test -f /usr/man/man4*/passwd.4* && dflt="$prefix/man/man4"
AF_MSG_ECHO_N([Where do manual pages for file formats go? [$dflt] ])
read MAN5DIR
test -z "$MAN5DIR" && MAN5DIR="$dflt"
AC_SUBST(MAN5DIR)
dflt="5"
test -f /usr/man/man4*/passwd.4* && dflt="4"
AF_MSG_ECHO_N([With what suffix? [$dflt] ])
read MAN5EXT
test -z "$MAN5EXT" && MAN5EXT="$dflt"
changequote(<<,>>)
MAN5SEC=`echo $ac_n "$MAN5EXT"$ac_c | tr '[a-z]' '[A-Z]'`
changequote([,])
AC_SUBST(MAN5EXT)
AC_SUBST(MAN5SEC)
AF_MSG_ECHO_N([Do manual pages need to be compressed? [n] ])
read yesno
case "$yesno" in
y)  AF_MSG_ECHO([ ])
    AF_MSG_ECHO([Checking which compression programs are available...])
    AC_CHECK_PROG(ac_cv_prog_compress, compress, [compress])
    dflt="$ac_cv_prog_compress"
    AC_CHECK_PROG(ac_cv_prog_gzip, gzip, [gzip])
    test -z "$dflt" && dflt="$ac_cv_prog_gzip"
    AC_CHECK_PROG(ac_cv_prog_pack, pack, [pack])
    test -z "$dflt" && dflt="$ac_cv_prog_pack"
    AF_MSG_ECHO([ ])
    AF_MSG_ECHO_N([Compress with which program? [$dflt] ])
    read mancompress
    test -z "$mancompress" && mancompress="$dflt"
    MANCOMPRESS=$mancompress
    case "$mancompress" in
    compress*)	dflt=".Z" ;;
    gzip*)	dflt=".gz" ;;
    pack*)	dflt=".z" ;;
    *)		dflt="" ;;
    esac
    test "`echo /usr/man/man1/*$dflt`" = "/usr/man/man1/*$dflt" && dflt=""
    AF_MSG_ECHO_N([With what compression suffix (\"none\" for none)? [$dflt] ])
    read mansuffix
    test -z "$mansuffix" && mansuffix="$dflt"
    test "$mansuffix" = "none" && mansuffix=""
    MAN1EXT="$MAN1EXT$mansuffix"
    MAN5EXT="$MAN5EXT$mansuffix" ;;
*)  MANCOMPRESS="cat" ;;
esac
AC_SUBST(MANCOMPRESS)
dflt="$prefix/info"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([How about info manuals? [$dflt] ])
read INFODIR
test -z "$INFODIR" && INFODIR="$dflt"
AC_SUBST(INFODIR)
AF_MSG_ECHO_N([Do info manuals need to be compressed? [n] ])
read yesno
case "$yesno" in
y)  AF_MSG_ECHO([ ])
    AF_MSG_ECHO([Checking which compression programs are available...])
    AC_CHECK_PROG(ac_cv_prog_gzip, gzip, [gzip])
    dflt="$ac_cv_prog_gzip"
    AC_CHECK_PROG(ac_cv_prog_compress, compress, [compress])
    test -z "$dflt" && dflt="$ac_cv_prog_compress"
    AC_CHECK_PROG(ac_cv_prog_pack, pack, [pack])
    test -z "$dflt" && dflt="$ac_cv_prog_pack"
    AF_MSG_ECHO([ ])
    AF_MSG_ECHO_N([Compress with which program? [$dflt] ])
    read infocompress
    test -z "$infocompress" && infocompress="gzip"
    INFOCOMPRESS=$infocompress ;;
*)  INFOCOMPRESS="" ;;
esac
AC_SUBST(INFOCOMPRESS)
])

AC_DEFUN(AF_PIPES,
[cat <<ENDOFMESSAGE

Programs that access mail files often need to run set user id or set group
id so that they can create lock files in the spool directory, to prevent
other programs from updating the mailbox while af is doing so.

Af can be configured to use sub-programs to read and write folders, with the
sub-programs running with any required ids, or af itself can run with such
permissions, provided that no special user or group id is required, or that
your system has POSIX id controls; ie the ability to switch between the real
and effective ids as required.  If you need to run setuid root, then you
MUST use subprograms to read and write files.  Sub-programs have a slight
performance advantage on multiprocessor machines, while making af itself
read the files has performance advantages on other machines.

If in doubt take the default - it should work on any system.

ENDOFMESSAGE
TARGETS="af afack Rnaf Afinfo"
LOCK_C=""
LOCK_O=""
AF_MSG_ECHO_N([Use subprograms to read and write files? [y] ])
read yesno
case "$yesno" in
n*)	LOCK_C="lock.c" ; LOCK_O="lock.o" ;;
*)	TARGETS="$TARGETS readmbox writembox"
	AC_DEFINE(READ_VIA_PIPES) ;;
esac
dflt=""
AC_SUBST(TARGETS)
AC_SUBST(LOCK_C)
AC_SUBST(LOCK_O)
])

AC_DEFUN(AF_INSTALL,
[test "$INSTALL" = "cp" && INSTALL="../install.sh"
cat <<ENDOFMESSAGE

As just mentioned, special permissions may be required to create lock files
in the spool directory.  If you need (and are able) to make af (or the
subprograms if you chose to use them) run with the required user and/or
group ids, and to make them run setuid or setgid, then you should type in
the name of the appropriate user and group as prompted.  Otherwise you
should just press return, and the executables will be installed with
ownership as for any file you create.

ENDOFMESSAGE
AF_MSG_ECHO_N([Which user id should the installed files have? ])
read uid
test -n "$uid" && uid="-o $uid"
AF_MSG_ECHO_N([Which group id should the installed files have? ])
read gid
test -n "$gid" && gid="-g $gid"
cat <<ENDOFMESSAGE

If you need af to run set user id or set group id to create lock files,
then you should give that requirement here.  A response of 'suid' means
that af (or the sub-programs) should run set user id, 'sgid' means run
set group id, and 'both' means both set user id and set group id.  If
you don't intend to use either set user id or set group id then just
type return.

ENDOFMESSAGE
AF_MSG_ECHO_N([Should the programs have 'suid', 'sgid' or 'both' set? ])
read sid
case "$sid" in
suid) sid="4" ;;
sgid) sid="2" ;;
both) sid="6" ;;
*)    sid="" ;;
esac
# set the defaults
if test -z "$LOCK_C"; then
  IAFOPTS="-m 755 $uid $gid -s"
  IMBXOPTS="-m ${sid}755 $uid $gid -s"
else
  IAFOPTS="-m ${sid}755 $uid $gid -s"
  IMBXOPTS="-m 755 $uid $gid -s"
fi
IBINOPTS="-m 755 $uid $gid -s"
ISCROPTS="-m 755 $uid $gid"
IDIROPTS="-d -m 755 $uid $gid"
IDOCOPTS="-m 644 $uid $gid"
AC_SUBST(IAFOPTS)
AC_SUBST(IBINOPTS)
AC_SUBST(ISCROPTS)
AC_SUBST(IMBXOPTS)
AC_SUBST(IDIROPTS)
AC_SUBST(IDOCOPTS)
])

AC_DEFUN(AF_MTA,
[cat <<ENDOFMESSAGE

Now we need to configure af for the Mail Transfer Agent (MTA) which you
are using.  This is the program called by af to deliver mail to the
correct addresses, whether locally or over a network.  The MTA is usually
one of submit, qmail-inject, sendmail, smail, or delivermail, (in roughly
descending order of likelihood if they are present), or possibly direct
SMTP via some remote host (if your system doesn't have a local main system).
As a last resort, /bin/rmail or /bin/mail may be used.  Note that even if
one of the above MTAs is present it may not be in use;  if you aren't sure
which MTA your system is using, then check before you answer.

The full path to the MTA is needed, not just the name of the program
(eg /usr/lib/sendmail, not just sendmail).  If you are not using a local
MTA, but want af to connect to a remote machine to send mail, enter SMTP
(in upper case) as your MTA.

First we'll check for certain likely MTAs...
ENDOFMESSAGE

#Initialise a few local things
NETMAIL_C=""
NETMAIL_O=""

# Now set up a path and look for MTAs
savepath="$PATH"
PATH="$PATH:/sbin:/usr/sbin:/usr/lib:/usr/local/lib:/etc:/usr/etc:\
/usr/local/etc:/usr/mmdf/bin:/usr/qmail/bin:/var/qmail/bin:\
/usr/local/qmail/bin:/var/local/qmail/bin"
AC_PATH_PROG(ac_cv_path_submit, submit)
test -z "$dflt" && dflt="$ac_cv_path_submit"
AC_PATH_PROG(ac_cv_path_qmail_inject, qmail-inject)
test -z "$dflt" && dflt="$ac_cv_path_delivermail"
AC_PATH_PROG(ac_cv_path_sendmail, sendmail)
test -z "$dflt" && dflt="$ac_cv_path_sendmail"
AC_PATH_PROG(ac_cv_path_smail, smail)
test -z "$dflt" && dflt="$ac_cv_path_smail"
AC_PATH_PROG(ac_cv_path_delivermail, delivermail)
test -z "$dflt" && dflt="$ac_cv_path_delivermail"
# We prefer SMTP to any of the others
test -z "$dflt" && test "$ac_cv_sys_networking" = "yes" && dflt="SMTP"
AC_PATH_PROG(ac_cv_path_rmail, rmail)
test -z "$dflt" && dflt="$ac_cv_path_rmail"
AC_PATH_PROG(ac_cv_path_mail, mail)
test -z "$dflt" && dflt="$ac_cv_path_mail"
PATH="$savepath"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([What is the full path to the MTA program? [$dflt] ])
read MTA
test -z "$MTA" && MTA="$dflt"
dflt=""
case "$MTA" in
*/submit)		MTA="$MTA -mrxto,cc*"
			MMDF="y"
			AC_DEFINE_UNQUOTED(MTA, "$MTA")
			AC_DEFINE(NO_MTA_DATE)
			AC_DEFINE(NO_MTA_GROUPS) ;;

*/qmail-inject)		MTA="$MTA -h"
			MMDF="n"
			AC_DEFINE_UNQUOTED(MTA, "$MTA")
			AC_DEFINE(NO_MTA_DATE) ;;

*/sendmail)		MTA="$MTA -oi -t"
			MMDF="n"
			AC_DEFINE_UNQUOTED(MTA, "$MTA")
			AC_DEFINE(NO_MTA_DATE)
			AC_DEFINE(NO_MTA_GROUPS) ;;

*/smail)	 	AF_MSG_ECHO_N([Is this smail version 3? [y] ])
			read yesno
			case "$yesno" in
			n*)	MMDF="n"
				AC_DEFINE_UNQUOTED(MTA, "$MTA")
				AC_DEFINE(MTA_NEEDS_ARGS)
				AC_DEFINE(NO_MTA_CC)
				AC_DEFINE(NO_MTA_DATE)
				AC_DEFINE(NO_MTA_RESENT)
				AC_DEFINE(NO_MTA_GROUPS) ;;
			*)	MTA="$MTA -oi -t"
				MMDF="n"
				AC_DEFINE_UNQUOTED(MTA, "$MTA")
				AC_DEFINE(NO_MTA_DATE)
				AC_DEFINE(NO_MTA_GROUPS) ;;
			esac ;;

*/delivermail)		MTA="$MTA -i -em"
			MMDF=""
			AC_DEFINE_UNQUOTED(MTA, "$MTA")
			AC_DEFINE(MTA_NEEDS_ARGS)
			AC_DEFINE(NO_MTA_CC)
			AC_DEFINE(NO_MTA_DATE)
			AC_DEFINE(NO_MTA_ID)
			AC_DEFINE(NO_MTA_GROUPS) ;;

*/rmail | */mail)	MMDF="n"
			AC_DEFINE_UNQUOTED(MTA, "$MTA")
			AC_DEFINE(MTA_NEEDS_ARGS)
			AC_DEFINE(MTA_NEEDS_UUCP)
			AC_DEFINE(NO_MTA_CC)
			AC_DEFINE(NO_MTA_DATE)
			AC_DEFINE(NO_MTA_ID)
			AC_DEFINE(NO_MTA_GROUPS) ;;

SMTP)			MMDF="n"
			AC_DEFINE(MTA_IS_SMTP)
			AC_DEFINE(MTA_NEEDS_ARGS)
			AC_DEFINE(NO_MTA_DATE)
			AC_DEFINE(NO_MTA_GROUPS)
			AF_MSG_ECHO_N([What is the default SMTP server? ])
			read SMTPSERVER
			AC_DEFINE_UNQUOTED(DEFAULT_SMTP_HOST, "$SMTPSERVER")
			NETMAIL_C="netmail.c"
			NETMAIL_O="netmail.o" ;;

*)			AF_ASK_MTA
			AF_MSG_ECHO([ ]) ;;
esac
AF_MSG_ECHO_N([Does your system store folders in MMDF format? [$MMDF] ])
read yesno
case "$yesno" in
y*)	AC_DEFINE(MTA_MMDF_FORMAT) ;;
esac

AF_MSG_ECHO_N([Does your system honour the Content-Length: header? [n] ])
read yesno
case "$yesno" in
y*)	AC_DEFINE(MTA_CONTENT_LENGTH) ;;
*)	AF_MSG_ECHO_N([Does your system insist that blank lines separate messages? [n] ])
	read yesno
	case "$yesno" in
	y*)	AC_DEFINE(MTA_BLANK_SEPARATED) ;;
	esac
esac

POP3SERVER=""
if test "$ac_cv_sys_networking" = "yes"; then
  AF_MSG_ECHO_N([Do you want to support remote POP3 mailboxes? [n] ])
  read yesno
  case "$yesno" in
  y*)	AC_DEFINE(READ_VIA_POP3)
	NETMAIL_C="netmail.c"
	NETMAIL_O="netmail.o"
	AF_MSG_ECHO_N([Do users' incoming mailboxes live on a POP3 server? [n] ])
	read yesno
	case "$yesno" in
	y*)	AF_MSG_ECHO_N([What is the full name of the server? ])
		read POP3SERVER
		AC_DEFINE_UNQUOTED(DEFAULT_POP3_HOST, "$POP3SERVER") ;;
	esac
  esac
fi

MAILFILE=""
if test -z "$POP3SERVER"; then
  AF_MSG_ECHO_N([Do users' mailboxes live in their home directories? [n] ])
  read yesno
  case "$yesno" in
  y*)	AF_MSG_ECHO_N([What is the default file for incoming mail called? [mailbox] ])
	read MAILFILE
	test -z "$MAILFILE" && MAILFILE="mailbox"
	AC_DEFINE_UNQUOTED(DEFAULT_MAILFILE, "$MAILFILE") ;;
  esac
fi

if test -z "$POP3SERVER" && test -z "$MAILFILE"; then
  dflt="/usr/mail"
  test -d "/var/mail" && dflt="/var/mail"
  test -d "/usr/spool/mail" && dflt="/usr/spool/mail"
  test -d "/var/spool/mail" && dflt="/var/spool/mail"
  AF_MSG_ECHO_N([Where do users' incoming mailboxes live? [$dflt] ])
  read MAILDIR
  test -z "$MAILDIR" && MAILDIR="$dflt"
  AC_DEFINE_UNQUOTED(MAILDIR, "$MAILDIR")
fi

if test -n "$NETMAIL_C"; then
  AF_MSG_ECHO_N([What is the timeout for network connections (in seconds)? [120] ])
  read NETTIMEOUT
  test -z "$NETTIMEOUT" && NETTIMEOUT="120"
  AC_DEFINE_UNQUOTED(NET_TIMEOUT, $NETTIMEOUT)
fi

# We need to substitute NETMAIL_[CO]
AC_SUBST(NETMAIL_C)
AC_SUBST(NETMAIL_O)

dflt=""
])

AC_DEFUN(AF_ASK_MTA,
[cat <<ENDOFMESSAGE
$MTA is not a known MTA, so you'll have to set it up by hand.
If you mail the details of the MTA, and the answers to the questions
you're about to get asked to me (af-bug@csv.warwick.ac.uk) then I'll
add the details, so other people (like you later) can get it
automagically configured.

ENDOFMESSAGE
AF_MSG_ECHO_N([What command-line arguments does $MTA require? ])
read ARGS
MTA="$MTA $ARGS"
AC_DEFINE_UNQUOTED(MTA, "$MTA")
AF_MSG_ECHO([Is it MMDF-like (messages separated by 'control A's)? [n] ])
read yesno
case "$yesno" in
y*) AC_DEFINE(MTA_IS_MMDF)
    MMDF="yes" ;;
*)  MMDF="" ;;
esac
AF_MSG_ECHO_N([Does it need the destination addresses on the command line [n] ])
read yesno
case "$yesno" in
y*) AC_DEFINE(MTA_NEEDS_ARGS)
    AF_MSG_ECHO_N([And can it handle non-UUCP addresses? [y] ])
    read yesno
    case "$yesno" in
    n*) AC_DEFINE(MTA_NEEDS_UUCP) ;;
    esac
    AF_MSG_ECHO_N([Does it handle 'Cc:', sending mail to the addresses? [y] ])
    read yesno
    case "$yesno" in
    n*) AC_DEFINE(NO_MTA_CC) ;;
    esac ;;
esac
AF_MSG_ECHO_N([Does the MTA add Date: headers to outgoing mail? [n] ])
read yesno
case "$yesno" in
y*) : ;; *) AC_DEFINE(NO_MTA_DATE) ;;
esac
AF_MSG_ECHO_N([Does the MTA add Message-Id: headers to outgoing mail? [y] ])
read yesno
case "$yesno" in
n*) AC_DEFINE(NO_MTA_ID) ;;
esac
AF_MSG_ECHO_N([Do Resent- headers take priority over normal ones? [y] ])
read yesno
case "$yesno" in
n*) AC_DEFINE(NO_MTA_RESENT) ;;
esac
AF_MSG_ECHO_N([Are groups like 'Af :malc@thing, kay@thing;' ok? [n] ])
read yesno
case "$yesno" in
y*) : ;; *) AC_DEFINE(NO_MTA_GROUPS) ;;
esac
dflt=""
])

AC_DEFUN(AF_MAXLOCK,
[cat <<ENDOFMESSAGE

Mailboxes in the spool directory can be locked during mail delivery (to
avoid simultaneous updates).  Af will not open a locked file but will
keep trying until either the file becomes unlocked or a specified time
(in seconds) has passed, at which point it returns an error.

ENDOFMESSAGE
dflt="15"
AF_MSG_ECHO_N([How many seconds should af wait for a file to be unlocked? [$dflt] ])
read MAXLOCK
test -z "$MAXLOCK" && MAXLOCK="$dflt"
AC_DEFINE_UNQUOTED(MAXLTRY, $MAXLOCK)
dflt=""
])

AC_DEFUN(AF_LOCKFUNC,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Checking how to lock mailboxes...])
name=""
AC_CHECK_FUNC(lockf, [name=lockf])
if test -z "$name"; then
  AC_CHECK_FUNC(flock, [name=flock])
fi
if test -n "$name"; then
  cat <<ENDOFMESSAGE

Af will always create lock files to lock mailboxes in traditional
unix fashion.  You have $name to set advisory locks on files, which
af can use as added protection against losing mail.  Normally this
is useful, but on some systems, especially if you are using NFS,
$name will not work properly, which may cause problems.
ENDOFMESSAGE
  AF_MSG_ECHO_N([Do you want to use $name to lock files? [y] ])
  read yesno
  case "$yesno" in
    n*)  name="" ;;
  esac
fi
case "$name" in
  lockf)	AC_DEFINE(HAVE_LOCKF) ;;
  flock)	AC_DEFINE(HAVE_FLOCK) ;;
esac
name=""
])

AC_DEFUN(AF_SIGFILE,
[cat <<ENDOFMESSAGE

Af allows the truncation of files which are to be automatically included
in outgoing mail ("signature files") to a maximum number of lines and/or
columns.  This prevents users having ridiculously large signature files
which tend to annoy the net community and waste network resources.  If
you don't want a limit to be enforced then answer 0 to each of the
following questions, otherwise enter the maximum size.

ENDOFMESSAGE
dflt=4
AF_MSG_ECHO_N([Maximum number of lines in signature files? [$dflt] ])
read SIGLINES
test -z "$SIGLINES" && SIGLINES="$dflt"
dflt="79"
AF_MSG_ECHO_N([Maximum number of columns in signature files? [$dflt] ])
read SIGCOLS
test -z "$SIGCOLS" && SIGCOLS="$dflt"
AC_DEFINE_UNQUOTED(SIGLINES, $SIGLINES)
AC_DEFINE_UNQUOTED(SIGCOLS, $SIGCOLS)
dflt=""
])

AC_DEFUN(AF_DEFEDITOR,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Checking which editors are available...])
AC_CHECK_PROG(ac_cv_prog_emacs, emacs, [emacs])
dflt="$ac_cv_prog_emacs"
AC_CHECK_PROG(ac_cv_prog_jove, jove, [jove])
test -z "$dflt" && dflt="$ac_cv_prog_jove"
AC_CHECK_PROG(ac_cv_prog_mg, mg, [mg])
test -z "$dflt" && dflt="$ac_cv_prog_mg"
AC_CHECK_PROG(ac_cv_prog_vi, vi, [vi])
test -z "$dflt" && dflt="$ac_cv_prog_vi"
AC_CHECK_PROG(ac_cv_prog_ed, ed, [ed])
test -z "$dflt" && dflt="$ac_cv_prog_ed"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([What is the default editor? [$dflt] ])
read EDITOR
test -z "$EDITOR" && EDITOR="$dflt"
AC_DEFINE_UNQUOTED(DEFEDITOR, "$EDITOR")
dflt=""
])

AC_DEFUN(AF_DEFPAGER,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Checking which pagers are available...])
AC_CHECK_PROG(ac_cv_prog_more, more, [more])
dflt="$ac_cv_prog_more"
AC_CHECK_PROG(ac_cv_prog_less, less, [less])
test -z "$dflt" && dflt="$ac_cv_prog_less"
AC_CHECK_PROG(ac_cv_prog_pg, pg, [pg])
test -z "$dflt" && dflt="$ac_cv_prog_pg"
AC_CHECK_PROG(ac_cv_prog_cat, cat, [cat])
test -z "$dflt" && dflt="$ac_cv_prog_cat"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([What is the default pager? [$dflt] ])
read PAGER
test -z "$PAGER" && PAGER="$dflt"
AC_DEFINE_UNQUOTED(DEFPAGER, "$PAGER")
dflt=""
])

AC_DEFUN(AF_DEFINFO,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Checking which info browsers are available...])
AC_CHECK_PROG(ac_cv_prog_info, info, [info])
dflt="$ac_cv_prog_info"
test -n "$dflt" && dflt="$dflt af" || dflt="typeout"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([What is the default command to browse the info manual? [$dflt] ])
read INFO
test -z "$INFO" && INFO="$dflt"
AC_DEFINE_UNQUOTED(DEFINFOBROWSER, "$INFO")
AC_DEFINE_UNQUOTED(TYPEINFOBROWSER, "$LIBDIR/Afinfo $INFODIR/af")
dflt=""
])

AC_DEFUN(AF_DEFSPELLCHECK,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Checking which spell checkers are available...])
AC_CHECK_PROG(ac_cv_prog_ispell, ispell, [ispell])
dflt="$ac_cv_prog_ispell"
test -n "$dflt" && dflt="$dflt -x" || dflt=""
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([What is the default command to spell-check a file? [$dflt] ])
read ISPELL
test -z "$ISPELL" && ISPELL="$dflt"
test -z "$ISPELL" && AC_DEFINE_UNQUOTED(DEFSPELLCHECKER, NULL)
test -z "$ISPELL" || AC_DEFINE_UNQUOTED(DEFSPELLCHECKER, "$ISPELL")
dflt=""
])

AC_DEFUN(AF_DEFSPOOLER,
[AF_MSG_ECHO([ ])
AF_MSG_ECHO([Checking how to spool output...])
pr=""
AC_CHECK_PROG(ac_cv_prog_pr, pr, [pr])
AC_CHECK_PROG(ac_cv_prog_lpr, lpr, [lpr], [lp])
dflt="$ac_cv_prog_pr | $ac_cv_prog_lpr"
test -z "$ac_cv_prog_pr" && dflt="$ac_cv_prog_lpr"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([What is the default command to send output to the printer? [$dflt] ])
read SPOOLER
test -z "$SPOOLER" && SPOOLER="$dflt"
AC_DEFINE_UNQUOTED(DEFSPOOLER, "$SPOOLER")
dflt=""
])

AC_DEFUN(AF_DEFMIME,
[cat <<ENDOFMESSAGE

Af is capable of handling non-text (or MIME) messages if there is
an external program (such as metamail) which can display, save, and
print them.  If you don't want af to handle non-text messages but
there is a default for these questions, then answer 'none' to ignore
the default and not compile in MIME support.

ENDOFMESSAGE
AF_MSG_ECHO([Checking if metamail is available...])
AC_CHECK_PROG(ac_cv_prog_metamail, metamail, [metamail])
test -n "$ac_cv_prog_metamail" && dflt="$ac_cv_prog_metamail -m af -p"
AF_MSG_ECHO([ ])
AF_MSG_ECHO_N([What is the default command to display non-text messages? [$dflt] ])
read MIMEPAGER
test -z "$MIMEPAGER" && MIMEPAGER="$dflt"
test "$MIMEPAGER" = "none" && MIMEPAGER=""
AC_DEFINE_UNQUOTED(DEFMIMEPAGER, "$MIMEPAGER")
test -n "$ac_cv_prog_metamail" && dflt="$ac_cv_prog_metamail -m af -h"
AF_MSG_ECHO_N([What is the default command to spool non-text messages? [$dflt] ])
read MIMEPRINTER
test -z "$MIMEPRINTER" && MIMEPRINTER="$dflt"
test "$MIMEPRINTER" = "none" && MIMEPRINTER=""
AC_DEFINE_UNQUOTED(DEFMIMEPRINTER, "$MIMEPRINTER")
test -n "$ac_cv_prog_metamail" && dflt="$ac_cv_prog_metamail -m af -w"
AF_MSG_ECHO_N([What is the default command to save non-text messages? [$dflt] ])
read MIMESAVER
test -z "$MIMESAVER" && MIMESAVER="$dflt"
test "$MIMESAVER" = "none" && MIMESAVER=""
AC_DEFINE_UNQUOTED(DEFMIMESAVER, "$MIMESAVER")
dflt=""
])

AC_DEFUN(AF_COFFEE,
[cat <<ENDOFMESSAGE

That's the end of the questions for configuring af.  The rest of the
configuration will now trundle away by itself.
This may take some time.....

ENDOFMESSAGE
])
