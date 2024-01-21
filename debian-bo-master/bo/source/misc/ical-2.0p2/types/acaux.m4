dnl ========================================================================
dnl Copyright (c) 1995 by Sanjay Ghemawat
dnl
dnl ========================================================================
acaux_fail=''
define(ACAUX_FAIL,
[AC_PROVIDE([$0])
echo ""
echo "*** I could not find the $1."
echo "    Please set the environment variable \"$2\" to the directory"
echo "    that contains the $1 and rerun configure."
echo ""
acaux_fail=yes])
dnl ========================================================================
dnl Usage: ACAUX_FIND_FILE(files,dirs)
dnl
dnl Find one of the named files in specified directories and set
dnl ac_dir to directory name, ac_file to file name.  If not found,
dnl set ac_dir/ac_file to empty strings.  First looks for all files
dnl in first directory, then in second directory, and so on.
dnl The files are searched for in the order of their names in "files".
dnl
AC_DEFUN(ACAUX_FIND_FILE,[
ac_dir=
ac_file=
for d in $2; do
    for f in $1; do
        if test -f "$d/$f"; then
	   ac_dir="$d"
	   ac_file="$f"
	   break
	   fi
	done
    if test -n "$ac_file"; then
	break
	fi
    done])
dnl ========================================================================
dnl Usage: ACAUX_FIND_LIB(name,[action-if-found,[action-if-not-found]])
dnl
dnl Look for named library.  If found, execute action-if-found, else
dnl execute action-if-not-found.  Default action-if-found is to add
dnl flags to "LIBS".  Default action-if-not-found is to do nothing.
dnl
AC_DEFUN(ACAUX_FIND_LIB,[
AC_MSG_CHECKING(for $1 library)
if test -z "$prefix"; then
  dirs="/usr/local/lib /usr/lib"
else
  dirs="$prefix/lib /usr/local/lib /usr/lib"
fi
ACAUX_FIND_FILE(lib$1.a lib$1.so lib$1.so.*,$dirs)
if test -z "$ac_dir"; then
   AC_MSG_RESULT(not found)
   ifelse([$3], , , [$3])
else
   AC_MSG_RESULT(found in $ac_dir)
   ifelse([$2], ,LIBS="-L$ac_dir -l$1 $LIBS",[$2])
fi])
dnl ========================================================================
dnl Usage: ACAUX_FIND_HEADER(name,[action-if-found,[action-if-not-found]])
dnl
dnl Look for named header file.  If found, execute action-if-found, else
dnl execute action-if-not-found.  Default action-if-found is to add
dnl directory to "CFLAGS".  Default action-if-not-found is to do nothing.
dnl
AC_DEFUN(ACAUX_FIND_HEADER,[
AC_MSG_CHECKING(for $1)
if test -z "$prefix"; then
  dirs="/usr/local/include /usr/include"
else
  dirs="$prefix/include /usr/local/include /usr/include"
fi
ACAUX_FIND_FILE($1,$dirs)
if test -z "$ac_dir"; then
   AC_MSG_RESULT(not found)
   ifelse([$3], , , [$3])
else
   AC_MSG_RESULT(found in $ac_dir)
   ifelse([$2], ,CFLAGS="-I$ac_dir $CFLAGS",[$2])
fi])
dnl ========================================================================
dnl
dnl Find Tcl stuff
dnl
AC_DEFUN(ACAUX_FIND_TCL,[
if test -z "$tcllib"; then
   ACAUX_FIND_LIB(tcl7.4,,ACAUX_FIND_LIB(tcl))
   if test -z "$ac_dir"; then
      ACAUX_FAIL(tcl library,tcllib)
      ac_dir=/nowhere
   fi
   tcllib="$ac_dir"
else
   LIBS="-L$tcllib -ltcl"
fi
if test -z "$tclinc"; then
   ACAUX_FIND_HEADER(tcl.h)
   if test -z "$ac_dir"; then
      ACAUX_FAIL(tcl header file,tclinc)
   fi
else
   CFLAGS="-I$tclinc"
fi
if test -z "$tclscripts"; then
   AC_MSG_CHECKING(for tcl scripts)
   dirs="/usr/local/lib/tcl7.4 /usr/lib/tcl7.4 /usr/local/lib/tcl /usr/lib/tcl"
   if test -n "$prefix"; then
      dirs="$prefix/lib/tcl7.4 $prefix/lib/tcl $dirs"
   fi
   ACAUX_FIND_FILE(init.tcl,$dirs)
   if test -z "$ac_dir"; then
      ACAUX_FAIL(tcl scripts,tclscripts)
   else
      AC_MSG_RESULT(found in $ac_dir)
      tclscripts="$ac_dir"
   fi
fi
AC_SUBST(tclscripts)])dnl
dnl ========================================================================
dnl
dnl Find Tk stuff
dnl
AC_DEFUN(ACAUX_FIND_TK,[
if test -z "$tklib"; then
   ACAUX_FIND_LIB(tk4.0,,ACAUX_FIND_LIB(tk))
   if test -z "$ac_dir"; then
      ACAUX_FAIL(tk library,tklib)
      ac_dir=/nowhere
   fi
   tklib="$ac_dir"
else
   LIBS="-L$tklib -ltk"
fi
if test -z "$tkinc"; then
   ACAUX_FIND_HEADER(tk.h)
   if test -z "$ac_dir"; then
      ACAUX_FAIL(tk header file,tkinc)
   fi
else
   CFLAGS="-I$tkinc"
fi
if test -z "$tkscripts"; then
   AC_MSG_CHECKING(for tk scripts)
   dirs="/usr/local/lib/tk4.0 /usr/lib/tk4.0 /usr/local/lib/tk /usr/lib/tk"
   if test -n "$prefix"; then
      dirs="$prefix/lib/tk4.0 $prefix/lib/tk $dirs"
   fi
   ACAUX_FIND_FILE(tk.tcl,$dirs)
   if test -z "$ac_dir"; then
      ACAUX_FAIL(tk scripts,tkscripts)
   else
      AC_MSG_RESULT(found in $ac_dir)
      tkscripts="$ac_dir"
   fi
fi
AC_SUBST(tkscripts)])dnl
dnl
dnl Exit if something failed
dnl
define(ACAUX_FAIL_CHECK,
[if test -n "$acaux_fail"; then
  AC_ERROR(Configuration failed.)
fi])dnl
dnl
dnl Check for function prototype
dnl
define(ACAUX_HAVE_PROTO,
[AC_PROVIDE([$0])
changequote(,)dnl
ac_tr_func=HAVE_`echo $1 | tr '[a-z]' '[A-Z]'`_PROTO
changequote([,])dnl
AC_COMPILE_CHECK([$1 prototype],[#include <$2>],[static void* ptr = &$1],
AC_DEFINE(${ac_tr_func}))])dnl
