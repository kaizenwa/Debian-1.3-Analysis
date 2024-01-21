dnl replace AC_DIR_HEADER so that we can hack a lynx make into the check
dnl for dirent, if we're not POSIX then it isn't safe
undefine([AC_DIR_HEADER])dnl
define(AC_DIR_HEADER,
[AC_PROVIDE([$0])AC_CHECKING(for directory library header)
ac_dir_header=
AC_DIR_HEADER_SPECIAL_CHECK(dirent.h, DIRENT)
AC_DIR_HEADER_CHECK(sys/ndir.h, SYSNDIR)
AC_DIR_HEADER_CHECK(sys/dir.h, SYSDIR)
AC_DIR_HEADER_CHECK(ndir.h, NDIR)

AC_CHECKING(for closedir return value)
AC_TEST_PROGRAM([#include <sys/types.h>
#include <$ac_dir_header>
int closedir(); main() { exit(closedir(opendir(".")) != 0); }], ,
AC_DEFINE(VOID_CLOSEDIR))
])dnl
dnl Subroutine of AC_DIR_HEADER.
dnl Just like AC_DIR_HEADER_CHECK, but always fails on Lynx
dnl since the existence of dirent.h doesn't actually imply correctness.
define(AC_DIR_HEADER_SPECIAL_CHECK, [dnl
if test -z "$ac_dir_header"; then
  AC_COMPILE_CHECK($1, [#ifdef __Lynx__
#ifndef _POSIX_SOURCE
 #error "Lynx dirent is only valid if really in the POSIX environment"
#endif
#endif
#include <sys/types.h>
#include <]$1[>],
	  	   [DIR *dirp = 0;],
		   AC_DEFINE($2) ac_dir_header=$1)dnl
fi])dnl
