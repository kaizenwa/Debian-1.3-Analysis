AC_DEFUN(RLPR_MAXHOSTNAMELEN_BROKEN,
[AC_MSG_CHECKING(whether MAXHOSTNAMELEN is incorrectly in netdb.h)
AC_CACHE_VAL(rlpr_cv_header_MAXHOSTNAMELEN_broken,
[AC_EGREP_CPP(yes,
[#include <netdb.h>
 #ifdef MAXHOSTNAMELEN
 yes
 #endif
], rlpr_cv_header_MAXHOSTNAMELEN_broken=yes)
])
if test "$rlpr_cv_header_MAXHOSTNAMELEN_broken" = "yes"; then
   AC_MSG_RESULT(yes)
   AC_DEFINE(MAXHOSTNAMELEN_BROKEN)
else
   AC_MSG_RESULT(no)
fi])
dnl
dnl
dnl
AC_DEFUN(RLPR_SETRESUID,
[AC_MSG_CHECKING(for setresuid)
AC_CACHE_VAL(rlpr_cv_func_setresuid,
[AC_EGREP_HEADER(setresuid, unistd.h, rlpr_cv_func_setresuid=yes)
])
if test "$rlpr_cv_func_setresuid" = "yes"; then
   AC_MSG_RESULT(yes)
   AC_DEFINE(HAVE_SETRESUID)
else
   AC_MSG_RESULT(no)
fi])
dnl
dnl
dnl
AC_DEFUN(RLPR_OFF_T_LONG_LONG,
[AC_MSG_CHECKING(whether off_t is a long long)
AC_CACHE_VAL(rlpr_cv_type_off_t_long_long,
[AC_TRY_RUN(
[ #include <sys/types.h>
  int main() { return sizeof(long) == sizeof(off_t); }
], rlpr_cv_type_off_t_long_long=yes)
])
if test "$rlpr_cv_type_off_t_long_long" = "yes"; then
   AC_MSG_RESULT(yes)
   AC_DEFINE(OFF_T_LONG_LONG)
else
   AC_MSG_RESULT(no)
fi])
