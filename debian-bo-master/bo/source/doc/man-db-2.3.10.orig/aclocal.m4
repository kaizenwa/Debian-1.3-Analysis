dnl
dnl Check to see if sprintf() returns the number of chars converted
dnl Take no arguments
dnl
AC_DEFUN(MAN_FUNC_ANSI_SPRINTF,
[AC_MSG_CHECKING(for ANSI C sprintf)

AC_CACHE_VAL(man_cv_func_ansi_sprintf,
[AC_TRY_RUN(
changequote(<<, >>)dnl
<<int main(void)
{char str[12];
if ((int)sprintf(str,"test_string")==11) exit(0);
exit(1);
}>>,
changequote([, ])dnl
man_cv_func_ansi_sprintf=yes, 
[man_cv_func_ansi_sprintf=no],
[man_cv_func_ansi_sprintf=no])dnl
])

test "$man_cv_func_ansi_sprintf" = "yes" && AC_DEFINE(ANSI_SPRINTF)

AC_MSG_RESULT($man_cv_func_ansi_sprintf)
])

dnl
dnl Check to see if nroff is GNU nroff, take nroff path as arg.
dnl
AC_DEFUN(MAN_PROG_GNU_NROFF,
[AC_MSG_CHECKING(whether nroff is GNU nroff)

AC_CACHE_VAL(man_cv_prog_gnu_nroff,
[if test `echo "\\n(.g" | $1 | tr -d '\n'` -eq 1
then
	man_cv_prog_gnu_nroff=yes
else
	man_cv_prog_gnu_nroff=no
fi])

if test "$man_cv_prog_gnu_nroff" = "yes" 
then
	AC_DEFINE(GNU_NROFF)
fi
AC_MSG_RESULT($man_cv_prog_gnu_nroff)
])
dnl
dnl Check to see if popen/pclose correctly handles multiple streams
dnl
AC_DEFUN(MAN_FUNC_PCLOSE,
[AC_MSG_CHECKING(for working pclose)

AC_CACHE_VAL(man_cv_func_pclose,
[AC_TRY_RUN(
[#include <stdio.h>
#include <sys/wait.h>
int
try_pclose (FILE *f, int expected)
{
  int s;
  s = pclose (f);
  if (s != -1 && WIFEXITED (s))
    return WEXITSTATUS (s) != expected;
  else 
    return 1;
}
int
main (void)
{
  FILE *f, *t;
  int i, sf, st;
  for (i = 0;  i <= 1;  ++i) {
    f = popen ("false", "r");
    sleep (1);
    t = popen ("true", "r");
    sleep (1);
    if (i) {
      st = try_pclose (t, 0);
      sf = try_pclose (f, 1);
    } else {
      sf = try_pclose (f, 1);
      st = try_pclose (t, 0);
    }
    if (sf || st) exit(1);
  }
  exit(0);
}],
man_cv_func_pclose=yes,
[man_cv_func_pclose=no],
[man_cv_func_pclose=no])dnl
])

test "man_cv_func_pclose" = "no" && AC_DEFINE(BROKEN_PCLOSE)

AC_MSG_RESULT($man_cv_func_pclose)
])
