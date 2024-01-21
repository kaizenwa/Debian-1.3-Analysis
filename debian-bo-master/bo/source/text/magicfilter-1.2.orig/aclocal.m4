dnl Like AC_PATH_PROGS, but add to the .h file as well
AC_DEFUN(AC_PATH_INCLUDE,
[AC_PATH_PROGS($1,$2)
if test -n "$$1"; then
  AC_DEFINE(HAVE_$1)
  AC_DEFINE_UNQUOTED(PATH_$1, $$1)
fi])

dnl Like AC_PATH_INCLUDE, but enclose in double quotes for inclusion into C
AC_DEFUN(AC_PATH_CINCLUDE,
[AC_PATH_PROGS($1,$2)
if test -n "$$1"; then
  AC_DEFINE(HAVE_$1)
  AC_DEFINE_UNQUOTED(PATH_$1, "$$1")
fi])

dnl Check to see if zcat is really gzip
AC_DEFUN(AC_PROG_GNU_ZCAT,
[AC_MSG_CHECKING(whether zcat is really gzip)

AC_CACHE_VAL(ac_cv_prog_gnu_zcat,
[if echo -e "\37\213\10\10\273\31\152\57\4\3\170\0\3\0\0\0\0\0\0\0\0\0\c"| $1 \
	2>&1 >/dev/null || grep "GNU" $1 2>&1 >/dev/null
then
	ac_cv_prog_gnu_zcat=yes
else
	ac_cv_prog_gnu_zcat=no
fi])

if test "$ac_cv_prog_gnu_zcat" = "yes" 
then
	AC_DEFINE(ZCAT_IS_GZIP)
fi
AC_MSG_RESULT($ac_cv_prog_gnu_zcat)
])

dnl Check to see if ditroff works
AC_DEFUN(AC_PROG_DITROFF,
[AC_MSG_CHECKING(whether ditroff works)

AC_CACHE_VAL(ac_cv_prog_ditroff,
[if cat << EOF | $1 2>&1 >/dev/null
x T ps
x res 72000 1 1
x init
p1
x trailer
V792000
x stop
EOF
then
	ac_cv_prog_ditroff=yes
else
	ac_cv_prog_ditroff=no
fi])

if test "$ac_cv_prog_ditroff" = "yes" 
then
	AC_DEFINE(DITROFF_WORKS)
fi
AC_MSG_RESULT($ac_cv_prog_ditroff)
])
