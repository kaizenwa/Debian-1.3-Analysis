
dnl AC_PATH_FILE(VARIABLE, PATH, DIR-TO-CHECK-FOR [, VALUE-IF-NOT-FOUND])
AC_DEFUN(AC_PATH_FILE,
[# Check for the "$3" file on the system, in path "$2".
ac_word=[$3];
AC_MSG_CHECKING([for $ac_word])
AC_CACHE_VAL(ac_cv_path_$1,
[case "[$]$1" in
  /*)
  ac_cv_path_$1="[$]$1" # Let the user override the test with a path.
  ;;
  *)
  ac_dir_list=[$2]
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in $ac_dir_list; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$ac_word; then
      ac_cv_path_$1="$ac_dir/$ac_word"
      break
    fi
  done
  IFS="$ac_save_ifs"
dnl If no 4th arg is given, leave the cache variable unset,
dnl so AC_PATH_DIRS will keep looking.
ifelse([$4], , , [  test -z "[$]ac_cv_path_$1" && ac_cv_path_$1="$4"
])dnl
  ;;
esac])dnl
$1="$ac_cv_path_$1"
if test -n "[$]$1"; then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])

dnl AC_PATH_FILES(VARIABLE, PATH, FILES-TO-CHECK-FOR [, VALUE-IF-NOT-FOUND])
AC_DEFUN(AC_PATH_FILES,
[for ac_prog in $3
do
AC_PATH_FILE($1, $2, [$]ac_prog)
test -n "[$]$1" && break
done
ifelse([$4], , , [test -n "[$]$1" || $1="$4"
])])

dnl AC_NC_PATH_DIR(VARIABLE, PATH, DIR-TO-CHECK-FOR, [, VALUE-IF-NOT-FOUND])
AC_DEFUN(AC_NC_PATH_DIR,
[# Check for the "$3" directory on the system in path "$2".
ac_word=[$3];
AC_MSG_CHECKING([for $ac_word])
ac_dir_list=[$2]
IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
for ac_dir in $ac_dir_list; do
  test -z "$ac_dir" && ac_dir=.
  if test -d $ac_dir/$ac_word; then
    ac_cv_path_$1="$ac_dir/$ac_word"
    break
  fi
done
IFS="$ac_save_ifs"
$1="$ac_cv_path_$1"
if test -n "[$]$1"; then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])

dnl AC_NC_PATH_DIRS(VARIABLE, PATH, FILES-TO-CHECK-FOR [, VALUE-IF-NOT-FOUND])
AC_DEFUN(AC_NC_PATH_DIRS,
[for ac_prog in $3
do
AC_NC_PATH_DIR($1, $2, [$]ac_prog)
test -n "[$]$1" && break
done
ifelse([$4], , , [test -n "[$]$1" || $1="$4"
])])

