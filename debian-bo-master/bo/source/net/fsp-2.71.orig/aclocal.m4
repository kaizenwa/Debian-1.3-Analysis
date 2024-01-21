dnl These are local macros that I've needed to define to keep the 
dnl configure.in file readable or to correct for errors on certain systems
dnl such as linux's egrep returning the wrong exit status.
dnl
define(FSP_PROGRAM_GREP,
[AC_REQUIRE([AC_PROG_CPP])AC_PROVIDE([$0])cat > conftest.c <<EOF
[$2]
EOF
eval "$CPP $DEFS conftest.c > conftest.out 2>&1"
if grep "$1" conftest.out >/dev/null 2>&1; then
  ifelse([$3], , :, [$3])
ifelse([$4], , , [else 
  $4
])dnl
fi
rm -f conftest*
])dnl
dnl
define(FSP_HEADER_GREP,
[AC_REQUIRE([AC_PROG_CPP])AC_PROVIDE([$0])echo '#include <$2>' > conftest.c
eval "$CPP $DEFS conftest.c > conftest.out 2>&1"
if grep "$1" conftest.out >/dev/null 2>&1; then
  ifelse([$3], , :, [$3])
ifelse([$4], , , [else 
  $4
])dnl
fi
rm -f conftest*
])dnl
dnl
dnl
dnl checks for typedefs
dnl
define(FSP_UID_T,
[echo checking for uid_t in sys/types.h
FSP_HEADER_GREP(uid_t, sys/types.h, ,
  AC_DEFINE(uid_t, int) AC_DEFINE(gid_t, int))])dnl
dnl
define(FSP_NLINK_T,
[echo checking for nlink_t in sys/types.h
FSP_HEADER_GREP(nlink_t, sys/types.h, , AC_DEFINE(nlink_t, int))])dnl
dnl
define(FSP_SIZE_T,
[echo checking for size_t in sys/types.h
FSP_HEADER_GREP(size_t, sys/types.h, , AC_DEFINE(size_t, unsigned))])dnl
dnl
define(FSP_PID_T,
[echo checking for pid_t in sys/types.h
FSP_HEADER_GREP(pid_t, sys/types.h, , AC_DEFINE(pid_t, int))])dnl
dnl
define(FSP_MODE_T,
[echo checking for mode_t in sys/types.h
FSP_HEADER_GREP(mode_t, sys/types.h, , AC_DEFINE(mode_t, int))])dnl
dnl
