AC_DEFUN(GPC_PATH_GCCSRC, [
#
# Ok, lets find the gcc source tree
# Warning: transition of version 9 to 10 will break this algorithm
# because 10 sorts before 9.
# the alternative search directory is involked by --with-gccsrc
#
no_gccsrc=true
AC_MSG_CHECKING(for gcc sources)
AC_ARG_WITH(gccsrc, [  --with-gccsrc           directory where gcc sources are], with_gccsrc=${withval})
AC_CACHE_VAL(ac_cv_gccsrc,[
# first check to see if --with-gccsrc was specified
if test x"${with_gccsrc}" != x ; then
  if test -f ${with_gccsrc}/c-parse.in ; then
    ac_cv_gccsrc=${with_gccsrc}
  else
    AC_MSG_ERROR([${with_gccsrc} directory doesn't contain gcc sources])
  fi
fi
# next check relative to current directory.
#
# since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_gccsrc}" = x ; then
  for i in \
		../gcc \
		`ls -dr ../gcc-[[0-9]]* 2>/dev/null` \
		../../gcc \
		`ls -dr ../../gcc-[[0-9]]* 2>/dev/null` \
		../../../gcc \
		`ls -dr ../../../gcc-[[0-9]]* 2>/dev/null` ; do
    if test -f $i/c-parse.in ; then
      ac_cv_gccsrc=$i
      break
    fi
  done
fi
# next check relative to private source directory
#
# since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_gccsrc}" = x ; then
  for i in \
		${srcdir}/../gcc \
		`ls -dr ${srcdir}/../gcc-[[0-9]]* 2>/dev/null` \
		${srcdir}/../../gcc \
		`ls -dr ${srcdir}/../../gcc-[[0-9]]* 2>/dev/null` \
		${srcdir}/../../../gcc \
		`ls -dr ${srcdir}/../../../gcc-[[0-9]]* 2>/dev/null` ; do
    if test -f $i/c-parse.in ; then
      ac_cv_gccsrc=$i
      break
    fi
  done
fi
# finally check in a few common install locations
#
# since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_gccsrc}" = x ; then
  for i in \
		`ls -dr ${prefix}/src/gcc-[[0-9]]* 2>/dev/null` \
		${prefix}/src/gcc ; do
    if test -f $i/c-parse.in ; then
      ac_cv_gccsrc=`(cd $i; pwd)`
      break
    fi
  done
fi
])
if test x"${ac_cv_gccsrc}" = x ; then
  gccsrc="# no gcc sources found"
  AC_MSG_ERROR([Can't find gcc sources])
fi
if test x"${ac_cv_gccsrc}" != x ; then
  no_gccsrc=""
  gccsrc="${ac_cv_gccsrc}"
  AC_MSG_RESULT([found in ${ac_cv_gccsrc}])
fi
# RTS subdirectory. If ${gccsrc} is a path relative to . then prepend
# ../ to it to form ${rts_gccsrc}
if test `echo ${ac_cv_gccsrc} | cut -c1` = "/" ; then
  rts_gccsrc="${ac_cv_gccsrc}"
else
  rts_gccsrc="../${ac_cv_gccsrc}"
fi
AC_SUBST(gccsrc)
AC_SUBST(rts_gccsrc)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_PATH_GCCBIN, [
#
# Repeat this exercise for GCC object code
# the alternative search directory is involked by --with-gccbin
#
no_gccbin=true
AC_MSG_CHECKING(for gcc objects)
AC_ARG_WITH(gccbin, [  --with-gccbin           directory where gcc objects are], with_gccbin=${withval})
AC_CACHE_VAL(ac_cv_gccbin,[
# first check to see if --with-gccbin was specified
if test x"${with_gccbin}" != x ; then
  if test -f ${with_gccbin}/xgcc ; then
    ac_cv_gccbin=${with_gccbin}
  else
    AC_MSG_ERROR([${with_gccbin} directory doesn't contain gcc object code])
  fi
fi
# next check relative to current directory.
#
# since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_gccbin}" = x ; then
  for i in \
		../gcc \
		`ls -dr ../gcc-[[0-9]]* 2>/dev/null` \
		`ls -dr ../gcc-* 2>/dev/null` \
		../../gcc \
		`ls -dr ../../gcc-[[0-9]]* 2>/dev/null` \
		`ls -dr ../../gcc-* 2>/dev/null` \
		../../../gcc \
		`ls -dr ../../../gcc-[[0-9]]* 2>/dev/null` \
		`ls -dr ../../gcc-* 2>/dev/null` ; do
    if test -f $i/xgcc ; then
      ac_cv_gccbin=$i
      break
    fi
  done
fi
# next check relative to private source directory
#
# since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_gccbin}" = x ; then
  for i in \
		${srcdir}/../gcc \
		`ls -dr ${srcdir}/../gcc-[[0-9]]* 2>/dev/null` \
		`ls -dr ${srcdir}/../gcc-* 2>/dev/null` \
		${srcdir}/../../gcc \
		`ls -dr ${srcdir}/../../gcc-[[0-9]]* 2>/dev/null` \
		`ls -dr ${srcdir}/../../gcc-* 2>/dev/null` \
		${srcdir}/../../../gcc \
		`ls -dr ${srcdir}/../../../gcc-[[0-9]]* 2>/dev/null` \
		`ls -dr ${srcdir}/../../gcc-* 2>/dev/null` ; do
    if test -f $i/xgcc ; then
      ac_cv_gccbin=$i
      break
    fi
  done
fi
# finally check in a few common install locations
#
# since ls returns lowest version numbers first, reverse its output
if test x"${ac_cv_gccbin}" = x ; then
  for i in \
		`ls -dr ${prefix}/src/gcc-[[0-9]]* 2>/dev/null` \
		${prefix}/src/gcc ; do
    if test -f $i/xgcc ; then
      ac_cv_gccbin=`(cd $i; pwd)`
      break
    fi
  done
fi
])
if test x"${ac_cv_gccbin}" = x ; then
  gccbin="# no gcc sources found"
  AC_MSG_ERROR([Can't find gcc objects])
fi
if test x"${ac_cv_gccbin}" != x ; then
  no_gccbin=""
  gccbin="${ac_cv_gccbin}"
  AC_MSG_RESULT([found in ${ac_cv_gccbin}])
fi
# RTS subdirectory. If ${gccbin} is a path relative to . then prepend
# ../ to it to form ${rts_gccbin}
if test `echo ${ac_cv_gccbin} | cut -c1` = "/" ; then
  rts_gccbin="${ac_cv_gccbin}"
else
  rts_gccbin="../${ac_cv_gccbin}"
fi
AC_SUBST(gccbin)
AC_SUBST(rts_gccbin)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_VERSION, [
AC_MSG_CHECKING(for gcc version)
AC_CACHE_VAL(ac_cv_gcc_version,[
ac_cv_gcc_version=`grep '^version=' ${gccbin}/Makefile | sed -e 's/version=//'`])
if test x"${ac_cv_gcc_version}" = x ; then
  gcc_version="# no gcc version found"
  AC_MSG_ERROR([Can't determine gcc version])
else
  gcc_version="${ac_cv_gcc_version}"
  req_version=`grep '^GCCVERSION = ' ${srcdir}/Makefile.in | sed -e 's/GCCVERSION = //'`
  if test "${gcc_version}" != ${req_version} ; then
    AC_MSG_ERROR([found gcc ${gcc_version}, require gcc ${req_version}])
  else
    AC_MSG_RESULT([${gcc_version}])
  fi
fi
AC_SUBST(gcc_version)
])

#--------------------------------------------------------------------

dnl Extract target from GCC Makefile
AC_DEFUN(GPC_GCC_TARGET, [
AC_MSG_CHECKING(for gcc target system type)
AC_CACHE_VAL(ac_cv_gcc_target,[
# Cannot disable autoconf's --target switch, but halt if we find one.
if test x"${target}" != xNONE ; then
  AC_MSG_ERROR([do NOT specify --target when you configure gpc, gpc uses the target you selected when you configured gcc])
fi
])
if test -f x${gccbin}/Makefile ; then
  AC_MSG_ERROR([${gccbin}/Makefile not found])
fi
ac_cv_gcc_target=`grep '^target=' ${gccbin}/Makefile | sed -e 's/target=//'`
if test x"${ac_cv_gcc_target}" = x ; then
  gcc_target="# no gcc target found"
  AC_MSG_ERROR([Can't find gcc target])
else
  gcc_target="${ac_cv_gcc_target}"
  AC_MSG_RESULT([${ac_cv_gcc_target}])
fi
target="${gcc_target}"
AC_SUBST(gcc_target)
])

#--------------------------------------------------------------------

dnl Find out host system type.
dnl If !cross then host=target
dnl Otherwise, use default values.
AC_DEFUN(GPC_GCC_HOST, [
AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([GPC_GCC_TARGET])
AC_MSG_CHECKING(for gcc host system type)
AC_CACHE_VAL(ac_cv_gcc_host,[
ac_cv_gcc_cross=`grep '^CROSS=' ${gccbin}/Makefile | sed -e 's/CROSS=//'`
if test x"${ac_cv_gcc_cross}" = x ; then
  cross_compiling="no"
  cross=""
  ac_cv_gcc_host="${ac_cv_gcc_target}"
else
  cross_compiling="yes"
  native="no"
  cross="${ac_cv_gcc_cross}"
  ac_cv_gcc_host="${host}"
fi
])
AC_MSG_RESULT([${ac_cv_gcc_host}])
host="${ac_cv_gcc_host}"
AC_SUBST(cross)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_USE_COLLECT2, [
AC_MSG_CHECKING(whether gcc needs collect2)
AC_CACHE_VAL(ac_cv_gcc_use_collect2,[
ac_cv_gcc_use_collect2=`grep '^USE_COLLECT2 = ' ${gccbin}/Makefile | sed -e 's/USE_COLLECT2 = //'`
if test x"${ac_cv_gcc_use_collect2}" = x ; then
  ac_cv_gcc_use_collect2="no"
  use_collect2=""
  maybe_use_collect2=""
else
  ac_cv_gcc_use_collect2="yes"
  use_collect2="ld"
  maybe_use_collect2="-DUSE_COLLECT2"
  AC_MSG_RESULT(["yes"])
fi
])
AC_MSG_RESULT([${ac_cv_gcc_use_collect2}])
AC_SUBST(use_collect2)
AC_SUBST(maybe_use_collect2)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_TARGET_DEFAULT, [
AC_MSG_CHECKING(for gcc default CPU target)
AC_CACHE_VAL(ac_cv_gcc_default_cpu,[
ac_cv_gcc_default_cpu=`grep '^MAYBE_TARGET_DEFAULT = ' ${gccbin}/Makefile | sed -e 's/MAYBE_TARGET_DEFAULT = //'`
])
maybe_target_default="${ac_cv_gcc_default_cpu}"
default_cpu=`echo "${maybe_target_default}" | sed -e 's/-DTARGET_CPU_DEFAULT=//'`
AC_MSG_RESULT([${default_cpu}])
AC_SUBST(maybe_target_default)
])

#--------------------------------------------------------------------

dnl Find out build system type.
AC_DEFUN(GPC_GCC_BUILD, [
AC_REQUIRE([GPC_GCC_HOST])
AC_MSG_CHECKING(for gcc build system type)
AC_CACHE_VAL(ac_cv_gcc_build,[
ac_cv_gcc_build=`grep '^build=' ${gccbin}/Makefile | sed -e 's/build=//'`
if test x"${ac_cv_gcc_build}" = x ; then
  ac_cv_gcc_build="${ac_cv_gcc_host}"
  cross_building="no"
else
  cross_building="yes"
  native="no"
fi
])
AC_MSG_RESULT([${ac_cv_gcc_build}])
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_PREFIX, [
AC_MSG_CHECKING(for gcc prefix)
AC_CACHE_VAL(ac_cv_gcc_prefix,[
ac_cv_gcc_prefix=`grep '^prefix =' ${gccbin}/Makefile | sed -e 's/prefix = //'`])
if test x"${ac_cv_gcc_prefix}" = x ; then
  gcc_prefix="# no gcc prefix found"
  AC_MSG_ERROR([Can't determine gcc prefix])
else
  gcc_prefix="${ac_cv_gcc_prefix}"
  AC_MSG_RESULT([${ac_cv_gcc_prefix}])
fi
prefix="${gcc_prefix}"
AC_SUBST(gcc_prefix)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_LOCAL_PREFIX, [
AC_MSG_CHECKING(for gcc local_prefix)
AC_CACHE_VAL(ac_cv_gcc_local_prefix,[
ac_cv_gcc_local_prefix=`grep '^local_prefix =' ${gccbin}/Makefile | sed -e 's/local_prefix = //'`])
if test x"${ac_cv_gcc_local_prefix}" = x ; then
  gcc_local_prefix="# no gcc local_prefix found"
  AC_MSG_ERROR([Can't determine gcc local_prefix])
else
  gcc_local_prefix="${ac_cv_gcc_local_prefix}"
  AC_MSG_RESULT([${ac_cv_gcc_local_prefix}])
fi
local_prefix="${gcc_local_prefix}"
AC_SUBST(gcc_local_prefix)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_EXTRA_OBJS, [
AC_MSG_CHECKING(for gcc extra_objs)
AC_CACHE_VAL(ac_cv_gcc_extra_objs,[
ac_cv_gcc_extra_objs=`grep '^EXTRA_OBJS = ' ${gccbin}/Makefile | sed -e 's/EXTRA_OBJS = //'`])
if test x"${ac_cv_gcc_extra_objs}" = x ; then
  gcc_extra_objs=""
  AC_MSG_RESULT([none])
else
  gcc_extra_objs="${gccbin}/${ac_cv_gcc_extra_objs}"
  AC_MSG_RESULT([${ac_cv_gcc_extra_objs}])
fi
AC_SUBST(gcc_extra_objs)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_EXTRA_GCC_OBJS, [
AC_MSG_CHECKING(for gcc extra_gcc_objs)
AC_CACHE_VAL(ac_cv_gcc_extra_gcc_objs,[
ac_cv_gcc_extra_gcc_objs=`grep '^EXTRA_GCC_OBJS = ' ${gccbin}/Makefile | sed -e 's/EXTRA_GCC_OBJS = //'`])
if test x"${ac_cv_gcc_extra_gcc_objs}" = x ; then
  gcc_extra_gcc_objs=""
  AC_MSG_RESULT([none])
else
  gcc_extra_gcc_objs="${gccbin}/${ac_cv_gcc_extra_gcc_objs}"
  AC_MSG_RESULT([${ac_cv_gcc_extra_gcc_objs}])
fi
AC_SUBST(gcc_extra_gcc_objs)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_GCC_OUT_OBJECT_FILE, [
AC_MSG_CHECKING(for gcc out_object_file)
AC_CACHE_VAL(ac_cv_gcc_out_object_file,[
ac_cv_gcc_out_object_file=`grep '^out_object_file=' ${gccbin}/Makefile | sed -e 's/out_object_file=//'`])
if test x"${ac_cv_gcc_out_object_file}" = x ; then
  out_object_file="# no gcc out_object_file found"
  AC_MSG_ERROR([Can't determine gcc out_object_file])
else
  out_object_file="${ac_cv_gcc_out_object_file}"
  AC_MSG_RESULT([${ac_cv_gcc_out_object_file}])
fi
AC_SUBST(out_object_file)
])
#--------------------------------------------------------------------

dnl Locate "ar" for ${target}
AC_DEFUN(GPC_PROG_AR, [
no_ar=true
AC_MSG_CHECKING(for cross-ar)
AC_ARG_WITH(ar, [  --with-ar               ar utility to use], with_ar=${withval})
AC_CACHE_VAL(ac_cv_prog_x_ar,[
# first check to see if --with-ar was specified
if test x"${with_ar}" != x ; then
  ac_cv_prog_x_ar=${with_ar}
fi
if test x"${ac_cv_prog_x_ar}" = x ; then
  # See if we're crosscompiling.
  #
  if test "${cross_compiling}" = "yes" ; then
    if test -f ${prefix}/bin/${target}-ar; then
      ac_cv_prog_x_ar=${target}-ar 
    else
      if test -f ${prefix}/${target}/bin/ar; then
        ac_cv_prog_x_ar=${prefix}/${target}/bin/ar
      fi
    fi
  else
    # Maybe we're crossbuilding.
    #
    if test "${cross_building}" = "yes" ; then
      if test -f ${prefix}/bin/${target}-ar; then
        ac_cv_prog_x_ar=${target}-ar 
      else
        if test -f ${prefix}/${target}/bin/ar; then
          ac_cv_prog_x_ar=${prefix}/${target}/bin/ar
        fi
      fi
    else
      # Just a native compiler. How boring ;-) 
      #
      ac_cv_prog_x_ar=ar
    fi
  fi
fi
])
if test x"${ac_cv_prog_x_ar}" = x ; then
  AR="# no ar found"
  AC_MSG_ERROR([Unable to find suitable ar. Rerun configure with --with-ar switch])
else
  no_ar=""
  AR="${ac_cv_prog_x_ar}"
  AC_MSG_RESULT([${ac_cv_prog_x_ar}])
fi
AC_SUBST(AR)
])

#--------------------------------------------------------------------

# Locate "ranlib" for ${target}
AC_DEFUN(GPC_PROG_RANLIB, [
no_ranlib=true
AC_MSG_CHECKING(for cross-ranlib)
AC_ARG_WITH(ranlib, [  --with-ranlib           ranlib utility to use], with_ranlib=${withval})
AC_CACHE_VAL(ac_cv_prog_x_ranlib,[
# first check to see if --with-ranlib was specified
if test x"${with_ranlib}" != x ; then
  ac_cv_prog_x_ranlib=${with_ranlib}
fi
if test x"${ac_cv_prog_x_ranlib}" = x ; then
  # See if we're crosscompiling.
  #
  if test "${cross_compiling}" = "yes" ; then
    if test -f ${prefix}/bin/${target}-ranlib; then
      ac_cv_prog_x_ranlib=${target}-ranlib
    else
      if test -f ${prefix}/${target}/bin/ranlib; then
        ac_cv_prog_x_ranlib=${prefix}/${target}/bin/ranlib
      fi
    fi
  else
    # See if we're crossbuilding.
    #
    if test "${cross_building}" = "yes" ; then
      if test -f ${prefix}/bin/${target}-ranlib; then
        ac_cv_prog_x_ranlib=${target}-ranlib
      else
        if test -f ${prefix}/${target}/bin/ranlib; then
          ac_cv_prog_x_ranlib=${prefix}/${target}/bin/ranlib
        fi
      fi
    else
      # Just a native compiler. How boring ;-) 
      #
      ac_cv_prog_x_ranlib=ranlib
    fi
  fi
fi
])
if test x"${ac_cv_prog_x_ranlib}" = x ; then
  RANLIB="# no ranlib found"
  AC_MSG_ERROR([Unable to find suitable ranlib. Rerun configure with --with-ranlib switch])
else
  no_ranlib=""
  RANLIB="${ac_cv_prog_x_ranlib}"
  AC_MSG_RESULT([${ac_cv_prog_x_ranlib}])
fi
AC_SUBST(RANLIB)
])

#--------------------------------------------------------------------

# Locate "gcc" for ${target}
AC_DEFUN(GPC_PROG_GCC, [
no_gcc=true
AC_MSG_CHECKING(for cross-gcc)
AC_ARG_WITH(gcc, [  --with-gcc              gcc used to build gpc], with_gcc=${withval})
AC_CACHE_VAL(ac_cv_prog_x_gcc,[
# first check to see if --with-gcc was specified
if test x"${with_gcc}" != x ; then
  ac_cv_prog_x_gcc=${with_gcc}
fi
if test x"${ac_cv_prog_x_gcc}" = x ; then
  # See if we're crosscompiling.
  #
  if test "${cross_compiling}" = "yes" ; then
    if test -f ${prefix}/bin/${target}-gcc; then
      ac_cv_prog_x_gcc=${target}-gcc
    else
      if test -f ${prefix}/${target}/bin/gcc; then
        ac_cv_prog_x_gcc=${prefix}/${target}/bin/gcc
      fi
    fi
  else
    # See if we're crossbuilding.
    #
    if test "${cross_building}" = "yes" ; then
      if test -f ${prefix}/bin/${target}-gcc; then
        ac_cv_prog_x_gcc=${target}-gcc
      else
        if test -f ${prefix}/${target}/bin/gcc; then
          ac_cv_prog_x_gcc=${prefix}/${target}/bin/gcc
        fi
      fi
    else
      # Just a native compiler. How boring ;-) 
      #
      ac_cv_prog_x_gcc=gcc
    fi
  fi
fi
])
if test x"${ac_cv_prog_x_gcc}" = x ; then
  AC_MSG_ERROR([Unable to find suitable gcc. Rerun configure with --with-gcc switch])
else
  no_gcc=""
  AC_MSG_RESULT([${ac_cv_prog_x_gcc}])
  if test "${cross_building}" = "yes" ; then
    ac_cv_prog_CC="${ac_cv_prog_x_gcc}"
  fi
fi
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_PROG_RTS_GCC, [
#
# RTS must be compiled with GCC.
# The GCC upon which GPC is based could be used.
# If you (like me) have a bug-fixed gcc installed, but the bugfixes
# break gpc (sources) then you know why I search for installed gcc first,
# then for ${gccbin}/xgcc -B{gccbin}/
#
AC_MSG_CHECKING(for gcc to use for RTS)
AC_ARG_WITH(rtsgcc, [  --with-rtsgcc           gcc used to build libgpc.a], with_rtsgcc=${withval})
AC_CACHE_VAL(ac_cv_prog_rts_gcc,[
# first check to see if --with-rtsgcc was specified
if test x"${with_rtsgcc}" != x ; then
  ac_cv_prog_rts_gcc=${with_rtsgcc}
fi
if test x"${ac_cv_prog_rts_gcc}" = x ; then
  # See if we're crossbuilding.
  #
  if test "${cross_building}" = "yes" ; then
    ac_cv_prog_rts_gcc="${ac_cv_prog_x_gcc}"
  else
    # See if we're crosscompiling.
    #
    if test "${cross_compiling}" = "yes" ; then
      ac_cv_prog_rts_gcc="${ac_cv_prog_x_gcc}"
    else
      #
      # Native compiler requested.
      # If GCC is installed, use it. Otherwise, use the gcc we just built.
      if test "${ac_cv_prog_gcc}" = "yes" ; then
        ac_cv_prog_rts_gcc="gcc"
      else
        ac_cv_prog_rts_gcc="${gccbin}/xgcc -B${gccbin}/"
      fi
    fi
  fi
fi
])
if test x"${ac_cv_prog_rts_gcc}" = x ; then
  RTS_GCC="# no rts_gcc found"
  AC_MSG_ERROR([Unable to find suitable gcc for libgpc.a. Rerun configure with --with-rtsgcc switch])
else
  RTS_GCC="${ac_cv_prog_rts_gcc}"
  AC_MSG_RESULT([${ac_cv_prog_rts_gcc}])
fi
AC_SUBST(RTS_GCC)
])

#--------------------------------------------------------------------

AC_DEFUN(GPC_BSD_RTS, [
#
# Enable BSD RTS only if
# (1) BSD style signals are supported
# (2) The user requested them (--with-bsd-rts)
#
AC_MSG_CHECKING(for BSD style RTS)
AC_ARG_WITH(bsdrts, [  --with-bsdrts           BSD style signals for RTS], with_bsdrts=${withval})
AC_CACHE_VAL(ac_cv_bsd_rts,[
# first check to see if --with-bsdrts was specified
if test x"${with_bsdrts}" != x ; then
  ac_cv_bsd_rts=${with_bsdrts}
fi
if test x"${with_bsdrts}" != x ; then
  # So you want BSD signals -- but what if you're really SYSV...
  AC_TRY_COMPILE([#include <signal.h>], [int i=SIGEMT;],
	ac_cv_bsd_rts=yes,
	ac_cv_bsd_rts=no)
  if test "${ac_cv_bsd_rts}" = "no"; then
    echo -n "detected SYSV ... "
  fi
else
  ac_cv_bsd_rts="no"
fi
])
AC_MSG_RESULT([${ac_cv_bsd_rts}])
if test "${ac_cv_bsd_rts}" = "yes"; then
  AC_DEFINE([BSD_RTS])
fi
])
