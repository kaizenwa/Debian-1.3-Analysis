dnl aclocal.m4 -- custom autoconf macros for the Universal NFS server
dnl Updated for Autoconf v2
dnl
dnl *********** rpcgen.new ***************
define(AC_PROG_RPCGEN,
[AC_PROGRAMS_CHECK(RPCGEN, rpcgen.new rpcgen, rpcgen)])dnl
dnl
dnl *********** rpcgen -C ****************
define(AC_RPCGEN_C,
  [AC_MSG_CHECKING(for rpcgen -C)
  AC_CACHE_VAL(nfsd_cv_prog_RPCGEN_C,
  [if $RPCGEN -C -c </dev/null >/dev/null 2>/dev/null
  then
    nfsd_cv_prog_RPCGEN_C=yes
  else
    nfsd_cv_prog_RPCGEN_C=no
  fi])dnl
  AC_MSG_RESULT($nfsd_cv_prog_RPCGEN_C)
  if test $nfsd_cv_prog_RPCGEN_C = yes; then
    RPCGEN_C=-C
  fi
  AC_SUBST(RPCGEN_C)
  test -n "$RPCGEN_C" && AC_DEFINE(HAVE_RPCGEN_C)
])dnl
dnl
dnl *********** rpcgen -I ****************
define(AC_RPCGEN_I,
  [AC_MSG_CHECKING(for rpcgen -I)
  AC_CACHE_VAL(nfsd_cv_prog_RPCGEN_I,
  [if $RPCGEN -I -c </dev/null >/dev/null 2>/dev/null
  then
    nfsd_cv_prog_RPCGEN_I=yes
  else
    nfsd_cv_prog_RPCGEN_I=no
  fi
  ]) dnl
  AC_MSG_RESULT($nfsd_cv_prog_RPCGEN_I)
  if test $nfsd_cv_prog_RPCGEN_I = yes; then
    RPCGEN_I=-I
  fi
  AC_SUBST(RPCGEN_I)
  test -n "$RPCGEN_I" && AC_DEFINE(HAVE_RPCGEN_I)
])dnl
dnl *********** xdrproc_t ****************
define(AC_XDRPROC_T,
  [AC_MSG_CHECKING(for xdrproc_t)
  AC_CACHE_VAL(nfsd_cv_type_xdrproc_t,
  [AC_EGREP_HEADER(xdrproc_t, rpc/xdr.h,
    nfsd_cv_type_xdrproc_t=yes, nfsd_cv_type_xdrproc_t=no)
  ]) dnl
  AC_MSG_RESULT($nfsd_cv_type_xdrproc_t)
  test $nfsd_cv_type_xdrproc_t = yes && AC_DEFINE(HAVE_XDRPROC_T)
])dnl
dnl
dnl *********** mountlist ****************
define(AC_MOUNTLIST,
  [AC_MSG_CHECKING(how to get list of mounted filesystems)
  AC_CACHE_VAL(nfsd_cv_func_mountlist,
    [mounted=

    # DEC Alpha running OSF/1.
    AC_TRY_LINK([
      #include <sys/types.h>
      #include <sys/mount.h>
      #include <sys/fs_types.h>],
      [struct statfs *stats;
      numsys = getfsstat ((struct statfs *)0, 0L, MNT_WAIT); ],
      nfsd_cv_func_mountlist=getfsstat mounted=1)
    if test -z "$mounted"; then
    # SVR4
    AC_HEADER_EGREP(getmntent, sys/mnttab.h,
      nfsd_cv_func_mountlist=getmntent2 mounted=1)
    fi
    if test -z "$mounted"; then
    # AIX.
    AC_TEST_CPP([#include <fshelp.h>], 
      nfsd_cv_func_mountlist=vmount mounted=1)
    fi
    if test -z "$mounted"; then
    # SVR3
    AC_TEST_CPP([#include <sys/statfs.h>
      #include <sys/fstyp.h>
      #include <mnttab.h>], 
      nfsd_cv_func_mountlist=fread_fstyp mounted=1)
    fi
    if test -z "$mounted"; then
    # 4.3BSD
    AC_TEST_CPP([#include <mntent.h>], 
      nfsd_cv_func_mountlist=getmntent1 mounted=1)
    fi
    if test -z "$mounted"; then
    # 4.4BSD and DEC OSF/1.
    AC_HEADER_EGREP(f_type;, sys/mount.h,  
      nfsd_cv_func_mountlist=getmntinfo mounted=1)
    fi
    if test -z "$mounted"; then
    # Ultrix
    AC_TEST_CPP([#include <sys/fs_types.h>
    #include <sys/mount.h>],
      nfsd_cv_func_mountlist=getmnt mounted=1)
    fi
    if test -z "$mounted"; then
    # SVR2
    AC_TEST_CPP([#include <mnttab.h>],
      nfsd_cv_func_mountlist=fread mounted=1)
    fi
    if test -z "$mounted"; then
      nfsd_cv_func_mountlist=unknown
    fi
  ]) dnl
  AC_MSG_RESULT($nfsd_cv_func_mountlist)
  case $nfsd_cv_mountlist in
    fread_fstyp) AC_DEFINE(MOUNTED_FREAD_FSTYP);;
    getfsstat)	 AC_DEFINE(MOUNTED_GETFSSTAT);;
    getmnt)	 AC_DEFINE(MOUNTED_GETMNT);;
    getmntent1)	 AC_DEFINE(MOUNTED_GETMNTENT1);;
    getmntent2)	 AC_DEFINE(MOUNTED_GETMNTENT2);;
    getmntinfo)	 AC_DEFINE(MOUNTED_GETMNTINFO);;
    vmount)	 AC_DEFINE(MOUNTED_VMOUNT);;
  esac
])dnl
dnl *********** FS usage *****************
define(AC_FSUSAGE,
  [AC_MSG_CHECKING(how to get filesystem space usage)
  AC_CACHE_VAL(nfsd_cv_func_statfs,
    [space= 
    
    # DEC Alpha running OSF/1
    AC_TRY_RUN([
      #include <sys/types.h>
      #include <sys/mount.h>
      main ()
      {
      struct statfs fsd;
      exit (statfs (".", &fsd, sizeof (struct statfs)));}
    ], nfsd_cv_func_statfs=OSF1 space=1)
  
    # SVR4
    if test -z "$space"; then
      AC_TEST_CPP([#include <sys/statvfs.h>
        #include <sys/fstyp.h>], nfsd_cv_func_statfs=statvfs space=1)
    fi
  
    # AIX
    if test -z "$space"; then
      AC_HEADER_EGREP(f_nlsdirtype, sys/statfs.h, 
        nfsd_cv_func_statfs=statfs2_bsize space=1)
    fi
  
    # SVR3
    if test -z "$space"; then
      AC_TRY_RUN([
        #include <sys/statfs.h>
        main ()
        {
        struct statfs fsd;
        exit (statfs (".", &fsd, sizeof (struct statfs)));}
      ], nfsd_cv_func_statfs=statfs4 space=1)
    fi
  
    # SVR4
    if test -z "$space"; then
      AC_TEST_CPP([#include <sys/statvfs.h>
        #include <sys/fstyp.h>], nfsd_cv_func_statfs=statvfs space=1)
    fi
  
  
    # 4.3BSD
    if test -z "$space"; then
      AC_TEST_CPP([#include <sys/vfs.h>],
        nfsd_cv_func_statfs=statfs2_bsize space=1)
    fi
  
    # 4.4BSD
    if test -z "$space"; then
      AC_HEADER_EGREP(MOUNT_UFS, sys/mount.h,
        nfsd_cv_func_statfs=statfs2_fsize space=1)
    fi
  
    # SVR2
    if test -z "$space"; then
      AC_TEST_CPP([#include <sys/filsys.h>],
        nfsd_cv_func_statfs=read space=1)
    fi
  
    # Ultrix
    if test -z "$space"; then
      AC_TEST_CPP([#include <sys/mount.h>],
        nfsd_cv_func_statfs=statfs2_fs_data space=1)
    fi
  
    if test -z "$space"; then
      nfsd_cv_func_statfs=unknown
    fi
    ]) dnl
    AC_MSG_RESULT($nfsd_cv_func_statfs)
    case $nfsd_cv_func_statfs in
    OSF1)		AC_DEFINE(STATFS_OSF1);;
    read)		AC_DEFINE(STAT_READ);;
    statfs2_bsize)	AC_DEFINE(STAT_STATFS2_BSIZE);;
    statfs2_fs_data)	AC_DEFINE(STAT_STATFS2_FS_DATA);;
    statfs2_fsize)	AC_DEFINE(STAT_STATFS2_FSIZE);;
    statfs4)		AC_DEFINE(STAT_STATFS4);;
    statvfs)		AC_DEFINE(STAT_STATVFS);;
    esac
])dnl
dnl *********** libwrap bug **************
define(AC_LIBWRAP_BUG,
  [if test -f site.mk; then
    . site.mk
  fi
  if test ! -z "$LIBWRAP_DIR"; then
    AC_MSG_CHECKING(for link problem with libwrap.a)
    AC_CACHE_VAL(nfsd_cv_lib_wrap_bug,
      [OLDLIBS=$LIBS
      LIBS="$LIBS $LIBWRAP_DIR $LIBWRAP_LIB"
      AC_TRY_LINK([
        extern int deny_severity;
      ],[
        deny_severity=1;
      ], nfsd_cv_lib_wrap_bug=no, nfsd_cv_lib_wrap_bug=yes)
    ]) dnl
    AC_MSG_RESULT($nfsd_cv_lib_wrap_bug)
    test $nfsd_cv_lib_wrap_bug = yes && AC_DEFINE(HAVE_LIBWRAP_BUG)
    LIBS=$OLDLIBS
  fi
])dnl
dnl *********** sizeof(dev_t) **************
dnl ** We have to kludge this rather than use AC_CHECK_SIZEOF because
dnl ** we have to include sys/types.h. Ugh.
define(AC_DEV_T_SIZE,
  [AC_MSG_CHECKING(size of dev_t)
   AC_CACHE_VAL(ac_cv_sizeof_dev_t,
   [AC_TRY_RUN(
    [#include <stdio.h>
     #include <sys/types.h>
     main()
     {
      FILE *f=fopen("conftestval", "w");
      if (!f) exit(1);
      fprintf(f, "%d\n", sizeof(dev_t));
      exit(0);
    }], ac_cv_sizeof_dev_t=`cat conftestval`, ac_cv_sizeof_dev_t=0)])
    AC_MSG_RESULT($ac_cv_sizeof_dev_t)
    AC_DEFINE(SIZEOF_DEV_T,$ac_cv_sizeof_dev_t)
  ])
