
/* XXX */
#undef CRAY_STACKSEG_END

/* Define if your rpcgen has the -C option to generate ANSI C.  */
#undef HAVE_RPCGEN_C

/* Define if your rpcgen has the -I option to generate servers
   that can be started from a port monitor like inetd or the listener.  */
#undef HAVE_RPCGEN_I

/* Define if your rpc/xdr.h declares the xdrproc_t type. */
#undef HAVE_XDRPROC_T

/* Define if your system uses the OSF/1 method of getting the mount list.  */
#undef MOUNTED_GETFSSTAT

/* Define if your system uses the SysVr4 method of getting the mount list.  */
#undef MOUNTED_GETMNTENT2

/* Define if your system uses the AIX method of getting the mount list.  */
#undef MOUNTED_VMOUNT

/* Define if your system uses the SysVr3 method of getting the mount list.  */
#undef MOUNTED_FREAD_FSTYP

/* Define if your system uses the BSD4.3 method of getting the mount list.  */
#undef MOUNTED_GETMNTENT1

/* Define if your system uses the BSD4.4 method of getting the mount list.  */
#undef MOUNTED_GETMNTINFO

/* Define if your system uses the Ultrix method of getting the mount list.  */
#undef MOUNTED_GETMNT

/* Define if your system uses the SysVr2 method of getting the mount list.  */
#undef MOUNTED_FREAD

/* Define if your system uses the OSF/1 method of getting fs usage.  */
#undef STATFS_OSF1

/* Define if your system uses the SysVr2 method of getting fs usage.  */
#undef STAT_READ

/* Define if your system uses the BSD4.3 method of getting fs usage.  */
#undef STAT_STATFS2_BSIZE

/* Define if your system uses the BSD4.4 method of getting fs usage.  */
#undef STAT_STATFS2_FSIZE

/* Define if your system uses the Ultrix method of getting fs usage.  */
#undef STAT_STATFS2_FS_DATA

/* Define if your system uses the SysVr3 method of getting fs usage.  */
#undef STAT_STATFS4

/* Define if your system uses the SysVr4 method of getting fs usage.  */
#undef STAT_STATVFS

/* Define if you're using libwrap.a to protect ugidd, and libwrap.a
 * gives you `undefined symbol: deny_severity' when linking.		*/
#undef HAVE_LIBWRAP_BUG

/* Hack. */
#undef AC_SIZEOF_DEV_T
