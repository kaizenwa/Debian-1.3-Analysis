/* include/defs.h.  Generated automatically by configure.  */
/* include/defs.h.in.  Generated automatically from configure.in by autoheader.  */

/* Define if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
/* #undef _ALL_SOURCE */
#endif

/* define this if you have gloh.h */
/* #undef HAVE_GLOB */

/* Undefine this if you need strtoul() */
/* #undef NEED_STRTOUL */

/* Define if the `getpgrp' function takes no argument.  */
#define GETPGRP_VOID 1

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef gid_t */

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#define HAVE_SYS_WAIT_H 1

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef mode_t */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define if the system does not provide POSIX.1 features except
   with this defined.  */
/* #undef _POSIX_1_SOURCE */

/* Define if you need to in order for stat and other things to work.  */
/* #undef _POSIX_SOURCE */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
/* #undef size_t */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define on System V Release 4.  */
/* #undef SVR4 */

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef uid_t */

/* define if allow sys/time.h with time.h */
#define TIME_WITH_SYS_TIME 1

/* define this if you are using BSD wait union thigs */
/* #undef BSDWAIT */

/* Define this if you have SUN_LEN in <sys/un.h> */
/* #undef HAVE_SUN_LEN */

/* define this if you are using -lcurses */
/* #undef USING_CURSES */

/* define this if you are using system V (unreliable) signals */
/* #undef SYSVSIGNALS */

/* define this if wait3() is declared */
/* #undef WAIT3_DECLARED */

/* define this if waitpid() is declared */
#define WAITPID_DECLARED 1

/* define this if waitpid() is unavailable */
/* #undef NEED_WAITPID */

/* define this if you have scandir() */
#define HAVE_SCANDIR 1

/* define this if you have memmove() */
#define HAVE_MEMMOVE 1

/* define this if you have setsid() */
#define HAVE_SETSID 1

/* define this if you have getpgid() */
/* #undef HAVE_GETPGID */

/* define this if you need getcwd() */
/* #undef NEED_GETCWD */ 

/* define this if you have hpux version 7 */
/* #undef HPUX7 */

/* define this if you have hpux version 8 */
/* #undef HPUX8 */

/* define this if you have an unknown hpux version (pre ver 7) */
/* #undef HPUXUNKNOWN */

/* define this if you have Ultrix */
/* #undef ULTRIX */

/* define this if an unsigned long is 32 bits */
/* #undef UNSIGNED_LONG32 */

/* define this if an unsigned int is 32 bits */
#define UNSIGNED_INT32 1

/* define this if you are unsure what is is 32 bits */
/* #undef UNKNOWN_32INT */

/* define this if you are on a svr4 derivative */
/* #undef SVR4 */

/* define this if you are on solaris 2.x */
/* #undef __solaris__ */

/* define this if you don't have struct linger */
/* #undef NO_STRUCT_LINGER */

/* define this if you are on svr3/twg */
/* #undef WINS */

/* define this if you need fchmod */
/* #undef NEED_FCHMOD */

/* define this to the location of normal unix mail */
#define UNIX_MAIL "/var/spool/mail"

/* define this if your header files declare sys_errlist */
#define SYS_ERRLIST_DECLARED 1

/* define this if you have uname(2) */
#define HAVE_UNAME 1

/* define this if you need strerror(3) */
/* #undef NEED_STRERROR */

/* define this if you have getpass */
#define HAVE_GETPASS 1

/* define this if you have gettimeofday */
#define HAVE_GETTIMEOFDAY 1

/* define this if you have BSDgettimeofday */
/* #undef HAVE_BSDGETTIMEOFDAY */

/* define this if you have sysconf */
#define HAVE_SYSCONF 1

/* The number of bytes in a unsigned int.  */
#define SIZEOF_UNSIGNED_INT 4

/* The number of bytes in a unsigned long.  */
/* #undef SIZEOF_UNSIGNED_LONG */

/* Define if you have the sigaction function.  */
#define HAVE_SIGACTION 1

/* Define if you have the signal function.  */
/* #undef HAVE_SIGNAL */

/* Define if you have the sigset function.  */
/* #undef HAVE_SIGSET */

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if you have the <limits.h> header file.  */
#define HAVE_LIMITS_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <netdb.h> header file.  */
#define HAVE_NETDB_H 1

/* Define if you have the <stdarg.h> header file.  */
#define HAVE_STDARG_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/fcntl.h> header file.  */
#define HAVE_SYS_FCNTL_H 1

/* Define if you have the <sys/file.h> header file.  */
#define HAVE_SYS_FILE_H 1

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/ptem.h> header file.  */
/* #undef HAVE_SYS_PTEM_H */

/* Define if you have the <sys/select.h> header file.  */
/* #undef HAVE_SYS_SELECT_H */

/* Define if you have the <sys/syslimits.h> header file.  */
/* #undef HAVE_SYS_SYSLIMITS_H */

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <sys/twg_config.h> header file.  */
/* #undef HAVE_SYS_TWG_CONFIG_H */

/* Define if you have the <sys/un.h> header file.  */
#define HAVE_SYS_UN_H 1

/* Define if you have the <sys/wait.h> header file.  */
#define HAVE_SYS_WAIT_H 1

/* Define if you have the <termcap.h> header file.  */
#define HAVE_TERMCAP_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the sun library (-lsun).  */
/* #undef HAVE_LIBSUN */

/* define this if you have (working) POSIX (O_NONBLOCK) non-blocking */
#define NBLOCK_POSIX 1

/* define this if you have BSD (O_NDELAY) non-blocking */
/* #undef NBLOCK_BSD */

/* define this if you have SYSV (FIONBIO) non-blocking */
/* #undef NBLOCK_SYSV */

/* certain broken systems need to define this */
/* #undef NEED_OSPEED */

/* #undef HAVE_BSDGETTIMEOFDAY */
/* #undef GETTOD_NOT_DECLARED */
#define HAVE_GETTIMEOFDAY 1
#define HAVE_GETRUSAGE 1
#define HAVE_DLLIB 1


/* Define this if compiling with SOCKS (the firewall traversal library).
   Also, you must define connect, getsockname, bind, accept, listen, and
   select to their R-versions. */
/* #undef SOCKS */
/* #undef connect */
/* #undef getsockname */
/* #undef bind */
/* #undef accept */
/* #undef listen */
/* #undef select */

/* #undef DEFAULT_SERVER */
