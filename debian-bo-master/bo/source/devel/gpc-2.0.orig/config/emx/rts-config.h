/* Define if you don't have vprintf but do have _doprnt.  */
#ifndef HAVE_DOPRNT
/* #undef HAVE_DOPRNT */
#endif

/* Define if your struct stat has st_rdev.  */
#ifndef HAVE_ST_RDEV
#define HAVE_ST_RDEV 1
#endif

/* Define if you have the vprintf function.  */
#ifndef HAVE_VPRINTF
#define HAVE_VPRINTF 1
#endif

/* Define if you have the ANSI C header files.  */
#ifndef STDC_HEADERS
#define STDC_HEADERS 1
#endif

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#ifndef TIME_WITH_SYS_TIME
#define TIME_WITH_SYS_TIME 1
#endif

/* Define if your <sys/time.h> declares struct tm.  */
#ifndef TM_IN_SYS_TIME
/* #undef TM_IN_SYS_TIME */
#endif

/* Define if you have the ftruncate function.  */
#ifndef HAVE_FTRUNCATE
#define HAVE_FTRUNCATE 1
#endif

/* Define if you have the gettimeofday function.  */
#ifndef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY 1
#endif

/* Define if you have the setlinebuf function.  */
#ifdef HAVE_SETLINEBUF
#undef HAVE_SETLINEBUF
#endif

/* Define if you have the strcasecmp function.  */
#ifdef HAVE_STRCASECMP
#undef HAVE_STRCASECMP
#endif

/* Define if you have the <fcntl.h> header file.  */
#ifndef HAVE_FCNTL_H
#define HAVE_FCNTL_H 1
#endif

/* Define if you have the <sys/file.h> header file.  */
#ifndef HAVE_SYS_FILE_H
#define HAVE_SYS_FILE_H 1
#endif

/* Define if you have the <sys/time.h> header file.  */
#ifndef HAVE_SYS_TIME_H
#define HAVE_SYS_TIME_H 1
#endif

/* Define if you have the <unistd.h> header file.  */
#ifndef HAVE_UNISTD_H
#define HAVE_UNISTD_H 1
#endif

/* Define if you have BSD style signals. */
/* #undef HAVE_SIGSYS */

#ifdef HAVE_DEVNULL
#undef HAVE_DEVNULL
#endif
#define NO_DEVNULL

#define NULL_DEVICE_NAME "NUL"
