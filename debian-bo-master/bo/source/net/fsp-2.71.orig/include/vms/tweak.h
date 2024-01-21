/* include/tweak.h. VMS version */
#ifndef _FSP_TWEAK_H_
#define _FSP_TWEAK_H_

/* redefine main() for executables */
#ifndef MERGE
#define main vms_main
#endif

/* define this if your system allows file names > 14 characters */
#define HAVE_LONG_FILE_NAMES 1

/* Define if you have dirent.h.  */
#define DIRENT 1

/* Define if you don't have vprintf but do have _doprnt.  */
#undef HAVE_DOPRNT

/* Define if you have unistd.h.  */
#undef HAVE_UNISTD_H

/* Define this if you have tzfile.h */
#undef HAVE_TZFILE_H

/* Define if you have the vprintf function.  */
#define HAVE_VPRINTF 1

/* Define this if your compiler understands ANSI prototyping */
#define HAVE_ANSI_PROTO 1

/* Define if you have the ANSI C header files. (ansi strings, ctype, etc.) */
#define STDC_HEADERS 1

/* Define this if you are running on an AIX system. */
#undef _ALL_SOURCE

/* Define this if you have posix compliance
#undef _POSIX_SOURCE

/* Define if you don't have dirent.h, but have sys/dir.h.  */
#undef SYSDIR

/* Define if you don't have dirent.h, but have sys/ndir.h.  */
#undef SYSNDIR

/* Define if the closedir function returns void instead of int.  */
#define VOID_CLOSEDIR 1

/* Define HAVE_D_INO if you have d_ino in dirent (or direct) struct */
/* see /usr/include/dir.h or ndir.h or dirent.h */
#undef HAVE_D_INO

/* Define HAVE_D_FILENO if you have d_fileno in dirent (or direct) struct */
/* see /usr/include/dir.h or ndir.h or dirent.h */
#undef HAVE_D_FILENO

/* Define to empty if the keyword does not work.  */
#undef const

/* Define the following if your site has a funky bsearch */
/* #define bsearch ansi_bsearch
 */

/* define the following if your site doesn't define 'strdup()' */
#define NEED_STRDUP 1

/* define the following if your site doesn't define 'random()' */
#define NEED_RANDOM 1

/* define the following if your site doesn't define 'bcopy()' */
#undef NEED_BCOPY

/* Define the following if your site has lockf() */
#undef HAVE_LOCKF

/* Define the following if you site has shared memory */
#undef HAVE_SHMEM

/* Define the following if your system has flock() */
#undef HAVE_FLOCK

/* define this if WEXITSTATUS is defined on your system */
#define HAVE_WEXITSTATUS 1

/* define this if S_ISREG is defined on your system */
#undef HAVE_S_ISREG

/* define this if S_ISDIR is defined on your system */
#undef HAVE_S_ISDIR

/* define this if you have 'union wait' in <sys/wait.h> */
#define HAVE_UNION_WAIT 1

/* define this if you have SECSPERDAY */
#undef HAVE_SECSPERDAY

/* define this if you have DAYSPERNYEAR */
#undef HAVE_DAYSPERNYEAR

/* define this if your have strings.h */
#undef HAVE_STRINGS_H

/* define this if your have string.h */
#define HAVE_STRING_H 1

/* define this if you have memory.h */
#undef HAVE_MEMORY_H

/* define this if your string.h has bogus type declarations for a few things */
#undef STRING_H_BOGUS

/* These two will be automatically set by the configure script if needed */
#define USE_SECSPERDAY (long)60*60*24
#define USE_DAYSPERNYEAR 365

#ifndef HAVE_WEXITSTATUS
#define WEXITSTATUS(x) ((x).w_T.w_Retcode)
#endif

#ifndef HAVE_S_ISREG
#define S_ISREG(mode) ((mode) & S_IFREG)
#endif

#ifndef HAVE_S_ISREG
#define S_ISDIR(mode) ((mode) & S_IFDIR)
#endif

#if defined(DIRENT)
#define HAVE_STRUCT_DIRENT
#else
#undef HAVE_STRUCT_DIRENT
#endif

#ifndef HAVE_SECSPERDAY
#define SECSPERDAY USE_SECSPERDAY
#endif

#ifndef HAVE_DAYSPERNYEAR
#define DAYSPERNYEAR USE_DAYSPERNYEAR
#endif

#if defined(HAVE_D_INO) && !defined(HAVE_D_FILENO)
#define d_fileno d_ino
#else
#if !defined(HAVE_D_INO) && defined(HAVE_D_FILENO)
#define d_ino d_fileno
#endif
#endif

#define WORD_TYPE_2 unsigned short
#define WORD_TYPE_4 unsigned long

#define BB_READ4(V) ntohl(*(WORD_TYPE_4 *)(V))
#define BB_WRITE4(V,A) *(WORD_TYPE_4 *)(V) = htonl(A)
#define BB_READ2(V) ntohs(*(WORD_TYPE_2 *)(V))
#define BB_WRITE2(V,A) *(WORD_TYPE_2 *)(V) = htons(A)

#include "proto.h"

#endif /* _FSP_TWEAK_H_ */
