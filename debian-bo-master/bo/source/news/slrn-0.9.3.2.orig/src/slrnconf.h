/* 
 * Use sysconf.h for unix!!!  This file is for NON-unix systems.
 * 
 * This file is used to indicate capabilities of the C compiler and
 * operating system.
 * 
 * See also slrnfeat.h for customization of slrn features.
 */

#define USE_DOMAIN_NAME 0
#define MY_DOMAIN_NAME "foo.bar"

#ifdef VMS
# ifndef MAIL_PROTOCOL
#   if defined(UCX) || defined(MULTINET)
#     define MAIL_PROTOCOL "SMTP%"
#   else
#     define MAIL_PROTOCOL "IN%"
#   endif
# endif
#endif

/* Basic include files. */

#if defined(__os2__) || defined (__DECC) || defined(VAXC)
# define HAVE_STDLIB_H 1
#else
# define HAVE_MALLOC_H 1
#endif

#if defined (__os2__)
# define HAVE_UNISTD_H 1
# define HAVE_MEMORY_H 1
# define HAVE_SYS_SOCKET_H 1
# define HAVE_NETINET_IN_H 1
# define HAVE_ARPA_INET_H 1
# define HAVE_DIRENT_H 1
#endif

/* 
 * Basic C library functions.
 */

#if defined(__os2__)
# define HAVE_PUTENV 1
#endif

#define HAVE_GETCWD 1
#define HAVE_MEMSET 1
#define HAVE_MEMCPY 1
#define HAVE_MEMCHR 1

#define SLRN_SERVER_ID_NNTP 1
#define SLRN_SERVER_ID_SPOOL 2

#define SLRN_POST_ID_NNTP 1
#define SLRN_POST_ID_INEWS 2
#define SLRN_POST_ID_PULL 3

