/***************************************************************************
 * LPRng - An Extended Print Spooler System
 *
 * Copyright 1988-1995 Patrick Powell, San Diego State University
 *     papowell@sdsu.edu
 * See LICENSE for conditions of use.
 *
 ***************************************************************************
 * MODULE: portable.h
 * PURPOSE:
 * The configure program generates config.h,  which defines various
 * macros indicating the presence or abscence of include files, etc.
 * However, there are some systems which pass the tests,  but things
 * do not work correctly on them.  This file will try and fix
 * these things up for the user.
 *
 * NOTE:  if there were no problems, this file would be:
 *    #include "config.h"
 *
 * Sigh. Patrick Powell Thu Apr  6 07:00:48 PDT 1995 <papowell@sdsu.edu>
 *    NOTE: thanks to all the folks who worked on the PLP software,
 *    Justin Mason <jmason@iona.ie> especially.  Some of the things
 *    that you have to do to get portability are truely bizzare.
 *
 * $Id: portable.h,v 1.2 1996/11/14 19:56:32 papowell Exp papowell $
 **************************************************************************/

#ifndef _PLP_PORTABLE_H
#define _PLP_PORTABLE_H 1

#ifndef __STDC__
LPR Lite requires ANSI Standard C to compile
#endif

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/*************************************************************************
 * ARGH: some things that "configure" can't get right.
 *************************************************************************/

/***************************************************************************
 * porting note: if you port PLP and you get some errors
 * caused by autoconf guessing the wrong set of functions/headers/structs,
 * add or change the entry for your system in the ARGH section below.
 * You might want to try and determine how your system is identified
 * by the C preprocessor and use this informaton rather than trying
 * to look for information in various f1les.
 *    Patrick Powell and Justin Mason
 ***************************************************************************/

/*************************************************************************
 * APOLLO Ports
 *  Thu Apr  6 07:01:51 PDT 1995 Patrick Powell
 * This appears to be historical.
 *************************************************************************/
#ifdef apollo
# define IS_APOLLO
/* #undef __STDC__ */
/* # define CONFLICTING_PROTOS */
#endif

/*************************************************************************
 * ULTRIX.
 * Patrick Powell Thu Apr  6 07:17:34 PDT 1995
 * 
 * Take a chance on using the standard calls
 *************************************************************************/
#ifdef ultrix
# define IS_ULTRIX
#endif


/*************************************************************************
 * AIX.
 *************************************************************************/
#ifdef _AIX32 
# define IS_AIX32
#endif

/*************************************************************************
 * Sun
 *************************************************************************/

#if defined(sun)
#endif


/*************************************************************************/
#if defined(NeXT)
# define IS_NEXT
# define __STRICT_BSD__
#endif

/*************************************************************************/
#if defined(__sgi) && defined(_SYSTYPE_SVR4)
# define IS_IRIX5
#endif

/*************************************************************************/
#if defined(__sgi) && defined(_SYSTYPE_SYSV)
#define IS_IRIX4
#endif

/*************************************************************************/
#if defined(__linux__) || defined (__linux) || defined (LINUX)
# define IS_LINUX
#endif


/*************************************************************************/
#if defined(__hpux) || defined(_HPUX_SOURCE)
# define IS_HPUX
# undef _HPUX_SOURCE
# define _HPUX_SOURCE 1
#endif
  
/*************************************************************************/

#if defined(__convex__) /* Convex OS 11.0 - from w_stef */
# define IS_CONVEX
# define LPASS8 (L004000>>16)
#endif

/*************************************************************************/

#ifdef _AUX_SOURCE
# define IS_AUX
# define _POSIX_SOURCE
#endif

/*************************************************************************/

#if defined(SNI) && defined(sinix)
# define IS_SINIX
#endif


/*************************************************************************/
#if defined(__svr4__) && !defined(SVR4)
# define SVR4 __svr4__
#endif

/***************************************************************************
 * Solaris SUNWorks CC compiler
 *  man page indicates __SVR4 is defined, as is __unix, __sun
 ***************************************************************************/
#if (defined(__SVR4) || defined(_SVR4_)) && !defined(SVR4)
# define SVR4 1
#endif


/*************************************************************************
 * we also need some way of spotting IS_DATAGEN (Data Generals),
 * and IS_SEQUENT (Sequent machines). Any suggestions?
 * these ports probably don't work anymore...
 *************************************************************************/


/*********************************************************************
 * GET STANDARD INCLUDE FILES
 * This is the one-size-fits-all include that should grab everthing.
 * This has a horrible impact on compilation speed,  but then, do you
 * want compilation speed or portability?
 *
 * Patrick Powell Thu Apr  6 07:21:10 PDT 1995
 *********************************************************************
 * If you do not have the following, you are doomed. Or at least
 * going to have an uphill hard time.
 * NOTE: string.h might also be strings.h on some very very odd systems
 *
 * Patrick Powell Thu Apr  6 07:21:10 PDT 1995
 *********************************************************************/

#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <sys/wait.h>
#include <string.h>
#include <sys/stat.h>
#ifdef HAVE_TIME_H
# include <time.h>
#endif
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

/*********************************************************************
 * yuck -- this is a nightmare! half-baked-ANSI systems are poxy (jm)
 *
 * Note that configure checks for absolute compliance, i.e.-
 * older versions of SUNOS, HP-UX, do not meet this.
 *
 * Patrick Powell Thu Apr  6 07:21:10 PDT 1995
 *********************************************************************/


#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
  extern int dup2 ();
  extern int execve ();
  extern uid_t geteuid (), getegid ();
  extern int setgid (), getgid ();
#endif


#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#else
  char *getenv( char * );
  void abort(void);
#endif

#ifndef HAVE_STRCHR
# define strchr			index
# define strrchr		rindex
#endif

#ifndef HAVE_ERRNO_DECL
 extern int errno;
#endif

#ifdef HAVE_SYS_FCNTL_H
# include <sys/fcntl.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif

#ifdef HAVE_MEMORY_H
# include <memory.h>
#else
  extern char *memchr();
#endif

/*********************************************************************
 * Note the <time.h> may already be included by some previous
 * lines.  You may need to edit this by hand.
 * Better solution is to put include guards in all of the include files.
 * Patrick Powell Thu Apr  6 07:55:58 PDT 1995
 *********************************************************************/
 
#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#else
# include <time.h>
# endif
#endif

/* varargs declarations: */

#if defined(HAVE_STDARG_H)
# include <stdarg.h>
# define HAVE_STDARGS    /* let's hope that works everywhere (mj) */
# define VA_LOCAL_DECL   va_list ap;
# define VA_START(f)     va_start(ap, f)
# define VA_SHIFT(v,t)	;	/* no-op for ANSI */
# define VA_END          va_end(ap)
#else
# if defined(HAVE_VARARGS_H)
#  include <varargs.h>
#  undef HAVE_STDARGS
#  define VA_LOCAL_DECL   va_list ap;
#  define VA_START(f)     va_start(ap)		/* f is ignored! */
#  define VA_SHIFT(v,t)	v = va_arg(ap,t)
#  define VA_END		va_end(ap)
# else
XX ** NO VARARGS ** XX
# endif
#endif

typedef RETSIGTYPE plp_signal_t;

/*************************
 * STTY functions to use *
 *************************/
#define SGTTYB  0
#define TERMIO  1
#define TERMIOS 2


/**********************************************************************
 *  SUNOS Definitions
 **********************************************************************/
#ifdef SUNOS
extern int _flsbuf(int, FILE *);
extern int _filbuf(FILE *);
extern int accept(int s, struct sockaddr *name, int *namelen);
extern int bind(int s, struct sockaddr *name, int namelen);
extern int connect(int s, struct sockaddr *name, int namelen);
extern void bzero(void *s, size_t n);
extern void endgrent( void );
extern int fflush( FILE *stream );
extern int fclose( FILE *stream );
extern int flock( int fd, int operation );
extern int fprintf(FILE *, const char *, ...);
extern int fputs(const char *, FILE *);
extern int fread (char *ptr, int size, int nitems, FILE*stream);
extern int fstat(int fd, struct stat *buf );
extern int ftruncate( int fd, off_t length );
extern int getdtablesize( void );
extern int getpeername(int s, struct sockaddr *name, int *namelen);
extern int getsockname(int s, struct sockaddr *name, int *namelen);
extern int getsockopt(int s, int level, int optname, char *optval,int *optlen);
extern int ioctl(int fd, int request, caddr_t arg );
extern int killpg(int pgrp, int sig );
extern int listen(int s, int backlog );
extern int lockf(int fd, int cmd, long size );
extern int lstat(const char *path, struct stat *buf );
extern int mkstemp(char *s );
extern int openlog( const char *ident, int logopt, int facility );
extern int rename(const char *, const char *);
extern int select (int width, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout);
extern void setgrent(void);
extern int setreuid( int ruid, int euid );
extern int setsockopt(int s, int level, int optname, const char *optval,int optlen);
extern int socket( int domain, int type, int protocol );
extern int socketpair(int, int, int, int *);
extern int stat(const char *path, struct stat *buf );
extern int strcasecmp( const char *, const char * );
extern int strncasecmp( const char *, const char *, int n );
extern int long strtol( char *str, char **ptr, int base );
extern int strftime(char *buf, int bufsize, const char *fmt, struct tm *tm);
extern void syslog(int, const char *, ...);
extern int system( const char *str );
extern int tgetent( char *buffer, char *name );
extern time_t time( time_t *t );
extern int tolower( int );
extern int toupper( int );
extern void tputs( const char *cp, int affcnt, int (*outc)() );
extern int vfprintf(FILE *, const char *, ...);
extern int vprintf(FILE *, const char *, va_list ap);
#endif

#endif	/* PLP_PORTABLE_H */
