/*
 * File:	config.h
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *              24 Oct 95   Framstag	changed SAFT-port
 *              21 Dec 95   Framstag	new default for nosendfile:
 *					/usr/local/etc/
 *              24 Jan 96   Framstag	added MAIL, CONFIG and DATEFORMAT
 *              28 Mar 96   Framstag    extended search for support programs
 *               2 Apr 96   Framstag    MAXLEN is now 4 KB
 *               3 Apr 96   Framstag    replaced MAIL with SENDMAIL
 *              11 Apr 96   Framstag    added pgp support
 *              30 Apr 96   Framstag    added RESPECT_MAIL_ALIASES
 *              13 Sep 96   Framstag	better SYSV checking
 *
 * Various definitions for the sendfile package.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */

/*
 * The first 3 defines are the directories where sendfile will be installed.
 * Only Makefile will use them.
 */


#define BINDIR		"/usr/local/bin"
#define MANDIR		"/usr/local/man/man1"
#define SERVERDIR	"/usr/local/sbin"

#define NOSENDFILE	"/usr/local/etc/nosendfile"
#define CONFIG		"/usr/local/etc/sendfile.cf"
#define ALIASES		"/usr/local/etc/sendfile.aliases"

#define SPOOL    	"/var/spool/sendfile"
#define INLOG		"/var/spool/sendfile/LOG/in"
#define OUTLOG		"/var/spool/sendfile/LOG/out"

/* uncomment the next lines only if you need special paths for these programs */
/* (recode and pgp are optional) */
/*
#define TAR		"/usr/bin/tar"
#define GZIP		"/usr/bin/gzip"
#define PGP		"/usr/local/bin/pgp"
#define RECODE		"/usr/local/bin/recode"
#define SENDMAIL	"/usr/lib/sendmail"
*/


/* ############ You should not change anything after this line ############ */

#define DATEFORMAT	"%d-%b-%Y %H:%M:%S"	/* see strftime(3) */
#define CHARSET		"ISO_8859-1:1987"	/* this is ISO Latin 1 */
#define VERSION		"1.5"
#define REVISION	"19961011"
#define PROTOCOL 	"SAFT"
#define SAFT	  	487	/* SAFT tcp port */
#define DAYSEC 		86400	/* seconds per day */
#define OVERSIZE  	32768	/* string oversize length */
#define MAXLEN	  	4096	/* max length of various strings */
#define FLEN	  	256	/* file/directory name length */
#define DLEN	  	30	/* date string length */
#define PACKET	  	512	/* data size per packet */
#define DEBUG			/* more debugging output */
#undef  DEBUG
#define ALT_MESSAGES		/* alternative message format */
#undef  ALT_MESSAGES
#define RESPECT_MAIL_ALIASES	/* look for elm aliases, too */
#undef  RESPECT_MAIL_ALIASES

#ifndef PGP
  #define PGP "pgp"
#endif
#ifndef TAR
  #define TAR "tar"
#endif
#ifndef GZIP
  #define GZIP "gzip"
#endif
#ifndef RECODE
  #define RECODE "recode"
#endif
#ifndef SENDMAIL
  #define SENDMAIL "/usr/lib/sendmail"
#endif

#ifdef NEXT
  #ifndef _NEXT_SOURCE
    #define _NEXT_SOURCE
  #endif
#endif

#if defined(IRIX64)
  #define IRIX
#endif

#if defined(SOLARIS1) || defined(CONVEXOS)
  #ifndef BSD
    #define BSD
  #endif
#endif

#if defined(AIX) || defined(LINUX) || defined(HPUX) || defined(OSF1) || defined(IRIX64) || defined(SOLARIS2) || defined(IRIX) || defined(__svr4__) || defined(SVR3)
  #ifndef SYSV
    #define SYSV
  #endif
#endif

/*
#ifdef SYSV
  #define killpg(a, b) kill(-(a), (b))
  #define bcopy(a, b, c) memcpy(b, a, c)
  #define bzero(a, b) memset(a, 0, b)
  #define bcmp memcmp
  #define index strchr
  #define rindex strrchr
  #define initstate srand
  #define random rand
  #define sigtype void
#endif
*/

#ifndef sigtype
  #ifdef NEXT
    #define sigtype void
  #else
    #define sigtype int
  #endif
#endif
