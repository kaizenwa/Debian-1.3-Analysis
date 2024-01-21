/* @(#) exitcodes.h,v 1.4 1992/08/08 17:33:25 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * exitcodes:
 *
 * 	"standard" exit code values for sites that do not have
 * 	a <sysexits.h> include file
 */

#ifndef HAVE_SYSEXITS

/*
 * use the default values for exit codes unless somebody want's to
 * override
 */
# ifndef EX_BASE
#  define EX_BASE 64
# endif	/* EX_BASE */

# undef EX_OK				/* remove EX_OK defined in SVR4.2 */
# define EX_OK		0		/* successful termination */
# define EX_USAGE	(EX_BASE+0)	/* command line usage error */
# define EX_DATAERR	(EX_BASE+1)	/* data format error */
# define EX_NOINPUT	(EX_BASE+2)	/* cannot open input */
# define EX_NOUSER	(EX_BASE+3)	/* addressee unknown */
# define EX_NOHOST	(EX_BASE+4)	/* host name unknown */
# define EX_UNAVAILABLE	(EX_BASE+5)	/* service unavailable */
# define EX_SOFTWARE	(EX_BASE+6)	/* internal software error */
# define EX_OSERR	(EX_BASE+7)	/* system error */
# define EX_OSFILE	(EX_BASE+8)	/* critical OS file missing */
# define EX_CANTCREAT	(EX_BASE+9)	/* can't create (user) output file */
# define EX_IOERR	(EX_BASE+10)	/* error in file i/o */
# define EX_TEMPFAIL	(EX_BASE+11)	/* temp failure; user can retry */
# define EX_PROTOCOL	(EX_BASE+12)	/* remote error in protocol */
# define EX_NOPERM	(EX_BASE+13)	/* permission denied */
#else	/* HAVE_SYSEXITS */

# include <sysexits.h>

#endif	/* HAVE_SYSEXITS */
