/*
 * pathnames	This file contains the definitions of the path names used
 *		by the NET base distribution.  Do not change the values!
 *
 * Version:	@(#)pathnames.h	1.30	09/06/93
 *
 * Author:	Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
 */

/* Pathnames of base-level NET programs. */
#define _PATH_BIN_IFCONFIG	"/sbin/ifconfig"
#define _PATH_BIN_ROUTE		"/sbin/route"
#define _PATH_BIN_PPP		"/usr/sbin/pppd"
#define _PATH_BIN_TERM		"/usr/local/bin/term"
#define _PATH_BIN_IPFW          "/sbin/ipfw"

/* Pathnames of some customizable files. */
#define _PATH_ETC_DIPHOSTS	"/etc/diphosts"
#define _PATH_DIP_PID		"/etc/dip.pid"

#ifdef FSSTND
#define _PATH_LOCKD		"/var/lock"	        /* lock files	*/
#else
#define _PATH_LOCKD		"/usr/spool/uucp"	/* lock files	*/
#endif

/* This one is outdated - now locks are owned by whoever invoked DIP */
#define _UID_UUCP		"uucp"			/* owns locks	*/

/* End of pathnames.h */
