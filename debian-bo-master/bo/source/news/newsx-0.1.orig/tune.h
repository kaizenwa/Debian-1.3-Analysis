/*  TAB P   VER 027   $Id: tune.h,v 1.5 1996/11/22 12:31:52 src Exp $
 *
 *  configuration
 *  tune as required
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

/*
 *  modify "ps" status?
 */
#define PS_STATUS 1	 

/*
 *  timeouts
 */
#define TIMEOUT     600     /* for TCP/IP operations: 10 minutes */
#define LOCKTIME    600     /* for lockfile: 10 minutes */
#define LOCKDELTA     5     /* poll cycle: 5 seconds */

/*
 *  guideline for size of incoming spool
 */
#define MINSPOOL   100000   /* 100 kbytes */


