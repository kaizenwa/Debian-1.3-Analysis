/*  VER 017  TAB P   $Id: news.h,v 1.7 1996/11/22 12:31:52 src Exp $
 *
 *  news system filename "common knowlegde"
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 *
 *  NOTE: shouldn't normally need to change these
 */

/* 
 *  common 
 */
#define HISTORY_FILE	"/history"
#define ACTIVE_FILE	"/active"
#define C_SYS_FILE	"/sys"		/* Cnews */
#define I_NEWSFEEDS	"/newsfeeds"	/* Inn */

/* 
 *  outgoing  
 */
#define OUT_GOING	"/out.going/"
#define C_TOGO		"/togo" 	/* Cnews */
#define C_LOCK		"LOCKb" 	/* Cnews */
#define I_LOCK		"LOCK." 	/* Inn */

/* 
 *  incoming
 */
#define IN_COMING	"/in.coming/"
#define IN_LATEST	"/in.hosts/"	

#define _IN_TMP 	".tmp." 	/* temp prefix, invisible to newsrun */
#define _INSPOOL	"%ld.%d.%d.t"	/* visible file, uncompressed */

/* 
 *  general
 */
#define _TMP		".tmp"
#define _OLD		".old"

