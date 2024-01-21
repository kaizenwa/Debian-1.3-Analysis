/*
 * (c) Copyright 1992 by Panagiotis Tsirigotis
 * All rights reserved.  The file named COPYRIGHT specifies the terms 
 * and conditions for redistribution.
 */

#ifndef LOG_H
#define LOG_H

/*
 * $Id: log.h,v 1.1 1996/01/24 19:29:19 chuck Exp $
 */

/*
 * Meaning of logtype flags:
 *
 *    L_NONE:           no logging
 *    L_FILE:           log output goes to a file
 *    L_SYSLOG:         log output goes to syslog(3)
 *    L_COMMON_FILE:    log output goes to the file specified in defaults
 */
typedef enum { L_NONE = 0, L_FILE, L_SYSLOG, L_COMMON_FILE } logtype_e ;

struct filelog
{
   char		*fl_filename ;						/* always malloc'ed		*/
	unsigned fl_soft_limit ;
	unsigned fl_hard_limit ;
} ;

#define FILELOG_SIZE_CONTROL( flp )		( flp->fl_soft_limit != 0 )	


struct syslog
{
   int sl_facility ;
	int sl_level ;
} ;

struct log
{
	logtype_e		l_type ;
	struct filelog l_fl ;
	struct syslog	l_sl ;
} ;

#define log_get_type( lp )					(lp)->l_type
#define log_set_type( lp, type )			(lp)->l_type = (type)

#define log_filelog( lp )				(&(lp)->l_fl)
#define log_syslog( lp )				(&(lp)->l_sl)

status_e log_start() ;
void log_end() ;

#endif	/* LOG_H */

