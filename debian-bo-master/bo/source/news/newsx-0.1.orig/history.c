/*  VER 019  TAB P   $Id: history.c,v 0.7 1996/11/22 12:31:52 src Exp $
 *  
 *  history database lookup
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "news.h"

#ifdef HAVE_LIBDBZ
#  include <dbz.h>
#  define HAVE_DB 1
#  define dbminit   dbzdbminit
#  define fetch     dbzdbmfetch
#  define dbmclose  dbzdbmclose
#else
#  ifdef HAVE_LIBDBM
#    include <dbm.h>
#    define HAVE_DB 1
#  endif
#endif

#ifndef HAVE_DB
/* use our local code... */
/* BUG: implement HAVE_xxx that does nothing.. */
#  include "dbz.c"
#  define HAVE_DB 1
#  define dbminit   dbzdbminit
#  define fetch     dbzdbmfetch
#  define dbmclose  dbzdbmclose
#endif

#ifdef HAVE_DB
static int hist_open = 0;

/*
 *  look up history database
 *  will convert message ID to news character case conventions
 *  return true if found there
 */
int history_lookup(char *msgid)
{
    datum key;
    datum val;
    char *p;
    int len;

    /* 
     *	convert host name to lower case
     */
    if (p = strchr(msgid,'@')) {
	while (*++p) *p = tolower(*p);
    }

    if (hist_open <= 0) {
	if (hist_open == 0) {
	    char historyname[PATH_MAX];
	
	    build_filename(historyname,NEWSCTL,HISTORY_FILE,NULL);
	    if (dbminit(historyname) == -1) {
		log_msg(L_ERRno,"cannot open history '%s'",historyname);
		hist_open = -1;
		return 0;
	    }
	    log_msg(L_DEBUG3,"history file %s opened",historyname);
	    hist_open = 1;
	} else {
	    return 0;
	}
    } 

    /* 
     *	look up database 
     */
    len = strlen(msgid);
    key.dptr = msgid;
    key.dsize = len+1;
    
    val = fetch(key);
    
    if (!(val.dptr)) {
	log_msg(L_DEBUG3,"msgid %s not in history",msgid);
	return 0;
    }
    /* OK, we found it */
    return 1;
}

/*
 *  done with history database
 */
void history_done(void)
{
    if (hist_open > 0) {
	dbmclose();
	hist_open = 0; 
    }
}

#else /* HAVE_DB */
int history_lookup(char *msgid) 
{ return 0; }
void history_done(void)
{ }
#endif
