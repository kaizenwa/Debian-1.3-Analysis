/*  VER 073  TAB P   $Id: common.h,v 1.11 1996/11/22 12:31:52 src Exp $
 *
 *  common header file
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "version.h"
#include "tune.h"

/* standard stuff */
#include <stdio.h>
#include <sys/types.h>
#include <ctype.h>

#if STDC_HEADERS
  #include <stdlib.h>
  #include <string.h>
#else 
  #if HAVE_STRINGS_H
    #include <strings.h>
  #endif
#endif

#if HAVE_UNISTD_H
  #include <unistd.h>
#endif

#if HAVE_LIMITS_H
  #include <limits.h>
#endif

#if TIME_WITH_SYS_TIME
  #include <time.h>
  #include <sys/time.h>
#else
  #if HAVE_SYS_TIME_H
    #include <sys/time.h>
  #else
    #include <time.h>
  #endif
#endif

/*
 *  socket
 */
#include "sock.h"

/* 
 *  names
 */
char *hostname; 		/* current NNTP server */
char *spoolname;		/* outgoing spool */
char *hostport; 		/* port for NNTP server */
char *pname;			/* this program */
char *spooldir; 		/* news spool */

/* 
 *  statistics, article counters
 */
int duplicate_articles; 	/* duplicate articles */
int missing_articles;		/* missing articles */
int posted_articles;		/* new articles */
long fetched_articles;		/* number of articles fetched */
long fetched_groups;		/* number of groups fetched */
int fetch_aborted;		/* fetch aborted, usually due to timeout */
long unseen_groups;		/* groups not seen before */
long unavailable_groups;	/* groups not on server */
long already_articles;		/* number of articles already read */
long history_articles;		/* number of articles in history database */
long net_bytecount;		/* total size of articles */
long gross_bytecount;		/* total size of NNTP traffic */

/* 
 *  options 
 */
int debug_flag;
int noaction_flag;
int nomsgid_flag;
int no_id_load_flag;
int inn_flag;
int mode_reader_flag;
int keep_old_flag;
int keep_path_flag;	    /* -k */
char *ai_username;	    /* -a username/password */
char *ai_password;
char *logfile;		    /* -l logfile */
char *folder;		    /* -f folder */
char *end_tag;		    /* -e end_tag */
int nopull_opt; 	    /* -g */
int nohist_opt; 	    /* -h */
int nopost_opt; 	    /* -p */
int timeout;		    /* -t n */
int noforce_flag;
int minspool;		    /* -b n */
char *connect_exec;	    /* -x cmd */
char *via_exec; 	    /* -y cmd */
int zap_flag;		    /* -z */
char *chat_file;	    /* -w chat */

/* 
 *  misc
 */
int post_allowed;

/* 
 *  log message types 
 */
#define L_ERR	 '?'
#define L_ERRno  '!'
#define L_INFO	 '&'
#define L_DEBUG  '='
#define L_DEBUG3 '3'
#define L_GET	 '<'
#define L_PUT	 '>'
