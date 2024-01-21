    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "server_def.h"
#include "s_extern.h"
#include "version.h"

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef VMS
#define FSP_STAT vms_stat
#else
#define FSP_STAT stat
#endif

#define ACTIONLOG0(X) \
do{ if(!old) { \
  fsplogs(); \
  fsploga("%s %s", inetstr, (X)); \
} } while (0)

#define ACTIONLOG1(X) \
do { if(!old) { \
  fsplogs(); \
  fsploga("%s %-8s /%.*s", inetstr, (X), l1, s1); \
} } while (0)

#define ACTIONINFO(F) \
do { if(!old) { \
  fsploga F; \
} } while (0)

#define ACTIONFAILED(M) \
do { if(!old) { \
  fsploga(": ERROR %s\n", (M)); \
  fsplogf(); \
} } while (0)

#define ACTIONOK() \
do { if(!old) { \
  fsploga("\n"); \
  fsplogf(); \
} } while (0)

#ifdef VMS  /* for the freopen() function */
# define NULL_DEV "nl:"
#else
# define NULL_DEV "/dev/null"
#endif

int max_nlen = FILE_NAME_LIMIT;
int inetd_mode = 0;
int dir_cache_limit = MAX_DIR_CACHE_COUNT;
int logfd = -1;  /* logfile file descriptor */

extern int daemonize;
extern int dbug;
extern int logging;
extern int priv_mode;
extern int read_only;
extern int no_unnamed;
extern int udp_port;
extern int run_uid;
extern char *logname;
extern unsigned int maxthcallowed;

char *config_file = 0 ;

/****************************************************************************
 * A slightly better logging function.. It now takes a format string and    *
 * any number of args.                                                      *
 * The file is opened and closed so that I can run a cron job to summarize  *
 * the file, then remove the already summarized portion automatically       *
 * without worrying about the file descriptors pointing to a file that no   *
 * longer really exists.                                                    *
 ****************************************************************************/
#define LOGBUFFER 1024
static char logbuf[LOGBUFFER];	/* buffer for log message */
static int logpos = 0;		/* current log message length */

/* append some text to the log message */
#ifdef __STDC__
static void fsploga(const char *fmt, ...)
#else
static void fsploga(va_alist)
  va_dcl
#endif
{
#ifdef __STDC__
  va_list args;
#else
  char *fmt;
  va_list args;
#endif

  if(logging && logfd >= 0) { /* don't log if we don't have a logfile */
#ifdef __STDC__
    va_start(args, fmt);
#else
    va_start(args);
    fmt = va_arg(args, char *);
#endif
    vsprintf(logbuf + logpos, fmt, args);
    logpos += strlen(logbuf + logpos);
    va_end(args);
  }
}

/* add a datestamp to the log message */
static void fsplogs PROTO0((void))
{
  struct stat sb;

  if(logging && logfd >= 0 && FSP_STAT(logname, &sb) == -1) {
    close(logfd);
#ifdef VMS
    if((logfd = vms_open(logname, "w")) < 0)
#else
    if((logfd = open(logname, O_WRONLY | O_APPEND | O_CREAT, 0644)) < 0)
#endif
{
perror(logname) ;
      logging = 0;
}
  }
  if(logging && logfd >= 0) {  /* don't log if we don't have a logfile */
    time_t sectime; /* current time in seconds */
    int timelen;
    char *timestr;

    sectime = time((time_t *)0);
    timestr = (char *)ctime(&sectime);
    timelen = strlen(timestr) - 1; /* strip the CR */
    timestr[timelen] = '\0';
    strcpy(logbuf + logpos, timestr);
    logbuf[timelen] = ' ';
    logpos += timelen + 1;
  }
}

/* flush the log message to file */
static void fsplogf PROTO0((void))
{
  if(logging && logfd >= 0) { /* don't log if we don't have a logfile */
    write(logfd, logbuf, logpos);
    logpos = 0;
  }
}

/****************************************************************************
* Send version information.
* Note: no bounds checking is currently performed.  As version information
*       grows, this will become required.
****************************************************************************/
static void server_show_version PROTO2(struct sockaddr_in *, from, UBUF *, ub)
{
  char buf[UBUF_SPACE], verflags = 0;

  strcpy(buf, VERSION_STR);
  strcat(buf, "\n");

  if(logging) verflags |= VER_LOG;
  if (read_only) verflags |= VER_READONLY;
  if (no_unnamed) verflags |= VER_REVNAME;
  if (priv_mode) verflags |= VER_PRIVMODE;
  if (maxthcallowed) verflags |= VER_THRUPUT;

  strcpy(ub->buf, buf);
  BB_WRITE4(ub->bb_pos,VER_BYTES);
  ub->buf[strlen(ub->buf)] = '\0';
  ub->buf[strlen(ub->buf)+1] = verflags;
  if(maxthcallowed) {
    BB_WRITE4(ub->bb_pos,VER_BYTES+4);
    ub->buf[strlen(ub->buf)+2] = (char)((maxthcallowed & 0xff000000)>>24);
    ub->buf[strlen(ub->buf)+3] = (char)((maxthcallowed & 0x00ff0000)>>16);
    ub->buf[strlen(ub->buf)+4] = (char)((maxthcallowed & 0x0000ff00)>>8);
    ub->buf[strlen(ub->buf)+5] = (char)(maxthcallowed & 0x000000ff);
	
    server_reply(from, ub, strlen(ub->buf)+1, VER_BYTES+4);
  } else {
    server_reply(from, ub, strlen(ub->buf)+1, VER_BYTES);
  }
}

/****************************************************************************
*  This is the dispatch loop for message that has been accepted.
*    bytes: size of the message received.
*       ub: pointer to the message buffer.
*      old: true if this message contains old sequence number (retransmit).
*       hp: pointer to the entry for the client host who sent this message.
*     from: pointer to the socket address structure of the client host.
****************************************************************************/

void server_get_packet PROTO5(int, bytes, UBUF *, ub, int, old,
			      HTAB *, hp, struct sockaddr_in *, from)
{
  unsigned long  inet_num, pos;
  unsigned short port_num;
  unsigned l1, l2;
  char *s1, *s2, *pe, inetstr_buf[128], *inetstr;
  FILE *fp;
  PPATH pp;
  struct stat sd; /* for logging of filesize */

  pos = BB_READ4(ub->bb_pos);
  l1  = BB_READ2(ub->bb_len);
  l2 = bytes - l1 - UBUF_HSIZE;
  s1 = ub->buf;
  s2 = ub->buf + l1;

  /* put remote inet_number in a var, for logging purposes */
  if (dbug || logging) {
    if (hp->hostname)
      inetstr = hp->hostname;
    else {
      sprintf(inetstr_buf,"%d.%d.%d.%d",
	      ((unsigned char *)(&hp->inet_num))[0],
	      ((unsigned char *)(&hp->inet_num))[1],
	      ((unsigned char *)(&hp->inet_num))[2],
	      ((unsigned char *)(&hp->inet_num))[3]);
      inetstr = inetstr_buf;
    }
  }

  if(dbug) fprintf(stderr,"rcv (%c,%d,%d,%lu) <--- %s\n", ub->cmd, l1, l2,
		   pos, inetstr);

  if(!old) {
    hp->last_key = hp->next_key;
    hp->next_key = get_next_key() + ((hp->last_key+1) & 0x00ff);
  }

  BB_WRITE2(ub->bb_key,hp->next_key);
  inet_num = hp->inet_num;
  port_num = from->sin_port;

  switch(ub->cmd) {
    case CC_VERSION:
      if(logging & L_VER) ACTIONLOG0("VERSION");
      server_show_version(from, ub);
      if(logging & L_VER) ACTIONOK();
      return;
    case CC_BYE:
      if(!old) hp->active = 0;
      server_reply(from,ub,0,0);
      return;
    case CC_GET_DIR :
      if (!pos && (logging & L_GETDIR)) ACTIONLOG1("GETDIR");
      if((pe = check_path(s1,l1,&pp)) ||
         (pe = server_get_dir(&pp, inet_num, &fp))) {
	if (pos && (logging & L_ERR) && (logging & L_GETDIR))
	  ACTIONLOG1("GETDIR");
	if ((logging & L_ERR) && (logging & L_GETDIR)) ACTIONFAILED(pe);
        else if (!pos && (logging & L_GETDIR)) ACTIONOK() ;
        send_error(from, ub, pe);
	return;
      }
      send_file(from,ub,fp,l2,s2);
      fclose(fp);
      if (!pos && (logging & L_GETDIR)) ACTIONOK();
      return;
    case CC_GET_FILE:
      if (!pos && (logging & L_GETFILE)) ACTIONLOG1("GETFILE");
      if((pe = check_path(s1,l1,&pp)) ||
	 (pe = server_get_file(&pp, &fp, inet_num, port_num))) {
	if (pos && (logging & L_ERR) && (logging & L_GETFILE))
	  ACTIONLOG1("GETFILE");
	if ((logging & L_ERR) && (logging & L_GETFILE)) ACTIONFAILED(pe);
        else if (!pos && (logging & L_GETFILE)) ACTIONOK() ;
        send_error(from, ub, pe);
	return;
      }
      if (!pos) {
	FSP_STAT(pp.fullp,&sd); /* log filesizes */
        if (logging & L_GETFILE) ACTIONINFO((" (%d)",sd.st_size));
      }
      send_file(from,ub,fp,l2,s2);
      if (!pos && (logging & L_GETFILE)) ACTIONOK();
      return;
    case CC_DEL_FILE:
      if(logging & L_DELFILE) ACTIONLOG1("DELFILE");
      if (read_only) {
	if((logging & L_ERR) && (logging & L_DELFILE))
	   ACTIONFAILED("Permission denied");
        else if (logging & L_DELFILE) ACTIONOK() ;
        send_error(from, ub, "Permission denied");
        return;
      }
      if(!old)
	if((pe = check_path(s1,l1,&pp)) ||
	   (pe = server_del_file(&pp,inet_num))) {
	  if((logging & L_ERR) && (logging & L_DELFILE)) ACTIONFAILED(pe);
          else if (logging & L_DELFILE) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      server_reply(from,ub,0,0);
      if(logging & L_DELFILE) ACTIONOK();
      return;
    case CC_DEL_DIR :
      if(logging & L_DELDIR) ACTIONLOG1("DELDIR");
      if (read_only) {
	if((logging & L_ERR) && (logging & L_DELDIR))
	  ACTIONFAILED("Permission denied");
        else if (logging & L_DELDIR) ACTIONOK() ;
        send_error(from, ub, "Permission denied") ;
	return;
      }
      if(!old)
	if((pe = check_path(s1,l1,&pp)) ||
	   (pe = server_del_dir(&pp,inet_num))) {
	  if((logging & L_ERR) && (logging & L_DELDIR)) ACTIONFAILED(pe);
          else if (logging & L_DELDIR) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      server_reply(from,ub,0,0);
      if(logging & L_DELDIR) ACTIONOK();
      return;
    case CC_UP_LOAD :
      if (!pos || read_only) {
	if(logging & L_UPLOAD) ACTIONLOG0("UPLOAD");
	if(read_only) {
	  if((logging & L_ERR) && (logging & L_UPLOAD))
	    ACTIONFAILED("Permission denied");
          else if (logging & L_UPLOAD) ACTIONOK() ;
          send_error(from, ub, "Permission denied") ;
	  return;
	}
      }
      if(!old)
	if(pe = server_up_load(s1,l1,pos, inet_num,port_num)) {
	  if (pos && (logging & L_ERR) && (logging & L_UPLOAD))
	    ACTIONLOG0("UPLOAD");
	  if ((logging & L_ERR) && (logging & L_UPLOAD)) ACTIONFAILED(pe);
          else if (!pos && (logging & L_UPLOAD)) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      server_reply(from,ub,0,0);
      if(!pos && (logging & L_UPLOAD)) ACTIONOK();
      return;
    case CC_INSTALL :
      if(logging & L_INSTALL) ACTIONLOG1("INSTALL");
      if (read_only) {
	if((logging & L_ERR) && (logging & L_INSTALL))
	  ACTIONFAILED("Permission denied");
        else if (logging & L_INSTALL) ACTIONOK() ;
        send_error(from, ub, "Permission denied") ;
	return;
      }
      if(!old)
	if((pe = check_path(s1,l1,&pp)) ||
	   (pe = server_install(&pp,inet_num,port_num))) {
	  if((logging & L_ERR) && (logging & L_INSTALL)) ACTIONFAILED(pe);
          else if (logging & L_INSTALL) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      server_reply(from,ub,0,0);
      if(logging & L_INSTALL) ACTIONOK();
      return;
    case CC_MAKE_DIR:
      if(logging & L_MAKEDIR) ACTIONLOG1("MAKEDIR");
      if (read_only) {
	if((logging & L_ERR) && (logging & L_MAKEDIR))
	  ACTIONFAILED("Permission denied");
        else if (logging & L_MAKEDIR) ACTIONOK() ;
        send_error(from, ub, "Permission denied") ;
	return;
      }
      if(!old)
	if((pe = check_path(s1,l1,&pp)) ||
	   (pe=server_make_dir(&pp,inet_num))) {
	  if((logging & L_ERR) && (logging & L_MAKEDIR)) ACTIONFAILED(pe);
          else if (logging & L_MAKEDIR) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      if(pe = server_get_pro(&pp,s1,inet_num)) {
	if((logging & L_ERR) && (logging & L_MAKEDIR)) ACTIONFAILED(pe);
        else if (logging & L_MAKEDIR) ACTIONOK() ;
        send_error(from, ub, pe) ;
	return;
      }
      server_reply(from,ub,strlen(ub->buf)+1,0);
      if(logging & L_MAKEDIR) ACTIONOK();
      return;
    case CC_GET_PRO :
      if(logging & L_GETPRO) ACTIONLOG1("GETPRO");
      if((pe=check_path(s1,l1,&pp)) || (pe=server_get_pro(&pp,s1,inet_num))) {
	if((logging & L_ERR) && (logging & L_GETPRO)) ACTIONFAILED(pe);
        else if (logging & L_GETPRO) ACTIONOK() ;
        send_error(from, ub, pe) ;
	return;
      }
      BB_WRITE4(ub->bb_pos,PRO_BYTES);
      server_reply(from,ub,strlen(ub->buf)+1,PRO_BYTES);
      if(logging & L_GETPRO) ACTIONOK();
      return;
    case CC_SET_PRO :
      if(logging & L_SETPRO) ACTIONLOG1("SETPRO");
      if(read_only) {
	if((logging & L_ERR) && (logging & L_SETPRO))
	  ACTIONFAILED("Permission denied");
        else if (logging & L_SETPRO) ACTIONOK() ;
        send_error(from, ub, "Permission denied") ;
	return;
      }
      if(!old)
	if((pe = check_path(s1,l1,&pp)) ||
	   (pe = server_set_pro(&pp,s2,inet_num))) {
	  if((logging & L_ERR) && (logging & L_SETPRO)) ACTIONFAILED(pe);
          else if (logging & L_SETPRO) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      if(pe = server_get_pro(&pp,s1,inet_num)) {
	if((logging & L_ERR) && (logging & L_SETPRO)) ACTIONFAILED(pe);
        else if (logging & L_SETPRO) ACTIONOK() ;
        send_error(from, ub, pe) ;
	return;
      }
      BB_WRITE4(ub->bb_pos,PRO_BYTES);
      server_reply(from,ub,strlen(ub->buf)+1,PRO_BYTES);
      if(logging & L_SETPRO) ACTIONOK();
      return;
    case CC_GRAB_FILE:
      if (!pos || read_only) {
	if(logging & L_GRABFILE) ACTIONLOG1("GRABFILE");
	if (read_only) {
	  if((logging & L_ERR) && (logging & L_GRABFILE))
	    ACTIONFAILED("Permission denied");
          else if (logging & L_GRABFILE) ACTIONOK() ;
          send_error(from, ub, "Permission denied") ;
	  return;
	}
      }
      if(pe = check_path(s1,l1,&pp)) {
	if (pos && (logging & L_ERR) && (logging & L_GRABFILE))
	  ACTIONLOG1("GRABFILE");
	if((logging & L_ERR) && (logging & L_GRABFILE))  ACTIONFAILED(pe);
        else if (!pos && (logging & L_GRABFILE)) ACTIONOK() ;
        send_error(from, ub, pe) ;
	return;
      }
      if(!old && !pos)
	if(pe=server_secure_file(&pp,inet_num,port_num)) {
	  if((logging & L_ERR) && (logging & L_GRABFILE)) ACTIONFAILED(pe);
          else if (logging & L_GRABFILE) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      if(pe = server_grab_file(&pp, &fp, inet_num, port_num)) {
	if (pos && (logging & L_ERR) && (logging & L_GRABFILE))
	  ACTIONLOG1("GRABFILE");
	if((logging & L_ERR) && (logging & L_GRABFILE)) ACTIONFAILED(pe);
        else if (!pos && (logging & L_GRABFILE)) ACTIONOK() ;
        send_error(from, ub, pe) ;
	return;
      }
      send_file(from,ub,fp,l2,s2);
      fclose(fp);
      if (!pos && (logging & L_GRABFILE)) ACTIONOK();
      return;
    case CC_GRAB_DONE:
      if(logging & L_GRABFILE) ACTIONLOG1("GRABDONE");
      if (read_only) {
	if((logging & L_ERR) && (logging & L_GRABFILE))
	  ACTIONFAILED("Permission denied");
        else if (logging & L_GRABFILE) ACTIONOK() ;
        send_error(from, ub, "Permission denied") ;
	return;
      }
      if(pe = check_path(s1,l1,&pp)) {
	if((logging & L_ERR) && (logging & L_GRABFILE)) ACTIONFAILED(pe);
        else if (logging & L_GRABFILE) ACTIONOK() ;
        send_error(from, ub, pe) ;
	return;
      }
      if(!old)
	if(pe = server_grab_done(&pp,inet_num,port_num)) {
	  if((logging & L_ERR) && (logging & L_GRABFILE)) ACTIONFAILED(pe);
          else if (logging & L_GRABFILE) ACTIONOK() ;
          send_error(from, ub, pe) ;
	  return;
	}
      server_reply(from,ub,0,0);
      if(logging & L_GRABFILE) ACTIONOK();
      return;
    default:
      if(logging & L_ERR) {
	ACTIONLOG0("UNKNOWN");
	ACTIONINFO((" (%d)", ub->cmd));
	ACTIONFAILED("Unknown FSP command");
        send_error(from, ub, "Unknown FSP command") ;
      }
      return;
  }
}

static int arg_err PROTO0((void))
{
  fputs("Usage: fspd [-f configfile]\n", stderr);
  exit(1);
}

int main PROTO2(int, argc, char **, argv)
{
  inetd_mode = !strcmp(argv[0],"in.fspd");

  if(argc != 1) {
	if (argc>3) arg_err() ;
	if (strncmp(argv[1],"-f",2)) arg_err() ;
	if (argc==3) config_file=argv[2] ;
	else config_file=argv[1]+2 ;
  }

  load_configuration();
    
  if(inetd_mode) {
    init_inetd();
    freopen(NULL_DEV,"r",stdin);
    freopen(NULL_DEV,"w",stdout);
    freopen(NULL_DEV,"w",stderr);
  }

  /* clear all entries in file-cache */
  clear_cache(fpcache, &cache_p);

  /* Moved setuid to here from below because log file was getting opened
   * by root, and fspd could no longer write to the log file after the
   * setuid. This should always open the file as run_uid
   * Modified A.E.J.Fellows 9 March 93
   */

  if(run_uid) if(setuid(run_uid) != 0) exit(1);
  init_home_dir();

  if (logging && *logname) {
    /* test to see if logfile can be written */
    /* open it append mode so that it doesn't wipe the file when
     * you are running under inetd. VMS uses file version number, so
     * the old logfile won't be erased.
     */
#ifdef VMS
    if((logfd=vms_open(logname, "w")) < 0) {
#else
    if((logfd=open(logname, O_WRONLY | O_APPEND | O_CREAT, 0644)) < 0) {
#endif
      fprintf(stderr, "Error opening logfile: %s, logging disabled.\n",
	      logname);
      logging = 0; /* no logging */
    }
  }

  if(!inetd_mode) {
    /* Fork and die to drop daemon into background   */
    /* Added Alban E J Fellows 12 Jan 93             */
    /* Moved by JT Traub to only do this if not running under inetd. */
    if(daemonize) {
#ifdef VMS
      freopen(NULL_DEV,"r",stdin); /* speed up */
      freopen(NULL_DEV,"w",stdout);
      freopen(NULL_DEV,"w",stderr);
      if (vfork() > 0)
#else
      if (fork() > 0)
#endif
	exit(0);
    }
    init_network(udp_port);
  }

  /* setuid and init_home_dir move from here to above the logging file open */
  init_htab();

  srandom(getpid());

  if(inetd_mode)
    server_loop(120*1000L);  /* 2 minutes */
  else
    while(1) server_loop(-1L);

  exit(0);
}
