/*
 * File:	sendfiled.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * Contrib.:	Thomas Tissen (tici@uni-paderborn.de)
 *		Olaf Erb (erb@insu1.etec.uni-karlsruhe.de)
 *		Rainer Bawidamann (widi@sol.wohnheim.uni-ulm.de)
 *
 * History:	
 *
 *   11 Aug 95   Framstag	initial version
 *   10 Sep 95   Framstag	added delete and resend function
 *    1 Nov 95   Framstag	eliminated some security flaws
 *                              added pgp signature entry
 *    5 Nov 95   Framstag	added NeXT support
 *   14 Nov 95   Framstag	added user config files
 *   21 Nov 95   Framstag	added chat client support
 *    4 Jan 96   Framstag	added allow-only flag to NOSENDFILE
 *   11 Jan 96   Framstag	added global and user config files
 *   27 Jan 96   Framstag	added maxspool and minfree config option
 *   31 Jan 96   Framstag	bug fixes for mail2user
 *    4 Feb 96   Olaf Erb	added notification=both
 *    5 Feb 96   Framstag	some code cleanup
 *    6 Feb 96   Framstag	added ATTR EXE
 *   				bug fixes for attribute handling
 *   19 Feb 96   Framstag	fixed problems with NFS and MSG
 *   				fixed statfs-problem with Solaris-2 (?)
 * 				enhanced msg2tty with non-blocking write
 *   20 Feb 96   Framstag	changed msg-tty and msg-fifo files to support 
 *				NFS (msg2tty)
 *   21 Feb 96   widi		better Solaris-2 support
 *   21 Feb 96   Framstag	bug fix with maxfiles option
 * 				better notification check
 *   22 Feb 96   Framstag	replaced string "localhost" with the
 *				real name of the local host
 *   23 Feb 96   Framstag	mail-notification now contains output
 *				of "receive -l", too
 *   17 Mar 96   Framstag	security bug (?) fixed:
 *                              no more chown on symlinks
 *   27 Mar 96   Framstag	security bug fixed V2.0:
 *                              no more chown on any links
 *    1 Apr 96   Framstag	corrected logfile bug
 *				swapped O_SYNC and O_NONBLOCK
 *    2 Apr 96   Framstag	fixed FROM line handling
 * 				added forwarding by user
 *    4 Apr 96   Framstag	allowed COMPRESSED=GZIP for TYPE option
 *    8 Apr 96   Framstag 	changed signature command
 * 				better checking for same files
 *   12 Apr 96   Framstag	added pgp support
 * 				added own SIGN attribute
 *   10 May 96   Framstag	added global alias file support
 *   12 May 96   Framstag	added checking of SPOOL/.nosendfile
 *   22 May 96   Framstag	added -c configfile runtime option
 *   21 Jun 96   Framstag	added global log files
 *   22 Jun 96   Framstag	better default date setting
 *   23 Jun 96   Framstag	nicer log file formats
 *   13 Jun 96   Framstag	differ between SYSV and BSD du
 *   24 Sep 96   Framstag	protocol-change:
 * 				CHARSET charset --> TYPE TEXT=charset
 *
 *
 * The sendfile-daemon of the sendfile package.
 * sendfiled receives files for a specified recipient, stores them in the
 * sendfile spool directory and informs the recipient.
 * sendfiled receives messages for a specified recipient and display them
 * on the recipients terminal.
 * sendfiled is spawned via the inetd superserver. See the README file
 * for installing hints.
 *
 * Copyright © 1995,1996 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <netdb.h>
#include <fcntl.h>
#include <time.h>
#include <utmp.h>
#include <dirent.h>
#include <pwd.h>
#include <signal.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#if defined(IRIX) || defined(IRIX64)
  #include <sys/statfs.h>
#elif defined(AIX)
  #include <sys/statfs.h>
  int statfs(char *, struct statfs *);
#elif defined(SOLARIS2)
  #include <sys/statvfs.h>
  #define statfs statvfs
  #define FSBS 1024  /* dirty hack against broken statvfs block information */
#elif defined(OSF1)
  #include <sys/mount.h>
  #include <ustat.h>
  int statfs(char *p, struct statfs *, int);
#elif defined(BSD) || defined(ULTRIX)
  #include <sys/param.h>
  #include <sys/mount.h>
#else
  #include <sys/vfs.h>
#endif
#ifdef SYSV
  #define DU "du -ks "
#else
  #define DU "du -s "
#endif

#include "config.h"     /* various definitions */
#include "reply.h"	/* the 3 digit reply codes with text messages */
#include "peername.h"	/* get the name of the calling host */
#include "string.h"	/* extended string functions */
#include "spool.h"	/* operations on files in the sendfile spool */
#include "net.h"	/* network stuff */
#include "io.h"		/* socket read/write */
#include "utf7.h"       /* UTF-7 coding */

/* stupid AIX comes with no include files for networking and other stuff */
#if defined(AIX) || defined(ULTRIX)
  #include "bsd.h"
#endif

#if !defined(HPUX)
  int gethostname(char *, int);
#endif

#ifdef LINUX
  int fileno(FILE *);
#endif

#ifndef AIX
  #ifndef CONVEXOS
    FILE *popen(const char *, const char *);
  #endif
  int pclose(FILE *);
  #if defined(IRIX) || defined(LINUX)
    #include <getopt.h>
  #else
    int getopt(int, char * const *, const char *);
    extern char *optarg;
  #endif
#endif

#ifdef ULTRIX
  int statfs(char *, struct fs_data *);
#endif

#ifdef NEXT
  int statfs(char *, struct statfs *);
#endif

#ifdef HPUX
  #define seteuid(a) setuid(a)
#endif

#ifndef OSF1
  int strncasecmp(const char *, const char *, size_t);
#endif

#ifndef _PATH_UTMP
  #define _PATH_UTMP "/etc/utmp"
#endif


/* get a command line */
int getline(char *);

/* write one line to header spool file */
void writeheader(int, const char *, const char *);

/* notify recipient and send reply to client */
void notify_reply(int *, char, const char *, const char *, const char *,
		  int, int, int);

/* write a message to all ttys of the user */
int msg2tty(const char *, const char *, int, int);

/* send a mail to the recipient */
void mail2user(const char *, const char *, const char *, int);

/* get the next spool id number */
int spoolid(int);

/* check killfile */
int restricted(const char *, const char *, char);

/* write-lock a file */
int wlock_file(int);

/* test the lock status of a file */
int tlock_file(int);

/* missed in <unistd.h> */
int seteuid(uid_t);

/* sendfile spool daemon for outgoing files */
void sfsd();

/* check outgoing spool if there are expired files */
void check_outspool(int);

/* check if user is allowed to use sendfile and create the user spool 
    directories */
int check_userspool(char *, int *, int *);

/* simple interrupt handler for SIGCHLD */
void sigchld();


/* global variables */
char
  *prg,				/* name of the game (not used) */
  *config,			/* config file name */
  *outgoing=SPOOL"/OUTGOING",	/* outgoing spool directory */
  localhost[FLEN], 		/* name of the local host */
  userspool[MAXLEN],		/* user spool directory */
  userconfig[MAXLEN];		/* user config directory */
int
  client=0,		/* flag to determine client or server */
  verbose=0;		/* flag for verbose mode */


int main(int argc, char *argv[])
{ int
    opt,  			/* arg-option to test for */
    bytes,			/* number of bytes to be received */
    infd,			/* input file descriptor */
    outfd,			/* input file descriptor */
    shfd,			/* spool header file descriptor */
    sdfd,			/* spool data file descriptor */
    lfd,			/* log file descriptor */
    nblocks,			/* number of PACKET length blocks */
    bn,				/* block number to read */
    maxfiles,			/* maximal number of allowed files per user */
    minfree,			/* minimum free spool disk space in MB */
    maxspool,			/* maximum spool disk space quota, total in MB */
    spoolsize,			/* current spool size in KB */
    notify,			/* flag for sending a message */
    bell,			/* flag for tty bell */
    forwarding,			/* flag for user forwarding */
    spooling,			/* flag for outgoing spooling */
    iterrupt,			/* flag for interruptable sendfiled */
    sys_bell,			/* global flag for tty bell */
    del_success,		/* flag for successfull deleting a file */
    flags,			/* source, text, compress, tar and exe flag */
    id,				/* spool file id */
    ruid,			/* recipient's user id */
    rgid,			/* recipient's group id */
    n;				/* simple loop variable */
  unsigned long int
    transmitted,		/* bytes already transmitted */
    size,			/* size of the file */
    osize;			/* original size of the file (uncompressed) */
  char
    *cp,			/* simple char pointer */
    *argp,			/* argument string pointer */
    *peer,			/* sender host name */
    *realr,			/* real recipient name */
    *aliasr,			/* alias recipient name */
    log,			/* type of global logging */
    acceptonly,			/* flag for accepting only files or messages */
    notification,		/* flag for notification by message or mail */
    sys_notification,		/* global flag for notification */
    line[MAXLEN],		/* incoming command line */
    arg[MAXLEN],		/* the argument(s) of the command line */
    cmd[MAXLEN],		/* the command itself */
    type[MAXLEN],		/* file type: binary, source or text */
    subtype[MAXLEN],		/* COMPRESSED or CRYPTED subtype */
    sizes[FLEN],		/* original and compressed file size */
    charset[MAXLEN],		/* name of the character set */
    attribute[MAXLEN],		/* tar or exe attribute */
    sign[MAXLEN],		/* pgp signature */
    comment[MAXLEN],		/* file comment in UTF-7 */
    date[MAXLEN],		/* date string */
    currentdate[FLEN],		/* current date */
    shfile[MAXLEN],		/* spool header file name */
    sdfile[MAXLEN],		/* spool data file name */
    tmp[3*MAXLEN], 		/* temporary string */
    real[MAXLEN],		/* sender real name in UTF-7 */
    forward[MAXLEN],		/* user forward address */
    sender[MAXLEN],		/* user@senderhost (real name) */
    utfsender[MAXLEN],		/* sender in UTF-7 */
    logsender[MAXLEN],		/* sender for log file */
    filename[MAXLEN],		/* file name in UTF-7 */
    recipient[MAXLEN],		/* local user */
    mailto[MAXLEN],		/* notification mail recipient */
    saftserver[MAXLEN],		/* real saft server name */
    dummy[MAXLEN],		/* dummy string for utf2iso name conversion */
    packet[PACKET],		/* data packet to read */
    msg[3*MAXLEN],		/* message to user-tty */
    logdata[OVERSIZE];		/* log file data */
  unsigned char *ucp;		/* simple unsigend char pointer */
  struct stat finfo;		/* information about a file */
#ifdef ULTRIX
  struct fs_data fsinfo;	/* information about the file system */
#else
  struct statfs fsinfo;		/* information about the file system */
#endif
  struct filelist *flp;		/* file list pointer */
  struct senderlist *sls;	/* sender list start */
  time_t timetick,tt1,tt2;	/* unix time (in seconds) */
  FILE 
    *inf,			/* for various files */
    *outf,			/* output file */
    *pp;			/* pipe stream */


  /* set default values */
  bell=1;
  sys_bell=1;
  ruid=0;
  rgid=0;
  flags=0;
  notify=0;
  minfree=0;
  iterrupt=0;
  maxspool=0;
  spooling=1;
  maxfiles=200;
  forwarding=1;
  acceptonly=0;
  transmitted=0;
  log='b';
  notification='t';
  sys_notification='t';
  *sign=0;
  *date=0;
  *mailto=0;
  *sender=0;
  *comment=0;
  *filename=0;
  *utfsender=0;
  *recipient=0;
  *attribute=0;
  *saftserver=0;
  config=CONFIG;
  strcpy(charset,CHARSET);
  strcpy(type,"BINARY");
  sls=NULL;
  flp=NULL;

  /* scan the command line */
  while ((opt=getopt(argc, argv, "c:")) > 0)
  { switch (opt)
    { case 'c': config=optarg; }
  }

  /* get the local host name */
  if (gethostname(localhost,FLEN-1)<0) strcpy(localhost,"localhost");

  /* send the server ready message */
  reply(220);

  /* parse the config-file */
  if ((inf=fopen(config,"r")))
  { while (fgets(line,MAXLEN-1,inf))
    {
      /* prepare line to be parsed */
      if ((cp=strchr(line,'#'))) *cp=0;
      if ((cp=strchr(line,'='))) *cp=' ';
      str_tolower(str_trim(line));

      /* is there an option and an argument? */
      if ((argp=strchr(line,' ')))
      { *argp=0; argp++;
	if (streq(line,"bell"))
	{ if (streq(argp,"off")) sys_bell=0;
	  continue;
	}
	if (streq(line,"iterrupt"))
	{ if (streq(argp,"on")) iterrupt=1;
	  continue;
	}
	if (streq(line,"forwarding"))
	{ if (streq(argp,"off")) forwarding=0;
	  continue;
	}
	if (streq(line,"spooling"))
	{ if (streq(argp,"off")) spooling=0;
	  continue;
	}
	if (streq(line,"maxfiles"))
	{ maxfiles=atoi(argp);
	  continue;
	}
	if (streq(line,"maxspool"))
	{ maxspool=atoi(argp);
	  continue;
	}
	if (streq(line,"minfree"))
	{ minfree=atoi(argp);
	  continue;
	}
	if (streq(line,"acceptonly"))
	{ acceptonly=*argp;
	  continue;
	}
	if (streq(line,"saftserver"))
	{ strcpy(saftserver,argp);
	  continue;
	}
	if (streq(line,"notification"))
	{ if (streq(argp,"mail")) sys_notification='m';
	  if (streq(argp,"both")) sys_notification='b';
	  if (streq(argp,"none")) sys_notification=0;
	  continue;
	}
	if (streq(line,"log"))
	{ if (streq(argp,"in"))   log='i';
	  if (streq(argp,"out"))  log='o';
	  if (streq(argp,"both")) log='b';
	  if (streq(argp,"none")) log=0;
	  continue;
	}
      }

    }
    fclose(inf);
  }


  /* main loop for command line parsing and getting data */
  for (;;)
  {
    /* get the next command line from the client */
    if (getline(line)==0) continue;

    /* extract the command name and the argument */
    strcpy(cmd,line);
    argp=strchr(cmd,' ');
    if (argp)
    { *argp=0;
      strcpy(arg,argp+1);
    } else
      *arg=0;
    str_toupper(cmd);

    /* HELP command? */
    if (streq(cmd,"HELP") || streq(cmd,"?"))
    { reply(214);
      continue;
    }

    /* TO command? */
    if (streq(cmd,"TO"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      /* is there a pending notification request? */
      if (notify) notify_reply(&notify,notification,sender,recipient,
			       mailto,bell,ruid,0);

      /* convert recipient name from UTF-7 to ISO Latin 1 */
      utf2iso(0,recipient,dummy,dummy,arg);

      realr=aliasr=NULL;

      /* is there a global alias file? */
      if ((inf=fopen(ALIASES,"r")))
      { while (fgets(line,MAXLEN-1,inf))
	{
	  /* prepare line to be parsed */
	  if ((cp=strchr(line,'#'))) *cp=0;
	  str_trim(line);

	  /* check alias and real recipient user name */
	  if ((realr=strchr(line,' ')))
	  { *realr=0; realr++;
	    aliasr=line;
	    if (streq(aliasr,recipient)) break;
	  }

	}
	fclose(inf);

	/* alias found? */
	if (aliasr && streq(aliasr,recipient))
	{ if (strchr(realr,'@'))
	  { printf("510 Admin has set a forward to: %s\r\n",realr);
	    fflush(stdout);
	    *recipient=0;
	    continue;
	  }
	  else
	    strcpy(recipient,realr);
	}

      }
      				 
      if (check_userspool(recipient,&ruid,&rgid)<0) continue;

      /* set global configs */
      *forward=0;
      bell=sys_bell;
      notification=sys_notification;
      strcpy(mailto,recipient);

      /* parse the user config-file */
      sprintf(tmp,"%s/config",userconfig);
      if ((inf=fopen(tmp,"r")))
      { while ((fgets(line,MAXLEN-1,inf)))
	{
	  /* prepare line to be parsed */
	  if ((cp=strchr(line,'='))) *cp=' '; else continue;
	  if ((cp=strchr(line,'#'))) *cp=0;
	  str_tolower(str_trim(line));

	  /* is there an option and an argument? */
	  if ((argp=strchr(line,' ')) && strlen(argp)>1)
	  { *argp=0; argp++;

	    /* bell on or off? */
	    if (streq(line,"bell"))
	    { if (streq(argp,"off"))
		bell=0;
	      else
		bell=1;
	      continue;
	    }

	    /* forward address specified? */
	    if (streq(line,"forward") && *argp && strchr(argp,'@'))
	    { strcpy(forward,argp);
	      continue;
	    }

	    /* determine notification type */
	    if (streq(line,"notification"))
	    { if (streq(argp,"none"))    notification=0;
	      if (streq(argp,"message")) notification='t';
	      if (strbeq(argp,"mail"))	 notification='m';
	      if (strbeq(argp,"both"))	 notification='b';

	      /* mail address specified to send notifications to? */
	      if ((argp=strchr(argp,' '))) strcpy(mailto,argp+1);

	      continue;
	    }

	  }
	}
	fclose(inf);
      }

      /* user forward? */
      if (forwarding && *forward && *filename)
      { printf("510 User has set a forward to %s\r\n",forward);
	fflush(stdout);
	continue;
      }

      reply(200);
      continue;
    }

    /* FROM command? */
    if (streq(cmd,"FROM"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      *real=0;

      /* is there a real name? */
      if ((cp=strchr(arg,' ')))
      { strcpy(real,cp+1);
	*cp=0;
      }

      /* save sender@host and real name */
      peer=peername(0);
      if (streq(peer,"localhost")) peer=localhost;
      if (strlen(arg)+strlen(peer)+strlen(real)+4<MAXLEN)
      { sprintf(utfsender,"%s@%s %s",arg,peer,real);
	sprintf(tmp,"%s@%s (%s)",arg,peer,real);
	utf2iso(0,sender,dummy,dummy,tmp);
      } else
      { sprintf(utfsender,"???@%s",peer);
	sprintf(sender,"???@%s",peer);
      }

      reply(200);
      continue;
    }

    /* CHARSET command? (only for compatibilty reason) */
    if (streq(cmd,"CHARSET"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      /* save the charset name */
      strcpy(charset,arg);
      reply(200);
      continue;
    }

    /* DATE command? */
    if (streq(cmd,"DATE"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      /* save the date */
      utf2iso(0,dummy,date,dummy,arg);

      /* parse ISO-8601 date & time string */
      if ((cp=strchr(date,'T'))) *cp=' ';
      if (!strchr(date,'-'))
      { strcpy(tmp,date);
	sprintf(date,"%c%c%c%c-%c%c-%c%c %c%c:%c%c:%c%c",
		tmp[0],tmp[1],tmp[2],tmp[3],
		tmp[4],tmp[5],
		tmp[6],tmp[7],
		tmp[9],tmp[10],
		tmp[11],tmp[12],
		tmp[13],tmp[14]);
      }

      reply(200);
      continue;
    }

    /* SIGN command? */
    if (streq(cmd,"SIGN"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      strcpy(sign,arg);

      reply(200);
      continue;
    }

    /* FILE command? */
    if (streq(cmd,"FILE"))
    {
      /* forward address set? */
      if (*saftserver)
      {
	/* is this a msg-only server? */
	if (acceptonly=='m') printf("510-This SAFT-server can only receive "
			 	    "messages, no files.\r\n");

	if (*recipient)
	  printf("510 For sending files use: %s@%s\r\n",recipient,saftserver);
	else
	  printf("510 For sending files use: user@%s\r\n",saftserver);

	fflush(stdout);
	continue;
      }

      /* is this a msg-only server? */
      if (acceptonly=='m')
      { printf("510 This SAFT-server can only receive messages, no files.\r\n");
	fflush(stdout);
	continue;
      }

      /* user forward? */
      if (forwarding && *forward)
      { printf("510 User has set a forward to %s\r\n",forward);
	fflush(stdout);
	continue;
      }

      /* receiving files currently disabled? */
      if (stat(SPOOL"/.nosendfile",&finfo)==0) reply(421);

      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      flp=NULL;

      /* save the filename (still in UTF-7) */
      strcpy(filename,arg);
      reply(200);
      continue;
    }

    /* ATTR command? */
    if (streq(cmd,"ATTR"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      str_toupper(arg);

      /* exe attribute? */
      if (streq(arg,"EXE"))
      { strcpy(attribute,arg);
	flags=(flags|F_EXE)&~F_TAR;
        reply(200);
        continue;
      }

      /* tar attribute? */
      if (streq(arg,"TAR"))
      { strcpy(attribute,arg);
	flags=(flags|F_TAR)&~F_EXE;
        reply(200);
        continue;
      }

      /* reset attribute? */
      if (streq(arg,"NONE"))
      { *attribute=0;
	flags=flags&~F_TAR&~F_EXE;
        reply(200);
        continue;
      }

      reply(504);
      continue;
    }

    /* TYPE command? */
    if (streq(cmd,"TYPE"))
    {
      /* parse the type command and check if it is valid */
      str_toupper(arg);
      if (strstr(arg,"COMPRESSED=GZIP"))
      { cp=strchr(arg,'=');
	*cp=0;
      }
      if (strstr(arg,"CRYPTED=PGP"))
      { cp=strchr(arg,'=');
	*cp=0;
      }
      
      /* save the type */
      strcpy(type,arg);
      
      /* charset specified? */
      if (strbeq(type,"TEXT="))
      { 
	/* save subtype (CRYPTED or COMPRESSED) */
	*subtype=0;
	if ((cp=strrchr(type,' ')))
	{ strcpy(subtype,cp);
	  *cp=0;
	}
	
	/* extract CHARSET from TYPE string */
	cp=strchr(type,'=');
	*cp=0;
	strcpy(charset,cp+1);
	strcat(type,subtype);
      }
	
      if (!(streq(type,"TEXT")              ||
	    streq(type,"TEXT CRYPTED")      ||
	    streq(type,"TEXT COMPRESSED")   ||
            streq(type,"SOURCE")            ||
	    streq(type,"SOURCE CRYPTED")    ||
	    streq(type,"SOURCE COMPRESSED") ||
	    streq(type,"BINARY")		   ||
	    streq(type,"BINARY CRYPTED")    ||
	    streq(type,"BINARY COMPRESSED")))
      {
	/* wrong type format */
	reply(501);
	continue;
      }

      /* save the flags */
      flags=flags&~F_SOURCE&~F_TEXT&~F_COMPRESS&~F_CRYPT;
      if (strstr(type,"TEXT"))       flags=flags|F_TEXT;
      if (strstr(type,"SOURCE"))     flags=flags|F_SOURCE;
      if (strstr(type,"CRYPTED"))    flags=flags|F_CRYPT;
      if (strstr(type,"COMPRESSED")) flags=flags|F_COMPRESS;

      reply(200);
      continue;
    }

    /* SIZE command? */
    if (streq(cmd,"SIZE"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      /* are the arguments correct? */
      { int s1,s2;
	if (sscanf(arg,"%d %d",&s1,&s2)!=2)
	{ reply(501);
	  continue;
	}
      }

      /* save the size(s) */
      sscanf(arg,"%ld %ld",&size,&osize);
      strcpy(sizes,arg);
      transmitted=0;

#if defined(IRIX)
      /* spool partition min free limit set? */
      if (minfree && statfs(userspool,&fsinfo,sizeof(struct statfs),0)==0)
      {
	/* not enough space in spool partition (in MB)? */
	if (fsinfo.f_bfree*fsinfo.f_bsize/1048576<minfree)
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,452);
      }
#elif defined(ULTRIX)
      /* spool partition min free limit set? */
      if (minfree && statfs(userspool,&fsinfo)==0)
      {
	/* not enough space in spool partition (in MB)? */
	if (((struct fs_data_req *)&fsinfo)->bfreen/1024<minfree)
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,452);
      }
#else
      /* spool partition min free limit set? */
#ifdef OSF1
      if (minfree && statfs(userspool,&fsinfo,sizeof(struct statfs))==0)
#else
      if (minfree && statfs(userspool,&fsinfo)==0)
#endif
      {
#ifdef FSBS
	/* hack, hack :-) */
	fsinfo.f_bsize=FSBS;
#endif
	/* not enough space in spool partition (in MB)? */
	if (fsinfo.f_bavail*fsinfo.f_bsize/1048576<minfree)
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,452);
      }
#endif

      /* spool quota limit set? */
      if (maxspool)
      { pp=popen(DU SPOOL,"r");
	if (fgets(tmp,100,pp))
	{
	  sscanf(tmp,"%d",&spoolsize);

	  /* not enough space in spool (in KB)? */
	  if (spoolsize+size/1024>maxspool*1024)
	    notify_reply(&notify,notification,sender,recipient,mailto,bell,
			 ruid,452);
	}
	pclose(pp);
      }

      reply(200);
      continue;
    }

    /* MSG command? */
    if (streq(cmd,"MSG"))
    {
      /* is this a file-only server? */
      if (acceptonly=='f')
      { reply(511);
	continue;
      }

      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      /* sender and recipient already specified? */
      if (*sender==0 || *recipient==0)
      { reply(503);
	continue;
      }

      /* check killfile */
      if (restricted(sender,recipient,'m'))
      { reply(523);
	continue;
      }

      /* convert message from UTF-7 to ISO Latin 1 and kill control codes */
      utf2iso(0,msg,dummy,dummy,arg);
      for (ucp=(unsigned char *)msg,n=0; *ucp; ucp++)
	if (*ucp==9 || *ucp==10 || (*ucp>31 && *ucp<127) || *ucp>159)
          tmp[n++]=*ucp;
      tmp[n]=0;

      timetick=time(0);
      strftime(currentdate,9,"%H:%M",localtime(&timetick));
      sprintf(msg,"\r\nMessage from %s at %s :\r\n%s\r\n",sender,currentdate,tmp);
      if (bell) strcat(msg,"\007");

      /* try to send to recipient ttys */
      if (msg2tty(recipient,msg,ruid,O_SYNC)<0)
        reply(522);
      else
        reply(200);
      continue;
    }

    /* DELETE command? */
    if (streq(cmd,"DEL"))
    {
      /* sender, recipient and filename already specified? */
      if (*sender==0 || *recipient==0 || *filename==0)
      { reply(503);
	continue;
      }

      transmitted=0;
      del_success=-1;

      /* are there any files in the spool directory from this user? */
      if ((sls=scanspool(sender)))
      {
	/* loop over files list */
        for (flp=sls->flist; flp!=NULL; flp=flp->next)
	{
	  /* if file found try to delete spool file */
          if (streq(filename,flp->fname)) del_success*=delete_sf(flp,0);

	}
      }

      if (del_success!=0)
      	reply(530);
      else
      	reply(200);

      continue;
    }

    /* DATA command? */
    if (streq(cmd,"DATA"))
    {
      /* sender, recipient, sizes and filename already specified? */
      if (*sender==0 || *recipient==0 || *filename==0 ||
	  (*sizes==0 && transmitted==0))
      { reply(503);
	continue;
      }

      /* does the top level spool directory exist? */
      if (stat(SPOOL,&finfo)<0 || (finfo.st_mode&S_IFMT)!=S_IFDIR)
	notify_reply(&notify,notification,sender,recipient,mailto,bell,
		     ruid,410);

      /* change effective uid to recipient */
      seteuid(ruid);

      /* go to the user spool directory */
      if (chdir(userspool)<0)
	notify_reply(&notify,notification,sender,recipient,mailto,bell,
		     ruid,410);

      /* check killfile */
      if (restricted(sender,recipient,'f'))
      { reply(523);
	continue;
      }

      /* resend option? */
      if (transmitted && flp)
      { if (transmitted==flp->csize)
	{ transmitted=0;
          reply(531);
	  continue;
	}

	/* open spool data file for appending */
        sprintf(sdfile,"%d.d",flp->id);
        if ((sdfd=open(sdfile,O_WRONLY|O_APPEND,S_IRUSR|S_IWUSR)) < 0)
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,412);

      }
      else /* new file */
      {
        /* check if the file has been already sent */
        if ((sls=scanspool(sender)))
	{
	  /* loop over files list */
          for (flp=sls->flist; flp!=NULL; flp=flp->next)
	  {
	    /* is it the same file and complete? */
            if (streq(filename,flp->fname) &&
		streq(date,flp->date) &&
                flp->csize==flp->tsize &&
		osize==flp->osize &&
		(flags|F_COMPRESS|F_CRYPT)==(flp->flags|F_COMPRESS|F_CRYPT))
	    { reply(531);
              *filename=0;
              break;
            }

          }

          /* return to main loop if file is already there */
          if (*filename==0) continue;

        }

        /* get next spool id */
        id=spoolid(maxfiles);
        if (id==0)       notify_reply(&notify,notification,sender,
				      recipient,mailto,bell,ruid,412);
        if (id>maxfiles) notify_reply(&notify,notification,sender,
				      recipient,mailto,bell,ruid,413);

        /* open spool header and data files */
        sprintf(shfile,"%d.h",id);
        sprintf(sdfile,"%d.d",id);
        sdfd=open(sdfile,O_WRONLY,S_IRUSR|S_IWUSR);
        shfd=open(shfile,O_WRONLY,S_IRUSR|S_IWUSR);
        if (shfd<0 || sdfd<0) notify_reply(&notify,notification,sender,
					   recipient,mailto,bell,ruid,412);

        /* lock the data file */
        wlock_file(sdfd);

	/* get the actual UTC time if date is not specified */
	if (!*date)
	{ timetick=time(0);
	  strftime(date,20,"%Y-%m-%d %H:%M:%S",gmtime(&timetick));
	}

        /* write the header lines */
        writeheader(shfd,"FROM",utfsender);
        writeheader(shfd,"FILE",filename);
        writeheader(shfd,"TYPE",type);
        writeheader(shfd,"SIZE",sizes);
        writeheader(shfd,"DATE",date);
        if (*attribute)
          writeheader(shfd,"ATTR",attribute);
        if (*sign)
          writeheader(shfd,"SIGN",sign);
        if (*comment)
          writeheader(shfd,"COMMENT",comment);
        close(shfd);

      }

      /* tell the client to send data */
      reply(302);

      /* read the file data in PACKET size blocks */
      /* and write it to the spool file */
      bytes=size-transmitted;
      nblocks=bytes/PACKET;
      transmitted=0;
      tt1=time(0);
      for (bn=1; bn<=nblocks; bn++)
      {
	/* check every 3 seconds if receiving files is currently disabled? */
	tt2=time(0);
	if (tt2-tt1>2)
	{ tt1=tt2;
	  if (stat(SPOOL"/.nosendfile",&finfo)==0) reply(421);
	}

	if (readn(0,packet,PACKET)<PACKET)
	{ close(sdfd);
          unlink(sdfile);
          unlink(shfile);
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,415);
        }
        if (writen(sdfd,packet,PACKET)<PACKET)
	{ close(sdfd);
          unlink(sdfile);
          unlink(shfile);
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,452);
        }

      }

      /* copy the last bytes to the spool file */
      if ((n=bytes-nblocks*PACKET))
      { if (readn(0,packet,n)<n)
	{ close(sdfd);
          unlink(sdfile);
          unlink(shfile);
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,415);
        }
        if (writen(sdfd,packet,n)<n)
	{ close(sdfd);
          unlink(sdfile);
          unlink(shfile);
	  notify_reply(&notify,notification,sender,recipient,mailto,bell,
		       ruid,452);
        }
      }
      close(sdfd);

      /* all ok */
      reply(201);

      /* get the receive date */
      timetick=time(0);
      strftime(currentdate,20,"%Y-%m-%d %H:%M:%S",localtime(&timetick));

      /* reformat file and sender name for log file */
      if (streq(attribute,"TAR")) strcat(filename," (archive)");
      if ((cp=strchr(utfsender,' ')))
      { *cp=0;
	sprintf(logsender,"%s (%s)",utfsender,cp+1);
	*cp=' ';
      } else
	strcpy(logsender,utfsender);
	
      
      /* create logfile if not there */
      close(open("log",O_CREAT|O_EXCL,S_IRUSR|S_IWUSR));

      /* try several times to lock-write the logfile */
      lfd=open("log",O_WRONLY|O_APPEND);
      for (n=1; n<5; n++)
      { if (wlock_file(lfd) >= 0)
	{
	  /* add header to empty log file */
	  stat("log",&finfo);
	  if (finfo.st_size==0)
	  { sprintf(tmp,"# use \"utf7decode %s/log\" to view this file\n\n",
		    userspool);
	    write(lfd,tmp,strlen(tmp));
	  }
	  
	  /* write to the log file */
          writeheader(lfd,"FROM",logsender);
          writeheader(lfd,"FILE",filename);
          writeheader(lfd,"DATE",currentdate);
          if (*comment) writeheader(lfd,"COMMENT",comment);
          if (*sign)	writeheader(lfd,"SIGN",sign);
	  write(lfd,"\n",1);
          break;

        }
        sleep(1);
      }
      close(lfd);

      /* reset uid to root */
      seteuid(0);

      /* add entry to global logfile if required */
      if ((log=='b' || log=='i') && (outf=fopen(INLOG,"a")))
      { fprintf(outf,"FROM\t%s\nTO\t%s\nDATE\t%s\nFILE\t%s\nSIZES\t%s\n\n",
		logsender,recipient,currentdate,filename,sizes);
	fclose(outf);
      }

      /* reset attributes */
      *filename = *sign = *comment = *attribute = *sizes = *date = 0;
      flags=transmitted=0;
      flp=NULL;
      strcpy(charset,CHARSET);
      strcpy(type,"BINARY");

      /* recipient has to be notified at the end */
      notify=1;

      continue;
    }

    /* RESEND command? */
    if (streq(cmd,"RESEND"))
    {
      /* sender, recipient and filename already specified? */
      if (*sender==0 || *recipient==0 || *filename==0 || *sizes==0)
      { reply(503);
        continue;
      }

      /* check if this file has been already sent */
      if ((sls=scanspool(sender)))
      {
	/* loop over files list */
        for (flp=sls->flist; flp!=NULL; flp=flp->next)
	{
	  /* is it the same file ? */
          if (streq(filename,flp->fname) &&
	      streq(date,flp->date) && flags==flp->flags)
	  {
	    /* with same sizes? */
	    sprintf(tmp,"%ld %ld",flp->csize,flp->osize);
	    if (streq(tmp,sizes))
	    {
	      /* number of bytes already transmitted */
	      transmitted=flp->tsize;
	      break;

	    }
	  }
	}
      }

      /* emulate reply(230) */
      printf("230 %ld bytes have already been transmitted.\r\n",transmitted);
      fflush(stdout);
      continue;
    }

    /* VERSION command? */
    if (streq(cmd,"VERSION"))
    { reply(215);
      continue;
    }

    /* COMMENT command? */
    if (streq(cmd,"COMMENT"))
    {
      /* is there an argument for "COMMENT" command? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      strcpy(comment,arg);
      reply(200);
      continue;
    }

    /* insider joke :-) */
    if (streq(cmd,"HOPPEL"))
    { reply(203);
      continue;
    }

    /* start outgoing spool daemon */
    if (streq(cmd,"START"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }

      str_toupper(arg);

      /* wrong argument? */
      if (!streq(arg,"SPOOLDAEMON"))
      { reply(501);
	continue;
      }

      /* start outgoing spool daemon if allowed */
      if (spooling && *saftserver==0)
        sfsd();
      else if (*saftserver)
      { printf("510 Your SAFT-server is: %s\r\n",saftserver);
	fflush(stdout);
      } else
	reply(502);

      continue;
    }

    /* log command for outgoing files */
    if (streq(cmd,"LOG"))
    {
      /* is there an argument? */
      if (*arg==0)
      { reply(505);
	continue;
      }
      
      /* saftserver defined? */
      if (*saftserver)
      { printf("510 Your SAFT-server is: %s\r\n",saftserver);
	fflush(stdout);
	continue;
      } 

      /* check sender spool */
      utf2iso(0,sender,dummy,dummy,arg);
      if (check_userspool(sender,&n,&n)<0) continue;

      /* open user tmp log and global log */
      sprintf(tmp,"%s/.outlog",userspool);
      infd=open(tmp,O_RDONLY);
      outfd=open(OUTLOG,O_WRONLY|O_CREAT|O_APPEND,S_IRUSR|S_IWUSR);
      if (infd<0 || outfd<0) reply(421);
      
      /* append user tmp log file to global log */
      if ((bytes=read(infd,logdata,OVERSIZE))<0) reply(421);
      if (write(outfd,logdata,bytes)!=bytes) reply(421);
      
      close(infd);
      close(outfd);
      
      reply(200);
      continue;
      
    }

    /* QUIT command? */
    if (streq(cmd,"QUIT"))
    {
      /* is there a pending notification request? */
      notify_reply(&notify,notification,sender,recipient,mailto,bell,ruid,221);
      close(0);
      exit(0);
    }

    /* unknown command or syntax error */
    reply(500);

  }

}


/*
 * getline - get a command line until LF
 *
 * INPUT:  ptr  - empty string
 *
 * OUTPUT: ptr  - string containing the command line
 *
 * RETURN: number of read bytes
 */
int getline(char *ptr)
{ int c,        /* one character */
      n,        /* number of read bytes */
      ctrl=0;	/* flag for non-ASCII character */

  ptr[0]=0;

  /* get max MAXLEN characters */
  for (n=0; n<MAXLEN-1; n++)
  {
    /* next char from socket */
    c=getchar();

    /* quit if there is no more a connection to the client */
    if (c==EOF) reply(421);

    /* surpress non-ASCII chars */
    if (c<9 || c==11 || c==12 || (c>13 && c<32) || c>126)
    { ctrl=1;
      n--;
    }
    else
    {
      /* copy the char into the command line string */
      ptr[n]=c;

      /* check for EOL */
      if (c=='\n') break;
      if (c=='\r') n--;

    }
  }

  /* input line overrun? */
  if (n==MAXLEN-1 && ptr[n]!='\n')
  {
    /* read to next lf */
    while (c!='\n') c=getchar();
    n=0;
    reply(506);
  } else
    if (ctrl) reply(205);

  ptr[n]=0;

  /* trim all white spaces */
  if (n) str_trim(ptr);

  return(strlen(ptr));
}


/*
 * writeheader - write one line of header information to log file
 *
 * INPUT: fd		- file descriptor
 *        attribute	- name of the header attribute
 *        value		- contents of the header attribute
 */
void writeheader(int fd, const char *attribute, const char *value)
{ int hsize;			/* header string size */
  char header[2*MAXLEN];	/* header string */

  sprintf(header,"%s\t%s\n",attribute,value);
  hsize=strlen(header);
  if (write(fd,header,hsize)<hsize) reply(412);
}


/*
 * notify_reply  - notify user and sent reply code if given a fatal error 
 *		   (reply code 4xx) will terminate sendfiled
 *
 * INPUT:  notify	- notify flag
 *         notification	- kind of notification
 *	   sender	- sender name
 *         recipient	- local recipient
 *         mailto	- address to send mail to
 *         bell		- flag for adding bell
 *         uid		- recipient user id
 * 	   replycode	- server reply code to send
 */
void notify_reply(int *notify, char notification,
		  const char *sender, const char *recipient, const char *mailto,
		  int bell, int uid, int replycode)
{ char msg[3*MAXLEN];	/* message to user-tty */

  if (*notify)
  {
#ifdef HAEGAR
    sprintf(msg,"\r\n\n>>> INFO <<< SAFT-Daemon has a new file for you!"
	        "\n\nFrom: %s ( saft://%s/%s )\n\nType \"receive\".\r\n\n",
	    sender,localhost,recipient);
#else
    sprintf(msg,"\r\n%s has sent a file to you (%s@%s). Type \"receive\".\r\n",
	    sender,recipient,localhost);
#endif
    if (bell) strcat(msg,"\007");
    switch (notification)
    { case 'b': msg2tty(recipient,msg,uid,O_NONBLOCK);
      case 'm': mail2user(mailto,sender,msg,uid); break;
      case 't': msg2tty(recipient,msg,uid,O_NONBLOCK);
    }
  }

  *notify=0;

  /* send reply if given */
  if (replycode) reply(replycode);
}


/*
 * msg2tty - send a one line message to the recipient (tty(s) or FIFO)
 *
 * INPUT:  recipient	- recipient of the message
 *         msg		- the message
 *         uid		- uid of recipient
 *         mode		- non-blocking or synchronous mode
 *
 * RETURN: 0 if successfull, -1 if failed
*/
int msg2tty(const char *recipient, const char *msg, int uid, int mode)
{ char
    *cp,		/* simple character pointer */
    tty[FLEN],		/* name of tty */
    user[9],		/* username */
    msgwhere[MAXLEN],	/* where to write message to */
    fifo[MAXLEN],	/* fifo name */
    msgcf[MAXLEN];	/* message control file */
  int
    utmpfd, 		/* file descriptor for utmp */
    mfd, 		/* message file descriptor */
    success;		/* return code */
  struct utmp uinfo;	/* information about a user */
  struct stat finfo;	/* information about a file */
  FILE *inf;		/* input file */

  success=0;
  user[8]=0;
  *msgwhere=0;

  sprintf(msgcf,"%s/config/tty@%s",userspool,localhost);

  /* cross fingers that this is not on a NFS */
  sprintf(fifo,"/tmp/msg.%s",recipient);

  /* change effective uid to recipient for security reasons */
  if (uid) seteuid(uid);

  /* is there a message fifo? */
  if (stat(fifo,&finfo)==0 && S_ISFIFO(finfo.st_mode))
  {
    /* is it locked by a chat client? */
    mfd=open(fifo,O_WRONLY|O_NONBLOCK);
    if (mfd>0 && tlock_file(mfd)==1)
    {
      /* strip off bell */
      if ((cp=strrchr(msg,7))) *cp=0;

      /* write to fifo */
      close(mfd);
      mfd=open(fifo,O_WRONLY|mode);
      success=write(mfd,msg,strlen(msg));
    }

    close(mfd);
  }

  /* is there a message control file? */
  if (success<=0 && (inf=fopen(msgcf,"r")))
  {
    /* read the tty name */
    fgets(tty,MAXLEN-1,inf);
    if ((cp=strchr(tty,'\n'))) *cp=0;
    fclose(inf);

    /* belongs the tty to the recipient and is it writable? */
    if (stat(tty,&finfo)==0 && finfo.st_uid==uid &&
	((finfo.st_mode&S_IWOTH) || (finfo.st_mode&S_IWGRP)))
    {
      /* write to dedicated tty */
      mfd=open(tty,O_WRONLY|mode);
      success=write(mfd,msg,strlen(msg));
      close(mfd);
    }
  }

  /* no write success so far? */
  if (success<=0)
  { success=0;

    /* search the utmp file (not standarisized, grrrr) */
    utmpfd=open(_PATH_UTMP,O_RDONLY);
    if (utmpfd<0) utmpfd=open("/var/adm/utmp",O_RDONLY);
    if (utmpfd<0) utmpfd=open("/var/run/utmp",O_RDONLY);
    if (utmpfd<0)
    { if (uid) seteuid(0);
      return(-1);
    }

    /* scan through utmp (currently logged in users) */
    while (read(utmpfd,(char *)&uinfo,sizeof(uinfo))>0)
    {
#if defined(NEXT) || defined(BSD) || defined(ULTRIX)
      strncpy(user,uinfo.ut_name,8);
      if (streq(recipient,user))
      {
#ifdef JEDPARSE
}
#endif
#else
      strncpy(user,uinfo.ut_user,8);
      if (uinfo.ut_type==USER_PROCESS && streq(recipient,user)) {
#endif
	/* get the tty */
	sprintf(tty,"/dev/%s",uinfo.ut_line);

	/* is the tty writeable? */
	if (stat(tty,&finfo)==0 &&
	    ((finfo.st_mode&S_IWOTH) || (finfo.st_mode&S_IWGRP)))
	{ mfd=open(tty,O_WRONLY|mode);

	  /* write message to tty */
	  success=success | write(mfd,msg,strlen(msg));
	  close(mfd);

	}
      }
    }
    close(utmpfd);
  }

  /* reset uid to root */
  if (uid) seteuid(0);

  if (success>0) return(0); else return(-1);
}


/*
 * mail2user - send the recipient a mail
 *
 * INPUT:  recipient	- recipient of the mail
 *         sender   	- sender of the file
 *         msg		- the message
 *         uid		- uid of local recipient
*/
void mail2user(const char *recipient, const char *sender, const char *msg,
	       int uid)
{ char *cp,		/* simple character pointer */
       line[MAXLEN],	/* one text line */
       cmd[MAXLEN];	/* command for pipe */
  FILE *pin,		/* input pipe */
       *pout;		/* output pipe */

  /* change uid to recipient */
  if (uid) setuid(uid);

  /* strip off bell */
  if ((cp=strchr(msg,7))) *cp=0;

  /* delete ' in sendername */
  while ((cp=strchr(sender,'\''))) *cp=' ';

  /* open pipe to sendmail */
  sprintf(cmd,SENDMAIL" %s",recipient);
  pout=popen(cmd,"w");

  /* fill out mail message */
  if (pout)
  { /* fprintf(pout,"From: %s\n",recipient); */
    fprintf(pout,"To: %s\n",recipient);
    fprintf(pout,"Subject: new file from %s\n\n",sender);

    /* try to open receive pipe */
    pin=popen(BINDIR"/receive -l","r");
    if (fgets(line,FLEN,pin)==NULL)
    { pclose(pin);
      pin=popen("receive -l","r");
      if (fgets(line,FLEN,pin)==NULL)
      { pclose(pin);
	pin=popen("/client/bin/receive -l","r");
	if (fgets(line,FLEN,pin)==NULL)
	{ pclose(pin);
	  pin=NULL;
	}
      }
    }

    /* add output from receive command, too */
    if (pin)
    { fprintf(pout,"\n");
      while (fgets(line,MAXLEN-1,pin)) fprintf(pout," %s",line);
      pclose(pin);
    }

    fprintf(pout,".\n");
    pclose(pout);
  }

  /* reset uid to root */
  if (uid) setuid(0);
}


/*
 * spoolid - find the next spool id and touch header and data file
 *
 * INPUT:  maxfiles  - maximum number of allowed spool files
 *
 * RETURN: spool id, 0 if failed
 *
 * The working directory has to be the user spool.
 */
int spoolid(int maxfiles)
{ int
    i,			/* simple loop count */
    fd,			/* file descriptor of spool id header file */
    id,			/* id number */
    idmax;		/* biggest id number */
  char
    *cp, 		/* character pointer to '.' in file name */
    file[MAXLEN];	/* complete file name */
#ifdef NEXT
  char tmp[MAXLEN];	/* tmp string */
  FILE *pp;		/* pipe */
#else
  struct dirent *dire;	/* directory entry */
  DIR *dp;		/* directory pointer */
#endif


  /* try to create next spool files */
  for (i=1; i<5; i++)
  {
    /* initialisize */
    id=0;
    fd=0;
    idmax=0;

#ifdef NEXT
    /* stupid NeXT has a broken readdir(); this is a dirty workaround */

    /* open spool dir */
    if ((pp=popen("ls . 2>/dev/null","r")) == NULL) return(NULL);

    /* scan through spool directory */
    while (fgets(tmp,MAXLEN-1,pp))
    {
      if ((cp=strrchr(tmp,'\n'))) *cp=0;
      cp=strchr(tmp,'.');
      if (cp && streq(cp,".h"))
      { *cp=0;
        id=atoi(tmp);
	if (id>idmax) idmax=id;
      }
    }
    pclose(pp);

#else
    /* open spool dir */
    dp=opendir(".");

    /* scan through spool directory and get the highest spool id */
    while ((dire=readdir(dp)))
    { cp=strchr(dire->d_name,'.');
      if (cp && streq(cp,".h"))
      { *cp=0;
        id=atoi(dire->d_name);
	if (id>idmax) idmax=id;
      }
    }
    closedir(dp);
#endif

    id=idmax+1;
    if (id>maxfiles) return(id);

    /* try to create header spool file */
    sprintf(file,"%d.h",id);
    fd=open(file,O_CREAT|O_EXCL,S_IRUSR|S_IWUSR);

    /* successfull? */
    if (fd>=0)
    {
      close(fd);

      /* set owner and group */
      /* chown(file,uid,gid); */

      /* create data spool file */
      sprintf(file,"%d.d",id);
      close(open(file,O_CREAT,S_IRUSR|S_IWUSR));
      /* chown(file,uid,gid); */

      return(id);
    }

    /* wait and test again */
    sleep(1);
  }

  /* failed */
  return(0);
}


/*
 * restricted  - check killfile
 *
 * INPUT:  sender     - sender name
 *         recipient  - local recipient
 *         type       - type of restriction: m(essage) or f(ile)
 *
 * RETURN: 1 if not allowed to send, 0 if allowed
 */
int restricted(const char *sender, const char *recipient, char type)
{ int m;
  char *cp,
       killfile[MAXLEN],
       kfu[MAXLEN],
       kfm[MAXLEN],
       line[MAXLEN],
       from[MAXLEN];
  FILE *inf;

  sprintf(killfile,"%s/config/restrictions",userspool);
  *kfm=*kfu=0;

  /* open and check killfile */
  inf=fopen(killfile,"r");
  if (inf==NULL) return(0);

  strcpy(from,sender);
  if ((cp=strchr(from,' '))) *cp=0;

  while (fgets(line,MAXLEN-1,inf))
  { line[MAXLEN-1]=0;
    sscanf(line,"%s%s",kfu,kfm);
    if (kfm[0]==0)
      m='b';
    else
      m=tolower(kfm[0]);
    kfm[1]=0;
    if (simplematch(from,kfu,1) && (m==type || m=='b'))
    { fclose(inf);
      return(1);
    }
  }

  fclose(inf);
  return(0);
}


/*
 * wlock_file - write-lock a file (POSIX conform)
 *
 * INPUT:  file descriptor
 *
 * RETURN: >= 0 if ok, -1 if error
 */
int wlock_file(int fd)
{ struct flock lock;	/* file locking structure */

  /* fill out the file locking structure */
  lock.l_type=F_WRLCK;
  lock.l_start=0;
  lock.l_whence=SEEK_SET;
  lock.l_len=0;

  /* try to lock the file and return the status */
  return(fcntl(fd,F_SETLK,&lock));
}


/*
 * tlock_file - test if a file is write-lock blocked (POSIX conform)
 *
 * INPUT:  fd  - file descriptor
 *
 * RETURN: 0 if no lock, 1 if locked, -1 on error
 */
int tlock_file(int fd)
{ int status;
  struct flock lock;    /* file locking structure */

  /* fill out the file locking structure */
  lock.l_type=F_WRLCK;
  lock.l_start=0;
  lock.l_whence=SEEK_SET;
  lock.l_len=0;

  /* test the lock status */
  status=fcntl(fd,F_GETLK,&lock);
  if (status>=0) status=(lock.l_type!=F_UNLCK);
  return(status);
}


/* void function for message() */
void cleanup() { }


/*
 * sfsd  - sendfile spool daemon
 */
void sfsd()
{ int
    lockf,		/* lock file descriptor */
    error,		/* error flag */
    status,		/* return status from send_data */
    sockfd,     	/* socket file descriptor */
    devnull,            /* file descriptor for child */
    bounce,		/* days after files were bounced to sender */
    retry;		/* minutes to wait for retrying to deliver */
  char
    *cp,		/* simple char pointer */
    *argp,		/* argument string pointer */
    *rs,		/* reply string from the remote server */
    sender[FLEN],	/* sender user name */
    tmp[MAXLEN],	/* tmp string */
    lockfn[MAXLEN],	/* lock file name */
    cmd[MAXLEN],	/* command for popen */
    line[MAXLEN],	/* incoming command line */
    dummy[MAXLEN],	/* dummy string for utf2iso name conversion */
    mailfn[FLEN],	/* status log mail file name */
    osdfn[MAXLEN];	/* outgoing spool data file name */
  pid_t pid;		/* process id */
  time_t timetick;	/* unix time (in seconds) */
  struct hostlist
    *hls,		/* host list start */
    *hlp;		/* host list pointer */
  struct outfilelist
    *oflp;		/* outgoing file list pointer */
  struct stat finfo;	/* information about a file */
  FILE *inf,		/* input file */
       *mailf,		/* mail file */
       *pout;		/* output pipe */
#ifdef NEXT
  FILE *dp;		/* directory pipe */
#else
  struct dirent *dire;	/* directory entry */
  DIR *dp;		/* directory pointer */
#endif

  retry=10;
  bounce=5;
  sprintf(lockfn,"%s/.lock",outgoing);
  
  /* try to create lock file */
  if ((lockf=open(lockfn,O_WRONLY|O_CREAT,S_IRUSR|S_IWUSR))<0) reply(421);
  
  /* check if there is a lock (sfsd already running) */
  if (wlock_file(lockf)<0)
  { close(lockf);
    reply(200);
    return;
  }

  /* establish simple signal handler for child process termination */
  signal(SIGCHLD,sigchld);

  /* spawn subprocess */
  pid=fork();
  if (pid<0) reply(453);
  if (pid==0) reply(200);

  /* is this the parent process? */
  if (pid!=0) return;

  /* disconnect from client */
  devnull=open("/dev/null",O_RDWR);
  dup2(devnull,fileno(stdin));
  dup2(devnull,fileno(stdout));
  dup2(devnull,fileno(stderr));

  if (chdir(outgoing)<0) exit;

  /* parse the config-file */
  if ((inf=fopen(config,"r")))
  { while (fgets(line,MAXLEN-1,inf))
    {
      /* prepare line to be parsed */
      if ((cp=strchr(line,'#'))) *cp=0;
      if ((cp=strchr(line,'='))) *cp=' ';
      str_tolower(str_trim(line));

      /* is there an option and an argument? */
      if ((argp=strchr(line,' ')))
      { *argp=0; argp++;
	if (streq(line,"bounce"))
	{ bounce=atoi(argp);
	  continue;
	}
	if (streq(line,"retry"))
	{ retry=atoi(argp);
	  continue;
	}
      }

    }
    fclose(inf);
  }

  if (retry<=0) retry=10;
  if (bounce<=0) bounce=5;
    
  /* run in endless loop */
  for (;;)
  {
    /* scan outgoing spool */
    check_outspool(bounce);
    hls=scanoutspool("");
    if (!hls) exit;

    /* fork an extra spool daemon for every host */
    for (hlp=hls; hlp; hlp=hlp->next)
    { if (fork()>0)
      {
	/* initiate the connection to the server */
	sockfd=open_connection(hlp->host,SAFT);
	if (sockfd<0) exit(0);

	/* no remote server or protocol error? */
	sock_getline(sockfd,line);
	if (!strbeq(line,"220 ") || !strstr(line,"SAFT")) exit(0);

	for (oflp=hlp->flist; oflp; oflp=oflp->next)
	{
	  error=0;
	  inf=fopen(oflp->oshfn,"r");

	  while (fgets(line,MAXLEN-1,inf))
	  {
	    if ((cp=strchr(line,'\n'))) *cp=0;

	    /* check the to line */
	    if (strbeq(line,"TO") && (cp=strchr(line,'@'))) *cp=0;

	    sock_putline(sockfd,line);
	    rs=getreply(sockfd);

	    if (!strbeq(rs,"200 "))
	    { error=1;
	      break;
	    }

	  }

	  fclose(inf);
	  if (error) continue;

	  sock_putline(sockfd,"DATA");
	  rs=getreply(sockfd);
	  if (!strbeq(rs,"302 ")) continue;

	  strcpy(osdfn,oflp->oshfn);
	  osdfn[strlen(osdfn)-1]='d';
	  stat(osdfn,&finfo);

	  /* send file data and check return status */
	  status=send_data(sockfd,finfo.st_size,osdfn,osdfn,0,3);
	  if (status>=0)
	  {
	    /* write status log */
	    sprintf(tmp,SPOOL"/LOG/%s:%s",oflp->from,hlp->host);
	    mailf=fopen(tmp,"a");
	    chmod(tmp,S_IRUSR|S_IWUSR);
	    sprintf(tmp,"%s to %s",oflp->fname,oflp->to);
	    utf2iso(0,line,dummy,dummy,tmp);
	    timetick=time(0);
	    strftime(tmp,21,DATEFORMAT,localtime(&timetick));
	    fprintf(mailf,"sending %s@%s at %s : ",line,hlp->host,tmp);
	    if (status)
	      fprintf(mailf,"already transmitted\n");
	    else
	      fprintf(mailf,"ok\n");
	    unlink(oflp->oshfn);
	    unlink(osdfn);
	    fclose(mailf);
	  }
	}

	/* look for status log files */
	if (chdir(SPOOL"/LOG")<0) exit(0);
#ifdef NEXT
	/* stupid NeXT has a broken readdir(); this is a dirty workaround */

	/* open LOG dir */
	sprintf(cmd,"ls *:%s 2>/dev/null",hlp->host);
	if ((dp=popen(cmd,"r")) == NULL) exit(0);

	/* scan through LOG directory */
	while (fgets(mailfn,MAXLEN-1,dp))
	{
	  if ((cp=strrchr(mailfn,'\n'))) *cp=0;
#ifdef JEDPARSE
}
#endif
#else
	/* open LOG dir */
	dp=opendir(".");

	/* scan through LOG directory */
	while ((dire=readdir(dp)))
	{
	  strcpy(mailfn,dire->d_name);
	  sprintf(tmp,"*:%s",hlp->host);
	  if (simplematch(mailfn,tmp,0)<1) continue;
#endif
	  mailf=fopen(mailfn,"r");
	  if (!mailf) continue;

	  strcpy(sender,mailfn);
	  cp=strchr(sender,':');
	  *cp=0;

	  /* open pipe to sendmail */
	  sprintf(cmd,SENDMAIL" %s",sender);
	  pout=popen(cmd,"w");
	  if (!pout) continue;

	  /* fill out mail message */
	  fprintf(pout,"To: %s\n",sender);
	  fprintf(pout,"Subject: sendfile spool daemon status report\n\n");
	  while (fgets(line,MAXLEN-1,mailf)) fprintf(pout,"%s",line);
	  pclose(pout);
	  fclose(mailf);

	  unlink(mailfn);

	}
#ifdef NEXT
	pclose(dp);
#else
	closedir(dp);
#endif
	exit;
      }
    }

    sleep(retry*60);
  }

}


/*
 * check_outspool  - check outgoing spool if there are expired files and
 *                   bounce them to the sender's incoming spool
 *
 * INPUT: bounce  - number of days after file is expired
 */
void check_outspool(int bounce)
{ int
    id;				/* spool file id */
  char
    *cp,			/* simple character pointer */
    *arg,			/* the argument(s) of the header line */
    hline[MAXLEN],		/* header line */
    tmp[MAXLEN],		/* temporary string */
    shfn[MAXLEN],		/* spool header file name */
    sdfn[MAXLEN],		/* spool data file name */
    oshfn[MAXLEN],		/* outgoing spool header file name */
    osdfn[MAXLEN];		/* outgoing spool data file name */
  time_t timetick;		/* unix time (in seconds) */
  FILE
    *inf,			/* input file */
    *outf;			/* output file */
  struct stat finfo;		/* information about a file */
  struct passwd *pwe;		/* password entry struct */
#ifdef NEXT
  FILE *pp;			/* pipe */
#else
  struct dirent *dire;		/* directory entry */
  DIR *dp;			/* directory pointer */
#endif

  timetick=time(0);

  /* mega stupid NeXT has broken readdir() */
#ifdef NEXT
  /* open spool dir */
  sprintf(tmp,"ls %s/*.h 2>/dev/null",outgoing);
  if ((pp=popen(tmp,"r")) == NULL) return;

  /* scan through spool directory */
  while (fgets(oshfn,MAXLEN-1,pp))
  {
    if ((cp=strrchr(oshfn,'\n'))) *cp=0;
#ifdef JEDPARSE
}
#endif
#else
  /* open spool dir */
  if (!(dp=opendir(outgoing))) return;

  /* scan through outgoing spool directory */
  while ((dire=readdir(dp)))
  {
    /* ignore non-header files */
    sprintf(oshfn,"%s/%s",outgoing,dire->d_name);
    if (!streq(&oshfn[strlen(oshfn)-2],".h")) continue;
#endif

    if (stat(oshfn,&finfo)<0) continue;

    /* spool time expired? */
    if (timetick>finfo.st_mtime+bounce*DAYSEC)
    {
      /* get user name and spool directory */
      pwe=getpwuid(finfo.st_uid);
      sprintf(userspool,SPOOL"/%s",pwe->pw_name);

      /* create user spool directory if necessary */
      if (mkdir(userspool,S_IRUSR|S_IWUSR|S_IXUSR)==0)
	chown(userspool,pwe->pw_uid,pwe->pw_gid);

      /* change uid and get user name */
      seteuid(finfo.st_uid);

      if (chdir(userspool)<0)
      { seteuid(0);
	continue;
      }

      if ((inf=fopen(oshfn,"r")) == NULL)
      { seteuid(0);
	continue;
      }

      /* get next spool id */
      id=spoolid(MAXLEN);
      if (id==0 || id>MAXLEN)
      { fclose(inf);
	seteuid(0);
	continue;
      }

      /* set file names */
      strcpy(osdfn,oshfn);
      osdfn[strlen(osdfn)-1]='d';
      sprintf(shfn,"%d.h",id);
      sprintf(sdfn,"%d.d",id);

      if (!(outf=fopen(shfn,"w")))
      { fclose(inf);
	seteuid(0);
	continue;
      }

      /* copy header file */
      while (fgets(hline,MAXLEN-1,inf))
      {
	/* prepare the header line */
	if ((cp=strchr(hline,'\n'))) *cp=0;
	cp=strchr(hline,'\t');
	if (!cp) continue;
	arg=cp+1;
	*cp=0;

	/* ignore old COMMENT */
	if (streq(hline,"COMMENT")) continue;

	/* add new bounce COMMENT */
	if (streq(hline,"TO"))
	{ sprintf(tmp,"cannot sent to %s",arg);
	  iso2utf(arg,tmp);
	  fprintf(outf,"COMMENT\t%s\n",arg);
	  continue;
	}

	/* add other header lines */
	fprintf(outf,"%s\t%s\n",hline,arg);

      }

      fclose(inf);
      fclose(outf);

      /* copy spool data file */
      if (fcopy(osdfn,sdfn,S_IRUSR|S_IWUSR)<0)
      { 
	/* on failure delete new spool file */
	unlink(shfn);
	unlink(sdfn);
      } 
      else
      { 
	/* on success delete outgoing spool file */
	unlink(oshfn);
	unlink(osdfn);
      }

      seteuid(0);

    }
  }

  /* safety fall back */
  chdir(outgoing);
}


/*
 * check_userspool - check if user is allowed to use sendfile and create the
 *                   user spool directories
 *
 * INPUT:  user	- user login name
 * 
 * OUTPUT: uid	- user id
 *         gid	- group id
 * 
 * RETURN: 0 if ok, -1 if failed
 */
int check_userspool(char *user, int *uid, int *gid)
{ char
    *cp,			/* simple character pointer */
#ifdef NEXT
    tmp[MAXLEN], 		/* temporary string */
#endif
    luser[MAXLEN];		/* check local user */
  FILE 
#ifdef NEXT
    *pp,			/* pipe stream */
#endif
    *inf;			/* for various files */
  struct passwd *pwe;		/* password entry struct */
  
  /* get user information */
  pwe=NULL;
#ifdef NEXT
  /* stupid NeXT has a broken getpwnam(); this is a dirty workaround */
  sprintf(tmp,"( nidump passwd . ; nidump passwd / ) | "
              "awk -F: '$1==\"%s\"{print $3,$4;exit}'",user);
  pp=popen(tmp,"r");
  if (fgets(tmp,MAXLEN-1,pp) && *tmp!='\n' && *tmp!=0)
  { pwe=(struct passwd *) malloc(sizeof(struct passwd));
    sscanf(tmp,"%d %d",uid,gid);
    pwe->pw_uid=*uid;
    pwe->pw_gid=*gid;
  }
  pclose(pp);
#else
  pwe=getpwnam(user);
#endif

  /* no such user? */
  if (pwe==NULL)
  { reply(520);
    *user=0;
    return(-1);
  }

  *uid=pwe->pw_uid;
  *gid=pwe->pw_gid;

  /* look in the sendfile disallow file */
  if ((inf=fopen(NOSENDFILE,"r")) && (fgets(luser,MAXLEN-1,inf)))
  { str_trim(luser);

    /* allow-only list? */
    if (strncasecmp(luser,"allow-only",10)==0)
    { for (;;)
      { if (fgets(luser,MAXLEN-1,inf)==NULL) break;
	if ((cp=strchr(luser,'#'))) *cp=0;
	str_trim(luser);
	if (streq(luser,user)) break;
      }
      fclose(inf);

      /* is the user not in the allow list? */
      if (!streq(luser,user))
      { reply(521);
	*user=0;
	return(-1);
      }

    } else /* deny list */
    { do
      { if ((cp=strchr(luser,'#'))) *cp=0;
	str_trim(luser);
      } while (!streq(luser,user) && (fgets(luser,MAXLEN-1,inf)));
      fclose(inf);

      /* is the user in the deny list? */
      if (streq(luser,user))
      { reply(521);
	*user=0;
	return(-1);
      }

    }
  }

  /* build user spool string */
  user[32]=0;
  sprintf(userspool,SPOOL"/%s",user);
  sprintf(userconfig,"%s/config",userspool);

  /* create user spool directory for user */
  if (mkdir(userspool,S_IRUSR|S_IWUSR|S_IXUSR)==0) chown(userspool,*uid,*gid);

  /* create user config directory */
  seteuid(*uid);
  mkdir(userconfig,S_IRUSR|S_IWUSR|S_IXUSR);
  seteuid(0);

  return(0);
}


/*
 * sigchld - simple interrupt handler for SIGCHLD
 */
void sigchld() 
{ waitpid(-1,NULL,WNOHANG);
  return;
}
