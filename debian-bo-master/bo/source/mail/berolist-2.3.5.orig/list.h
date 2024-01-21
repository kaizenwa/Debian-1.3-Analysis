/*  BeroList Mailing List - Configuration file                    
    (c) 1996 by Bernhard Rosenkraenzer, root@startrek.in-trier.de */

/* VERSION will be used in error messages */
#define VERSION "BeroList-2.3.5"

/* Comment out if you do not want BeroList to create log files */
#define LOG

/* Location of logfiles - must be writable by bin */
#define LOGDIR "/usr/inet/lists/logs"

/* Directory of list members, configs, etc. - must be read-writable by bin */
#define LISTDIR "/usr/inet/lists/lists"

/* File containing list of users that may not send messages to the list,
   see README */
#define KILLFILE "/usr/inet/lists/killfile"

/* SMTP server used to send messages   */
/* localhost should work in most cases */
#define SMTP_SERVER "localhost"

/* NNTP server settings used to send copies of messages to newsgroups */
/* Check README for details.                                          */
#define HAS_NNTP
#ifdef HAS_NNTP
	#define DEFAULT_NNTP_SERVER "localhost"
	#define DEFAULT_NNTP_DISTRIBUTION "local"
#endif


/* Define ARCHIVE if you want to keep a GDBM archive of messages */
#define ARCHIVE

/* Hostname for mailing list server
   If HOST is not defined, BeroList will try to determine it itself
   This is _NOT_ recommended. (see README) */
#define HOST "ufp.in-trier.de"

/* Maximum number of members for a list */
#define MAX_MEMBERS 1000

/* Uncomment the following lines if you do not want sender and list addresses
   in your message */
/* #define NO_SENDER_ADDRESS */
/* #define NO_LIST_ADDRESS   */   

/* If USE_SENDMAIL is defined, BeroList will use sendmail to send out
   messages (the 1.x way).                                            */
/* #define USE_SENDMAIL                                               */

/* Path to sendmail - required if USE_SENDMAIL is defined. */
#ifdef USE_SENDMAIL
	#define SENDMAIL "/usr/bin/sendmail"
	/* define TRUSTED if 'bin' may run sendmail -f - RECOMMENDED
	Not necessary if USE_SENDMAIL is undefined. */
	/* #define TRUSTED */
#endif

/* That's it. Go on and run configure and make. */
