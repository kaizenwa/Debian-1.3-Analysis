#ifndef _SLRN_FEATURES_H
#define _SLRN_FEATURES_H

/* If you enable local SPOOL support, you must also enable NNTP support
 * or INEWS support.  Also, see below for filenames regarding the location
 * of the local spool and inews.  Currently only unix supports this.
 * 
 * NOTE: if you intend to use slrnpull, make sure you enable SPOOL_SUPPORT
 *       in addition to PULL_SUPPORT.
 */
#define SLRN_HAS_PULL_SUPPORT	1
#define SLRN_HAS_NNTP_SUPPORT   1
#define SLRN_HAS_SPOOL_SUPPORT	0
#define SLRN_HAS_INEWS_SUPPORT	0

/* Set this to 1 to force the user to use INEWS for posting.  This also means
 * that slrn will use the hardcoded inews program pathname.
 */
#define SLRN_FORCE_INEWS	0    

#define SLRN_HAS_SPOILERS	1
#define SLRN_HAS_MIME		1
#define SLRN_HAS_SORT_BY_SCORE	1

/* If non-zero '~' characters will be used at the end of an article.  This 
 * is what the vi editor does. */
#define SLRN_HAS_TILDE_FEATURE	1

/* If non-zero, a special message will be displayed when article is the last
 * one in a thread.
 */
#define SLRN_HAS_END_OF_THREAD	1

/* See README.GroupLens.  This only works if SLRN_USE_SLTCP is non-zero. 
 * This will not work under VMS because sltcp.c will not work with VMS.
 * Also make sure NNTP support is enabled.
 */
#define SLRN_HAS_GROUPLENS	0

/* If set to 1, slrn will cache message-ids during a session and use those
 * to eliminate cross-posts.  This should not be necessary if the server 
 * supports Xref in its overview database.
 */
#define SLRN_HAS_MSGID_CACHE	0

/* Set this to 0 if you do not want the slang interpreter made available. */
#define SLRN_HAS_SLANG		1

/* Set this to 0 if you do not want slrn to generate message-id header. */
#define SLRN_HAS_GEN_MSGID	1

/* If you want mapping ISO Latin <--> native character set, set this to 1.
 * Supported character sets: isolatin and ibm850
 */
#define SLRN_HAS_CHARACTER_MAP	1

/* If you want to use uudeview's uulib instead of slrn's builtin one
 * change this to 1.  You will also need to edit the Makefile to point to
 * the uudeview include file and and library.  Only enable this if you
 * have uudeview installed on your system.
 */
#define SLRN_HAS_UUDEVIEW	0


/* The SLTCP code has not been ported to VMS due to the lack of a standardized
 * tcp-ip interface on VMS.  It should be ok on Unix and OS/2.
 */
#ifndef VMS
# define SLRN_HAS_DECODE	1
# define SLRN_HAS_PIPING	1
# define SLRN_USE_SLTCP 	1
#else
# define SLRN_HAS_PIPING	0
# define SLRN_HAS_DECODE	0
# define SLRN_USE_SLTCP		0
#endif

/* If non-zero, an rn style lock file will be created if .newsrc is the newsrc
 * file.
 */
#if defined(VMS) || defined(__os2__)
# define SLRN_HAS_RNLOCK	0
#else
# define SLRN_HAS_RNLOCK	1
#endif

/* ----------------  LOCAL SPOOL and INEWS filenames and configuration ----------------------- */

#if SLRN_HAS_INEWS_SUPPORT
  /* Note the -S flag.  slrn appends the signature to the file to be posted and
   * the -S flag tells inews not to also do this.  The -h flag must be used.
   */
# define SLRN_INEWS_PROGRAM      "/usr/local/bin/inews -S -h"
#endif

#if SLRN_HAS_SPOOL_SUPPORT || defined(SLRNPULL_CODE)

  /* Root directory names */
# define SLRN_SPOOL_ROOT	"/export/news"
  /* SLRN_NOV_ROOT gives the root directory for overview files
   * if you don't have overview files, leave as SLRN_SPOOL_ROOT for now */
# define SLRN_SPOOL_NOV_ROOT	SLRN_SPOOL_ROOT
  /* SLRN_NOV_FILE gives filename for overview file in each directory */
# define SLRN_SPOOL_NOV_FILE	".overview"

# define SLRN_SPOOL_INNROOT	"/export/opt/inn"
  /* If the following filenames are relative ones, they are considered to be
   * relative to SLRN_SPOOL_INNROOT.
   */
# define SLRN_SPOOL_ACTIVE	"data/active"
# define SLRN_SPOOL_ACTIVETIMES	"data/active.times"
# define SLRN_SPOOL_NEWSGROUPS	"data/newsgroups"

  /* set to 1 to allow scanning the active file for article ranges if there's
   * no .overview file -- if 0 or no active file, then look at filenames in
   * the spool directory instead.  0 seems best... */
# define SPOOL_ACTIVE_FOR_ART_RANGE 0
#endif

/* Default Startup mode.  Should slrn use spool or nntp?  What about default
 * posting agent?
 */
#if SLRN_HAS_NNTP_SUPPORT
# define SLRN_DEFAULT_SERVER_OBJ	SLRN_SERVER_ID_NNTP
# define SLRN_DEFAULT_POST_OBJ		SLRN_POST_ID_NNTP
#else
# define SLRN_DEFAULT_SERVER_OBJ	SLRN_SERVER_ID_SPOOL
# define SLRN_DEFAULT_POST_OBJ		SLRN_POST_ID_INEWS
#endif

#if SLRN_HAS_INEWS_SUPPORT && SLRN_FORCE_INEWS
# undef SLRN_DEFAULT_POST_OBJ
# define SLRN_DEFAULT_POST_OBJ SLRN_POST_ID_INEWS
#endif



/* ---------------- end of INEWS and LOCAL SPOOL configuration ----------- */

#ifdef VMS
/* 
 *                                                            VMS filenames
 */
# define SLRN_USER_SLRNRC_FILENAME	"slrn.rc"
# define SLRN_LETTER_FILENAME		"slrn-letter.txt"
# define SLRN_ARTICLE_FILENAME		"slrn-article.txt"
# define SLRN_FOLLOWUP_FILENAME		"slrn-followup.txt"
# define SLRN_SIGNATURE_FILE		".signature"
# ifndef SLRN_LIB_DIR
#  define SLRN_LIB_DIR			"sys$manager:"
# endif
#else
# ifdef __os2__
/*
 *                                                             OS/2 filenames
 */
#  define SLRN_USER_SLRNRC_FILENAME	"slrn.rc"
#  define SLRN_LETTER_FILENAME		"letter.txt"
#  define SLRN_ARTICLE_FILENAME		"article.txt"
#  define SLRN_FOLLOWUP_FILENAME	"followup.txt"
#  define SLRN_SIGNATURE_FILE		"signatur.txt"
#  define SLRN_SENDMAIL_COMMAND		"sendmail -t -af"
#  ifndef SLRN_LIB_DIR
#    define SLRN_LIB_DIR		"C:/os2"
#  endif
# else
#  ifdef __unix__
/* 
 *                                                             Unix filenames
 */
#   define SLRN_USER_SLRNRC_FILENAME	".slrnrc"
#   define SLRN_LETTER_FILENAME	".letter"
#   define SLRN_ARTICLE_FILENAME	".article"
#   define SLRN_FOLLOWUP_FILENAME	".followup"
#   define SLRN_SIGNATURE_FILE		".signature"
#   define SLRN_SENDMAIL_COMMAND	"/usr/lib/sendmail -oi -t -oem -odb"
#   ifndef NNTPSERVER_FILE
#    define NNTPSERVER_FILE		"/usr/local/lib/news/nntp_server"
#   endif
#   ifndef SLRN_LIB_DIR
#    define SLRN_LIB_DIR		"/usr/local/lib/slrn"
#   endif
#  endif			       /* unix */
# endif				       /* os2 */
#endif				       /* vms */

/* #define OUR_ORGANIZATION "organization-name" */
/* #define OUR_HOSTNAME "host.name.here" */
/* #define NNTPSERVER_NAME  "my.server.name" */

#if SLRN_HAS_GROUPLENS
# undef SLRN_USE_SLTCP
# define SLRN_USE_SLTCP 1
#endif

/* The rest of the files apply to slrnpull. */

/* This must be set to an absolute pathname. */
#define SLRNPULL_ROOT_DIR	"/var/spool/news/slrnpull"

/* The remaing variables are specified as relative names with respect to the
 * SLRNPULL_ROOT_DIR.
 */

/* slrnpull configuration filename. */
#define SLRNPULL_CONF		"slrnpull.conf"

/* slrnpull outgoing post directory. */
#define SLRNPULL_OUTGOING_DIR	"out.going"

/* The file that will be used for killing articles as they are fetched. */
#define SLRNPULL_SCORE_FILE	"score"

/* All news article retrived from the server will be placed in newsgroup
 * subdirectories in this directory.
 */
#define SLRNPULL_NEWS_DIR	"news"

/* File where messages and errors will be placed. */
#define SLRNPULL_LOGFILE	"log"

#if SLRN_HAS_PULL_SUPPORT && !SLRN_HAS_SPOOL_SUPPORT
# undef SLRN_HAS_PULL_SUPPORT
# define SLRN_HAS_PULL_SUPPORT 0
#endif

#if !SLRN_HAS_NNTP_SUPPORT
# undef SLRN_HAS_GROUPLENS
# define SLRN_HAS_GROUPLENS 0
#endif

#endif				       /* _SLRN_FEATURES_H */
