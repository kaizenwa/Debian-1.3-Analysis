/* -*- c -*-
 *
 * Author:      James Brister <brister@vix.com> -- berkeley-unix --
 * Start Date:  Wed Dec 27 08:34:23 1995
 * Project:     INN (innfeed)
 * File:        config.h
 * RCSId:       $Id: config.h,v 1.14 1996/12/07 01:29:51 brister Exp $
 *
 * Copyright:   Copyright (c) 1996 by Internet Software Consortium
 *
 *              Permission to use, copy, modify, and distribute this
 *              software for any purpose with or without fee is hereby
 *              granted, provided that the above copyright notice and this
 *              permission notice appear in all copies.
 *
 *              THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE
 *              CONSORTIUM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *              SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *              MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET
 *              SOFTWARE CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT,
 *              INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *              WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *              WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 *              TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 *              USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Description: The application configuration values. This file is #include'd
 *              before any system header files, so it can't rely on any CPP
 *              symbols other that what the compiler defines.
 * 
 */


#if 0

Many of the values in here will be eventually moved to a run-time config
file.

#endif


#if ! defined ( config_h__ )
#define config_h__

  /* Go edit sysconfig.h for platform differences */
#include "sysconfig.h"

/* set this to 1 if you want innfeed.status to be wrapped in HTML code. */
#define GEN_HTML 0

  /**********************************************************************/
  /*                     Application specific defines                   */
  /**********************************************************************/

  /* same as INN's value. Yes, this will eventually come from inn's config
     files. */
#define NEWSSPOOL "/var/spool/news"

  /* the path to the run-time config file */
#define CONFIG_FILE "/var/tmp/innfeed/innfeed.conf"

  /* where to scribble the process id */
#define PID_FILE "/var/tmp/innfeed/innfeed.pid"

  /* the path to a log file where stdout and stderr will go if they're
     pointing at /dev/null on program start. This will catch any fatal
     messages that syslog didn't. Can override with '-l' option. If stderr
     and stdout are going to a tty then this is not used. */
#define LOG_FILE "/var/tmp/innfeed/log/innfeed.log"

  /* where the backlog files get stored */
#define TAPE_DIRECTORY "/var/tmp/innfeed/tapes"

  /* the full pathname of the file to get a printed dump of the system when
     a SIGINT is delivered (or SNAPSHOT_ON_DIE is non-zero--see below). */
#define SNAPSHOT_FILE "/var/tmp/innfeed/innfeed.snapshot"

  /* If this file exists at startup then it's the same as having done '-d
     1' on the command line. This is a cheap way of avoiding continual
     reloading of the newsfeeds file when debugging. */
#define DEBUG_FILE "/var/tmp/innfeed/innfeed.debug"

  /* if defined to a non-zero number, then a snapshot will be printed
     whenever die() is called (e.g. on assert failure). This can use up a
     lot of disk space. */
#define SNAPSHOT_ON_DIE 0

/* Define this be an existing directory (or NULL). If innfeed deliberatly
   dumps core it will chdir() to this directory first (if non-NULL) */
#define CORE_DIRECTORY TAPE_DIRECTORY

  /* strings that get added to the end of a peer name for generating
     backlog file names.  A peername cannot end in any of these string
     (e.g. having a peer called 'mypeer.input' will not work) */
#define OUTPUT_TAIL ".output"
#define INPUT_TAIL ".input"
#define LOCK_TAIL ".lock"
#define CKPT_TAIL ".checkpoint"

  /* number of articles a tape has in memory before it dumps them to disk */
#define TAPE_HIGHWATER 5

  /* the default number of seconds between checkpointing the tapes. */
#define TAPE_CHECKPOINT_PERIOD 30

  /* the maxmmum number of articles a host has backed up in its queue before
     giving them to its tape. */
#define HOST_HIGHWATER 10

  /* rough estimate of average article line length (including
     headers). Smaller number means more efficient article preparation (for
     transfer), but, if much smaller than reality, then more memory
     wastage. */
#define CHARS_PER_LINE 60

  /* set this to 1 if you want all the connections to a host to log their
     respective stats when the Host logs the summary */
#define LOG_CONNECTION_STATS 0

  /* How many seconds between logging statistics on transfers */
#define STATS_PERIOD (60 * 10)  /* 10 minutes */

  /* how many seconds before issuing a 'final' stats log and resetting all
     the counters (if innfeed runs for more than a day it's tricky to
     generate meaningful reports with innlog). */
#define STATS_RESET_PERIOD (60 * 60 * 12) /* 12 hours */

  /* How many seconds between logging statistics on article allocation.
     For no logging set to 0 */
#define ARTICLE_STATS_PERIOD (10 * 60) /* 10 minutes */

  /* The number of seconds we first wait for trying to reconnect on a
     failure. */
#define INITIAL_REESTABLISHMENT_PERIOD 30

  /* maximum number of seconds between restablishment attempt. The timeout
     period doubles on each failure. */
#define MAX_REESTABLISHMENT_PERIOD (60 * 60 * 6)/* 6 hours */

  /* Default number of seconds before closing down connections and setting
     them back up again. This is because some nntp servers hold the history
     file open and a long running innfeed gives those systems problems....  */
#define CLOSE_PERIOD (60 * 60)   /* 1 hour */

  /* max number of parallel connections to a single remote. This is just a
     sanity check for the runtime config file. */
#define MAX_CONNECTION_COUNT 50

  /* default size in bytes for buffers */
#define BUFFER_SIZE 256

  /* amount we expand buffers on partial reads */
#define BUFFER_EXPAND_AMOUNT 128 

  /* minimum number of seconds between log messages for starting
     spooling. i.e. if the connection bounces up and down this will prevent
     frequent logging of the spooling message. 0 turns off this logging. */
#define SPOOL_LOG_PERIOD 600

  /* some big numbers just for sanity checking */
#define MAX_MAXCHECKS 10000     /* no more than 10000 articles at a time */
#define MAX_MAXART_TOUT 86400   /* one day max between articles from inn */
#define MAX_RESP_TOUT 3600      /* one hour max to wait for response */

  /* Range limits for low-pass filter for no-CHECK mode. Stay within
     (0,+10) */
#define LOW_PASS_FILTER_ON 8.0
#define LOW_PASS_FILTER_OFF 7.5

  /* the maximum number of peers we'll handle (not connections) */
#define MAX_HOSTS 50

  /* If we have more that this much article contents in memory then we've
     probably got a memory leak so we print a snapshot and dump core. */
#define SOFT_ARTICLE_BYTE_LIMIT (1024 * 1024 * 10) /* 10MB */

/* define SELECT_RATIO to the number of times through the main loop before
   checking on the fd from inn again.... */
#define SELECT_RATIO 3

#if defined (DO_BIND_USE_SIZEOF)
#define AF_UNIX_SOCKSIZE(S)     (sizeof S)  
#else
#define AF_UNIX_SOCKSIZE(S)     (sizeof S.sun_family + strlen(S.sun_path) + 1)
#endif  /* defined(DO_BIND_USE_SIZEOF) */


#if ! defined (USE_DMALLOC)

#undef ALLOC
#define ALLOC(TYPE, COUNT) (TYPE *) malloc(sizeof(TYPE) * (COUNT))

#undef MALLOC
#define MALLOC(SIZE) (char *) malloc(SIZE)

#undef CALLOC
#define CALLOC(TYPE, COUNT) (TYPE *) calloc((COUNT), sizeof(TYPE))

#undef REALLOC
#define REALLOC(ptr, TYPE, COUNT) \
                  (TYPE *) realloc((char *)(ptr), (sizeof(TYPE) * (COUNT)))

#undef REMALLOC
#define REMALLOC(ptr, SIZE) (char *) realloc((char *)(ptr),(SIZE))

#undef FREE
#define FREE(ptr) free((char *)(ptr))

#else

#include <dmalloc.h>

#endif /* !defined (USE_DMALLOC) */


#if defined (DBTIMES)

  /* some small values for testing things. */

#undef STATS_PERIOD
#define STATS_PERIOD 30   /* 30 seconds */

#undef STATS_RESET_PERIOD
#define STATS_RESET_PERIOD (6 * 60) /* 6 minutes */

#undef ARTICLE_STATS_PERIOD
#define ARTICLE_STATS_PERIOD (6 * 60) /* 7 minutes */

#undef CLOSE_PERIOD
#define CLOSE_PERIOD (3 * 60)   /* 5 minutes */

#endif /* DBTIMES */
  
#endif /* config_h__ */
