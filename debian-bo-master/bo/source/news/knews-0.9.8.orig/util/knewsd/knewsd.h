/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */

/*
 * Configuration:  all of these can set at runtime instead
 */

/* This is mandatory */
#define SPOOL_DIR		"/var/spool/news"

/* This is advisable */
#define ACTIVE_FILE		"/usr/local/news/active"

/* Entirely voluntary */
/* #define NEWSGROUPS_FILE	"/usr/local/news/newsgroups"*/

/* May be necessary */
/* #define OVERVIEW_DIR		"/var/spool/news/over.view"*/


/* Define this to be able to post.  SHOULD be an exec'ed shell command. */
/* #define POSTING_AGENT	"exec inews -h"*/
