    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#ifndef _FSP_SERVER_DEF_H_
#define _FSP_SERVER_DEF_H_

#include "common_def.h"
#include "server_conf.h"

#define NBSIZE (2*sizeof(UBUF))

/****************************************************************************
*  HTAB is structure for storing client information for one client machine.
*  They makes it easy to reuse regular unix tool's source for new purposes.
****************************************************************************/

typedef struct HTAB HTAB;

struct HTAB {	unsigned long	inet_num;	/* inet number of client     */
		char	       *hostname;	/* hostname of client        */
		unsigned long   last_acc;	/* last sucessful access time*/
		unsigned short	next_key;	/* next key client should use*/
		unsigned short	last_key;	/* previous key client used  */
		unsigned short   acc_cnt;	/* number of successful acc  */
		unsigned short  active:1; };	/* session continuing.	     */

/****************************************************************************
*  IPrange is the structure for storing information about disabled, ignored
*  or normal hosts.
****************************************************************************/
typedef struct {
    unsigned char lo[4];
    unsigned char hi[4];
    char *text;
} IPrange;

/*****************************************************************************
* The PPATH structure is filled in by the function check_path when given a
* path string.  See server_file.c for more info.
*****************************************************************************/

typedef struct { char *fullp; /* ptr to string containing full pathname  */
		 char *f_ptr; /* ptr to begining of last path component  */
		 int   f_len; /* length of last component in path        */
		 char *d_ptr; /* ptr to beginning of directory component */
		 int   d_len; /* length of directory part of path.       */
		 char *r_ptr; /* part of path that belongs to remote FSP */
                 char *passwd; /* ptr to password                        */
		 char  inetstr[16]; /* inet-address of remote-FSP        */
		 char  portstr[8];  /* port of remote-FSP                */
	       } PPATH;

typedef struct { FILE *fp;
                 char filename[NBSIZE];
                 unsigned long inet_num;
                 unsigned long port_num;
               } FPCACHE;

extern int dbug;   /* debug flag */
extern char *home_dir;
extern FPCACHE *cache_p, fpcache[FSP_FILE_CACHE+1];
extern int max_nlen;
extern int always_use_cache_dir;
extern int dir_cache_limit;
extern char *dir_cache_dir;

/* THCCOUNT is the number of seconds used to compute average throughput.
 * 10 seconds seems to be a good value
 */
#define THCCOUNT 10

#define REVERSE_ERR_MSG "Permission denied -- can't identify host.\n\
  Sorry, we can't reverse name you.  If you know that your site normally\n\
  can be, try again in a few minutes when the local maps may have been\n\
  updated.  Otherwise, this service will remain unavailable to you; check\n\
  with your local admins for why this is the case.\n"

#endif /* _FSP_SERVER_DEF_H_ */
