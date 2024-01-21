    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#ifndef _FSP_CLIENT_DEF_H_
#define _FSP_CLIENT_DEF_H_

#include "common_def.h"
#include "client_conf.h"

/*************************************************************************
* The following global variables are used to change the behavior when    *
* fgrab encounters a file with the same name in the current directory    *
* Added 11 Jan 93 by A.E.J.Fellows                                       *
*************************************************************************/

#define NOCLOBBER	1       /* Will not overwrite file of same name  */
#define CLOBBER		2       /* Will always overwrite file even if
                                   remote file is not found              */
#define UNIQUE		3       /* Create unique name to avoid overwrite */
#define CLOBBERONFIND	4       /* Only overwrite if file downloaded     */
#define APPEND		5	/* Downloads will attempt to append to 
				   end of file if it already exists      */

/*************************************************************************
* The following global variables are used by fdu		         *
* Added 5. Apr. 93 by M.Meskes                                           *
*************************************************************************/

#define RECURSIVE	1
#define SUM		2
#define EACH		4


/****************************************************************************
* These structures are used to implement a opendir/readdir mechanism similar
* to that of the normal opendir/reader mechanism in unix.
****************************************************************************/

typedef struct DDLIST {	struct DDLIST *next;
			char          *path;
			RDIRENT  **dep_root;
			int         ref_cnt; } DDLIST;

typedef struct RDIR { DDLIST   *ddp;
		      RDIRENT **dep; } RDIR;

typedef struct rdirent { unsigned long  d_fileno;
			 unsigned short d_reclen;
			 unsigned short d_namlen;
			 char          *d_name; } rdirent;

extern int client_trace;
extern int client_intr_state;
extern int key_persists;
extern unsigned long target_delay;
extern unsigned long udp_sent_time;
extern unsigned short client_buf_len;
extern unsigned short client_net_len;
extern char *env_local_dir;
extern int env_timeout;

#endif /* _FSP_CLIENT_DEF_H_ */
