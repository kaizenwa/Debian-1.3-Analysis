    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#ifndef _FSP_COMMON_DEF_H_
#define _FSP_COMMON_DEF_H_

#include <stdio.h>

#ifdef VMS
#include "param.h"
#include "types.h"
#include "socket.h"
#include "stat.h"
#include <unixio.h>
#include <unixlib.h>
#define malloc VAXC$MALLOC_OPT
#define realloc VAXC$REALLOC_OPT
#define free VAXC$FREE_OPT
#define unlink delete
#else
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#endif

#include <errno.h>
#include <netinet/in.h>

#ifdef VMS
#include "time.h"
#else
#include <sys/time.h>
#include <fcntl.h>
#endif

#include <signal.h>

#ifdef VMS
#include "dirent.h"
#else
#ifdef DIRENT
#include <dirent.h>
#else
#ifdef SYSDIR
#include <sys/dir.h>
#else
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif
#endif
#endif
#endif

/****************************************************************************
*  UBUF is the structure of message exchanged between server and clients. 
*
*    The 'buf' part of the buffer is variable lenght up to max of 1024.
*    The 'key' field is used by the server for sequence identification.
*    The 'seq' field is used by the client for sequence identification.
*
*  Client's message to server contain a key value that is the same as the
*  key value of the previous message received from the server.  Similarly,
*  the server's message to client contains a seq value that is the same
*  as the seq value of the previous message from the client. 
*
*  The buf field is logically partitioned into two parts by the len field.
*  The len field indicate the size of the first part of the buffer starting
*  at buf[0].  The rest of the buffer is the second field.  In some cases
*  both fields can contain information.
*
****************************************************************************/

#define UBUF_HSIZE 12                           /* 12 bytes for the header */
#define UBUF_SPACE 1024			        /* maximum payload.        */

typedef struct UBUF {            char       cmd; /* message code.             */
                        unsigned char       sum; /* message checksum.         */
                        unsigned char bb_key[2]; /* message key.              */
                        unsigned char bb_seq[2]; /* message sequence number.  */
                        unsigned char bb_len[2]; /* number of bytes in buf 1. */
                        unsigned char bb_pos[4]; /* location in the file.     */

                        char   buf[UBUF_SPACE];
                    } UBUF;

/* definition of cmd */

#define CC_VERSION	0x10	/* return server's version string.	*/
#define CC_ERR          0x40    /* error response from server.          */
#define CC_GET_DIR      0x41    /* get a directory listing.             */
#define CC_GET_FILE     0x42    /* get a file.                          */
#define CC_UP_LOAD      0x43    /* open a file for writing.             */
#define CC_INSTALL      0x44    /* close a file opened for writing.     */
#define CC_DEL_FILE     0x45    /* delete a file.                       */
#define CC_DEL_DIR      0x46    /* delete a directory.                  */
#define CC_GET_PRO      0x47    /* get directory protection.            */
#define CC_SET_PRO      0x48    /* set directory protection.            */
#define CC_MAKE_DIR     0x49    /* create a directory.                  */
#define CC_BYE          0x4A    /* finish a session.                    */
#define CC_GRAB_FILE    0x4B	/* atomic get+delete a file.		*/
#define CC_GRAB_DONE    0x4C	/* atomic get+delete a file done.	*/
#define CC_LIMIT	0x80	/* # > 0x7f for future cntrl blk ext.   */

/* definition of global bitfield for version information */
/* Global information is also going to be a bit vector   */
#define VER_BYTES	1	/* currently only 8 bits or less of info  */
#define VER_LOG		0x01	/* does the server do logging             */
#define VER_READONLY	0x02	/* is the server in readonly mode         */
#define VER_REVNAME	0x04	/* does the server refuse non reversables */
#define VER_PRIVMODE	0x08	/* Is the server being run 'private' mode */
#define VER_THRUPUT	0x10	/* does the server enforce thruput control*/

/* definition of directory bitfield for directory information */
/* directory information is just going to be a bitfield encoding
 * of which protection bits are set/unset
 */
#define PRO_BYTES	1	/* currently only 8 bits or less of info  */
#define DIR_OWNER	0x01	/* does caller own directory              */
#define DIR_DEL		0x02	/* can files be deleted from this dir     */
#define DIR_ADD		0x04	/* can files be added to this dir         */
#define DIR_MKDIR	0x08	/* can new subdirectories be created      */
#define DIR_PRIV	0x10	/* are files readable by non-owners       */
#define DIR_README	0x20	/* does this dir contain an readme file?  */

/* definition of logging information */
#define L_NONE		0x000
#define L_ERR		0x001
#define L_VER		0x002
#define L_GETDIR	0x004
#define L_GETFILE	0x008
#define L_UPLOAD	0x010
#define L_INSTALL	0x020
#define L_DELFILE	0x040
#define L_DELDIR	0x080
#define L_SETPRO	0x100
#define L_MAKEDIR	0x200
#define L_GRABFILE	0x400
#define L_GETPRO	0x800
#define L_ALL		0xfff

/****************************************************************************
*  RDIRENT is the structure of a directory entry contained in a .FSP_CONTENT
*  file.  Each entry contains a 4 bytes quantity 'time', a 4 bytes quentity
*  'size', and 1 byte of 'type'.  Then followed by x number of bytes of
*  'name'.  'name' is null terminated.  Then followed by enough number of
*  padding to fill to an 4-byte boundary.  At this point, if the next entry
*  to follow will spread across 1k boundary, then two possible things will
*  happen.  1) if the header fits between this entry and the 1k boundary,
*  a complete header will be filled in with a 'type' set to RDTYPE_SKIP.
*  And then enough bytes to padd to 1k boundary.  2) if the header does
*  not fit, then simply pad to the 1k boundary.  This will make sure that
*  messages carrying directory information carry only complete directory
*  entries and no fragmented entries.  The last entry is type RDTYPE_END.
****************************************************************************/

#define RDHSIZE (2*4+1)

typedef struct RDIRENT { unsigned char bb_time[4];
                         unsigned char bb_size[4];
                         unsigned char type      ;
                         char          name[1]   ; } RDIRENT;

#define RDTYPE_END      0x00
#define RDTYPE_FILE     0x01
#define RDTYPE_DIR      0x02
#define RDTYPE_SKIP     0x2A

#define NULLP ((char *) 0)

#define MIN_DELAY	500L

#endif /* _FSP_COMMON_DEF_H_ */
