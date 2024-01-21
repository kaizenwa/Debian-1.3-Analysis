/*
   Time-stamp: <96/07/19 20:14:53 yusuf>

   $Id: zip.c,v 1.16 1996/07/27 20:42:34 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: zip.c,v 1.16 1996/07/27 20:42:34 yusuf Exp $";
#endif /* lint */



/* This module of code has been shamelessley ripped off from the
 * GNU gzip package - version 1.2.4.
 * 
 * Not only has it been ripped off, it has been hopelessly adulterated
 * for use by taper.
 * 
 * Since gzip works pretty well, any bugs found are more than likely
 * to have been caused me in the modification process so blame me
 * and not the GNU team.
 * 
 * 
 * Yusuf Nagree
 * 
*/



/* zip.c -- compress files to the gzip or pkzip format
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

#ifdef RCSID
static char rcsid[] = "$Id: zip.c,v 1.16 1996/07/27 20:42:34 yusuf Exp $";
#endif

#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#include "tailor.h"
#include "gzip.h"
#include "crypt.h"
#include "lzw.h"

#include <unistd.h>
#include <fcntl.h>

local ulg crc;       /* crc on uncompressed file data */

#include <time.h>

#include <stdlib.h>

#include <dirent.h>
typedef struct dirent dir_type;
#define NLENGTH(dirent) ((int)strlen((dirent)->d_name))
#define DIR_OPT "DIRENT"

#include <utime.h>
#define TIME_OPT "UTIME"

typedef RETSIGTYPE (*sig_type) OF((int));

#ifndef MAX_PATH_LEN
#  define MAX_PATH_LEN   1024 /* max pathname length */
#endif


#define PART_SEP "."


DECLARE(uch, inbuf,  INBUFSIZ +INBUF_EXTRA);
DECLARE(uch, outbuf, OUTBUFSIZ+OUTBUF_EXTRA);
DECLARE(ush, d_buf,  DIST_BUFSIZE);
DECLARE(uch, window, 2L*WSIZE);
DECLARE(ush, tab_prefix0, 1L<<(BITS-1));
DECLARE(ush, tab_prefix1, 1L<<(BITS-1));


/* local variables */

int decompress = 0;   /* decompress (-d) */
int verbose = 0;      /* be verbose (-v) */
int quiet = 0;        /* be quiet (-q) */
int test = 0;         /* test .gz file integrity - used in util.c */
char *progname;       /* program name */
int method = DEFLATED;/* compression method */
int level = 1;        /* compression level */
int exit_code = 0 ;   /* program exit code */
int save_orig_name;   /* set if original name must be saved */
long time_stamp;      /* original time stamp (modification time) */
long ifile_size;      /* input file size, -1 for devices (debug only) */
char *env;            /* contents of GZIP env variable */
char **args = NULL;   /* argv pointer if GZIP env variable defined */

long bytes_in;             /* number of input bytes */
long bytes_out;            /* number of output bytes */
long total_in = 0;         /* input bytes for all files */
long total_out = 0;        /* output bytes for all files */
char ifname[MAX_PATH_LEN]; /* input file name */
char ofname[MAX_PATH_LEN]; /* output file name */
int  remove_ofname = 0;	   /* remove output file on error */
struct stat istat;         /* status for input file */
int  gz_ifd;               /* input file descriptor */
int  gz_ofd; 	           /* output file descriptor */
unsigned insize;           /* valid bytes in inbuf */
unsigned inptr;            /* index of next byte to be processed in inbuf */
unsigned outcnt;           /* bytes in output buffer */


void abort_gzip()
{	
	kill(getpid(), SIGSEGV);   /* tell main program seg faulted */
}


/* ===========================================================================
 * Deflate in to out.
 */
int zip(in, out)
    int in, out;            /* input and output file descriptors */
{
    uch  flags = 0;         /* general purpose bit flags */
    ush  attr = 0;          /* ascii/binary flag */
    ush  deflate_flags = 0; /* pkzip -es, -en or -ex equivalent */

    gz_ifd = in;
    gz_ofd = out;
    outcnt = 0;
    
    clear_bufs();

    /* Write the header to the gzip file. See algorithm.doc for the format */

    method = DEFLATED;

    if (save_orig_name) {
	flags |= ORIG_NAME;
    }

    /* Write deflated file to zip file */
    crc = updcrc(0, 0);

    bi_init(out);
    ct_init(&attr, &method);
    lm_init(level, &deflate_flags);

    (void)deflate();

    put_long(crc);
    put_long(isize);

    flush_outbuf();
    return 0;
}


/* ===========================================================================
 * Read a new buffer from the current input file, perform end-of-line
 * translation, and update the crc and input file size.
 * IN assertion: size >= 2 (for end-of-line translation)
 */
int file_read(buf, size)
    char *buf;
    unsigned size;
{
    unsigned len;

    Assert(insize == 0, "inbuf not empty");

    len = read(gz_ifd, buf, size);
    if (len == (unsigned)(-1) || len == 0) return (int)len;

    crc = updcrc((uch*)buf, len);
    isize += (ulg)len;
    return (int)len;
}

