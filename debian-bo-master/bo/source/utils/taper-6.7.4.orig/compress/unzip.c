/*
   Time-stamp: <96/07/19 20:14:46 yusuf>

   $Id: unzip.c,v 1.16 1996/07/27 20:42:33 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: unzip.c,v 1.16 1996/07/27 20:42:33 yusuf Exp $";
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



/* unzip.c -- decompress files in gzip or pkzip format.
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * The code in this file is derived from the file funzip.c written
 * and put in the public domain by Mark Adler.
 */

/*
   This version can extract files in gzip or pkzip format.
   For the latter, only the first entry is extracted, and it has to be
   either deflated or stored.
 */

#ifdef RCSID
static char rcsid[] = "$Id: unzip.c,v 1.16 1996/07/27 20:42:33 yusuf Exp $";
#endif

#include "tailor.h"
#include "gzip.h"
#include "crypt.h"

/* PKZIP header definitions */
#define LOCSIG 0x04034b50L      /* four-byte lead-in (lsb first) */
#define LOCFLG 6                /* offset of bit flag */
#define  CRPFLG 1               /*  bit for encrypted entry */
#define  EXTFLG 8               /*  bit for extended local header */
#define LOCHOW 8                /* offset of compression method */
#define LOCTIM 10               /* file mod time (for decryption) */
#define LOCCRC 14               /* offset of crc */
#define LOCSIZ 18               /* offset of compressed size */
#define LOCLEN 22               /* offset of uncompressed length */
#define LOCFIL 26               /* offset of file name field length */
#define LOCEXT 28               /* offset of extra field length */
#define LOCHDR 30               /* size of local header, including sig */
#define EXTHDR 16               /* size of extended local header, inc sig */


/* Globals */

int decrypt;        /* flag to turn on decryption */
char *key;          /* not used--needed to link crypt.c */


/* ===========================================================================
 * Unzip in to out.  This routine works on both gzip and pkzip files.
 *
 * IN assertions: the buffer inbuf contains already the beginning of
 *   the compressed data, from offsets inptr to insize-1 included.
 *   The magic header has already been checked. The output buffer is cleared.
 */
extern int decompress;
int unzip(in, out)
    int in, out;   /* input and output file descriptors */
{
    int res;

    gz_ifd = in;
    gz_ofd = out;

    updcrc(NULL, 0);           /* initialize crc */

    /* Decompress */

    decompress=1;
    insize=0; inptr=0;
    res = inflate();
    
    if (res == 3) {
	error("out of memory");
    } else if (res != 0) {
	error("invalid compressed data--format violated");
    }

    return 0;
}
