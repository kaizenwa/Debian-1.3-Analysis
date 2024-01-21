#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	irafio.c (IRAF Input Output)
 * Purpose:	Read an image in from the IRAF pipe
 * Subroutine:	open_imtool_connection()	returns: void
 * Subroutine:	close_imtool_connection()	returns: void
 * Subroutine:	rename_imtool_connection()	returns: void
 * Subroutine:	imtool_input()			returns: int
 * Subroutine:	imtool_output()			returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} based on code from Doug Tody's IRAF Imtool (NOAO)
 *		{1} Michael VanHilst	adapted			9 July 1989
 *		{2} Michael VanHilst added output		2 Nov 1989
 *		{3} MVH used generalized remote connection IO	28 March 1990
 *		{n} <who> -- <does what> -- <when>
 * "For the moment we take an IIS model 70 command/data stream as input; this
 * is used to load images into the image display.  This is a kludge interface
 * for the prototype, convenient since the high level software is written for
 * the IIS." - explanation by Doug Tody
 */

#include <stdio.h>		/* stderr, FILE, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include "hfiles/control.h"	/* define struct connectRec */
#include "hfiles/imtool.h"	/* define struct imtoolRec, codes */
extern struct controlRec control;

/*
 * Subroutine:	open_imtool_connection
 * Purpose:	open the connection for IRAF
 */
void open_imtool_connection ()
{
#ifdef IMTOOL
  int open_connection();
  extern void read_imtool_packet();

  control.IRAF_in.func = read_imtool_packet;
  if( open_connection(&control.IRAF_in) != -1 ) {
    if( control.IRAF_in.type != IOP_socket )
      (void)open_connection(&control.IRAF_out);
    if( control.verbose )
      (void)printf("Open to accept IRAF input\n");
  }
#else
  (void)fprintf(stderr, "IRAF imtool support not compiled\n");
#endif
}

/*
 * Subroutine:	close_imtool_connection
 * Purpose:	close the connection for IRAF
 */
void close_imtool_connection ()
{
  close_connection(&control.IRAF_in);
  if( control.IRAF_in.type != IOP_socket )
    close_connection(&control.IRAF_out);
  if( control.verbose )
    (void)printf("Closed IRAF input channel\n");
}

/*
 * Subroutine:	rename_imtool_connection
 * Purpose:	assign a new name to the imtool connection
 */
void rename_imtool_connection ( name, in )
     char *name;
     int in;
{
  char *string, *calloc_errchk();

  string = calloc_errchk(strlen(name)+2, 1, "string buffer");
  strcpy(string, name);
  if( in )
    control.IRAF_in.name = string;
  else
    control.IRAF_out.name = string;
}

#ifdef IMTOOL

/*
 * Subroutine:	imtool_output
 * Purpose:	Write image data back to iraf device channel
 */
void imtool_output ( imhead, odev, imagebuf, width, height )
     struct imtoolRec *imhead;
     struct connectRec *odev;
     short *imagebuf;
     int width, height;
{
  int ndatabytes;
  int x, y;
  int offset;
  int nleft;
  int bytes;
  int buferr = 0;
  char fifobuf[SZ_FIFOBUF + 4];
  int write_connection();
  static int compact_short_to_byte();

  ndatabytes = -(int)imhead->thingct;
  x = imhead->x & 077777;
  y = imhead->y & 077777;
  offset = (y * width) + x;
  /* if requested segment overflows,must be in error */
  if( (offset + ndatabytes) > (width * height) ) {
    (void)fprintf(stderr, "attempted read out of bounds on buffer\n");
    (void)fprintf(stderr, "Sending NULL data\n");
    imagebuf = NULL;
  } else
    imagebuf += offset;
  bzero(fifobuf, SZ_FIFOBUF + 4);
  bytes = SZ_FIFOBUF;
  for( nleft = ndatabytes; nleft > 0; nleft -= bytes ) {
    if( nleft < SZ_FIFOBUF )
      bytes = nleft;
    /* load buffer for writing */
    if( imagebuf != NULL ) {
      if( compact_short_to_byte(imagebuf, (unsigned char *)fifobuf, bytes) )
	buferr = 1;
    }
    if( write_connection(odev, fifobuf, bytes) < 0 )
      return;
    imagebuf += bytes;
  }
  if( buferr )
    (void)fprintf(stderr, "WARNING: data not IRAF compatible\n");
}

/*
 * Subroutine:	imtool_input
 * Purpose:	Load image data from iraf input device channel
 * Called by:	imtool_response() in RemoteImtool.c
 * Returns:	-1 if read no bytes, 1 if trouble, else 0
 */
int imtool_input ( imhead, idev, readbuf, iwdth, owdth, y1, y2,
		   imagebuf, imbufsz )
     struct imtoolRec *imhead;
     struct connectRec *idev;
     char *readbuf;
     int iwdth, owdth;
     int *y1, *y2;
     short *imagebuf;
     int imbufsz;
{
  int ndatabytes, ndatavals;
  int packed;
  int errcnt;
  int x, y;
  int nvals, nleft;
  int got, try, gotten;
  short *obuf;
  int read_connection();
  static void expand_byte_to_short();

  /* get data count and packing type */
  ndatavals = -(int)imhead->thingct;
  if( !(imhead->tid & PACKED) ) {
    ndatabytes = ndatavals * 2;
    packed = 0;
  } else {
    ndatabytes = ndatavals;
    packed = 1;
  }
  /* get bytes and upper left coordinates */
  x = imhead->x & 077777;
  y = imhead->y & 077777;
  /* check for running out of bounds */
  *y1 = y;
  *y2 = y + (ndatavals / iwdth);
  if( ((y * owdth + ndatavals) * 2) > imbufsz ) {
    (void)fprintf(stderr,"Error: Imtool read beyond buffer %d, %d\n", y, *y2);
    return( -1 );
  }
  /* compute pointer to output buffer starting entry */
  obuf = imagebuf + (y * owdth) + x;
  /* read the data into the frame buffer. */
  for( nleft = ndatabytes;  nleft > 0;  nleft -= nvals ) {
    try = nvals = (nleft < SZ_FIFOBUF) ? nleft : SZ_FIFOBUF;
    errcnt = 0;
    gotten = 0;
    while( (got = read_connection(idev, &(readbuf[gotten]), try)) < try ) {
      (void)fprintf(stderr, "Pipe data under-read: %d %d\n", got, try);
      (void)fflush(stderr);
      try -= got;
      gotten += got;
      if( errcnt++ > 10 )
	return( 1 );
    }
    /* move input data to image buffer */ 
    if( packed ) {
      /* 8 bit integers in */
      expand_byte_to_short ((unsigned char *)readbuf, obuf, nvals);
      obuf += nvals;
    } else if( readbuf != (char *)obuf ) {
      /* 16 bit integers in */
      bcopy(readbuf, (char *)obuf, nvals);
      obuf += (nvals / 2);
    }
  }
  return( 0 );
}

/*
 * Subroutine:	expand_byte_to_short
 * Purpose:	Unpack byte data for use as shorts
 */
static void expand_byte_to_short ( bts, ints, cnt )
     register unsigned char *bts;
     register short *ints;
     register int cnt;
{
  while (cnt-- > 0) {
    *ints = *bts;
    ints++;
    bts++;
  }
}

static int compact_short_to_byte ( ints, bts, cnt )
     register short *ints;
     register unsigned char *bts;
     register int cnt;
{
  int buferr = 0;
  while( cnt-- > 0 ) {
    if( *ints > 255 ) {
      *bts = 255;
      buferr = 1;
    } else if( *ints < 0 ) {
      *bts = 0;
      buferr = 1;
    } else
      *bts = *ints;
    ++ints;
    ++bts;
  }
  return( buferr );
}


#endif
