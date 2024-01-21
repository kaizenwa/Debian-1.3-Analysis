#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	irafpipe.c (IRAF Pipe Connection)
 * Purpose:	Read an image in from the IRAF pipe
 * Subroutine:	read_imtool_packet()		returns: void
 * Copyright:	1990 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} based on code from Doug Tody's IRAF Imtool (NOAO)
 *		{1} Michael VanHilst	adapted			9 July 1989
 *		{2} MVH rearranged structure			29 March 1990
 *		{n} <who> -- <does what> -- <when>
 * "For the moment we take an IIS model 70 command/data stream as input; this
 * is used to load images into the image display.  This is a kludge interface
 * for the prototype, convenient since the high level software is written for
 * the IIS." - explanation by Doug Tody
 */

#ifdef IMTOOL

#include <stdio.h>		/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>		/* get X types and constants */
#include "hfiles/control.h"	/* struct connectRec */
#include "hfiles/imtool.h"	/* struct imtoolRec, codes */

/*
 * Subroutine:	read_imtool_packet()
 * Purpose:	event handler for packet input from iraf
 */
void read_imtool_packet( port )
     struct connectRec *port;
{
  int ndatabytes;
  int bytes;
  int last_packet;
  struct imtoolRec imhead;
  static int byteswap = 0;	/* l: flag to byteswap packets */
  static int errcnt=0;		/* l: count packet errors (before giving up) */
  static int noblock=0;		/* l: check if in non-blocking loop */
#ifdef DEBUG
  int pkt;			/* l: make packet code discrete variable */
#endif
  int read_connection();
  void imtool_response(), swap_bytes();
  static int check_packet_sum();

  /* read the header */
  bytes = read_connection(port, (char *)&imhead, sizeof(struct imtoolRec));
  /* check size of read */
  if( bytes < sizeof(struct imtoolRec) ) {
    /* if EOF, must be problem with this end of pipe, call plumber */
    /* read 0 may happen once on EOF, happens forever if non-blocking */
    if( bytes == 0 ) {
      /* pipe mustn't be blocking, count the bad passes through here */
      /* noblock is set to zero everywhere but here */
      ++noblock;
      return;
    } else {
      noblock = 0;
#ifdef DEBUG
      (void)fprintf(stderr, "Pipe read error\n");
#endif
      return;
    }
  }

#if DEBUG
  if( getenv("DEBUG_IMTOOL") ) {
	char  *s;
	short su;

	(void)fprintf(stderr, "\n=== Imtool header ===\n");
	(void)fprintf(stderr, "  tid:     %d  -- ", imhead.tid);
	if(imhead.tid & PACKED)		(void)fprintf(stderr, "PACKED ");
	if( imhead.tid & COMMAND )	(void)fprintf(stderr, "COMMAND ");
	if( imhead.tid & IIS_READ )	(void)fprintf(stderr, "IIS_READ ");
	if( imhead.tid & IMC_SAMPLE )	(void)fprintf(stderr, "IMC_SAMPLE ");
	(void)fprintf(stderr, "\n");
	(void)fprintf (stderr, "  count:   %d\n", -imhead.thingct);
	su = imhead.subunit & 077;
	if( su == MEMORY )		s = "MEMORY";
	else if( su == LUT )		s = "LUT";
	else if( su == FEEDBACK )	s = "FEEDBACK";
	else if( su == WCS )		s = "WCS";
	else if( su == IMCURSOR )	s = "IMCURSOR";
	else if( su == 12 )		s = "ZOOM/PAN (ignored)";
	else				s = "???";
	(void)fprintf(stderr, "  subunit: %d -- %s\n", su, s);
	/* (void)fprintf(stderr, "  chksum:  %x\n", imhead.checksum); */
	(void)fprintf(stderr, "  x: %d  y: %d  z: %d  t: %d\n",
		      imhead.x & 077777, imhead.y & 077777,
		      imhead.z & 07777, imhead.t & 077);
  }
#endif

  noblock = 0;
  if( byteswap )
    swap_bytes((char *)&imhead, sizeof(struct imtoolRec));
  /* check the packet checksum (debug it after 2nd try), don't die */
  if( !check_packet_sum((char *)&imhead, sizeof(struct imtoolRec),
			 &byteswap, (errcnt > 2), 0) ) {
    /* no hope of syncing on next read, better flush out pipe */
    flush_connection(port);
    errcnt++;
    return;
  }
  /* read the count field */
  if( (ndatabytes = -(int)imhead.thingct) < 0 ) {
    (void)fprintf(stderr,"bad packet\n");
    return;
  }
  /* check for packed short's (we wish) */
  if( !(imhead.tid & PACKED) )
    ndatabytes *= 2;
  imtool_response(port, &imhead, ndatabytes);
}


/*
 * Subroutine:	check_packet_sum
 * Purpose:	Check packet sum against checksum (and verify byteswap)
 * Returns:	1
 */
static int check_packet_sum ( packet, size, byteswap, errmsg, die )
     char *packet;
     int size;
     int *byteswap, errmsg, die;
{
  int i, sum, second_try=0;
  register short *p;
  void swap_bytes(), exit_errmsg();

  while( 1 ) {
    /* add up the sum */
    for( i=0, sum=0, p=(short *)packet;  i < 8;  i++ )
      sum += *p++;
    /* sum (including checksum) should roll at 0177777 */
    if( (sum & 0177777) == 0177777 )
      return( 1 );
    /* if not, bswap and try again (note danger of swab in place) */
    swap_bytes(packet, size);
    *byteswap = !(*byteswap);
    if( second_try++ ) {
      /* after retry, report the error */
      if( errmsg ) {
	(void)fprintf(stderr, "imtool: bad data header checksum\n");
#ifdef DEBUG
	/* display first few bytes both swapped and unswapped */
	if( *byteswap )
	  /* if was byteswapped, start unswapped */
	  swap_bytes(packet, size);
	/* what it looks like unswapped */
	(void)fprintf(stderr, "noswap:");
	for( i=0, p=(short *)packet;  i < 8;  i++ )
	  (void)fprintf(stderr, " %6o", p[i]);
	(void)fprintf(stderr, "\n");
	/* what it looks like swapped */
	swap_bytes(packet, size);
	(void)fprintf(stderr, "  swap:");
	for( i=0, p=(short *)packet;  i < 8;  i++ )
	  (void)fprintf(stderr, " %6o", p[i]);
	(void)fprintf(stderr, "\n");
#endif
      }
      /* imtool dies at this point */
      if( die )
        exit_errmsg("FATAL ERROR: imtool pipe datastream synch lost\n");
      else
	return( 0 );
    }
  }
}

#endif
