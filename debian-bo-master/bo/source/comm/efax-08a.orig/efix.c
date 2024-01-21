#define Copyright         "Copyright 1996 Ed Casas"

#define Version		  "efix v 0.2"

/*
    Copyright (C) 1996  Ed Casas

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Please contact the author if you wish to use efax or efix for
    purposes not covered by the GNU GPL.

    You may contact the author by e-mail at: edc@cce.com, by mail
    at: 2629 West 3rd Ave, Vancouver, BC, Canada, V6K 1M4, or by
    fax at: +1 604 734 5291.

*/

const char *Usage =
  "Usage:\n"
  "  %s [ option ]... [ file ]... \n"
"Options (defaults):\n"
  "  -i  f   input format (auto):\n"
  "     fax     fax (\"Group3\") 1-D coded image\n"
  "     text    text\n"
  "     pbm     raw PBM (portable bit map)\n"
  "     tiffg3  TIFF, Group 3 fax compression\n"
  "     tiffraw TIFF, no compression\n"
  "  -o  f   output format (tiffg3):\n"
  "     fax     fax (\"Group3\") 1-D coded image\n"
  "     pbm     Portable Bit Map\n"
  "     pgm     Portable Gray Map (decimated by 4)\n"
  "     pcl     HP-PCL (e.g. HP LaserJet)\n"
  "     ps      Postscript (e.g. Apple Laserwriter)\n"
  "     tiffg3  TIFF, Group 3 fax compression\n"
  "     tiffraw TIFF, no compression\n"
  "  -n pat  printf() pattern for output file name (ofile)\n"
  "  -f fnt  use PBM font file fnt for text (built-in)\n"
  "  -l  n   lines per text page (66)\n"
  "  -v lvl  print messages of type in string lvl (ewi)\n"
  "  -s XxY  scale input by X and Y (Y optional) (1x1)\n"
  "  -r XxY  resolution of output is X by Y (dpi, Y optional) (204x196)\n"
  "  -p WxH  pad/truncate output to width W by height H (215x297mm)\n"
  "  -d R,D  displace output right R, down D (opposite if -ve) (0,0)\n"
  "  -O f    overlay file f (none)\n"
  "\n"
  "Add 'in', 'cm', 'mm', or 'pt' to -p and -d arguments (default in[ches]).\n" 
  "Default output size and resolution is same as input (if known).\n" 
  ;

#include <ctype.h>		/* ANSI C */
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "efaxlib.h"
#include "efaxmsg.h"

#ifndef INT_MAX
#define INT_MAX 32767
#endif

/* Allowed input and output formats. *** MUST match enum *** */

char *iformatstr[] = { "6text", "1pbm", "2fax", "7tiffg3", "8tiffraw", 0 } ;
char *oformatstr[] = { "1pbm" , "2fax", "3pcl", "4ps",  "5pgm", 
			 "7tiffg3", "8tiffraw", 0 } ;

/* Look up a string in a NULL-delimited table where the first
   character of each string is the digit to return if the rest of
   the string matches.  Returns the value of the digit for the
   matching string or -1 if no matches. */

int lookup ( char **tab, char *s )
{
  char **p ;
  for ( p=tab ; p && *p && strcmp ( *p+1, s ) ; p++ ) ;
  return p && *p ? ( **p - '0' ) : -1 ;
}


/* Extract pair of values from string.  If it's a `dim'ension,
   two values are required and they are converted to inches, else
   the y value is optional.  Returns 0 or 2 on error. */

int getxy ( char *arg, float *x, float *y, int dim )
{
  int i, n, nc=0, err=0 ;
  char c ;
  static char *unitstr[] = { "0in", "1cm", "2mm", "3pt", 0 } ;
  static float unitval[] = { 1.0, 2.54, 25.4, 72.0, 1.0 } ;

  if ( ! arg ) 
    err = msg ( "E2 missing argument" ) ;

  if ( !x || !y )
    err = msg ( "E2 can't happen (getxy)" ) ;

  if ( ! err ) {
    n = sscanf ( arg , "%f%c%f%n", x, &c, y, &nc ) ;
    switch ( n ) {
    case 0 : err = msg ( "E2bad X value in (%s)", arg ) ; break ;
    case 2 : err = msg ( "E2bad Y value in (%s)", arg ) ; break ;
    }      
  }

  if ( ! err ) {
    if ( dim ) {
      if ( n != 3 ) {
	err = msg ( "Emissing Y dimension in (%s)", arg ) ;
      } else {
	while ( arg [ nc ] && isspace ( arg [ nc ] ) ) nc++ ;
	if ( arg [ nc ] ) {
	  if ( ( i = lookup ( unitstr, arg+nc ) ) >= 0 ) {
	    *x /= unitval [ i ] ;
	    *y /= unitval [ i ] ;
	  } else {
	    err = msg ( "E2bad units: `%s'", arg+nc ) ;
	  }
	}
      }
    } else {
      if ( n == 1 ) *y = *x ;
    }
  }
  
  if ( ! err )
    msg ( "Aconverted (%s) into %f x %f", arg, *x, *y ) ;

  return err ;
}


int main( int argc, char **argv)
{
  int err=0, done=0, i, c ;
  int nr, pels, ovnr, ovpels, no ;	/* run/pixel/repeat counts */
  int linesout ;
  int page, ilines, olines ;		/* page & line counts */
  int xs, ys, w, h, ixsh, iysh ;	/* integer scale, size & shift */
  short runs [ MAXRUNS ] , ovruns [ MAXRUNS ] ;
  
  float					 /* defaults: */
    xsc=1.0, ysc=1.0,		         /* scale */
    xsh=0.0, ysh=0.0,			 /* shift */
    dxres = 204.145,			 /* o/p res'n: 1728/215mm * 25.4 x */
    dyres = 195.58,			 /* 7.7 * 25.4 */
    dxsz = 215 / 25.4,			 /* o/p size: 8.5" x A4 */
    dysz = 297 / 25.4 ;

  float				/* arguments: */
    axres = 0, ayres = 0, axsz = 0, aysz = 0 ;

  float				/* values used: */
    xres = 0, yres = 0, xsz = 0, ysz = 0 ;

  IFILE ifile, ovfile ;
  OFILE ofile ;

  char *defifnames [ 2 ] = { "-", 0 },  *ovfnames [ 2 ] = { 0, 0 } ;
  char **ifnames = defifnames ;

  int iformat=AUTO, oformat=TIFF_FAX, pglines=0 ;
  char *ofname=0 ;

  faxfont font, *pfont=0 ;	/* text font */

  /* initialize */

  argv0 = argv[0] ;

  /* process arguments */

  while ( !err && (c=nextopt(argc,argv,"n:i:o:O:v:l:f:r:s:p:d:") ) != -1) {
    switch ( c ) {
    case 'n':
      ofname = nxtoptarg ;
      break ;
    case 'i': 
      if ( ( iformat = lookup ( iformatstr, nxtoptarg ) ) < 0 ) 
	err = msg ( "E2invalid input type (%s)", nxtoptarg ) ;
      break ;
    case 'o': 
      if ( ( oformat = lookup ( oformatstr, nxtoptarg ) ) < 0 )
	err = msg ( "E2invalid output type (%s)", nxtoptarg ) ;
      break ;
    case 'O': 
      ovfnames[0] = nxtoptarg ;
      break ;
    case 'v': 
      verb[0] = nxtoptarg ;
      msg ( "A " Version ) ;
      for ( i=0 ; i<argc ; i++ ) msg ( "Aargv[%d]=%s", i, argv[i]) ;
      break ;
    case 'l':
      if ( sscanf ( nxtoptarg , "%d", &pglines ) != 1 || pglines <= 0 ) {
	err = msg ( "E2bad page length (%s)", nxtoptarg ) ;
	pglines = 0 ;
      }
      break ;
    case 'f' :
      if ( ! ( err = readfont ( nxtoptarg, &font ) ) )
	pfont = &font ;
      break ;
    case 's' : err = getxy ( nxtoptarg, &xsc , &ysc , 0 ) ; break ;
    case 'r' : err = getxy ( nxtoptarg, &axres, &ayres, 0 ) ; break ;
    case 'p' : err = getxy ( nxtoptarg, &axsz , &aysz , 1 ) ; break ;
    case 'd' : err = getxy ( nxtoptarg, &xsh , &ysh , 1 ) ; break ;
    default : fprintf ( stderr, Usage, argv0 ) ; err = 2 ; break ;
    }
  }

  msg ( "I " Version " " Copyright ) ;

  if ( nxtoptind < argc ) {
    ifnames = argv + nxtoptind ;
    if ( argv [ argc ] ) err = msg ("E2can't happen(unterminated argv)") ;
  }

  newIFILE ( &ifile, iformat, ifnames ) ;
  if ( pfont ) ifile.font = pfont ;
  if ( pglines ) ifile.pglines = pglines ;

  newIFILE ( &ovfile, iformat, ovfnames ) ;

  newOFILE ( &ofile, oformat, ofname, 0, 0, 0, 0 ) ;

  for ( page = 0 ; ! err && ! done ; page++ ) {

    if ( nextipage ( &ifile, 1 ) ) { 
      done=1 ; 
      continue ; 
    }

    /* set output size and resolution equal to input if none specified */

    if ( ifile.xres <= 0 ) ifile.xres = dxres ;
    if ( ifile.yres <= 0 ) ifile.yres = dyres ;

    xres = axres > 0 ? axres : ifile.xres ;
    yres = ayres > 0 ? ayres : ifile.yres ;

    xsz = axsz > 0 ? axsz : ( ifile.w > 0 ? ifile.w / ifile.xres : dxsz ) ;
    ysz = aysz > 0 ? aysz : ( ifile.h > 0 ? ifile.h / ifile.yres : dysz ) ;


    w = xsz * xres + 0.5 ;	      /* output dimensions in pixels */
    h = ysz * yres + 0.5 ;
    
    ixsh = xsh * xres ;		      /* x/y shifts in pixels/lines */
    iysh = ysh * yres ;
    
    if ( ( w & 7 ) != 0 )	/* just about everything requires... */
      msg ("Iimage width rounded to %d pixels", 
	   w = ( w + 7 ) & ~7 ) ;
    
    if ( ofile.format == PGM && h & 3 ) /* PGM x4 decimation requires... */
      msg ("I PGM image height rounded up to %d lines", 
	   h = ( h + 3 ) & ~3 ) ;
    
    if ( w <= 0 || h <= 0 || xres < 0 || yres < 0 )
      err = msg ( "E2negative/zero scaling/size/resolution" ) ;
    
    if ( ofile.format == PCL &&	/* check for strange PCL resolutions */
	( xres != yres || ( xres != 300 && xres != 150 && xres != 75 ) ) )
      msg ( "Wstrange PCL resolution (%.0fx%.0f)", xres, yres ) ;
    
    if ( w > MAXBITS*8 )	/* make sure output will fit... */
      err = msg( "E2requested output width too large (%d pixels)", w ) ;
    
    ofile.w = w ; 
    ofile.h = h ; 
    ofile.xres = xres ; 
    ofile.yres = yres ;

    /* scale according to input file resolution */

    xs = 256 * xsc * xres / ifile.xres + 0.5 ;
    ys = 256 * ysc * yres / ifile.yres + 0.5 ;

    if ( xs <= 0 || ys <= 0 )
      err = msg ( "E2negative/zero scaling" ) ;

    if ( *ovfnames )		      /* [re-]open overlay file */
      if ( nextipage ( &ovfile , 1 ) ) { 
	err=2 ; 
	continue ; 
      }

    if ( nextopage ( &ofile, page ) ) { 
      err=2 ; 
      continue ; 
    }
    linesout=0 ;

    /* y-shift */

    if ( iysh > 0 ) {
      writeline ( &ofile, ( ( *runs = w ), runs ), 1, iysh ) ;
      linesout += iysh ;
    } else {
      for ( i=0 ; i < -iysh ; i++ ) 
	readline ( &ifile, runs, 0 ) ;
    }    

    /* copy input to output */
    
    olines = ilines = 0 ; 
    
    while ( linesout < h ) {

      if  ( ( nr = readline ( &ifile, runs, &pels ) ) < 0 )
	break ;
      else
	ilines++ ;

      if ( *ovfnames ) {
	if ( ( ovnr = readline ( &ovfile, ovruns, &ovpels ) ) >= 0 )
	  nr = runor ( runs, nr, ovruns, ovnr, 0, &pels ) ; 
      }

      /* x-scale, x-shift & x-pad input line */
    
      pels  = ( xs == 256 ) ? pels : xscale ( runs, nr, xs ) ;
      pels += ( ixsh == 0 ) ?   0  : xshift ( runs, nr, ixsh ) ;
      nr    = ( pels == w ) ?  nr  : xpad   ( runs, nr, w - pels ) ;

      /* y-scale by deleting/duplicating lines. */

      no = ( ( ilines * ys ) >> 8 ) - olines ;

      if ( linesout + no > h ) no = h - linesout ;
      olines += no ;

      writeline ( &ofile, runs, nr, no ) ;
      linesout += no ;
    }

    /* y-pad */

    if ( linesout < h )
      writeline ( &ofile, ( ( *runs = w ), runs ), 1, h - linesout ) ;
    
    if ( ferror ( ifile.f ) ) err = msg ( "ES2input error:" ) ;
  }

  nextopage ( &ofile, EOF ) ;

  return err ;
}
