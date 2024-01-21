#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgnanli.c (Region Annuli)
 * Purpose:	Create annuli and parse lists of radii
 * Subroutine:	fit_annuli_edge()		returns: void
 * Subroutine:	new_annulus_edge()		returns: void
 * Subroutine:	parse_radii()			returns: int
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  11 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, sscanf, NULL */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/cursor.h"	/* cursorRec */

/*
 * Subroutine:	fit_annuli_edge
 * Purpose:	If annuli edge matches previous edge, free redundancy
 * Note:	By saoimage convention, base cursor record of annuli is
 *		redundant to the last ring manipulated (this enables
 *		the manipulation)
 */
void fit_annuli_edge ( cursor )
     struct cursorRec *cursor;
{
  struct cursorRec *region;
  struct cursorRec *edge;
  int not_different();
  void free_cursor();

  region = cursor->next_region;
  edge = region->next_region;
  while( edge->next_annulus != NULL )
    edge = edge->next_annulus;
  if( (region->type == edge->type) &&
      not_different(region->rot.angle, edge->rot.angle) &&
      not_different(region->file.X, edge->file.X) &&
      not_different(region->file.Y, edge->file.Y) && 
      not_different(region->file.Xdim, edge->file.Xdim) &&
      not_different(region->file.Ydim, edge->file.Ydim) ) {
    /* region is part of annuli, not a separate region */
    cursor->next_region = region->next_region;
    if( cursor->next_region->annuli ) {
      /* if annuli exist, we don't need this edge */
      free_cursor(region);
    } else {
      /* if creating the first ring around a cursor, make redundant */
      edge->annuli = 1;
      edge->next_annulus = region;
    }
  }
}

/*
 * Subroutine:	new_annuli_edge
 * Purpose:	reposition region to be an annular edge
 */
void new_annulus_edge ( cursor )
     struct cursorRec *cursor;
{
  struct cursorRec *region;
  struct cursorRec *edge;

  edge = cursor->next_region;
  region = edge->next_region;
  cursor->next_region = region;
  if( region->annuli == 0 )
    /* this is an error condition (see fit_annuli_edge above) */
    region->annuli = 1;
  while( region->next_annulus != NULL )
    region = region->next_annulus;
  region->next_annulus = edge;
}

/*
 * Subroutine:	parse_radii
 * Purpose:	Get a list of radii from line
 * Returns:	Number of radii found
 * Note:	"vala valb valc ..." and "val1 val2 n=int" are both
 *		permitted syntax forms
 */
int parse_radii ( line, radius, maxcnt )
     char *line;	/* i: string with first radius as next token */
     float *radius;	/* o: buffer for float radius vals */
     int maxcnt;	/* i: size of radius buffer */
{
  int i, cnt;
  static int expand_radii();
  static char *got_nequal();
  char *next_token();

  i = 0;
  /* while there are tokens, look at the next one */
  while( (line != 0) && (i < maxcnt) ) {
    if( sscanf(line, "%f", &radius[i]) == 1 ) {
      /* next token was a number, got next radius */
      i++;
    } else {
      /* next token not a number, check for n=integer statement */
      if( ((line = got_nequal(line)) != NULL) &&
	  (sscanf(line, "%d", &cnt) == 1) ) {
	/* got n=cnt, add cnt radii between previous two radii */
	i = expand_radii(radius, i, maxcnt, cnt);
      } else {
	/* token neither radius nor n=int, must be error, keep prior radii */
	(void)fprintf(stderr,"WARNING: cannot parse radii params: %s\n",line);
        return( i );
      }
    }
    line = next_token(line, 1);
  }
  return( i );
}

/*
 * Subroutine:	expand_radii
 * Purpose:	Given an n=cnt, install cnt radii in list between last
 *		two radii
 * Returns:	Total number of radii in radius buffer
 */
static int expand_radii ( radius, i, maxcnt, cnt )
     float *radius;	/* i/o: array list of radii */
     int i;		/* i: current array size */
     int maxcnt;	/* i: maximum allowed size */
     int cnt;		/* i: number of bands to divide last interval */
{
  double first, last, inc;

  /* last radius will be placed at end of new listing */
  i--;
  /* get the limits and the increment to make cnt bands in this interval */
  last = radius[i];
  first = radius[i-1];
  inc = (last - first) / (double)cnt;
  /* set cnt to the last index, and move last radius */
  cnt += i-1;
  /* do not exceed max array index */
  if( cnt > maxcnt )
    cnt = maxcnt - 1;
  else
    radius[cnt] = radius[i];
  /* fill in the intermediate radii */
  while( i<cnt ) {
    first += inc;
    radius[i++] = first;
  }
  return( cnt + 1 );
}

/*
 * Subroutine:	got_nequal
 * Purpose:	Determine is line has tokens of form n= or n =
 * Returns:	Ptr to char after '=', else NULL
 * Note:	The n of n= can be any non-space char(s) except '='
 */
static char *got_nequal ( line )
     char *line;
{
  int i = 0;
  /* check for premature end */
  if( (line[i] == '\0') || (line[i] == '\n') )
    return( NULL );
  /* advance to token */
  while( line[i] == ' ' )
    i++;
  /* advance to end of token, or equal sign */
  while( (line[i] != ' ') && (line[i] != '\0') &&
	 (line[i] != '\n') && (line[i] != '=') )
    i++;
  /* if stopped on space, advance to next symbol */
  while( line[i] == ' ' )
    i++;
  /* symbol must be an equal sign, or we have an error */
  if ( (line[i] == '=') && (line[i+1] != '\0') && (line[i+1] != '\n') )
    return( &line[i+1] );
  else
    return( NULL );
}
