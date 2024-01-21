#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	grphpos.c (Color Graph Position)
 * Subroutine:	select_best_hash_position()		returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 11 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */

#define ABS(a) ((a) < 0 ? (-(a)) : (a))

/*
 * Subroutine:	select_best_hash_position
 * Purpose:	Select the best place in the list for a new hash mark, based
 *		on its coordinates and those of existing hash marks.
 * Returns:	The hash index to give the new hash mark.
 * Note:	match is used to force new vertex to use same cell_level as
 *		the vertex which it is next to. When vertexes have same
 *		screen coordinate, converting to a real may produce values
 *		in the wrong order.
 */
int select_best_hash_position ( x, y, hash, hash_cnt, match, vertical )
     int x, y;
     XRectangle hash[];
     int hash_cnt;
     int *match;		/* o: close hash with same x value */
     int vertical;		/* i: flag for vertical, else horizontal */
{
  int i;

  /* initially no existing hash with which to match level */
  *match = -1;
  if( hash_cnt == 0 )
    return( 0 );
  if( vertical ) {
    /* vertical */
    for( i = 0; ((i < hash_cnt) && (y < hash[i].y)); i++ );
    if( (i == hash_cnt) || (y > hash[i].y) )
      /* y between this and previous hash mark (or beyond either end) */
      return( i );
    *match = i;
    /* else y same as hash i's y */
    if( y == (hash_cnt - 1) ) {
      /* same as last hash mark */
      if( x > hash[i].x ) {
	if( hash[i-1].x > hash[i].x )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].x < hash[i].x )
	  return( i );
	else
	  return( i+1 );
      }
    }
    if( y > hash[i+1].y ) {
      /* same y as one other hash mark */
      if( i == 0 ) {
	/* same as first one */
	if( x > hash[i].x ) {
	  if( hash[i+1].x < hash[i].x )
	    return( i );
	  else
	    return( i+1 );
	} else {
	  if( hash[i+1].x < hash[i].x )
	    return( i );
	  else
	    return( i+1 );
	}
      }
      /* not same as first one */
      if( x > hash[i].x ) {
	if( hash[i-1].x > hash[i+1].x )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].x < hash[i+1].x )
	  return( i );
	else
	  return( i+1 );
      }
    }
    {
      int close, dx, j;
      /* y matches two or more hashes' y */
      close = ABS(x - hash[i].x);
      /* advance to closest hash with same y */
      for( j=i+1; (j<hash_cnt) && (hash[j].y == y); j++ ) {
	dx = ABS(x - hash[j].x);
	if( dx <= close ) {
	  close = dx;
	  i = j;
	}
      }
      *match = i;
    }
    if( i == 0 ) {
      /* first hash (second also same y) */
      if( x > hash[i].x ) {
	if( hash[i+1].x < hash[i].x )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i+1].x > hash[i].x )
	  return( i );
	else
	  return( i+1 );
      }
    }
    if( i == (hash_cnt - 1) ) {
      /* last hash (previous also same y) */
      if( x > hash[i].x ) {
	if( hash[i-1].x > hash[i].x )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].x < hash[i].x )
	  return( i );
	else
	  return( i+1 );
      }
    }
    /* not an extreme hash, at least one other with same y */
    if( hash[i-1].y == hash[i+1].y ) {
      /* middle of three with same y */
      if( x > hash[i].x ) {
	if( hash[i-1].x > hash[i+1].x )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].x < hash[i+1].x )
	  return( i );
	else
	  return( i+1 );
      }
    }
    if( hash[i-1].y == y ) {
      if( x > hash[i].x ) {
	if( hash[i-1].x > hash[i].x )
	  /* new position between two with same y */
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].x < hash[i].x )
	  /* new position between two with same y */
	  return( i );
	else
	  return( i+1 );
      }
    } else {
      if( x > hash[i].x ) {
	if( hash[i+1].x > hash[i].x )
	  /* new position between two with same y */
	  return( i+1 );
	else
	  return( i );
      } else {
	if( hash[i+1].x < hash[i].x )
	  /* new position between two with same y */
	  return( i+1 );
	else
	  return( i );
      }
    }
  } else {
    /* horizontal */
    for( i = 0; ((i < hash_cnt) && (x > hash[i].x)); i++ );
    if( (i == hash_cnt) || (x < hash[i].x) )
      /* x between this an previous hash mark (or beyond either end) */
      return( i );
    /* else x same as hash i's x */
    *match = i;
    if( x == (hash_cnt - 1) ) {
      /* same as last hash mark */
      if( y > hash[i].y ) {
	if( hash[i-1].y > hash[i].y )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].y < hash[i].y )
	  return( i );
	else
	  return( i+1 );
      }
    }
    if( x < hash[i+1].x ) {
      /* same x as one other hash mark */
      if( i == 0 ) {
	/* same as first one */
	if( y > hash[i].y ) {
	  if( hash[i+1].y < hash[i].y )
	    return( i );
	  else
	    return( i+1 );
	} else {
	  if( hash[i+1].y < hash[i].y )
	    return( i );
	  else
	    return( i+1 );
	}
      }
      /* not same as first one */
      if( y > hash[i].y ) {
	if( hash[i-1].y > hash[i+1].y )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].y < hash[i+1].y )
	  return( i );
	else
	  return( i+1 );
      }
    }
    {
      int close, dy, j;
      /* x matches two or more hashes' x */
      close = ABS(y - hash[i].y);
      /* advance to closest hash with same x */
      for( j=i+1; (j<hash_cnt) && (hash[j].x == x); j++ ) {
	dy = ABS(y - hash[j].y);
	if( dy <= close ) {
	  close = dy;
	  i = j;
	}
      }
      *match = i;
    }
    if( i == 0 ) {
      /* first hash (second also same x) */
      if( y > hash[i].y ) {
	if( hash[i+1].y < hash[i].y )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i+1].y > hash[i].y )
	  return( i );
	else
	  return( i+1 );
      }
    }
    if( i == (hash_cnt - 1) ) {
      /* last hash (previous also same x) */
      if( y > hash[i].y ) {
	if( hash[i-1].y > hash[i].y )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].y < hash[i].y )
	  return( i );
	else
	  return( i+1 );
      }
    }
    /* not an extreme hash, at least one other with same x */
    if( hash[i-1].x == hash[i+1].x ) {
      /* middle of three with same x */
      if( y > hash[i].y ) {
	if( hash[i-1].y > hash[i+1].y )
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].y < hash[i+1].y )
	  return( i );
	else
	  return( i+1 );
      }
    }
    if( hash[i-1].x == x ) {
      if( y > hash[i].y ) {
	if( hash[i-1].y > hash[i].y )
	  /* new position between two with same x */
	  return( i );
	else
	  return( i+1 );
      } else {
	if( hash[i-1].y < hash[i].y )
	  /* new position between two with same x */
	  return( i );
	else
	  return( i+1 );
      }
    } else {
      if( y > hash[i].y ) {
	if( hash[i+1].y > hash[i].y )
	  /* new position between two with same x */
	  return( i+1 );
	else
	  return( i );
      } else {
	if( hash[i+1].y < hash[i].y )
	  /* new position between two with same x */
	  return( i+1 );
	else
	  return( i );
      }
    }
  }
}
