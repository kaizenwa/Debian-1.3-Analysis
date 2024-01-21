#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clrmap.c (Color Map)
 * Purpose:	Set up the cell storemap XColor array
 * Subroutine:	make_cellstore_from_tables()	returns: void
 * Subroutine:	make_cellstore_from_cellmaps()	returns: void
 * Subroutine:	make_cellmap_from_table()	returns: void
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   16 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */
#include <math.h>			/* define pow */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/color.h"		/* color structs */
#include "hfiles/define.h"		/* INTERP, INCSZ, and more */

/*
 * Subroutine:	make_cellstore_from_table
 * Purpose:	Fill the color map according to the color vertex tables
 */
void make_cellstore_from_tables ( color )
     struct colorRec *color;
{
  void make_cellmap_from_table(), make_cellstore_from_cellmaps();

  make_cellmap_from_table(&color->ctable.red);
  make_cellmap_from_table(&color->ctable.green);
  make_cellmap_from_table(&color->ctable.blue);
  make_cellstore_from_cellmaps(color);
}

/*
 * Subroutine:	make_cellstore_from_cellmaps
 * Purpose:	Fill XColor storemap from individual cellmaps
 */
void make_cellstore_from_cellmaps ( color )
     struct colorRec *color;
{
  double gamma;
  double *cellmap;
  double *gammamap;
  register int i;
  int ncolors;
  XColor *cellstore;

  cellstore = color->cellstore;
  ncolors = color->ncolors;

  cellmap = color->ctable.red.cellmap;
  if( color->ctable.red.do_gamma ) {
    gammamap = color->ctable.red.gammamap;
    gamma = 1.0 / color->ctable.red.gamma;
    for( i=0; i<ncolors; i++ )
      cellstore[i].red = (unsigned short)
	(65535.0 * (gammamap[i] = pow(cellmap[i], gamma)));
  } else {
    for( i=0; i<ncolors; i++ )
      cellstore[i].red = (unsigned short)(cellmap[i] * 65535.0);
  }
  cellmap = color->ctable.green.cellmap;
  if( color->ctable.green.do_gamma ) {
    gammamap = color->ctable.green.gammamap;
    gamma = 1.0 / color->ctable.green.gamma;
    for( i=0; i<ncolors; i++ )
      cellstore[i].green = (unsigned short)
	(65535.0 * (gammamap[i] = pow(cellmap[i], gamma)));
  } else {
    for( i=0; i<ncolors; i++ )
      cellstore[i].green = (unsigned short)(cellmap[i] * 65535.0);
  }
  cellmap = color->ctable.blue.cellmap;
  if( color->ctable.blue.do_gamma ) {
    gammamap = color->ctable.blue.gammamap;
    gamma = 1.0 / color->ctable.blue.gamma;
    for( i=0; i<ncolors; i++ )
      cellstore[i].blue = (unsigned short)
	(65535.0 * (gammamap[i] = pow(cellmap[i], gamma)));
  } else {
    for( i=0; i<ncolors; i++ )
      cellstore[i].blue = (unsigned short)(cellmap[i] * 65535.0);
  }
}

/*
 * Subroutine:	make_cellmap_from_table
 * Purpose:	Fill cellstore values based on table values
 * Method:	Set map values using interpolated ramps between map entries
 */
void make_cellmap_from_table ( table )
     struct subtableRec *table;
{
  double cell_factor;
  double level_last;
  double level_next;
  double start_intensity;	/* l: intensity at start cell */
  double intensity_inc;		/* l: change in intensity between cells */
  double intensity_last;
  double intensity_next;
  double *cellmap;		/* l: intensity by cell array */
  int start_cell, stop_cell;	/* l: cells at ends of ramp function */
  int table_cnt;
  int map_sz;
  int table_i, map_i;

  /* initialization phase */
  table_cnt = table->vertex_cnt;
  cellmap = table->cellmap;
  map_sz = table->map_sz;
  cell_factor = (double)(map_sz - 1);
  table_i = 0;
  while( (table->cell_level[table_i] < 0.0) && (++table_i < table_cnt) );
  if( table_i >= table_cnt ) {
    /* entire table is below 0.0: set entire map with last intensity */
    start_intensity = table->intensity[table_cnt - 1];
    for( map_i=0; map_i<map_sz; map_i++ )
      cellmap[map_i] = start_intensity;
    return;
  } else if( table_i > 0 ) {
    /* table extends partially below 0.0 */
    level_last = table->cell_level[table_i - 1];
    intensity_last = table->intensity[table_i - 1];
  } else {
    /* table starts at or above 0.0 */
    level_last = 0.0;
    intensity_last = table->intensity[table_i];
  }

  /* middle phase: run through map or table */
  start_cell = 0;
  do {
    level_next = table->cell_level[table_i];
    intensity_next = table->intensity[table_i];
    stop_cell = (int)(level_next * cell_factor);
    if( stop_cell >= map_sz )
      /* stop at end of map if there */
      stop_cell = map_sz - 1;
    if( stop_cell >= start_cell ) {
      /* if levels traverse any map cells, set them */
      start_intensity =
	INTERP(level_last, ((double)start_cell)/cell_factor, level_next,
	       intensity_last, intensity_next);
      if( stop_cell == start_cell ) {
	/* if only one cell is traversed, set it */
	cellmap[start_cell] = start_intensity;
      } else {
	intensity_inc = INCSZ(level_last, level_next,
			      intensity_last, intensity_next, cell_factor);
	for( map_i=start_cell; map_i<=stop_cell; map_i++ ) {
	  cellmap[map_i] = start_intensity;
	  start_intensity += intensity_inc;
	}
	/* check for slight over-step at end */
	if( cellmap[stop_cell] < 0.0 )
	  cellmap[stop_cell] = 0.0;
	else if( cellmap[stop_cell] > 1.0 )
	  cellmap[stop_cell] = 1.0;
      }
      start_cell = stop_cell + 1;
    }
    level_last = level_next;
    intensity_last = intensity_next;
  } while( (level_last < 1.0) && (++table_i < table_cnt) );
    
  /* completion phase */
  if( start_cell < map_sz ) {
    /* table ended before 1.0 */
    for( map_i=start_cell; map_i<map_sz; map_i++ )
      cellmap[map_i] = intensity_last;
  }
}
