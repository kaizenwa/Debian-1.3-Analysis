#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	clrmenu.c (Color Menu)
 * Purpose:	Read and Write file and internally stored color tables
 * Subroutine:	fetch_colortable()		returns: int
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  16 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/constant.h"		/* code values */
#include "hfiles/color.h"		/* color structs */
#include "hfiles/colormap.h"		/* color table storage structs */
#include "hfiles/define.h"		/* define SZ_FNAME, etc. */
#include "hfiles/edit.h"		
/* EditStruct */
#include "defs/colormap.def"		/* define color tables */

EditStruct *color_edit;	/* key to popup editor for file name input */


/*
 * Subroutine:	fetch_colortable
 * Purpose:	Get a new pre-defined color table and load it for use
 * Returns:	1 if succeeded, else 0
 */
int fetch_colortable ( color, table_code, imagefile )
     struct colorRec *color;
     int table_code;
     char *imagefile;		/* i: imagefile name to print in output file */
{
  ColorTable *newtable;
  static void load_newtable(), write_color_file();
  static ColorTable *read_color_file();

  switch( table_code ) {
  case MOP_Init_A:
    newtable = &gray_map;
    break;
  case MOP_Init_B:
    newtable = &bbs_map;
    break;
  case MOP_Init_C:
    newtable = &hip_map;
    break;
  case MOP_Init_D:
    newtable = &imp8_map;
    break;
  case MOP_Init_E:
    newtable = &a_map;
    break;
  case MOP_Init_F:
    newtable = &b_map;
    break;
  case MOP_Read:
    if( (newtable = read_color_file()) == NULL )
      return( 0 );
    break;
  case MOP_Write:
    write_color_file(imagefile, color);
    return( 0 );
  default:
    (void)fprintf(stderr, "WARNING: unknown color map code!\n");
    return( 0 );
  }
  load_newtable(&color->ctable, newtable);
  return( 1 );
}

static ColorTable ctable;

/*
 * Subroutine:	read_color_file
 * Purpose:	Open and read a color table file, and load it into the
 *		current color table.
 */
static ColorTable *read_color_file ( )
{
  FILE *fp;
  int open_input_file(), parse_color_file();
  EditStruct *init_edit_popup();

  if( color_edit == NULL )
    color_edit = init_edit_popup((char *)NULL, SZ_FNAME);
  /* open coordinate output file for writing */
  if( open_input_file(&fp, color_edit, 0,
		       "Enter name of pseudocolor file:") <= 0 )
    return( NULL );
  bzero((char *)&ctable, sizeof(ColorTable));
  if( parse_color_file(fp, &ctable, CTBL_MAX) ) {
    (void)fclose(fp);
    return( &ctable );
  } else {
    (void)fclose(fp);
    return( NULL );
  }
}

/*
 * Subroutine: write_color_file
 * Purpose:    Open and write a color table file
 */
static void write_color_file ( imagefile, color )
     char *imagefile;
     struct colorRec *color;
{
  FILE *fp;
  int error;
  static void print_one_color();
  EditStruct *init_edit_popup();
  int open_output_file();
  void timestamp();

  error = 0;
  if( color->ctable.red.vertex_cnt > CTBL_MAX ) {
    (void)fprintf(stderr,
		  "WARNING: too many red entries for portable table\n");
    error = 1;
  }
  if( color->ctable.green.vertex_cnt > CTBL_MAX ) {
    (void)fprintf(stderr,
		  "WARNING: too many green entries for portable table\n");
    error = 1;
  }
  if( color->ctable.blue.vertex_cnt > CTBL_MAX ) {
    (void)fprintf(stderr,
		  "WARNING: too many blue entries for portable table\n");
    error = 1;
  }
  if( color_edit == NULL )
    color_edit = init_edit_popup((char *)NULL, SZ_FNAME);
  /* open coordinate output file for writing */
  if( open_output_file(&fp, color_edit, 0,
		      "Enter name of pseudocolor file:") <= 0 )
    error = 1;
  if( error ) {
    (void)fprintf(stderr,"No color file written\n");
    XBell(color->display, 80);
    return;
  }
  /* timestamp the first entry in the output file. */
  (void)fprintf(fp, "# SAOimage color table\n");
  timestamp(fp, imagefile);
  (void)fprintf(fp, "PSEUDOCOLOR\n");
  (void)fprintf(fp, "RED:");
  print_one_color(fp, &color->ctable.red);
  (void)fprintf(fp, "GREEN:");
  print_one_color(fp, &color->ctable.green);
  (void)fprintf(fp, "BLUE:");
  print_one_color(fp, &color->ctable.blue);
  fclose(fp);
}

/*
 * Subroutine:
 * Purpose:
 */
static void print_one_color ( fp, table )
     FILE *fp;
     struct subtableRec *table;
{
  int i, j;
  if( table->do_gamma )
    (void)fprintf(fp, " gamma %.3f\n", table->gamma);
  else
    (void)fprintf(fp, "\n");
  for( i=0, j=0; i<table->vertex_cnt; i++ ) {
    (void)fprintf(fp,"(%.3f,%.3f)",table->cell_level[i],table->intensity[i]);
    if( ++j >= 5 ) {
      j=0;
      (void)fprintf(fp, "\n");
    }
  }
  (void)fprintf(fp, "\n");
}

/*
 * Subroutine:	load_newtable
 * Purpose:	Load an internally stored color table for use
 */
static void load_newtable ( ctable, new )
     struct colorTable *ctable;
     ColorTable *new;
{
  static void load_subtable();

  load_subtable(&ctable->red, &new->red);
  load_subtable(&ctable->green, &new->green);
  load_subtable(&ctable->blue, &new->blue);
}

/*
 * Subroutine:	load_subtable
 * Purpose:	Load one color of the internal color table
 */
static void load_subtable ( subtable, new )
     struct subtableRec *subtable;
     struct SubTable *new;
{
  int i;

  subtable->fixed_cells = 0;
  if( new->do_gamma ) {
    subtable->do_gamma = 1;
    subtable->gamma = new->gamma;
  } else {
    subtable->do_gamma = 0;
    subtable->gamma = 1.0;
  }
  subtable->invert_order = 0;
  subtable->vertex_cnt = new->vertex_cnt;
  for( i=0; i<new->vertex_cnt; i++ ) {
    subtable->cell_level[i] = new->cell_level[i];
    /* base levels used for dynamic manipulation */
    subtable->base_level[i] = new->cell_level[i] - 0.5;
    subtable->intensity[i] = new->intensity[i];
  }
  subtable->bias = 0.5;
  subtable->contrast = 1.0;
}
