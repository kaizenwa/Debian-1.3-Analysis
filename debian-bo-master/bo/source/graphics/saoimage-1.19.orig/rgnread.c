#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgnread.c (Region Read)
 * Purpose:	Open and read ASCII region specification files and cursor lists
 * Subroutine:	read_regions()		returns: void
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		9 August 1989
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <ctype.h>		/* toupper, isalpha, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/define.h"	/* SZ_FNAME, SZ_LINE */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main SAOimage parameter structures */
#include "hfiles/edit.h"	/* EditStruct */
#include "hfiles/region.h"	/* region parsing record (reg_param) */

extern EditStruct *region_edit;	/* key to popup editor for file name input */


#ifdef ANSIC

void	    read_regions();
static int  check_special(	char *line);
static int  determine_file_type(char *line, int len);
static void parse_imtool_points(char *line);
static void parse_pros_regions ( char *line )
static void parse_point(	 char *line, int exclude);

#else

static int check_special();
static int determine_file_type();
static void parse_imtool_points(), parse_pros_regions(), parse_point();
  EditStruct *init_edit_popup();
  int open_input_file();
  void disp_dispbox();
  int check_parens(), burst_line();
  void make_next_region(), fit_annuli_edge(), new_annulus_edge();
  struct cursorRec *get_new_cursor();
  void set_cursor_from_file_coords(), free_cursor(), disp_region();

#endif


/*
 * Subroutine:	read_regions
 * Purpose:	Read cursors into the save cursor records from a file
 * Note:	Each new entry is added at base of list
 */
void read_regions ( )
{
  FILE *fd;
  int type;
  char line[SZ_LINE];

  if( region_edit == NULL )
    region_edit = init_edit_popup((char *)NULL, SZ_FNAME);
  /*  Open coordinate output file for writing  */
  if( open_input_file(&fd, region_edit, 0,
		      "Enter file name of IRAF list or PROS regions:") <= 0 )
    return;
  while( (fgets(line, SZ_LINE, fd) != NULL) &&
	 ((type = determine_file_type(line, SZ_LINE)) == 0) );
  if( type == SOP_Imtool ) {
    do {
      /*  Parse the line, allocating region records  */
      parse_imtool_points (line);
    } while( fgets(line, SZ_LINE, fd) != NULL );
  } else if( type == SOP_PROS ) {
    do {
      /*  Parse the line, allocating region records  */
      parse_pros_regions (line);
    } while( fgets(line, SZ_LINE, fd) != NULL );
  }
  /*  Close the region file  */
  (void)fclose(fd);
  /*  Draw the display with regions labeled as per display selection  */
  (void)disp_dispbox();
}


/*  Subroutine:	determine_file_type
 *  Purpose:	Determine if line is commented or blank
 *  Returns:	0 if line is blank fo comment, SOP_Imtool if starts with a
 *		number, SOP_PROS if starts with a letter, +, or -
 */
#ifdef ANSIC
static int determine_file_type ( char *line, int len )
#else
static int determine_file_type ( line, len )
     char *line;
     int len;
#endif
{
  int i;
  if( check_special(line) )
    return( SOP_PROS );
  for( i=0; i<len; i++ ) {
    if( (line[i] == '\0') || (line[i] == '#') || (line[i] == '\n') )
      return( 0 );
    if( isascii(line[i]) && (!isspace(line[i])) ) {
      if( isdigit(line[i]) )
	return( SOP_Imtool );
      else if( isalpha(line[i]) || (line[i] == '!') ||
	       (line[i] == '-') || (line[i] == '+') )
	return( SOP_PROS );
    }
  }
  return( 0 );
}


/*  Subroutine:	parse_imtool_points
 *  Purpose:	Read region info in imtool's format
 */
#ifdef ANSIC
static void parse_imtool_points ( char *line )
#else
static void parse_imtool_points ( line )
     char *line;
#endif
{
  /*  Ignore blank or commented lines  */
  if( (line[0] != '\0') && (line[0] != '#') && (line[0] != '\n') )
    /*  Parse line as single point for exclusion (IRAF bad pixel list)  */
    parse_point(line, 1);
}


/*  Subroutine:	parse_pros_regions
 *  Purpose:	Read region info in PROS format
 */
#ifdef ANSIC
static void parse_pros_regions ( char *line )
#else
static void parse_pros_regions ( line )
     char *line;
#endif
{
  int count, len, i;
  int scratch_sz = 128;
  float scratch[128];
  struct reg_param records[REG_LIMIT];
  char temp[SZ_LINE];


  /*  If we get a special label type, advance beyond the protective #  */
  if( check_special(line) )
    (void)strcpy(temp, &line[1]);
  else
    (void)strcpy(temp, line);
  /*  Check parens and set all token separators to ascii space  */
  if( (len = check_parens(line, temp)) < 5 )
    return;
  /*  Identify and count distinct region specifications  */
  if( (count = burst_line(line, temp, len, records)) < 1 )
    return;
  /*  If 2 regions forming an annulus  */
  if( (count == 2) && (records[0].not == 0) && (records[1].not == 1) ) {
    make_next_region(records[1].type, records[1].line,
		     records[1].exclude, 0, scratch, scratch_sz);
    /*  If the inner edge is same as previous edge  */
    if( cursor.next_region->next_region != NULL )
      fit_annuli_edge(&cursor);
    make_next_region(records[0].type, records[0].line,
		     records[0].exclude, 1, scratch, scratch_sz);
    new_annulus_edge(&cursor);
  } else {
    for( i=0; i<count; i++ )
      make_next_region(records[i].type, records[i].line,
		       records[i].exclude, 1, scratch, scratch_sz);
  }
}


/*  Subroutine:	parse_point
 *  Purpose:	parse a point's parameters
 *  Note:	Recurse on multiple center parameters, to make additional
 *		point regions
 */
#ifdef ANSIC
static void parse_point ( char *line, int exclude )
#else
static void parse_point ( line, exclude )
     char *line;		/* line to parse for center */
     int exclude;		/* include or exclude point(s) */
#endif
{
  struct cursorRec *region;

  /*  Get cleared record space and set some key parameters  */
  region = get_new_cursor(0, 0);
  region->type = COP_Point;
  region->exclude_region = exclude;
  if( sscanf(line, "%f%*c%f", &region->file.X, &region->file.Y) != 2 ) {
    free_cursor(region);
  } else {
    /*  Set up this point region  */
    set_cursor_from_file_coords(region, &coord.filetodisp);
    if( (region->next_region = cursor.next_region) != NULL )
      region->index = cursor.next_region->index + 1;
    else
      region->index = 1;
    cursor.next_region = region;
    if( (region->exclude_region = exclude) )
      region->draw = &color.gcset.excl;
    else
      region->draw = &color.gcset.incl;
    region->win.display = dispbox.display;
    region->win.ID = dispbox.ID;
    region->overwrites_image_data = 1;
    disp_region(region);
  }
}


/*  Subroutine:	check_special
 *  Purpose:	Look for special label type cursor declaration
 */
#ifdef ANSIC
static int check_special( char *line )
#else
static int check_special( line )
     char *line;
#endif
{
  if(   (strncmp(line, "# ARROW(", 8) == 0)
     || (strncmp(line, "#-ARROW(", 8) == 0)
     || (strncmp(line, "# TEXT(", 7) == 0)
     || (strncmp(line, "#-TEXT(", 7) == 0) )
    return( 1 );
  else
    return( 0 );
}
