#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgnwrite.c (Region Write)
 * Purpose:	Write descriptions of saved regions to a file
 * Subroutine:	write_regions()		returns: void
 * Subroutine:	timestamp()		returns: void
 * UNIX calls:	time(), asctime(), localtime(), fclose()
 * Copyright:	1989, 1990, 1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  9 July 1989
 *		{1} MVH removed comma from list output		   7 Dec 1989
 *              {2} MVH BSDonly strings.h compatability           19 Feb 1990
 *              {3} Doug Mink  cast strlen to int for max          4 May 1995
 *              {4} Doug Mink  fix time() declaration for VMS      8 Sep 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, FILE, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <time.h>		/* needed for asctime() */
#include <X11/Xlib.h>		/* get X types and constants */
#include <X11/Xutil.h>		/* X window manager, visual stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/color.h"	/* colors used by cursor */
#include "hfiles/cursor.h"	/* define cursor parameter structures */
#include "hfiles/define.h"	/* define SZ_FNAME, MIN, MAX, etc. */
#include "hfiles/image.h"	/* image description structure */
#include "hfiles/edit.h"	/* define EditStruct */
#include "hfiles/extern.h"	/* pass common structures */

#ifndef VMS
#define	IRAFFILE	"frame.%d.%d"
#else
#define	IRAFFILE	"frame.%d_%d"
#endif

char *ProsName = "saoimage.reg";

EditStruct *region_edit;

/*
 * Subroutine:	write_regions
 * Purpose:	Write region info to a file
 */
void write_regions ( cursor, image, output_type )
     struct cursorRec *cursor;
     struct imageRec *image;
     int output_type;		/* i: SOP_Imtool or SOP_PROS */
{
  FILE *fd;
  static int last_output_type = 0;
  char fname[SZ_FNAME];
  static void write_region_imtool();
  int open_output_file();
  EditStruct *init_edit_popup();
  void set_path_iraf(), write_region_pros(), load_edit_struct(), timestamp();

  /* return if there is nothing to write */
  if( cursor->next_region == NULL ) {
    (void)fprintf(stderr, "WARNING: no saved cursors to write!\n");
    return;
  }
  /* imtool type output is different */
  if( output_type == SOP_Imtool ) {
    if( last_output_type != SOP_Imtool ) {
      /* get name of coordinate output file */
      strcpy (fname,img.filename);
      strcat (fname,".plist");
      /* sprintf(fname, IRAFFILE, 1, 1); */
      /* set_path_iraf(fname); */
      if( region_edit == NULL )
	region_edit = init_edit_popup(fname, SZ_FNAME);
      else
	load_edit_struct(region_edit, fname, strlen(fname));
      last_output_type = SOP_Imtool;
    }
    /* open coordinate output file for writing */
    if( open_output_file(&fd, region_edit, 0,
			 "Enter file name for IRAF list:") <= 0 )
      return;
    /* timestamp the first entry in the output file. */
    /* timestamp(fd, image->filename); */
    /* put in a line to explain parameter list */
    /* (void)fprintf(fd, "# (x y)\n"); */
    write_region_imtool(fd, cursor->next_region, image);
  } else {
    if( last_output_type != SOP_PROS ) {
      if( region_edit == NULL )
	region_edit = init_edit_popup(ProsName, SZ_FNAME);
      else
	load_edit_struct(region_edit, ProsName, strlen(fname));
      last_output_type = SOP_PROS;
    }
    /* open region output file for writing */
    if( open_output_file(&fd, region_edit, 0,
			 "Enter file name for regions:") <= 0 )
      return;
    /* timestamp the first entry in the output file. */
    timestamp(fd, image->filename);
    /* put in a line to explain parameter list */
    (void)fprintf(fd,"# shape x, y, [x dimension, y dimension], [angle]\n");
    /* write the pros region specifications */
    write_region_pros(fd, cursor->next_region);
  }
  (void)fclose(fd);
}

/*
 * Subroutine:	write_region_imtool
 * Purpose:	Write region info in imtool's format (iraf "list")
 * Note:	Output is a list of ascii pairs giving only center coords
 */
static void write_region_imtool ( fd, region, image )
     FILE *fd;
     struct cursorRec *region;
     struct imageRec *image;
{
  if( region != NULL ) {
    /* recurse first to reverse order of regions */
    if( region->next_region != NULL )
      write_region_imtool(fd, region->next_region, image);
    if( image->index_base )
      (void)fprintf(fd, "%5d %5d\n", RND(region->file.X), RND(region->file.Y));
    else
      (void)fprintf(fd, "%5d %5d\n", (int)region->file.X, (int)region->file.Y);
  }
}

/*
 * Subroutine:	timestamp
 * Purpose:	Write date and filename (if known) to region file
 */
void timestamp ( fd, imagename )
     FILE *fd;
     char *imagename;
{
  char  line[SZ_LINE];
  int len;
  static void set_time_string();

  /* timestamp the first entry in the output file. */
  set_time_string (line);
  /* get rid of any trailing new line */
  len = MAX(0, (int)strlen(line) - 1);
  if (line[len] == '\n')
    line[len] = '\0';
  /* print file name if one is given */
  if ((imagename != NULL) && (strlen(imagename)) != 0) {
    (void)fprintf(fd, "# %s\n", imagename);
  }
  /* print date and time */
  (void)fprintf(fd, "# %s\n", line);
}

/*
 * Subroutine:	set_time_string (time)
 * Purpose:	Print the time in the passed string
 * UNIX calls:	time(), asctime(), localtime()
 */
static void set_time_string ( time_string )
     char *time_string;
{
  long timeofday;
#
  /* I don't know where this should be defined, it's not in <time.h> */
  /* it's a time_t which is defined as a long in <sys/types.h> */
#ifdef LONG64
  int time();
#else
#ifndef VMS
  long time();
#else
  time_t time();
#endif
#endif

  timeofday = time((long *)0);
  /* note asctime(localtime()) can be replaced by ctime() in newer OS's */
  (void)strcpy(time_string, asctime(localtime(&timeofday)));
}
