#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	imgnew.c (Image New)
 * Purpose:	Middle level routines to do all for a new image or buffer
 * Subroutine:	new_display()			returns: void
 * Subroutine:	new_panimage()			returns: void
 * Subroutine:	show_filename()			returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		1 July 1989
 *		{1} Jay Travisano (STScI)    VMS changes        10 Nov 1989
 *              {2} MVH BSDonly strings.h compatability         19 Feb 1990
 *		{3} MVH support for file buffer coord sys	19 Feb 1990
 *		{4} MVH altered inconsistancy in load_mainbuf	 1 Jan 1991
 *		{5} MVH changed call to copy_buf_replicate	19 Jun 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#define strchr index
#define strrchr rindex
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define SZ_FNAME */
#include "hfiles/constant.h"	/* constants and codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
extern struct windowRec desktop;


/*  Subroutine:	new_display
 *  Purpose:	Create all image coordinate parameters from the beginning
 * 		and take care of all that might be affected
 */
void new_display ( dispnow, clear, load, scale )
     int dispnow;	/* i: put-display-in-window-now flag */
     int clear;		/* i: clear img buf before reading flag */
     int load;		/* i: load-the-buffer flag */
     int scale;		/* i: make-new-scalemap flag (must have load)  */
{
  void set_disptran(), new_pancursor(), new_scalemap();
  void combine_transform(), set_edges(), set_dispoff(), set_magnifier();
  void adjust_cursor_coords(), map_dispbox(), disp_dispbox();
  static void  load_mainbuf();

  /*  Set disp to img and disp to file transforms  */
  set_disptran(&coord);
  /*  If the buffer needs new contents, fill it  */
  if( load || coord.buferror ) {
    /*  Load main buffer and set related coordinate parameters  */
    load_mainbuf(clear);
    /*  Set up or redraw the panbox cursor  */
    new_pancursor(0);
    /*  If none exists, set up buffer-to-display scaling map(s)  */
    if( scale )
      new_scalemap();
  }
  /*  Set transform to get from disp to buf coords  */
  combine_transform(&coord.disptobuf, &coord.disptoimg, &coord.imgtobuf);
  set_edges(&coord.disptobuf, &coord.buf, &coord.disp, &coord.bd);
  /*  Redefine the edges in the display window if at edge of buffer  */
  if( (coord.bd.block < 0) || (coord.bd.clip != 0) )
    set_dispoff(&coord.disptobuf, &coord.disp, &coord.bd);
  /*  Update cursor and magnifier info to correspond to current display  */
  adjust_cursor_coords(&cursor, &coord);
  set_magnifier();
  /*  Fill display buffer with new display image  */
  map_dispbox();
  if( dispnow )
    /*  Display the image  */
    disp_dispbox();
}


/*  Subroutine:	load_mainbuf
 *  Purpose:	Set buf related coords and load image data into main buffer
 */
static void load_mainbuf ( clear )
     int clear;
{
  int zoom;
  void set_buftran(), set_fbuftran(), load_image(), show_filename();

  /*  Select appropriate buffer contents in image field and file coords  */
  set_buftran(&coord);
  /*  Set file data buffer transform  */
  set_fbuftran(&img, &coord.buf, &coord.fbuf, &coord.buftofbuf);
  /*  Announce the loading  */
  if( control.verbose )
    (void)printf("Loading new image data into main buffer, please wait.\n");
  /*  Clear the buffer  */
  if( clear ) {
    bzero((char *)buffer.shortbuf, buffer.shortbuf_sz * sizeof(short));
/* need to zero file buffer too? 
    bzero((char *)buffer.filebuf, buffer.filebuf_sz);
*/
  }
  /*  Load the image  */
  load_image(&coord.fb, &coord.buf, &coord.buftofile, buffer.shortbuf,
	     buffer.filebuf, buffer.shortbuf_square, buffer.shortbuf_double);
  coord.buferror = 0;
  /*  Set the summed value compensation flag  */
  if( (img.block_type == SOP_ZoomSum) && ((zoom = coord.fb.block) > 1) )
    buffer.shortbuf_summing = zoom * zoom;
  else
    buffer.shortbuf_summing = 1;
  /*  Free data buffer if we don't need to keep it  */
  if( (!img.buffix) && ((char *)buffer.shortbuf != buffer.filebuf) ) {
    free( buffer.filebuf );
    buffer.filebuf = NULL;
    buffer.filebuf_sz = 0;
  }
  show_filename();
}


/*  Subroutine:	new_panimage
 *  Purpose:	Read image data into the panbox short buffer
 */
void new_panimage ()
{
  int block, block_x, block_y;
  void load_image(), copy_buf_replicate(), copy_buf_max(), map_panbox();

  buffer.panbuf_summing = 1;
  block = coord.fp.block;
  /*  If main buffer does not cover entire image, read from the file  */
  if( coord.bufcheck ) {
    if( control.verbose )
      (void)printf("Loading image data for wide view panning window.\n");
    /* Load in image data directly (%% warning, rework is needed aka filebuf */
    load_image(&coord.fp, &coord.pan, &coord.pantofile,
	       buffer.panbuf, buffer.filebuf, 0, 0);
    if( img.block_type == SOP_ZoomSum )
      buffer.panbuf_summing = block * block;
  } else {
    if( coord.pantoimg.inx_outx < 1.0 ) {
      copy_buf_replicate(buffer.shortbuf, buffer.panbuf, coord.pantoimg.ixzoom,
			 coord.buf.width, coord.pan.Xwdth, coord.pan.Yhght);
    } else {
      block_x = (int)coord.pantoimg.inx_outx;
      block_y = (int)coord.pantoimg.iny_outy;
      if (block_x > 1 || block_y > 1) {

	/*  Subzoom, blockaverage or zoommax may be used  */
	switch( img.panbox_zoomtype ) {
	case SOP_ZoomSamp:
	  copy_buf_subsample(buffer.shortbuf, buffer.panbuf, block_x,
			     coord.buf.width, coord.pan.Xwdth,
			     coord.pan.Yhght);
	  break;
	case SOP_ZoomSum:
	  copy_buf_sum(buffer.shortbuf, buffer.panbuf, block_x, block_y, 0,
		       coord.buf.width, coord.pan.Xwdth, coord.pan.Yhght);
	  break;
	case SOP_ZoomAv:
	  copy_buf_sum(buffer.shortbuf, buffer.panbuf, block_x, block_y, 1,
		       coord.buf.width, coord.pan.Xwdth, coord.pan.Yhght);
	  break;
	case SOP_ZoomMax:
	default:
	  copy_buf_max(buffer.shortbuf, buffer.panbuf, block_x, block_y,
		       coord.buf.width, coord.pan.Xwdth, coord.pan.Yhght);
	  break;
	}
      } else {
	bcopy((char *)buffer.shortbuf, (char *)buffer.panbuf,
	      coord.pan.Xwdth * coord.pan.Yhght * sizeof(short));
      }
    }
    /*  Same zoom summing as buffer  */
    buffer.panbuf_summing = buffer.shortbuf_summing;
  }
  /*  Calculate the max value in the buffer for scaling purposes  */
  if( img.block_type == SOP_ZoomSum ) {
    register int buf_max;
    register short *buf, *bufend;

    buf = buffer.panbuf;
    bufend = buf + (panbox.xwidth * panbox.yheight * sizeof(short));
    buf_max = *buf;
    while( buf < bufend ) {
      if( *buf > buf_max )
	buf_max = *buf;
      buf++;
    }
    buffer.panbuf_max = buf_max;
  }
  /*  Map the short data into the display buffer  */
  map_panbox();
}


/*  Subroutine:	show_filename
 *  Purpose:	Print the image file name in the desktop
 */
void show_filename ( )
{
  int len, dir_len, file_len;
  int dir_x, dir_y, file_y;
  char *dir_end;
  char dir_string[SZ_FNAME];
  char file_string[SZ_FNAME];
  static XFontStruct *fontstruct = NULL;
  GC gc, set_edit_gc();
  XFontStruct *get_fontstruct();

  if( fontstruct == NULL )
    fontstruct = get_fontstruct(1);
  file_y = 2 * fontstruct->ascent;
  dir_y = file_y + fontstruct->ascent + fontstruct->descent;
  dir_x = 5;
  XClearArea(desktop.display, desktop.ID, 0, 0, panbox.x, btnbox.y, False);
  if( (img.filename != NULL) && ((len = strlen(img.filename)) > 0) ) {
   if( (img.file_type == SOP_Imtool) || (img.file_type == SOP_PROS) ) {
      dir_len  = strlen(strcpy(dir_string, "(IRAF)"));
      file_len = strlen(strcpy(file_string, img.filename));
   } else {
#ifndef VMS
    (void)strcpy(file_string, "file: ");
    if( (dir_end = strrchr(img.filename, '/')) == NULL ) {
      (void)strcpy(dir_string, "dir: .");
      dir_len = 6;
      (void)strcat(file_string, img.filename);
      file_len = len + 6;
    } else {
      (void)strcpy(dir_string, "dir: ");
      dir_len = dir_end - img.filename;
      (void)strncat(dir_string, img.filename, dir_len);
      dir_len += 5;
      (void)strcat(file_string, dir_end+1);
      file_len = 10 + len - dir_len;
    }
#else
    /*  Parse VMS-style directory/filename  */

    (void)strcpy(file_string, "file: ");
    (void)strcpy(dir_string,  "dir:  ");

    if( (dir_end = strrchr(img.filename, '/')) == NULL )
      if( (dir_end = strrchr(img.filename, ']')) == NULL )
        dir_end = strrchr(img.filename, ':');

    if( dir_end == NULL ) {
      (void)strcat(dir_string, "[]");
      (void)strcat(file_string, img.filename);
    } else {
      dir_len = dir_end - img.filename + 1;
      (void)strncat(dir_string, img.filename, dir_len);
      (void)strcat(file_string, dir_end+1);
    }

    dir_len  = strlen(dir_string);
    file_len = strlen(file_string);
#endif
   }

    gc = set_edit_gc(fontstruct->fid, color.gcset.menu.foreground,
		     color.gcset.menu.background);
    XDrawImageString(desktop.display, desktop.ID, gc,
		     dir_x, file_y, file_string, file_len);
    XDrawImageString(desktop.display, desktop.ID, gc,
		     dir_x, dir_y, dir_string, dir_len);
  }
}
