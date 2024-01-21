#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	irafdisp.c (Iraf Display)
 * Purpose:	Respond to input from IRAF meant for Imtool
 * Subroutine:	imtool_reinit()			returns: int
 * Subroutine:	disp_subpiece()			returns: void
 * Subroutine:	set_imtool_scale()		returns: void
 * Subroutine:	set_imtool_color()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst				7 Nov 1989
 *		{1} MVH don't set imtool_colors if halftone    24 Nov 1989
 *		{2} FV change scale_max to 199 to insure        9 Oct 1992
 *		      unit scaling
 *		{n} <who> -- <does what> -- <when>
 */

#ifdef IMTOOL

#include <stdio.h>		/* stderr, FILE, NULL, etc. */
#include <X11/Xlib.h>		/* get X types and constants */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main SAOimage parameter structures */
#include "hfiles/scale.h"	/* define scaling constants */

/*
 * Subroutine:	imtool_reinit
 * Purpose:	Reinitialize parameters after any buffer size change and
 *		before loading new
 */
void imtool_reinit ( width, height )
     int width, height;
{
  void init_img(), init_dispcen(), d_transform(), new_panbox();
  int init_imagebuf();

  img.fiX1 = 1;
  img.fiY1 = 1;
  img.fiX2 = width;
  img.fiY2 = height;
  /* force default of display center on new size, else use current center */
  if( (img.filecols != width) || (img.filerows != height) ) {
    img.filecols = width;
    img.filerows = height;
    img.fdcenX = 0.0;
    img.fdcenY = 0.0;
    img.fdblock = 0;
  } else {
    float fdx, fdy;
    /* translate from file to img coords */
    d_transform(&coord.imgtofile, coord.id.cenX, coord.id.cenY, &fdx, &fdy);
    img.fdcenX = (double)fdx;
    img.fdcenY = (double)fdy;
    img.fdblock = coord.id.block;
  }
  /* initialize imaging parameters */
  if( control.IRAF_out.protocol == IOP_PROS )
    img.file_type = SOP_PROS;
  else
    img.file_type = SOP_Imtool;
  /* imtool packets are 8bit (for now, anyway) */
  img.bytepix = 1;
  init_img(&img, &coord);
  init_dispcen(&img, &coord);
  (void)init_imagebuf();
  /* don't flag that buffer must be loaded, it's not that kind of load */
  coord.buferror = 0;
  buffer.load_filebuf = 0;
  /* set up the panbox accordingly */
  new_panbox(1);
}

/*
 * Subroutine:	disp_subpiece
 * Purpose:	Map and display a sub-section of the full buffer.
 */
void disp_subpiece ( x1, y1, x2, y2 )
     int x1, y1, x2, y2;	/* i: coordinates in buffer.shortbuf */
{
  float dx1, dy1, dx2, dy2;
  float iX1, iY1, iX2, iY2;
  Coordsys disp;
  Edges bd;
  int dy, dh;
  int bh, block, py_off;
  char *data;
  short *sb, *pb;
  void d_transform(), set_edges(), map_dispbox(), disp_dispbox();
  void set_dispoff(), new_panimage();

  /* set box to map only the given section */
  bcopy((char *)&coord.disp, (char *)&disp, sizeof(Coordsys));
  bcopy((char *)&coord.bd, (char *)&bd, sizeof(Edges));
  d_transform(&coord.buftoimg, (double)x1, (double)y1, &iX1, &iY1);
  d_transform(&coord.imgtodisp, (double)iX1, (double)iY1, &dx1, &dy1);
  d_transform(&coord.buftoimg,
	      (double)(x2 + 1), (double)(y2 + 1), &iX2, &iY2);
  d_transform(&coord.imgtodisp, (double)iX2, (double)iY2, &dx2, &dy2);
  /* ignore if section is not visible in the display window */
  if( (dx1 <= (float)dispbox.width) && (dx2 > 1.0) &&
      (dy1 <= (float)dispbox.height) && (dy2 > 1.0) ) {
    /* clip edges to the display window */
    if( dx1 < 0.0 )
      dx1 = 0.0;
    if( dx2 > (float)dispbox.width )
      dx2 = (float)dispbox.width;
    if( dy1 < 0.0 )
      dy1 = 0.0;
    if( dy2 > (float)dispbox.height )
      dy2 = (float)dispbox.height;
    coord.disp.X1 = dx1;
    coord.disp.Y1 = dy1;
    coord.disp.X2 = dx2;
    coord.disp.Y2 = dy2;
    coord.disp.Y1i = (int)dy1;
    coord.disp.Y2i = (int)dy2 - 1;
    coord.disp.Yhght = 1 + coord.disp.Y2i - coord.disp.Y1i;
    set_edges(&coord.disptobuf, &coord.buf, &coord.disp, &coord.bd);
    /* redefine the edges in the display window if at edge of buffer */
    if( coord.bd.block < 0 )
      set_dispoff(&coord.disptobuf, &coord.disp, &coord.bd);
    map_dispbox();
    bcopy((char *)&disp, (char *)&coord.disp, sizeof(Coordsys));
    bcopy((char *)&bd, (char *)&coord.bd, sizeof(Edges));
    /* set box to only draw the given section */
    dy = dispbox.yzero;
    dh = dispbox.yheight;
    data = dispbox.image.data;
    dispbox.yzero = dy1;
    dispbox.yheight = (int)(0.5 + dy2 - dy1);
    /* note this works for halftone (bit/pixel) and color (byte/pixel) */
    dispbox.image.data += (dispbox.yzero * dispbox.image.bytes_per_line);
    disp_dispbox();
    dispbox.yzero = dy;
    dispbox.yheight = dh;
    dispbox.image.data = data;
  }
  /* replace the panbox with a new one */
  block = (int)coord.pantoimg.inx_outx;
  bh = coord.pan.Yhght;
  dh = panbox.yheight;
  dy = panbox.yzero;
  data = panbox.image.data;
  pb = buffer.panbuf;
  sb = buffer.shortbuf;
  /* this should always be true, since imtool doesn't do less than 512 */
  if( block >= 1 ) {
    y1 = (y1 / block) * block;
    y2 = (((y2 / block) + 1) * block) - 1;
    if( y2 >= coord.buf.Yhght )
      y2 = coord.buf.Yhght - 1;
    py_off = y1 / block;
    buffer.shortbuf += (y1 * coord.buf.width);
    buffer.panbuf += (py_off * coord.pan.Xwdth);
    coord.pan.Yhght = (1 + y2 - y1) / block;
    panbox.yzero = py_off;
    panbox.yheight = coord.pan.Yhght;
    panbox.image.data += (panbox.yzero * panbox.image.bytes_per_line);
  } else
    py_off = -1;
  new_panimage();
  disp_panbox();
  if( py_off >= 0 ) {
    buffer.shortbuf = sb;
    buffer.panbuf = pb;
    panbox.yzero = dy;
    panbox.yheight = dh;
    panbox.image.data = data;
  }
  coord.pan.Yhght = bh;
}

/*
 * Subroutine:	set_imtool_scale
 * Purpose:	Force the scalemap to match the range of imtool image data
 */
void set_imtool_scale()
{
  void set_imtool_colors(), make_scalemap(), touch_submenu_button();

  /* identify min and max used by imtool */
  buffer.clipmin = 1;
  buffer.clipmax = 200;
  buffer.scale_min = 0;
  buffer.scale_max = 199;
  /* force new scaling, but not hisotgram equalize if it would be invoked */
  if( (color.scale.mode == SOP_HistEq) &&
      (color.ncolors < 200) && (color.ncolors > 1) ) {
    color.scale.mode = SOP_Linear;
    touch_submenu_button(SOP, SOP_Linear);
  }
  if( color.ncolors == 1 ) {
    color.ncolors = 256;
    make_scalemap(0, 199);
    color.ncolors = 1;
  } else {
    make_scalemap(0, 199);
    set_imtool_colors();
  }
}

/*
 * Subroutine:	set_imtool_colors
 * Purpose:	Map colors used as graphics options in imtool
 */
void set_imtool_colors ()
{
  unsigned char *lookup;

  lookup = buffer.scalemap + SCALEOFF;
  /* imtool cursor color */
  lookup[201] = (unsigned char)color.gcset.draw.foreground;
  lookup[202] = (unsigned char)color.hard.true_black;
  lookup[203] = (unsigned char)color.hard.true_white;
  lookup[204] = (unsigned char)color.hard.red;
  lookup[205] = (unsigned char)color.hard.green;
  lookup[206] = (unsigned char)color.hard.blue;
  lookup[207] = (unsigned char)color.hard.yellow;
  lookup[208] = (unsigned char)color.hard.std_black;	/* imtool cyan */
  lookup[209] = (unsigned char)color.hard.std_white;	/* imtool magenta */
  lookup[210] = (unsigned char)color.hard.yellow;	/* imtool coral */
  lookup[211] = (unsigned char)color.hard.red;		/* imtool maroon */
  lookup[212] = (unsigned char)color.hard.yellow;	/* imtool orange */
  lookup[213] = (unsigned char)color.hard.std_white;	/* imtool khaki */
  lookup[214] = (unsigned char)color.hard.red;		/* imtool orchid */
  lookup[215] = (unsigned char)color.hard.green;	/* imtool turquoise */
  lookup[216] = (unsigned char)color.hard.blue;		/* imtool violet */
  lookup[217] = (unsigned char)color.hard.std_white;	/* imtool wheat */
}

#endif
           
                                                               
                                                              

                                                               
                                                             
