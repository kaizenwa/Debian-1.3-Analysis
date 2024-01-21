#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	disppsct.c (Display Postscript)
 * Purpose:	Dump contents of the display window to a postscript printer.
 * Subroutine:	screen_dump()			returns: void
 * Copyright:	1989 NOAO & Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Origin:      Doug Tody, NOAO (written for IRAF's Imtool)
 * Modified:	{0} Michael VanHilst	initial version		1 July 1989
 *		{1} Jay Travisano (STScI)    VMS changes	10 Nov 1989
 *              {2} MVH BSDonly strings.h compatability		19 Feb 1990
 *              {3} Change R_DISPOSE printer from lw to lp	 1 May 1995
 *		{n} <who> -- <does what> -- <when>
 */

#ifndef SUN
#include <sys/file.h>
#endif

#include <stdio.h>		/* stderr, NULL, etc. */

#ifndef VMS
#include <pwd.h>
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <time.h>
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define SZ_FNAME */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/region.h"	/* regdrawRec */
#include "hfiles/scale.h"	/* SCALEOFF */
extern struct windowRec desktop;
extern struct regdrawRec rgdraw;

#ifndef PSCRIPT
screen_dump ()
{
  (void)fprintf(stderr, "Compiled without PostScript support.\n");
  return;
}
#else

#define	R_DISPOSE "lpr -Plp -r %s"	/* printer dispose command	*/
#define P_DISPOSE "lpr -P%s -r %s"	/* dispose to user's printer	*/
#define	SEGSIZE		16		/* output segment size (bytes)	*/
#define	SEGBITS		(SEGSIZE*8)
#define	BITFLIP		0		/* bit-flip each byte for P.S.?	*/
#define	NGREY		256		/* max color table size		*/
#define MARGIN		 36		/* 0.5 inch			*/
#define	PAGE_WIDTH	612		/* 8.5x11, 72pt/inch         	*/
#define	PAGE_HEIGHT	792		/* 8.5x11, 72pt/inch         	*/

#define	RT_DATA		'a'		/* record type codes		*/
#define	RT_ZERO		'b'
#define	RT_FULL		'c'
#define	RT_BKG1		'd'
#define	RT_BKG2		'e'
#define	BKGPAT_1	"22"		/* atom for stipple pattern	*/
#define	BKGPAT_2	"88"		/* atom for stipple pattern	*/

static	void bitmap_to_postscript();
static	char *make_label();
static  void black_border();

/* SCREENDUMP -- Make a hardcopy of the indicated region of the screen on a
 * hardcopy device.  Currently only two output formats are supported, Sun
 * rasterfile output, or Postscript output for devices such as the Apple laser
 * writer.  Postscript output is the default; rasterfile output is enabled
 * if the variable 'R_RASTERFILE' is defined in the environment, with the
 * string value being an sprintf format string with one decimal integer
 * argument, used to create the rasterfile filename (e.g., "frame.%d").
 * The variable 'R_DISPOSE' may be defined to specify how the Postscript or
 * Sun rasterfile is to be disposed of.  If 'R_DISPOSE' is not defined and
 * rasterfile output is specified, no dispose command will be issued.  If the
 * variable is not defined and Postscript output is specified, the default
 * dispose command defined above will be executed.
 *
 */
void screen_dump ( nobuttons )
     int nobuttons;	/* i: omit-the-buttons from the output image */
{
  register int i;
  int width, height, area;
  unsigned char *obuf;
  unsigned char *zero;

  static int index = 0;		/* count calls to produce unique file names */
  char tempfile[80], dispose[80];
  int depth=8;			/* only bytes top PS dither is supported */
  double wscale, hscale;
  double scale;
  int color_levels;		/* number of color levels in hardare */
  int status;
  int fd;
  char *str;
  FILE *fp;
  unsigned long draw_mask, draw_fore, draw_back;
  unsigned long menu_fore, menu_back;
  unsigned long incl_fore, incl_back;
  unsigned long excl_fore, excl_back;
  int filled;
  unsigned long fore, back;
  XImage *ximage;

#ifndef SUN
  char *mktemp();
#endif

  char *calloc_errchk(), *getenv();
  void close_disk(), clear_margins(), map_buf_repzoom();
  void map_buf_subzoom(), disp_dispbox(), disp_panbox();

  /* if you want to grab the server do it here */
  /* suspend all screen interactions while processing */
  /* get dimensions */
  if( color.ncolors <= 1 ) {
    /* strategy: clear image, draw cursors, copy cursors, */
    width = dispbox.width;
    height = dispbox.height;
    area = width * height;
    /* allocate a data buffer and the line-all-zeroes array */
    if( ((obuf = (unsigned char *)
	  calloc_errchk(area, sizeof(char), (char *)NULL)) == 0) ||
        ((zero = (unsigned char *)
	  calloc_errchk(height, sizeof(char), (char *)NULL)) == 0) ) {
      (void)fprintf(stderr, "Print failure: cannot allocate data buffers\n");
      return;
    }
    if( coord.bd.clip ) {
      /* if image does not fill display buffer, do something about it */
      clear_margins((unsigned char *)dispbox.image.data, &coord.bd,
		    &coord.disp, 1, color.hard.std_white);
    }
    if( coord.bd.block < -1 )
      map_buf_repzoom(&coord, (unsigned char *)obuf,
		      buffer.shortbuf, buffer.scalemap + SCALEOFF);
    else
      map_buf_subzoom(&coord, (unsigned char *)obuf,
		      buffer.shortbuf, buffer.scalemap + SCALEOFF,
		      MAX(coord.bd.block, 1));
    if( color.halftone.inverse == 0 ) {
      /* non-inverted image has high values dark */
      register unsigned char *ptr = obuf;
      register unsigned char *obuf_end = obuf + area;
      do {
	*ptr = 255 - *ptr;
      } while( ++ptr < obuf_end );
    }
    /* make the edge a black border */
    black_border(obuf, width, height);
  } else {
    unsigned short *map;

    width = desktop.width;
    height = desktop.height;
    color_levels = DisplayCells(color.display, color.screen);
    if( nobuttons )
      height -= btnbox.height;
    /* get byte image and convert to equivalent intensity */
    /* allocate a val-to-intensity map and a this-line-all-zeroes array */
    if( ((map = (unsigned short *)
	 calloc_errchk(NGREY, sizeof(short), "print map")) == 0) ||
        ((zero = (unsigned char *)
	  calloc_errchk(height, sizeof(char), (char *)NULL)) == 0) ) {
      (void)fprintf(stderr, "Print failure: cannot allocate data buffers\n");
      return;
    } 
    
    {
      double rwght, gwght, bwght;
      XColor cdef[256];
      int val;

      /* Black&White = (.35*Red +.55*Green + .10*Blue) */
      /* scaling from 65536 to 256 requires /256 */
      rwght = 0.35 / 256.0;
      gwght = 0.55 / 256.0;
      bwght = 0.10 / 256.0;
      for( i=0; i<color_levels; i++ )
	cdef[i].pixel = i;
      XQueryColors(color.display, color.colormap, cdef, color_levels);
      for( i=0; i<color_levels; i++ ) {
	val = (int)((rwght * (double)cdef[i].red) +
		    (gwght * (double)cdef[i].green) +
		    (bwght * (double)cdef[i].blue));
	if( val >= 256 ) {
	  map[i] = 255;
	} else if( val < 0 ) {
	  map[i] = 0;
	} else {
	  map[i] = val;
	}
      }
    }
    /* set cursor colors (for max contrast with background) */
    map[color.hard.std_black] = 0;
    map[color.hard.std_white] = 255;
    if( map[color.pixvalmap[0]] < 128 ) {
      fore = color.hard.std_white;
      back = color.hard.std_black;
    } else {
      fore = color.hard.std_black;
      back = color.hard.std_white;
    }
    draw_mask = color.gcset.draw.mask;
    draw_fore = color.gcset.draw.foreground;
    draw_back = color.gcset.draw.background;
    menu_fore = color.gcset.menu.foreground;
    menu_back = color.gcset.menu.background;
    incl_fore = color.gcset.incl.foreground;
    incl_back = color.gcset.incl.background;
    excl_fore = color.gcset.excl.foreground;
    excl_back = color.gcset.excl.background;
    filled = rgdraw.filled_label;
    color.gcset.draw.mask = AllPlanes;
    color.gcset.draw.foreground = fore;
    color.gcset.draw.background = back;
    color.gcset.menu.foreground = fore;
    color.gcset.menu.background = back;
    color.gcset.incl.foreground = fore;
    color.gcset.incl.background = back;
    color.gcset.excl.foreground = fore;
    color.gcset.excl.background = back;
    rgdraw.filled_label = 1;
    /* redraw the display with this coloring */
    XRaiseWindow(desktop.display, desktop.ID);
    disp_dispbox();
    disp_panbox();
    XSync(desktop.display, 0);
    /* read data direct from window (image starts at x/y + borderwidth) */
    ximage = XGetImage(desktop.display, desktop.ID, 0, 0,
		       desktop.width, desktop.height, AllPlanes, ZPixmap);
    color.gcset.draw.mask = draw_mask;
    color.gcset.draw.foreground = draw_fore;
    color.gcset.draw.background = draw_back;
    color.gcset.menu.foreground = menu_fore;
    color.gcset.menu.background = menu_back;
    color.gcset.incl.foreground = incl_fore;
    color.gcset.incl.background = incl_back;
    color.gcset.excl.foreground = excl_fore;
    color.gcset.excl.background = excl_back;
    rgdraw.filled_label = filled;
    disp_dispbox();
    disp_panbox();
    XSync(desktop.display, 0);
    if( ximage == NULL ) {
      (void)fprintf(stderr, "ERROR: could not read image\n");
      return;
    }
    /* remap image values to monochrome intensities */
    obuf = (unsigned char *)ximage->data;
    {
      register unsigned char *buf, *bufend, *ibuf;
      buf = obuf;
      if( nobuttons )
	bufend = buf + (width * btnbox.y);
      else
	bufend = obuf + (width * desktop.height);
      do {
	*buf = map[*buf];
      } while( ++buf < bufend );
      if( nobuttons ) {
	ibuf = obuf + (width * (dispbox.y - 1));
	bufend = obuf + (width * desktop.height);
	do {
	  *buf++ = map[*ibuf];
	} while( ++ibuf < bufend );
      }
    }
  }
  /* make the edge a black border */
    /*  black_border(obuf, width, height); */
  /* Create a temporary file to hold postscript program. */
  /* add unique char (mkstemp doesn't avoid duplicating temp file names) */
#ifndef VMS
  sprintf(tempfile, "/tmp/ps%cXXXXXX", 'a' + index);
#else
  sprintf(tempfile, "_ps%cXXXXXX.tmp", 'a' + index);
#endif
  if( ++index > 26 ) index = 0;
  /* open a file with a system chosen unique name */
#ifdef SUN
  if( (fd = mkstemp(tempfile)) == -1 ) {
    perror("Cannot make temp file");
#else
  (void)mktemp(tempfile);
  /* open the file with a call to open with both write and create flags */
  if( (fp = fopen(tempfile, "w")) == NULL ) {
#endif
    (void)fprintf(stderr, "cannot create temporary file %s\n", tempfile);
    (void)fflush(stderr);
  } else {
#ifdef SUN
    fp = fdopen(fd, "a");
#endif
    /* Autoscale to fit output page. */
    wscale = ((double)PAGE_WIDTH - 2*MARGIN) / (double)width;
    hscale = ((double)PAGE_HEIGHT - 2*MARGIN) / (double)height;
    if( wscale < hscale )
      scale = wscale;
    else
      scale = hscale;

    /* Translate the bitmap into a postscript program. */
    bitmap_to_postscript(fp, obuf, width, height, depth, width, zero, scale);

    free((char *)zero);
    (void)fclose(fp);
#ifdef SUN
    close_disk(fd, tempfile);
#endif

    /* Dispose of tempfile to the printer.  We leave it up to the dispose
     * command to delete the temporary file when finished.  The dispose
     * command may be passed as an environment variable if desired, e.g.,
     * to specify a printer device other than "lw" (laserwriter).
     */
    if( (str = getenv("R_DISPOSE")) != NULL ) {
      sprintf(dispose, str, tempfile);
    } else if( (str = getenv("PRINTER")) != NULL ) {
      sprintf(dispose, P_DISPOSE, str, tempfile);
    } else
      sprintf(dispose, R_DISPOSE, tempfile);
#ifndef VMS
    if( (status = system(dispose)) != 0 )
#else
    if( !((status = system(dispose)) & 1) )
#endif
      (void)fprintf(stderr, "screendump: exit status %d\n", status);
  }
  if( color.ncolors <= 1 )
    free((char *)obuf);
  else
    (void)XDestroyImage(ximage);
}

/* BITMAP_TO_POSTSCRIPT -- Translate a memory bitmap into a postscript program
 * using image compression where regions of the image are all zeroes.  This is
 * done as follows: [1] lines of the bitmap are divided into segments of N
 * bytes, [2] if all N bytes are zero a single zero byte is transmitted,
 * otherwise a byte with the value one is transmitted, followed by N bytes of 
 * literal data.  Lines which are entirely zero are not transmitted at all.
 * The goal is to significantly reduce the amount of data to be pushed through
 * the laserwriter serial interface while keeping things simple enough that
 * postscript will hopefully be able to process the bitmap efficiently.
 *
 * NOTE: Postscript is supposed to be able to copy bitmaps directly without
 * any transformations if all the right conditions are met, e.g., unitary
 * matrices, pixrect resolution matches device resolution, etc.  We do not
 * make use of this here due to the great volume of data which would have to
 * pushed through the laserwriter serial interface at 9600 baud to transmit
 * a fully resolved bitmap.  If a parallel interface were available, e.g.,
 * if the laserwriter is on the ethernet, then this would be the way to go.
 */
static void
bitmap_to_postscript (fp, bitmap, width, height, depth, linebytes, zero, scale)
     register FILE *fp;
     unsigned char *bitmap;
     int width, height, depth;
     int linebytes;
     unsigned char *zero;
     double scale;
{
  register unsigned char *ip;
  register char	*op, *hp;
  register int	n;
  unsigned char	*segp;
  char	hbuf[NGREY*2];
  char	obuf[SEGSIZE*2];
  char	rt_full[SEGSIZE*2+1];
  char	bkg_1[SEGSIZE*2+1];
  char	bkg_2[SEGSIZE*2+1];
  int	partseg, seg, nsegs, allzeroes, i, j, last_j;
  int	llx, lly, urx, ury;
  long	clock;

  clock = time (0);

  /* Initialize the hbuf array, which contains the hex encoded
   * representations of the NGREY possible binary byte values.
   */
  for (n=0, op=hbuf;  n < NGREY;  n++) {
    i = ((n >> 4) & 017);
    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
    i = (n & 017);
    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
  }
  
  /* Set up the background (stipple) pattern arrays, used to represent
   * the Sunview background pattern outside of windows.
   */
  for (op=bkg_1, hp=BKGPAT_1, n=SEGSIZE;  --n >= 0;  ) {
    *op++ = hp[0];
    *op++ = hp[1];
  }   *op++ = '\0';
  for (op=bkg_2, hp=BKGPAT_2, n=SEGSIZE;  --n >= 0;  ) {
    *op++ = hp[0];
    *op++ = hp[1];
  }   *op++ = '\0';
  
  /* RT_FULL is a solid line, another common pattern. */
  for (op=rt_full, n=SEGSIZE*2;  --n >= 0;  )
    *op++ = 'F';
  *op++ = '\0';
  
  /* Initialize obuf, in case a partseg call causes the full buffer to
   * be written out before the garbage elements at the end have been
   * initialized to legal values.
   */
  bcopy ((char *)rt_full, (char *)obuf, SEGSIZE*2);

  /* Calculate bounding box. */
  llx = MARGIN;
  ury = PAGE_HEIGHT - MARGIN;
  urx = llx + (width * scale);
  lly = ury - (height * scale);

  /* Define the postscript necessary to receive and output the lines
   * of the pixrect with image compression.
   */
  (void)fprintf(fp, "%%!PS-Adobe-2.0 EPSF-2.0\n");
  (void)fprintf(fp, "%%%%Title: SAOimage screendump\n");
  (void)fprintf(fp, "%%%%Creator: SAOimage\n");
  (void)fprintf(fp, "%%%%CreationDate: %s", asctime(localtime(&clock)));
  (void)fprintf(fp, "%%%%PageOrientation: Portrait\n");
  (void)fprintf(fp, "%%%%Pages: 1\n");
  (void)fprintf(fp, "%%%%BoundingBox: %d %d %d %d\n", llx, MARGIN/2, urx, ury);
  (void)fprintf(fp, "%%%%DocumentFonts: Times-Roman\n");
  (void)fprintf(fp, "%%%%DocumentNeededFonts: Times-Roman\n");
  (void)fprintf(fp, "%%%%EndComments\n");
  (void)fprintf(fp, "/r_data %d string def\n", SEGSIZE);
  (void)fprintf(fp, "/r_zero %d string def\n", SEGSIZE);
  (void)fprintf(fp, "/r_full %d string def\n", SEGSIZE);
  (void)fprintf(fp, "/r_bkg1 %d string def\n", SEGSIZE);
  (void)fprintf(fp, "/r_bkg2 %d string def\n", SEGSIZE);
  (void)fprintf(fp, "currentfile r_full readhexstring %s\n", rt_full);
  (void)fprintf(fp, "currentfile r_bkg1 readhexstring %s\n", bkg_1);
  (void)fprintf(fp, "currentfile r_bkg2 readhexstring %s\n", bkg_2);
  (void)fprintf(fp, "clear\n");

  (void)fprintf(fp, "/dline {\n");

  if (depth == 8) {
    (void)fprintf(fp, " %d div neg 0 exch translate\n", height);
    (void)fprintf(fp, " %d %d 8\n", width, 1);
    (void)fprintf(fp, " [ %d 0 0 %d 0 %d ]\n", width, -height, height);
  } else {
    (void)fprintf(fp," 0 exch translate %d %d true matrix\n", width, 1);
  }

  (void)fprintf(fp, " { currentfile read pop dup %d eq\n", RT_DATA);
  (void)fprintf(fp, "     { pop currentfile r_data readhexstring pop }\n");
  (void)fprintf(fp, "     { dup %d eq\n", RT_ZERO);
  (void)fprintf(fp, "       { pop r_zero }\n");
  (void)fprintf(fp, "       { dup %d eq\n", RT_FULL);
  (void)fprintf(fp, "         { pop r_full }\n");
  (void)fprintf(fp, "         { %d eq\n", RT_BKG1);
  (void)fprintf(fp, "           { r_bkg1 }\n");
  (void)fprintf(fp, "           { r_bkg2 }\n");
  (void)fprintf(fp, "           ifelse }\n");
  (void)fprintf(fp, "         ifelse }\n");
  (void)fprintf(fp, "       ifelse }\n");
  (void)fprintf(fp, "     ifelse\n");

  if (depth == 8)
    (void)fprintf(fp, " } image} def\n");
  else
    (void)fprintf(fp, " } imagemask} def\n");

  (void)fprintf(fp, "%%%%EndProlog\n");
  (void)fprintf(fp, "%%%%Page: 1 1\n");

  (void)fprintf(fp, "gsave\n");
  (void)fprintf(fp, "%d %d translate\n", llx, lly);
  (void)fprintf(fp, "%d %d scale\n", urx-llx, ury-lly);

  nsegs = width / (SEGBITS / depth);
  partseg = linebytes - (nsegs * SEGSIZE);

  /* Output successive lines of the pixrect.  All zero lines are omitted
   * and data compression is used for large regions of zeroes embedded
   * within a line.
   */
  for (j=0, last_j=0;  j < height;  j++) {
    if (zero[j])
      continue;

    (void)fprintf(fp, "\n%d dline\n", j - last_j);
    last_j = j;

    /* Output an integral number of line segments in hexstring format,
     * i.e., two hex digits output per binary input byte.
     */
    segp = bitmap + j*linebytes;
    for (seg=0;  seg < nsegs;  seg++, segp += SEGSIZE) {
      /* Quick scan of the data to see if it is all zeroes. */
      allzeroes = 1;
      for (ip=segp, n=SEGSIZE;  --n >= 0;  )
	if (*ip++) {
	  allzeroes = 0;
	  break;
	}
      
      if (allzeroes) {
	(void)putc(RT_ZERO, fp);
      } else {
	/* Encode the data segment in hex format. */
	for( ip=segp, op=obuf, n=SEGSIZE;  --n >= 0;  ) {
	  hp = hbuf + (*ip++ * 2);
	  *op++ = *hp++;
	  *op++ = *hp++;
	}
	
	if( (obuf[0] == rt_full[0]) &&
	    (strncmp(obuf, rt_full, SEGSIZE*2) == 0) ) {
	  (void)putc(RT_FULL, fp);
	} else if( (obuf[0] == bkg_1[0]) &&
		   (strncmp(obuf, bkg_1, SEGSIZE*2) == 0) ) {
	  (void)putc (RT_BKG1, fp);
	} else if( (obuf[0] == bkg_2[0]) &&
		   (strncmp(obuf, bkg_2, SEGSIZE*2) == 0) ) {
	  (void)putc(RT_BKG2, fp);
	} else {
	  (void)putc(RT_DATA, fp);
	  (void)fwrite(obuf, SEGSIZE*2, 1, fp);
	}
      }
    }

    /* Write out any partial segment at the end of the line.  We must
     * always write a full segment, even if the data at the end is
     * garbage, else synchronization will be lost.
     */
    if (partseg) {
      for( op=obuf, n=partseg;  --n >= 0;  ) {
	hp = hbuf + (*ip++ * 2);
	*op++ = *hp++;
	*op++ = *hp++;
      }
      (void)putc(RT_DATA, fp);
      (void)fwrite(obuf, SEGSIZE*2, 1, fp);
    }
  }

  /* Add the [SAO] logo and timestamp at the bottom of the page and
   * output the page.
   */
  (void)fprintf(fp, "\ngrestore\n");
  (void)fprintf(fp, "/Times-Roman findfont 6 scalefont setfont\n");
  if( img.filename != NULL ) {
    (void)fprintf(fp, "%d %d moveto\n", 2*MARGIN, MARGIN/2);
    (void)fprintf(fp, "(%s) show\n", img.filename);
  }
  (void)fprintf(fp, "%d %d moveto\n", width - 144, MARGIN/2);
  (void)fprintf(fp, "(%s) show\n", make_label());
  (void)fprintf(fp, "showpage\n");
  (void)fprintf(fp, "%%%%EOF\n");
}

/*
 * Subroutine:	make_label
 * Purpose:	Generate the label for the output printer page.
 * Unix call:	time(), gethostname(), getpwuid(), endpwent()
 */
static char *make_label()
{
  static char buf[128];
  char hostname[32];
  char username[32];
#ifndef VMS
  struct passwd *pw;
#else
  char *getenv();
#endif

#ifdef SUN
  long time();		/* should be in time.h, but not in SunOS */
#endif
  long clock;
  int gethostname();

  clock = time((long *)0);
  (void)gethostname(hostname, 32);
#ifndef VMS
  pw = getpwuid(getuid());
  (void)strcpy(username, pw->pw_name);
  (void)endpwent();
#else
  (void)strcpy(username,getenv("USER"));
#endif

  sprintf(buf, "SAOimage  %s@%s  %s",
		username, hostname, asctime(localtime(&clock)));
  return(buf);
}

static void black_border ( obuf, width, height )
     unsigned char *obuf;
     int width, height;
{
  register unsigned char *buf, *bufend;
  buf = obuf;
  bufend = buf + (width * height);
  while( (buf+=width) < bufend ) {
    *buf = 0;
    *(buf-1) = 0;
  }
  buf = bufend - width;
  while( ++buf < bufend )
    *buf = 0;
  buf = obuf;
  bufend = buf + width;
  while( buf < bufend )
    *buf++ = 0;
}

#endif
