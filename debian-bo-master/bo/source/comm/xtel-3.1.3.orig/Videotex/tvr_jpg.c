/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1996  Lectra Systemes & Pierre Ficheux
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
static char rcsid[] = "$Id: tvr_jpg.c,v 1.3 1997/02/21 16:57:03 pierre Exp $";

/*
 * Interface JPEG du mode Teletel vitesse rapide
 */

#ifndef NO_TVR

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>	
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>

#include "VideotexP.h"

#define TVRMESSAGE(code,string)	string,

typedef enum { JMSG_FIRSTADDONCODE=1000, JERR_TVR_X11_COLORSPACE, JERR_TVR_PHOTO_HDR, JERR_TVR_X11_ALLOC_COLOR, JERR_TVR_X11_ALLOC_IMAGE, JMSG_LASTADDONCODE
} ADDON_MESSAGE_CODE;

static const char * const tvr_message_table[] = {
TVRMESSAGE(JMSG_FIRSTADDONCODE, NULL)
TVRMESSAGE(JERR_TVR_X11_COLORSPACE, 	"X11 output must be RGB")
TVRMESSAGE(JERR_TVR_PHOTO_HDR, 		"Not a TVR Photo header")
TVRMESSAGE(JERR_TVR_X11_ALLOC_COLOR,	"Can't allocate color %d/%d/%d\n")
TVRMESSAGE(JERR_TVR_X11_ALLOC_IMAGE,	"Can't allocate XImage data\n")
  NULL
};

#include "jpeglib.h"
#include "jerror.h"		/* get library error codes too */

static int nb_bits_rouge;
static int nb_bits_vert;
static int nb_bits_bleu;
static int decalage_rouge;
static int decalage_vert;
static int decalage_bleu;
static unsigned long red_mask;
static unsigned long green_mask;
static unsigned long blue_mask;
static int last_color;
static unsigned long pixels[256];
static Boolean last_was_full_screen;

typedef struct {
    unsigned short red, green, blue;
} RGBColor;

/*
 * For 12-bit JPEG data, we either downscale the values to 8 bits
 * (to write standard byte-per-sample PPM/PGM files), or output
 * nonstandard word-per-sample PPM/PGM files.  Downscaling is done
 * if PPM_NORAWWORD is defined (this can be done in the Makefile
 * or in jconfig.h).
 * (When the core library supports data precision reduction, a cleaner
 * implementation will be to ask for that instead.)
 */

#if BITS_IN_JSAMPLE == 8
#define BYTESPERSAMPLE 1
#else
#ifdef PPM_NORAWWORD
#define BYTESPERSAMPLE 1
#else
#define BYTESPERSAMPLE 2
#endif
#endif

/*
 * Object interface for djpeg's output file encoding modules
 */

typedef struct djpeg_dest_struct * djpeg_dest_ptr;

struct djpeg_dest_struct {
  /* start_output is called after jpeg_start_decompress finishes.
   * The color map will be ready at this time, if one is needed.
   */
  JMETHOD(void, start_output, (j_decompress_ptr cinfo,
			       djpeg_dest_ptr dinfo));
  /* Emit the specified number of pixel rows from the buffer. */
  JMETHOD(void, put_pixel_rows, (j_decompress_ptr cinfo,
				 djpeg_dest_ptr dinfo,
				 JDIMENSION rows_supplied));
  /* Finish up at the end of the image. */
  JMETHOD(void, finish_output, (j_decompress_ptr cinfo,
				djpeg_dest_ptr dinfo));

  /* Output pixel-row buffer.  Created by module init or start_output.
   * Width is cinfo->output_width * cinfo->output_components;
   * height is buffer_height.
   */
  JSAMPARRAY buffer;
  JDIMENSION buffer_height;
};


/* Private version of data destination object */

typedef struct {
  struct djpeg_dest_struct pub;	/* public fields */

  /* Usually these two pointers point to the same place: */
  char *iobuffer;		/* fwrite's I/O buffer */
  JSAMPROW pixrow;		/* decompressor output buffer */
  size_t buffer_width;		/* width of I/O buffer */
  JDIMENSION samples_per_row;	/* JSAMPLEs per output row */

  /* X11 fields */
  Display *display;
  int screen, nplanes;
  GC gc;
  Window win, win_sauve;
  Colormap cmap;
  XImage *ximage;
  XImage *ximage_double;
  char *ximage_data, *pimage_data, *ximage_data_double;
  short *pimage_data_short;
  unsigned int *pimage_data_int;
  RGBColor colors[256];
  Boolean flag_double, flag_bs;
  int modulo;
  int x_photo, y_photo;
} x11_dest_struct;

typedef x11_dest_struct * x11_dest_ptr;

/* Expanded data source object for stdio input */

typedef struct {
  struct jpeg_source_mgr pub;	/* public fields */
  int infile;
  JOCTET * buffer;		/* start of buffer */
  boolean start_of_file;	/* have we gotten any data yet? */
  int data_length;
} my_source_mgr;

typedef my_source_mgr * my_src_ptr;

#define INPUT_BUF_SIZE  1	/* choose an efficiently fread'able size */

/*
 * Initialize source --- called by jpeg_read_header
 * before any data is actually read.
 */

METHODDEF void
init_source (j_decompress_ptr cinfo)
{
  my_src_ptr src = (my_src_ptr) cinfo->src;

  /* We reset the empty-input-file flag for each image,
   * but we don't clear the input buffer.
   * This is correct behavior for reading a series of images from one source.
   */
  src->start_of_file = TRUE;
}


/*
 * Fill the input buffer --- called whenever buffer is emptied.
 */
METHODDEF boolean
fill_input_buffer (j_decompress_ptr cinfo)
{
  my_src_ptr src = (my_src_ptr) cinfo->src;
  size_t nbytes;

  if ((src->data_length)-- == 0) {
      char c;

      if (match (src->infile, "\033p#@\177")) {
	  src->data_length = get_header_length (src->infile);
	  read (src->infile, &c, 1);	/* Type header */
      }
      else
	  ERREXIT(cinfo, JERR_TVR_PHOTO_HDR);
  }

  nbytes = read(src->infile, src->buffer, INPUT_BUF_SIZE);

  if (nbytes <= 0) {
    if (src->start_of_file)	/* Treat empty input file as fatal error */
      ERREXIT(cinfo, JERR_INPUT_EMPTY);
    WARNMS(cinfo, JWRN_JPEG_EOF);
    /* Insert a fake EOI marker */
    src->buffer[0] = (JOCTET) 0xFF;
    src->buffer[1] = (JOCTET) JPEG_EOI;
    nbytes = 2;
  }

  src->pub.next_input_byte = src->buffer;
  src->pub.bytes_in_buffer = nbytes;
  src->start_of_file = FALSE;

  return TRUE;
}


/*
 * Skip data --- used to skip over a potentially large amount of
 * uninteresting data (such as an APPn marker).
 */

METHODDEF void
skip_input_data (j_decompress_ptr cinfo, long num_bytes)
{
  my_src_ptr src = (my_src_ptr) cinfo->src;

  /* Just a dumb implementation for now.  Could use fseek() except
   * it doesn't work on pipes.  Not clear that being smart is worth
   * any trouble anyway --- large skips are infrequent.
   */
  if (num_bytes > 0) {
    while (num_bytes > (long) src->pub.bytes_in_buffer) {
      num_bytes -= (long) src->pub.bytes_in_buffer;
      (void) fill_input_buffer(cinfo);
      /* note we assume that fill_input_buffer will never return FALSE,
       * so suspension need not be handled.
       */
    }
    src->pub.next_input_byte += (size_t) num_bytes;
    src->pub.bytes_in_buffer -= (size_t) num_bytes;
  }
}


/*
 * An additional method that can be provided by data source modules is the
 * resync_to_restart method for error recovery in the presence of RST markers.
 * For the moment, this source module just uses the default resync method
 * provided by the JPEG library.  That method assumes that no backtracking
 * is possible.
 */

/*
 * Terminate source --- called by jpeg_finish_decompress
 * after all data has been read.  Often a no-op.
 *
 * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
 * application must deal with any cleanup that should happen even
 * for error exit.
 */

METHODDEF void
term_source (j_decompress_ptr cinfo)
{
  /* no work necessary here */
}


/*
 * Prepare for input from a stdio stream.
 * The caller must have already opened the stream, and is responsible
 * for closing it after finishing decompression.
 */

LOCAL void
jpeg_fd_src (j_decompress_ptr cinfo, int infile, int l)
{
  my_src_ptr src;

  /* The source object and input buffer are made permanent so that a series
   * of JPEG images can be read from the same file by calling jpeg_stdio_src
   * only before the first one.  (If we discarded the buffer at the end of
   * one image, we'd likely lose the start of the next one.)
   * This makes it unsafe to use this manager and a different source
   * manager serially with the same JPEG object.  Caveat programmer.
   */
  if (cinfo->src == NULL) {	/* first time for this JPEG object? */
    cinfo->src = (struct jpeg_source_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  sizeof(my_source_mgr));
    src = (my_src_ptr) cinfo->src;
    src->buffer = (JOCTET *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  INPUT_BUF_SIZE * sizeof(JOCTET));
  }

  src = (my_src_ptr) cinfo->src;
  src->pub.init_source = init_source;
  src->pub.fill_input_buffer = fill_input_buffer;
  src->pub.skip_input_data = skip_input_data;
  src->pub.resync_to_restart = jpeg_resync_to_restart; /* use default method */
  src->pub.term_source = term_source;
  src->infile = infile;
  src->pub.bytes_in_buffer = 0; /* forces fill_input_buffer on first read */
  src->pub.next_input_byte = NULL; /* until buffer loaded */
  src->data_length = l;
}

/*
 * Write some pixel data
 */
METHODDEF void
put_pixel_rows (j_decompress_ptr cinfo, djpeg_dest_ptr dinfo,
		  JDIMENSION rows_supplied)
{
  x11_dest_ptr dest = (x11_dest_ptr) dinfo;
  register char * bufferptr;
  register int pixval;
  register JSAMPROW ptr;
  register JDIMENSION col;
  XColor color;
  register int i, mod;
  register unsigned short red, green, blue;

  ptr = dest->pub.buffer[0];
  bufferptr = dest->iobuffer;

  for (col = cinfo->output_width; col > 0; col--) {
    /* 256 colors */	
    if (dest->nplanes == 8) {
      register JSAMPROW color_map0 = cinfo->colormap[0];
      register JSAMPROW color_map1 = cinfo->colormap[1];
      register JSAMPROW color_map2 = cinfo->colormap[2];

      pixval = GETJSAMPLE(*ptr++);
      color.red = (unsigned short)GETJSAMPLE(color_map0[pixval]) << 8;
      color.green = (unsigned short)GETJSAMPLE(color_map1[pixval]) << 8;
      color.blue = (unsigned short)GETJSAMPLE(color_map2[pixval]) << 8;

      for (i = 0 ; i != last_color ; i++) {
	if (color.red == dest->colors[i].red && color.green == dest->colors[i].green && color.blue == dest->colors[i].blue)
	  break;
      }

      if (i == last_color) {
	dest->colors[last_color].red = color.red;
	dest->colors[last_color].green = color.green;
	dest->colors[last_color].blue = color.blue;

	if (cinfo->err->trace_level)
	  fprintf (stderr, "put_demapped_rgb: allocation %d %d %d ", color.red, color.green, color.blue);

	if (!XAllocColor (dest->display, dest->cmap, &color)) {
	  ERREXIT3(cinfo, JERR_TVR_X11_ALLOC_COLOR, color.red >> 8, color.green >> 8, color.blue >> 8);
	}

	if (cinfo->err->trace_level)
	  printf ("==> %d\n", color.pixel);

	*(dest->pimage_data++) = pixels[last_color] = color.pixel;
	(last_color)++;
      }
      else {
	*(dest->pimage_data++) = pixels[i];
      }
    }
    /* > 256 colors */
    else {
      red = (unsigned short)GETJOCTET(*ptr++);
      green = (unsigned short)GETJOCTET(*ptr++);
      blue = (unsigned short)GETJOCTET(*ptr++);

      /* 16 M colors */
      if (dest->nplanes == 24) {
	*(dest->pimage_data_int++) = ((((unsigned int)red) << decalage_rouge) & red_mask) | ((((unsigned int)green) << decalage_vert) & green_mask) | (((unsigned int)blue) & blue_mask);
      }
      /* 32K or 65K colors */
      else if (dest->nplanes == 16 || dest->nplanes == 15) {
	red = red >> (8 - nb_bits_rouge);
	green = green >> (8 - nb_bits_vert);
	blue = blue >> (8 - nb_bits_bleu);

	*(dest->pimage_data_short++) = ((((unsigned short)red) << decalage_rouge) & red_mask) | ((((unsigned short)green) << decalage_vert) & green_mask) | (((unsigned short)blue) & blue_mask);
      }
    }
  }

  if (cinfo->err->trace_level)
       printf ("put_demapped_rgb: output_scanline = %d\n", cinfo->output_scanline);

  mod = dest->modulo;

  if ((cinfo->output_scanline % mod == 0) && cinfo->output_scanline) {
      if (cinfo->err->trace_level)  
	  printf ("put_demapped_rgb: %d\n", cinfo->output_scanline);
	  
      if (dest->flag_double) {
	  register unsigned int x, y;
	  
	  /* Duplique l'image en doublant la taille */
	  for (y = 0 ; y < (mod << 1) ; y+=2) {
	      for (x = 0 ; x < dest->ximage_double->width ; x+=2) {
		  register unsigned long pixel = XGetPixel (dest->ximage, x/2, y/2);
		  XPutPixel (dest->ximage_double, x, y, pixel);
		  XPutPixel (dest->ximage_double, x, y+1, pixel);
		  XPutPixel (dest->ximage_double, x+1, y, pixel);
		  XPutPixel (dest->ximage_double, x+1, y+1, pixel);
	      }
	  }
	  
	  XPutImage (dest->display, dest->win, dest->gc, dest->ximage_double, 0, 0, dest->x_photo, dest->y_photo + ((cinfo->output_scanline - mod) << 1), cinfo->output_width << 1, mod << 1);
	  if (!dest->flag_bs)
	      XCopyArea (dest->display, dest->win, dest->win_sauve, dest->gc, dest->x_photo, dest->y_photo + ((cinfo->output_scanline - mod) << 1), cinfo->output_width << 1, mod << 1, dest->x_photo, dest->y_photo + ((cinfo->output_scanline - mod) << 1));

      }
      else {
	  XPutImage (dest->display, dest->win, dest->gc, dest->ximage, 0, 0, dest->x_photo, dest->y_photo + (cinfo->output_scanline - mod), cinfo->output_width, mod);
	  if (!dest->flag_bs)
	      XCopyArea (dest->display, dest->win, dest->win_sauve, dest->gc, dest->x_photo, dest->y_photo + (cinfo->output_scanline - mod), cinfo->output_width, mod, dest->x_photo, dest->y_photo + (cinfo->output_scanline - mod));
      }

      dest->pimage_data = dest->ximage_data;
      dest->pimage_data_short = (short*) dest->ximage_data;
      dest->pimage_data_int = (unsigned int*) dest->ximage_data;

      /* Recalcul du modulo */
      if (cinfo->output_height - cinfo->output_scanline < mod)
	  dest->modulo = cinfo->output_height - cinfo->output_scanline;
  }
}

/*
 * Startup: write the file header.
 */

METHODDEF void
start_output_x11 (j_decompress_ptr cinfo, djpeg_dest_ptr dinfo)
{
  x11_dest_ptr dest = (x11_dest_ptr) dinfo;

  /* Emit file header */
  switch (cinfo->out_color_space) {
  case JCS_RGB:

      /* Creation de l'image */
      if (!(dest->ximage_data = (char*) calloc (1, sizeof(unsigned int) * cinfo->output_width * 8))) {
	  ERREXIT(cinfo, JERR_TVR_X11_ALLOC_IMAGE);
      }
      
      if (dest->flag_double) {
	  if (!(dest->ximage_data_double = (char*) calloc (1, sizeof(unsigned int) * cinfo->output_width * 2 * 16))) {
	      perror ("start_output_x11: calloc ximage_data");
	      exit (1);
	  }
      }

      /* Pointeurs char, short, int */
      dest->pimage_data = dest->ximage_data;
      dest->pimage_data_short = (short*) dest->ximage_data;
      dest->pimage_data_int = (unsigned int*) dest->ximage_data;

      dest->ximage = XCreateImage(dest->display, DefaultVisual (dest->display, dest->screen), dest->nplanes, ZPixmap, 0, dest->ximage_data, cinfo->output_width, 8, 8 * sizeof(unsigned int), 0) ;
#ifdef __i386__
      dest->ximage->byte_order = LSBFirst;
#else
      dest->ximage->byte_order = MSBFirst;
#endif

      if (dest->flag_double) {
	  dest->ximage_double = XCreateImage(dest->display, DefaultVisual (dest->display, dest->screen), dest->nplanes, ZPixmap, 0, dest->ximage_data_double, cinfo->output_width << 1, 16, 8 * sizeof(unsigned int), 0) ;
#ifdef __i386__
	  dest->ximage_double->byte_order = LSBFirst;
#else
	  dest->ximage_double->byte_order = MSBFirst;
#endif
      }


    break;
  default:
    ERREXIT(cinfo, JERR_TVR_X11_COLORSPACE);
  }
}


/*
 * Finish up at the end of the file.
 */

METHODDEF void
finish_output_x11 (j_decompress_ptr cinfo, djpeg_dest_ptr dinfo)
{
    x11_dest_ptr dest = (x11_dest_ptr) dinfo;
    XFlush (dest->display);
    XDestroyImage (dest->ximage);
    if (dest->flag_double)
	XDestroyImage (dest->ximage_double);
}

/*
 * The module selection routine for X format output.
 */

GLOBAL djpeg_dest_ptr
jinit_write_x11 (j_decompress_ptr cinfo, VideotexWidget w)
{
  x11_dest_ptr dest;
  unsigned long r, g, b;
  Visual *visual;
  register VideotexPart *pv = &w->videotex;

  /* Create module interface object, fill in method pointers */
  dest = (x11_dest_ptr)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
				  sizeof(x11_dest_struct));
  dest->pub.start_output = start_output_x11;
  dest->pub.finish_output = finish_output_x11;

  /* Calculate output image dimensions so we can allocate space */
  jpeg_calc_output_dimensions(cinfo);

  /* Create physical I/O buffer.  Note we make this near on a PC. */
  dest->samples_per_row = cinfo->output_width * cinfo->out_color_components;
  dest->buffer_width = dest->samples_per_row * (BYTESPERSAMPLE * sizeof(char));
  dest->iobuffer = (char *) (*cinfo->mem->alloc_small)
    ((j_common_ptr) cinfo, JPOOL_IMAGE, dest->buffer_width);

  if (cinfo->quantize_colors || BITS_IN_JSAMPLE != 8 ||
      sizeof(JSAMPLE) != sizeof(char)) {
    /* When quantizing, we need an output buffer for colormap indexes
     * that's separate from the physical I/O buffer.  We also need a
     * separate buffer if pixel format translation must take place.
     */
    dest->pub.buffer = (*cinfo->mem->alloc_sarray)
      ((j_common_ptr) cinfo, JPOOL_IMAGE,
       cinfo->output_width * cinfo->output_components, (JDIMENSION) 1);
  } else {
    /* We will fwrite() directly from decompressor output buffer. */
    /* Synthesize a JSAMPARRAY pointer structure */
    /* Cast here implies near->far pointer conversion on PCs */
    dest->pixrow = (JSAMPROW) dest->iobuffer;
    dest->pub.buffer = & dest->pixrow;
  }

  dest->pub.buffer_height = 1;
  dest->pub.put_pixel_rows = put_pixel_rows;
  dest->display = XtDisplay(w);
  dest->screen = DefaultScreen(dest->display);
  dest->gc = pv->gc;
  dest->win = XtWindow(w);
  dest->flag_bs = pv->flag_bs;
  dest->win_sauve = pv->ecran_sauve;
  dest->nplanes = DisplayPlanes (dest->display, dest->screen);
  dest->cmap = DefaultColormap (dest->display, dest->screen);
  dest->flag_double = !pv->petite_fonte;
  dest->modulo = 8;
  dest->x_photo = pv->rectangle_photo.x;
  dest->y_photo = pv->rectangle_photo.y;

  /*
   * Calcul des decalages et nb bits si + de 256 couleurs
   */
  if (dest->nplanes > 8) {
    visual = DefaultVisual (dest->display, dest->screen);
    r = red_mask = visual->red_mask;
    g = green_mask = visual->green_mask;
    b = blue_mask = visual->blue_mask;
	
    nb_bits_rouge = 0;
    nb_bits_vert = 0;
    nb_bits_bleu = 0;
    decalage_rouge = 0;
    decalage_vert = 0;
    decalage_bleu = 0;
    
    while (!(r & 1)) {
      r >>= 1;
      decalage_rouge++;
    }
    while (r & 1) {
      r >>= 1;
      nb_bits_rouge++;
    }
    while (!(g & 1)) {
      g >>= 1;
      decalage_vert++;
    }
    while (g & 1) {
      g >>= 1;
      nb_bits_vert++;
    }
    while (!(b & 1)) {
      b >>= 1;
      decalage_bleu++;
    }
    while (b & 1) {
      b >>= 1;
      nb_bits_bleu++;
    }
  }

  /* Liberation eventuelle des couleurs precedentes */
  if (last_was_full_screen) {
      XFreeColors (dest->display, dest->cmap, pixels, last_color, 0);
      last_color = 0;
  }

  last_was_full_screen = (cinfo->output_width == 320 && cinfo->output_height == 240);

  return (djpeg_dest_ptr) dest;
}

/*
 * Marker processor for COM markers.
 * This replaces the library's built-in processor, which just skips the marker.
 * We want to print out the marker as text, if possible.
 * Note this code relies on a non-suspending data source.
 */

LOCAL unsigned int
jpeg_getc (j_decompress_ptr cinfo)
/* Read next byte */
{
  struct jpeg_source_mgr * datasrc = cinfo->src;

  if (datasrc->bytes_in_buffer == 0) {
    if (! (*datasrc->fill_input_buffer) (cinfo))
      ERREXIT(cinfo, JERR_CANT_SUSPEND);
  }
  datasrc->bytes_in_buffer--;
  return GETJOCTET(*datasrc->next_input_byte++);
}

METHODDEF boolean
COM_handler (j_decompress_ptr cinfo)
{
  boolean traceit = (cinfo->err->trace_level >= 1);
  INT32 length;
  unsigned int ch;
  unsigned int lastch = 0;

  length = jpeg_getc(cinfo) << 8;
  length += jpeg_getc(cinfo);
  length -= 2;			/* discount the length word itself */

  if (traceit)
    fprintf(stderr, "COM_handler: Comment, length %ld:\n", (long) length);

  while (--length >= 0) {
    ch = jpeg_getc(cinfo);
    if (traceit) {
      /* Emit the character in a readable form.
       * Nonprintables are converted to \nnn form,
       * while \ is converted to \\.
       * Newlines in CR, CR/LF, or LF form will be printed as one newline.
       */
      if (ch == '\r') {
	fprintf(stderr, "\n");
      } else if (ch == '\n') {
	if (lastch != '\r')
	  fprintf(stderr, "\n");
      } else if (ch == '\\') {
	fprintf(stderr, "\\\\");
      } else if (isprint(ch)) {
	putc(ch, stderr);
      } else {
	fprintf(stderr, "\\%03o", ch);
      }
      lastch = ch;
    }
  }

  if (traceit)
    fprintf(stderr, "\n");

  return TRUE;
}

/* Fonction de decodage */
void decode_jpeg_tvr (w)
VideotexWidget w;
{
    char c;
    struct jpeg_decompress_struct cinfo;
    struct jpeg_error_mgr jerr;
    djpeg_dest_ptr dest_mgr = NULL;
    int input_file;
    JDIMENSION num_scanlines;
    register VideotexPart *pv = &w->videotex;

    /* Initialize the JPEG decompression object with default error handling. */
    cinfo.err = jpeg_std_error(&jerr);

    cinfo.err->trace_level = 0;

    jpeg_create_decompress(&cinfo);
    /* Add some application-specific error messages */
    jerr.addon_message_table = tvr_message_table;
    jerr.first_addon_message =  JMSG_FIRSTADDONCODE;
    jerr.last_addon_message =  JMSG_LASTADDONCODE;

    /* Insert custom COM marker processor. */
    jpeg_set_marker_processor(&cinfo, JPEG_COM, COM_handler);

    /* Specify data source for decompression */
    jpeg_fd_src(&cinfo, pv->fd_connexion, pv->longueur_data_jpeg);

    /* Read file header, set default decompression parameters */
    (void) jpeg_read_header(&cinfo, TRUE);
    
    if (DisplayPlanes(XtDisplay(w), DefaultScreen(XtDisplay(w))) <= 8) {
      /* 50 couleurs */
      cinfo.desired_number_of_colors = 50;
      cinfo.quantize_colors = TRUE;
    }
    else
      cinfo.quantize_colors = FALSE;
    
    /* Initialize the output module now to let it override any crucial
     * option settings (for instance, GIF wants to force color quantization).
     */
    
    dest_mgr = jinit_write_x11(&cinfo, w);
    
    /* Start decompressor */
    jpeg_start_decompress(&cinfo);
    
    /* Write output file header */
    (*dest_mgr->start_output) (&cinfo, dest_mgr);
    
    /* Process data */
    while (cinfo.output_scanline < cinfo.output_height) {
	num_scanlines = jpeg_read_scanlines(&cinfo, dest_mgr->buffer,
					    dest_mgr->buffer_height);
	(*dest_mgr->put_pixel_rows) (&cinfo, dest_mgr, num_scanlines);
    }
    
    /* Finish decompression and release memory.
     * I must do it in this order because output module has allocated memory
     * of lifespan JPOOL_IMAGE; it needs to finish before releasing memory.
     */
    (*dest_mgr->finish_output) (&cinfo, dest_mgr);
    jpeg_finish_decompress(&cinfo);
    jpeg_destroy_decompress(&cinfo);
}

/* liberation des couleurs forcee a cause de l'effacement d'ecran */
void libere_couleurs_tvr (w)
VideotexWidget w;
{
    if (last_color) {
	XFreeColors (XtDisplay(w), DefaultColormap (XtDisplay(w), DefaultScreen(XtDisplay(w))), pixels, last_color, 0);
	last_color = 0;
    }
}

#endif /* NO_TVR */
