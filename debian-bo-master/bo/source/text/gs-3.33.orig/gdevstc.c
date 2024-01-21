/* Copyright (C) 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gdevstc.c */
/* Epson Stylus-Color Printer-Driver */

/***
 *** This file was "copied" from gdevcdj.c (ghostscript-3.12), which was 
 *** contributed by:
 ***    George Cameron      - g.cameron@biomed.abdn.ac.ukis
 ***    Koert Zeilstra      - koert@zen.cais.com
 ***    Eckhard Rueggeberg  - eckhard@ts.go.dlr.de
 ***
 *** I just didn't like to complicate the HP-Drivers by dealing with the
 *** code generation for the Epson printer.
 *** Gunther Hess           - hess@ims.fhg.de (changes soon)
 ***
 *** P.S.: there is some documentation, see devices.doc
 ***
 *** Revision-History:
 *** 16-DEC-1994  1.1  - initial Version (GS-Dithering & Plain-Write)
 *** 17-DEC-1994  1.2  - 24Bit CMYK uni/float FS, buggy runlength write
 *** 29-DEC-1994  1.3  - 32Bit CMYK uni/float FS, runlength write
 *** 30-DEC-1994  1.4  - 24Bit CMY -> 32Bit bidir/long FS, runlength write
 ***  4-JAN-1995  1.5  - FS-Improvements, Int-only, Microweave
 ***              1.6  - Fixed margin & Papersize-stuff
 ***              1.7  - acummulate skips in FS-Mode
 ***              1.8  - more margin-fixes, 1st mail to aladdin
 ***  26-JAN-1995 1.9  - Bug-Fix (omitted WIDTH/HEIGHT-Init)
 ***  29-JAN-1995 1.10 - Bidir & Softweave added, threshold removed -> aladdin
 ***  30-JAN-1995 1.11 - Bug in backward color-FS fixed.
 ***        NOTE: 1.10 still has a bug in get/put_params. If you compile
 ***                   with -DNOSPOTSIZE things might be o.k., but this
 ***                   inhibits external spotsize-control. Another Workaround
 ***                   seems to be setting the SpotSize always.
 ***   5-mar-1995 1.12 - L. Peter Deutsch - updated put_params routine,
 ***                   believe I fixed the spot size bug, but have no way to
 ***                   test the result.
 ***/

/*
 *  from the gs command line, eg.:
 *
 *  gs -sDEVICE=stcolor -r720x720 -dSpotSize='{2.6 2.8 2 2.6}' tiger.ps
 *
 */

#include "std.h"      /* to stop stdlib.h redefining types */
#include <stdlib.h>   /* for rand() */
#include "gdevprn.h"
#include "gsparam.h"
#include "gsstate.h"


/* Default margins are from A4, since I don't know other Values */

#ifndef   STCOLOR_MARGINS /* left bottom  right    top */
#define   STCOLOR_MARGINS   0.100, 0.532, 0.200, 0.334
#endif /* STCOLOR_MARGINS */

#ifndef  STCOLOR_WIDTH
# ifdef A4
#   define  STCOLOR_WIDTH   83 /* 1/10" : 8.264" */
# else
#   define  STCOLOR_WIDTH   85
# endif
#endif /* STCOLOR_WIDTH */
#ifndef   STCOLOR_HEIGHT
# ifdef A4
#   define  STCOLOR_HEIGHT 117 /* 1/10" : 11.694  */
# else
#   define  STCOLOR_HEIGHT 110
# endif
#endif /* STCOLOR_HEIGHT */

#ifndef   X_DPI
#define   X_DPI   360
#endif /* X_DPI */
#ifndef   Y_DPI 
#define   Y_DPI   360
#endif /* Y_DPI */

/* Colour mapping procedures */
private dev_proc_map_rgb_color(stcolor_map_rgb_color);
private dev_proc_map_color_rgb(stcolor_map_color_rgb);

/* Print-page, parameters and miscellaneous procedures */
private dev_proc_open_device(stcolor_open);
private dev_proc_print_page(stcolor_print_page);
private dev_proc_get_params(stcolor_get_params);
private dev_proc_put_params(stcolor_put_params);

/* The device descriptor */
typedef struct stcolor_device_s {
	gx_device_common;
	gx_prn_device_common;
	float spotsize[4];             /* Density & Color-Adjust */
	int outputcode;                /* Control over Code-Generation */
} stcolor_device;

#define stcolor(p) ((stcolor_device *)(p))

private gx_device_procs stcolor_procs = {
        stcolor_open,
        gx_default_get_initial_matrix,
        gx_default_sync_output,
        gdev_prn_output_page,
        gdev_prn_close,
        stcolor_map_rgb_color,
        stcolor_map_color_rgb,
        NULL,   /* fill_rectangle */
        NULL,   /* tile_rectangle */
        NULL,   /* copy_mono */
        NULL,   /* copy_color */
        NULL,   /* draw_line */
        gx_default_get_bits,
        stcolor_get_params,
        stcolor_put_params
};


stcolor_device far_data gs_stcolor_device = {
   prn_device_body(gx_device_printer, stcolor_procs, "stcolor",
      STCOLOR_WIDTH, STCOLOR_HEIGHT,
      X_DPI,  Y_DPI,
      0,  0, 0, 0,       /* margins established by _open */ 
      0, 24, 0, 0, 0, 0, /* just depth, but nothing else, set_bpp fills it */
      stcolor_print_page),
      {1.0, 1.0, 1.0, 1.0},
     25                  /* Default "OutputCode": runlength, s-weave, bidir */
};


/* Forward references */
private void set_bpp(P2(gx_device *, int));

/* ------ Internal routines ------ */
                               /*      K     C     M     Y */
static const byte stc_colors[] =  { 0x00, 0x02, 0x01, 0x04 };   /* TX-Order */

/*
 * Macros for dithering
 */
# define FS_RAND (((((long) rand()) << 8) & 0xffffffL) - 0x800000L)

# define FS_DITHER(PIXEL,OUT,BIT,THRESHOLD,SPOT,ERR3_16,ERR5_16,ERR8_16){\
   long Err = (PIXEL) + (ERR5_16) + ((ERR8_16)-(((ERR8_16)+4)>>3));	\
   if(Err > (THRESHOLD)) {						\
      Err -= SPOT;							\
      OUT |= BIT;							\
   }									\
   ERR3_16 +=  (3*Err+8)>>4;						\
   ERR5_16  = ((5*Err+8)>>4)+(((ERR8_16)+4)>>3);			\
   ERR8_16  = Err-((5*Err+8)>>4)-((3*Err+8)>>4);			\
}

private int
stcolor_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
   int in_bytes,       /* max # of bytes from ghostscript */
       out_pixels,     /* max # of pixels written */
       out_bytes,      /* rounded up to bytes */
       out_scans;      /* number of scanlines */

   int  blubber;       /* controls messages */
   int  npass,         /* # of print-passes (softweave) */
        ncolor,        /* # of colors written */
        nbuffer;       /* # of buffered scanlines */

   byte *dyn_mem,      /* allocated memory */
       **d_buf,        /* pointers to scanlines */
        *g_buf;        /* the input-buffer */
   int  *w_buf;        /* width in bytes of scanlines */
   long *e_buf,        /* Floyd-Steinberg error-vector (colors*(npixels+2)) */
        *err,          /* Floyd-Steinberg error-value  (colors) */
        *spot,         /* Floyd-Steinberg Spotsize     (colors) */
        *threshold;    /* Floyd-Steinberg Threshold    (colors) */    

   int   dyn_siz,      /* size of allocated memory */
         stc_y,        /* head-position */
         skip,         /* number of lines to skip */
         buf_y,        /* top of buffer */
         buf_i,        /* scan-buffer index */
         buf_msk,      /* mask for scan-buffer index */
         dir;          /* Floyd-Steinberg Direction (toggles fwd <> rev) */

   byte escp_dot[8],   /* Graphics-cmd */
        escp_skip[8],  /* Verical skip */
        escp_color[4]; /* <CR> color-command */
   int  escp_unit,     /* 3600 / YDPI */
        escp_weave,    /* controls hardware Microweave */
        escp_unidir;   /* controls unidirectional print */
#define ESCP_VERTICAL   escp_dot[3]
#define ESCP_HORIZONTAL escp_dot[4]
#define ESCP_HEADS      escp_dot[5]
#define ESCP_COLOR      escp_color[3]

/*
 * Process output-code (but resolution may change things)
 */

   if( stcolor(pdev)->outputcode & 16)       escp_unidir = 0;
   else                                      escp_unidir = 1;

   if((stcolor(pdev)->outputcode & 12) == 4) escp_weave  = 1;
   else                                      escp_weave  = 0;

   if((stcolor(pdev)->outputcode & 12) == 8) npass       = 1;
   else                                      npass       = 0;

   if(stcolor(pdev)->outputcode & 512)       blubber     = 1;
   else                                      blubber     = 0;

/*
 * color or monochrome ?
 */
    if(pdev->color_info.num_components > 1) ncolor = 4;
    else                                    ncolor = 1;

/*
 * Process resolutions. some inhibit microweave, others require softweave.
 */
   
   ESCP_HORIZONTAL = 3600 / (int) pdev->x_pixels_per_inch;
   escp_unit       = 3600 / (int) pdev->y_pixels_per_inch;
   ESCP_VERTICAL   = escp_unit; /* Softweave sets this to 3600/90 */
   ESCP_HEADS      = 1;         /* changed if, ?x90 -> 15, 720x720 ->1 */

   if(       pdev->x_pixels_per_inch == 180) {

      if(       pdev->y_pixels_per_inch ==  90) {

         escp_weave    =  0; /* impossible */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  1;

      } else if(pdev->y_pixels_per_inch == 180) {

         if(npass) {
            ESCP_VERTICAL = 3600 / 90;
            ESCP_HEADS    = 15;
            npass         =  2;
         } else {
         /* ESCP_HEADS    = 1 , 8, 24 o.k. */
            npass         = 1; 
         }

      } else if(pdev->y_pixels_per_inch == 360) {

         escp_weave    =  0; /* softweave required */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  4;

      } else if(pdev->y_pixels_per_inch == 720) {

         escp_weave    =  0; /* softweave required */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  8;

      } else {

         if(blubber) fprintf(stderr,
            "stcolor: Y-Dpi (%d) not one of 90/180/360/720\n",
            (int) pdev->y_pixels_per_inch);
         return_error(gs_error_rangecheck);

      }

   } else if(pdev->x_pixels_per_inch == 360) {

      if(       pdev->y_pixels_per_inch ==  90) {

         escp_weave    =  0; /* impossible */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  1;

      } else if(pdev->y_pixels_per_inch == 180) {

         if(npass) {
            ESCP_VERTICAL = 3600 / 90;
            ESCP_HEADS    = 15;
            npass         =  2;
         } else {
         /* ESCP_HEADS    = 1 , 8, 24 o.k. */
            npass         = 1; 
         }

      } else if(pdev->y_pixels_per_inch == 360) {

         if(npass) {
            ESCP_VERTICAL = 3600 / 90;
            ESCP_HEADS    = 15;
            npass         =  4;
         } else {
         /* ESCP_HEADS    = 1 , 8, 24 o.k. */
            npass         = 1; 
         }

      } else if(pdev->y_pixels_per_inch == 720) {

         escp_weave    =  0; /* softweave required */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  8;

      } else {

         if(blubber) fprintf(stderr,
            "stcolor: Y-Dpi (%d) not one of 90/180/360/720\n",
            (int) pdev->y_pixels_per_inch);
         return_error(gs_error_rangecheck);

      }

   } else if(pdev->x_pixels_per_inch == 720) {

      if(       pdev->y_pixels_per_inch ==  90) {

         escp_weave    =  0; /* impossible */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  1;

      } else if(pdev->y_pixels_per_inch == 180) {

         escp_weave    =  0; /* softweave required */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  2;

      } else if(pdev->y_pixels_per_inch == 360) {

         escp_weave    =  0; /* softweave required */
         ESCP_VERTICAL = 3600 / 90;
         ESCP_HEADS    = 15;
         npass         =  4;

      } else if(pdev->y_pixels_per_inch == 720) {

         if(npass) {
            ESCP_VERTICAL = 3600 / 90;
            ESCP_HEADS    = 15;
            npass         =  8;
         } else {
            ESCP_HEADS    =  1; /* manual says,that this is required */
            npass         =  1; 
         }

      } else {

         if(blubber) fprintf(stderr,
            "stcolor: Y-Dpi (%d) not one of 90/180/360/720\n",
            (int) pdev->y_pixels_per_inch);
         return_error(gs_error_rangecheck);

      }

   } else {

      if(blubber) fprintf(stderr,
        "stcolor: X-Dpi (%d) not one of 180/360/720\n",
        (int) pdev->x_pixels_per_inch);
      return_error(gs_error_rangecheck);

   }

/*
 * compute some sizes, that depend on the printing-area
 */

   in_bytes   = gdev_prn_raster(pdev);
   out_pixels = pdev->width - 
      (dev_l_margin(pdev)+dev_r_margin(pdev))*pdev->x_pixels_per_inch;
   out_bytes  = (out_pixels+7)/8;
   out_pixels = out_bytes * 8;
   out_scans  = pdev->height - 
      (dev_t_margin(pdev)+dev_b_margin(pdev))*pdev->y_pixels_per_inch;


/*
 * compute the size of the dynamic memory
 */
    for(nbuffer = 1; nbuffer < ((int) ESCP_HEADS * npass); nbuffer <<= 1);
    nbuffer *= ncolor;      /* ncolor is either 2^0 or 2^2 */
    buf_msk  = nbuffer - 1; /* nbuffer is a power of 2, including 2^0 */

/*                          w_buf        d_buf            *d_buf */
    dyn_siz  = nbuffer * (sizeof(int) + sizeof(byte *) + out_bytes);
    if(pdev->color_info.depth > 4) /* need: e_buf, spot, threshold, err */
       dyn_siz += (out_pixels+5) * sizeof(long) * ncolor; 
    dyn_siz += in_bytes; /* input-buffer (g_buf) */

/*
 * get the buffer, and set the pointers 
 */

   dyn_mem = gs_malloc(dyn_siz,1,"stcolor_print_page");

   d_buf    = (byte **) dyn_mem;    /* Pointers may be longest */

   if(pdev->color_info.depth > 4) { /* longs may be longer than int's */
      int i;

      e_buf     = (long *) (d_buf + nbuffer);
      spot      = e_buf     + ncolor * (out_pixels+2);
      threshold = spot      + ncolor;
      err       = threshold + ncolor;

      w_buf     = (int  *) (err + ncolor);

/*    randomly preset error-buffers */
      for(i = 0; i < (ncolor * (out_pixels+2)); ++i) e_buf[i] = (FS_RAND*5)/16;
      for(i = 0;  i <  ncolor;                   ++i) err[i]   = (FS_RAND*8)/16;
      e_buf    += ncolor; /* !! additional pixel at the beginning */

/*    compute spotsize */
      for(i = 0; i < ncolor; ++i) {
         spot[i] = stcolor(pdev)->spotsize[i] * 255.0 * 65536.0 + 0.49;
         if(spot[i] <= 0) spot[i] = 255<<16;
         threshold[i] = spot[i]>>1;
      }

   } else {

      e_buf     = NULL;
      spot      = NULL;
      threshold = NULL;
      err       = NULL;

      w_buf     = (int  *) (d_buf + nbuffer);

   }

   g_buf    = (byte *) (w_buf + nbuffer);
   d_buf[0] = g_buf + in_bytes;
   for(buf_i = 1; buf_i < nbuffer; ++buf_i)
         d_buf[buf_i] = d_buf[buf_i-1] + out_bytes;


/*
 * "schwall" to the user
 */

   if(blubber) {
      fprintf(stderr,"stcolor: Resolution:    %4dx%4d DpI\n",
         (int) pdev->x_pixels_per_inch,(int) pdev->y_pixels_per_inch);
      fprintf(stderr,"         Media-Size:    %4dx%4d Pixel\n",
          pdev->width,pdev->height);
      fprintf(stderr,"         Printing-Area: %4dx%4d Pixel\n",
          out_pixels,out_scans);
      fprintf(stderr,"         Buffer-Size:%12d Bytes\n",dyn_siz);
      fputs("        ",stderr);
      if(escp_weave) fputs(" Microweave",stderr);
      if(npass > 1) fprintf(stderr," Softweave with %d passes",npass);
      fprintf(stderr," %d Heads",ESCP_HEADS);
      if(escp_unidir) fputs(" unidirectional\n",stderr);
      else            fputs(" bidirectional\n",stderr);

      if(ncolor > 1) {

         int color;
         fputs("         color",stderr);
         if(pdev->color_info.depth > 4) {

            fputs(" Floyd-Steinberg",stderr);
            for(color = 0; color < ncolor; ++color)
               fprintf(stderr," %c(%.2f)",
                  "KMCVYRG?"[stc_colors[color] < 7 ? stc_colors[color] : 7],
                  (double)spot[color]/(255.0*65536.0));

         } else {

            fputs(" GhostScript ",stderr);
            for(color = 0; color < 4; ++color) 
               putc("KMCVYRG?"[stc_colors[color] < 7 ? stc_colors[color] : 7],
                   stderr);
         }
         putc('\n',stderr);

      } else {
         
         if(pdev->color_info.depth > 4) {
           fprintf(stderr,"         mono Floyd-Steinberg K(%.2f)\n",
             (double)spot[0]/(255.0*65536.0));
         } else {
            fputs("         mono GhostScript\n",stderr);
         }

      }
   } /* "schwall" */


/* 
 * initialize the printer (and related data)
 */
   fwrite("\033@\r\033(G\001\000\001", 1, 9, prn_stream);   /* => g-mode */

   fwrite("\033(U\001\0",1,5,prn_stream);                   /* units (y) */
      fputc(escp_unit,prn_stream);

   fwrite("\033(C\002\0",1,5,prn_stream);                   /* Page-Length */
      fputc( ((int) pdev->height)       & 0xff,prn_stream);
      fputc((((int) pdev->height) >> 8) & 0xff,prn_stream);

   fwrite("\033(c\004\0\0\0",1,7,prn_stream);               /* Margins */
      fputc( out_scans    & 0xff,prn_stream);
      fputc((out_scans>>8)& 0xff,prn_stream);

   if(escp_weave) fwrite("\033(i\001\0\001",1,6,prn_stream); /* uweave on */
   else           fwrite("\033(i\001\0\000",1,6,prn_stream); /* uweave off */

   if(escp_unidir) fwrite("\033U\1",1,3,prn_stream);        /* unidir on */
   else            fwrite("\033U\0",1,3,prn_stream);        /* unidir off */

   escp_dot[0]   = '\033'; escp_dot[1] = '.';               /* print graph. */
   escp_dot[2]   = '\001';                                  /* rl-code */

   escp_skip[0]  = '\033'; escp_skip[1] = '(';              /* skip-rel */
   escp_skip[2]  = 'v';    escp_skip[3] = '\002';
   escp_skip[4]  = '\000';

   escp_color[0] = '\r'; escp_color[1] = '\033';            /* \r, color */
   escp_color[2] = 'r';  

   if(ncolor == 1) ESCP_COLOR = stc_colors[0]; /* suppress selection */
   else            ESCP_COLOR = 0xff;          /* force initial selection */

/*
 * prepare run-values, then loop over scans 
 */
   skip       =  0; /* Begin at Top of Form */
   stc_y      =  0; /* current printer y-Position */
   buf_y      =  0; /* Top-Position with in the buffer */
   buf_i      =  0; /* next free line in buffer */
   dir        =  0; /* First FS-Run: forward */

   while(stc_y < out_scans) {  /* Until all scans are processed */
      int nprint,nspace,need,color;

/*
 * compute spacing & used heads
 * (I've not checked wether initialisation works with ESCP_HEADS != 15)
 */
      if((npass == 1) || (stc_y >= ESCP_HEADS)) { /* in normal mode */
         nprint = ESCP_HEADS;
         nspace = ESCP_HEADS;
      } else if((stc_y) < npass) {                /* initialisation-process */
         nprint = ESCP_HEADS - stc_y * ((ESCP_HEADS+1)/npass);
         nspace = 1;
      } else {                                   /* switch to normal mode */
         nprint = ESCP_HEADS - stc_y * ((ESCP_HEADS+1)/npass);
         nspace = ESCP_HEADS - stc_y;
      }

      need = stc_y + npass * nprint; /* required data for this command */

      if(buf_y < need) { /* Nr. 5 (give me input) */

/*       read as much as the buffer can hold */
         if(ncolor == 1) need = stc_y + nbuffer;
         else            need = stc_y + (nbuffer>>2);


         for(; buf_y < need; buf_i = buf_msk & (buf_i+ncolor), ++buf_y) {

            int color;
            byte *pixel;
            
/*          initialize output data 1st -> may take shortcut */

            for(color = 0; color < ncolor; ++color) {
               memset(d_buf[buf_i+color],0,out_bytes);
               w_buf[buf_i+color] = 0;
            }


/*          "read data", immediately continue if all is white */

            if(buf_y < out_scans) {
               int bytein;

               gdev_prn_get_bits(pdev, buf_y, g_buf, &pixel);

               for(bytein = 0; bytein < in_bytes; ++bytein)
                  if(pixel[bytein]) break;

               if(bytein >= in_bytes) continue; /* white stays white */

            } else {

               continue;

            }


/*          convert data for the various cases */

            if(pdev->color_info.depth <  8) { /* GhostScript cases */

               if(ncolor == 1) { /* GhostScript-mono => no conversion */

                  int i,w;
                  byte *op;

                  w  = -1;
                  op = d_buf[buf_i];
                  for(i = 0; i < out_bytes; ++i) if((*op++ = *pixel++)) w = i;

                  w_buf[buf_i] = w + 1;

               } else {          /* GhostScript-color => split bits */

                  int  bytein,byteout,color;
                  byte bitin,bitout;
                  
                  byteout = 0; bitout = 0x80;
                  
                  for(bytein = 0; (byteout < out_bytes) && (bytein < in_bytes);
                    ++bytein) { /* over groups of 2 pixels */

                      for(color = 0, bitin = 0x80; bitin; bitin >>= 1) {

                         if(pixel[bytein] & bitin) 
                            d_buf[buf_i+color][byteout] |= bitout;

                         if(++color == ncolor) {
                            color = 0;
                            if((bitout >>= 1) == 0) {
                               if(++byteout == out_bytes) break;
                               bitout = 0x80;
                            }
                         }
                      }

                  }             /* over groups of 2 pixels */

                  for(color = 0; color < ncolor; ++color) { /* widths */

                     for(byteout = out_bytes; byteout; --byteout) { /* bytes */
                        if(d_buf[buf_i+color][byteout-1]) {
                           w_buf[buf_i+color] = byteout;
                           break;
                        }
                     }                                             /* bytes */
                  }                                         /* widths */

               }           /* Ghostscript mono/color */

            } else { /* Floyd-Steinberg */

               if(ncolor == 1) { /* Floyd-Steinberg mono */

                  int  bytein;
                  long *errp,k;
                  byte *byteout,bitout;

                  if(dir) { /* backward-dither */

                     bytein  = out_pixels-1; /* Pixel-Number */
                     byteout = d_buf[buf_i]+(bytein>>3);
                     bitout  = 0x80>>(bytein&7);
                     errp    = e_buf + bytein;

                     for(; bytein >= 0; --bytein) { /* loop over pixels */

                        k = pixel[bytein];   k <<= 16;

                        FS_DITHER(k, *byteout,bitout,*threshold,*spot,
                        errp[1],errp[0], err[0]);

                        errp--;

                        if((bitout <<= 1) == 0) {
                           bitout  = 0x01;
                           byteout--;
                        }

                     } /* loop over pixels */

                     dir = 0; /* next will be forward */

                  } else {  /* forward-dither */

                     bytein  = 0;
                     byteout = d_buf[buf_i];
                     bitout  = 0x80;
                     errp    = e_buf;

                     for(; bytein < out_pixels; ++bytein) { /* loop over pxls */

                        k = pixel[bytein];   k <<= 16;

                        FS_DITHER(k, *byteout,bitout,*threshold,*spot,
                        errp[-1],errp[0], err[0]);

                        errp++;

                        if((bitout >>= 1) == 0) {
                           bitout  = 0x80;
                           byteout++;
                        }

                     } /* loop over pixels */

                     dir = 1; /* next will be backward */

                  } /* Floyd-Steinberg mono backward/foreward */

                  byteout = d_buf[buf_i]+out_bytes;
                  for(bytein = out_bytes; bytein; --bytein) { /* bytes */
                        if(*--byteout) {
                           w_buf[buf_i] = bytein;
                           break;
                        }
                  }

               } else { /* Floyd-Steinberg color */

                  int   bytein,color,byteout;
                  long *errp,kcmy[4];
                  byte  bitout;

                  if(dir) { /* backward-dither */

                     bytein  = out_pixels-1; /* Pixel-Number */
                     byteout = bytein>>3;
                     bitout  = 0x80>>(bytein&7);
                     errp    = e_buf + bytein * 4 + 3; /* Kcmy */
                     bytein  =         bytein * 3 + 2; /*  cmy */

                     while(bytein >= 0) { /* loop over pixels */
                        
                        kcmy[0] = 255<<16;
                        for(color = 3; color; --color) {
                           kcmy[color] = pixel[bytein--];
                           if((kcmy[color] <<= 16) < kcmy[0]) 
                              kcmy[0] = kcmy[color];
                        }

                        if(kcmy[0])
                           for(color = 1; color < 4; ++color) 
                              kcmy[color] -= kcmy[0];


                        for(color = 3; color >= 0; --color) {

                           FS_DITHER(kcmy[color],
                              d_buf[buf_i+color][byteout],bitout,
                              threshold[color],spot[color],
                              errp[4],errp[0], err[color]);

                           errp--;

                        }

                        if((bitout <<= 1) == 0) {
                           bitout  = 0x01;
                           byteout--;
                        }

                     } /* loop over pixels */

                     dir = 0; /* next will be forward */

                  } else {  /* forward-dither */

                     bytein  = 0;
                     byteout = 0;
                     bitout  = 0x80;
                     errp    = e_buf;

                     while(byteout < out_bytes) { /* loop over pixels */
                        
                        kcmy[0] = 255<<16;
                        for(color = 1; color < 4; ++color) {
                           kcmy[color] = pixel[bytein++];
                           if((kcmy[color] <<= 16) < kcmy[0]) 
                              kcmy[0] = kcmy[color];
                        }

                        if(kcmy[0])
                           for(color = 1; color < 4; ++color) 
                              kcmy[color] -= kcmy[0];


                        for(color = 0; color < 4; ++color) {

                           FS_DITHER(kcmy[color],
                              d_buf[buf_i+color][byteout],bitout,
                              threshold[color],spot[color],
                              errp[-4],errp[0], err[color]);

                           errp++;

                        }

                        if((bitout >>= 1) == 0) {
                           bitout  = 0x80;
                           byteout++;
                        }

                     } /* loop over pixels */

                     dir = 1; /* next will be backward */

                  } /* Floyd-Steinberg color backward/foreward */

                  for(byteout = 0; byteout < out_bytes; ++byteout) { /* k-sep */

                     bitout = 0xff;
                     for(color = 1; color < 4; ++color)
                           bitout &= d_buf[buf_i+color][byteout];

                     bitout |= d_buf[buf_i][byteout];
                     d_buf[buf_i][byteout] = bitout;
                     if(bitout) w_buf[buf_i] = byteout+1;

                     bitout = ~bitout;
                     for(color = 1; color < 4; ++color) {
                        if(d_buf[buf_i+color][byteout] &= bitout) 
                           w_buf[buf_i+color] = byteout+1;
                     }

                  } /* k-sep */

               } /* Floyd-Steinberg mono/color */

            } /* GhostScript / Floyd-Steinberg */

         } /* Fill the entire buffer */

      } /* Nr. 5 (give me input) */

/*
 *    Nr. 5 has got enough input, now he should print it
 */

      for(color = 0; color < ncolor; ++color) { /* print the colors */
         int buf_a,iprint,w;
            
         buf_a = buf_msk & (stc_y * ncolor + color);
         w = 0;
         for(iprint = 0; iprint < nprint; ++iprint) { /* check width */
            if(w_buf[buf_a] > w) w = w_buf[buf_a];
            buf_a = buf_msk & (buf_a + ncolor * npass);
         }                                            /* check width */
         if(w == 0) continue; /* shortcut */


         if(skip) {                 /* really position the printer */
            escp_skip[5] =  skip       & 0xff;
            escp_skip[6] = (skip >> 8) & 0xff;
            fwrite(escp_skip,1,7,prn_stream);
            skip = 0;
         }                          /* really position the printer */

         if(ESCP_COLOR == stc_colors[color]) { /* return */

            putc(escp_color[0],prn_stream);

         } else {                             /* color-switch */

            ESCP_COLOR = stc_colors[color];
            fwrite(escp_color,1,4,prn_stream);

         }                                    /* return / color-switch */

         escp_dot[6] = 255 & (w<<3);  /* width in Raster-Pixels */
         escp_dot[7] = 255 & (w>>5);

         fwrite(escp_dot,1,8,prn_stream); /* send command */

         buf_a = buf_msk & (stc_y * ncolor + color);
         for(iprint = 0; iprint < nprint; ++iprint) { /* send data */
            byte *line;
            int is,ie,cdata,crun;

            is     = 0; 
            cdata  = 0; 
            crun   = 1; 
            line   = d_buf[buf_a];
            for(ie = 1; ie < w; ++ie) {   /* RL-Encoding */
               if(line[ie] == line[is]) {
                  crun++;
               } else {
                  if(crun < 4) {
                     cdata += crun;
                  } else {
                     while(cdata > 0) {
                        int n2do = cdata < 128 ? cdata : 128;
                        putc(n2do-1,prn_stream);
                        fwrite(line+is-cdata,1,n2do,prn_stream);
                        cdata -= n2do;
                     }
                     while(crun > 0) {
                        int n2do = crun < 128 ? crun : 128;
                        putc(256 - n2do + 1,prn_stream);
                        putc(line[is]      ,prn_stream);
                        crun -= n2do;
                     }
                  }
                  crun = 1;
                  is = ie;
               }
            }                             /* RL-Encoding */

            if(crun < 4) { cdata += crun; is += crun; crun = 0; }

            while(cdata > 0) {          /* trailer-data */
               int n2do = cdata < 128 ? cdata : 128;
               putc(n2do-1,prn_stream);
               fwrite(line+is-cdata,1,n2do,prn_stream);
               cdata -= n2do;
            }                           /* trailer-data */
            while(crun > 0) {           /* trailer-run */
               int n2do = crun < 128 ? crun : 128;
               putc(256 - n2do + 1,prn_stream);
               putc(line[is]      ,prn_stream);
               crun -= n2do;
            }                           /* trailer-run */


            buf_a = buf_msk & (buf_a + ncolor * npass);
         }                                            /* send data */

         while(iprint++ < ESCP_HEADS) {  /* add empty rows */
            int crun = w;
            while(crun > 0) {  /* RL-Limits */
               int n2do = crun < 128 ? crun : 128;
               putc(256 - n2do + 1,prn_stream);
               putc(             0,prn_stream);
               crun -= n2do;
            }                  /* RL-Limits */
         }                            /* add empty rows */
      }                                             /* print the colors */

      skip  += nspace;
      stc_y += nspace;

   }                           /* Until all scans are processed */

   fputs("\033@\f", prn_stream); /* leave-graphics, eject */
   fflush(prn_stream);

   gs_free(dyn_mem,dyn_siz,1,"stcolor_print_page");

   return 0;
}

/*
 * Map a r-g-b color to a color index.
 * We complement the colours, since we're using cmy anyway, and
 * because the buffering routines expect white to be zero.
 */
private gx_color_index
stcolor_map_rgb_color(gx_device *pdev,
                      gx_color_value r, gx_color_value g, gx_color_value b)
{
   gx_color_index rv;
   unsigned long black,cyan,magenta,yellow;

   if(pdev->color_info.num_components == 1) { /* monochrome modes */

      rv    = 255.4999
            - 0.30 * gx_color_value_to_byte(r)
            - 0.59 * gx_color_value_to_byte(g)
            - 0.11 * gx_color_value_to_byte(b);

      if(pdev->color_info.depth == 1) {

         if(rv  > (gx_color_index) 127) rv = (gx_color_index) 1;
         else                           rv = (gx_color_index) 0;

      } else if(rv > (gx_color_index) 255) {

         rv = (gx_color_index) 255;

      }
       
   } else {                                   /* color modes */

      cyan     = 255 - gx_color_value_to_byte(r);
      magenta  = 255 - gx_color_value_to_byte(g);
      yellow   = 255 - gx_color_value_to_byte(b);
   

      if(pdev->color_info.depth == 4) {

         rv = (gx_color_index) 0;
         if(yellow  > 127) rv |= (gx_color_index) 1;
         if(magenta > 127) rv |= (gx_color_index) 2;
         if(cyan    > 127) rv |= (gx_color_index) 4;
         if(rv == (gx_color_index) 7) rv = (gx_color_index) 8;

      } else if(pdev->color_info.depth == 24) {

         rv = (gx_color_index) ((((cyan << 8) | magenta) << 8) | yellow);

      } else {

         black    = cyan  < magenta ?  cyan : magenta;
         black    = black < yellow  ? black : yellow;
         cyan    -= black;
         magenta -= black;
         yellow  -= black;
         rv = (gx_color_index) 
              ((((((black << 8) | cyan) << 8) | magenta) << 8) | yellow);

      }

   } /* color / monochrome - device */

   return rv;
}


/* Map a color index to a r-g-b color. */
private int
stcolor_map_color_rgb(gx_device *pdev, gx_color_index color,
             gx_color_value prgb[3])
{

   if(pdev->color_info.num_components == 1) {

      if(pdev->color_info.depth == 1) {

         if(color > (gx_color_index) 0) 
            prgb[0] = gx_color_value_from_byte(0);
         else
            prgb[0] = gx_color_value_from_byte(255);

      } else {

         prgb[0] = gx_color_value_from_byte(255 - color);

      }

      prgb[1] = prgb[0];
      prgb[2] = prgb[1];

   } else {

      if(pdev->color_info.depth == 4) {

         switch(color) {
            case (gx_color_index) 0: /* White */
               prgb[0] = gx_color_value_from_byte(255);
               prgb[1] = gx_color_value_from_byte(255);
               prgb[2] = gx_color_value_from_byte(255);
               break;
            case (gx_color_index) 1: /* Yellow */
               prgb[0] = gx_color_value_from_byte(255);
               prgb[1] = gx_color_value_from_byte(255);
               prgb[2] = gx_color_value_from_byte(  0);
               break;
            case (gx_color_index) 2: /* Magenta */
               prgb[0] = gx_color_value_from_byte(255);
               prgb[1] = gx_color_value_from_byte(  0);
               prgb[2] = gx_color_value_from_byte(255);
               break;
            case (gx_color_index) 3: /* red */
               prgb[0] = gx_color_value_from_byte(255);
               prgb[1] = gx_color_value_from_byte(  0);
               prgb[2] = gx_color_value_from_byte(  0);
               break;
            case (gx_color_index) 4: /* cyan */
               prgb[0] = gx_color_value_from_byte(  0);
               prgb[1] = gx_color_value_from_byte(255);
               prgb[2] = gx_color_value_from_byte(255);
               break;
            case (gx_color_index) 5: /* green */
               prgb[0] = gx_color_value_from_byte(  0);
               prgb[1] = gx_color_value_from_byte(255);
               prgb[2] = gx_color_value_from_byte(  0);
               break;
            case (gx_color_index) 6: /* blue */
               prgb[0] = gx_color_value_from_byte(  0);
               prgb[1] = gx_color_value_from_byte(  0);
               prgb[2] = gx_color_value_from_byte(255);
               break;
            default: /* black */
               prgb[0] = gx_color_value_from_byte(  0);
               prgb[1] = gx_color_value_from_byte(  0);
               prgb[2] = gx_color_value_from_byte(  0);
               break;
         }

      } else {

         prgb[0] = gx_color_value_from_byte(255 - ((color >> 16) & 255));
         prgb[1] = gx_color_value_from_byte(255 - ((color >>  8) & 255));
         prgb[2] = gx_color_value_from_byte(255 - ( color        & 255));
       

      }
   }

   return 0;
}

/* ------------------------------------------------------------------------- */
/* stcolor device-utilities */

/* Open the printer and set up the margins. */
private int
stcolor_open(gx_device *pdev)
{   /* Change the margins if necessary. */
  static float m[4] = { STCOLOR_MARGINS };

  /* Set up color params if put_params has not already done so */

  if (pdev->color_info.num_components == 0) 
     set_bpp(pdev, pdev->color_info.depth);

  if((dev_l_margin(pdev) == 0) && (dev_r_margin(pdev) == 0) &&
     (dev_b_margin(pdev) == 0) && (dev_t_margin(pdev) == 0)) {

     gx_device_set_margins(pdev, m, true);
  }


  return gdev_prn_open(pdev);
}

/* ------------------------------------------------------------------------- */
/* slightly modified utilities from gdevcdj.c */


/*
 * fill in color_info from bpp (bits_per_pixel)
 */

private void
set_bpp(gx_device *pdev, int bpp)
{ 
   gx_device_color_info *ci = &pdev->color_info;

/* the distinct bpp's are -infinity-1, 3-7,   8-23, 24-infinity */
/* and yield                   1       4      8bw   24          */

   if(bpp < 3) {
      ci->num_components = 1; /* Black & White */
      ci->depth          = 1;
      ci->max_gray       = 1;
      ci->max_color      = 0;
      ci->dither_grays   = 2;
      ci->dither_colors  = 0;
  } else if(bpp < 8) {        /* external dithering */
      ci->num_components = 3;
      ci->depth          = 4;
      ci->max_gray       = 1;
      ci->max_color      = 1;
      ci->dither_grays   = 2;
      ci->dither_colors  = 2;
  } else if(bpp < 24) {       /* monochrome Floyd-Steinberg */
      ci->num_components = 1;
      ci->depth          = 8;
      ci->max_gray       = 255;
      ci->max_color      = 0;
      ci->dither_grays   = 5;
      ci->dither_colors  = 0; /* unused ? */
  } else {                    /* color Floyd-Steinberg */
      ci->num_components = 3;
      ci->depth          = 24;
      ci->max_gray       = 255;
      ci->max_color      = 255;
      ci->dither_grays   = 5; /* unused ? */
      ci->dither_colors  = 5; /* unused ? */
  }
}

/* Get parameters. */

private int
stcolor_get_params(gx_device *pdev, gs_param_list *plist)
{   
   int code;
   gs_param_float_array ssa;

   code = gdev_prn_get_params(pdev, plist);
   if ( code < 0 )
     return code;

   code = param_write_int(plist, "OutputCode",  
			  &stcolor(pdev)->outputcode);
   if ( code < 0 )
     return code;

   ssa.data = stcolor(pdev)->spotsize; ssa.size = 4; ssa.persistent = false;
   code = param_write_float_array(plist, "SpotSize", &ssa);
#  if 0
      fprintf(stderr,
        "wfa-Code = %d, ssa.data = 0x%x, ssa.size = %d, ssa.persistent = %d\n",
       code, (int)ssa.data, ssa.size, (int)ssa.persistent);
#   endif
   return code;
}

/* Put parameters. */
private int
stcolor_put_params(gx_device *pdev, gs_param_list *plist)
{
   int code;
   int bpp = pdev->color_info.depth;
   gx_device_color_info save_info;
   int outcode = stcolor(pdev)->outputcode;
   gs_param_float_array ssa; /* Spotsize */

   code = param_read_int(plist, "BitsPerPixel", &bpp);
   if ( code < 0 )
     return code;

   code = param_read_int(plist, "OutputCode", &outcode);
   if ( code < 0 )
     return code;

   code = param_read_float_array(plist, "SpotSize", &ssa);
#  if 0
      fprintf(stderr,
        "rfa-Code = %d, ssa.data = 0x%x, ssa.size = %d, ssa.persistent = %d\n",
        code, (int)ssa.data, ssa.size, (int)ssa.persistent);
#  endif
   if(code == 1) ssa.size = 0; /* unfilled */
   if(code < 0) return code;

   save_info = pdev->color_info;
   set_bpp(pdev, bpp);
   code = gdev_prn_put_params(pdev, plist);
   if ( code < 0 )
     {	pdev->color_info = save_info;
	return code;
     }

   for ( code = 0; (code < ssa.size) && (code < 4); ++code )
     stcolor(pdev)->spotsize[code]  = ssa.data[code];

   stcolor(pdev)->outputcode = outcode;

   if ( bpp != save_info.depth ) {
      if ( pdev->is_open ) gs_closedevice(pdev);
   }
   return 0;
}
