/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * readjpeg.c - interface to the IJG's JPEG software, derived from
 *               their example.c.
 */

#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <setjmp.h>
#include <sys/file.h>  /* for open et al */
#include "3deffects.h"
#include <jpeglib.h>
#include "zgv.h"
#include "readjpeg.h"
#include "vgadisp.h"   /* for pixelsize */
#include "rc_config.h"
#include "rcfile.h"



hffunc howfar;

/* and we need to use all this stuff from vgadisp.c */
extern int width,height,numcols;
extern byte *theimage;
byte *pal;

/* our jpeg clean up routine (for zgv.c) needs this */
FILE *global_jpeg_infile;

/* The way that libjpeg v5 works means that more-or-less *has* to
 * global if I want to allow aborting in mid-read. Oh well. :-(
 */
static struct jpeg_decompress_struct cinfo;

/* stuff for error routines */
struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */
  jmp_buf setjmp_buffer;	/* for return to caller */
  };

typedef struct my_error_mgr * my_error_ptr;



jpegerr(msgtext)
char *msgtext;
{
char buf[256];

strcpy(buf,"Error reading JPEG - ");
strcat(buf,msgtext);
if(cfg.onefile_progress)
  fprintf(stderr,buf);
else
  msgbox(zgv_ttyfd,buf,MSGBOXTYPE_OK, idx_light,idx_dark,idx_black);
}


METHODDEF void
my_error_exit(j_common_ptr cinfo)
{
my_error_ptr myerr=(my_error_ptr) cinfo->err;
char buf[JMSG_LENGTH_MAX];

(*cinfo->err->format_message)(cinfo,buf);

jpegerr(buf);	/* report the error message */

/* cleanup is done after jump back, so just do that now... */
longjmp(myerr->setjmp_buffer, 1);
}


/* No warning messages */
METHODDEF void
my_output_message(j_common_ptr cinfo)
{
}


/* different error_exit... we call this (from zgv.c) if we aborted from
 * reading a JPEG. (we asked vgadisp.c, via is_this_file_jpeg(), whether it
 * was a JPEG or not.)
 * we use the setjmpbuf from zgv.c.
 */
int aborted_file_jpeg_cleanup()
{
jpeg_destroy_decompress(&cinfo);
free(pal);
fclose(global_jpeg_infile);
}



int read_JPEG_file(filename,howfarfunc,palette)
char *filename;
hffunc howfarfunc;
byte **palette;
{
static FILE *in;
struct my_error_mgr jerr;
int row_stride;		/* physical row width in output buffer */
int tmp,f;
unsigned char *ptr;

theimage=NULL;
howfar=howfarfunc;
if(((*palette)=(byte *)malloc(768))==NULL)
  return(_PICERR_NOMEM);
pal=*palette;

if((in=global_jpeg_infile=fopen(filename,"rb"))==NULL)
  return(_PICERR_NOFILE);

cinfo.err=jpeg_std_error(&jerr.pub);
jerr.pub.error_exit=my_error_exit;
jerr.pub.output_message=my_output_message;

if(setjmp(jerr.setjmp_buffer))
  {
  jpeg_destroy_decompress(&cinfo);
  fclose(global_jpeg_infile);	/* this uses the global FILE *. */
  free(pal);
  return(_PICERR_CORRUPT);	/* could actually be anything... */
  }

/* Now we can initialize the JPEG decompression object. */
jpeg_create_decompress(&cinfo);

jpeg_stdio_src(&cinfo,in);	/* indicate source is the file */

jpeg_read_header(&cinfo,TRUE);

/* setup parameters for decompression */

switch(cfg.jpegspeed)
  {
  case 1:  cinfo.dct_method=JDCT_FLOAT; break;
  case 3:  cinfo.dct_method=JDCT_IFAST; break;
  default: cinfo.dct_method=JDCT_ISLOW;
  }

if(pixelsize==1)	/* if our preference is for 8-bit... */
  {
  cinfo.quantize_colors=TRUE;
  cinfo.desired_number_of_colors=256;
  cinfo.two_pass_quantize=TRUE;
  }

/* fix to greys if greyscale - this is required to read greyscale JPEGs */
if(cinfo.jpeg_color_space==JCS_GRAYSCALE)
  {
  cinfo.out_color_space=JCS_GRAYSCALE;
  cinfo.desired_number_of_colors=256;
  cinfo.quantize_colors=FALSE;
  cinfo.two_pass_quantize=FALSE;
  pixelsize=1;
  for(f=0;f<256;f++)
    pal[f]=pal[256+f]=pal[512+f]=f;
  }

width=cinfo.image_width;
height=cinfo.image_height;
theimage=(byte *)malloc(pixelsize*width*height);
if(theimage==NULL)
  {
  jpegerr("Out of memory");
  longjmp(jerr.setjmp_buffer,1);
  }


jpeg_start_decompress(&cinfo);

/* read the palette (if greyscale, this has already been done) */
if(pixelsize==1 && cinfo.jpeg_color_space!=JCS_GRAYSCALE)
  for(f=0;f<cinfo.actual_number_of_colors;f++)
    {
    pal[    f]=cinfo.colormap[0][f];
    pal[256+f]=cinfo.colormap[1][f];
    pal[512+f]=cinfo.colormap[2][f];
    }

/* read the image */
ptr=theimage; row_stride=pixelsize*width;
if(pixelsize==1)
  while(cinfo.output_scanline<height)
    {
    jpeg_read_scanlines(&cinfo,&ptr,1);
    ptr+=row_stride;
    if(howfar!=NULL) howfar(cinfo.output_scanline,height);
    }
else
  while(cinfo.output_scanline<height)
    {
    jpeg_read_scanlines(&cinfo,&ptr,1);
    for(f=0;f<width;f++) { tmp=*ptr; *ptr=ptr[2]; ptr[2]=tmp; ptr+=3; }
    if(howfar!=NULL) howfar(cinfo.output_scanline,height);
    }

jpeg_finish_decompress(&cinfo);
jpeg_destroy_decompress(&cinfo);
fclose(in);

/* XXX: At this point you may want to check to see whether any corrupt-data
 * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
 */

return(_PIC_OK);
}
