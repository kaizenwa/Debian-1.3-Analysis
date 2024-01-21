/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * readpng.c - interface to pnglib, derived from their example.c
 *              and readjpeg.c.
 */

#ifdef PNG_SUPPORT

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>
#include <setjmp.h>
#include <png.h>
#include "zgv.h"
#include "readpng.h"
#include "readpnm.h"
#include "vgadisp.h"   /* for pixelsize */
#include "rc_config.h"
#include "rcfile.h"



hffunc howfar;

/* we need to use all this stuff from vgadisp.c */
extern int width,height,numcols;
extern byte *theimage;
static byte *pal;

/* our png clean up routine (for zgv.c) needs this */
FILE *global_png_infile;

/* must be global to allow aborting in mid-read */
static png_struct pngstr;
static png_info pnginfo;
static int number_passes,ilheight,dithering;



/* we call this (from zgv.c) if we aborted.
 * we use the setjmpbuf from zgv.c.
 */
int aborted_file_png_cleanup()
{
png_read_destroy(&pngstr,&pnginfo);
free(pal);
fclose(global_png_infile);
}


static int dither_png(howfarfunc,number_passes,ilheight)
hffunc howfarfunc;
int number_passes,ilheight;
{
unsigned char *ptr;
int y;

/* ok, we have 3*width*height allocated (actually 3*width*(height+2),
 * but let's not be pedantic :-)). This currently contains the
 * 24-bit image. If we dither this into the same space, it works
 * ok and doesn't overwrite anything as it goes. We then, for each
 * line, have to copy it into the right place in the new 8-bit image.
 * plus we should call the howfar func. as dithering counts as the
 * 2nd 50% of loading.
 */
for(y=0,ptr=theimage;y<height;y++,ptr+=width*3)
  {
  ditherline(ptr,y,width);
  if(y>0) memcpy(theimage+y*width,ptr,width);
  if(howfarfunc!=NULL) howfarfunc(ilheight+y*number_passes,ilheight*2);
  }

ditherfinish();
pixelsize=1;
}


int read_png_file(filename,howfarfunc,palette)
char *filename;
hffunc howfarfunc;
byte **palette;
{
static FILE *in;
png_struct *png_ptr=&pngstr;
png_info *png_info=&pnginfo;
unsigned char *rowptr;
int f,y,alpha;

theimage=NULL;
if(((*palette)=(byte *)malloc(768))==NULL)
  return(_PICERR_NOMEM);
pal=*palette;

if(setjmp(png_ptr->jmpbuf))
  {
  /* if we get here, there was an error. */
  /* don't use local variables here, they may have been blasted */
  fclose(global_png_infile);	/* this uses the global FILE *. */
  png_read_destroy(&pngstr,&pnginfo);
  if(!cfg.errignore) free(pal);
  if(cfg.errignore && theimage!=NULL && dithering)
    dither_png(howfarfunc,number_passes,ilheight);
  return(_PICERR_PNG_ERR);
  }

png_read_init(png_ptr);
png_info_init(png_info);

if((in=global_png_infile=fopen(filename,"rb"))==NULL)
  return(_PICERR_NOFILE);

png_init_io(png_ptr,in);
png_read_info(png_ptr,png_info);

width=png_info->width;
height=png_info->height;

/* doing this here will result in it allocating enough
 * for the 24-bit image even if will be dithered. This is *required*
 * given the way we have to dither, in case a file is interlaced.
 */
if((png_info->color_type==PNG_COLOR_TYPE_RGB ||
    png_info->color_type==PNG_COLOR_TYPE_RGB_ALPHA) &&
   (pixelsize==1 || cfg.jpeg24bit==0))		/* dither? */
  {
  pixelsize=3;
  if(ditherinit(width)==0)
    return(_PICERR_NOMEM);
  make_332_palette(pal);
  dithering=1;
  }
else
  dithering=0;

/* allocate image memory */
theimage=(byte *)malloc(pixelsize*width*(height+2));
if(theimage==NULL) return(_PICERR_NOMEM);

/* setup transformations */

/* fix to greys if greyscale - this is required */
if(png_info->color_type==PNG_COLOR_TYPE_GRAY ||
   png_info->color_type==PNG_COLOR_TYPE_GRAY_ALPHA ||
   png_info->color_type==PNG_COLOR_TYPE_PALETTE)
  pixelsize=1;

if(png_info->color_type==PNG_COLOR_TYPE_GRAY ||
   png_info->color_type==PNG_COLOR_TYPE_GRAY_ALPHA)
  {
  int numcols=(1<<png_info->bit_depth);

  pixelsize=1;
  for(f=0;f<numcols;f++)
    pal[f]=pal[256+f]=pal[512+f]=(f*255)/(numcols-1);
  }

/* need to be able to set screen gamma */
#if 0
if(png_info->valid & PNG_INFO_gAMA)
  png_set_gamma(png_ptr,screen_gamma,png_info->gamma);
#endif

/* can't handle 16-bit data - cut it down to 8-bit */
if(png_info->bit_depth==16)
  png_set_strip_16(png_ptr);

#if 0
/* this is in the example, not sure if I need it */
if(png_info->bit_depth==1 && png_info->color_type==PNG_TYPE_COLOR_GRAY)
  png_set_invert(png_ptr);
#endif

#if 0
/* unpack to byte-per-pixel */
/* doesn't seem to work, I have to do it by hand :-( */
if(png_info->bit_depth<8)
  png_set_packing(png_ptr);
#endif

/* output BGR if applicable, not RGB */
if(png_info->color_type==PNG_COLOR_TYPE_RGB ||
   png_info->color_type==PNG_COLOR_TYPE_RGB_ALPHA)
  png_set_bgr(png_ptr);

/* handle alpha/transparency by applying to existing contents of buffer */
if(png_info->color_type==PNG_COLOR_TYPE_RGB_ALPHA ||
   png_info->color_type==PNG_COLOR_TYPE_GRAY_ALPHA)
  {
  png_set_alpha(png_ptr);
  alpha=1;
  }
else
  alpha=0;

/* update palette with any transformations */
png_start_read_image(png_ptr);

/* if not 24-bit output and not greyscale, read the palette */
if(pixelsize==1 && png_info->color_type!=PNG_COLOR_TYPE_GRAY
                && png_info->color_type!=PNG_COLOR_TYPE_GRAY_ALPHA)
  for(f=0;f<png_info->num_palette;f++)
    {
    pal[    f]=png_info->palette[f].red;
    pal[256+f]=png_info->palette[f].green;
    pal[512+f]=png_info->palette[f].blue;
    }

if(png_info->interlace_type)
  {
  number_passes=png_set_interlace_handling(png_ptr);
  ilheight=height*number_passes;
  }
else
  ilheight=height,number_passes=1;

if(alpha || cfg.errignore)
  memset(theimage,0,width*(height+1)*pixelsize);

/* read the image */
for(y=0;y<ilheight;y++)
  {
  rowptr=theimage+(y%height)*width*pixelsize;
  /* must use png_read_row as png_read_rows is broken */
  if(cfg.errignore && ilheight>height)	/* fully draw interlace bits */
    png_read_row(png_ptr,NULL,rowptr);
  else
    png_read_row(png_ptr,rowptr,NULL);
  if(alpha) png_combine_row(png_ptr,rowptr,rowptr+width*pixelsize,0xff);
  
  if(howfarfunc!=NULL) howfarfunc(y,dithering?ilheight*2:ilheight);
  }

/* unpack bytes if bpp is 1/2/4; pixelsize is implicitly 1
 * I shouldn't have to do this, y'know :-(
 * must be done after in case of interlace
 */
if(png_info->bit_depth<8)
  for(y=0;y<height;y++)
    unpack_bits(png_info->bit_depth,theimage+y*width,width,height);

/* dither if needed */
if(dithering) dither_png(howfarfunc,number_passes,ilheight);

png_read_end(png_ptr,NULL);
png_read_destroy(png_ptr,png_info);
fclose(in);

return(_PIC_OK);
}


unpack_bits(bit_depth,rowptr,width,height)
int bit_depth;
unsigned char *rowptr;
int width,height;
{
int mask,maskst,f;
unsigned char *srcptr,*dstptr,*bufptr;

srcptr=rowptr; bufptr=dstptr=theimage+width*height;

switch(bit_depth)
  {
  case 1:
    for(f=0;f<width;f++)
      {
      *dstptr++=((*srcptr)&(0x80>>(f&7)))?1:0;
      if((f&7)==7) srcptr++;
      }
    memcpy(rowptr,bufptr,width);
    break;

  case 2:
    maskst=0xc0; mask=maskst;
    for(f=0;f<width;f++)
      {
      *dstptr++=(((*srcptr)&mask)>>((3-(f&3))*2));
      mask>>=2;
      if((f&3)==3) srcptr++,mask=maskst;
      }
    memcpy(rowptr,bufptr,width);
    break;

  case 4:
    maskst=0xf0; mask=maskst;
    for(f=0;f<width;f++)
      {
      *dstptr++=(((*srcptr)&mask)>>(((f&1)^1)*4));
      mask>>=4;
      if(f&1) srcptr++,mask=maskst;
      }
    memcpy(rowptr,bufptr,width);
    break;
  }
}


#endif /* PNG_SUPPORT */
