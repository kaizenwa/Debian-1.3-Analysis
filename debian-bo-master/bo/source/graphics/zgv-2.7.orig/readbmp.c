/* Zgv v2.7 - GIF, JPEG, PNM and BMP viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-5 Russell Marks. See README for license details.
 *
 * readbmp.c - BMP loader.
 *
 * BMP support by Carsten Engelmann (cengelm@gwdg.de).
 * (Based on readpnm.c.)
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include "readbmp.h"
#include "zgv.h"
#include "rcfile.h"
#include "rc_config.h"

static int flip ();

/* for aborted_file_bmp_cleanup() */
static unsigned char *work_bmap, *work_pal;
static FILE *work_in;

int read_bmp_file (filename, howfarfunc, bmap, pal, output_type, pp)
char *filename;
hffunc howfarfunc;
unsigned char **bmap;
unsigned char **pal;
int *output_type;
PICINFO *pp;
{
  FILE *in;
  int i, j;
  int r, g, b;
  int iswindows = 0;
  int dummy, count, done;
  byte *buf;
  byte read[2];
  byte *p;
  byte *ptr, *dptr, *hptr;
  unsigned int w;
  unsigned int h;
  unsigned int palsize;
  BITMAPINFOHEADER bih;
  BITMAPCOREHEADER bch;

  *bmap = NULL;
  *pal = NULL;

  if ((in = fopen (filename, "rb")) == NULL)
    return (_PICERR_NOFILE);

  if ((buf = (byte *) malloc (54)) == NULL)
    return (_PICERR_NOMEM);

  fread (buf, 1, 26, in);

  if (buf[0] != 'B')
    return (_PICERR_BADMAGIC);

  if (buf[1] != 'M')
    return (_PICERR_BADMAGIC);


  memcpy (&bch, buf + 14, 12);

  if (bch.bcSize == 40)		/* truly MS_Windows 3.x ?*/
    {
      fread (buf + 26, 1, 28, in);/* then we need the rest */
      memcpy (&bih, buf + 14, 40);
      iswindows = TRUE;
    }
  else
    iswindows = FALSE;

  if ((p = malloc (768)) == NULL)
    return (_PICERR_NOMEM);

  ptr = p;

  /* only actually used if using 8-bit display, but defined always
	 */
  for (r = 16; r < 256; r += 32)
    for (g = 16; g < 256; g += 32)	/* colours are 3:3:2 */
      for (b = 32; b < 256; b += 64)
	{
	  *ptr = r;
	  ptr[256] = g;
	  ptr[512] = b;
	  ptr++;
	}

  if (iswindows)		/*  MS_Windows 3.x */
    {
      pp->width = w = bih.biWidth;
      pp->height = h = bih.biHeight;
      pp->bpp = bih.biBitCount;

  /* Here the "offbits" -  we need 'em for the
     * palette size - e.g. XV uses different sizes
     */
      palsize = (*(unsigned *) (buf + 10) - 54) / 4;
    }
  else                         /*  OS/2 V1.1       */
    {
      pp->width = w = bch.bcWidth;
      pp->height = h = bch.bcHeight;
      pp->bpp = bch.bcBitCount;
      palsize = (*(unsigned *) (buf + 10) - 26) / 3;
    }

  if (w == 0 || h == 0 || palsize > 256)
    return (_PICERR_CORRUPT);

  if ((pp->bpp >> 3) < 3)
    *output_type = 1;

  /* w is the size of a horizontal line in bytes
   * bih.biWidth is the number of valid pixels in a line
   * the latter one is passed to vgadisp.c as pp->width
   */
  switch (pp->bpp)
    {
    case 1:
      if (w % 32)
        w = (w / 32) * 32 + 32;;
      break;
    case 4:
      if (w % 8)
	w = (w / 8) * 8 + 8;
      break;
    case 8:
      if (w % 4)
	w = (w / 4) * 4 + 4;
      break;
    case 24:
      if ((w * 3) % 4)
	w = (w * 3 / 4) * 4 + 4;
      else
	w = w * 3;
      break;
    default:
      return (_PICERR_CORRUPT);
    }

  if (w == 0 || h == 0)
    return (_PICERR_CORRUPT);

  if ((*pal = malloc (768)) == NULL)
    return (_PICERR_NOMEM);

  if ((pp->bpp == 24) && (*output_type == 3))
    dummy = 3;
  else
    dummy = 1;
  if ((*bmap = malloc (w * (h + 2) * dummy)) == NULL)
    return (_PICERR_NOMEM);
  memset (*bmap, 0, w * (h + 2) * dummy);

  switch (pp->bpp)
    {
    case 1:
      /* 1bit non compressed */
      ptr = *pal;
          fread (ptr , 1, 3, in);
          if (iswindows)
            fread (&dummy, 1, 1, in);
          fread (ptr + 3, 1, 3, in);
          if (iswindows)
            fread (&dummy, 1, 1, in);
      for (i = 0; i < 2; i++)
        {
          p[i] = ptr[3 * i + 2];
          p[i + 256] = ptr[3 * i + 1];
          p[i + 512] = ptr[3 * i];
        }
      free (*pal);
      *pal = p;
      ptr = *bmap;
	for (j = h - 1; j >= 0; j--)
	    for (i = 0, count=0 ; i < (w >> 3); i++)
             {
		hptr = ptr + j * pp->width;
                dummy = fgetc (in);
                if (count < pp->width)
                  {
                    hptr[count] = (dummy & 128)?1:0;count++;
                    hptr[count] = (dummy & 64)?1:0;count++;
                    hptr[count] = (dummy & 32)?1:0;count++;
                    hptr[count] = (dummy & 16)?1:0;count++;
                    hptr[count] = (dummy & 8)?1:0;count++;
                    hptr[count] = (dummy & 4)?1:0;count++;
                    hptr[count] = (dummy & 2)?1:0;count++;
                    hptr[count] = dummy & 1;count++;
                  }
	       if (howfarfunc != NULL)
                    howfarfunc (h-j, h);
              }
        pp->numcols=2;
	break;
    case 4:
      /* 4bit non compressed */
      ptr = *pal;
      for (i = 0; i < palsize; i++)
	{
	  fread (ptr + 3 * i, 1, 3, in);
	  if (iswindows)
	    fread (&dummy, 1, 1, in);
	}
      for (i = 0; i < palsize; i++)
	{
	  p[i] = ptr[3 * i + 2];
	  p[i + 256] = ptr[3 * i + 1];
	  p[i + 512] = ptr[3 * i];
	}
      free (*pal);
      *pal = p;
      ptr = *bmap;
      if ((!iswindows) || (bih.biCompression == 0))
	{
	  for (j = h - 1; j >= 0; j--)
	    for (i = 0, count = 0; i < (w / 2); i++)
	      {
		dummy = fgetc (in);
		if (count < pp->width)
		  {
		    ptr[count + j * pp->width] = dummy >> 4;
		    count++;
		  }
		if (count < pp->width)
		  {
		    ptr[count + j * pp->width] = dummy & 15;
		    count++;
		  }
	      }
	}
      else
	{
	  /* 4bit RLE compressed */
	  done = 0;
	  count = 0;
	  while (done == 0)
	    {
	      fread (read, 1, 2, in);
	      if (*read)
		{
		  i = 0;
		  do
		    {
		      *ptr = read[1] >> 4;
		      ptr++;
		      i++;
		      if (i < (read[0]))
			{
			  *ptr = read[1] & 15;
			  ptr++;
			  i++;
			}
		    }
		  while (i < (*read));
		}
	      else if (read[1] == 0)
		{
		  if (howfarfunc != NULL)
		    howfarfunc (count, h);
		  count++;
		}
	      else if (read[1] == 1)
		done = 1;
	      else if (read[1] == 2)
		{
		  /* This isn't really tested */
		  ptr += fgetc (in) + bih.biWidth * fgetc (in);
		}
	      else
		{
		  dptr = hptr = (byte *) malloc (read[1] >> 1);
		  fread (dptr, 1, read[1] >> 1, in);
		  if (read[1] % 4 > 1)
		    dummy = fgetc (in);
		  i = 0;
		  do
		    {
		      *ptr = (*dptr) >> 4;
		      i++;
		      ptr++;
		      if (i < read[1])
			{
			  *ptr = (*dptr) & 15;
			  i++;
			  ptr++;
			}
		      dptr++;
		    }
		  while (i < read[1]);
		  free (hptr);
		}
	    }
	  if (_PICERR_NOMEM == flip (*bmap, bih.biWidth, bih.biHeight))
	    return (_PICERR_NOMEM);
	}
      pp->width = w;
      pp->numcols= 16;
      break;

    case 8:
      /* 8bit non compressed */
      ptr = *pal;
      for (i = 0; i < palsize; i++)
	{
	  fread (ptr + 3 * i, 1, 3, in);
	  if (iswindows)
	    dummy = fgetc (in);
	}
      for (i = 0; i < palsize; i++)
	{
	  p[i] = ptr[3 * i + 2];
	  p[i + 256] = ptr[3 * i + 1];
	  p[i + 512] = ptr[3 * i];
	}
      free (*pal);
      *pal = p;
      ptr = *bmap;
      if ((!iswindows) || (bih.biCompression == 0))
	for (i = h - 1; i >= 0; i--)
	  {
	    fread (ptr + pp->width * i, 1, w, in);
	    if (howfarfunc != NULL)
	      howfarfunc (h - i, h);
	  }
      else
	/* 8bit RLE compressed */
	{
	  done = 0;
	  count = 0;
	  while (done == 0)
	    {
	      fread (read, 1, 2, in);
	      if (read[0])
		for (i = 0; i < (int) read[0]; i++)
		  {
		    *ptr = read[1];
		    ptr++;
		  }
	      else if (read[1] == 0)
		{
		  if (howfarfunc != NULL)
		    howfarfunc (count, h);
		  count++;
		}
	      else if (read[1] == 1)
		done = 1;
	      else if (read[1] == 2)
		ptr += fgetc (in) + bih.biWidth * fgetc (in);
	      else
		{
		  fread (ptr, 1, read[1], in);
		  if (read[1] % 2)
		    fgetc (in);
		  ptr += read[1];
		}
	    }
	  if (_PICERR_NOMEM == flip (*bmap, bih.biWidth, bih.biHeight))
	    return (_PICERR_NOMEM);
	}
      pp->numcols= 256;
      break;

    case 24:
      /* true color */
      free (*pal);
      *pal = p;			/* XXX Do we need this here ? */
      ptr = *bmap;
      if ((*output_type) == 1)
	{
	  /* Truecolor BMPs and 8-bit display */
	  if (ditherinit (pp->width) == 0)
	    return (_PICERR_NOMEM);
	  for (i = 1; i <= h; i++)
	    {
	      hptr = ptr + pp->width * i;
	      fread (hptr, 1, w, in);
	      ditherline (hptr, i, pp->width);
	      if (howfarfunc != NULL)
		howfarfunc (i, h);
	    }
	  ditherfinish ();
	  if (_PICERR_NOMEM == flip (*bmap, pp->width, pp->height))
	    return (_PICERR_NOMEM);
	  pp->bpp = 8;
          pp->numcols= 256;
	}
      else
	for (i = h - 1; i >= 0; i--)
	  {
	    fread (ptr + pp->width * 3 * i, 1, w, in);
	    if (howfarfunc != NULL)
	      howfarfunc (h - i, h);
	  }
      break;
    }

  /* save stuff in case of abort */
  work_in = in;
  work_bmap = *bmap;
  work_pal = *pal;

  free (buf);
  fclose (in);
  return (_PIC_OK);
}

static int flip (byte * image, unsigned int w, unsigned int h)
{
  unsigned int i;
  byte *hptr;

  if ((hptr = (byte *) malloc (w)) == 0)
    return (_PICERR_NOMEM);
  for (i = 0; i < (h / 2); i++)
    {
      memcpy (hptr, image + i * w, w);
      memcpy (image + i * w, image + (h - i - 1) * w, w);
      memcpy (image + (h - i - 1) * w, hptr, w);
    }
  free (hptr);
  return (_PIC_OK);
}

aborted_file_bmp_cleanup ()
{
  free (work_bmap);
  free (work_pal);
  fclose (work_in);
}
