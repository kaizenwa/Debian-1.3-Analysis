#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	text.c (Text Image)
 * Subroutine:	btn_MakeTextImage()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		24 March 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include <string.h>
#include "btnchars.h"

btn_InitTool ()
{
  /* do nothing (dummy for compatability) */
}

/*
 * Subroutine:	btn_MakeTextImage
 * Purpose:	given a string, return a Z image of the word(s), and some info
 * Note:	unoccupied and unoccupied refers to highlighting when pointer
 *		is on the button, or somewhere else
 */
char *btn_MakeTextImage ( title, width, height, breaklist, breakcnt, sign, in )
     char *title;		/* i: the label string */
     int *width, *height;	/* o: width and height of image */
     int *breaklist;		/* o: array of x offsets of right char edges */
     int *breakcnt;		/* o: number of chars */
     int sign;			/* i: sign is code for rotation */
     int in;			/* i: flag for occupied or unoccupied font */
{
  struct bitfont *textfont;
  char *image;
  char *btn_RotateImage(), *btn_Alloc();
  static int btn_ListTextBreaks();
  static void btn_MakeZImage();

  /* set font for occupation state */
  if( in )
    textfont = intext;
  else
    textfont = outtext;
  /* set list of lengths to breaks between letters */ 
  *breakcnt = btn_ListTextBreaks(title, textfont, breaklist);
  /* determine size of full text label */
  *width = breaklist[*breakcnt - 1];
  *height = textfont->height;
  image = btn_Alloc(*width * *height, 1, "text image");
  /* create the label image (use Z image, it's easier to rotate) */
  btn_MakeZImage(title, *breakcnt, textfont, breaklist,
		 *width, *height, image);
  /* rotate if desired */
  if( sign != 0 )
    image = btn_RotateImage(image, *width, *height, sign);
  return( image );
}

/*
 * Subroutine:	btn_ListTextBreaks
 * Purpose:	create list of x offsets of breaks between letter in label
 */
static int btn_ListTextBreaks ( title, textfont, breaklist )
     char *title;
     struct bitfont *textfont;
     int *breaklist;
{
  unsigned char *alpha;
  int len, cnt, i;

  cnt = strlen(title);
  alpha = (unsigned char *)title;
  len = 0;
  for( i=0; i<cnt; i++ ) {
    len += textfont[(int)alpha[i]].width;
    breaklist[i] = len;
  }
  return( cnt );
}

/*
 * Subroutine:	btn_MakeZImage
 * Purpose:	make 8 bit deep image (with only 1's and 0's) of label
 */
static void btn_MakeZImage ( title, cnt, textfont, breaklist,
			     width, height, image )
     char *title;
     int cnt;
     struct bitfont *textfont;
     int *breaklist;
     int width;
     int height;
     char *image;
{
  unsigned char *alpha;
  int byte, bit;
  int x, i;
  static void add_one_alpha();

  alpha = (unsigned char *)title;
  x = 0;
  for( i=0; i<cnt; i++ ) {
    add_one_alpha(&textfont[alpha[i]], x, width, height, image);
    x = breaklist[i];
  }
}

/*
 * Subroutine:	add_one_alpha
 * Purpose:	add one letter to label Z image
 */
static void add_one_alpha ( alpha, x, width, height, image )
     struct bitfont *alpha;
     int x;
     int width, height;
     char *image;
{
  int bit;
  int y, ycnt;
  int byte;
  char *image_char;
  char *form;
  char mask;

  form = alpha->bits;
  if( height == alpha->height ) {
    image += x;
    ycnt = height;
  } else {
    if( height > alpha->height ) {
      ycnt = alpha->height;
      /* character is taller than the image */
      form += (height - ycnt) * alpha->byte_width;
    } else {
      /* character is shorter than image */
      ycnt = height;
      image += ((alpha->height - height) * width) + x;
    }
  }
  for( y=0; y<ycnt; y++ ) {
    image_char = image;
    for( byte=0; byte<alpha->byte_width; byte++ ) {
      for( bit=0; bit<8; bit++ ) {
	mask = 1 << bit;
        if( *form & mask )
	  *image_char = 1;
	++image_char;
      }
      ++form;
    }
    image += width;
  }
}
