#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	Bitmap.c
 * Subroutine:	btn_ByteToBitmap()	returns: unsigned char *
 * Subroutine:	btn_WriteBitmaps()	returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>

#define ROW_WIDTH 12

/*
 * Subroutine:	btn_ByteToBitmap
 * Purpose:	given a Z image return a bitmap image
 */
unsigned char *btn_ByteToBitmap ( image, width, height, byte_width )
     char *image;
     int width, height;
     int *byte_width;
{
  unsigned char *bitmap, *map;
  int wdth, row;
  int line_bit, byte_end;
  unsigned char bit;
  char *btn_Alloc();

  /* allocate the bitmap */
  wdth = (width + 7) / 8;
  bitmap = (unsigned char *)btn_Alloc(wdth * height, 1, "bitmap");
  /* make bitmap one row at a time */
  for( row = 0; row < height; row++ ) {
    map = bitmap + (row * wdth);
    line_bit = 0;
    byte_end = 8;
    /* go through one line */
    while( line_bit < width ) {
      bit = 1;
      if( byte_end > width )
	byte_end = width;
      /* go through one byte */
      while( line_bit < byte_end ) {
	if( *image != 0 )
	  *map |= bit;
	image++;
	line_bit++;
	bit = bit << 1;
      }
      /* advance to next byte */
      byte_end += 8;
      map++;
    }
  }
  /* set byte width and return bitmap */
  *byte_width = wdth;
  return( bitmap );
}

/*
 * Subroutine:	btn_WriteBitmaps
 * Purpose:	write bitmap as array declaration in the file
 */
void
btn_WriteBitmaps ( file, title, ext, icon, mask, width, height, byte_width )
     FILE *file;
     char *title;
     char *ext;
     unsigned char *icon;
     unsigned char *mask;
     int width, height;
     int byte_width;
{
  int byte, row_end, map_end;

  (void)fprintf(file, "#define %s_%s_width %d\n", title, ext, width);
  (void)fprintf(file, "#define %s_%s_height %d\n", title, ext, height);
  (void)fprintf(file, "static char %s_%s_label[] = {\n", title, ext);
  map_end = byte_width * height;
  row_end = ROW_WIDTH;
  byte = 0;
  while( byte < map_end ) {
    (void)fprintf(file, "  0x%02x", icon[byte]);
    if( row_end > map_end )
      row_end = map_end;
    while( ++byte < row_end )
      (void)fprintf(file, ", 0x%02x", icon[byte]);
    if( byte == map_end )
      (void)fprintf(file, " };\n");
    else
      (void)fprintf(file,",\n");
    row_end += ROW_WIDTH;
  }
  if( mask != NULL ) {
    (void)fprintf(file, "static char %s_%s_mask[] = {\n", title, ext);
    row_end = ROW_WIDTH;
    byte = 0;
    while( byte < map_end ) {
      (void)fprintf(file, "  0x%02x", mask[byte]);
      if( row_end > map_end )
	row_end = map_end;
      while( ++byte < row_end )
	(void)fprintf(file, ", 0x%02x", mask[byte]);
      if( byte == map_end )
	(void)fprintf(file, " };\n");
      else
	(void)fprintf(file,",\n");
      row_end += ROW_WIDTH;
    }
  }
}
