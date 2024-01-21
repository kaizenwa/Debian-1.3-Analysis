/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * readpng.h - external prototypes for readjpeg.c
 */

#ifdef PNG_SUPPORT
extern int read_png_file(char *,hffunc,byte **);
extern int aborted_file_png_cleanup();
#endif
