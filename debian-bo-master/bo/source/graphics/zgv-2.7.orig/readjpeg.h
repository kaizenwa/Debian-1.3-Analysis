/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * readjpeg.h - external prototypes for readjpeg.c
 */
 
extern int read_JPEG_file(char *,hffunc,byte **);
extern int aborted_file_jpeg_cleanup();
