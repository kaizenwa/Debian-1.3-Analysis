/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * gifeng.h - prototypes for gifeng.c
 */

extern int readgif(char *,byte **,byte **,hffunc,int);
extern void getgifinfo(PICINFO *);
extern int aborted_file_gif_cleanup();
