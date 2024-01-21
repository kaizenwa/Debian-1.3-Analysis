/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * vgadisp.h - prototypes for vgadisp.c
 */


/* required for config reading/writing by zgv.c */
/* pixelsize is also reqd. by readjpeg.c */
extern int curvgamode,zoom,virtual,vkludge,brightness,pixelsize;
extern double contrast;

/* reqd. by zgv.c:makexv332() */
extern unsigned char *theimage,*image_palette;
extern int width,height;

extern int pic_incr;	/* for ^p and ^n */

extern int readpicture(char *,hffunc,int);
extern int is_this_file_jpeg();
