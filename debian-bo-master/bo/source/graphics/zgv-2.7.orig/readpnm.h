/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * readpnm.h
 */

extern int read_pnm_file(char *filename,hffunc howfarfunc,
	unsigned char **bmap,unsigned char **pal,int *output_type,
	PICINFO *pp);
extern int read_xv332(char *filename,unsigned char **bmap,
	int *width,int *height);

extern int ditherinit(int);
extern int ditherfinish();
extern int ditherline(unsigned char *,int,int);
extern int make_332_palette(unsigned char *palptr);

extern int aborted_file_pnm_cleanup();
