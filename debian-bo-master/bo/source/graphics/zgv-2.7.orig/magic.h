/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * magic.h
 */

#define _IS_GIF			1
#define _IS_JPEG		2
#define _IS_PNM			3
#define _IS_BMP			4
#define _IS_TGA			5
#define _IS_PNG			6
#define _IS_BAD		 	9	

extern int magic_ident(char *filename);
