/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-5 Russell Marks. See README for license details.
 *
 * readbmp.h - header for readbmp.c.
 *
 * BMP support by Carsten Engelmann (cengelm@gwdg.de)
 */

#define TRUE 1
#define FALSE 0

/* BITMAPFILEHEADER
 *
 * Bitmap File Information
 *
 * The BITMAPFILEHEADER data structure contains information about the type,
 * size, and layout of a device-independent bitmap (DIB) file.
 */
typedef struct BITMAPFILEHEADER {
        short   bfType;
        int     bfSize;
        short   bfReserved1;
        short   bfReserved2;
        int     bfOffBits;
} BITMAPFILEHEADER;

typedef struct BITMAPINFOHEADER{
	unsigned int  biSize;
   	unsigned int  biWidth;
   	unsigned int  biHeight;
   	unsigned short   biPlanes;
   	unsigned short   biBitCount;
   	unsigned int  biCompression;
   	unsigned int  biSizeImage;
   	unsigned int  biXPelsPerMeter;
   	unsigned int  biYPelsPerMeter;
   	unsigned int  biClrUsed;
   	unsigned int  biClrImportant;
} BITMAPINFOHEADER;

typedef struct BITMAPCOREHEADER{
        unsigned int  bcSize;
        unsigned short  bcWidth;
        unsigned short  bcHeight;
        unsigned short   bcPlanes;
        unsigned short   bcBitCount;
} BITMAPCOREHEADER;

extern int aborted_file_bmp_cleanup();
extern int read_bmp_file();
