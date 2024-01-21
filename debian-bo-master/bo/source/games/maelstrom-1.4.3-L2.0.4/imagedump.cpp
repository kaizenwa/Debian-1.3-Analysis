
#include <stdio.h>
#include <string.h>

#include "debug.h"
#include "imagedump.h"


#if IMAGE_FORMAT == IMAGE_GIF
#include "gifencode.cpp"

/* Default to no transparency */
static int transparent = -1;
void SetTransparency(int which)
{
	transparent = which;
}

/* The global image dump function */
void ImageDump(FILE *dumpfp, int width, int height, int ncolors,
				unsigned long *new_colors, unsigned char *data)
{
	int bpp, i;
	int *Red, *Green, *Blue;

	/* Find out how many bits of color required to represent our colors */
	for ( bpp=1, i=2; i < ncolors; ++bpp )
		i *= 2;
#ifdef DEBUG
error("Dump: width = %d, height = %d, ncolors = %d, bpp = %d\n",
						width, height, ncolors, bpp);
#endif

	/* Allocate and fill the colors */
	Red = new int[i]; memset(Red, 0, i);
	Green = new int[i]; memset(Green, 0, i);
	Blue = new int[i]; memset(Blue, 0, i);
	for ( i=0; i<ncolors; ++i ) {
		Red[i] = (new_colors[i]>>16)&0xFF;
		Green[i] = (new_colors[i]>>8)&0xFF;
		Blue[i] = (new_colors[i]&0xFF);
	}
	if ( transparent >= 0 ) {
		for ( i=0; i<ncolors; ++i ) {
			if ( transparent == new_colors[i] ) {
				transparent = i;
				break;
			}
		}
		if ( i == ncolors ) {
			error(
	"Warning: transparent color 0x%.6x not found\n", transparent);
			transparent = -1;
		}
	}
	GIFEncode(dumpfp, width, height, 1, 0, transparent, bpp,
						Red, Green, Blue, data);
}
#endif /* IMAGE_FORMAT is GIF */


#if IMAGE_FORMAT == IMAGE_XPM
static char *xpmchars =
	".#abcdefghijklmnopqrstuvwxyzABCDEFGHIKLMNOPQRSTUVWXYZ0123456789";
const  int  nxpmchars = 64;

/* The global image dump function */
void ImageDump(FILE *dumpfp, int width, int height, int ncolors,
				unsigned long *new_colors, unsigned char *data)
{
	int i, x, len = (width*height);

	/* Output the XPM. :-) */
	fprintf(dumpfp, "/* XPM */\n");
	fprintf(dumpfp, "static char *image[] = {\n");
	fprintf(dumpfp, "/* width height num_colors chars_per_pixel */\n");
	fprintf(dumpfp, "\"   %d    %d        %d          %d\",\n",
				width, height, ncolors, (ncolors/nxpmchars)+1);
	fprintf(dumpfp, "/* colors */\n");
	if ( ncolors > nxpmchars ) {
		for ( i=0; i<ncolors; ++i ) {
			fprintf(dumpfp, "\"%c%c c #%6.6x\",\n",
				xpmchars[i/nxpmchars], xpmchars[i%nxpmchars],
								new_colors[i]);
		}
	} else {
		for ( i=0; i<ncolors; ++i ) {
			fprintf(dumpfp, "\"%c c #%6.6x\",\n",
				xpmchars[i%nxpmchars], new_colors[i]);
		}
	}
	fprintf(dumpfp, "/* pixels */\n");
	if ( ncolors > nxpmchars ) {
		for ( i=0, x=0; i<len; ++i ) {
			if ( x == 0 )
				fprintf(dumpfp, "\"");
			fprintf(dumpfp, "%c%c", xpmchars[data[i]/nxpmchars],
						xpmchars[data[i]%nxpmchars]);
			if ( ++x == width ) {
				fprintf(dumpfp, "\",\n");
				x = 0;
			}
		}
	} else {
		for ( i=0, x=0; i<len; ++i ) {
			if ( x == 0 )
				fprintf(dumpfp, "\"");
			fprintf(dumpfp, "%c", xpmchars[data[i]%nxpmchars]);
			if ( ++x == width ) {
				fprintf(dumpfp, "\",\n");
				x = 0;
			}
		}
	}
	fprintf(dumpfp, "};\n/* A Maelstrom ScreenShot */\n");
}
#endif /* IMAGE_FORMAT is XPM */
