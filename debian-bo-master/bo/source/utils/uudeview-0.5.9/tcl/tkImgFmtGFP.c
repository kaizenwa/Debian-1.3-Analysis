/*
 * tkImgFmtGFP.c --
 *
 * Handler for GFP photo image strings. I wanted to read an inline GIF file
 * (from a string), but couldn't do because GIF files may contain zeroes.
 * So I invented the GFP format :-) They're identical with GIF files with
 * only the following differences:
 *
 *  - a Magic of GFP87a or GFP89a
 *  - all 0x00 are replaced by 0x01 0x01
 *  - all 0x01 are replaced by 0x01 0x02
 *  - all 0x20 are replaced by 0x01 0x21
 *
 * The latter is done because all backslash-newline sequences in a string
 * are interpreted as a space. This way, all spaces in the string can be
 * ignored
 *
 * There's also a little converter, gif2gfp, that reads a gif file
 * and writes a tcl string with the gfp data.
 *
 * Frank Pilhofer, fp@informatik.uni-frankfurt.de, March 1996
 *
 * I don't claim any copyrights, use the original statement below. Also,
 * this file shouldn't have any bugs that the gif code hadn't.
 *
 * ------
 * 
 * A photo image file handler for GIF files. Reads 87a and 89a GIF files.
 * At present there is no write function.
 *
 * Derived from the giftoppm code found in the pbmplus package 
 * and tkImgFmtPPM.c in the tk4.0b2 distribution by -
 *
 * Reed Wade (wade@cs.utk.edu), University of Tennessee
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * This file also contains code from the giftoppm program, which is
 * copyrighted as follows:
 *
 * +-------------------------------------------------------------------+
 * | Copyright 1990, David Koblas.                                     |
 * |   Permission to use, copy, modify, and distribute this software   |
 * |   and its documentation for any purpose and without fee is hereby |
 * |   granted, provided that the above copyright notice appear in all |
 * |   copies and that both that copyright notice and this permission  |
 * |   notice appear in supporting documentation.  This software is    |
 * |   provided "as is" without express or implied warranty.           |
 * +-------------------------------------------------------------------+
 */

/*
 * header definitions for my own project
 */

#if 1
#define PROJECT_UUDEVIEW
#endif

/*
 * only define if this version of Tk has the necessary hooks
 */

#ifdef PROJECT_UUDEVIEW

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#else

#ifndef TK_PHOTO
#define TK_PHOTO
#endif

#endif

#ifdef TK_PHOTO

static char sccsid[] = "@(#) tkImgFmtGFP.c 1.3 96/03/10 22:49:02";

#ifdef PROJECT_UUDEVIEW

#ifdef SYSTEM_WINDLL
#include <windows.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <tk.h>

#else

#include "tkInt.h"
#include "tkPort.h"

#endif

#ifdef __WATCOMC__
#pragma aux (__cdecl) Tcl_Eval
#pragma aux (__cdecl) Tcl_GetVar
#pragma aux (__cdecl) Tcl_SetVar
#pragma aux (__cdecl) Tcl_AppendResult
#pragma aux (__cdecl) Tcl_SetResult
#pragma aux (__cdecl) Tcl_CreateCommand
#pragma aux (__cdecl) Tcl_PosixError
#pragma aux (__cdecl) Tk_CreatePhotoImageFormat
#pragma aux (__cdecl) Tk_GetColor
#pragma aux (__cdecl) Tk_PhotoExpand
#pragma aux (__cdecl) Tk_PhotoPutBlock
#pragma aux (__cdecl) Tk_MainWindow
#pragma aux (__cdecl) Tk_GetUid
#pragma aux (__cdecl) Tk_FreeColor
#endif

/*
 * The format record for the GFP file format:
 */

static int UUTCLFUNC StringMatchGFP _ANSI_ARGS_((char *string,
		    char *formatString, int *widthPtr, int *heightPtr));
static int UUTCLFUNC StringReadGFP  _ANSI_ARGS_((Tcl_Interp *interp,
		    char *string, char *formatString,
		    Tk_PhotoHandle imageHandle, int destX, int destY,
		    int width, int height, int srcX, int srcY));

static Tk_PhotoImageFormat tkImgFmtGFP = {
	"GFP",			/* name */
	NULL,           /* fileMatchProc */
	StringMatchGFP, /* stringMatchProc */
	NULL,           /* fileReadProc */
	StringReadGFP,  /* stringReadProc */
	NULL,           /* fileWriteProc */
	NULL,           /* stringWriteProc */
};

#define INTERLACE		0x40
#define LOCALCOLORMAP		0x80
#define BitSet(byte, bit)	(((byte) & (bit)) == (bit))
#define MAXCOLORMAPSIZE		256
#define CM_RED			0
#define CM_GREEN		1
#define CM_BLUE			2
#define MAX_LWZ_BITS		12
#define LM_to_uint(a,b)         (((b)<<8)|(a))

/* #define ReadOK(file,buffer,len)	(fread(buffer, len, 1, file) != 0) */

/*
 * I should be burned at stake for this macro
 */

#define GB(s) (((*(s)==32)?*(s)++:0),(*(s)?((*(s)==1)?(s)++,(*(s)++)-1:*(s)++):*(s)))

static int
ReadOK(s, buffer, len)
	char **s;
	unsigned char *buffer;
	int len;
{
  while (len--)
    *buffer++ = GB(*s);

  return **s;
}

/*
 * Prototypes for local procedures defined in this file:
 */

static int		DoExtension _ANSI_ARGS_((char **s, int label,
			    int *transparent));
static int		GetCode _ANSI_ARGS_((char **s, int code_size,
			    int flag));
static int		GetDataBlock _ANSI_ARGS_((char **s,
			    unsigned char *buf));
static int		LWZReadByte _ANSI_ARGS_((char **s, int flag,
			    int input_code_size));
static int		ReadColorMap _ANSI_ARGS_((char **s, int number,
			    unsigned char buffer[3][MAXCOLORMAPSIZE]));
static int		ReadGFPHeader _ANSI_ARGS_((char **s, int *widthPtr,
			    int *heightPtr));
static int		ReadImage _ANSI_ARGS_((Tcl_Interp *interp,
			    char *imagePtr, char **s, int len, int height,
			    unsigned char cmap[3][MAXCOLORMAPSIZE],
			    int interlace, int transparent));

/*
 *----------------------------------------------------------------------
 *
 * FileMatchGFP --
 *
 *  This procedure is invoked by the photo image type to see if
 *  a file contains image data in GFP format.
 *
 * Results:
 *  The return value is 1 if the first characters in file f look
 *  like GFP data, and 0 otherwise.
 *
 * Side effects:
 *  The access position in f may change.
 *
 *----------------------------------------------------------------------
 */

static int UUTCLFUNC
StringMatchGFP(s, formatString, widthPtr, heightPtr)
    char *s;			/* The image file, open for reading. */
    char *formatString;		/* User-specified format string, or NULL. */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here if the file is a valid
				 * raw GFP file. */
{
	return ReadGFPHeader(&s, widthPtr, heightPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * FileReadGFP --
 *
 *	This procedure is called by the photo image type to read
 *	GFP format data from a file and write it into a given
 *	photo image.
 *
 * Results:
 *	A standard TCL completion code.  If TCL_ERROR is returned
 *	then an error message is left in interp->result.
 *
 * Side effects:
 *	The access position in file f is changed, and new data is
 *	added to the image given by imageHandle.
 *
 *----------------------------------------------------------------------
 */

static int UUTCLFUNC
StringReadGFP(interp, s, formatString, imageHandle, destX, destY,
	width, height, srcX, srcY)
    Tcl_Interp *interp;		/* Interpreter to use for reporting errors. */
    char *s;			/* The image file, open for reading. */
    char *formatString;		/* User-specified format string, or NULL. */
    Tk_PhotoHandle imageHandle;	/* The photo image to write into. */
    int destX, destY;		/* Coordinates of top-left pixel in
				 * photo image to be written to. */
    int width, height;		/* Dimensions of block of photo image to
				 * be written to. */
    int srcX, srcY;		/* Coordinates of top-left pixel to be used
				 * in image being read. */
{
    int fileWidth, fileHeight;
    int nBytes;
    Tk_PhotoImageBlock block;
    unsigned char buf[100];
    int bitPixel;
    unsigned int colorResolution;
    unsigned int background;
    unsigned int aspectRatio;
    unsigned char localColorMap[3][MAXCOLORMAPSIZE];
    unsigned char colorMap[3][MAXCOLORMAPSIZE];
    int useGlobalColormap;
    int transparent = -1;

    if (!ReadGFPHeader(&s, &fileWidth, &fileHeight)) {
	Tcl_AppendResult(interp, "couldn't read GFP header from string",
			 NULL);
	return TCL_ERROR;
    }
    if ((fileWidth <= 0) || (fileHeight <= 0)) {
	Tcl_AppendResult(interp, "GFP image file has dimension(s) <= 0",
			 NULL);
	return TCL_ERROR;
    }

    buf[0] = GB(s);
    buf[1] = GB(s);
    buf[2] = GB(s);

    bitPixel = 2<<(buf[0]&0x07);
    colorResolution = (((buf[0]&0x70)>>3)+1);
    background = buf[1];
    aspectRatio = buf[2];

    if (BitSet(buf[0], LOCALCOLORMAP)) {    /* Global Colormap */
	if (!ReadColorMap(&s, bitPixel, colorMap)) {
	    Tcl_AppendResult(interp, "error reading color map",
		    (char *) NULL);
	    return TCL_ERROR;
	}
    }

    if ((srcX + width) > fileWidth) {
	width = fileWidth - srcX;
    }
    if ((srcY + height) > fileHeight) {
	height = fileHeight - srcY;
    }
    if ((width <= 0) || (height <= 0)
	    || (srcX >= fileWidth) || (srcY >= fileHeight)) {
	return TCL_OK;
    }

    Tk_PhotoExpand(imageHandle, destX + width, destY + height);

    block.width = fileWidth;
    block.height = fileHeight;
    block.pixelSize = 3;
    block.pitch = 3 * fileWidth;
    block.offset[0] = 0;
    block.offset[1] = 1;
    block.offset[2] = 2;
    nBytes = fileHeight * block.pitch;
    block.pixelPtr = (unsigned char *) ckalloc((unsigned) nBytes);

    while (1) {
        buf[0] = GB(s);
	if (!*s) {
	    /*
	     * Premature end of image.  We should really notify
	     * the user, but for now just show garbage.
	     */

	    break;
	}

	if (buf[0] == ';') {
	    /*
	     * GFP terminator.
	     */

	    break;
	}

	if (buf[0] == '!') {
	    /*
	     * This is a GFP extension.
	     */
	    buf[0]=GB(s);
	    if (!*s) {
		interp->result =
			"error reading extension function code in GFP image";
		goto error;
	    }
	    if (DoExtension(&s, buf[0], &transparent) < 0) {
		interp->result = "error reading extension in GFP image";
		goto error;
	    }
	    continue;
	}

	if (buf[0] != ',') {
	    /*
	     * Not a valid start character; ignore it.
	     */
	    continue;
	}

	buf[0] = GB(s); buf[1] = GB(s); buf[2] = GB(s);
	buf[3] = GB(s); buf[4] = GB(s); buf[5] = GB(s);
	buf[6] = GB(s); buf[7] = GB(s); buf[8] = GB(s);

	if (!*s) {
	    interp->result = "couldn't read left/top/width/height in GFP image";
	    goto error;
	}

	useGlobalColormap = ! BitSet(buf[8], LOCALCOLORMAP);

	bitPixel = 1<<((buf[8]&0x07)+1);

	if (!useGlobalColormap) {
	    if (!ReadColorMap(&s, bitPixel, localColorMap)) {
		    Tcl_AppendResult(interp, "error reading color map", 
			    (char *) NULL);
		    goto error;
	    }
	    if (ReadImage(interp, (char *) block.pixelPtr, &s, fileWidth,
		    fileHeight, localColorMap, BitSet(buf[8], INTERLACE),
		    transparent) != TCL_OK) {
		goto error;
	    }
	} else {
	    if (ReadImage(interp, (char *) block.pixelPtr, &s, fileWidth,
		    fileHeight, colorMap, BitSet(buf[8], INTERLACE),
		    transparent) != TCL_OK) {
		goto error;
	    }
	}

    }

    Tk_PhotoPutBlock(imageHandle, &block, destX, destY, fileWidth, fileHeight);
    ckfree((char *) block.pixelPtr);
    return TCL_OK;

    error:
    ckfree((char *) block.pixelPtr);
    return TCL_ERROR;

}

/*
 *----------------------------------------------------------------------
 *
 * ReadGFPHeader --
 *
 *	This procedure reads the GFP header from the beginning of a
 *	GFP file and returns the dimensions of the image.
 *
 * Results:
 *	The return value is 1 if file "f" appears to start with
 *	a valid GFP header, 0 otherwise.  If the header is valid,
 *	then *widthPtr and *heightPtr are modified to hold the
 *	dimensions of the image.
 *
 * Side effects:
 *	The access position in f advances.
 *
 *----------------------------------------------------------------------
 */

static int
ReadGFPHeader(s, widthPtr, heightPtr)
    char **s;			/* Image file to read the header from */
    int *widthPtr, *heightPtr;	/* The dimensions of the image are
				 * returned here. */
{
    unsigned char buf[7];

    buf[0] = GB(*s); buf[1] = GB(*s); buf[2] = GB(*s);
    buf[3] = GB(*s); buf[4] = GB(*s); buf[5] = GB(*s);

    if ((!**s)
	    || ((strncmp("GFP87a", (char *) buf, 6) != 0)
	    && (strncmp("GFP89a", (char *) buf, 6) != 0))) {
	return 0;
    }

    buf[0] = GB(*s); buf[1] = GB(*s); buf[2] = GB(*s);
    buf[3] = GB(*s);

    *widthPtr = LM_to_uint(buf[0],buf[1]);
    *heightPtr = LM_to_uint(buf[2],buf[3]);
    return 1;
}

/*
 *-----------------------------------------------------------------
 * The code below is copied from the GFPtoppm program and modified
 * just slightly.
 *-----------------------------------------------------------------
 */

static int
ReadColorMap(s,number,buffer)
char    **s;
int     number;
unsigned char   buffer[3][MAXCOLORMAPSIZE];
{
	int     i;
	unsigned char   rgb[3];

	for (i = 0; i < number; ++i) {
		if (! ReadOK(s, rgb, sizeof(rgb)))
			return 0;

		buffer[CM_RED][i] = rgb[0] ;
		buffer[CM_GREEN][i] = rgb[1] ;
		buffer[CM_BLUE][i] = rgb[2] ;
	}
	return 1;
}



static int
DoExtension(s, label, transparent)
char    **s;
int label;
int	*transparent;
{
	static unsigned char buf[256];
	int count = 0;

	switch (label) {
		case 0x01:      /* Plain Text Extension */
			break;

		case 0xff:      /* Application Extension */
			break;

		case 0xfe:      /* Comment Extension */
			do {
				count = GetDataBlock(s, (unsigned char*) buf);
			} while (count > 0);
			return count;

		case 0xf9:      /* Graphic Control Extension */
			count = GetDataBlock(s, (unsigned char*) buf);
			if (count < 0) {
				return 1;
			}
			if ((buf[0] & 0x1) != 0) {
				*transparent = buf[3];
			}

			do {
			    count = GetDataBlock(s, (unsigned char*) buf);
			} while (count > 0);
			return count;
	}

	do {
	    count = GetDataBlock(s, (unsigned char*) buf);
	} while (count > 0);
	return count;
}

static int ZeroDataBlock = 0;

static int
GetDataBlock(s, buf)
char   **s;
unsigned char   *buf;
{
	unsigned char   count;

	if (! ReadOK(s,&count,1)) {
		return -1;
	}

	ZeroDataBlock = count == 0;

	if ((count != 0) && (! ReadOK(s, buf, count))) {
		return -1;
	}

	return count;
}


static int
ReadImage(interp, imagePtr, s, len, height, cmap, interlace, transparent)
Tcl_Interp *interp;
char 	*imagePtr;
char    **s;
int len, height;
unsigned char   cmap[3][MAXCOLORMAPSIZE];
int interlace;
int transparent;
{
	unsigned char   c;
	int     v;
	int     xpos = 0, ypos = 0, pass = 0;
	char 	*colStr;


	/*
	 *  Initialize the Compression routines
	 */
	if (! ReadOK(s,&c,1))  {
	    Tcl_AppendResult(interp, "error reading GFP image: ",
		    Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}

	if (LWZReadByte(s, 1, c) < 0) {
	    interp->result = "format error in GFP image";
	    return TCL_ERROR;
	}

	if (transparent!=-1 && 
		(colStr = Tcl_GetVar(interp, "TRANSPARENT_GFP_COLOR", 0L))) {
		XColor *colorPtr;
		colorPtr = Tk_GetColor(interp, Tk_MainWindow(interp), 
							  Tk_GetUid(colStr));
		if (colorPtr) {
/*
			printf("color is %d %d %d\n", 
					colorPtr->red >> 8, 
					colorPtr->green >> 8, 
					colorPtr->blue >> 8);
*/
			cmap[CM_RED][transparent] = colorPtr->red >> 8;
			cmap[CM_GREEN][transparent] = colorPtr->green >> 8;
			cmap[CM_BLUE][transparent] = colorPtr->blue >> 8;
			Tk_FreeColor(colorPtr);
		}
	}

	while ((v = LWZReadByte(s,0,c)) >= 0 ) {

		imagePtr[ (xpos*3)  +  (ypos *len*3)] = cmap[CM_RED][v];
		imagePtr[ (xpos*3)  +  (ypos *len*3) +1] = cmap[CM_GREEN][v];
		imagePtr[ (xpos*3)  +  (ypos *len*3) +2] = cmap[CM_BLUE][v];

		++xpos;
		if (xpos == len) {
			xpos = 0;
			if (interlace) {
				switch (pass) {
					case 0:
					case 1:
						ypos += 8; break;
					case 2:
						ypos += 4; break;
					case 3:
						ypos += 2; break;
				}

				if (ypos >= height) {
					++pass;
					switch (pass) {
						case 1:
							ypos = 4; break;
						case 2:
							ypos = 2; break;
						case 3:
							ypos = 1; break;
						default:
							return TCL_OK;
					}
				}
			} else {
				++ypos;
			}
		}
		if (ypos >= height)
			break;
	}
	return TCL_OK;
}

static int
LWZReadByte(s, flag, input_code_size)
char   **s;
int flag;
int input_code_size;
{
	static int  fresh = 0;
	int     code, incode;
	static int  code_size, set_code_size;
	static int  max_code, max_code_size;
	static int  firstcode, oldcode;
	static int  clear_code, end_code;
	static int  table[2][(1<< MAX_LWZ_BITS)];
	static int  stack[(1<<(MAX_LWZ_BITS))*2], *sp;
	register int    i;


	if (flag) {

		set_code_size = input_code_size;
		code_size = set_code_size+1;
		clear_code = 1 << set_code_size ;
		end_code = clear_code + 1;
		max_code_size = 2*clear_code;
		max_code = clear_code+2;

		GetCode(s, 0, 1);

		fresh = 1;

		for (i = 0; i < clear_code; ++i) {
			table[0][i] = 0;
			table[1][i] = i;
		}
		for (; i < (1<<MAX_LWZ_BITS); ++i) {
			table[0][i] = table[1][0] = 0;
		}

		sp = stack;

		return 0;

	} else if (fresh) {

		fresh = 0;
		do {
			firstcode = oldcode = GetCode(s, code_size, 0);
		} while (firstcode == clear_code);
		return firstcode;
	}

	if (sp > stack)
		return *--sp;

	while ((code = GetCode(s, code_size, 0)) >= 0) {
		if (code == clear_code) {
			for (i = 0; i < clear_code; ++i) {
				table[0][i] = 0;
				table[1][i] = i;
			}

			for (; i < (1<<MAX_LWZ_BITS); ++i) {
				table[0][i] = table[1][i] = 0;
			}

			code_size = set_code_size+1;
			max_code_size = 2*clear_code;
			max_code = clear_code+2;
			sp = stack;
			firstcode = oldcode = GetCode(s, code_size, 0);
			return firstcode;

	} else if (code == end_code) {
		int     count;
		unsigned char   buf[260];

		if (ZeroDataBlock)
			return -2;

		while ((count = GetDataBlock(s, buf)) > 0)
			;

		if (count != 0)
			return -2;
	}

	incode = code;

	if (code >= max_code) {
		*sp++ = firstcode;
		code = oldcode;
	}

	while (code >= clear_code) {
		*sp++ = table[1][code];
		if (code == table[0][code])
			printf("circular table entry BIG ERROR\n");
		code = table[0][code];
	}

	*sp++ = firstcode = table[1][code];

	if ((code = max_code) <(1<<MAX_LWZ_BITS)) {

		table[0][code] = oldcode;
		table[1][code] = firstcode;
		++max_code;
		if ((max_code>=max_code_size) && (max_code_size < (1<<MAX_LWZ_BITS))) {
			max_code_size *= 2;
			++code_size;
		}
	}

	oldcode = incode;

	if (sp > stack)
		return *--sp;
	}
	return code;
}


static int
GetCode(s, code_size, flag)
char    **s;
int code_size;
int flag;
{
	static unsigned char    buf[280];
	static int      curbit, lastbit, done, last_byte;
	int         i, j, ret;
	unsigned char       count;

	if (flag) {
		curbit = 0;
		lastbit = 0;
		done = 0;
		return 0;
	}


	if ( (curbit+code_size) >= lastbit) {
		if (done) {
			/* ran off the end of my bits */
			return -1;
		}
		buf[0] = buf[last_byte-2];
		buf[1] = buf[last_byte-1];

		if ((count = GetDataBlock(s, &buf[2])) == 0)
			done = 1;

		last_byte = 2 + count;
		curbit = (curbit - lastbit) + 16;
		lastbit = (2+count)*8 ;
	}

	ret = 0;
	for (i = curbit, j = 0; j < code_size; ++i, ++j)
		ret |= ((buf[ i / 8 ] & (1 << (i % 8))) != 0) << j;


	curbit += code_size;

	return ret;
}

/*
 * Initialization procedure. Register our Image format
 */

int
Gfp_Init (Tcl_Interp *interp)
{
  static int initcounter=0;

  if (initcounter++ == 0)
    Tk_CreatePhotoImageFormat (&tkImgFmtGFP);

  return TCL_OK;
}

#endif
