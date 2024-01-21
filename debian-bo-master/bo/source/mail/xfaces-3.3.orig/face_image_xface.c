/*                             -*- Mode: C++-C -*- 
 * 
 *		 Copyright 1994 Christopher B. Liebman
 *
 *     Permission to use, copy, modify, distribute, and sell this software
 *     and its documentation for any purpose is hereby granted without fee,
 *     provided that the above copyright notice appear in all copies and that
 *     both that copyright notice and this permission notice appear in
 *     supporting documentation, and that the name Christopher B. Liebman not
 *     be used in advertising or publicity pertaining to distribution of this
 *     software without specific, written prior permission.
 *
 *    THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 *    ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 *    LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 *    PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 *    B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 *    INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 *    PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 *    WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author          : Chris Liebman
 * Created On      : Sun Jan 23 12:03:58 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sun Feb 20 11:56:50 1994
 * Update Count    : 40
 * Status          : Released
 * 
 * HISTORY
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sat Jan 29 23:04:42 1994 #37 (Chris Liebman)
 *    Added new image type struct.
 * 
 * PURPOSE
 * 	Handle X-Face header images.
*/

#ifndef lint
static char *RCSid = "$Id: face_image_xface.c,v 1.3 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"
#include "face_image.h"

typedef struct face_image_xface
{
    Pixmap		pixmap;
    Pixmap		shape;
} FaceImageXFace;


static char*	buffer = NULL;
static int	buffer_size = 0;
static unsigned char bits[512];
static unsigned char rbits[] =
{
    0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
    0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
    0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
    0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
    0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
    0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
    0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
    0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
    0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
    0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
    0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
    0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
    0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
    0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
    0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
    0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
    0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
    0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
    0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
    0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
    0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
    0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
    0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
    0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
    0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
    0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
    0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
    0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
    0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
    0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
    0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
    0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff,
};


/*
 *    Free an xface file.
*/

void
FaceImageXFaceFree(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXFace	*fix = data;
    
    if (fix->pixmap != None)
    {
	XFreePixmap(XtDisplay(TheFrame), fix->pixmap);
    }
    
    if (fix->shape != None)
    {
	XFreePixmap(XtDisplay(TheFrame), fix->shape);
    }
    
    XtFree((void *)fix);
}

/*
 *    Return pixmap for xface.
*/

Pixmap
FaceImageXFacePixmap(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXFace	*fix = data;
    
    return(fix->pixmap);
}

/*
 *    Return shape for xface
*/

Pixmap
FaceImageXFaceShape(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXFace	*fix = data;
    
    return(fix->shape);
}

static FaceImageType FaceImageTypeXFace =
{
    "x-face",
    NULL,
    FaceImageXFaceFree,
    FaceImageXFacePixmap,
    FaceImageXFaceShape,
    NULL,
    NULL,
    NULL,
};

/*
 *    Create an XFace image.
*/

FaceImage*
FaceImageXFaceCreate(str)
String	str;
{
    FaceImage*		image;
    FaceImageXFace*	fix = NULL;
    Pixmap		pixmap;
    int			len;
    int			i;
    int			val;
    char*		p;
    
    if (str == NULL)
    {
	return NULL;
    }
    
    len = strlen(str);
    
    /*
     *   Make sure that buffer is big enough.
    */
    
    if (buffer_size < len)
    {
	if (buffer != NULL)
	{
	    XtFree(buffer);
	}
	
	buffer_size = len + 4096;
	buffer = XtMalloc(buffer_size);
    }
    
    /*
     *  Put header data in the buffer.
    */
    
    strcpy(buffer, str);
    
    /*
     *  Uncompress the face.
    */
    
    if (uncompface(buffer) < 0)
    {
	return NULL;
    }
    
    for (i = 0, p = buffer; i < 144 && p != NULL; ++i)
    {
	if (*p == '\0')
	{
	    return NULL;
	}
	
	/*
	 *   Skip leading spaces.
	*/
	
	while(*p == ' ' || *p == '\t' || *p == '\n')
	{
	    ++p;
	}
	
	/*
	 *   Process one value.
	*/
	
	sscanf(p, "0x%x", &val);
	
	bits[i*2]   = rbits[(val >> 8) & 0xff];
	bits[i*2+1] = rbits[val & 0xff];
	
	/*
	 *  Skip just past the next comma.
	*/
	
	p = index(p, ',');
	++p;
    }
    
    /*
     *   Take the image data and create a bitmap.
    */
    
    pixmap = XCreateBitmapFromData(XtDisplay(TheTopLevel),
				   XtWindow(TheTopLevel),
				   bits, 48, 48);
    
    if (pixmap == None)
    {
	return NULL;
    }
    
    FaceImageColorize(&pixmap, 48, 48);
    
    /*
     *  We got one!  Create a private image struct.
    */
    
    fix = (FaceImageXFace*)XtMalloc(sizeof(FaceImageXFace));
    fix->pixmap = pixmap;
    fix->shape  = None;
    
    image = FaceImageCreate(str, &FaceImageTypeXFace, (void*)fix);
    
    return image;
}
