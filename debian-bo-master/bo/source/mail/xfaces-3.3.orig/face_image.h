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
 * Created On      : Tue Jan 11 14:11:30 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Jan 31 22:45:43 1994
 * Update Count    : 13
 * Status          : Released
 * 
 * HISTORY
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sat Jan 29 20:36:37 1994 #11 (Chris Liebman)
 *    Now use FaceImageType instead of FaceImageFuncs.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 11:22:55 1994 #2 (Chris Liebman)
 *    Minor rearangement.
 * 
 * PURPOSE
 *	Definitions for images.
 *
 * $Id: face_image.h,v 1.5 1994/02/23 13:17:02 liebman Exp $
*/

#ifndef FACE_IMAGE_H_
#define	FACE_IMAGE_H_

/*
 * Here is the function set for a face image.
*/

struct face_image_type
{
    char*		name;
    void*             (*read) P_((char* file, void* type_data));
    void              (*free) P_((void* data, void* type_data));
    Pixmap            (*pixmap) P_((void *data, void* type_data));
    Pixmap            (*shape) P_((void *data, void* type_data));
    char*             extension;
    void*             data;
    FaceImageType     *next;
};

/*
 * Here is a face image structure.
*/

struct face_image
{
    String		file;		/* image file name */
    String		label;		/* user@host label. */
    int			refs;		/* Total refs on this face. */
    int			list_count;	/* Count of times in display list. */
    FaceImageType	*type;		/* image functions. */
    void		*data;		/* image data */
    struct face_image	*next;		/* Pointer to next face. */
    struct face_image	*prev;		/* Pointer to prev face. */
};


#endif /* FACE_IMAGE_H_ */
