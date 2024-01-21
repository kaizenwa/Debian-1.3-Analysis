/*                             -*- Mode: C++-C -*- 
 * 
 *  
 * 	     Copyright 1994 Christopher B. Liebman
 * 
 *  Permission to use, copy, modify, distribute, and sell this software
 *  and its documentation for any purpose is hereby granted without fee,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name Christopher B. Liebman not
 *  be used in advertising or publicity pertaining to distribution of this
 *  software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 * B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 * INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 * PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * 
 * 
 * Author          : Chris Liebman
 * Created On      : Sat Jan 29 15:03:34 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Jan 31 22:52:06 1994
 * Update Count    : 15
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Search structs.
 *
 * $Id: face_search.h,v 1.1 1994/02/01 03:52:16 liebman Exp $
*/

#ifndef FACE_SEARCH_H
#define FACE_SEARCH_H

struct face_search_type
{
    char*		name;
    int			(*search) P_((MailItem* item, FaceSearchData* data));
    void		*data;
    FaceSearchType*	next;
};

struct face_search_data
{
    FaceFormat		format;
    FaceSearchType*	search;
    FaceImageType**	itypes;
    FaceSoundType**	stypes;
    char**		paths;
    FaceSearchData*	next;
};

#endif /* FACE_SEARCH_H */
