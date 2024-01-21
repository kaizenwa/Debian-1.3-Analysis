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
 * Last Modified On: Sat Feb 12 22:22:41 1994
 * Update Count    : 2
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Definitions for sounds.
 *
 * $Id: face_sound.h,v 1.3 1994/02/13 22:33:30 liebman Exp $
*/

#ifndef FACE_SOUND_H_
#define	FACE_SOUND_H_

/*
 * Here is a face sound structure.
*/

struct face_sound
{
    String		file;		/* sound file name */
    int			refs;		/* Total refs on this face. */
#ifdef USE_BUCKETS
    AuBucketID		bucket;
#else
    Sound		sound;
#endif
    struct face_sound	*next;		/* Pointer to next face. */
    struct face_sound	*prev;		/* Pointer to prev face. */
};

#endif /* FACE_SOUND_H_ */
