/*
 * File:	killring.h
 * Purpose:	Declarations for the kill ring.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: killring.h,v 1.2 1996/12/08 20:29:16 liw Exp $"
 * Description:	A kill ring holds all deleted text so that it can be
 *		easily retrieved.  Each deletions gets its own entry in
 *		the ring, except that several continuous deletions at the
 *		same location (for example, deleting several lines) can
 *		be joined into one entry.  The latest deletion is always the
 *		first entry in the ring.  The next entry is the second
 *		latest deletion, and so on.  After the oldest deletion,
 *		the next deletion is the latest one.  This allows the user
 *		to circulate between deletions so as to easily find the one
 *		he's looking for.
 *
 *		The kill ring is global for all windows and buffers, so that
 *		it is possible to use it to move things between windows.
 *		
 *		The kill ring communicates using Sbufmarks from Publib.
 *		The whole editor uses the same mechanism.
 *
 *		There is currently a static maximum limit on how many 
 *		entries there can be in the kill ring.  This may be made
 *		dynamic in the future, after I've thunk about how to make
 *		a better limit.
 */
 
#ifndef killring_h
#define killring_h

#include <publib.h>

int killring_add(Sbufmark *);
int killring_prepend_to_latest(Sbufmark *);
int killring_append_to_latest(Sbufmark *);
int killring_get_first(Sbufmark *);
int killring_get_next(Sbufmark *);
Sbufmark *killring_get_first_mark(void);

#endif
