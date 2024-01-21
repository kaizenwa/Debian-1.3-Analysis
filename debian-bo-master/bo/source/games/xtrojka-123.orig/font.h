/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	31.xii.1995
 *	modified:
 *
 *	Some macro's for easy font handling
 */

#ifndef _font_h_
#define _font_h_

/*
 *	this macro is useful for non-proportional fonts only!!!
 */
#define FontWidth(s)	 ((s)->max_bounds.rbearing - (s)->min_bounds.lbearing)

#define FontHeight(s)	((s)->max_bounds.ascent + (s)->max_bounds.descent)

#endif /* _font_h_ */

