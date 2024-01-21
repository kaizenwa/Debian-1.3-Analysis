/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	12.iii.1996
 *	modified:
 *
 *
 */

#ifndef _window_h_
#define _window_h_

/*
 * dimensions of the various windows (those are fixed!)
 */

#define WIN_XSIZE	182		/* width + height of trojka window */
#define WIN_YSIZE	380	

/*
 * dimensions of a pillar 
 */
#define PILLAR_XSIZE	51
#define PILLAR_YSIZE	WIN_YSIZE


/*
 * dimensions of a block
 */
#define BLOCK_XSIZE	36
#define BLOCK_YSIZE	19


/*
 *	TODO: read these from the application environment!!!!!!!!!!
 */
#define MBARHEIGHT	24
#define BORDER		4


/*
 *	function prototypes
 */
void init_windows(void);
void fix_dimensions(Widget);
void clear_window(Widget);

#endif /* _window_h_ */
