#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	util.c (Utility Functions)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	A calloc with error checking that every library needs
 * Subroutines:	btn_Alloc()			returns: char *
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		18 March 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>	/* define stderr, NULL */

/*
 * Subroutine:	btn_Alloc
 * Purpose:	calloc with printf'less error message and exit for failure
 * Returns:	char pointer to allocated and cleared space
 * Called by:	MakeButtonBox(), btn_LabelButtons() in MakeBtnBox.c
 * Called by:	btn_MakeXImages() in MakeXImage.c
 * Xlib calls:	none
 * Post-state:	space allocated on aligned boundary and zeroed
 * Note:	printf does malloc so don't use it to report a malloc failure
 */
static char *errnote = " allocation failure\n";
char *btn_Alloc ( count, size, errmess )
     int count;
     unsigned int size;
     char *errmess;
{
  char *space;
  char *calloc();

  if( (space = (char *)calloc((unsigned)count, size)) == NULL ) {
    fputs(errmess, stderr);
    fputs(errnote, stderr);
    exit( 0 );
  }
  return( space );
}
