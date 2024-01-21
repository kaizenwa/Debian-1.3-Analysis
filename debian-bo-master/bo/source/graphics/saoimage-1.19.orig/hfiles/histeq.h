#ifndef lint
static char SccsHistEqId[] = "%W%  %G%";
#endif

/* Module:	HistEq.h
 * Purpose:	Define the structs for histogram equalization
 * Modified:	{0} Michael VanHilst	initial version		  30 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#define MAX(a,b) (((a) > (b)) ? (a) : (b))

/* link describes a section of the histogram to have levels allocated */
typedef struct histogramLink {
  int low;		/* index of lowest histogram entry */
  int high;			/* index of highest histogram entry */
  int range;			/* number of histogram entries covered */
  int nz_entries;		/* number of non-zero histogram entries */
  int pixel_area;		/* number of image pixels within range */
  int max_entry;		/* largest area for any single entry */
  int excess_pixels;		/* pixel area in excess of average */
  int color_levels;			/* number of color levels allocated */
  struct histogramLink *next;	/* link list pointer */
  int pad;
} SubrangeLink;

/* list describing details of color level allocation for a histogram range */
typedef struct histogramList {
  int pixel_area;	/* pixel area that is covered by this color level */
  int first, last;	/* first and last histogram entries cor this color */
  int pad;
  int shrink_area;	/* area covered by omitting last non-zero entry */
  int shrink_entry;	/* index for last to excude last non-zero entry */
  int stretch_area;	/* area covered by including next non-zero entry */
  int stretch_entry;	/* index for last to include next non-zero entry */
} SubrangeList;
