#ifndef lint
static char SccsRegionId[] = "%W%  %G%";
#endif

/* Module:	Region.h
 * Purpose:	Define the structs for region drawing parameters
 * Modified:	{0} Michael VanHilst	initial version		29 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

struct regdrawRec {
  int visible;			/* draw-regions-in-display-window */
  int label;			/* label-regions-when-drawn */
  int label_xoff;		/* x text draw offset from point */
  int label_yoff;		/* y text draw offset from point */
  XFontStruct *fontstruct;	/* font used */
  int filled_label;		/* draw-text-with-background-filled */
  int label_height;		/* minimum vertical distance between labels */
  int pad;
} regdraw;


/* parameter record for regions given on a single line */
#define REG_LIMIT 32
struct reg_param {
  int type;
  int exclude;
  char *line;
  char connector;
  char not;
};
