/* Module:	colormap.h
 * Purpose:	Define the structs for color tables and related constants
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{1} Valdes  increased CTBL_MAX to 256		   9 Oct 1992
 *		{n} <who> -- <does what> -- <when>
 */

#define CTBL_MAX 256
/* structures for internal color map info storage */

struct SubTable {
  int vertex_cnt;		/* number of vertices defined */
  int do_gamma;			/* flag for gamma correction != 1.0 */
  double gamma;			/* gamma correction factor */
  double cell_level[CTBL_MAX];	/* cell levels (usable range is 0-1) */
  double intensity[CTBL_MAX];	/* intensity at each cell level (0-1) */
};

typedef struct _ColorTable {
  struct SubTable red;
  struct SubTable green;
  struct SubTable blue;
} ColorTable;
