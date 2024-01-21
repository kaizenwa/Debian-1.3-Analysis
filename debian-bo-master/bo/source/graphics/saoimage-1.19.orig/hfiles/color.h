#ifndef lint
static char SccsColorId[] = "%W%  %G%";
#endif

/* Module:	Color.h
 * Purpose:	Define the structs for color control parameters and some
 *		related constants
 * Modified:	{0} Michael VanHilst	initial version		   9 May 1989
 *		{1} Valdes  increased PSEUDOZ to 256		   9 Oct 1992
 *		{n} <who> -- <does what> -- <when>
 */

#define UNKNOWN 0
#define CAN     1
#define CANNOT  2
#define MAY     0
#define MUST    8
#define MAYNOT 16

#define COLMAPSZ 256		/* maximum number of color cells */
#define PSEUDOSZ 256		/* maximum number of table vertices */

struct palletteRec {		/* widely used read-only color pixels */
  int std_black;		/* from BlackPixel */
  int std_white;		/* from WhitePixel */
  int true_black;		/* true 0,0,0 black */
  int true_white;		/* true max,max,max white */
  int red;
  int green;
  int blue;
  int yellow;
};

struct colmapRec {
  int private_used;		/* flag using private colormap */
  int private_installed;	/* flag private map is installed */
  int default_permit;		/* user permission to use default map */
  int default_enable;		/* confirmed ability to use default map */
  int private_permit;		/* user permission to use private map */
  int private_enable;		/* confirmed ability to use private map */
  Colormap default_colormap;	/* default colormap of root */
  Colormap private_colormap;	/* private created colormap */
  XVisualInfo *default_vinfo;	/* info about the default visual */
  XVisualInfo *private_vinfo;	/* info about a private visual */
};

struct cellspecRec {
  int overlay;			/* request for overlay plane */
  int wanted;			/* desired (maximum) number of cells */
  int min;			/* minimum acceptable number of cells */
  int got;			/* actual number of cells reserved */
};

typedef struct _GCspec {
  unsigned long foreground;
  unsigned long background;
  int func;
  unsigned long mask;
} GCspec;

struct gcsetRec {
  GCspec disp;			/* image display parameters */
  GCspec draw;			/* cursor drawing parameters */
  GCspec track;			/* cursor tracking parameters */
  GCspec undraw;		/* cursor erasing parameters */
  GCspec incl;			/* include region parameters */
  GCspec excl;			/* exclude region parameters */
  GCspec menu;			/* stc_black on std_white */
  GCspec red;			/* for drawing red lines */
  GCspec green;			/* for drawing green lines */
  GCspec blue;			/* for drawing blue lines */
  GCspec black;			/* for drawing black lines */
  GCspec white;			/* for drawing white lines */
};

struct curcolorRec {
  int disp_one;			/* color map slots for graphics */
  int disp_two;
  char *default_cur;		/* default name of cursor color */
  char *default_one;		/* default names of graphics colors */
  char *default_two;
  char *desired_cur;		/* setable name of cursor color */
  char *desired_one;		/* setable names of graphics colors */
  char *desired_two;
  XColor color_cur;		/* description of the cursor color */
  XColor color_one;		/* description of the graphics color */
  XColor color_two;
};

struct halftoneRec {
  int mode;			/* dither, diffuse */
  int matrixID;			/* Matrix1, Matrix2 */
  int inverse;			/* halftone maintains it's own flag */
  int pad;
  short *matrix;		/* dithering weight matrix */
  short *errbuf;		/* error diffusion error buffer */
};

struct subtableRec {
  int fixed_cells;		/* cellmap written directly (no table) */
  int vertex_cnt;		/* number of vertices defined */
  int map_sz;			/* number of cells in map */
  int do_gamma;			/* flag for gamma correction != 1.0 */
  int invert_order;		/* order inverted with negative contrast */
  int pad;			/* hold space for double allignment */
  double contrast;		/* cell = (base * contrast) + bias */
  double bias;			/* base = (cell - bias) / contrast */
  double gamma;			/* gamma correction factor */
  double cell_level[PSEUDOSZ];	/* cell levels (usable range is 0-1) */
  double base_level[PSEUDOSZ];	/* original levels - .5 (range -0.5-+0.5) */
  double intensity[PSEUDOSZ];	/* intensity at each cell level (0-1) */
  double cellmap[COLMAPSZ];	/* intensity by cell (0-1) */
  double gammamap[COLMAPSZ];	/* intnsity by cell with gamma correction */
};

struct colorTable {
  struct subtableRec red;
  struct subtableRec green;
  struct subtableRec blue;
};

struct scalemodeRec {
  int mode;
  int wrap_cnt;
  double root_power;
  double log_expo;
};

struct colorRec {
  Display *display;		/* display server */
  int screen;			/* screen id for display server */
  int screen_depth;		/* number of color hardware planes */
  Visual *visual;		/* visual of the current colormap */
  Colormap colormap;		/* server colormap being used */
  int monochrome;		/* only one color with fixed values */
  int single_plane;		/* one plane, use dither or diffuse */
  int cursor_overlay;		/* overlay plane in use flag */
  int ncolors;			/* number of colors reserved for image */
  int inverse;			/* forward or inverse values */
  int colormap_mode;		/* halftone, pseudo, fixed */
  int color_tableID;		/* id of predefined colorset */
  int control_mode;		/* ContBias, ThreshSat, gamma */
  int control_mode_ext;		/* R_G_B, RGB (separate, combined) */
  int colors_alloced;		/* flag that colors were alloc'ed */
  int old_mode;			/* previous mode (when new change) */
  int image_plane_mask;		/* mask for image planes only */
  unsigned long overlay_mask;	/* mask with bit for overlay cursor */
  struct scalemodeRec scale;	/* parameters for image scaling modes */
  struct colmapRec map;		/* describe which colormap to use */
  struct cellspecRec cells;	/* request for cells */
  struct palletteRec hard;	/* read-only basic colors */
  struct halftoneRec halftone;	/* halftoning stuff */
  struct curcolorRec cur;	/* cursor drawing stuff */
  struct gcsetRec gcset;	/* specifications of GC's used */
  struct colorTable ctable;	/* pseudocolor vertex table */
  unsigned long pixvalmap[COLMAPSZ];	/* byte val to hardware pixel val */
  XColor cellstore[COLMAPSZ];		/* used to set colors */
};
