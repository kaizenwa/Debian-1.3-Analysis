#ifndef lint
static char SccsCursorId[] = "%W%  %G%";
#endif

/* Module:	Cursor.h
 * Purpose:	Define the structs for cursor manipulation parameters
 * Modified:	{0} Michael VanHilst	initial version		  3 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

/* get Color declaration if it is not already defined (for GCspec) */
#ifndef COLMAPSZ
#include "color.h"
#endif

#define XSIDE 1			/* codes for sizing on one dimension only */
#define YSIDE 2
#define XYCORNER 3		/* code for sizing by a corner (both axes) */

/* maximum number of points for drawing things (beginning repeated at end) */
#define CURSOR_MAX 80

/* file coordinates */
typedef struct _CurFCoord {
  float X, Y;				/* center dimension (or other ref) */
  float Xdim, Ydim;			/* dimension appropriate to type */
  double area;				/* area if computed */
} CurFCoord;

/* display window coordinates */
typedef struct _CurWCoord {
  Display *display;			/* display where draw */
  Window ID;				/* drawable where to draw */
  int x, y;				/* integer center coordinates */
  double X, Y;				/* float center coordinates */
  double rayX, rayY;			/* center to side distance */
} CurWCoord;

/* rotation angle parameters */
typedef struct _CurRot {
  double angle;				/* rotation angle in radians */
  double sin, cos;			/* sin and cos of angle */
  double cut_A, cut_B;			/* angles of pie cut */
} CurRot;

/* parameters used for manipulation */
typedef struct _CurCtrl {
  int active_vertex;			/* index of vertex being maniputated */
  int active_side;			/* sizing of x dim, y dim, or both */
  double axis_ratio;			/* fixed ratio of Xdim and Ydim */
} CurCtrl;

/* brief parameter set for vertex of polygon */
typedef struct _PolyPoint {
  float winX, winY;			/* int coordinates of point */
  float fileX, fileY;			/* file coordinates */
  int unset;				/* display point has been moved */
} PolyPoint;

/* main structure */
struct cursorRec {
  CurFCoord file;			/* file coordinates */
  CurWCoord win;			/* window coordinates */
  CurRot rot;				/* rotation angle parameters */
  CurCtrl ctrl;				/* interactive control parameters */
  int type;				/* type (code in Constant.h) */
  int cut;				/* apply-pie-cut flag */
  int label_point;			/* label-the-point flag */
  int overwrites_image_data;		/* cursor-overwrites-image-data */
  int index;				/* index for regions and annuli */
  int exclude_region;			/* region type and suppress drawing */
  int point_cnt;			/* number of points for XDrawLines */
  XPoint *points;			/* absolute-coord drawing vertices */
  int rectangle_cnt;			/* number of hash-mark boxes */
  XRectangle *rectangles;		/* for XDrawRectangles */
  int poly_cnt;				/* number of vertices in polygon */
  PolyPoint *poly;			/* array of polygon vertex coords */
  int annuli;				/* annuli of this type */
  struct cursorRec *next_annulus;	/* annulus linklist */
  struct cursorRec *next_region;	/* region linklist */
  GCspec *draw;				/* color set to draw this cursor */
};
