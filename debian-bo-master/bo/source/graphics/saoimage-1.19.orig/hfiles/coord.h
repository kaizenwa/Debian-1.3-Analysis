#ifndef lint
static char SccsCoordId[] = "%W%  %G%";
#endif

/* Module:	Coord.h
 * Purpose:	Define the structs for coordinate handling and some related
 *		constants
 * Modified:	{0} Michael VanHilst	initial version	     31 December 1988
 *		{1} MVH replaced rep variables in cornerRec	 21 June 1991
 *		{n} <who> -- <does what> -- <when>
 */

#define LX	 1		/* flags to show which edges clipped */
#define TY	 2
#define RX	 4
#define BY	 8
#define LXTY	 3		/* flags to test clipped corners */
#define RXBY	12

typedef struct fieldRec {
  float X1, X2, Y1, Y2;		/* edges of subsection area in its system */
  int X1i, X2i, Y1i, Y2i;	/* first and last coords of subsection */
  int Xwdth, Yhght;		/* dimensions of subsection */
  float cenX, cenY;		/* native coordinates of official center */
  int width, height;		/* dimensions of system */
  double ioff;			/* ir offset (realX = (float)intX + ioff) */
} Coordsys;

typedef struct transfRec {
  float inx_outx, iny_outx, add_outx;
  float inx_outy, iny_outy, add_outy;
  float iadd_outx, iadd_outy;
  int no_rot;
  int int_math, zoom, multiply, flip;
  int ixzoom, iyzoom;
} Transform;

/* parameters to map data from one window (src) to another (dst)
/* clipping occurs when the (dst) window extends beyond the (src) window */
/* clipping codes (unique bit) follow this struct declaration */
typedef struct cornerRec {
  float cenX, cenY;			/* dst's center in src's coords */
  double zoom;				/* src to dst zoom */
  int srcX1, srcX2, srcY1, srcY2;	/* src left, top, right, bottom pixs */
  int srcXwdth, srcYhght;		/* dst's dim's in src's coords */
  int clip;				/* code where src clips dst */
  int block;				/* integer blocking code */
  int dstX1, dstX2, dstY1, dstY2;	/* four edges of map in dst */
  int dstXwdth, dstYhght;		/* dimensions as delivered to dst */
  int dst_x, dst_y;			/*  src(0,0) in dst coords  */
  int src_x, src_y;			/*  dst(0,0) in src coords  */
} Edges;

struct coordRec {
  Coordsys file;			/* the image file */
  Coordsys fbuf;			/* buffer for data larger than i*2 */
  Coordsys img;				/* TV oriented reference system */
  Coordsys buf;				/* i*2 buffer for data */
  Coordsys disp;			/* char buffer for main display */
  Coordsys pan;				/* all buffers for pan/zoom window */
  Transform imgtofile, filetoimg;
  Transform pantoimg, imgtopan;
  Transform disptoimg, imgtodisp;
  Transform buftoimg, imgtobuf;
  Transform fbuftoimg, imgtofbuf;
  Transform buftofile, filetobuf;
  Transform disptofile, filetodisp;
  Transform buftofbuf;			/* i*2 buffer to file data buffer */
  Transform pantofile;
  Transform disptobuf;
  Transform filetoaux;			/* 2nd system in coord track string */
  Edges id;				/* outline of disp in img coords */
  Edges tid;				/* proposed edges of disp in img */
  Edges ib;				/* edges of buffer contents in img */
  Edges bd;				/* edges of disp in the buffer */
  Edges fb;				/* edges of buffer in file */
  Edges fp;				/* edges of pan window in file */
  int bufcheck;				/* flag to buf change to suit disp */
  int buferror;				/* flag proposed disp needs new buf */
  int imtool_aux;			/* transform to true coords exists */
  int pad;
};
