#ifndef lint
static char SccsImageId[] = "%W%  %G%";
#endif

/* Module:	Image.h
 * Purpose:	Define the struct for image reading & manipulation parameters
 * Modified:	{0} Michael VanHilst	initial version	      24 January 1989
 * Modified:	{1} Doug Mink     move WCS info to wcs.h      18 October 1995
 * Modified:	{2} Doug Mink     add initial zoom factor      2 January 1996
 *		{n} <who> -- <does what> -- <when>
 */

struct imageRec {
  char *filename;		/* name of image file */
  int fd;			/* open handle of image file */
  int headersize;		/* bytes at top of file used for header */
  char *header;			/* stored header */
  int file_type;		/* type of file (array,fits,iraf,etc) */
  int storage_type;		/* code for file storage (short, real, etc) */
  int bytepix;			/* number of bytes for pixel of data */
  int nimage;			/* number of image if more than one in file */
  int keep_data;		/* flag to keep file data in memory */
  int byte_swap;		/* flag to byte swap data after reading file */
  int row_order;		/* row one as top or bottom */
  int index_base;		/* 0 or 1 based index */
  int rotate_code;		/* rotation to apply */
  int block_type;		/* type of blocked file reading */
  int filecols, filerows;	/* dimensions of image file */
  int fiX1, fiX2, fiY1, fiY2;	/* image file subsection selection */
  double ficenX, ficenY;	/* center of image subsection used */
  int fiblock;			/* base file to image blocking selection */
  int dispcols, disprows;	/* initial display window size selection */
  int dispfix;			/* flag that display size was specified */
  int keepfilebuf;		/* flag to keep file format data buf */
  int fdblock;			/* initial file to disp blocking selection */
  double fdcenX, fdcenY;	/* initial display center in file coords */
  int bufmax;			/* limit of buffer size (square) */
  int buffix;			/* flag to prevent its being readjusted */
  int dispxmax, dispymax;	/* limits of display window size */
  int dispmin;			/* (2 max's but symmetric min) */
  int energy;			/* energy filter (bitwise) for xray images */
  int scaling;			/* initial scaling selection */
  int fscaled;			/* scale and offset used to write file data */
  double fscale, fbias;		/* prior scale and offset of file data */
  double fimin, fimax;		/* limits to clip image values (if a<b) */
  double fiscale, fibias;	/* scale and offset to fit data into buffer */
  int fiscaled;			/* scale and offset used in file read */
  int pmax, pmin;		/* image value limits for scaling */
  int pfix;			/* flag to use limits as given or calculate */
  int imtoolWZT;		/* imtool scaling type */
  double imtoolwcs[6];		/* iraf imtool WCS coordinates */
  double img_adj;		/* offset adjust needed for ximage coords */
   /* fX=((imgX+adj)*A+(imgY+adj)*C)+E,fY=((imgX+adj)*B+(imgY+adj)*D)+F */
  double imtoolWZ[2];		/* imtool greyscale Z values */
  int fbconfig;			/* iraf imtool frame configuration number */
  int frame_number;		/* imtool frame number */
  int imtool_200;		/* current image is from imtool */
  int panbox_zoomtype;		/* type of zoom from image (av, max, sample) */
  double zoom;			/* zoom factor */
};
