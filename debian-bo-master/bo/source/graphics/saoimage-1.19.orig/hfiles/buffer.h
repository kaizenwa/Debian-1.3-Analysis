#ifndef lint
static char SccsBufId[] = "%W%  %G%";
#endif

/* Module:	Buffer.h
 * Purpose:	Define structure to hold and describe buffers for imaging
 * Modified:	{0} Michael VanHilst	initial version	      1 December 1988
 *		{n} <who> -- <does what> -- <when>
 */

/* Notes:
 *  Image data is stored as 16 bit signed integers
 *  Data may have been scaled and biased from image file as per Image.h
 *  Scalemap and histogram have one cell per possible 16 bit value
 *   Scalemap has its mapping to screen display values
 *   Histogram counts the number of its occurences in the display image section
 *  Scalemap and Histogram are the alloc pointers, they must be offset by
 *   ZERO_OFFSET to use them as an array with the signed integer as index
 *  Min and max establish the range for which mapping is figured
 *   mm parameters refer to the previous calculation of the histogram and
 *   can be used to determine if it needs to be updated for a different section
 *  zoomsum refers to mapping adjustments that may be needed if summed blocking
 *   is used in file reading, as different zooms will change the range of
 *   values in the buffer
 *  Panbuf contains a sample of the full image for use in making the pan
 *   window display.  It has the same size as the pan window for fast access
 */

struct sampleRec {
  int img_leftX, img_rightX;
  int img_topY, img_lowY;
};

struct bufferRec {
  char *filebuf;		/* pointer to input data buffer (any type) */
  short *shortbuf;		/* 16 bit image array storage */
  short *panbuf;		/* 16 bit image array storage for pan box */
  int filebuf_sz;		/* number of bytes allocated for filebuf */
  int shortbuf_sz;		/* number of shorts allocated */
  int panbuf_sz;
  int shortbuf_square;		/* got excess buffer size (for rotation) */
  int shortbuf_double;
  /* measurement of min and max (always followed by rescaling) */
  int clipmin, clipmax;		/* fimin and fimax in shortbuf, if used */
  double cmdMin, cmdMax;	/* min and max values from command line */
  int cmdmin, cmdmax;		/* cmdmin/max as scaled to img values */
  int scale_min, scale_max;	/* img min and max values for scaling */
  int min_given, max_given;	/* scale min and max given from command line */
  int hist_area;		/* effective histogram area for histeq */
  int pad;
  struct sampleRec mm;		/* info about when minmax determined */
  int *histogram;		/* 64k map for histogramming data */
  /* img short int to hardware byte scale map */
  unsigned char *scalemap;	/* 64k map for digitizing to fewer levels */
  /* parameters for comparing values with different summing block areas */
  int panbuf_summing;		/* block summing manifest in pan buffer */
  int shortbuf_summing;		/* block summing for shortbuf values */
  int scalemap_summing;		/* block summing of histogram when smap made */
  /* support for buffer to display scaling */
  int panbuf_max;		/* maximum value in pan buffer */
  int shortbuf_max;
  int load_filebuf;		/* OK and request to load data */
  float lfrac;			/* Fraction of peak of histogram for width */
};
