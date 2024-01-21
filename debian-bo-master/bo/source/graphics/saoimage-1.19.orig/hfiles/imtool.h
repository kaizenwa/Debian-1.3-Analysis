
	/* template for IRAF list-format point cursor coordinate filename */
#ifdef VMS
#define	COORDFILE	"frame.%d_%d"
#else
#define	COORDFILE	"frame.%d.%d"
#endif
	/* template for world coordinate system file name */
#define	WCSFILE		"imtool_%d.wcs"
#define	SZ_WCSBUF	320	/* WCS text buffer size */
	/* user's local frame buffer configurations */
#define	FBCONFIG_1	".imtoolrc"
	/* system wide frame buffer configurations */
#ifndef FBCONFIG_2
#define	FBCONFIG_2	"/usr/local/lib/imtoolrc"
#endif
	/* two choices of system environment varibale names */
#define	FBCONFIG_ENV1	"imtoolrc"
#define	FBCONFIG_ENV2	"IMTOOLRC"
	/* default values */
#define	DEF_NCONFIG		1	/* number of f.b. configs */
#define	DEF_NFRAMES		1	/* save memory; only one frame */
#define	DEF_FRAME_WIDTH		512	/* 512 square frame */
#define	DEF_FRAME_HEIGHT	512	/* 512 square frame */
	/* range of display vazlues used by imtool */
#define CMS_DATASTART	1	/* minimum data */
#define CMS_DATAEND	200	/* value imtool uses as data maximum */
#define CMS_DATARANGE	200	/* range of display values in imtool */
	/* types of scaling algorithms used by images.tv.display */
#define W_UNITARY	0	/* direct unscaled */
#define W_LINEAR	1	/* linear but from given low to high */
#define W_LOG		2	/* log from low to high */

	/* codes for the imtool packet (IIS) header */
#define	MEMORY		001	/* packet with image data */
#define	LUT		02	/* switch display buffer */
#define	FEEDBACK	005	/* for imtool, do clear and fbconfig */
#define	IMCURSOR	020	/* logical image cursor */
#define	WCS		021	/* used to set WCS */
#define	PACKED		0040000	/* bit indicates byte data (in tid) */
#define	COMMAND		0100000	/* bit with LUT to set disp frame */
#define	IIS_READ	0100000	/* tid w/ MEMORY to read back dsip buf */
#define	IMC_SAMPLE	0040000	/* code for read cursor now */
#define	IMT_FBCONFIG	077	/* mask for fbconfig num (in tid) */


struct imtoolRec {
  short tid;		/* Type of message identifier ("transfer id") */
  short thingct;	/* Count appropriate to message type */
  short subunit;	/* Intended target type (MEMORY,LUT,FEEDBACK,etc) */
  short checksum;	/* Error check */
  short x, y;		/* (y,x & 01777) are UL coordinates in MEMORY packet */
  short z;		/* (z & 07777) is frame number in FEEDBACK & MEMORY */
  short t;		/* ? */
};

	/* maximum transfer size (pixels per packet) */
#ifdef VMS
#define SZ_FIFOBUF 8192
#else
#ifdef IRAF25
#define SZ_FIFOBUF 4096
#else
#define SZ_FIFOBUF 4000		/* transfer size for FIFO i/o */
#endif
#endif
                              
                                                              
