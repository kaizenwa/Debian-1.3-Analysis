#ifndef _EFAXLIB_H
#define _EFAXLIB_H

#include <stdio.h>

		/*  T.4 fax encoding/decoding */

#ifndef u_char
#define u_char unsigned char
#endif

/* Maximum path length */

#ifndef PATH_MAX

#ifdef FILENAME_MAX
#define PATH_MAX FILENAME_MAX
#else
#define PATH_MAX 255
#endif

#endif


#define DEFPGLINES 66           /* default lines per page */


			   /* Buffer sizes.  */

/* The maximum scan line width, MAXRUNS, is conservatively
   set at 8k pels. This is enough for the longest standard T.4 coding line
   width (2432 pels), the longest encodeable run (2623 pels) and with
   32-pel-wide characters allows up to 256 characters per line.  Converted
   to T.4 codes, each pair of runs takes up to 25 bits to code.  MAXCODES
   must also be at least the maximum minimum line length (1200 cps*40 ms ~=
   48 bytes).  */

#define MAXRUNS 8192
#define MAXBITS (MAXRUNS/8+1)
#define MAXCODES (MAXRUNS*25/8/2+1)

/* Line/font size limits */

#define MAXLINELEN 256				/* maximum length of string */
#define MAXFONTW 32				/* maximum char width */
#define MAXFONTH 48				/* maximum char height */
#define MAXFONTBUF (MAXFONTW*MAXFONTH/8*256) 	/* PBM font buffer size */

/* Longest run encodeable by the T.4 encoding tables used. */

#define MAXRUNLEN (2560+63)

     /* Codes for EOL and number of EOLs required for RTC */

#define EOLCODE 1
#define EOLBITS 12
#define RTCEOL  5
			   /* Fonts */

#define STDFONTW    8		/* the built-in font width, height & size */
#define STDFONTH    16
#define STDFONTBUF  4096

typedef struct fontstruct {
  int h, w ;
  u_char buf [ MAXFONTBUF ] ;
  short offset [ 256 ] ;
} faxfont ;

extern u_char stdfont [ ] ;	/* compressed bit map for built-in font */

int readfont ( char *fname, faxfont *font ) ;

		    /* T.4 Encoding/Decoding */

typedef struct t4tabstruct { 
  short code, bits, rlen ;			/* code, bits, run length */
} t4tab ;

extern t4tab wtab [ ( 64 + 27 + 13 ) + 1 ] ;	/* white runs */
extern t4tab btab [ ( 64 + 27 + 13 ) + 1 ] ;	/* black runs */

typedef struct dtabstruct {			/* decoder table entry */
  struct dtabstruct *next ;
  short bits, code ;
} dtab ;

			     /* Image Input */

#define bigendian ( * (u_char*) &short256 )
extern short short256 ;

#define NFORMATS 11

enum formats { AUTO=0, PBM=1, FAX=2, PCL=3, PS=4, PGM=5, 
		 TEXT=6, TIFF_FAX=7, TIFF_RAW=8, DFAX=9, TIFF=10 } ;

extern char *formatname [ NFORMATS ] ;

typedef struct decoderstruct {
  long x ;				 /* undecoded bits */
  short shift ;				 /* number of unused bits - 9 */
  dtab *tab ;				 /* current decoding table */
  int eolcnt ;				 /* EOL count for detecting RTC */
} DECODER ;

void newDECODER ( DECODER *d ) ;

#define IFILEBUFSIZE 512

typedef struct ifilestruct {	/* input image file state  */
  FILE *f ;			/* file pointer */

  char **fnames ;		/* array of file names */
  int i, n ;			/* index of current and number of file names */

  int forceformat ;		/* file format to assume (or AUTO) */
  int format ;			/* file format to use */

  int w, h ;			/* image width, height */
  int lines ;			/* scan lines remaining in page */
  float xres, yres ;		/* x and y resolution, dpi */

  long prev, cur, next ;	/* offsets to previous, current & next image */

  faxfont *font ;		/* TEXT: font to use */
  int pglines ;			/* TEXT: text lines per page */
  char text [ MAXLINELEN ] ;	/* TEXT: current string */
  int txtlines ;		/* TEXT: scan lines left in text l. */
  int charw, charh, lmargin ;	/* TEXT: desired char w, h & margin */

  DECODER d ;			/* FAX: T.4 decoder state */

  u_char revbits ;		/* TIFF: fill order is LS to MS bit */
  u_char bigend ;		/* TIFF: big-endian byte order */

  u_char buf [ IFILEBUFSIZE ], *bufp ;	 /* stream variables... */
  int bufcnt, bufmax ;
} IFILE ;

void    newIFILE ( IFILE *f, int format, char **fname ) ;
char *ifname ( IFILE *f, int i ) ;
void logifnames ( IFILE *f, char *s ) ;
int nextipage ( IFILE *f, int dp ) ;
int alldone ( IFILE *f ) ;
int stdinnext ( IFILE *f ) ;
int     readline ( IFILE *f, short *runs, int *pels ) ;

/* Retrieve next character from image file f.  The character
   comes from the start-of-file buffer or from the file. */

#define ifgetc( f ) ( f->bufcnt > 0 ? ( f->bufcnt--, *f->bufp++ ) : \
		     getc ( f->f ) )

			    /* Image Output */

typedef struct encoderstruct {
  long x ;				 /* unused bits */
  short shift ;				 /* number of unused bits - 8 */
} ENCODER ;

void newENCODER ( ENCODER *e ) ;

typedef struct ofilestruct {		 /* input image file state  */
  FILE *f ;				 /* file pointer */
  int format ;				 /* file format */
  char *fname ;			         /* file name pattern */
  char cfname [ PATH_MAX + 1 ] ;	 /* current file name */
  float xres, yres ;			 /* x and y resolution, dpi */
  int w, h ;			         /* width & height, pixels */
  int lastpageno ;			 /* PS: last page number this file */
  int pslines ;			         /* PS: scan lines written to file */
  int bytes ;			         /* TIFF: data bytes written */
  ENCODER e ;				 /* T.4 encoder state */
} OFILE ;

void  newOFILE ( OFILE *f, int format, char *fname, 
		float xres, float yres, int w, int h ) ;
int  nextopage ( OFILE *f, int page ) ;
void writeline ( OFILE *f, short *runs, int nr, int no ) ;

			/*  Scan Line Processing */

u_char   *putcode ( ENCODER *e, short code , short bits , u_char *buf ) ;
u_char *runtocode ( ENCODER *e, short *runs, int nr, u_char *buf ) ;

/* int bittorun ( u_char *buf, int n, short *runs ) ; */
int texttorun ( u_char *txt, faxfont *font, short line, 
	       int w, int h, int lmargin,
	       short *runs, int *pels ) ;

int   xpad ( short *runs, int nr, int pad ) ;
int xscale ( short *runs, int nr, int xs ) ;
int xshift ( short *runs, int nr, int s ) ;

int runor ( short *a, int na, short *b, int nb, short *c, int *pels ) ;

/* Bit reversal lookup tables (note that the `normalbits' array
   is the one actually used for the bit reversal.  */

unsigned char reversebits [ 256 ], normalbits [ 256 ] ;

void initbittab(void) ;

/* Other Stuff */

int ckfmt ( char *p, int n ) ;

#endif
