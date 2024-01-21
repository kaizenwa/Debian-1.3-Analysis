/*
 * NAME
 *	ps2pk - creates a PK font from a Type 1 PostScript font
 * SYNOPSIS:
 *	pk2pk [options] type1 [pkname]
 * DESCRIPTION
 *	This program renders a given Type 1 PostScript font at a given
 *	pointsize (default 10.0 points) and resolution (default 300dpi)
 *      into a TeX PKfile.
 *
 *      To generate the PKfile ps2pk needs a valid type1 fontname (for
 *      example Utopia-Regular, Utopia-Regula.pfb or Utopia-Regula.pfa)
 *      and its corresponding AFMfile (Utopia-Regular.afm). The program 
 *      accepts the MSDOS binary type1 format (Utopia-Regula.pfb) and
 *      its ASCII equivalent (Utopia-Regular or Utopia-Regula.pfa). To
 *      locate the type1 font and its AFM file ps2pk will use the environment
 *	T1INPUTS if it is set otherwise its built in default (the -v flag will
 *	report which filenames are looked for).
 *
 *      The program will make a PKfile in which the character codes are
 *      derived from their AFM defined values. This can be overruled by
 *      specifying an explicit <encoding> file via the -e option. This
 *      <encoding> file will be located via the environment variable
 *      $T1INPUTS if it is set otherwise via its built in default.
 *
 *	Options and arguments:
 *       -a<AFMfile>    The name of the AFMfile can be overruled with this
 *                      option.
 *       -e<encoding>   Name of file containing encoding scheme 
 *			(default the encoding named in the AFM file is used).
 *	 -E<extension>	The extension factor (real value, default 1.0).
 *       -m<modename>   One of the mode.mf names. Default "Unknown".
 *	 -P<pointsize>	Here the desired pointsize (real value) can be 
 *			specified (default 10.0 points).
 *	 -R<baseres>	Base resolution; if it differs from xres it is
 *			used for calculation of "mag" value (default 300dpi).
 *	 -r<Y/X>	Metafont aspect_ratio.  Obviates need to calculate
 *			yres for every case of xres (default 300/300).
 *	 -S<slant>	The slant (real value, default 0.0).
 *	 -X<xres>	The resolution (integer value) in the X direction 
 *			(default 300 dpi).
 *	 -Y<yres>	The resolution (integer value) in the Y direction 
 *			(defaults to the value of <xres>).
 *	 -v		Verbose flag. (Gives extra noise.)
 *	
 *	 type1		The name of the PostScript type 1 font. The name
 *			of the AFMfile will be constructed from this name by 
 *			removing the extension (if supplied) and adding 
 *                      ".afm". 
 *	 [pkname]	The name of the resulting PK font can be overruled
 *			with this name. The default name of the PK font is
 *			derived from the basename of the type1 font, the
 *			pointsize and <xres>. For example:
 *			   ps2pk -P17.28 Utopia-Regular
 *			will result in:
 *				Utopia-Regular17.300pk
 *	
 * ACKNOWLEDGEMENT
 *	This program uses the type1 hinting and rendering software IBM 
 *	has donated to the X-consortium.
 * SEE ALSO
 *	``METAFONT: The Program'', Donald Knuth.
 *	``The GFtoPK processor'', belonging to the METAFONTware.
 *	afm2tfm(1)
 *	pk2bm(1)
 * VERSION
 *	1.3 (August 1992)
 *	1.4 (December 1994)
 * AUTHOR
 *	Piet Tutelaers
 *	rcpt@urc.tue.nl
 */

int testing = 0;
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* begin interface to type 1 software */
#include "ffilest.h"

FontScalableRec vals;
FontEntryRec entry;
#define Succesful	85

extern int  Type1OpenScalable ();
extern int  Type1RegisterFontFileFunctions();
extern void Type1CloseFont();

/* end interface to type 1 software */

#include "filenames.h"
#include "pkout.h"

#ifdef KPSE
#include <c-auto.h>
#include <tex-file.h>
#include <tex-make.h>
#endif

char *encfile = NULL, *afmfile;

typedef char *encoding[256];
void getenc(char *, char *, encoding, int [256]);

#define POINTSPERINCH 72.27
#define DEFAULTRES 300
#define DEFAULTPOINTSIZE 10.0

double pointsize = 0.0; /* wanted pointsize */
int W, H,	        /* width and height of character glyph */
    base_resolution = 0,
    x_resolution = 0, 
    y_resolution = 0,
    aspect_numerator = 0,
    aspect_denominator = 0;
int verbose = 0;

main(argc, argv)
int argc; char *argv[];
{  char *argp, c;
   int done, i;
   char *myname = "ps2pk", *psname, *psbasename, *afmname = NULL, 
        *encname = NULL, *psfile, 
        *modename, pkname[80], *fontname;
   
   FontPtr fontptr;
   unsigned char glyphcode[1]; /* must be an array */
   CharInfoRec *glyphs[1];
   unsigned int count;
   int charcode, rc = -1, charno;
   char comment[256], envstr[256], *testenv = envstr;
   long cs;
   encoding ev;
   int WX[256];

   float efactor = 1.0, slant = 0.0;
   
   /* proto's */
   int next_pixel();
   void print_pixmap();
   void first_pixel(CharInfoRec *);
   INT32 TFMwidth(int);
   int h_escapement(int);
   char *locate(char *, char *, char *);
   long checksum(encoding, int [256]);

   modename = "Unknown mode";
   while (--argc > 0 && (*++argv)[0] == '-') {
      done=0;
      while ((!done) && (c = *++argv[0]))  /* allow -bcK like options */
      	 switch (c) {
      	 case 'a':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    afmname = argv[0]; 
	    done = 1;
      	    break;
	  case 'e':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    encname = argv[0]; 
	    done = 1;
      	    break;
      	 case 'E':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    efactor = atof(argv[0]); 
	    done = 1;
      	    break;
	  case 'm':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
	    }
            modename =  argv[0];
	    done = 1;
      	    break;
      	 case 'P':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    pointsize = atof(argv[0]); 
	    done = 1;
      	    break;
      	 case 'R':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    base_resolution = atoi(argv[0]); 
	    done = 1;
      	    break;
      	 case 'r':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
            argp = argv[0];
	    aspect_numerator = atoi(argp); 
            while (*(argp) != '/') argp++; argp++;
	    aspect_denominator = atoi(argp); 
	    done = 1;
      	    break;
      	 case 'S':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    slant = atof(argv[0]); 
	    done = 1;
      	    break;
      	 case 'X':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    x_resolution = atoi(argv[0]); done = 1;
      	    break;
      	 case 'Y':
      	    if (*++argv[0] == '\0') {
      	       argc--; argv++;
      	    }
	    y_resolution = atoi(argv[0]); done = 1;
	    if (x_resolution == 0) x_resolution = y_resolution;
      	    break;
      	 case 'v':
      	    verbose = 1; done = 1; break;
      	 default:
      	    fatal("%s: %c illegal option\n", myname, c);
      	 }
      }

   if (argc < 1 || argc >2) {
      printf("Ps2pk: version 1.4-m (Jan. 1994)");
      printf("  PK specials added (Dec. 1994)\n");
      printf("Usage: %s [ options ] type1font [pkname]\n", myname);
      printf("options: -v -e<enc> -X<xres> -E<expansion> -S<slant>\n");
      printf("options: -R<baseres> -r<aspect_ratio>  -m<modename>\n");
      printf("options: -P<pointsize> -Y<yres> -a<AFMfile>\n");
      exit(1);
   }


   psname = argv[0]; argc--; argv++;

#ifdef KPSE
   kpse_set_progname(myname);
   /* check if we have all files otherwise leave with message */   
   if ((testenv = getenv("T1INPUTS")) == NULL) {
     kpse_init_format(kpse_tex_format);
     psfile = kpse_find_file(psname,kpse_tex_format,0);
   } else {
#endif
     psfile = locate("T1INPUTS", T1INPUTS, psname);
#ifdef KPSE
   }
#endif
   if (psfile == NULL) fatal("Can't find %s\n", psname);
   psbasename = trim_basename(psname, ".");
   if (afmname ==  NULL) afmname = fullname(psbasename, ".afm");
#ifdef KPSE
   if (testenv == NULL) {
     afmfile = kpse_find_file(afmname,kpse_tex_format,0);
   } else {
#endif
     afmfile = locate("T1INPUTS", T1INPUTS, afmname);
#ifdef KPSE
   }
#endif
   if (afmfile == NULL) fatal("Can't find %s\n", afmname);
   if (verbose) 
      printf("\nLoading CharMetrics from %s", afmname);
   if (encname) {
#ifdef KPSE
     if (testenv == NULL) {
       encfile = kpse_find_file(encname,kpse_tex_format,0);
     } else {
#endif
       encfile = locate("T1INPUTS", T1INPUTS, encname);
#ifdef KPSE
     }
#endif
      if (encfile == NULL) fatal("Can't find %s\n", encname);
      if (verbose) {
         printf(" and encoding vector from %s ...", encname);
      }
    }
    encname = (char *) malloc(128);
    *encname = '\0';
    /* Don't need the filename version of this any longer */
    fflush(stdout);
    fontname = (char *) malloc(128);
    getenc(fontname, encname, ev, WX);
    if (efactor != 1.0)
       for (i=0; i < 256; i++) {
          if (ev[i] == NULL) continue;
          WX[i] = WX[i] * efactor + 0.5;
       }
    if (verbose) printf("\nFontName %s; Encoding %s\n", fontname, encname);
    fflush(stdout);

    if (pointsize == 0.0) pointsize = DEFAULTPOINTSIZE;
    if (base_resolution == 0 ) {
       if (aspect_denominator > 0 ) base_resolution = aspect_denominator;
       else base_resolution = DEFAULTRES;
    }
    if ( (aspect_numerator > 0) && (aspect_denominator == 0) )
       aspect_denominator = base_resolution;
    if (x_resolution == 0) x_resolution = base_resolution;
    if (y_resolution == 0) {
       if (aspect_denominator > 0) {
          y_resolution = 
	     (int)((x_resolution * aspect_numerator *1.0) / 
                   (aspect_denominator * 1.0) + 0.5);
       } else {
          y_resolution = x_resolution;
       }
     } else {
       if (verbose) 
	  printf("\nExplicit y_resolution overrides aspect_ratio.\n");
   }
     
   
   if (argc == 1) strcpy(pkname,  argv[0]);
   else
      sprintf(pkname, "%s%d.%dpk", 
         psbasename, (int) (pointsize + 0.5), x_resolution);
    
   /* next values are needed! */
   vals.x =     x_resolution;
   vals.y =     y_resolution;
   vals.point = 10.0 * pointsize + 0.5; /* DECIPOINTS */
   vals.pixel = pointsize * y_resolution / POINTSPERINCH + 0.5;
   if (testing)
      printf("x=%d, y=%d, point=%d, pixels=%d\n", vals.x, vals.y, vals.point, vals.pixel);
   
   if (verbose) {
      printf("Checking %s font ...", psname);
      fflush(stdout);
   }
   Type1RegisterFontFileFunctions();
   if (verbose) printf(" done\n");

	/* next line prevents UNIX core dumps */
   entry.name.name = "-adobe-utopia-medium-r-normal--0-0-0-0-p-0-iso8859-1";
   if (verbose) {
      printf("Creating character glyphs for %s ...", psname);
      fflush(stdout);
   }
   rc = Type1OpenScalable(ev, &fontptr, 0, &entry, 
            psfile, &vals, 0, 0, efactor, slant);
   if (rc != Succesful) 
      fatal("Type1OpenScalable error (result: %d) on %s \n",  rc, psfile);
   if (verbose) printf("\n %s == %s.\n", psname, fontname);

   pk_open(pkname);
   sprintf(comment, "%s rendered at %f points", psname, pointsize);
   cs = checksum(ev, WX);
   pk_preamble(comment, pointsize, cs, x_resolution, y_resolution);
   charno = 0;
   if (verbose) printf("Creating %s from %s.\n", pkname, psname);
   for (charcode = 0; charcode < 256; charcode++) {
      if (ev[charcode] == NULL) continue;
      glyphcode[0] = charcode;
      (fontptr->get_glyphs)(fontptr, 1, glyphcode, 0, &count, glyphs);
      if (count > 0) {
         if (verbose) {
            printf("'%03o ", charcode);
            fflush(stdout); charno++;
            if (charno == 8) { 
               putchar('\n'); charno = 0;
            }
         }
	 first_pixel(glyphs[0]); /* assigns W and H */
         if (testing) {
            printf("Width = %d, Height = %d\n", W, H);
	    print_pixmap();
	 }
         pk_char(charcode,			   /* charcode */
            TFMwidth(WX[charcode]), 		   /* TFMwidth */
            h_escapement(WX[charcode]),		   /* h_escapement */
            W, H,  				   /* width and height */
            - glyphs[0]->metrics.leftSideBearing,  /* h_offset */
            glyphs[0]->metrics.ascent,             /* v_offset */
            next_pixel);			   /* pixel generator */
      }
   }
   if (verbose) putchar('\n');
   pk_postamble(fontname,encname,modename,
		base_resolution,x_resolution,y_resolution,
		pointsize);
   pk_close();
   exit (0);  /* Do not send random value to system */
}

#if defined(AMIGA) || defined(VMS)
#define PATH_SEP ',' 
#define DIR_SEP '/' 
#elif defined(MSDOS)
#define PATH_SEP ';' 
#define DIR_SEP '\\' 
#else
#define PATH_SEP ':' 
#define DIR_SEP '/' 
#endif

#define MAXPATHLEN 80 
static char the_filename[MAXPATHLEN];

/* A simple function to locate a file given an environment */
char *locate(char *env, char *default_env, char *file)
{  int i, len; FILE *F; char *str;

   if ((env = getenv(env)) == NULL) env = default_env;
   len = strlen(file);
   while (1) {
      i = 0;
      while (*env != '\0' && *env != PATH_SEP && i < MAXPATHLEN - 1 - len) 
         the_filename[i++] = *env++;
      if (i == MAXPATHLEN - 1 - len) 
         fatal("Path element too long: %s\n", the_filename);
      the_filename[i] = '\0';
      if (i > 0 && (isalnum(the_filename[i-1]) || the_filename[i-1] == '.'))
         the_filename[i++] = DIR_SEP;
      the_filename[i] = '\0';
      strcat(the_filename, file);
      if (verbose) printf("Trying to open %s\n", the_filename);
      if (F = fopen(the_filename, "r")) {
      	 fclose(F); 
         str = (char *) malloc(strlen(the_filename) + 1);
         if (str == NULL) fatal("Out of memory\n");
         strcpy(str, the_filename);
      	 return str;
      }
      if (*env == '\0') break;
      if (*env == PATH_SEP) env++;
   }
   return NULL;
}

/*
 * The checksum should guarantee that our PK file belongs to the correct TFM
 * file! Exactly the same as the afm2tfm (dvips5487) calculation.
 */
long checksum(encoding ev, int width[256])
{
   int i ;
   long s1 = 0, s2 = 0;
   char *p ;

   for (i=0; i<256; i++) {
      if (ev[i] == NULL) continue;
      s1 = (s1<<1) ^ width[i];
      for (p=ev[i]; *p; p++)
	 s2 = s2 * 3 + *p ;
   }
   return (s1<<1) ^ s2 ;
}

static int row, col;
static int data, bitno;
static unsigned char *p_data;

int next_pixel()
{  int pixel;

   while (row < H) {
       if (col++ < W) { 
           if (bitno == 0) { data = *p_data++; bitno = 8; }
           pixel = data & 0x80? BLACK: WHITE; 
           data <<= 1; bitno--; return pixel;
       }
       col = 0; row++; bitno = 0;
   }
   fatal("Not that many pixels!\n");
} 

void first_pixel(CharInfoRec *G)
{
   row = col = 0;
   p_data = (unsigned char *) G->bits;
   W = G->metrics.rightSideBearing - G->metrics.leftSideBearing;
   H = G->metrics.ascent + G->metrics.descent;
   bitno = 0; 
}

void print_pixmap()
{  int c, r;
   unsigned char *save_p_data;

   save_p_data = p_data;
   if (W*H == 0) return; /* easy */
   for (r = 0; r < H; r++) {
      for (c = 0; c < W; c++)
	 if (next_pixel() == BLACK) putchar('X');
	 else putchar('.');
      putchar('\n');
   }
   /* reset data for scanning pixmap */
   p_data = save_p_data;
   bitno = 0; row = 0; col = 0;
}

/* Next function computes the width as a fix_word. A fix_word is 
   an integer representation for fractions. The decimal point is 
   left from bit 20. (The method is `stolen' from afm2tfm.) */
 
INT32 TFMwidth(int wx)
{  
   return (((wx  / 1000) << 20) +
           (((wx % 1000) << 20) + 500) / 1000) ;
}

static float HXU = -1.0; /* horizontal pixels per design unit */

/* the horizontal escapent is the number of pixels to next origin */
int h_escapement(int wx)
{
   if (HXU == -1.0) 
      HXU = (pointsize * x_resolution) / 72270.0;
   return wx * HXU +  0.5;
}

/* Next stuff is needed by type1 rendering functions */

int CheckFSFormat(format, fmask, bit, byte, scan, glyph, image)
       int format,fmask,*bit,*byte,*scan,*glyph,*image;
{
       *bit = *byte = 1;
       *glyph = *scan = *image = 1;
       return Successful;
 
}
 
char *MakeAtom(p)
       char *p;
{
       return p;
}

GetClientResolutions(resP)
       int *resP;
{
       *resP = 0;
}

char *Xalloc(int size)
{
       return(malloc(size));
}
 
void Xfree(void *p)
{
       free(p);
}

FontDefaultFormat() { ; }
 
FontFileRegisterRenderer() { ; }
 
GenericGetBitmaps() { ; }
GenericGetExtents() { ; }
 
FontParseXLFDName() { ; }
FontComputeInfoAccelerators() { ; }

