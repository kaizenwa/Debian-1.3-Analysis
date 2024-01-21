/* gcc -ansi doesn't predefine `unix', since ANSI forbids it.  And AIX
   generally doesn't predefine unix, who knows why.  */
#ifndef unix
#if defined (__unix__) || defined (_AIX)
#define unix
#endif
#endif

#define USEPXL

/*
 *  default font path
 *  can be overridden by environment variable TEXPXL
 *  or -a command line option
 *  popular choice:
 * #define  FONTAREA       "/usr/local/lib/tex/fonts/pk"
 */
#ifndef FONTAREA
#ifdef vms
#define FONTAREA    "tex$pkdir:"
#else
#ifdef _AMIGA
#define FONTAREA    "TeX:pk"
#else
#define FONTAREA    "/usr/local/lib/tex/pk300"
#endif
#endif
#endif

/* if DO_SUBDIRECTORIES is specified, search to this depth */
#ifndef MAX_SUBDIR_SEARCH_DEPTH 
#define MAX_SUBDIR_SEARCH_DEPTH 10
#endif

/* 
 * if your LaserJet II P or LaserJet III or LaserJet 2000
 * complains about not enough memory, you might try to reduce 
 * the value below or uncomment to use the default settings
 */  
#ifdef LJ2P
#define  MAX_FONTS_PER_PAGE 255         /* maximum number of fonts per page */
#endif


/* Timing is not very portable.... if you have troubles, use
 * -DNO_TIMING in the Makefile
 */

#ifdef u3b2
#define NO_TIMING
#endif
#ifdef _AMIGA
#define NO_TIMING
#endif
#ifdef KPATHSEA
#define NO_TIMING
#endif

#ifndef NO_TIMING
#define TIMING
#endif

/*
 * per default use MakeTexPK in unix environments unless it is not wanted 
 */
#ifdef unix
#ifndef MAKETEXPK
/* name of the program which is called to generate missing pk files
 */
#define MAKETEXPK "MakeTeXPK"
#endif
#endif

#ifdef _AMIGA
#ifndef MAKETEXPK
/* name of the program which is called to generate missing pk files
 */
#define MAKETEXPK "MakeTeXPK"
#endif
#endif



#ifdef NO_MAKETEXPK
#undef MAKETEXPK
#endif

/*
 * assure that LJ2P is defined when LJ4 is defined;
 * compile with support for LJ4's resident fonts
 */
#ifdef LJ4
#define LJ2P
/* te: the code for resident lj-fonts breaks the ps-fonts stuff. This 
 *     is much mure important IMHO
 */
#define LJ_RESIDENT_FONTS
#endif

/*
 * assure that LJ2 is defined when LJ2P is defined
 */
#ifdef LJ2P
#ifndef LJ2
#define LJ2
#endif
#endif

/*
 * assure that LJ is defined when LJ2 of LJ4 is defined
 */
#if defined(LJ2)
#ifndef LJ
#define LJ
#endif
#endif

/*
 * assure that IBM3812 is not defined when LJ is defined
 */ 
#ifdef LJ
#ifdef IBM3812
#undef IBM3812
#endif
#endif

#ifdef LJ_RESIDENT_FONTS
#ifndef DEFAULT_TFM_PATH
#define DEFAULT_TFM_PATH   "/usr/local/lib/tex/fonts"
#endif
#endif


#define  _TRUE      (bool) 1
#define  _FALSE     (bool) 0

#define  STRSIZE         255     /* stringsize for file specifications  */

typedef  char    bool;

/* The smallest signed type: use `signed char' if ANSI C, `short' if
   char is unsigned, otherwise `char'.  */
#ifndef SCHAR_TYPE
#ifdef __STDC__
#define SCHAR_TYPE signed char
#else /* not __STDC */
#ifdef __CHAR_UNSIGNED__
#define SCHAR_TYPE short
#else
#define SCHAR_TYPE char
#endif
#endif /* not __STDC__ */
#endif /* not SCHAR_TYPE */
typedef SCHAR_TYPE signed_char;


#if !defined(u3b2) && !defined(LONG_64_BITS)
#define  ARITHMETIC_RIGHT_SHIFT
#endif

#if SIZEOF_LONG > 4
#define long4 int
#else
#define long4 long
#endif

bool findfile();


/* 
 * maximal number of characters in font file
 * #define  LASTFNTCHAR  127        7-bit classic version
 * #define  LASTFNTCHAR  255        8-bit fonts
 */

#ifdef SEVENBIT 
#define LASTFNTCHAR 127
#else
#define LASTFNTCHAR  255
#endif



/* this information is needed in findfile.c and dvi2xx.c, NO CUSTOMIZATION */
#ifdef LJ
# ifndef MFMODE300
#  define MFMODE300 "cx"     /* mode definition for metafont 300dpi */
# endif
# ifdef LJ4
#  ifndef MFMODE600
#   define MFMODE600 "ljfour"   /* mode definition for metafont 600dpi */
#  endif
# else
#  define MFMODE MFMODE300       /* default mode */
# endif
#endif

#ifdef IBM3812
#define RESOLUTION    240
#ifndef MFMODE
#define MFMODE "ibmteot"    /* mode definition for metafont */
#endif
#endif


#ifdef unix
#define OS "Unix"
#define READ_BINARY     "r"
#define WRITE_BINARY    "w"
#if !defined (STDC_HEADERS) && !defined (labs)
#define labs(x) abs(x)
#endif
#endif
#ifdef MSDOS
#define OS "MS-DOS"
#define READ_BINARY     "rb"
#define WRITE_BINARY    "wb"
#define MSC5
#endif
#ifdef OS2
#define OS "OS/2"
#define READ_BINARY     "rb"
#define WRITE_BINARY    "wb"
#define MSC5
#endif

#ifdef vms
#define OS "VMS"
#include <ssdef.h>
#include <stsdef.h>
#define ftell vms_ftell		    /* use some external routines, because */
#define fseek vms_fseek		    /* of some bugs in the VMS run time    */
#define getchar vms_getchar	    /* library */
#define getenv vms_getenv
#define ungetc vms_ungetc
#define getname vms_getname
#define READ_BINARY     "rb"
#define WRITE_BINARY    "wb","rfm=fix","bls=512","mrs=512" /* fixed records */
#define labs(x) abs(x)
#endif

#ifdef _AMIGA
#define OS "Amiga"
#define READ_BINARY     "r"
#define WRITE_BINARY    "w"
#ifdef __SASC
#define sys_errlist __sys_errlist
#include <stdlib.h>
#endif
#endif


/* Information returned by tfm_read_info. */
typedef struct {
  /* These string lengths are imposed by the TFM format. Either of these
     values may be the empty string.  */
  char coding_scheme[40];
  char family[20];
   
  /* The second fontdimen. */
  unsigned interword;
   
  /* These values are what will work to select the font in PCL. If this
     TFM file doesn't have the `KN' extensions (distinguishable by the
     family == "HPAUTOTFM"). */
#define SPACING_FIXED 0
#define SPACING_PROPORTIONAL 1
  unsigned spacing;
  int weight;
  unsigned style;
  unsigned typeface_id;

  /* TFM files can always have 256 characters, even if we're using the
     old pixel format that only supports 128. The values are fix-words
     scaled by the design size; i.e., straight from the TFM file. */
  long4 widths[256];
} tfm_info_type;
