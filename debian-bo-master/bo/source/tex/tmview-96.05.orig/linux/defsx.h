/* defs for the linux version of tmview */



/***************************************************************************** 
  defaults for commandline options: 
******************************************************************************/

/* set the default resulution in dpi (option -r) */
#define XRES      300      
#define YRES      300


/* set the default papersize in mm (option -p)*/
#define PAPXMM    210.0     
#define PAPYMM    297.0   


/* set the default displaysize (option -d) */
/* only svgamodes 5, 10, 11 and 12 !       */
/* if set, $GSVGAMODE is ignored !         */

/* Example 1: ignore $GSVGAMODE, use mode 10 always */
/* #define VGAXDIM 640 */
/* #define VGAYDIM 480 */

/* Example 2: use $GSVGAMODE. must be set to 5,10,11 or 12 therefore */
#define VGAXDIM 0
#define VGAYDIM 0

/* Matthias Eckermann asked for 16-color-modes ...   */
/* and therefor there's a compiletime option MECK    */
/* When set, forget about anything mentioned above   */
/* When set, 640x480x16 i.e. mode 4 is used          */
/* Should be useful for anybody with vga-support only*/
/* #define  MECK */ 


/* define default printable area  (option -k) */
#define LRANDMM 4.0         
#define RRANDMM 4.0	    
#define ORANDMM 4.0			
#define URANDMM 12.0


/* location of the top left corner from     */
/* the dvifile on the page (options -h -v)  */
#define HOFFMM 25.4 
#define VOFFMM 25.4	


/* set defaults for the options -t, -f and -n */
#define TFMDIR  "./:/usr/lib/texmf/fonts/tmp/tfm//:/usr/lib/texmf/fonts//"
#define PKDIR   "./:/usr/lib/texmf/fonts/tmp/pk//:/usr/lib/texmf/fonts//"
#define PKNAME  "@N.@Mpk"

/* Users kept asking about using kpathsea ... so: */
/* to use kpathsea, define KPATHSEA below. to use tmviews built in    */
/* font searching, don't define KPATHSEA.                             */
/* since I'm not distributing KPATHSEA, you'll have to get the library*/
/* from somewhere. See CTAN. I did test with the library whih comes   */
/* with dviljk-2.5                                                    */ 

/* #define KPATHSEA */
#ifdef KPATHSEA
#define KPATHMODE NULL   /* put f.e. "cx" here, to consist on cx fonts */
#define KPATHMAKE 0      /* put 1 here, to make kpathsea run MakeTeXPK,*/
                         /* if a font is not found.                    */
#define KPATHDEFF "cmr10"/* fallback font                              */
#endif

/* set defaults for the option -s */
#define STARTUPFILENAME "~/.dvisvga"


/* if DOSFILES is defined, a dosfilesystem is expected, */ 
/* incl. ugly naming conventions, like drive-letters and "\" instead ob "/". */
/* Dont use this with linux !!! */
/* (even when using fonts on a mounted dos-partition) */ 

/* #define DOSFILES */       
#ifdef DOSFILES
#define DIRSEPSTR "\\"
#define DIRSEPCHAR '\\'
#else
#define DIRSEPSTR "/"
#define DIRSEPCHAR '/'
#endif

/******************************************************************************
  memory and storage types
******************************************************************************/

/* nothing to be changed in this section ? */


#define SMAX      100   /* dvi stack size */
#define MAXFONTS  256   /* max number of different fonts */


/* 
   for reasons of lazy programing the maximum stringlength of pathnames is
   restricted. this contains any recursivly searched paths. So if you set the
   searchpath for files to the (stupid ?) value of "//", you have to set
   MAXPATHSTR to the length of the longest path on your system.
*/
#define MAXPATHSTR 300

/*
   (not lazy but) because of safty the recursionlevel of recursiv path 
   searching is limited to. While the total number of subdirectories between
   all "//" within a searchstring might be imense, only up to MAXPATHDEP
   will be scanned. 
*/
#define MAXPATHDEP 10

/* 
   since glyphs are expanded from PKfiles before beeing drawn and since
   therefor linear memory is allocated, the following limit of such buffers
   results in to large glyphs not to be drawn. example: 
   using 300dpi glyphs at a maximum size of 21x28cm (600x800pt) result in a
   maximum buffersize of 8,200,000 pixels. because of the greyscaling
   this results in up to 4Mb.
   restricting the maximum glyph size to 7.5x10cm gives a limit of 1Mb.

   Note, that the buffermemory is only allocated when needed, and at the
   minimum size w.r.t. the glyph to be drawn. So hight limits dont hurt.
*/

#define MAXLRUPOOL (1024L*1024L)
#define MAXLINMEM (MAXLRUPOOL/3)

/* please dont touch storge types !!! */
/* they make tmview to go with linux/svgalib */

#define BMLONG                  

#ifdef	BMLONG
#define	BMUNIT			unsigned long
#define	BITS_PER_BMUNIT		32
#define BITS_LOG2               5
#define	BYTES_PER_BMUNIT	4
#endif
#ifdef	BMSHORT
#define	BMUNIT			unsigned short
#define	BITS_PER_BMUNIT		16
#define BITS_LOG2               4
#define	BYTES_PER_BMUNIT	2
#endif
#ifdef BMCHAR
#define BMUNIT                  unsigned char
#define	BITS_PER_BMUNIT		8
#define BITS_LOG2               3
#define	BYTES_PER_BMUNIT	1
#endif


#define BLACKNWHITE 1             /* number of bits per pixel in blacknwhite */

#ifndef MECK
#define COLORS_PER_GREY 16        /* 2^BITS_PER_GREY */
#else
#define COLORS_PER_GREY 8         /* use only 8 of 16 greyscales */
#endif
#define BITS_PER_GREY 4           /* must be a divisor of BITS_PER_BMUNIT */
#define BITS_PER_GREY_LOG2 2      
/* #define GREYINBMU   */         /* waste memory. might be a bit faster    */

#ifdef GREYINBMU
#define GREYSCALE BITS_PER_BMUNIT /* number of bits per pixel in greymodus */
#define GREYSCALE_LOG2 BITS_LOG2
#else
#define GREYSCALE BITS_PER_GREY
#define GREYSCALE_LOG2 BITS_PER_GREY_LOG2
#endif




/******************************************************************************
start up defaults and no gammacorrection 
******************************************************************************/

/* use GAMMA to darken or lighten the displayed glyphs */
/* GAMMA \in(0,\infty), larger GAMMA -> darker drawings */
/* set GAMMA to 1 for neutral greyscales. */
#define GAMMA 1.2   

/* shrinkfactor corresponds to 1/zoomfactor */
/* so the following allows a zooming range from 0.1 to 2 */
#define MAXSHRINK 10.0
#define MINSHRINK 0.5

/* startup with zoomfactor 0.33 */
#define SHRINK 3.0
/* startup in the fast zooming mode, only integer-shrinkfactors */
#define ISHRINKONLY 1      
/* startup in the slow zooming mode, arbitrary shrinkfactors */
/* #define ISHRINKONLY 0      */

/* startup in the stay-where-you-are-page-moving-mode */
#define PAGEMOVETOP 0          
/* startup in the do-a-center-after-page-moving-mode */
/* #define PAGEMOVETOP 1 */          

/* startup in the scroll-over-pages-mode */
#define MOVEOVERPAGES 1           
/* startup in the dont-scroll-over-pages-mode */
/* #define MOVEOVERPAGES 0 */          
         

#define FINE 1.4142          /* faktor used with <f> and <c> */
#define PCOLORS GREYSCALE    /* startup with greyscales */
/* #define PCOLORS GREYSCALE /* startup with blacknwhite */

#define MOUSESPEED  3     /* speed of mousemotion */
#define HASMOUSE          /* do this to have a little mousesoupport */
/* #define HASWINDOWS     /* svgalib is no window system */
#define NOTHINGONSCREEN 1 /* buffer all drawings in the offscreen mem */


#define VERBOSE 0         /* do not be verbose */
#define SAVESTARTUP 0     /* dont save startup values */

/* set the default unit of measurement. you may change this, see <t> */
/* UNIT has to be an integer between 0 and 7, defining: 
/* 0 -> MM, 1 -> CM, and so on ... IN, BP, PT, PC, CC, ... 7 -> DD   */
#define UNIT 0            /* set default unit to MM   */

#define MARKON 0          /* dont show up with marks on */
#define HYPON 0           /* when MARKON is 1, HYPON 1 sets it to halfhyper */

#define MARKSIZE (vgaxdim /35) /* be carefull with this */


/*****************************************************************************
 more or less private stuff.
 nothing to be changed below this line  ! 
******************************************************************************/


#define	SUB(a, b)	((BMUNIT *) (((char *) a) - b))
#define MIN(a, b)	((a)<(b)?(a):(b))
#define MAX(a, b)	((a)>(b)?(a):(b))

#define MMTOPXL(a) ((long) floor(xres*(a)/25.4/fshrink + 0.5))
#define PXLTOMM(a) ((float)((a)*25.4/xres*fshrink))
#define PZTOPXL(pz) ((int) floor(MAX(0,MIN(vgaxdim-1,(pz)/100.0*vgaxdim+0.5))))
#define PXLTOPZ(pxl) ((float)(MAX(0,MIN(100,(pxl)*100.0/vgaxdim))))

#define INSIDEPXL (0.05* MIN(vgaxdim,vgaydim)) 

#define uchar unsigned char
#define ushort unsigned short
#define uint unsigned int
#define ulong unsigned long
#define ptr void *

#define STRCASECMP(a,b) strcasecmp(a,b)
#define STRNCASECMP(a,b,c) strncasecmp(a,b,c)

#define CEIL(a) (ceil(a))
#define FLOOR(a) (floor(a))
#define ROUND(a) ( ((a)>=0 ? floor(a+0.5) : ceil(a-0.5)) )

#define LCEIL(a) ((long)(ceil(a)))
#define LFLOOR(a) ((long)(floor(a)))
#define LROUND(a) ( (long)((a)>=0 ? floor(a+0.5) : ceil(a-0.5)) )

#define ROUNDUP(x,y) (((x)+(y)-1)/(y))
#define	ADD(a, b)	((BMUNIT *) (((char *) a) + b))

typedef struct bitmap {
	short w, h;		/* width and height in pixels */
	short bmu_wide;  	/* scan-line width in BMUNIT  */
	BMUNIT *bits;		/* pointer to the bits */
        char type;              /* number of bits per pixel */
} bitmap;

typedef struct bigbitmap {
        short w, h;
        short bmu_wide;
        BMUNIT **bits;         /*vector of  pointer to rows*/
        char type;
} bigbitmap;

typedef struct chdesc { 
   /* general inf*/ char fty; uchar ch; int fontdataptr; 
   /* pk  section*/ short hof, vof;  bitmap bmp;  long tfw, addr; uchar flag; 
   /* pk shrunken*/ short hof2,vof2; bitmap bmp2; float shrink;
   /* tfm section*/ long  tfmtfh, tfmtfw, tfmtfd; 
} chdesc;

#define PKTYPE 1
#define TFMTYPE 2

typedef struct fontdesc {
  chdesc *chv;          /* array of chdesc        */
  short mch;            /* length of chv          */
  char* fonam;          /* font id: name          */
  char* fopath;         /* .......  path          */
  long  fodvimag;       /* .......  load at mag   */
  long  foscf;          /* .......          scf   */
  long  fodsz;          /* .......          dsz   */
  char* pkfile;         /* pointer to pkfile-name */
  char (* encoding)[5]; /* pointer to encoding    */
  short usecount;       /* how often used         */
} fontdesc;

typedef struct fontdescvect {
  long dfn;  
  int fontdataptr; 
} fontdescvect;

typedef struct {
	int type;   /* Type of anchor: URL, etc from WWW anchor list */
	char *name; /* Name string for anchor (Null by default) */
	char *href; /* A reference from this anchor */
	long llx, lly, urx, ury;         /* Box on page where anchor located */
	long x1pxl, y1pxl, x2pxl, y2pxl; /* Box on page where anchor located */
} HTeX_Anchor;


#define NOTDONE 0
#define DOING 1
#define DONE 2

#define HTeX_A_NAME 1
#define HTeX_A_HREF 2

typedef struct pagelistelement {
  int num;          /* number of page */ 
  long addr;        /* address in dvi-file */
  long count[10];   /* tex counters */
  HTeX_Anchor* anchorlist; /* list of anchors on this page */
  int nanchors;     /* number of anchors in list */
  int maxanchors;   /* maximal number of anchors in list */
  int anchorsdone;  /* !=0 >> page parsed allready */
} pagelistelement;

typedef union drawlistdata { 
  struct { int h; int v; chdesc* chdp; } chr;
  struct { int x; int y; int w; int h; uchar c; } rect;
} drawlistdata;

typedef void (selfdrawfunction)(drawlistdata*);

typedef struct drawlistelement {
  struct drawlistelement *next;
  struct drawlistelement *prev;
  selfdrawfunction *drawmyself;
  union drawlistdata data; 
} drawlistelement; 

typedef struct {
        char *dvifilename;
        int name;	
        int pagenum; 
	int dvixpos, dviypos;
        float fshrink;
        int marksxpxl, marksypxl; 
} bookmark;

typedef struct {
  bookmark** d;
  int n;
} bookmarklist;

typedef struct {
        char *dvifilename;
        float papxmm, papymm, voffmm, hoffmm;
        float centerxmm, centerymm;
        float lrandmm, rrandmm, orandmm, urandmm;
        bookmark lastpos;
        bookmarklist bookmks;
} filemark;

#define MAXMKS 10000  /* must be addresable by int */
#define FILETYP 1
#define MANTYP 2

#define PGMAGIC 42.42e42


/*************************************************************************
now there will be some colorstuff. colors are changable here. but it's
the best to leave this untouched ...
**************************************************************************/


#ifndef MECK
/* definition of colors as bytes in the internal buffer */
/* use lots of different values, so you may design a rainbow tmview */ 
#define WHITECOL          0
#define BLACKCOL          (COLORS_PER_GREY-1)
#define BORDERCOL         COLORS_PER_GREY
#define TEXTCOL           COLORS_PER_GREY+1
#define TEXTBACKCOL       COLORS_PER_GREY+2
#define FRAMECOL	  COLORS_PER_GREY+3
#define TFMCOL            COLORS_PER_GREY+4
#define RECTCOL           COLORS_PER_GREY+5

#define BACKCOLS          COLORS_PER_GREY+16 /* start at least transparent */
#define MARKSCOL          COLORS_PER_GREY+17
#define MARKHCOL          COLORS_PER_GREY+18
#define FOUNDCOL          COLORS_PER_GREY+19
#define HREFCOL           COLORS_PER_GREY+20
#define MARKDCOL          COLORS_PER_GREY+21
#define MIXEDCOL          COLORS_PER_GREY+22 /* mixed all                  */

#define NUMBER_OF_COLORS  COLORS_PER_GREY+23 /*number of colors */

#else  /* MECK set, i.e. 16-color-mode */
/* set up 8 greylevels, 3 solid colors, 3 transparent and one mixed */
/* this is hardwired, cause there are no more colors left           */
/* if you still want to chande the colors, change the tripels below,*/
/* that is, make PALDRED look green or so                           */

#define WHITECOL          0
#define BLACKCOL          7
#define TEXTCOL           7 
#define TEXTBACKCOL       15  
#define FRAMECOL	  8   /* dark red */
#define BORDERCOL         9   /* dark green */
#define TFMCOL            10  /* dark blue */
#define RECTCOL           10 

#define BACKCOLS          11  /* mixing modus only */
#define MARKDCOL          12  /* light red */
#define MARKHCOL          13  /* light green */
#define FOUNDCOL          13 
#define MARKSCOL          14  /* light blue */
#define HREFCOL           14  
#define MIXEDCOL          15  /* mixed all */

#define NUMBER_OF_COLORS  15  /*number of colors */

#endif


/* definition of some rgb triples, 6-bit-per-componente   */
/* in fact beside of greyscales no other colors are used. */
/* to make green more blue, edit here.                    */

#define PALWHITE 63,63,63     /* white */
#define PALLGREY 55,55,55     /* light grey */
#define PALBLACK 0,0,0        /* black */
#define PALDRED 40,10,5       /* dark red */
#define PALDGREEN 0,40,10     /* dark green */
#define PALDBLUE 3,10,40      /* dark blue */
#define PALLRED 63,50,43      /* light red */
#define PALLGREEN 40,63,50    /* light green */
#define PALLBLUE 43,50,63     /* light blue */

/* definition of colors when in 256-color-mode              */
/* this determines the colors on the display                */
/* the following may be changed without any (?) restictions.*/
/* setting                                                  */ 
/* #define SETPALFRAMECOL FRAMECOL,0,63,0                   */
/* results in having a bright green as framecolor           */
/* BUT: when in 16-color-mode i.e. MECK set, things are not */
/* that easy. So dont touch.  */


#define SETPALTEXTCOL     TEXTCOL,PALBLACK 
#define SETPALTEXTBACKCOL TEXTBACKCOL,PALLGREY 
#define SETPALFRAMECOL	  FRAMECOL,PALDRED
#define SETPALBORDERCOL   BORDERCOL,PALDGREEN
#define SETPALTFMCOL      TFMCOL,PALDBLUE
#define SETPALRECTCOL     RECTCOL,PALDBLUE 
#define SETPALMARKSCOL    MARKSCOL,PALLBLUE      
#define SETPALMARKHCOL    MARKHCOL,PALLGREEN
#define SETPALFOUNDCOL    FOUNDCOL,PALLGREEN    
#define SETPALHREFCOL     HREFCOL,PALLBLUE
#define SETPALMARKDCOL    MARKDCOL,PALLRED
#define SETPALMIXEDCOL    MIXEDCOL,PALLGREY


/*************************************************************************
some more dirty things, like limited lenght on input strings ...
all internal, dont touch !!!!!!!!!
**************************************************************************/


#define MAXARGSTR 80

#define COMDO 0
#define COMDEF 1

#define STATUSTYPES 2

#define KEYNOP    128  
#define KEYNEXT   KEYNOP+1
#define KEYPREV   KEYNOP+2
#define KEYUP     KEYNOP+3
#define KEYDOWN   KEYNOP+4
#define KEYRIGHT  KEYNOP+5
#define KEYLEFT   KEYNOP+6
#define KEYCENTER KEYNOP+7
#define KEYFIRST  KEYNOP+8
#define KEYLAST   KEYNOP+9
#define KEYHOME   KEYNOP+10
#define KEYEND    KEYNOP+11
#define KEYESC    KEYNOP+12
#define KEYRET    KEYNOP+13
#define KEYRESIZE KEYNOP+14
#define KEYTAB    KEYNOP+15


































