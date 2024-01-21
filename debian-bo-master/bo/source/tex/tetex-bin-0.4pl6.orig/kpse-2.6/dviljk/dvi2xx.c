#define VERSION "dvilj version 2.5"
/**********************************************************************
 ****************************  Intro  *********************************
 **********************************************************************
 * This program translates TeX's DVI-Code into device dependent
 * code of either the
 *
 *     -   HP-LASERJET+ and compatibles (PCL), or the
 *     -   IBM 3812 pageprinter
 *
 * depending on the preprocessor switches specified before compilation.
 * The program is written to run on a PC XT/AT/PS2 under MS-DOS. It can
 * be compiled nicely with MSC Rel. 3.0-5.1 with option -AL (large memory
 * model).  Take care that in the CONFIG.SYS file the FILES parameter
 * is set to 20; otherwise reduce MAXOPEN.  640K are recommended.
 * I use link option /stack:9000 to increase runtime stack.
 * It also works without modifications under Unix System V.
 **********************************************************************
 *            Adapted for the PC:    Gustaf Neumann
 *            +1002 stuff        University of Economics
 *            +3812 support      Augasse 2-6
 *            +Output buffering      A-1090 Vienna, AUSTRIA
 *            +lpt binary support    Tel. *43-222-340525/533
 *            +pk-89 stuff                   773
 *            +pixelpaths
 *            +alternative directory structure
 *            +code compiles also under Unix V (HP/UX)
 *                      (thx Michael Haberler)
 *            +huge characters (a character bigger than 32K)
 *                      formats PXL1001 and PXL 1002
 *                     (use: raster graphics)
 *            +reduction of the produced code
 *            +new options -X -Y -c -g
 *            +changed options -r (LJ now default from first to last)
 *                     -x,-y  (accept floats)
 *            +new option -z for LJ: print testpage containing
 *                     pagecounter after printjob
 *            +try to overcome font limit on LJ (max 16 fonts/page) and
 *             (max 32 fonts/document):
 *                     additional fonts are drawn as bitmap-
 *                     graphics.
 *            +allows to set character close to the paperedge on LJ
 *            +gf-supprt (by Joe Kelsey joe@Pacer.com)
 *                gf.c and gf.h from mitdevices/dvi2ps
 *            +clipping of rules
 *            +OS/2 defines from Rutger Berns, apprmb@hheouh50.bitnet
 *
 *            BITNET/EARN:       NEUMANN at AWIWUW11
 **********************************************************************
 * fixes in LJ-mode:  rule-drawing,
 *            characters with 127<=height<=200
 *            reset printer at beginning and end of each job
 *            better positioning of rules
 * 30.1.89 (0.48) bug fixed for files containing >32 fonts (thanks A. Brosig),
 *                different font assignment heuristic
 * fixes in 3812-mode:  14.juli 87  positioning of rastered characters
 *            better positioning of rules
 * general fixes
 * 1.1.88         page origin set to 1in/1in (hopefully everywhere)
 * 22.7.88        reset y-position for each page-eject
 * 15.1.89        fixing bug is space allocation for EmitFileName
 *                (thanks to Bernhard Simon)
 * 15.3.91 (0.49) landscape support for lj ii p, lj iii and lj 2000 
 *                fixing rule drawing problems for IBM3812 in landcape mode,
 *                256 character clean (lj family and IBM3812) 
 * 5.5.91 (0.50)  -DSEVENBIT added for older LJ-emulations
 *                -D1, -D2, -D-, -D1-, -D2- options added due to suggestions 
 *                from Tomasz Wolniewicz
 **********************************************************************
 * Preprocessor switches:
 *      #define DEBUG    for massive printing of trace information
 *               when -d cmdline option specified
 *      #define IBM3812  produce output for the IBM3812 pageprinter
 *      #define LJ       produce output for the HP Laserjet+ or LJ II
 *      #define LJ2P     produce output for the HP Laserjet LJ IIP, LJ III
 *                       or LaserJet 2000
 *      #define LJ_LARGE_FONT_MEMORY  large FONT Memory for LJ printer family
 *      #define DRAWGLYPH draws PK-Glyphs on stderr
 *      #define USEPXL   use PXL and PK fonts rather than gf fonts
 */
/**********************************************************************
 ************************  Global Definitions  ************************
 **********************************************************************/
/* #define IBM3812 */
/* #define LJ */
/* #define DRAWGLYPH */

#define KPATHSEA
#ifdef KPATHSEA
#include <kpathsea/config.h>
#include <kpathsea/c-limits.h>
#include <kpathsea/c-memstr.h>
#include <kpathsea/magstep.h>
#include <kpathsea/proginit.h>
#include <kpathsea/progname.h>
#include <kpathsea/tex-glyph.h>
#else
#include <string.h>
#include <stdio.h>
#ifdef  unix
#include <limits.h>
#endif
#endif

#include "config.h"
#include "commands.h"
#include <signal.h>
#include <ctype.h>
#ifdef vms
#include <file.h>
#else
#include <fcntl.h>
#endif
#ifdef MSDOS
#include <dos.h>     /* only for binaryopen on device  */
#endif

#define  DVIFORMAT     2
#define  UNKNOWN      -1
#define  FIRSTFNTCHAR  0

#ifdef   OPEN_MAX                    /* ... in a friendly unix system  */
#ifndef vms
#define  MAXOPEN_OS    (OPEN_MAX - 8)
#else
#define  MAXOPEN_OS    12     /* OPEN_MAX seems to be 8 on VMS systems */
#endif
#else
#define  MAXOPEN_OS    12     /* limit on number of open font files */
#endif

#ifdef LJ_RESIDENT_FONTS
/* we have to read tfm files as well */
#define  MAXOPEN       (MAXOPEN_OS - 1)
#else
#define  MAXOPEN       MAXOPEN_OS
#endif

#define  NFNTCHARS       LASTFNTCHAR+1
#define  STACK_SIZE      100     /* DVI-stack size                     */
#define  NONEXISTANT     -1      /* offset for PXL files not found     */
#define  NO_FILE        ((FILE *)-1)
#define  NEW(A) ((A *)  malloc(sizeof(A)))
#define  EQ(a,b)        (strcmp(a,b)==0)
#define  MM_TO_PXL(x)   (int)(((x)*RESOLUTION*10)/254)
#define  PT_TO_PXL(x)   (int)((long4)((x)*RESOLUTION*100l)/7224)
#define  PT_TO_DVI(x)   (long4)((x)*65536l)
#define  BOPENCMD fopen
#define  BINOPEN(f) BOPENCMD(f,READ_BINARY)
/* SMALL_SIZE characters are loaded into font storage of the printer   */
/* LARGE_SIZE characters are rastered                                  */
/* HUGE_SIZE characters are not loaded into the memory of the host     */
#define  SMALL_SIZE (unsigned char) 0
#define  LARGE_SIZE (unsigned char) 1
#define  HUGE_SIZE  (unsigned char) 2
#define  HUGE_CHAR_PATTERN 32767l
#define  BYTES_PER_PIXEL_LINE 500    /* max number of bytes per pixel line */


#define PK_POST 245
#define PK_PRE 247
#define PK_ID 89

/* to speedup the program a little: redefinition of PixRound and PutWord */
/*#define PIXROUND(x,c) ((((double)x+(double)(c>>1))/(double)c)+0.5)*/
#define PIXROUND(x,c) (((x)+c)/c)
#define PUTWORD(w)  EMITC((char)(w>>8)&0xff); EMITC((char)w&0xff)
/*************************************************************************/
#define  EMIT            fprintf              /* output a formatted string   */
#ifndef vms
# define  EMITB(len,b)   fwrite(b,1,len,outfp)  /* output binary data of len */
#else
    /* VMS doesn't like to use fwrite on a file with fixed record sizes,
       so use number of putc calls */
# define  EMITB(len,b)   for (kk = 0;kk < len; kk++) putc(*(b+kk),outfp);
#endif
#define  EMITWORD(w)     PUTWORD((w))        /* output a 2 byte word of data */

#define  MoveOver(b)  h += (long4) b
#define  MoveDown(a)  v += (long4) a
#define  qfprintf if (!G_quiet) fprintf
#define  qprintf  if (!G_quiet) printf
#define  LARGER(a,b) (((a)>(b)) ? (a) : (b))

#ifdef IBM3812
#define  PRINTER      "IBM 3812 pageprinter"
#define  EMITC(c)      PMPoutC(c)               /* output a single character */
#define  PMPcont(l)    PMPout(-1,(char *)l)         /* next l bytes continuous */
#define  PMPflush      PMPout(0l,"")                     /* flush PMP-buffer */
#define  EMITL(l,d)    PMPout((int)l,d)      /* EMIT-logical: via PMP-buffer */
#define  hconvRESOLUTION   240
#define  vconvRESOLUTION   240
#define  CHAR_WIDTH_LARGE  100       /*  limit for loading into printer font */
#define  CHAR_HEIGTH_LARGE 127       /*  limit for loading into printer font */
#define  OUTBUFSIZE     20000        /*   size of output buffer for PMP cmds */
                      /*   has to be less max(signed int)     */
#define  MAXFONTSTORAGE      130000l /* font storage in the 3812 pageprinter */
#define  EMITFILE_EXTENSION    ".pmp"      /* default extension of emit file */
#define  XDEFAULTOFF    RESOLUTION        /* y default offset on page 1 inch */
#define  YDEFAULTOFF    RESOLUTION        /* y default offset on page 1 inch */
#define  CHARSTRINGMAX  80                /* bufferlength for SetString      */
#define  MAX_PAGE_WIDTH  2040
#define  MAX_PAGE_HEIGHT 3360
/**********************************************************************/
/**************  Positioning for the 3812  ****************************/
/**********************************************************************/
#define VERT_HALF(n) ((short)((n+1)>>1)-1)
#define HOR_HALF(n)  ((short)(n>>1))
#define MoveHor(n)  if ((n)!=0) { PMPcont(3); PMPout(1,"\342"); EMITWORD((n)); }
#define MoveVert(n) if ((n)!=0) { PMPcont(3); PMPout(1,"\343"); EMITWORD((n)); }
#endif

#ifdef LJ
# ifdef LJ4
#  ifdef LJ4L
#   define  PRINTER       "HP Laserjet 4L"
#  else
#   define  PRINTER       "HP Laserjet 4"
#  endif
# else
#  ifdef LJ2P
#   define  PRINTER       "HP LaserJet IIP"
#  else
#   ifdef LJ2 
#    define  PRINTER       "HP LaserJet 2"
#   else
#    define  PRINTER       "HP LaserJet"
#   endif
#  endif
# endif

# ifdef LJ4
#  ifdef LJ4L
int   RESOLUTION = 300;
char *MFMODE     = MFMODE300;
#  else
int   RESOLUTION = 600;
char *MFMODE     = MFMODE600;
#  endif
# else
#  define RESOLUTION 300
# endif
# define  hconvRESOLUTION   RESOLUTION
# define  vconvRESOLUTION   RESOLUTION
# ifdef LJ2
/* the printer limit of the LJ2P is actually 16384x16384, 
  * but to exploit it, one would need lots of memory in the printer
 */
#  define  CHAR_WIDTH_LARGE  200     /* limit for loading into printer font */
#  define  CHAR_HEIGTH_LARGE 255         /* y_offset reaches the same size! */
# else   /* such as LaserJet+, Laserjet II */
#  define  CHAR_WIDTH_LARGE  100     /* limit for loading into printer font */
#  define  CHAR_HEIGTH_LARGE 127         /* y_offset reaches the same size! */
# endif
# define  EMITFILE_EXTENSION  ".lj"       /* default extension of emit file */
# ifndef MAX_FONTS_PER_PAGE
#  define  MAX_FONTS_PER_PAGE 16        /* maximum number of fonts per page */
# endif
# define  HANDLE_MAX_FONTS  255      /* max nr of fonts handled (rasterfont) */
# define  FONTS_DOWNLOADABLE 32    /* max nr of fonts that can be downloaded */
# define  MAXFONTSTORAGE (395l*1024l)                /* standard user memory */
# define  EMITC(c)       putc(c,outfp)          /* output a single character */
# define  EMITL(l,d)     EMITB(l,d)                  /* EMIT-logical = EMITB */
# ifdef LJ4
#  define  XDEFAULTOFF   RESOLUTION 
#  define  YDEFAULTOFF   RESOLUTION
# else
#  define  XDEFAULTOFF   RESOLUTION-54   /*x default offset on page 1in (LJ2)*/
#  define  YDEFAULTOFF   RESOLUTION+9      /* y default offset on page 1inch */
# endif
# define  max(x,y)       if ((y)>(x)) x = y
# ifndef vms
#  define  INT_ASCII(b,i) \
   if (i == 0) b[0] = '\0'; else sprintf((char *)b,"%hd",i)
# else
#  define  INT_ASCII(b,i) \
   if (i == 0) b[0] = '\0'; else sprintf((char *)b,"%d",i)
# endif
#endif
#ifdef SEVENBIT
#define VIS   33
#define VIS2  (VIS+32)
unsigned char
VisChar(c)
unsigned char   c;
{
    c &= 0xff;
    if (c < VIS)
        return ((unsigned char)(160 + c));
    if (c < 128)
        return (c);
    if (c < (255 - VIS2))
        return ((unsigned char)(VIS2 + c));
    return (255);
}
#else
#define VisChar(c) (unsigned char)(c)
#endif


/**********************************************************************/
/***********************  external definitions  ***********************/
/**********************************************************************/

#ifndef _AMIGA
#ifndef unix
long    access();
FILE   *BOPENCMD();
void    exit();
int     fclose();
int     fprintf();
int     fseek();
char   *index();
int     printf();
int     sscanf();
int     strcmp();
char   *strcpy();
#ifdef MSC5
unsigned int strlen();
#endif
void    free();
void    setbuf();
#endif

#ifdef MSDOS
int     intdos();
#endif
#endif


#ifndef USEPXL
/* interface to gf.c */
extern FILE *gfin;
extern int checksum;
extern long4 tfm_wd[], char_pointer[];
extern char char_exists[];
extern int num_cols, num_rows, num_bytes, x_offset, y_offset;
extern unsigned char bits[];
extern int gf_font_max_m, gf_font_max_n, gf_font_min_n;
extern int gettochar();
extern void readbits();
extern void readpost();
extern void seekpost();
extern int seekchar();
#endif


/**********************************************************************/
/*************************  Global Procedures  ************************/
/**********************************************************************/
/* Note: Global procedures are declared here in alphabetical order, with
   those which do not return values typed "void".  Their bodies occur in
   alphabetical order following the main() procedure.  The names are
   kept unique in the first 6 characters for portability. */
double  ActualFactor();
void    AllDone();
#ifdef  MSDOS
void    AssureBinary();  /* DOS and Microsoft C dependent !!! */
#endif
void    CopyFile();
void	CopyHPFile();
void    DecodeArgs();
void    DoBop();
long4    DoConv();
void    DoSpecial();
void    EmitChar();
void    Fatal();
void    FindPostAmblePtr();
void    FormFeed();
void    GetBytes();
void    GetFontDef();
char    *GetKeyStr();
bool    GetKeyVal();
bool    IsSame();
void    lcase();
void    LoadAChar();
long4    NoSignExtend();
/* see cautionary note in code, re arithmetic vs logical shifts */
void    OpenFontFile();
long4    PixRound();
void    PkRaster();
void    PutWord();
void    RasterLine();
void    RasterChar();
void    ReadFontDef();
void    ReadPostAmble();
void    SetChar();
void    SetFntNum();
void    SetPosn();
void    SetRule();
void    SetString();
long4    SignExtend();
/* see cautionary note in code, re arithmetic vs logical shifts */
void    SkipFontDef();
void    Warning();
unsigned char   skip_specials() ;

#ifdef IBM3812
void    PMPout();
void    PMPoutC();
#endif

/**********************************************************************/
/***********************  Font Data Structures  ***********************/
/**********************************************************************/

struct char_entry {             /* character entry */
#ifdef USEPXL
    unsigned short  width, height;      /* width and height in pixels */
    short   xOffset, yOffset, yyOffset; /* x offset and y offset in pixels*/
#endif
    struct {
        bool isloaded;
        union {
            long4    fileOffset;
            long4    *pixptr;
        } address;
    } where;
    long4    tfmw;             /* TFM width                 */
    long4    cw;               /* character width in pixels */
    unsigned char   flag_byte;          /* for PK-files    */
    unsigned char   charsize;
};
struct font_entry {    /* font entry */
    long4    k, c, s, d;
    int     a, l;
    char n[STRSIZE];          /* FNT_DEF command parameters                */
    long4    font_mag;         /* computed from FNT_DEF s and d parameters  */
    /*char psname[STRSIZE];*/ /* PostScript name of the font               */
    char    name[STRSIZE];    /* full name of PXL file                     */
    FILE * font_file_id;      /* file identifier (NO_FILE if none)         */
#ifdef USEPXL
    long4    magnification;    /* magnification read from PXL file          */
    long4    designsize;       /* design size read from PXL file            */
#endif
    struct char_entry ch[NFNTCHARS];   /* character information            */
    struct font_entry *next;
    unsigned short ncdl;      /* #of different chars actually downloaded   */
    unsigned short plusid;    /* Font id in Printer                        */
    bool used_on_this_page;
#ifdef LJ_RESIDENT_FONTS
    bool resident_p;          /* is font resident in printer?              */
    char symbol_set[40];      /* symbol set value (resident fonts)         */
    unsigned short resid;     /* typeface id (resident fonts)              */
    unsigned spacing;         /* 0=monospace, 1=variable (resident fonts)  */
    unsigned style;           /* upright/italic/... (resident fonts)       */
    int weight;               /* regular/bold/... (resident fonts)         */
    double pitch;             /* chars per inch (monospaced resident fonts)*/
#endif
    enum PxlId {
        id1001, id1002, pk89    } id;
#ifdef LJ
    unsigned short max_width, max_height, max_yoff;
#endif
};


struct pixel_list {
    FILE *pixel_file_id;    /* file identifier  */
    int     use_count;      /* count of "opens" */
};
/**********************************************************************/
/*************************  Global Variables  *************************/
/**********************************************************************/
long    FirstPage  = -1000000;  /* first page to print (uses count0)   */
long    LastPage   = 1000000;   /* last page to print                  */
long    PrintPages = 1000000;   /* nr of pages to print                */
bool    FirstPageSpecified = _FALSE;
bool    LastPageSpecified = _FALSE;
char    G_progname[STRSIZE];    /* program name                        */
char    filename[STRSIZE];      /* DVI file name                       */
char    rootname[STRSIZE];      /* DVI filename without extension      */
char    *HeaderFileName = "";   /* file name & path of Headerfile      */
char    *EmitFileName = "";     /* file name & path for output         */
#ifdef IBM3812
bool    FirstAlternate = _FALSE; /* first page from alternate casette ? */
#endif
bool    Reverse = _FALSE;        /* process DVI pages in reverse order? */
bool    Landscape = _FALSE;      /* print document in ladscape mode     */
bool    ResetPrinter = _TRUE;    /* reset printer at the begin of the job*/
bool    DoublePage = _FALSE;     /* print on both sides of a paper      */
bool    PrintSecondPart = _TRUE; /* print First Part when DoublePage    */
bool    PrintFirstPart = _TRUE;  /* print Second Part when DoublePage   */
bool    PrintEmptyPages = _TRUE; /* print Empty pages in DoublePage mode*/
short   PageParity = 1;
#ifdef MAKETEXPK
#ifdef NOMAKETEXPK
#define MAKETEXPK_BY_DEFAULT_VALUE 0
#else
#define MAKETEXPK_BY_DEFAULT_VALUE 1
#endif
bool    makeTexPK = MAKETEXPK_BY_DEFAULT_VALUE;
#endif

#ifdef LJ
#ifdef LJ2P
int     DuplexMode = 0;
#endif
#ifdef LJ4
bool    econoMode = _FALSE;
#endif
bool    PrintTestPage = _FALSE; /* print testpage with pagecounter after job */
unsigned short pagesize = 0;    /* page size value                      */
unsigned short pgsiz_dots = 0;  /* page size in dots (for rule-clipping)*/
#endif


#ifndef vms
short   G_errenc = 0;           /* has an error been encountered?      */
#else
long4    G_errenc = SS$_NORMAL;  /* has an error been encountered?      */
#endif
bool    G_header = _FALSE;      /* copy header file to output?         */
bool    G_quiet = _FALSE;       /* for quiet operation                 */
bool    G_noverbatim = _TRUE;   /* inform user about pxl-files used    */
bool    G_nowarn = _FALSE;      /* don't print out warnings            */
short   x_origin;               /* x-origin in dots                    */
short   y_origin;               /* y-origin in dots                    */
short   x_goffset;              /* global x-offset in dots             */
short   y_goffset;              /* global y-offset in dots             */
unsigned short ncopies = 1;     /* number of copies to print           */
long4    hconv, vconv;           /* converts DVI units to pixels        */
long4    den;                    /* denominator specified in preamble   */
long4    num;                    /* numerator specified in preamble     */
long4    h;                      /* current horizontal position         */
long4    hh = 0;                 /* current h on device                 */
long4    v;                      /* current vertical position           */
long4    vv = 0;                 /* current v on device                 */
long4    mag;                    /* magnification specified in preamble */
long    usermag = 0;            /* user specified magnification        */
int     ndone = 0;              /* number of pages converted           */
int     nopen = 0;              /* number of open PXL files            */
#ifdef vms
int	kk;			/* loop variable for EMITB	       */
#endif
FILE  *outfp = NULL;            /* output file                         */
FILE  *pxlfp;                   /* PXL file pointer                    */
FILE  *dvifp  = NULL;           /* DVI file pointer                    */
struct font_entry *prevfont = NULL; /* font_entry pointer previous font*/
struct font_entry *fontptr;     /* font_entry pointer                  */
struct font_entry *hfontptr = NULL; /* font_entry pointer              */
struct font_entry *pfontptr = NULL; /* previous font_entry pointer     */
struct pixel_list pixel_files[MAXOPEN+1]; /* list of open PXL files    */
long4    postambleptr;           /* Pointer to the postamble            */
long4    ppagep;                 /* previous page pointer               */
static int      last_ry = UNKNOWN;      /* last y-position on page     */
static int      last_rx = UNKNOWN;      /* last x-position on page     */
long4   StartPrintPages;         /* notpad for double paged output      */
int    WouldPrint    = 0;
bool   ZeroPage = _FALSE;       /* Document starts with a Zero Page    */
bool   EvenPage = _FALSE;       /* Document starts with an even Page   */
long4   LastPtobePrinted = 0;
int    G_ncdl = 0;

long4    allocated_storage = 0; /* size of mallocated storage (statistics) */
long4    power[32] ;
long4    gpower[33] ;


#ifdef DEBUG
int Debug = 0;
#endif

#ifdef LJ
int   fonts_used_on_this_page = MAX_FONTS_PER_PAGE+1;
char  rasterfont[HANDLE_MAX_FONTS];
    /* raster if fonts/page>MAX_FONTS_PER_PAGE*/
#ifdef LJ_RESIDENT_FONTS
unsigned resident_count = 0;
#ifndef KPATHSEA
char *TFMpath = DEFAULT_TFM_PATH;
#endif
#endif
#endif

long4     used_fontstorage = 0;

#ifdef IBM3812
char    PMPformat[20];
char    CharString[CHARSTRINGMAX];
unsigned int CharStringPos = 0;
#define CharStringOut \
    if (CharStringPos>0) { \
        PMPcont(CharStringPos+1);\
        PMPoutC((unsigned char)CharStringPos);\
        PMPout(CharStringPos, CharString); \
        CharStringPos=0; }
#endif

#ifdef TIMING
/************************timing stuff*********************/
#ifdef BSD_TIME_CALLS
#ifndef vms
#include <sys/timeb.h>
#else
#include <timeb.h>
#endif
struct timeb timebuffer;
double  start_time;
#else
#include <sys/time.h>
struct timeval Tp;
double  start_time;
#endif
#endif



/**********************************************************************/
/*******************************  main  *******************************/
/**********************************************************************/
void
main(argc, argv)
int     argc;
char    *argv[];
{
    struct stack_entry {  /* stack entry */
        long4    h, v, w, x, y, z;  /* what's on stack */
    };
    short   command;          /* current command                         */
    long4    count[10];        /* the 10 counters at begining of each page*/
    long4    cpagep = 0;       /* current page pointer                    */
    bool Emitting = _FALSE;   /* outputting typsetting instructions?     */
    int     i;                /* command parameter; loop index           */
    int     k;                /* temporary parameter                     */
    char    n[STRSIZE];       /* command parameter                       */
    int     PassNo = 0;       /* which pass over the DVI page are we on? */
    bool SkipMode = _FALSE;   /* in skip mode flag                       */
    int     sp = 0;           /* stack pointer                           */
    struct stack_entry stack[STACK_SIZE];  /* stack                      */
    char    SpecialStr[STRSIZE]; /* "\special" strings                   */
    long4    val, val2;        /* temporarys to hold command information  */
    long4    w = 0;            /* current horizontal spacing              */
    long4    x = 0;            /* current horizontal spacing              */
    long4    y = 0;            /* current vertical spacing                */
    long4    z = 0;            /* current vertical spacing                */


#ifdef vms
    extern noshare int errno;
    extern noshare char *sys_errlist[];
#else
#ifndef HAVE_EXTERN_SYS_ERRLIST
    extern  char *sys_errlist[];
#endif
    extern  int     errno;
#endif

    x_origin = XDEFAULTOFF; /* x-origin in dots                    */
    y_origin = YDEFAULTOFF; /* y-origin in dots                    */

    setbuf(stderr, NULL);
    (void) strcpy(G_progname, argv[0]);
#ifdef KPATHSEA
    kpse_set_progname (argv[0]);
#endif
    DecodeArgs( argc, argv );

#ifdef KPATHSEA
    kpse_init_prog ("DVILJ", RESOLUTION, MFMODE, makeTexPK, "cmr10");
#endif

    power [ 0 ] = 1 ;
    for ( i = 1 ; i <= 31 ; i ++)
        power [ i ] = power [ i - 1 ] << 1 ;
    gpower[0] = 0l ;
    for ( i = 1 ; i <= 32 ; i ++)
        gpower[i] = gpower[i - 1] + power[i - 1] ;

    if ((i = (int) NoSignExtend(dvifp, 1)) != PRE)  {
        Fatal(
        "%s: PRE doesn't occur first--are you sure this is a DVI file?\n\n",
                G_progname);
    }
    i = (int) SignExtend(dvifp, 1);
    if (i != DVIFORMAT)  {
        Fatal( "%s: DVI format = %d, can only process DVI format %d files\n\n",
                G_progname, i, DVIFORMAT);
    }

    if (*EmitFileName == '-')
        outfp = stdout;
    else
        if ((outfp = fopen(EmitFileName, WRITE_BINARY)) == NULL)
        Fatal("opening output file: fopen(%s) : %s",
            EmitFileName,sys_errlist[errno]);

#ifdef MSDOS
    AssureBinary(outfp);
#endif

#ifdef TIMING
#ifdef BSD_TIME_CALLS
    ftime(&timebuffer);
    start_time = timebuffer.time + (float)(timebuffer.millitm) / 1000.0;
#else
    gettimeofday(&Tp, NULL);
    start_time = Tp.tv_sec + ((float)(Tp.tv_usec))/ 1000000.0;
#endif
#endif

    /* it is important that these be the very first things output !!! */
    if ( G_header )
        CopyFile( HeaderFileName );

    /*****************************/
    /*for( i0=0; i0<nif; i0++ )  */    /* copy all included files */
    /*    CopyFile( Ifile[i0] ); */
    /*****************************/

#ifdef IBM3812
    PMPout(3, "\307\310\366");          /* unload all fonts and macros */
    EMITWORD(MAX_PAGE_WIDTH);
    EMITWORD(MAX_PAGE_HEIGHT);
    if (Landscape)
        PMPout(2, "\322\1");
#endif
#ifdef LJ
    if (ResetPrinter)
      EMIT(outfp, "\033E");
#ifdef LJ4
    EMIT(outfp, "\033%%-12345X@PJL SET RESOLUTION=%u\012", RESOLUTION);
    EMIT(outfp, "@PJL SET PAGEPROTECT=OFF\012");
    EMIT(outfp, "@PJL ENTER LANGUAGE=PCL\012");
    EMIT(outfp, "\033&u%uD\033*t%uR", RESOLUTION, RESOLUTION);
    if (econoMode)
      EMIT(outfp, "\033*v1T");
#endif
#ifdef LJ2P
    if (DuplexMode) 
      EMIT(outfp, "\033&l%dH", DuplexMode); 
#endif
    if (Landscape)
      EMIT(outfp, "\033&l1O\033*rF");
    if (pagesize>0)
#ifndef vms
      EMIT(outfp, "\033&l%hdaE\033&aL", pagesize);
#else
      EMIT(outfp, "\033&l%daE\033&aL", pagesize);
#endif
    else
      EMIT(outfp, "\033&lE\033&aL");

    if (ncopies>1)
#ifndef vms
      EMIT(outfp, "\033&l%hdX", ncopies);
#else
      EMIT(outfp, "\033&l%dX", ncopies);
#endif
#endif
    if (DoublePage) {
         StartPrintPages = PrintPages;
#ifdef IBM3812
         Reverse = (bool)!Reverse; /* perverse and strange */
#endif

    }
    if (Reverse) {
#ifdef DEBUG
        if (Debug)
              fprintf(stderr, "reverse\n");
#endif
        ReadPostAmble(_TRUE);
        fseek(dvifp, ppagep, 0);
    } else {
        ReadPostAmble(_TRUE);
        fseek(dvifp,  14l, 0);
        k = (int) NoSignExtend(dvifp, 1);
        GetBytes(dvifp, n, k);
    }
    PassNo = 0;

    while (_TRUE)  {
        command = (short) NoSignExtend(dvifp, 1)   ;
#ifdef DEBUG
        if (Debug)
               fprintf(stderr,"CMD:\t%d\n", command);
#endif
        switch (command)  {
        case SET1:
        case SET2:
        case SET3:
        case SET4:
            val = NoSignExtend(dvifp, (int) command - SET1 + 1);
            if (!SkipMode)
                SetChar(val, command, PassNo, _TRUE,_FALSE);
            break;
        case SET_RULE:
            val = NoSignExtend(dvifp, 4);
            val2 = NoSignExtend(dvifp, 4);
            if (Emitting)
                SetRule(val, val2, 1);
            break;
        case PUT1:
        case PUT2:
        case PUT3:
        case PUT4:
            val = NoSignExtend(dvifp, (int) command - PUT1 + 1);
            if (!SkipMode)
                SetChar(val, command, PassNo, _TRUE,_FALSE);
            break;
        case PUT_RULE:
            val = NoSignExtend(dvifp, 4);
            val2 = NoSignExtend(dvifp, 4);
            if (Emitting)
                SetRule(val, val2, 0);
            break;
        case NOP:
            break;
        case BOP:
            cpagep = ftell(dvifp) - 1;
            for (i = 0; i <= 9; i++)
                count[i] = NoSignExtend(dvifp, 4);
            ppagep = NoSignExtend(dvifp, 4);
            h = v = w = x = y = z = 0;
            hh = vv = 0;
            sp = 0;
            fontptr = NULL;
            prevfont = NULL;
            DoBop();
/*
fprintf(stderr,"skimode %d, count %d, F %d, L %d\n",
	(int)SkipMode,(int)count[0],(int)FirstPageSpecified,(int)LastPageSpecified);
*/
            SkipMode = (bool) ((FirstPageSpecified && count[0] < FirstPage) || 
                                              (LastPageSpecified && count[0] > LastPage ));
/*
fprintf(stderr,"skimode %d, count %d, F %d, L %d\n",
	(int)SkipMode,(int)count[0],(int)FirstPageSpecified,(int)LastPageSpecified);
*/

            if (DoublePage && !SkipMode) {
               if (PassNo == 0) {
                  LastPtobePrinted=count[0];
                  if (!Reverse && (WouldPrint == 0)) {
                      if (count[0] == 0l) {
                         ZeroPage = _TRUE;
                         EvenPage = _FALSE;
                      }
                      else {
                         EvenPage = (bool) ( (count[0]<0? labs(count[0])+1: count[0]) %2 == 0);

                         if (PrintEmptyPages && EvenPage && PageParity==1) {
                           WouldPrint ++;
			   if (PrintFirstPart) {
			     qfprintf(stderr,"[EvenPage] ");
			     FormFeed();
			   }
                         }
                      }
                  }
                  WouldPrint ++;
/*
    fprintf(stderr, "doublepage %d, page parity = %d, 1=%d 2=%d, Reverse %d, WouldPrint %d, fpZ %d\n",
        (int)DoublePage, (int)PageParity,(int)PrintFirstPart,(int)PrintSecondPart,
        (int)Reverse, (int)WouldPrint, (int)ZeroPage);
*/
		}
	       if (!PrintFirstPart && PageParity==1) { 
		 if (count[0]==0l) {
		   ZeroPage = _TRUE;
		   EvenPage = _FALSE;
		 }
		 SkipMode = _TRUE;
	       }
               else { 
/*
fprintf(stderr,"FirstPart\n count %d, mod %d, pp %d\n",(int)count[0],(int)count[0]%2,PageParity);
*/
		 SkipMode = (bool)(PageParity != (short)(
							 (count[0]<0?  labs(count[0])+1: count[0])%2));
		 if (count[0]==0l) SkipMode=(bool)!SkipMode;
	       }

            }
            Emitting = (bool)((PassNo != 0) && !SkipMode);
/*
fprintf(stderr,"Emitting= %d, PassNo=%d, SkipMode = %d\n",(int)Emitting,(int)PassNo,(int)SkipMode);
*/
            if ( !SkipMode ) {
                if ( PassNo == 0)
                    qfprintf(stderr, "[%ld", (long)count[0]);
            }
            break;
        case EOP:
            if ( !SkipMode ) {
                if ( PassNo == 0 ) {
                    /* start second pass on current page */
                    fseek(dvifp, cpagep, 0);
                    PassNo = 1;
#ifdef DEBUG
		    if (Debug)
                      fprintf(stderr,"\nStarting second pass\n");
#endif

                } else {
                   /* end of second pass, and of page processing */

                    last_ry = UNKNOWN;
                    FormFeed();
                    ++ndone;

                    qfprintf(stderr, "] ");
                    if ( (ndone % 10) == 0 )
                          qfprintf(stderr, "\n");

                    if (DoublePage) --PrintPages;
                    if (--PrintPages < 1) AllDone(_TRUE);
                    PassNo = 0;
                }
            } else
                PassNo = 0;

            if ( PassNo == 0 && Reverse ) {
                if ( ppagep > 0 )
                    fseek(dvifp, ppagep, 0);
                else {
                   if (DoublePage && !SkipMode)
                     ZeroPage = (bool)(count[0]==0l);

                   if (ZeroPage)
                     EvenPage = _FALSE;
                   else
                     EvenPage = (bool) ((int)LastPtobePrinted%2 == 0);

                   AllDone(_FALSE);
                }
            }
            break;
        case PUSH:
            if (sp >= STACK_SIZE)
                Fatal("stack overflow");
            stack[sp].h = h;
            stack[sp].v = v;
            stack[sp].w = w;
            stack[sp].x = x;
            stack[sp].y = y;
            stack[sp].z = z;
            sp++;
            break;
        case POP:
            --sp;
            if (sp < 0)
                Fatal("stack underflow");
            h = stack[sp].h;
            v = stack[sp].v;
            w = stack[sp].w;
            x = stack[sp].x;
            y = stack[sp].y;
            z = stack[sp].z;
            break;
        case RIGHT1:
        case RIGHT2:
        case RIGHT3:
        case RIGHT4:
            val = SignExtend(dvifp, (int) command - RIGHT1 + 1);
            if (Emitting)
                MoveOver(val);
            break;
        case W0:
            if (Emitting)
                MoveOver(w);
            break;
        case W1:
        case W2:
        case W3:
        case W4:
            w = SignExtend(dvifp, (int)command - W1 + 1);
            if (Emitting)
                MoveOver(w);
            break;
        case X0:
            if (Emitting)
                MoveOver(x);
            break;
        case X1:
        case X2:
        case X3:
        case X4:
            x = SignExtend(dvifp, (int)command - X1 + 1);
            if (Emitting)
                MoveOver(x);
            break;
        case DOWN1:
        case DOWN2:
        case DOWN3:
        case DOWN4:
            val = SignExtend(dvifp, (int)command - DOWN1 + 1);
            if (Emitting)
                MoveDown(val);
            break;
        case Y0:
            if (Emitting)
                MoveDown(y);
            break;
        case Y1:
        case Y2:
        case Y3:
        case Y4:
            y = SignExtend(dvifp, (int)command - Y1 + 1);
            if (Emitting)
                MoveDown(y);
            break;
        case Z0:
            if (Emitting)
                MoveDown(z);
            break;
        case Z1:
        case Z2:
        case Z3:
        case Z4:
            z = SignExtend(dvifp, (int)command - Z1 + 1);
            if (Emitting)
                MoveDown(z);
            break;
        case FNT1:
        case FNT2:
        case FNT3:
        case FNT4:
            if (!SkipMode) {
                SetFntNum(NoSignExtend(dvifp, (int)command -FNT1 +1),Emitting);
            }
            break;
        case XXX1:
        case XXX2:
        case XXX3:
        case XXX4:
            k = (int) NoSignExtend(dvifp, (int)command - XXX1 + 1);
            GetBytes(dvifp, SpecialStr, k);
            if (Emitting)
                DoSpecial(SpecialStr, k);
            break;
        case FNT_DEF1:
        case FNT_DEF2:
        case FNT_DEF3:
        case FNT_DEF4:
            k = (int) NoSignExtend(dvifp, (int)command - FNT_DEF1 + 1);
            SkipFontDef();    /* SkipFontDef(k); */
            break;
        case PRE:
            Fatal("PRE occurs within file");
            break;
        case POST:
            AllDone(_FALSE);
            PassNo = 0;
            break;
        case POST_POST:
            Fatal("POST_POST with no preceding POST");
            break;
        default:
            if (command >= FONT_00 && command <= FONT_63) {
                if (!SkipMode)
                    SetFntNum((long4) command - FONT_00, Emitting);
            } else if (command >= SETC_000 && command <= SETC_127) {
                if (!SkipMode) {
                    SetString(command, PassNo);
                }
            } else
                Fatal("%d is an undefined command", command);
            break;
        }
    } /* while _TRUE */
}


/*-->ActualFactor*/
/**********************************************************************/
/**************************  ActualFactor  ****************************/
/**********************************************************************/
double  /* compute the actual size factor given the approximation */
ActualFactor(unmodsize)
long4    unmodsize;                 /* actually factor * 1000 */
{
    double  realsize;     /* the actual magnification factor */
    realsize = (double)unmodsize / 1000.0;
    if (abs((int)(unmodsize - 1095l))<2)
        realsize = 1.095445115; /*stephalf*/
    else if (abs((int)(unmodsize - 1315l))<2)
        realsize = 1.31453414; /*stepihalf*/
    else if (abs((int)(unmodsize - 1577l))<2)
        realsize = 1.57744097; /*stepiihalf*/
    else if (abs((int)(unmodsize - 1893l))<2)
        realsize = 1.89292916; /*stepiiihalf*/
    else if (abs((int)(unmodsize - 2074l))<2)
        realsize = 2.0736;   /*stepiv*/
    else if (abs((int)(unmodsize - 2488l))<2)
        realsize = 2.48832;  /*stepv*/
    else if (abs((int)(unmodsize - 2986l))<2)
        realsize = 2.985984; /*stepvi*/
    /* the remaining magnification steps are represented with sufficient
       accuracy already */
    return(realsize);
}


/*-->AllDone*/
/**********************************************************************/
/****************************** AllDone  ******************************/
/**********************************************************************/
void
AllDone(PFlag)
bool PFlag;
{
#ifdef TIMING
    double  time;
#endif

    if (DoublePage && (PageParity==1)) { /* Shall we go around again?*/
        int k;
        char    n[STRSIZE];

        if (PrintEmptyPages && EvenPage && Reverse && PrintFirstPart) {
            WouldPrint ++;
            qfprintf(stderr,"[EvenPage] ");
            FormFeed();
        }
#ifdef LJ
        Reverse = (bool)!Reverse;
#endif
        if (Reverse) {
            if (!PFlag) {
               fseek (dvifp, postambleptr, 0);
               (void) NoSignExtend(dvifp, 1);
               ppagep = NoSignExtend(dvifp, 4);
            }
            fseek(dvifp, ppagep, 0);
        } else {
            fseek(dvifp,  14l, 0);
        k = (int) NoSignExtend(dvifp, 1);
        GetBytes(dvifp, n, k);
        }

	if (PrintSecondPart) {
	  if (PrintFirstPart) {
	    qfprintf(stderr,"\n----------------------starting second pass\n");
#ifdef LJ
	    EMIT(outfp, "\033&l2H"); /* Manual Feed */
#endif
#ifdef IBM3812
	    PMPout(6,"\326\001\305\300\326\252");
	    /* set display; ring bell; stop; clear display */
	    PMPflush;
#endif
	  }

	  if (PrintEmptyPages && Reverse) {
	    if (ZeroPage) WouldPrint++;
	    if ((WouldPrint%2) == 1) {
	      qfprintf(stderr,"[Padding] ");
	      FormFeed();
	    }
	  }
	  WouldPrint = 0;
	  if (PrintEmptyPages && !Reverse && ZeroPage) {
	    WouldPrint++;
	    qfprintf(stderr,"[ZeroPage] ");
	    FormFeed();
	  }
	  PageParity = 0;
	  PrintPages = StartPrintPages;
	  return;
	}
    }

    if (EvenPage && DoublePage && !Reverse) WouldPrint++;

    if (PrintEmptyPages && DoublePage && PrintSecondPart) {
        if (Reverse) {
	  if (ZeroPage) {
            WouldPrint ++;
            qfprintf(stderr,"[ZeroPage] ");
            FormFeed();
	  }
        }
        else if ((WouldPrint%2) != 0) {
            qfprintf(stderr,"[Padding] ");
            FormFeed();
        }
    }

#ifdef IBM3812
    PMPout(10, "\373\010PMP.init");  /* re-init printer  */
    PMPflush;

    if (used_fontstorage > MAXFONTSTORAGE) {
        Warning("\n\7used font_storage of %s: %ld Bytes (of %ld)\7",
            PRINTER, (long)used_fontstorage, (long)MAXFONTSTORAGE);
        Warning("Try to format file in separate runs!");
    } else
        qfprintf(stderr,
           "\nAll done, used font_storage of %s: %ld Bytes (of %ld)",
             PRINTER, (long)used_fontstorage, (long)MAXFONTSTORAGE);
#endif
#ifdef LJ
    qfprintf(stderr, "\nAll done, used font_storage of %s: %ld Bytes",
             PRINTER, (long)used_fontstorage);
#ifdef LJ_RESIDENT_FONTS
    qfprintf(stderr, " + %d resident font%s", resident_count,
             resident_count == 1 ? "" : "s");
#endif
    EMIT(outfp, "\033E");
#ifdef LJ4
    EMIT(outfp, "\033%%-12345X");   /* what does it? */
#endif
    if (PrintTestPage) EMIT(outfp, "\033z");
#ifdef vms
    /* last record is not flushed to file, unless it is completely filled */
    for (kk = (int)((*outfp)->_cnt); kk > 0; --kk) putc('\0',outfp);
    fflush(outfp);
#endif
#endif

    if (!G_quiet) {
        fprintf(stderr,"\nDynamically allocated storage: %ld Bytes \n",
                    (long)allocated_storage);
        fprintf(stderr,"%d characters downloaded as soft fonts\n", G_ncdl);

#ifdef TIMING
#ifdef BSD_TIME_CALLS
        ftime(&timebuffer);
        time = (timebuffer.time + (float)(timebuffer.millitm)/1000.0) 
	       - start_time;
#else
	gettimeofday(&Tp, NULL);
	time = (Tp.tv_sec + (float)(Tp.tv_usec)/1000000.0) - start_time;
#endif

        if (ndone > 0) {
            fprintf(stderr,
           "Time of complete run: %.2f seconds, %d page(s), %.2f seconds/page",
                      time, ndone, time / ndone);
            fprintf(stderr," (%.2f ppm)\n",(ndone * 60) / time);
        }
        fprintf(stderr,"\n");
#endif
    }

    exit(G_errenc);
}


/*-->CopyFile*/   /* copy a file straight through to output */
/*********************************************************************/
/***************************** CopyFile ******************************/
/*********************************************************************/
void
CopyFile( str )
char    *str;
{
    FILE    * spfp;
    char    t;
    if ( (spfp = BINOPEN(str)) == NULL ) {
        Warning("Unable to open file %s", str );
        return;
    }
    qfprintf(stderr," [%s", str);
    for (t = (char)getc(spfp); !feof(spfp); t = (char)getc(spfp))
        putc(t, outfp);
    fclose(spfp);
    qfprintf(stderr,"]");
}


/*-->CopyHPFile*/  /* copy a HP file to output removing unwanted control codes*/
/*********************************************************************/
/***************************** CopyHPFile ******************************/
/*********************************************************************/
void
CopyHPFile( str )
char    *str;
{
    FILE    * spfp;
    char    t,numstr[20];
    int		count,rx,ry,miny,minx,num;
    
    if ( (spfp = BINOPEN(str)) == NULL ) {
        Warning("Unable to open file %s", str );
        return;
    }	
    minx=32767;			/* Set to a high value initially */
    miny=32767;
    
/* Pass through the input PCL file twice.  The first time find the smallest  */
/* x- and y-offsets so that they can be subtracted out when sending          */
/* positioning commands later.   The second pass strips the unwanted commands */
/* from the input file and outputs the rest */
    
    qfprintf(stderr," [%s", str);
    for (t = (char)getc(spfp); !feof(spfp); t = (char)getc(spfp))
    {
      if (t==0x1B)	/* Find the escape character */
    	{
	  t=(char) getc(spfp);
	  if (t==0x2A) 	/* This indiactes the start of a graphics command */
	    {    								

	      t=(char) getc(spfp);
	      switch(t)
		{
		case(0x70):
		  /* These are the graphics positioning commands */
		  /* Find the smallest x and y offsets */ 
		  num=0;
		  count=0;
		  for (t=(char) getc(spfp);t<0x40;t=(char) getc(spfp))
		    numstr[count++]=t;
		  numstr[count]=0;
		  num= atoi (numstr);
		  
		  /* Take into account the possible different ordering */
		  /* of the commands (x first, y first) */
						
		  if ((t==0x59)||(t==0x79))
		    {
		      if (numstr[0]!='+'&&numstr[0]!='-'&&numstr[0]!='-')
			if (num<miny) miny=num;
		      if (t==0x79)
			{
			  num=0;
			  count=0;
			  for (t=(char) getc(spfp);t<0x40;t=(char) getc(spfp))
			    numstr[count++]=t;
			  numstr[count]=0;
			  num=atoi (numstr);
			  if(numstr[0]!='+'&&numstr[0]!='-'&&numstr[0]!='-')
			    if (num<minx) minx=num;
			}
		    }
		  if ((t==0x58)||(t==0x78))
		    {
		      if (numstr[0]!='+'&&numstr[0]!='-'&&numstr[0]!='-')
			if (num<minx) minx=num;
		      if (t==0x78)
			{
			  num=0;
			  count=0;
			  for (t=(char) getc(spfp);t<0x40;t=(char) getc(spfp))
			    numstr[count++]=t;
			  numstr[count]=0;
			  num= atoi (numstr);
			  if (numstr[0]!='+'&&numstr[0]!='-'&&numstr[0]!='-')
			    if (num<miny) miny=num;
			}
		    }
		  break;
		  /* Ignore all other commands for the moment - just read them */							
		case(0x74):	
		  for (t=(char) getc(spfp);t != 0x52; t=(char) getc(spfp));
		  break;
		  
		case(0x72):
		  for (t=(char) getc(spfp);((t< 0x40)||(t>0x60)); t=(char) getc(spfp));
		  break;

		case(0x62):
		  num=0;
		  count=0;
		  /* Read in the correct number of bytes of raster graphics */
		  /* so that we don't get commands and raster graphics confused */
		  
		  for (t=(char) getc(spfp);((t<0x40)||(t>=0x60));t=(char) getc(spfp))
		    numstr[count++]=t;
		  numstr[count]=0;
		  if (t==0x4D)
		    for(t=numstr[count=0];t!=0;t=numstr[++count]);
		  if (t==0x57)
		    {
		      for(t=numstr[count=0];t!=0;t=numstr[++count]);
		      for(count= atoi(numstr);count>0;count--)
			t=(char) getc(spfp);	
		    }
		  break;
							    
		case(0x63):
		  for (t=(char) getc(spfp);((t< 0x40)||(t>0x60)); t=(char) getc(spfp));
		  break;		    								
		  
		default:
		  break;

    				
		}
	    }
    	}
    }
    fclose(spfp);
    qfprintf(stderr,"]");
       
    if ( (spfp = BINOPEN(str)) == NULL ) {
      Warning("Unable to open file %s", str );
      return;
    }
    qfprintf(stderr," [%s", str);

/* Pass through the input file again but this time output the */
/* retained PCL commands */

    for (t = (char)getc(spfp); !feof(spfp); t = (char)getc(spfp))
      {
    	if (t==0x1B)
	  {
	    t=(char) getc(spfp);
	    if (t==0x2A)
	      {    								
		t=(char) getc(spfp);
		switch(t)
		  {
		  case(0x70):
		    num=0;
		    count=0;
		    for (t=(char) getc(spfp);t<0x40;t=(char) getc(spfp))
		      numstr[count++]=t;
		    numstr[count]=0;
		    num= atoi (numstr);
		    if ((t==0x59)||(t==0x79))
		      {
			if (numstr[0]!='+'&&numstr[0]!='-')
			  {
			    /* Subtract the minimum offset found in first pass */
			    /* and add in the current vertical offset */
			    ry=num-miny+(int)PIXROUND(v,vconv)+y_goffset;	
			    /* Output the new positioning command */
			    EMIT(outfp,"\033*p%dY",ry);
			  }
			
			else if (num>=0) EMIT(outfp,"\033*p%c%dY",numstr[0],num);
			else EMIT(outfp,"\033*p%dY",num);
								
			if (t==0x79)
			  {
			    num=0;
			    count=0;
			    for (t=(char) getc(spfp);t<0x40;t=(char) getc(spfp)) 
			      numstr[count++]=t;   /* reconstructed code */
			    numstr[count]=0;
			    num=atoi (numstr);
			    if(numstr[0]!='+'&&numstr[0]!='-')
			      {	
				/*Add in correct horizontal offset */
				rx=num-minx+(int)PIXROUND(h,hconv)+x_goffset;
				EMIT(outfp,"\033*p%dX",rx);
			      }
			    else if (num>=0) EMIT(outfp,"\033*p%c%dX",numstr[0],num);
			    else EMIT(outfp,"\033*p%dX",num);
			  }
		      }
		    			
		    if ((t==0x58)||(t==0x78))
		      {
			if(numstr[0]!='+'&&numstr[0]!='-')
			  {
			    /*Add in the correct horizontal offset*/
			    rx=num-minx+(int)PIXROUND(h,hconv)+x_goffset;
			    EMIT(outfp,"\033*p%dX",rx);
			  }
			else if (num>=0) EMIT(outfp,"\033*p%c%dX",numstr[0],num);
			else EMIT(outfp,"\033*p%dX",num);
									
			if (t==0x78)
			  {
			    num=0;
			    count=0;
			    for (t=(char) getc(spfp);t<0x40;t=(char) getc(spfp))
			      numstr[count++]=t;
			    numstr[count]=0;
			    num= atoi (numstr);
			    if(numstr[0]!='+'&&numstr[0]!='-')
			      {
				/*Add in correct vertical offset*/
				ry=num-miny+(int)PIXROUND(v,vconv)+y_goffset;
				EMIT(outfp,"\033*p%dY",ry);
			      }
			    else if (num >=0) EMIT(outfp,"\033*p%c%dY",numstr[0],num);
			    else EMIT(outfp,"\033*p%dY",num);
	   						}
		      }
		    break;
    							
		  case(0x74):
		    /* Set the Raster resolution */
		    EMIT(outfp,"\033*t");
		    for (t=(char) getc(spfp);t != 0x52; t=(char) getc(spfp))
		      putc(t,outfp);
		    putc(t,outfp);
		    break;
   							
		  case(0x72):
		    /* Raster Graphics commands such as start */
		    EMIT(outfp,"\033*r");
		    for (t=(char) getc(spfp);((t< 0x40)||(t>0x60)); t=(char) getc(spfp))
		      putc(t,outfp);
		    putc(t,outfp);
		    break;

		  case(0x62):
		    /* Transfer the correct number of bytes of raster graphics */
		    EMIT(outfp,"\033*b");
		    num=0;
		    count=0;
		    for (t=(char) getc(spfp);((t<0x40)||(t>=0x60));t=(char) getc(spfp))
		      numstr[count++]=t;
		    numstr[count]=0;
		    if (t==0x4D)
		      {
			for(t=numstr[count=0];t!=0;t=numstr[++count])
			  putc(t,outfp);
			EMIT(outfp,"M");
		      }
		    if (t==0x57)
		      {
			for(t=numstr[count=0];t!=0;t=numstr[++count])
			  putc(t,outfp);
			EMIT(outfp,"W");
			for(count= atoi(numstr);count>0;count--)
			  {
			    t=(char) getc(spfp);
			    putc(t,outfp);
			  }	
		      }
		    break;
    								
		  case(0x63):
		    /* Rectangular draw commands */
		    EMIT(outfp,"\033*c");
		    for (t=(char) getc(spfp);((t< 0x40)||(t>0x60)); t=(char) getc(spfp))
		      putc(t,outfp);
		    putc(t,outfp);
		    break;		    								
       								
		  default:
		    break;

    				
		  }
	      }
	  }
      }
    fclose(spfp);
    qfprintf(stderr,"]");
}



/*-->DecodeArgs*/
/*********************************************************************/
/***************************** DecodeArgs ****************************/
/*********************************************************************/
void
DecodeArgs( argc, argv )
int     argc;
char    *argv[];
{
    int     argind;            /* argument index for flags      */
    char    curarea[STRSIZE];  /* current file area             */
    char    curname[STRSIZE];  /* current file name             */
    char    *tcp, *tcp1;       /* temporary character pointers  */
    char    *this_arg;
    double  x_offset=0.0, y_offset=0.0;

#ifndef KPATHSEA
    if ((tcp = getenv("TEXPXL")) != NULL) PXLpath = tcp;
#ifdef LJ_RESIDENT_FONTS
    if ((tcp = getenv("TFMFONTS")) != NULL) 
      TFMpath = tcp;
    else if ((tcp = getenv("TEXFONTS")) != NULL) 
      TFMpath = tcp;
#endif
#endif

    argind = 1;
    while (argind < argc) {
        tcp = argv[argind];
        if (*tcp == '-' && *(tcp+1)) {
            ++tcp;
            switch (*tcp) {
#ifdef IBM3812
            case 'b':       /* first page from alternate casette */
                FirstAlternate = _TRUE;
                break;
#endif
            case 'c':       /* number of copies to print */
                if ( sscanf(tcp + 1, "%hd", &ncopies) != 1 )
                   Fatal("Argument of -c is not a valid integer\n");
                if (ncopies<1) {
                  Warning("argument of -c < 1; set to 1!");
                  ncopies=1;
                }
                break;
#ifdef DEBUG
            case '-':       /* - selects Debug output */
		tcp++;
		if (*tcp == 'D') {
		  Debug = _TRUE;
#ifdef KPATHSEA
            	  sscanf (tcp + 1, "%u", &kpathsea_debug);
#endif
                }
                break;
#endif
#ifdef LJ2P
            case 'd':       /* d selects DUPLEX mode  */
		tcp++;
		if (*tcp == '1' ) DuplexMode = 1;
		else if (*tcp == '2') DuplexMode = 2;
                else {
		  Warning("Invalid DUPLEX mode, assuming DUPLEX=1, Long-Edge Binding");
		  DuplexMode = 1;
		}
                break;
#endif
            case 'D':       /* D selects DoublePage  */
                DoublePage = _TRUE;
		tcp++;
		if (*tcp == '1' || *tcp == '2') {
		  if (*tcp == '2') PrintFirstPart = _FALSE;
		  else             PrintSecondPart = _FALSE;
		  tcp++;
		}
		if (*tcp == '-') PrintEmptyPages = _FALSE;
                break;
#ifdef LJ4
            case 'E':       /* do not reset printer (go) */
                econoMode = _TRUE;
                break;
#endif
            case 'e':       /* emit file is specified */
                EmitFileName = ++tcp;
#ifdef MSDOS
                /* delete trailing ':' (causing hangup) */
                if (EmitFileName[strlen(EmitFileName)-1] == ':')
                    EmitFileName[strlen(EmitFileName)-1] = '\0';
#endif
#ifdef OS2  /* repeated to avoid problems with stupid c preprocessors  */
                /* delete trailing ':' (causing hangup) */
                if (EmitFileName[strlen(EmitFileName)-1] == ':')
                    EmitFileName[strlen(EmitFileName)-1] = '\0';
#endif
                break;
            case 'f':       /* next arg is starting pagenumber */
                if ( sscanf(tcp + 1, "%ld", &FirstPage) != 1 )
                    Fatal("Argument is not a valid integer\n");
                FirstPageSpecified = _TRUE;
                break;
#ifdef LJ
            case 'g':       /* do not reset printer (go) */
                ResetPrinter = _FALSE;
                break;
#endif
            case 'h':     /* copy header file through to output  */
                HeaderFileName = ++tcp;
                G_header = _TRUE;
                break;
#if defined(LJ2P) || defined(IBM3812)
            case 'l':       /* landscape  */
                Landscape = _TRUE;
                break;
#endif
#ifdef MAKETEXPK
	    case 'M':
		/* -M, -M1 => don't make font; -M0 => do.  */
	        makeTexPK = *(tcp + 1) == '0';
	        break;
#endif
            case 'x':       /* specify x-offset */
                this_arg = 0;
                if (!(*++tcp)) {
                    this_arg = (++argind >= argc ? 0 : argv[argind]);
                } else {
                    this_arg = tcp;
                }
                if (!this_arg
                     || sscanf(this_arg,"%lf", &x_offset) != 1)
         Fatal("Argument of -x is not a valid floating point number\n");
                break;
            case 'y':       /* specify y-offset */
                this_arg = 0;
                if (!(*++tcp)) {
                    this_arg = (++argind >= argc ? 0 : argv[argind]);
                } else {
                    this_arg = tcp;
                }
                if (!this_arg || sscanf(this_arg, "%lf", &y_offset) != 1)
                Fatal("Argument of -y is not a valid floating point number\n");
                break;
            case 'X':       /* specify X-origin in dots */
                this_arg = 0;
                if (!(*++tcp)) {
                    this_arg = (++argind >= argc ? 0 : argv[argind]);
                } else {
                    this_arg = tcp;
                }
                if (!this_arg || sscanf(this_arg,"%hd", &x_origin) != 1)
                   Fatal("Argument of -X is not a valid integer\n");
                break;
            case 'Y':       /* specify Y-origin in dots */
                this_arg = 0;
                if (!(*++tcp)) {
                    this_arg = (++argind >= argc ? 0 : argv[argind]);
                } else {
                    this_arg = tcp;
                }
                if (!this_arg ||
                     sscanf(this_arg, "%hd", &y_origin) != 1)
                        Fatal("Argument of -Y is not a valid integer\n");
                break;
            case 'm':       /* specify magnification to use */
                switch ( (*++tcp) ) {
                case '#':
                     /* next arg is a magnification to use */
                    if ( sscanf(tcp + 1, "%ld", &usermag) != 1 )
                       Fatal("Argument of mag is not a valid integer\n");
                    break;
                case '0':
                    usermag = 1000;
                    break;
                case 'h':
                case 'H':
                    usermag = 1095;
                    break;
                case '1':
                    usermag = 1200;
                    break;
                case 'q':
                    usermag = 1250;
                    break;
                case '2':
                    usermag = 1440;
                    break;
                case '3':
                    usermag = 1728;
                    break;
                case '4':
                    usermag = 2074;
                    break;
                case '5':
                    usermag = 2488;
                    break;
                default:
                    Fatal("%c is a bad mag step\n", *tcp);
                }
                break;
#ifdef SUPERCOMMENT
            case 'o':     /* PostScript command to send */
                if ( ++argind >= argc )
                    Fatal("No argument following -o\n", 0);
                PScmd[nps++] = argv[argind];
                break;
#endif
            case 'p':       /* print n pages  */
                if ( sscanf(tcp + 1, "%ld", &PrintPages) != 1 )
                    Fatal("Argument is not a valid integer\n");
                if (PrintPages < 1)
                  Fatal("Argument of -p must be greater than 0\n");
                break;
            case 'q':       /* quiet operation */
                G_quiet = _TRUE;
                break;
            case 'r':       /* switch order to process pages */
                Reverse = (bool)(!Reverse);
                break;
#ifdef LJ
            case 's':       /* specify X-origin in dots */
                this_arg = 0;
                if (!(*++tcp)) this_arg = (++argind >= argc ? 0 : argv[argind]);
                else           this_arg = tcp;
                if (!this_arg || sscanf(this_arg,"%hd", &pagesize) != 1)
                   Fatal("Argument of -s is not a valid integer\n");
/*
 * The original pgsiz_dots assumed  a resolution of 300dpi. This loses
 * at 600dpi so we must scale all.
 */
                switch (pagesize) {
/* 
 * The original pgsiz_dots assumed  a resolution of 300dpi. This loses
 * at 600dpi so we must scale all. 
 */
                case 1: pgsiz_dots = 3150; break;         /* executive */
                case 2: pgsiz_dots = 3300; break;         /* letter */
                case 3: pgsiz_dots = 4200; break;         /* legal */
                case 26: pgsiz_dots = 3507; break;        /* a4 */
                case 80: pgsiz_dots = 2250; break;        /* monarc */
                case 81: pgsiz_dots = 2850; break;        /* com10 */
                case 90: pgsiz_dots = 2598; break;        /* int dl */
                case 91: pgsiz_dots = 2704; break;        /* int c5 */
                default: Fatal(
#ifndef vms
                   "%hd is a bad value for pagesize (1,2,3,26,80,81,90,91)",
#else
                   "%d is a bad value for pagesize (1,2,3,26,80,81,90,91)",
#endif
                   pagesize);
                }
                break;
#endif
            case 't':       /* ending pagenumber */
                if ( sscanf(tcp + 1, "%ld", &LastPage) != 1 )
                    Fatal("Argument is not a valid integer\n");
                LastPageSpecified = _TRUE;
                break;
            case 'v':    /* verbatim mode (print pxl-file names) */
                G_noverbatim = _FALSE;
                break;
            case 'w':       /* don't print out warnings */
                G_nowarn = _TRUE;
                break;
#ifdef LJ
            case 'z':
                PrintTestPage = (bool)(!PrintTestPage);
                break;
#endif
#ifdef LJ4
            case 'R':       /* number of copies to print */
                if ( sscanf(tcp + 1, "%d", &RESOLUTION) != 1 )
                   Fatal("Argument of -R is not a valid integer\n");
                if (RESOLUTION != 300 && RESOLUTION != 600 ) {
                  Warning("Resolution must be 300 or 600! Assuming 300.dpi.");
                  RESOLUTION=300;
                } else {
		  if (RESOLUTION == 600) {
		    MFMODE = MFMODE600;
		    x_origin = 600;
		    y_origin = 600;
		  }
		}
                break;
#endif
            default:
                fprintf(stderr, "%c is not a legal flag\n", *tcp);
            }
        } else {

            (void) strcpy(filename, tcp);
	    if(!strcmp(filename, "-")) {
 	      EmitFileName="-";
	      dvifp=stdin;
	    } else {
	      tcp = strrchr(argv[argind], '/');
	      /* split into directory + file name */
	      if (tcp == NULL)  {
                curarea[0] = '\0';
                tcp = argv[argind];
	      } else {
                (void) strcpy(curarea, argv[argind]);
                curarea[tcp-argv[argind]+1] = '\0';
                tcp += 1;
	      }

	      (void) strcpy(curname, tcp);
	      /* split into file name + extension */
	      tcp1 = strrchr(tcp, '.');
	      if (tcp1 == NULL) {
                (void) strcpy(rootname, curname);
                strcat(curname, ".dvi");
	      } else {
                *tcp1 = '\0';
                (void) strcpy(rootname, curname);
                *tcp1 = '.';
	      }

	      (void) strcpy(filename, curarea);
	      (void) strcat(filename, curname);

	      if ((dvifp = BINOPEN(filename)) == NULL)  {
		/* do not insist on .dvi */
		if (tcp1 == NULL)  {
		  curname[strlen (curname) - 4] = '\0';
		  filename[strlen (filename) - 4] = '\0';
		}
		if (tcp1 != NULL || (dvifp = BINOPEN(filename)) == NULL)  {
		  perror (filename);
		  exit (EXIT_FAILURE);
		}
	      }
	    } /* dvi filename != '-" */
        }
        argind++;
    }

#ifdef LJ4
    pgsiz_dots *= (int)(RESOLUTION/300); /* rescale dots to page */
#endif    
    x_goffset = (short) MM_TO_PXL(x_offset) + x_origin;
    y_goffset = (short) MM_TO_PXL(y_offset) + y_origin;

    if (dvifp == NULL)  {
      fprintf(stderr,"\nThis is the DVI to %s converter version %s",
          PRINTER, VERSION);
#ifdef SEVENBIT
      fprintf(stderr,", 7bit");
#endif
      fprintf(stderr," (%s)\n", OS);
      fprintf(stderr,"usage: %s [OPTIONS] dvifile\n", G_progname);

      fprintf(stderr,"OPTIONS are:\n");
#ifdef DEBUG
      fprintf(stderr,"\t--D ..... turns debug output on\n");
#endif
      fprintf(stderr,
        "\t-aX ..... X= searchpath leading to pixel-files (.pk or .pxl)\n");
#ifdef IBM3812
      fprintf(stderr,
        "\t-b  ..... take paper for first page from alternate casette\n");
#endif
      fprintf(stderr,"\t-cX ..... X= number of copies\n");
#ifdef LJ2P
      fprintf(stderr,"\t-dX ..... duplex, X=1: long-edge, 2: short-edge binding\n");
#endif
      fprintf(stderr,"\t-D  ..... turns doublepage output on; ");
      fprintf(stderr,"-D1 odd pages only, -D2 even\n");
#ifdef LJ4
      fprintf(stderr,"\t-E  ..... print in econo-mode\n");
#endif
      fprintf(stderr,"\t-eX ..... X= output file\n");
      fprintf(stderr,"\t-fX ..... print from begin of page number X\n");
#ifdef LJ
      fprintf(stderr,
          "\t-g  ..... do not reset printer at begin of job (go)\n");
#endif
      fprintf(stderr,"\t-hX ..... X= name of headerfile\n");
#ifdef LJ2P
      fprintf(stderr,"\t-l  ..... landscape mode\n");
#endif
#ifdef MAKETEXPK
      fprintf(stderr,"\t-MX ..... Don't generate missing PK files\n");
#endif
      fprintf(stderr,"\t-mX ..... magnification X=0;h;1;2;3;4;5;#xxxx\n");
      fprintf(stderr,"\t-pX ..... print X pages\n");
      fprintf(stderr,"\t-q  ..... quiet operation\n");
      fprintf(stderr,"\t-r  ..... process pages in reverse order\n");
#ifdef LJ4
      fprintf(stderr,"\t-RX ..... set resolution to 300 or 600 dpi\n");
#endif
#ifdef LJ
      fprintf(stderr,"\t-sX ..... set paper size to X (see documentation)\n");
#endif
      fprintf(stderr,"\t-tX ..... print to end of page number X\n");
      fprintf(stderr,"\t-w  ..... don't print out warnings\n");
      fprintf(stderr,"\t-v  ..... tell user which pixel-files are used\n");
      fprintf(stderr,"\t-xX ..... X= x-offset on printout in mm\n");
      fprintf(stderr,"\t-yX ..... X= y-offset on printout in mm\n");
      fprintf(stderr,"\t-XO ..... O= x page origin in dots (default=%d)\n",
            XDEFAULTOFF );
      fprintf(stderr,"\t-YO ..... O= y page origin in dots (default=%d)\n",
            YDEFAULTOFF );
#ifdef LJ
      fprintf(stderr,
          "\t-z  ..... print test page with pagecounter after job\n");
#endif
      fprintf(stderr,
	  "\t-   ..... dvifile is stdin (must be seekable); implies -e-\n");
      exit(1);
    }
    if (EQ(EmitFileName, "")) {
        if ((EmitFileName = (char *)malloc( STRSIZE )) != NULL)
            allocated_storage += STRSIZE;
        else
            Fatal("Can't allocate storage of %d bytes\n",STRSIZE);
        (void) strcpy(EmitFileName, curname);
        if ((tcp1 = strrchr(EmitFileName, '.'))) 
	  *tcp1 = '\0';
	strcat(EmitFileName, EMITFILE_EXTENSION);
    }
}


/*-->DoConv*/
/*********************************************************************/
/********************************  DoConv  ***************************/
/*********************************************************************/
long4
DoConv(num, den, convResolution)
long4    num, den;
int     convResolution;
{
    /*register*/ double conv;
    conv = ((double)num / (double)den) *
            ((double)mag / 1000.0) *
          ((double)convResolution/254000.0);

    return((long4) ((1.0/conv)+0.5));
}


/*-->DoSpecial*/
/*********************************************************************/
/*****************************  DoSpecial  ***************************/
/*********************************************************************/
typedef enum  { None, String, Integer /*, Number, Dimension*/ }


ValTyp;
typedef struct {
    char    *Key;       /* the keyword string */
    char    *Val;       /* the value string */
    ValTyp  vt;         /* the value type */
    union {         /* the decoded value */
        int     i;
        float   n;
    } v;
} KeyWord;
typedef struct {
    char    *Entry;
    ValTyp  Typ;
} KeyDesc;
#define PSFILE 0
#define ORIENTATION 1
#define RESETPOINTS 2
#define DEFPOINT 3
#define FILL 4
#define GRAY 5
#define PATTERN 6
#define HPFILE 7
KeyDesc KeyTab[] = {
    { "file", String },
    { "orientation", Integer},
    { "resetpoints", String},
    { "defpoint", String},
    { "fill", String},
    { "gray", Integer},
    { "pattern", Integer},
    { "hpfile", String}
   /*,
             {"hsize", Dimension},
             {"vsize", Dimension},
             {"hoffset", Dimension},
             {"voffset", Dimension},
             {"hscale", Number},
             {"vscale", Number}*/


};


#define NKEYS (sizeof(KeyTab)/sizeof(KeyTab[0]))
void
DoSpecial( str, n )
/* interpret a \special command, made up of keyword=value pairs */
char    *str;
int     n;
{
  char    spbuf[STRSIZE], xs[STRSIZE], ys[STRSIZE];
  char    *sf = NULL;
  float   x,y;
  long4     x_pos, y_pos;
  KeyWord k;
  int     i, j, j1;
  static int   GrayScale=10, Pattern=1;
  static bool  GrayFill=_TRUE;
  static long4  p_x[80], p_y[80];
  str[n] = '\0';
  spbuf[0] = '\0';

  SetPosn(h, v);
  while ( (str = GetKeyStr(str, &k)) != NULL ) {
    /* get all keyword-value pairs */
    /* for compatibility, single words are taken as file names */
    if ( k.vt == None && access(k.Key, 0) == 0) {
        if ( sf )
            Warning("More than one \\special file name given. %s ignored", sf);
        (void) strcpy(spbuf, k.Key);
        sf = spbuf;
/*
        for (j = 1; ((sf[j]=='/' ? sf[j]='\\':sf[j]) != '\0'); j++);
*/
    } else if ( GetKeyVal( &k, KeyTab, NKEYS, &i ) && i != -1 )
        switch (i) {
        case PSFILE:
        case HPFILE:
            if ( sf )
                Warning("More than one \\special file name given. %s ignored",
                            sf);
            (void) strcpy(spbuf, k.Val);
            sf = spbuf;
/*
            for (j=1; ((sf[j]=='/' ? sf[j]='\\':sf[j]) != '\0'); j++) ;
*/
            break;
        case ORIENTATION:
#ifdef IBM3812
            if ((k.v.i >= 0) && (k.v.i < 4)) {
                last_ry = UNKNOWN;
                sprintf(PMPformat, "\322%c", (unsigned char)k.v.i);
                PMPout(2, PMPformat);
		if (k.v.i == 1) Landscape = _TRUE;
		else if (k.v.i == 0) Landscape = _FALSE;
#endif
#ifdef LJ
            if ((k.v.i >= 0) && (k.v.i < 2)) {
                last_ry = UNKNOWN;
                EMIT(outfp, "\033&l%dO\033*rF", (unsigned char)k.v.i);
#endif
            } else
                Warning( "Invalid orientation (%d)given; ignored.", k.v.i);
            break;
        case RESETPOINTS:
            (void) strcpy(spbuf, k.Val);

            sf = NULL;
            break;

        case DEFPOINT:
          (void) strcpy(spbuf, k.Val);
           i=sscanf(spbuf,"%d(%[^,],%s)",&j,xs,ys);
           if (i>0) {
              x_pos=h; y_pos=v;
              if (i>1) {
                  if (sscanf(xs,"%fpt",&x)>0) {
                     fprintf(stderr,"x = %f\n",x);
                     x_pos = PT_TO_DVI(x);
             }
              }
              if (i>2) {
                  if (sscanf(ys,"%fpt",&y)>0) {
                     fprintf(stderr,"y = %f\n",y);
                     y_pos = PT_TO_DVI(y);
                }
              }
              p_x[j]=x_pos;
              p_y[j]=y_pos;
           } else
             Warning("invalid point definition\n");

           sf = NULL;
           break;

        case FILL:
          (void) strcpy(spbuf, k.Val);
           i=sscanf(spbuf,"%d/%d %s",&j,&j1,xs);
           if (i>1) {
#ifdef LJ
              SetPosn(p_x[j], p_y[j]);
              x_pos = (long4)PIXROUND(p_x[j1]-p_x[j], hconv);
              y_pos = (long4)PIXROUND(p_y[j1]-p_y[j], vconv);
              if (labs(x_pos)<labs(y_pos)) x_pos=x_pos+3;
              else                         y_pos=y_pos+3;
              if (GrayFill) {
		EMIT(outfp, "\033*c%lda%ldb%dg2P", (long)x_pos, (long)y_pos, GrayScale);
              } else {
		EMIT(outfp, "\033*c%lda%ldb%dg3P", (long)x_pos, (long)y_pos, Pattern);
              }
              last_ry = UNKNOWN;
#endif
           }
           break;
        case GRAY:
            if ((k.v.i >= 0) && (k.v.i < 101)) {
                GrayScale = k.v.i;
                GrayFill = _TRUE;
            } else
                Warning( "Invalid gray scale (%d) given; ignored.", k.v.i);
            break;
        case PATTERN:
            if ((k.v.i >= 0) && (k.v.i < 7)) {
                Pattern = k.v.i;
                GrayFill = _FALSE;
            } else
                Warning( "Invalid pattern (%d) given; ignored.", k.v.i);
            break;

        default:
            Warning("Can't handle %s=%s command; ignored.", k.Key, k.Val);
            break;
        }
    else
        Warning("Invalid keyword or value in \\special - <%s> ignored", k.Key);
  }
  if ( sf ) {
    last_ry = UNKNOWN;
#ifdef IBM3812
    PMPflush;
#endif
    if (i==HPFILE)
      CopyHPFile( sf );
    else
      CopyFile( sf );
  }
}


/*-->EmitChar*/
/**********************************************************************/
/****************************  EmitChar  ******************************/
/**********************************************************************/
void                     /* output a character bitmap */
EmitChar(c, ce)
long4    c;
struct char_entry *ce;
{
    register int    i;
    register unsigned char  *sl;
    unsigned short  nbpl, nwpl;
    long4    total;
#ifdef LJ
    unsigned char cnv_buffer[10];
#endif

  
#ifdef LJ
/*
printf("Emit character %c(%d) id=%d, yoff=%d[%d], w=%d[%d], h=%d[%d]\n",
        (char)c,(int)c,
    fontptr->plusid,
    ce->yOffset, fontptr->max_yoff,
    ce->width,   fontptr->max_width,
    ce->height,  fontptr->max_height
);
*/
#endif



    if ( fontptr->ncdl == 0 ) {
#ifdef IBM3812
        used_fontstorage += 1028;
#endif
#ifdef LJ
        if (fontptr->max_width == 0) { /* we have no realistic values */
                fontptr->max_yoff = CHAR_HEIGTH_LARGE;
                fontptr->max_width = CHAR_WIDTH_LARGE;
                fontptr->max_height = CHAR_HEIGTH_LARGE*2;
        }

        /* open font dict before first char, set active font */
        INT_ASCII(cnv_buffer,fontptr->plusid);
#ifdef LJ4
        EMIT(outfp, "\033*c%sD\033)s68W", cnv_buffer);
        EMITB(6, "\0\104\024\2\0\0");   
#else
        EMIT(outfp, "\033*c%sD\033)s26W", cnv_buffer);
#ifdef SEVENBIT
        EMITB(6, "\0\032\0\1\0\0");   
#else
        EMITB(6, "\0\032\0\2\0\0");   
#endif
#endif /* LJ 4 */
        EMITWORD( fontptr->max_yoff);
        EMITWORD( fontptr->max_width);
        EMITWORD( fontptr->max_height);
        EMITB(8, "\0\1\1\25\0\4\0\4");
        EMITB(6, "\0\0\0\0\0\0");
#ifdef LJ4
	EMITB(22,"\0\0\0\0\0\0\0\0\0\0\0\0\0\377\0\0\0\0\0\0\0\0");
	EMITB(16,"                ");
	EMITB(4 ,"\2\130\2\130"); 
#endif
        EMIT(outfp, "\033*c4F");
#endif
    }
    if ( fontptr != prevfont ) {   /* because this isn't done on pass 0 */
#ifdef LJ
        INT_ASCII(cnv_buffer,fontptr->plusid);
        EMIT(outfp, "\033(%sX", cnv_buffer);
#endif
        prevfont = fontptr;
    }

#ifdef USEPXL
    if (fontptr->id == pk89)   {
        nwpl = 0; /* initialize variable */
        nbpl = ((int)(ce->width) +  7) >> 3;
        total = (long4)ce->height * nbpl;
    } else if (fontptr->id == id1002)   {
        nwpl = 0; /* initialize variable */
        nbpl = ((int)(ce->width) +  7) >> 3;
        total = (long4)ce->height * nbpl;
    } else if (fontptr->id == id1001) {
        nwpl = ((int)(ce->width) + 31) >> 5;
        nbpl = ((int)(ce->width) + 7) >> 3;
        total = (long4)ce->height * nbpl;
    } else {
      /* should never be necessary */
      nwpl = 0;
      nbpl = 0;
      total = 0;
    }
#else
    nbpl = (num_cols + 7) >> 3;
    total = num_rows * nbpl;
#endif
/***************************************************************
    if ( ((char) c == 'i') && (fontptr->plusid == 0)) {
        long4 j;
        fprintf(stderr, "cols=%ld, ncols=%ld\n",(long)nwpl,(long)nbpl);

        fprintf(stderr, "%ld Bytes:->",(long)total);
        for (j=0; j<total;j++) {
            char *ch; char ic;
            ch = (char *)(ce->where.address.pixptr);
            ic = *(ch+j);
            fprintf(stderr,"%X.",ic);
                }
        fprintf(stderr,"<- Now Emitting\n");
        }
***************************************************************/
#ifdef USEPXL
#ifdef IBM3812
    if ((short)(ce->height) - ce->yOffset > 55) {
        ce->yyOffset = (short) ce->height - ce->yOffset;
        ce->yOffset  = (short) ce->height;
    } else {
        ce->yyOffset = (short) 0;
    }
#endif
#ifdef LJ
        ce->yyOffset = (short) 0;
#endif
#endif

    /* ce->cw = (long4)(((double)ce->tfmw / (double)hconv) +0.5); */
    /* set active font to nn, load font pattern  xx ... */
#ifdef IBM3812
    PMPcont(total + 9);
#ifdef USEPXL
    sprintf(PMPformat, "\323%c\360%c%c%c",
        (unsigned char)fontptr->plusid,
        (unsigned char)VisChar((char)c),
        (unsigned char)ce->height,
        (unsigned char)ce->width);
    PMPout(6, PMPformat);
    PMPoutC((char)(-(ce->xOffset)));
    PMPoutC((char)(ce->cw - (-ce->xOffset + ce->width)));
    PMPoutC((char)(ce->yOffset));
#else
    sprintf(PMPformat, "\323%c\360%c%c%c",
        (unsigned char)fontptr->plusid,
        (unsigned char)VisChar((char)c),
        (unsigned char)num_rows,
        (unsigned char)num_cols);
    PMPout(6, PMPformat);
    PMPoutC((char)(-x_offset));
    PMPoutC((char)(ce->cw - (-x_offset + num_cols)));
    PMPoutC((char)num_rows-y_offset);
#endif
#endif
#ifdef LJ
    INT_ASCII(cnv_buffer,fontptr->plusid);
    EMIT(outfp, "\033*c%sd%dE\033(s%ldW", cnv_buffer,
        (unsigned int)VisChar((char)c), (long)(total + 16));
    EMITB(4, "\4\0\016\1");
/*    EMITC((char)(Landscape==_TRUE)); */
    EMITC((char)0);
    EMITC((char)0);
#ifdef USEPXL
    EMITWORD(-ce->xOffset);
    EMITWORD(ce->yOffset);
    EMITWORD(ce->width);
    EMITWORD(ce->height);
#else
    EMITWORD(-x_offset);
    EMITWORD(num_rows-y_offset);
    EMITWORD(num_cols);
    EMITWORD(num_rows);
#endif
    EMITWORD((int)ce->cw * 4);
#endif
#ifdef USEPXL

    if (fontptr->id == pk89)
        PkRaster(ce, _FALSE);
    else if (fontptr->id == id1002)
        for (i = 0; i < (int) ce->height; i++) {
            sl = ((unsigned char *)(ce->where.address.pixptr) + i * nbpl);
            EMITL(nbpl, sl);
        }
    else if (fontptr->id == id1001)
        for (i = 0; i < (int) ce->height; i++) {
            sl = (unsigned char *)(ce->where.address.pixptr + i * nwpl);
            EMITL(nbpl, sl);
        }
#else
    for (i = num_rows; i > 0; i--)
      {
        EMITL (nbpl, bits + (i-1) * nbpl);
      }
#endif
#ifdef IBM3812
#ifdef USEPXL
    used_fontstorage += (long4)ce->height * ((ce->width + 15) >> 4)
        *2 + 14;
#else
    used_fontstorage += (long4)num_rows * ((num_cols + 15) >> 4)
        *2 + 14;
#endif
#endif
#ifdef LJ
#ifdef USEPXL
    used_fontstorage += 64 * (((int)((ce->height * ce->width) - 1) / 64) + 1);
#else
    used_fontstorage += 64 * ((((num_rows * num_cols) - 1) / 64) + 1);
#endif
#endif
    fontptr->ncdl += 1;
    G_ncdl += 1;
}


/*-->RasterLine*/
/**********************************************************************/
/****************************  RasterLine  ****************************/
/**********************************************************************/
void
RasterLine(ce, nbpl, current_line, buffer)
struct char_entry *ce;
unsigned short  nbpl, current_line;
char    *buffer;
{
#ifdef IBM3812
    long4    total;
    static unsigned short   maxlines;

    if (current_line == 0) {
#ifdef USEPXL
        maxlines = ce->height;

        MoveVert(-ce->yOffset);      /* move cursor up */
        MoveHor(-ce->xOffset);       /* move cursor right */
#else
        maxlines = num_rows;

        MoveVert(-(num_rows-y_offset));      /* move cursor up */
        MoveHor(-x_offset);       /* move cursor right */
#endif
        last_ry = UNKNOWN;       /* next time full positioning */
    }

    if (current_line % maxlines == 0) {
        if (current_line > 0) {    /* maxlines lines have been printed*/
            MoveVert(maxlines);   /*   move cursor down     */
            last_ry = UNKNOWN;    /* next time full positioning */
        }
#ifdef USEPXL
        total = (long4)(ce->height - current_line) * (long4)nbpl;
#else
        total = (long4)(num_rows - current_line) * (long4)nbpl;
#endif
        if ((total + 9) > 65535) {
            maxlines = (unsigned short)((65535 - 9) / nbpl);
            total = (long4)maxlines * (long4)nbpl;
        }

        PMPcont(total + 9);
        PMPout(2, "\365\0");
        EMITWORD(maxlines);
#ifdef USEPXL
        EMITWORD(ce->width);
#else
        EMITWORD(num_cols);
#endif
        PMPoutC((unsigned char) (total >> 16) & 0xFF);
        PMPoutC((unsigned char) (total >> 8 ) & 0xFF);
        PMPoutC((unsigned char)  total     & 0xFF);
    }
    EMITL((int)nbpl, buffer);
#endif
#ifdef LJ
    register int    emitbytes;

    for (emitbytes = (int)nbpl;
        (*(buffer + emitbytes - 1) == '\0') && (emitbytes > 0);
        emitbytes--) ;
    EMIT(outfp, "\033*b%dW", emitbytes);
    EMITL(emitbytes, buffer);
#endif
}


/*-->RasterChar*/
/**********************************************************************/
/****************************  RasterChar  ****************************/
/**********************************************************************/
void                     /* raster a character bitmap */
RasterChar(ce)
struct char_entry *ce;
{
    int     i;
    register unsigned char  *sl;
    unsigned short  nbpl, nwpl;
    unsigned char   raster_line_buf[BYTES_PER_PIXEL_LINE];

#ifdef DEBUG
    if (Debug)
        fprintf(stderr,"Raster character ...size=%d \n", (int)ce->charsize );
#endif

#ifdef USEPXL
    if (fontptr->id == pk89)   {
        nwpl = 0; /* initialize variable */
        nbpl = ((int)(ce->width) +  7) >> 3;
    } else if (fontptr->id == id1002)   {
        nwpl = 0; /* initialize variable */
        nbpl = ((int) ce->width +  7) >> 3;
    } else if (fontptr->id == id1001) {
        nwpl = ((int) ce->width + 31) >> 5;
        nbpl = ((int) ce->width + 7) >> 3;
    } else {
      /* should never be necessary */
      nwpl = 0;
      nbpl = 0;
    }
#else
    nbpl = (num_cols + 7) >> 3;
#endif

#ifdef LJ
    EMIT(outfp, "\033*t%dR\033*r1A",RESOLUTION);
#endif
                                            { /* read pixel from file */
    if ((ce->charsize == HUGE_SIZE) && (fontptr->id != pk89))
        OpenFontFile();
#ifdef USEPXL
        fseek(pxlfp, ce->where.address.fileOffset, 0);
#else
        fseek(gfin, ce->where.address.fileOffset, 0);
        gettochar();
        readbits();
#endif
    }

#ifdef USEPXL
    if (fontptr->id == pk89)
        PkRaster(ce, _TRUE);
    else if (fontptr->id == id1002) {
        for (i = 0; i < (int) ce->height; i++) {
            if (ce->charsize == HUGE_SIZE) {
                fread(raster_line_buf, 1, (int) nbpl, pxlfp);
                sl = raster_line_buf;
            } else
                sl = ((unsigned char *)(ce->where.address.pixptr)
                    + i * nbpl);

            RasterLine(ce, (unsigned int)nbpl, i, sl);
        }
    } else if (fontptr->id == id1001) {
        long4    filediff;
        filediff = (long4)nwpl * 4 - nbpl;
        for (i = 0; i < (int) ce->height; i++) {
            if (ce->charsize == HUGE_SIZE) {
                fread(raster_line_buf, 1, (int) nbpl, pxlfp);
                /* skip fill bytes */
                fseek(pxlfp, filediff, 1);
                sl = raster_line_buf;
            } else
                sl = (unsigned char *)(ce->where.address.pixptr + i * nwpl);

            RasterLine(ce, (unsigned int)nbpl, i, sl);
        }
    }
#else
    for (i = num_rows; i > 0; i--)
      RasterLine(ce, (unsigned int)nbpl, i, bits + (i-1) * nbpl);
#endif
#ifdef LJ
    EMIT(outfp, "\033*rB");
#endif
    last_ry = UNKNOWN;
}


/*-->Fatal*/
/**********************************************************************/
/******************************  Fatal  *******************************/
/**********************************************************************/
void
Fatal(fmt, a, b, c)      /* issue a fatal error message */
char    *fmt;           /* format              */
char    *a, *b, *c;     /* arguments           */
{
    fprintf(stderr, "\n");
    fprintf(stderr, "%s: FATAL--", G_progname);
    fprintf(stderr, fmt, a, b, c);
    fprintf(stderr, "\n\n");
#ifndef vms
    exit(2);
#else
    exit(SS$_ABORT);
#endif
}


/*-->FindPostAmblePtr*/
/**********************************************************************/
/************************  FindPostAmblePtr  **************************/
/**********************************************************************/
void
FindPostAmblePtr(postambleptr)
long4    *postambleptr;
/* this routine will move to the end of the file and find the start
    of the postamble */
{
    long4    i;
    fseek (dvifp,  0l, 2);   /* goto end of file */
    *postambleptr = ftell (dvifp) - 4;
    fseek (dvifp, *postambleptr, 0);
    while (_TRUE) {
        fseek (dvifp, --(*postambleptr), 0);
        if (((i = NoSignExtend(dvifp, 1)) != 223) &&
            (i != DVIFORMAT))
            Fatal ("Bad end of DVI file");
        if (i == DVIFORMAT)
            break;
    }
    fseek (dvifp, (*postambleptr) - 4, 0);
    (*postambleptr) = NoSignExtend(dvifp, 4);
    fseek (dvifp, *postambleptr, 0);
}


/*-->GetBytes*/
/**********************************************************************/
/*****************************  GetBytes  *****************************/
/**********************************************************************/
void
GetBytes(fp, cp, n)     /* get n bytes from file fp */
/*register*/ FILE *fp;      /* file pointer  */
register char   *cp;    /* character pointer */
int     n;            /* number of bytes  */
{
    while (n--)
        *cp++ = (char)getc(fp);
}


/*-->GetFontDef*/
/**********************************************************************/
/**************************** GetFontDef  *****************************/
/**********************************************************************/
void
GetFontDef()
/***********************************************************************
   Read the font  definitions as they  are in the  postamble of the  DVI
   file.
***********************************************************************/
{
    unsigned char   byte;
    while (((byte = (unsigned char) NoSignExtend(dvifp, 1)) >= FNT_DEF1) &&
        (byte <= FNT_DEF4)) {
        switch (byte) {
        case FNT_DEF1:
            ReadFontDef ( NoSignExtend(dvifp, 1));
            break;
        case FNT_DEF2:
            ReadFontDef ( NoSignExtend(dvifp, 2));
            break;
        case FNT_DEF3:
            ReadFontDef ( NoSignExtend(dvifp, 3));
            break;
        case FNT_DEF4:
            ReadFontDef ( NoSignExtend(dvifp, 4));
            break;
        default:
            Fatal ("Bad byte value in font defs");
            break;
        }
    }
    if (byte != POST_POST)
        Fatal ("POST_POST missing after fontdefs");
}


/*-->GetKeyStr*/
/**********************************************************************/
/*****************************  GetKeyStr  ****************************/
/**********************************************************************/
/* extract first keyword-value pair from string (value part may be null)
 * return pointer to remainder of string
 * return NULL if none found
 */
char    KeyStr[STRSIZE];
char    ValStr[STRSIZE];
char
*GetKeyStr( str, kw )
char    *str;
KeyWord *kw;
{
    char    *s, *k, *v, t;
    if ( !str )
        return( NULL );
    for ( s = str; *s == ' '; s++)
        ;          /* skip over blanks */
    if ( *s == '\0' )
        return( NULL );
    for ( k = KeyStr; /* extract keyword portion */
    *s != ' ' && *s != '\0' && *s != '=';
        *k++ = *s++)
        ;
    *k = '\0';
    kw->Key = KeyStr;
    kw->Val = v = NULL;
    kw->vt = None;
    for ( ; *s == ' '; s++)
        ;            /* skip over blanks */
    if ( *s != '=' )         /* look for "=" */
        return( s );
    for ( s++; *s == ' '; s++);      /* skip over blanks */
    if ( *s == '\'' || *s == '\"' )  /* get string delimiter */
        t = *s++;
    else
        t = ' ';
    for ( v = ValStr; /* copy value portion up to delim */
    *s != t && *s != '\0';
        *v++ = *s++)
        ;
    if ( t != ' ' && *s == t )
        s++;
    *v = '\0';
    kw->Val = ValStr;
    kw->vt = String;
    return( s );
}


/*-->GetKeyVal*/
/**********************************************************************/
/*****************************  GetKeyVal  ****************************/
/**********************************************************************/
/* get next keyword-value pair decode value according to table entry  */
bool
GetKeyVal( kw, tab, nt, tno)
KeyWord *kw;
KeyDesc tab[];
int     nt;
int     *tno;
{
    int     i;
    char    c = '\0';
    *tno = -1;
    for (i = 0; i < nt; i++)
        if ( IsSame(kw->Key, tab[i].Entry) ) {
            *tno = i;
            switch ( tab[i].Typ ) {
            case None:
                if ( kw->vt != None )
                    return( _FALSE );
                break;
            case String:
                if ( kw->vt != String )
                    return( _FALSE );
                break;
            case Integer:
                if ( kw->vt != String )
                    return( _FALSE );
                if ( sscanf(kw->Val, "%d%c",
                    &(kw->v.i), &c) != 1
                     || c != '\0' )
                    return( _FALSE );
                break;
/*              case Number:
 *              case Dimension:
 *                  if( kw->vt != String ) return( _FALSE );
 *                  if( sscanf(kw->Val,"%f%c",
 *                     &(kw->v.n), &c) != 1
 *                  || c != '\0' ) return( _FALSE );
 *                  break;
 */
            }
            kw->vt = tab[i].Typ;
            return( _TRUE );
        }
    return( _TRUE );
}



/*-->IsSame*/
/**********************************************************************/
/*******************************  IsSame  *****************************/
/**********************************************************************/
bool                  /* compare strings, ignore case */
IsSame(a, b)
char    *a, *b;
{
    char *x, *y;
    x = a;
    y = b;
    for ( ; *a != '\0'; a++,b++)
        if ( tolower(*a) != tolower(*b) )
            return( _FALSE );
    return( *x == *y ? _TRUE : _FALSE );
}


/*-->NoSignExtend*/
/**********************************************************************/
/***************************  NoSignExtend  ***************************/
/**********************************************************************/
long4
NoSignExtend(fp, n)     /* return n byte quantity from file fd */
register FILE *fp;      /* file pointer    */
register int    n;      /* number of bytes */
{
    long4    x;      /* number being constructed */
    x = 0;
    while (n--)  {
        x <<= 8;
        x |= getc(fp);
    }
    return(x);
}


/*-->OpenFontFile*/
/**********************************************************************/
/************************** OpenFontFile  *****************************/
/**********************************************************************/
void
OpenFontFile()
/***********************************************************************
    The original version of this dvi driver reopened the font file  each
    time the font changed, resulting in an enormous number of relatively
    expensive file  openings.   This version  keeps  a cache  of  up  to
    MAXOPEN open files,  so that when  a font change  is made, the  file
    pointer, pxlfp, can  usually be  updated from the  cache.  When  the
    file is not found in  the cache, it must  be opened.  In this  case,
    the next empty slot  in the cache  is assigned, or  if the cache  is
    full, the least used font file is closed and its slot reassigned for
    the new file.  Identification of the least used file is based on the
    counts of the number  of times each file  has been "opened" by  this
    routine.  On return, the file pointer is always repositioned to  the
    beginning of the file.
***********************************************************************/
{
    int     i, least_used, current;
    struct pixel_list tmp;

#ifdef DEBUG
    if (Debug)
        fprintf(stderr,"open font file %p\n", fontptr->font_file_id);
#endif
/*
fprintf(stderr,"? %lx == %lx\n", pfontptr,fontptr);
*/
    if ((pfontptr == fontptr) && (pxlfp != NO_FILE))
        return;         /* we need not have been called */

       if (fontptr->font_file_id == NO_FILE)
        return;         /* we need not have been called */

    tmp = pixel_files[1];
    for (current = 1;
        (current <= nopen) && (tmp.pixel_file_id != fontptr->font_file_id); ) {
        ++current;
        tmp = pixel_files[current];
    }
    /* try to find file in open list */

    if (current <= nopen)       /* file already open */ {
        if ( (pxlfp = pixel_files[current].pixel_file_id) != NO_FILE )
            fseek(pxlfp, 0l, 0);
            /* reposition to start of file */
    } else {
            /* file not in open list          */
        if (nopen < MAXOPEN)    /* just add it to list    */
            current = ++nopen;
        else  {
            /* list full -- find least used file,     */
            /* close it, and reuse slot for new file  */
            least_used = 1;
            for (i = 2; i <= MAXOPEN; ++i)
                if (pixel_files[least_used].use_count>pixel_files[i].use_count)
                    least_used = i;
            if (pixel_files[least_used].pixel_file_id != NO_FILE) {
                FILE * fid;
                struct font_entry *fp;
                fid = pixel_files[least_used].pixel_file_id;
                /* mark file as being closed in the entry */
                fp = hfontptr;
                while (fp != NULL && fp->font_file_id != fid) fp = fp->next;
                if (fp == NULL)
                   Fatal("Open file %x not found in font entry list.\n", fid);
                else {
                    fp->font_file_id = NULL;
                }
                fclose( fid );
            }
#ifdef DEBUG
            if (Debug)
                 fprintf(stderr,"\n__reuse slot %d\n", least_used);
#endif
            current = least_used;
        }
        if ((pxlfp = BINOPEN(fontptr->name)) == NULL) {
            Warning("PXL-file %s could not be opened", fontptr->name);
            pxlfp = NO_FILE;
        } else {
#ifdef DEBUG
             if (Debug) 
	       fprintf(stderr,"Opening File  <%s> /%p/, Size(font_entry)=%d\n",
		       fontptr->name, pxlfp, sizeof(struct font_entry ));
#endif

        }
        pixel_files[current].pixel_file_id = pxlfp;
        pixel_files[current].use_count = 0;
    }
    pfontptr = fontptr;         /* make previous = current font */
    fontptr->font_file_id = pxlfp;      /* set file identifier */
    pixel_files[current].use_count++;   /* update reference count */
#ifndef USEPXL
    gfin = pxlfp;
#endif
}


/*-->PixRound*/
/**********************************************************************/
/*****************************  PixRound  *****************************/
/**********************************************************************/
long4
PixRound(x, conv)       /* return rounded number of pixels */
long4    x;          /* in DVI units     */
long4    conv;       /* conversion factor */
{
    return((x + conv) / conv);
}

#ifdef LJ_RESIDENT_FONTS
/*-->TryResident*/
/**********************************************************************/
/****************************  TryResident  ***************************/
/**********************************************************************/
bool
TryResident(fontptr)
struct font_entry *fontptr;
{
  extern bool tfm_read_info ();
  tfm_info_type tfm_info;
  
  /* To determine if a font is resident, check for a special family
     value (header bytes 12..16 in the TFM file). This seems cleaner,
     and certainly more convenient, than somehow reading an external
     ljfonts.map file in which we'd have to specify information for all
     the resident fonts.  */
  if (tfm_read_info (fontptr->n, &tfm_info)
      && tfm_info.family[0]
      && strcmp (tfm_info.family, "HPAUTOTFM") == 0) {
    unsigned i;
    double factor = fontptr->s / (double) 0x100000;

    resident_count++;
    fontptr->resident_p = _TRUE;
    strcpy (fontptr->symbol_set, tfm_info.coding_scheme);
    fontptr->resid = tfm_info.typeface_id;
    fontptr->spacing = tfm_info.spacing;
    fontptr->style = tfm_info.style;
    fontptr->weight = tfm_info.weight;

    if (fontptr->spacing == SPACING_FIXED) {
      /* Have to select the point in pitch (characters per inch) instead
         of point size, and thus have to figure out the pitch that
         corresponds to the point size at which the font is used.
         
         To do this, take the width of the interword space, and see how
         many of those characters will fit in the at size. Then convert
         to how many characters will fit in one inch. That's our pitch.
         
         All the builtin LJ4 fonts that are monospaced are Intellifont,
         which have 72.307 points per inch. Not that it really makes any
         difference. We don't worry about this elsewhere, since all
         point sizes are rounded to .25pt anyway, which is more than the
         difference between the various definitions of `point'. */
      double ds_in_points = fontptr->s / 65536.0;
      double w_in_points = tfm_info.interword / (double) 0x100000;
      if (ds_in_points == 0 || w_in_points == 0) {
        /* Avoid division by zero if no interword space. */
        Warning ("%s: Can't determine pitch for this monospaced font.\n", 
                 fontptr->n);
        fontptr->pitch = 10; /* Result will look awful, which is good. */
      } else {
        fontptr->pitch = 72.307 / (ds_in_points * w_in_points);
      }
    }
    
#ifdef DEBUG
    if (Debug)
      fprintf(stderr, "%6s: typeface=%u\tspacing=%u\tstyle=%u\tweight=%d\n",
              fontptr->n, fontptr->resid, fontptr->spacing,
              fontptr->style, fontptr->weight);
#endif
    for (i = 0; i < NFNTCHARS; i++) {
      struct char_entry *cptr = &(fontptr->ch[i]);
      cptr->tfmw = (long4) (tfm_info.widths[i] * factor);
      cptr->cw = ((fontptr->ch[i].tfmw) / (double) hconv) + .5;
      cptr->width = 
	cptr->height = 
	  cptr->xOffset = 
	    cptr->yOffset = 
	      cptr->yyOffset = 0;
    }
    return _TRUE;
  } else {
    fontptr->resident_p = _FALSE;

    if (tfm_info.family[0]
	&& strcmp (tfm_info.family, "UNSPECIFIED") == 0) {
      Warning("font family for %s is UNSPECIFIED; need to run dvicopy?",
	      fontptr->n);
#if 0
      return _TRUE;
    } else {
      return _FALSE;
    }
#else
    }
    /*
     * te: to make psfonts accessable, since we might
     *     use the pk files generated from ps2pk or gsftopk
     */
      return _FALSE;
#endif
  }
}
#endif



/*-->ReadFontDef*/
/**********************************************************************/
/****************************  ReadFontDef  ***************************/
/**********************************************************************/
void
ReadFontDef(k)
long4    k;
{
    long4    t;
    unsigned short i;
    struct font_entry *tfontptr; /* temporary font_entry pointer   */
    struct char_entry *tcharptr; /* temporary char_entry pointer  */
    static int      plusid = 0;
    bool font_found = _FALSE;
#ifdef LJ_RESIDENT_FONTS
    bool resident_font_located = _FALSE;
#endif
#ifdef LJ
    int depth, max_depth;
#endif

#ifdef DEBUG
    if (Debug)
      fprintf(stderr,"Mallocating %d Bytes)...\n", sizeof(struct font_entry ));
#endif

    if ((tfontptr = NEW(struct font_entry )) == NULL)
        Fatal("can't malloc space for font_entry");

    allocated_storage += sizeof(struct font_entry );

    tfontptr->next = hfontptr;
    tfontptr->font_file_id = NULL;
    fontptr = hfontptr = tfontptr;
    tfontptr->ncdl = 0;
    tfontptr->k = k;
    tfontptr->c = NoSignExtend(dvifp, 4); /* checksum */
    tfontptr->s = NoSignExtend(dvifp, 4); /* space size */
    tfontptr->d = NoSignExtend(dvifp, 4); /* design size */
    tfontptr->a = (int) NoSignExtend(dvifp, 1); /* length for font name */
    tfontptr->l = (int) NoSignExtend(dvifp, 1); /* device length */

#ifdef LJ
    tfontptr->max_width = tfontptr->max_height = tfontptr->max_yoff =
                          max_depth = 0;
#endif

    GetBytes(dvifp, tfontptr->n, tfontptr->a + tfontptr->l);
    tfontptr->n[tfontptr->a+tfontptr->l] = '\0';

    tfontptr->font_mag = (long4)((
         ActualFactor((long4)(1000.0*tfontptr->s/(double)tfontptr->d+0.5))
         * ActualFactor(mag)
#ifdef USEPXL
         * RESOLUTION * 5.0
#else
         * RESOLUTION
#endif
           ) + 0.5);
/*
printf("[%ld]=%lf * %lf * %lf + 0.5 = %ld\n",
    ((long)(1000.0*tfontptr->s/(double)tfontptr->d+0.5)),
    ActualFactor((long4)(1000.0*tfontptr->s/(double)tfontptr->d+0.5)),
    ActualFactor(mag),
    (double)RESOLUTION * 5,
    (long)tfontptr->font_mag );
*/

#ifdef LJ_RESIDENT_FONTS
    /* Pass in the name; fills in resident_p and resid (if resident). */

    resident_font_located = (bool) TryResident(tfontptr);
    
    if (tfontptr->resident_p)
      return;

    if (!(resident_font_located)) {
#endif

#ifdef KPATHSEA
    {
      kpse_glyph_file_type font_ret;
      char *name;
      unsigned dpi
        = kpse_magstep_fix ((unsigned) (tfontptr->font_mag / 5.0 + .5),
                            RESOLUTION, NULL);
      tfontptr->font_mag = dpi * 5; /* save correct dpi */
      
      name = kpse_find_pk (tfontptr->n, dpi, &font_ret);
      if (name)
        {
          font_found = _TRUE;
          strcpy (tfontptr->name, name);
          free (name);
          
          if (!STREQ (tfontptr->n, font_ret.name)) {
              fprintf (stderr,
                       "dvilj: Font %s not found, using %s at %d instead.\n",
                       tfontptr->n, font_ret.name, font_ret.dpi);
              tfontptr->c = 0; /* no checksum warning */
            }
          else if (!kpse_bitmap_tolerance ((double)font_ret.dpi, (double) dpi))
            fprintf (stderr,
                     "dvilj: Font %s at %d not found, using %d instead.\n",
                     tfontptr->name, dpi, font_ret.dpi);
          if (!( (G_noverbatim) || (G_quiet) ) )
            fprintf(stderr,"%d: using font <%s>\n", plusid,tfontptr->name);
        }
      else
        {
          tfontptr->font_file_id = NO_FILE;
          fprintf (stderr,
            "dvilj: Font %s at %u not found, characters will be left blank.\n",
            tfontptr->n, dpi);
        }
    }
#else /* not KPATHSEA */
      if (!(findfile(PXLpath, 
		     tfontptr->n, 
		     tfontptr->font_mag, 
		     tfontptr->name,
		     _FALSE,
		     0))) {
	Warning(tfontptr->name); /* contains error messsage */
	tfontptr->font_file_id = NO_FILE;
      }
      else {
	font_found = TRUE;
	if (!( (G_noverbatim) || (G_quiet) ) )
	  fprintf(stderr,"%d: using font <%s>\n",
		  plusid,tfontptr->name);
      }
#endif /* not KPATHSEA */

#ifdef LJ_RESIDENT_FONTS
    }
#endif

    tfontptr->plusid = plusid;
    plusid++;

    /* sprintf(tfontptr->psname,"%s.%ld.%d",
       tfontptr->n,(long)tfontptr->font_mag,tfontptr->plusid);*/

#ifdef LJ
    if (plusid >= HANDLE_MAX_FONTS)
        Fatal("can handle only %d fonts! ask a wizzard...\n",
               HANDLE_MAX_FONTS);
#endif
    if (tfontptr != pfontptr) {
        if (font_found) OpenFontFile();
        else
            pxlfp = NO_FILE;
    }
#ifdef USEPXL
    if ( pxlfp == NO_FILE ) {        /* allow missing pxl files */
        tfontptr->magnification = 0;
        tfontptr->designsize = 0;
#endif
        for (i = FIRSTFNTCHAR; i <= LASTFNTCHAR; i++) {
            tcharptr = &(tfontptr->ch[i]);
#ifdef USEPXL
            tcharptr->width = 0;
            tcharptr->height = 0;
            tcharptr->xOffset = 0;
            tcharptr->yOffset = 0;
#endif
            tcharptr->where.isloaded = _FALSE;
            tcharptr->where.address.fileOffset = NONEXISTANT;
            tcharptr->tfmw = 0;
        }
#ifdef USEPXL
        return;
    }
    t = (long4) NoSignExtend(pxlfp, 1);
    if (t == 0) {
        t = (long4) NoSignExtend(pxlfp, 1);
        t = (long4) NoSignExtend(pxlfp, 2);
        if (t == 1002)
            tfontptr->id = id1002;
        else if (t == 1001)
            tfontptr->id = id1001;
        else
            Fatal("Unknown Version of PXL-format\n");
    } else {
        if (t == PK_PRE)    {
            unsigned char   temp_byte;
            temp_byte = (unsigned char) NoSignExtend(pxlfp, 1);
            if (temp_byte != PK_ID) Fatal(
               "Wrong Version of pk file!  (%d should be 89)\n",
                             (int)temp_byte);
            else
                tfontptr->id = pk89;
        } else
            Fatal("unknown font format in file <%s> !\n",fontptr->name);
    }

    if ((tfontptr->id == id1002) || (tfontptr->id == id1001)) {
        fseek(pxlfp, -20l, 2);

        t = NoSignExtend(pxlfp, 4);
        if ((tfontptr->c != 0) && (t != 0) && (tfontptr->c != t))
    Warning("font = \"%s\",\n->tfm checksum = %lX,\n->pxl checksum = %lX",
                          tfontptr->name, tfontptr->c, t);
        tfontptr->magnification = NoSignExtend(pxlfp, 4);
        tfontptr->designsize    = NoSignExtend(pxlfp, 4);

        if (tfontptr->id == id1001)
            fseek(pxlfp, (long) (NoSignExtend(pxlfp, 4) * 4), 0);
        else
            fseek(pxlfp, (long) NoSignExtend(pxlfp, 4) , 0);

        for (i = FIRSTFNTCHAR; i <= 127; i++) {   /* only defined for 7bit*/
            tcharptr = &(tfontptr->ch[i]);
            tcharptr->width   = (unsigned short) NoSignExtend(pxlfp, 2);
            tcharptr->height  = (unsigned short) NoSignExtend(pxlfp, 2);
            tcharptr->xOffset = (short) SignExtend(pxlfp, 2);
            tcharptr->yOffset = (short) SignExtend(pxlfp, 2);
            tcharptr->where.isloaded = _FALSE;
            if (tfontptr->id == id1001)
                tcharptr->where.address.fileOffset = NoSignExtend(pxlfp,4) * 4;
            else
                tcharptr->where.address.fileOffset = NoSignExtend(pxlfp,4);
            tcharptr->tfmw = (long4)
            (   (double)(NoSignExtend(pxlfp, 4))
              * (double)tfontptr->s / (double) 0x100000 );
            tcharptr->cw = (long4)(((double)tcharptr->tfmw/(double)hconv) + 0.5);

            if (tcharptr->width  > CHAR_WIDTH_LARGE  ||
                tcharptr->height > CHAR_HEIGTH_LARGE )
                tcharptr->charsize = LARGE_SIZE;
            else
                tcharptr->charsize = SMALL_SIZE;
#ifdef LJ
            max(tfontptr->max_width,tcharptr->width);
            max(tfontptr->max_height,tcharptr->height);
            if (tcharptr->yOffset > 0  && (int)tfontptr->max_yoff < (int)tcharptr->yOffset) 
	      tfontptr->max_yoff = tcharptr->yOffset;
            if ((depth = tcharptr->height - tcharptr->yOffset)>max_depth)
                   max_depth = depth;
#endif

        }
#ifdef LJ
        tfontptr->max_height = max_depth ? tfontptr->max_yoff+max_depth :
                                           tfontptr->max_yoff+1;
#endif
    } else { /* PK 89 format */
        unsigned char   temp_byte;
        register unsigned char  flag_byte;
        long4    hppp, vppp, pkloc, packet_length;
        int     car, ii;

        /* read comment */
        for ( ii = temp_byte = (unsigned char) NoSignExtend(pxlfp, 1);
              ii>0; ii--) {
            flag_byte = (unsigned char) NoSignExtend(pxlfp, 1);
#ifdef DEBUG
            if (Debug) fprintf(stderr, "%c", flag_byte ) ;
#endif
        }
#ifdef DEBUG
        if (Debug) fprintf(stderr, "\n");
#endif
        pkloc = 3 + (int)temp_byte;
        tfontptr->designsize = NoSignExtend(pxlfp, 4);

        t = NoSignExtend(pxlfp, 4);
        if ((tfontptr->c != 0) && (t != 0) && (tfontptr->c != t))
          Warning("font = \"%s\",\n->tfm checksum = %lX,\n->pxl checksum = %lX",
                 tfontptr->name, tfontptr->c, t);

        hppp = NoSignExtend(pxlfp, 4);
        vppp = NoSignExtend(pxlfp, 4);
        if (hppp != vppp)
            Warning("aspect ratio is %ld:%ld (should be 1:1)!", (long)hppp,(long)vppp);
        tfontptr->magnification = (long4)(hppp * 72.27 * 5 / 65536l + 0.5);

        pkloc += 16;
        flag_byte = skip_specials(&pkloc);

        while (flag_byte != PK_POST) {
        if ((flag_byte & 7) == 7) {
        /* fprintf(stderr,"\nRead long character preamble\n"); */

           packet_length = (unsigned long4)NoSignExtend(pxlfp,4);
           if ((car = (int)NoSignExtend(pxlfp, 4)) > (LASTFNTCHAR))
                Fatal("Bad character (%d) in PK-File\n",(int)car) ;

           tcharptr = &(tfontptr->ch[car]);
           tcharptr->where.address.fileOffset = pkloc;
           /* set pkloc to end_of_packet */
           pkloc += packet_length + 8;

           tcharptr->tfmw = (long4) NoSignExtend(pxlfp, 4);
           (void) NoSignExtend(pxlfp, 4); /* horesc not used */
           (void) NoSignExtend(pxlfp, 4); /* not used */

           tcharptr ->width   = (unsigned short) NoSignExtend(pxlfp, 4);
           tcharptr ->height  = (unsigned short) NoSignExtend(pxlfp, 4);
           tcharptr ->xOffset = (short) SignExtend(pxlfp, 4);
           tcharptr ->yOffset = (short) SignExtend(pxlfp, 4);
           tcharptr ->where.isloaded = _FALSE;
        } else if (flag_byte & 4) {
            /* fprintf(stderr,"Read extended short character preamble\n"); */

            packet_length = ((long4) flag_byte & 3) * 65536l +
                (unsigned short) NoSignExtend(pxlfp, 2);
            if ((car = (int)NoSignExtend(pxlfp, 1)) > (LASTFNTCHAR))
                Fatal("Bad character (%d) in PK-File\n",(int)car) ;

            tcharptr = &(tfontptr->ch[car]);
            tcharptr->where.address.fileOffset = pkloc;
            /* set pkloc to end_of_packet */
            pkloc += packet_length + 3;

            tcharptr->tfmw = (long4) NoSignExtend(pxlfp, 3);
/*
            { register unsigned short t;
              t = (unsigned short) NoSignExtend(pxlfp, 1);
              tcharptr->tfmw = t * 65536l +
              (unsigned short) NoSignExtend(pxlfp, 2);
            }
*/
            /* horesc not used */
            (void) NoSignExtend(pxlfp, 2) ;
            tcharptr ->width   = (unsigned short) NoSignExtend(pxlfp,2);
            tcharptr ->height  = (unsigned short) NoSignExtend(pxlfp,2);
            tcharptr ->xOffset = (short) SignExtend(pxlfp, 2);
            tcharptr ->yOffset = (short) SignExtend(pxlfp, 2);
            tcharptr ->where.isloaded = _FALSE;
        } else {
            /* fprintf(stderr,"<Read short character preamble@>\n"); */

            packet_length = ((long4)flag_byte & 3) * 256 +
                NoSignExtend(pxlfp, 1) ;
            if ((car = (int)NoSignExtend(pxlfp, 1)) > (LASTFNTCHAR))
                Fatal("Bad character (%d) in PK-File\n",(int)car) ;

            tcharptr = &(tfontptr->ch[car]);
            tcharptr->where.address.fileOffset = pkloc;
            /* set pkloc to end_of_packet */
            pkloc += packet_length + 2 ;

            tcharptr->tfmw = (long4) NoSignExtend(pxlfp, 3);
/*
            { register unsigned short t;
              t = (unsigned short) NoSignExtend(pxlfp, 1);
              tcharptr->tfmw = t * 65536l +
              (unsigned short) NoSignExtend(pxlfp, 2);
            }
*/
            /* horesc not used */
            (void) NoSignExtend(pxlfp, 1) ;
            tcharptr ->width   = (unsigned short) NoSignExtend(pxlfp,1);
            tcharptr ->height  = (unsigned short) NoSignExtend(pxlfp,1);
            tcharptr ->xOffset = (short) SignExtend(pxlfp, 1);
            tcharptr ->yOffset = (short) SignExtend(pxlfp, 1);
            tcharptr ->where.isloaded = _FALSE;
        }

        tcharptr->tfmw = (long4)
           ( tcharptr->tfmw * (double)tfontptr->s / (double) 0x100000 );

        tcharptr->cw = (long4)(((double)tcharptr->tfmw /
            (double)hconv) + 0.5);

        if (tcharptr->width  > CHAR_WIDTH_LARGE  ||
            tcharptr->height > CHAR_HEIGTH_LARGE )
            tcharptr->charsize = LARGE_SIZE;
        else
            tcharptr->charsize = SMALL_SIZE;

#ifdef LJ
/*
printf("char=%d: this=%d, max_width=%d, this=%d,max_height=%d, this=%d,max_yoff=%d\n",
       car, tcharptr->width, tfontptr->max_width,
       tcharptr->height,tfontptr->max_height, 
       tcharptr->yOffset,tfontptr->max_yoff);
*/
        max(tfontptr->max_width, tcharptr->width);
        max(tfontptr->max_height,tcharptr->height);
	if (tcharptr->yOffset > 0  && (int)tfontptr->max_yoff < (int)tcharptr->yOffset)
	  tfontptr->max_yoff = tcharptr->yOffset;

        if ((depth = tcharptr->height - tcharptr->yOffset) > max_depth)
	  max_depth = depth;
#endif
/*
fprintf(stderr,"char=%d, yotcharptr=%lx, flag_byte=%d, font=%lx\n",car, tcharptr,flag_byte,tfontptr);
*/
        tcharptr->flag_byte = flag_byte;
        fseek(pxlfp, (long) pkloc, 0);
        flag_byte = skip_specials(&pkloc);

        } /* end of while */
#ifdef LJ
tfontptr->max_height = max_depth ? tfontptr->max_yoff+max_depth :
                                   tfontptr->max_yoff+1;
#endif

/*
printf("fontid=%d: max_width=%u, max_height=%d, max_yoff=%u\n",
        tfontptr->plusid, tfontptr->max_width,
        tfontptr->max_height, tfontptr->max_yoff);
*/

#else
    if ( pxlfp == NO_FILE )        /* allow missing pxl files */
    return;

    gfin = pxlfp;
    seekpost();
    readpost();
    if ((tfontptr->c != 0) && (checksum != 0) && (tfontptr->c != checksum))
    Warning("font = \"%s\",\n-->font checksum = %d,\n-->dvi checksum = %d",
        tfontptr->name, tfontptr->c, checksum);

    for(i=FIRSTFNTCHAR; i<=LASTFNTCHAR; i++) {
    if (char_exists[i]) {
        tcharptr = &(tfontptr->ch[i]);
        tcharptr->tfmw = (long4)(((float)tfm_wd[i]*(float)tfontptr->s) /
           (float)((long4)1l<<20));
        tcharptr->where.address.fileOffset = char_pointer[i];
      }
#ifdef LJ
/*                 GF USER PLEASE CHECK IF THIS CODE WORKS
    tfontptr->max_width = gf_font_max_m;
    tfontptr->max_height = gf_font_max_n;
    tfontptr->max_yoff = gf_font_min_n;
*/
#endif
#endif
/*****************************************************************************/
/*if (tcharptr->charsize==LARGE_SIZE)                                        
     fprintf(stderr,"%d:\t <%c> w=%d h=%d xO=%d yO=%d tfmw=%ld cw=%ld %d\n",       
     i,(char) i,                                                          
     tcharptr->width,tcharptr->height,tcharptr->xOffset,tcharptr->yOffset,
     (long)tcharptr->tfmw, (long)tcharptr->cw, (int)(tcharptr->charsize));           
 */
/*****************************************************************************/
    }
  }


unsigned char
skip_specials( pkloc )
long4    *pkloc;
{
    long4    i, j;
    register unsigned char  flag_byte;
    do {
    flag_byte = (unsigned char) NoSignExtend(pxlfp, 1);
/*
fprintf(stderr,"flagbyte = %d, pkloc=%ld\n",(int)flag_byte,(long)*pkloc);
*/

    (*pkloc) ++;
    if (flag_byte  >= 240)
        switch (flag_byte) {
        case 240:
        case 241:
        case 242:
        case 243 : {
            i = 0 ;
            for (j = 240; j <= (long4)flag_byte; j++) {
                i = 256 * i + NoSignExtend(pxlfp, 1) ;
                (*pkloc) ++;
            }
            for (j = 1; j <= i; j++) {
                (void) NoSignExtend(pxlfp, 1) ;
                (*pkloc) ++;
            }
            break;
        }
        case 244 : {
            i = NoSignExtend(pxlfp, 4);
            (*pkloc) += 4;
            break;
        }
        case 245 :
            break;
        case 246 :
            break ;
        case 247:
        case 248:
        case 249:
        case 250:
        case 251:
        case 252:
        case 253:
        case 254:
        case 255: {
            Fatal("Unexpected flagbyte %d!\n",
                 (int)flag_byte) ;
            }
        }
    } while (!((flag_byte < 240) || (flag_byte == PK_POST))) ;
    return(flag_byte);
}


/*-->ReadPostAmble*/
/**********************************************************************/
/**************************  ReadPostAmble  ***************************/
/**********************************************************************/
/***********************************************************************
    This  routine  is  used  to  read  in  the  postamble  values.    It
    initializes the magnification and checks  the stack height prior  to
    starting printing the document.
***********************************************************************/
void
ReadPostAmble(load)
bool load;
{
    FindPostAmblePtr (&postambleptr);
    if (NoSignExtend(dvifp, 1) != POST)
        Fatal ("POST missing at head of postamble");
#ifdef DEBUG
    if (Debug)
        fprintf(stderr,"got POST command\n");
#endif
    ppagep = NoSignExtend(dvifp, 4);
    num = NoSignExtend(dvifp, 4);
    den = NoSignExtend(dvifp, 4);
    mag = NoSignExtend(dvifp, 4);
    if ( usermag > 0 && usermag != mag )
        Warning("DVI magnification of %ld over-ridden by user (%ld)",
                     (long)mag, (long)usermag );
    if ( usermag > 0 )
        mag = usermag;
    hconv = DoConv(num, den, hconvRESOLUTION);
    vconv = DoConv(num, den, vconvRESOLUTION);
    (void) NoSignExtend(dvifp, 4);   /* height-plus-depth of tallest page */
    (void) NoSignExtend(dvifp, 4);   /* width of widest page */
    if (NoSignExtend(dvifp, 2) >= STACK_SIZE)
        Fatal ("Stack size is too small");
    (void) NoSignExtend(dvifp, 2);   /* this reads the number of pages in */
                     /* the DVI file */
#ifdef DEBUG
    if (Debug)
        fprintf(stderr,"now reading font defs");
#endif
    if (load)
        GetFontDef ();
}


/*-->LoadAChar*/
/**********************************************************************/
/***************************** LoadAChar ******************************/
/**********************************************************************/
void
LoadAChar(c, ptr)
long4    c;
register struct char_entry *ptr;
{
    long4    *pr;
    long4    bytes;

    if (ptr->where.address.fileOffset == NONEXISTANT 
#ifdef LJ_RESIDENT_FONTS
	|| fontptr->resident_p
#endif
	) {
      ptr->where.isloaded = _FALSE;
      return;
    }

    OpenFontFile();

#ifdef DEBUG
    if (Debug)
      fprintf(stderr, "LoadAChar: <%c>(%ld) from file at pos %ld\n",
          (char)c,(long)c,(long)ptr->where.address.fileOffset);
#endif

#ifdef USEPXL

    fseek(pxlfp, ptr->where.address.fileOffset, 0);

    if (fontptr->id == pk89) {
#ifdef PARANOIA
        unsigned char   temp;
        temp = (unsigned char) NoSignExtend(pxlfp, 1);

        if ((int)(ptr->flag_byte) != (int)temp) {
	       fprintf(stderr,"font=%lx, ptr=%lx\n",fontptr,ptr);
                Fatal("%d: oh boy! old flag %d, new flag %d, ptr=%lx\n",
                          (int)c,(int)(ptr->flag_byte),(int)temp,ptr);
            }
#endif

        if ((ptr->flag_byte & 7) == 7) {
            bytes = ((long4) NoSignExtend(pxlfp, 4)) - 28;
            fseek(pxlfp, ptr->where.address.fileOffset + 36, 0);
/*
fprintf(stderr,"bytes=%ld, seeking at %ld\n",
            (long)bytes, (long)ptr->where.address.fileOffset + 36);
*/
        } else if ((ptr->flag_byte & 4) == 4) {
            bytes = ((long4)ptr->flag_byte & 3)
                * 65536l + NoSignExtend(pxlfp, 2) - 13;
            fseek(pxlfp, ptr->where.address.fileOffset + 16, 0);
        } else {
            bytes = ((long4)ptr->flag_byte & 3)
                * 256 + NoSignExtend(pxlfp, 1) - 8;
            fseek(pxlfp, ptr->where.address.fileOffset + 10, 0);
        }
    } else if (fontptr->id == id1002)
        bytes =  ((( (long4)ptr->width + 7) >> 3) * (long4) ptr->height);
    else if (fontptr->id == id1001)
        bytes =  4 * (((long4)ptr->width + 31) >> 5) * (long4)ptr->height;
    else 
        bytes = 0;

    if (bytes > 0) {
                                          /* do NOT load Huge characters */
      if ((bytes > HUGE_CHAR_PATTERN) && (fontptr->id != pk89)) {
	qfprintf(stderr,"Huge Character <%c> (%ld Bytes)\n", (char)c, (long)bytes);
        ptr->charsize = HUGE_SIZE;
        ptr->where.isloaded = _FALSE;
      } else {
        if ( (pr = (long4 *)malloc( bytes )) == NULL )
            Fatal("Unable to allocate %ld bytes for char <%c>\n",
                         (long)bytes, (char)c);
/*
 * else fprintf(stderr,"allocating %ld bytes char <%c>(%d)\t at 0x%lx\n",
 *                       (long)bytes, (char)c,(int)c,(long)pr);
 */ 
#ifdef DEBUG
        if (Debug)
          fprintf(stderr,
           "Allocating Char <%c>, FileOffset=%lX, Bytes=%ld (%d) <%d>\n",
              (char) c, (long)ptr->where.address.fileOffset, (long)bytes,
              (int)bytes, (unsigned int)bytes);
#endif
        allocated_storage += bytes;
        fread(pr, 1, (int) bytes , pxlfp);
        ptr->where.address.pixptr = pr;
      }
    } 
#else
    fseek(gfin, ptr->where.address.fileOffset, 0);
    gettochar();
    readbits();
    if (num_bytes > HUGE_CHAR_PATTERN)
      ptr->charsize = HUGE_SIZE;
#endif
    ptr->where.isloaded = _TRUE; 
    if (ptr->charsize != SMALL_SIZE
#ifdef LJ
        /* we might have problems at the edge of the paper with diff. sized characters
         * the correct treatment would be to check whether the bounding box of 
         * tfontptr is within the paper relative to the current position  
         */
	|| fontptr->max_height > CHAR_HEIGTH_LARGE  
	|| (rasterfont[fontptr->plusid])
#endif
       )
        return;

    EmitChar(c, ptr);
#ifdef USEPXL
    /* we should really free the space used by the PXL data after this
       point, but it is not large, and besides, we may want to be
       more clever in the future, about sending bitmaps.  So keep
       the data around */
#endif
}
/*-->SetChar*/
/**********************************************************************/
/*****************************  SetChar  ******************************/
/**********************************************************************/
void
SetChar(c, command, PassNo, do_posn,in_string)
long4    c;
short   command;
int     PassNo;
bool do_posn,in_string;
{
    register struct char_entry *ptr;  /* temporary char_entry pointer */
    bool pos_after = _FALSE;

    ptr = &(fontptr->ch[c]);
    if (!((ptr->where.isloaded) || (ptr->charsize == HUGE_SIZE)))
        LoadAChar(c, ptr);
    if (PassNo == 0)
        return;

    if (do_posn) {
#ifdef IBM3812
        if (CharStringPos>0) {
            fprintf(stderr,"!!!! That should never happen!!!\n");
            CharStringOut;
        }
#endif
        SetPosn(h, v);
    }

/*
printf("(%d) hh=%ld (+%ld/+%ld), h=%ld, xh=%ld,xhh=%ld, [%ld|%ld] ->%d\n",
    (int)do_posn,(long)hh,(long)ptr->cw,(long)ptr->cw*(long)hconv,(long)h,
    (long)PIXROUND(h, hconv),
    (long)PIXROUND(hh, hconv),
    (long)labs((hh-h)),(long)hconv,(labs((hh-h))>hconv)
    );
*/

    if (in_string && (labs((hh-h))>hconv)) {
#ifdef IBM3812
        CharStringOut;
#endif
        SetPosn(h, v);
    }

/*
fprintf(stderr,"raster?=%d - last_ry=%d, last_rx=%d,mmax-height=%d\n",
	(int)last_ry < fontptr->max_height, (int)last_ry,(int)last_rx,
	(int)fontptr->max_height);
*/

    if (fontptr->font_file_id != NO_FILE) {      /* ignore missing fonts */
         if (
#ifdef LJ_RESIDENT_FONTS
	     !fontptr->resident_p && 
#endif
	     (ptr->charsize != SMALL_SIZE 
#ifdef LJ
	      /* the LaserJet cannot print characters 
	       * where the bounding box lies outside the 
	       * paper edge. Missing: right paper edge
	       */
	      || last_ry < (int)fontptr->max_height
	      || fontptr->max_height > CHAR_HEIGTH_LARGE
	      || (rasterfont[fontptr->plusid])
/***** KYOCERA *****/
#ifdef SEVENBIT
              || (c==32)
#endif
/***** KYOCERA *****/
#endif
	      )) {
#ifdef LJ
	  int     tmp;
	  char    sign;
	  
	  if (!do_posn) {
	    SetPosn(h, v);
	  }
#ifdef USEPXL
	  tmp = (int) -ptr->yOffset;
#else
	  tmp = (int) num_rows-y_offset;
#endif
	  if (tmp != 0) {
	    if (tmp < 0) {
	      sign = '-'; tmp = -tmp;
	    } else
	      sign = '+';
	    EMIT(outfp, "\033*p%c%dY", sign, tmp);
	  }
#ifdef USEPXL
	  tmp = (int) -ptr->xOffset;
#else
	  tmp = (int) -x_offset;
#endif
	  if (tmp != 0) {
	    if (tmp < 0) {
	      sign = '-'; tmp = -tmp;
	    } else
	      sign = '+';
	    EMIT(outfp, "\033*p%c%dX", sign, tmp);
	  }
#endif
#ifdef IBM3812
	  CharStringOut;
#endif
#ifdef DEBUG
    if (Debug)
#ifndef vms
        fprintf(stderr,"Raster character <%c> %hd\n", (char) c,(short)c);
#else
        fprintf(stderr,"Raster character <%c> %d\n", (char) c,(short)c);
#endif
#endif
            RasterChar(ptr);
            pos_after = _TRUE;
        } else {
	    unsigned char cc;
	    cc = VisChar((char)c);
#ifdef IBM3812
#ifdef USEPXL
            if ( ptr->yyOffset || (!in_string) ) {
                CharStringOut;
                MoveVert(ptr->yyOffset);
                sprintf(PMPformat, "\01%c", cc);
                PMPout(2, PMPformat);
                MoveVert((int)-(ptr->yyOffset));
            } else {
#endif
                if (CharStringPos==CHARSTRINGMAX)
                    CharStringOut;

                CharString[CharStringPos] = cc;
                CharStringPos++;
#ifdef USEPXL
	    }
#endif
#endif
#ifdef LJ
#define TRANSPARENTCHAR(ch) \
	    if ((ch == 0l) || (ch >= 7l && ch <= 15l) || (ch == 27l)) \
		EMIT(outfp, "\033&p1X%c", (unsigned char)ch); \
	    else EMITC((unsigned char)ch)
#ifdef USEPXL
            if (ptr->yyOffset) {
#ifndef vms
                EMIT(outfp, "\033*p+%hdY", ptr->yyOffset);
                TRANSPARENTCHAR(cc);
                EMIT(outfp, "\033*p-%hdY", ptr->yyOffset);     /* GUGUGU 255 */
#else
                EMIT(outfp, "\033*p+%dY", ptr->yyOffset);
                TRANSPARENTCHAR(cc);
                EMIT(outfp, "\033*p-%dY", ptr->yyOffset);     /* GUGUGU 255 */
#endif
            } else
#endif
/*                EMITC( (unsigned char)c);*/
               { TRANSPARENTCHAR(cc);}
#endif
      }
        hh += (long4) ptr->cw*hconv;
    }
    if (command <= SET4)
        h += ptr->tfmw;
    if (pos_after) {
        SetPosn(h, v);
      }
}
 

void
DoBop()
{
    struct font_entry *p;
#ifdef LJ
    register short i;
    if (fonts_used_on_this_page > MAX_FONTS_PER_PAGE) {
       for (i = 0; i < HANDLE_MAX_FONTS; i++)
          rasterfont[i] = _FALSE;
    }
    fonts_used_on_this_page = 0;
#endif
    for (p = hfontptr; p; p = p->next) {
        p->used_on_this_page = _FALSE;
    }
}


/*-->SetFntNum*/
/**********************************************************************/
/****************************  SetFntNum  *****************************/
/**********************************************************************/
void
SetFntNum(k, Emitting)
long4    k;
bool Emitting;
/*  this routine is used to specify the font to be used in printing future
    characters */
{
#ifdef LJ
    static unsigned short plusid = 0;
#endif
    fontptr = hfontptr;
    while ((fontptr != NULL) && (fontptr->k != k))
        fontptr = fontptr->next;
    if (fontptr == NULL)
        Fatal("font %ld undefined", (long)k);
    if (Emitting && fontptr->font_file_id != NO_FILE) {
        if (!fontptr->used_on_this_page
#ifdef LJ_RESIDENT_FONTS
	    && !fontptr->resident_p
#endif
	    ) {
            fontptr->used_on_this_page = _TRUE;
#ifdef LJ
            if (++fonts_used_on_this_page > MAX_FONTS_PER_PAGE) {
              qfprintf(stderr,"this is the %d. font on this page!",
                       fonts_used_on_this_page);
              qfprintf(stderr," (max = %d) rastering characters!\n",
                       MAX_FONTS_PER_PAGE);
               rasterfont[fontptr->plusid] = _TRUE;
            }
#endif
        }
#ifdef DEBUG
	if (Debug)
	  fprintf(stderr, "Switching to font #%ld (%s).\n", k, fontptr->n);
#endif
        /* activate font */
#ifdef IBM3812
        sprintf(PMPformat, "\323%c", (unsigned char)fontptr->plusid);
        PMPout(2, PMPformat);
#endif
#ifdef LJ
        if (!rasterfont[fontptr->plusid]) {
#ifdef LJ_RESIDENT_FONTS
	  if (fontptr->resident_p) {
#ifdef DEBUG
            if (Debug)
              fprintf(stderr, "Resident font #%d.\n", fontptr->resid);
#endif
	    EMIT(outfp, "\033(%s", fontptr->symbol_set);
	    EMIT(outfp, "\033(s%up%.2f%c%us%db%uT",
		        fontptr->spacing,
		        /* height in points, or pitch */
		        fontptr->spacing ? fontptr->s / 65536.0
                                         : fontptr->pitch ,
                        fontptr->spacing ? 'v' : 'h', /* height or pitch? */
                        fontptr->style,       /* upright, italic, ... */
                        fontptr->weight,      /* regular, bold, ... */
                        fontptr->resid);
	  } else
#endif /* LJ_RESIDENT_FONTS */	  
          if (fontptr->plusid>0) EMIT(outfp, "\033(%dX", fontptr->plusid);
          else                   EMIT(outfp, "\033(X");
        }
/* else fprintf(stderr,"I am doing rasterfont for plusid=%d instead\n",
                fontptr->plusid);
*/
#endif
    }
#ifdef LJ    /* reassignment of printer font id  0.48 */
    else if (fontptr->font_file_id != NO_FILE
#ifdef LJ_RESIDENT_FONTS
	     && !fontptr->resident_p
#endif    
	     ) {
      if (fontptr->ncdl == 0) {
#ifdef DEBUG
	if (Debug)
	  fprintf(stderr, "Changing plusid from %d to %d\n", 
		  fontptr->plusid, (int)plusid);
#endif
	fontptr -> plusid = plusid;
	plusid ++;
      }
    }
#endif
}


/*-->SetPosn*/
/**********************************************************************/
/*****************************  SetPosn  ******************************/
/**********************************************************************/
void                  /* output a positioning command */
SetPosn(x, y)
long4    x, y;
{
    int     rx, ry;
    rx = (int)PIXROUND(x, hconv) + x_goffset;
    ry = (int)PIXROUND(y, vconv) + y_goffset;

/*
* fprintf(stderr,"setposn to %ld/%ld, %d/%d\n",(long)x,(long)y,rx,ry);
*/

#ifdef IBM3812
    PMPcont(3);
    PMPoutC('\340');
    EMITWORD(LARGER(rx,0));

    if (last_ry != ry) { /* necessary to set new y-position */
        PMPcont(3);
        PMPoutC('\341');
        EMITWORD(LARGER(ry,0));
    }
#endif
#ifdef LJ
    if (last_ry != ry)   /* necessary to set new y-position */
        EMIT(outfp, "\033*p%dx%dY", LARGER(rx,0), LARGER(ry,0));
    else
        EMIT(outfp, "\033*p%dX", LARGER(rx,0));
#endif

    last_ry = ry;    /* last y-position on output device */
    last_rx = rx;    /* last x-position on output device */
/*
 * must know where device "really" is horizontally, for rel. posning.
 * (maybe in the future), but we always use direct positioning for
 * vertical movement.
 */
    /* hh = rx * hconv; */
    hh = x;
    vv = y;
/*
 *     fprintf(stderr,"DoPosn: x=%ld, y=%ld, rx=%d, ry=%d, hh=%ld, vv=%ld\n",
 *               (long)x,(long)y,rx,ry,(long)hh,(long)vv);
 */
}


#ifdef IBM3812
/*-->PMPLine*/
/**********************************************************************/
/*****************************  PMPLine  ******************************/
/**********************************************************************/
void       /* drawing lines on the 3812 using PMP vector commands */
PMPLine(w, y, x)
int     w, y, x;
{

    if ((w == 0) || ((x == 0) && (y == 0)))
        return;

/*
fprintf(stderr,"w=%d / %d - %d, y=%d / %d - %d, x=%d / %d - %d\n",
        w,(char)(w & 0xff),(int)((signed_char)(w & 0xff)),
        y,(char)(y & 0xff),(int)((signed_char)(y & 0xff)),
        x,(char)(x & 0xff),(int)((signed_char)(x & 0xff)));
*/

    if ( (((signed_char)(x & 0xff)) == x ) &&
        ( ((signed_char)(y & 0xff)) == y ) ) {
        PMPcont(6);
        PMPout(1, "\370");
        EMITWORD(3);      /* length of vector */
        PMPoutC((unsigned char)(0x80 | 0x00 | (unsigned char) w));
        PMPoutC((signed_char)(y & 0xff));
        PMPoutC((signed_char)(x & 0xff));
/*
        fprintf(stderr,"F8 00 03: w=%d, x=%d(%d-%.2X), y=%d(%d-%.2X),\n",
        w,x,(char)(x & 0xff),(signed_char)(x & 0xff),
          y,(char)(y & 0xff),(signed_char)(y & 0xff));
*/

    } else {
        PMPcont(8);
        PMPout(1, "\370");
        EMITWORD(4 + 1);      /* length of vector */
        PMPoutC((unsigned char)(0xC0 | 0x00 | (unsigned char) w));
        EMITWORD(y);
        EMITWORD(x);
/*
        fprintf(stderr,"F8 00 05: w=%d, x=%d, y=%d,\n", w,x,y);
*/
    }
}


#endif
/*-->SetRule*/
/**********************************************************************/
/*****************************  SetRule  ******************************/
/**********************************************************************/
void                   /*   this routine will draw a rule */
SetRule(a, b, Set)
long4    a, b;
int     Set;
{
    long4    xx, yy;
#ifdef IBM3812
    short   hor_offset, vert_offset, ll;
#endif
    if ( a > 0 && b > 0 ) {
        SetPosn(h, v);             /* lower left corner */
        xx = (long4)PIXROUND(b, hconv);     /* width */
        yy = (long4)PIXROUND(a, vconv);     /* height */

#ifdef DEBUG
        if (Debug)
            fprintf(stderr,"Rule xx=%ld, yy=%ld\n", (long)xx, (long)yy);
#endif

#ifdef IBM3812
        hor_offset  = (short)(last_ry - yy);
        if (hor_offset < 0) yy += hor_offset;
        if (last_rx < 0) xx += last_rx;

        if (Landscape) {
          if (last_ry > MAX_PAGE_WIDTH) yy += MAX_PAGE_WIDTH-last_ry;
          hor_offset  = (short)(MAX_PAGE_HEIGHT - (last_rx + xx));
        } else {
          if (last_ry > MAX_PAGE_HEIGHT) yy += MAX_PAGE_HEIGHT-last_ry;
          hor_offset  = (short)(MAX_PAGE_WIDTH - (last_rx + xx));
        }
        if (hor_offset < 0) xx += hor_offset;

        if ((xx > 31) && (yy > 31)) {
/*
 *   fill area by multiple lines  (kind of a mess)
 *   process for simplicity always horizontally
 */

/* fprintf(stderr, "large box: w=%d, x=%d, y=%d\n",(int)yy,(int)xx,0);*/

            hor_offset  = HOR_HALF(30);
            MoveHor(hor_offset);
            vert_offset = VERT_HALF(30);
            MoveVert(-vert_offset);
            ll = (short)xx - 30;

            for (; yy > 30; yy -= 30) {
                PMPLine(30, 0, ll);
                MoveHor(-ll);
                MoveVert(-30);
            }

            hor_offset  = -hor_offset     + HOR_HALF(yy);
            MoveHor(hor_offset);
            vert_offset = (vert_offset - 30) + VERT_HALF(yy);
            MoveVert(-vert_offset);

            PMPLine((int)yy, 0, (int)(xx - yy));

        } else if ( (yy < xx) && (xx > 0) ) {

/* fprintf(stderr, "hori rule: w=%d, x=%d, y=%d\n",(int)yy,(int)(xx-yy),0);*/

            hor_offset  = HOR_HALF(yy);
            vert_offset = VERT_HALF(yy);

            MoveHor(hor_offset);
            MoveVert(-vert_offset);

            PMPLine((int)yy, 0, (int)(xx - yy));
        } else if ( (xx < yy) && (yy > 0)) {

            hor_offset  = HOR_HALF(xx);
            vert_offset = VERT_HALF(xx);
/*
 fprintf(stderr, "move: x=%d, y=%d\n",hor_offset,-vert_offset);
 fprintf(stderr, "vert rule: w=%d, x=%d, y=%d\n",(int)xx,0,(int)-(yy-xx));
*/
            MoveHor(hor_offset);
            MoveVert(-vert_offset);

            PMPLine((int)xx, (int)-(yy - xx), 0);
        } else if (xx == yy) {
            short     y0;  /* small square box!! */

            y0 = (short)yy / 2;
            hor_offset  = HOR_HALF(y0);
            MoveHor(hor_offset);
            vert_offset = VERT_HALF(y0);
            MoveVert(-vert_offset);
            ll = (short)xx - y0;

            PMPLine((int)y0, 0, ll);

            hor_offset  = -(ll + hor_offset);
            vert_offset = (y0 - vert_offset);

            yy -= (long4)y0;
            hor_offset  += HOR_HALF(yy);
            MoveHor(hor_offset);
            vert_offset += VERT_HALF(yy);
            MoveVert(-vert_offset);

            PMPLine((int)yy, 0, (int)xx - yy);
        }
#endif
#ifdef LJ
	if (last_ry + 1 < yy) yy = last_ry + 1;
	if (last_rx < 0) xx += last_rx;

	if (((int)pgsiz_dots >0) && ((int)last_ry > (int)pgsiz_dots))
	  yy += (long4)pgsiz_dots - (long4)last_ry;

        if ((yy>0) && (xx>0))
                EMIT(outfp, "\033*p-%ldY\033*c%lda%ldbP", (long)yy - 1, (long)xx, (long)yy);
#endif
        last_ry = UNKNOWN;       /* next time full positioning */
    }
    if (Set)
        h += b;
}


/*-->SetString*/
/**********************************************************************/
/*****************************  SetString  ****************************/
/**********************************************************************/
void
SetString(firstch, PassNo)    /* read and set a consecutive string of chars */
short   firstch;
int     PassNo;
{
    short   c;
    register unsigned short i;

#ifdef DEBUG
    if (Debug)
      fprintf(stderr, "SetString ");
#endif
    for (i = 0, c = firstch; c >= SETC_000 && c <= SETC_127; i++) {
#ifdef DEBUG
        if (Debug)
          fprintf(stderr, "%d(%c) ", c, c);
#endif
        SetChar((long4)c,  c, PassNo, (bool)(i==0),_TRUE);
        c = (short) NoSignExtend(dvifp, 1);
    }
    fseek(dvifp, -1l, 1);    /* backup one character */
#ifdef IBM3812
    CharStringOut;
#endif
#ifdef DEBUG
    if (Debug)
      fprintf(stderr, "...SetString\n");
#endif
}

#ifndef ARITHMETIC_RIGHT_SHIFT
long4   signTab[5] = {0,0x00000080,0x00008000,0x00800000,0x00000000};
long4 extendTab[5] = {0,~0^0xff,~0^0xffff,~0^0xffffff,~0^0xffffffff};
#endif

/*-->SignExtend*/
/**********************************************************************/
/****************************  SignExtend  ****************************/
/**********************************************************************/
long4
SignExtend(fp, n)   /* return n byte quantity from file fd */
register FILE *fp;  /* file pointer    */
register int    n;  /* number of bytes */
{
    int     n1;     /* number of bytes      */
    long4    x;      /* number being constructed */
#ifdef SIGN_DEBUG
    long4    x0;     /* copy of x  */
#endif
    x = getc(fp);   /* get first (high-order) byte */
    n1 = n--;
    while (n--)  {
        x <<= 8;
        x |= getc(fp);
    }
/*
 *   NOTE: This code assumes that the right-shift is an arithmetic, rather
 *   than logical, shift which will propagate the sign bit right.   According
 *   to Kernighan and Ritchie, this is compiler dependent!
 */

#ifdef SIGN_DEBUG
    x0 = x;
#endif

#ifdef ARITHMETIC_RIGHT_SHIFT
    x <<= 32 - 8 * n1;
    x >>= 32 - 8 * n1; /* sign extend */
#else
    if (x & signTab[n1]) x |= extendTab[n1];
#endif

#ifdef SIGN_DEBUG
    fprintf(stderr,"\tSignExtend(fp,%d)=%lX, was=%lX,%d\n",
	    n1,x,x0,x0&signTab[n1]);
#endif

#ifdef DEBUG
    if (Debug > 1)
        fprintf(stderr,"\tSignExtend(fp,%d)=%lx\n", n1, x);
#endif
    return(x);
}


/*-->SkipFontDef*/
/**********************************************************************/
/****************************  SkipFontDef  ***************************/
/**********************************************************************/
void
SkipFontDef()
{
    int     a, l;
    char    n[STRSIZE];

    (void) NoSignExtend(dvifp, 4);
    (void) NoSignExtend(dvifp, 4);
    (void) NoSignExtend(dvifp, 4);
    a = (int) NoSignExtend(dvifp, 1);
    l = (int) NoSignExtend(dvifp, 1);
    GetBytes(dvifp, n, a + l);
}


/*-->Warning*/
/**********************************************************************/
/*****************************  Warning  ******************************/
/**********************************************************************/
void                           /* issue a warning */
Warning(fmt, a, b, c, d)
char    *fmt;         /* format    */
char    *a, *b, *c, *d;   /* arguments */
{
    if ( G_nowarn || G_quiet )
        return;

    fprintf(stderr, "%s: warning: ", G_progname);
    fprintf(stderr, fmt, a, b, c, d);
    fprintf(stderr, "\n");
}

void
PutWord(w)
int     w;
{
    EMITC((char)(w >> 8) & 0xff);
    EMITC((char)w & 0xff);
}


#ifdef IBM3812
/*-->PMPout*/
/*****************************************************************************/
/* This routine produces the PMP-envelopes for the 3812. Its semantics are:

   first arg == 0  ... flush buffer
   first arg == -1 ... number of bytes specified in the second argument
               have to be continuous, that is they must not
               be disrupted by ENTER PMP etc.
   first arg > 0       output first arg bytes

               If arg2 > OUTBUFSIZE ... flush buffer,
                        switch to unbuffered mode
                        (dont't collect PMP commands)
               If arg2+bufferpointer > OUTBUFSIZE ... flush buffer,
                        block will fit into buffer
               otherwise ..... block will fit into buffer

  Buffering is done to reduce the ENTER PMP-commands. Initially
  the 3812 is in PC-ASCII mode. In order to issue a PMP-command it is
  necessary to enter PMP mode. The ENTER-PMP-command contains the
  number of bytes that will be interpreted as PMP-commands. In the
  most naive approach for each primitive command (eg. set cursor) you
  have to produce a seperate ENTER-PMP-envelope (5 bytes). It is
  favourable to collect as many PMP commands as possible in one envelope. */
/*****************************************************************************/
void
PMPout(l, s)
char    *s;
int     l;
{
    static char     buffer[OUTBUFSIZE];
    static unsigned short   bp = 0;         /* range 0..OUTBUFSIZE */
    static long4     continuous = 0l;
    static bool buffered = _TRUE;

    if (l == 0) {
        if (bp == 0)
            return;
        EMIT(outfp, "\033[C%c%c", (unsigned char)(bp & 0xFF),
            (unsigned char)(bp >> 8));
        EMITB((int)bp, buffer);
        bp = 0;
        return;
    }
    if (l == -1) {
        continuous = (long4)s;
        if (continuous + (long4)bp + 5l > (long4) OUTBUFSIZE)
            PMPflush;
        buffered = (bool) ((continuous + 5l <= (long4) OUTBUFSIZE));
        if (!buffered) {
            EMIT(outfp, "\033[C%c%c",
                (unsigned char)(continuous & 0xFF),
                (unsigned char)((continuous >> 8) & 0xFF));
        }
        return;
    }
    if (buffered) {
        register int    i;
        if ( ((long4)l + bp) > OUTBUFSIZE)
            PMPflush;
        for (i = 0; i < l; i++)
            buffer[bp+i] = s[i];
        bp += (unsigned short)l;
    } else {
        EMITB((int)l, s);
        buffered = (bool) ((continuous -= (long4)l) <= 0) ;
    }
}


void
PMPoutC(c)
char    (c);
{
    PMPout(1, &c);
}


#endif
#ifdef MSDOS
/*-->AssureBinary*/
/**********************************************************************/
/*************************** AssureBinary *****************************/
/**********************************************************************/
/* This procedure is both DOS AND MSC dependent. The MSC file open on */
/* a device ignores the 'binary' of the "wb" parameter and opens the  */
/* file in ascii mode. This procedure sets the file f to binary mode  */
/* if it is connected to a device that is not console input or output */
/* or the null device. For other operating systems this routine is    */
/* useless. (Background: MSDOS 3.2 Technical Reference upd 1 pg 6-137 */
/**********************************************************************/
void
AssureBinary(f)
FILE *f;
{
    union REGS regs;              /* registers for bios call */

    regs.h.ah = (unsigned char) 0x44;     /* IOCTL            */
    regs.h.al = (unsigned char) 0x00;     /* get device information   */
    regs.x.bx = (unsigned int) fileno(f); /* handle from MSC      */
    intdos(&regs, &regs);         /* call DOS interrupt       */
                          /* ---> result in DX    */

    if (  (regs.h.dl & 0x80)     /* file handle points to a device */
         && !(regs.h.dl & 0x07) )    /* neither console i/o or null    */ {

        regs.h.dl  |= 0x20;      /* set BINARY bit in device info  */

        regs.h.ah = (unsigned char) 0x44;    /* IOCTL         */
        regs.h.al = (unsigned char) 0x01;    /* set device information*/
        regs.x.bx = (unsigned int) fileno(f); /* handle from MSC      */
        regs.h.dh = (unsigned char) 0x00;    /* clear DH          */
        intdos(&regs, &regs);           /* call DOS interrupt     */
    }
}


#endif

#ifdef USEPXL
bool getbit ();
unsigned char   getnyb ();
long4    pk_packed_num ();


#define  PKBYTE   *pkloc; pkloc ++
#define  OUTCHAR(c) raster_line_buf[bp]= (unsigned char)c; bp++

unsigned char   bitweight, inputbyte ;
unsigned char   dyn_f ;
unsigned char   *pkloc;
int     repeatcount;

void              /* <Read and translate raster description@>*/
PkRaster(ce, raster)
struct char_entry *ce;
bool raster;
{
    int     rp;
    int     current_line;
    int     wordwidth ;
    bool turnon;
    unsigned short  nbpl;
    long4    rowsleft, word, wordweight, hbit, count, i, j, tl;
    long4    row[101] ;
    unsigned char   raster_line_buf[BYTES_PER_PIXEL_LINE];
    unsigned short  bp;


    if (ce->charsize == HUGE_SIZE)
        Fatal( "cannot process currently PK font patterns of that size!\n");


    current_line = 0;
    pkloc = (unsigned char *)ce->where.address.pixptr;
    dyn_f = (unsigned char)(ce->flag_byte >> 4);
    turnon = (bool)((ce->flag_byte & 8) == 8);
    wordwidth = (int)(ce->width + 31) >> 5 ;
    nbpl = ((int)(ce->width) +  7) >> 3;

    bitweight = 0 ;
    if (dyn_f == 14) {
        /*printf("<Get raster by bits@>\n");*/
        for (i = 1; i <= (long4)ce->height; i++) {
        word = 0 ;
        wordweight = 31 ;
        bp = 0;            /* Sowa */

#ifdef DRAWGLYPH
           printf("     |");
#endif
        for (j = 1; j <= (long4) ce->width; j++) {
            bool getbit;
            /* bp = 0;               SOWA */
/*******************************************begin Getbit *********/
            bitweight /= 2 ;
            if ( bitweight == 0 ) {
                inputbyte = PKBYTE ;
                bitweight = 128 ;
            }
            getbit = (bool)
             ( inputbyte >= bitweight ) ;
            if ( getbit )
                inputbyte -= bitweight ;
/*********************************************end Getbit *********/

            if (getbit)
                word += power[wordweight] ;

            wordweight --;
            if (wordweight == -1) {

#ifdef DRAWGLYPH
   { int k;
     for (k=31; k>=0; k--) {
         if ((power[k] & word)!=0) printf("M");
         else printf(".");
     }
   }
#endif

            OUTCHAR((word >> 24 & 0xFF));
            OUTCHAR((word >> 16 & 0xFF));
            OUTCHAR((word >> 8 & 0xFF));
            OUTCHAR((word    & 0xFF));

            word = 0 ;
            wordweight = 31 ;
            }
        }
        if (wordweight < 31) {
#ifdef COMMENT
#ifdef DRAWGLYPH
   { int k;
     for (k=15; k>=0; k--) {
        if ((power[k] & word)!=0) printf("Z");
        else printf(":");
     }
    }
    printf("|\n ----|");
#endif
#endif

            for (j = 3; j >= (wordwidth * 4 - (long4)nbpl);
            j--) {

                OUTCHAR(((word >> (j << 3)) & 0xff));

#ifdef DRAWGLYPH
   { int k;
     for (k=7; k>=0; k--) {
        if ((power[k] & ((word >> (j << 3)) & 0xff))!=0) printf("M");
        else printf(".");
     }
   }
#endif

            }
        }

        if (raster) {
            RasterLine(ce, (unsigned int)nbpl,
                current_line, raster_line_buf);
            current_line++;
        } else
            EMITL(bp, raster_line_buf);

#ifdef DRAWGLYPH
   printf("|\n");
#endif
        }
    } else {
        /* fprintf(stderr, "@<Create normally packed raster@>\n"); */
        rowsleft = (long4) ce->height ;
        hbit = (long4) ce->width ;
        repeatcount = 0 ;
        wordweight = 32 ;
        word = 0 ;
        rp = 1 ;
        while ( rowsleft > 0 ) {
        count = pk_packed_num() ;
        bp = 0;

        while (count > 0) {
            if ((count < wordweight) && (count < hbit)) {
            if (turnon)
                word +=
                    gpower[wordweight] -
                    gpower[wordweight - count] ;

            hbit -= count ;
            wordweight -= count ;
            count = 0 ;
            } else if ((count >= hbit) && (hbit <=
            wordweight)) {

            if (turnon)
                word +=
                    gpower[wordweight] -
                    gpower[wordweight - hbit] ;

            row[rp] = word ;

            /*fprintf(stderr, " @<Send row@> \n");*/
            for (i = 0; i <= (long4) repeatcount; i++) { int ii;

#ifdef DRAWGLYPH
  printf("***  |");
#endif
                for (ii = 1; ii < wordwidth; ii++) {
                tl = row[ii];

                OUTCHAR((tl >> 24 & 0xFF));
                OUTCHAR((tl >> 16 & 0xFF));
                OUTCHAR((tl >> 8  & 0xFF));
                OUTCHAR((tl       & 0xFF));

#ifdef DRAWGLYPH
   { int k;
     for (k=31; k>=0; k--)  {
         if ((power[k] & row[ii])!=0) printf("M");
         else printf(".");
     }
   }
#endif
                }
                tl = row[wordwidth];
                for (j = 3; j >= (wordwidth *4 - (long4)nbpl);
                 j--) {

                 OUTCHAR(((tl >> (j << 3)) & 0xff));

#ifdef DRAWGLYPH
   { int k;
     for (k=7; k>=0; k--) {
         if ((power[k] & ((tl >> (j << 3)) & 0xff))!=0) printf("M");
         else printf(".");
     }
   }
#endif
                }

                if (raster) {
                    RasterLine(ce,
                    (unsigned int)nbpl,
                     current_line,
                     raster_line_buf);
                    current_line++;
                } else
                    EMITL(bp, raster_line_buf);

                bp = 0;

#ifdef DRAWGLYPH
   printf("|  ");
   for (j=1;j<=(long4)wordwidth;j++) printf("%02lX/",row[j]);
   printf(" raster=%d\n",raster);
#endif
            }

            rowsleft -=  (long4)repeatcount + 1 ;
            repeatcount = 0 ;
            rp = 1 ;
            word = 0 ;
            wordweight = 32 ;
            count -= hbit ;
            hbit = (long4)ce->width ;
            } else {
            if (turnon) word += gpower[wordweight] ;
            row[rp] = word ;
            rp = rp + 1 ;
            word = 0 ;
            count -= wordweight ;
            hbit -= wordweight ;
            wordweight = 32 ;
            }
        }   /* .....while count > 0 */
        if (turnon)
            turnon = _FALSE;
        else
            turnon = _TRUE;
        } /* ...... rowsleft > 0 */
        if ((rowsleft != 0) || (hbit != (long4)ce->width))
            Fatal("Bad pk file----more bits than required!\n");
    } /* .... create normally packed raster */
}


unsigned char   getnyb ()
{
    register unsigned char  temp ;
    if ( bitweight == 0 ) {
        inputbyte = PKBYTE ;
        bitweight = 16 ;
    }
    temp = inputbyte / bitweight ;
    inputbyte -= temp * bitweight ;
    bitweight /= 16 ;
    return ( temp ) ;
}


long4
pk_packed_num ()
{ /*@<Packed number procedure@>= */
    register int    i;
    long4    j;

    i = (int)getnyb();
    if (i == 0) {
        do {
            j = (long4)getnyb();
            i++;
        } while (j == 0) ;
        while (i > 0) {
            j = j * 16 + (long4)getnyb() ;
            i--;
        };
        return (j - 15 + (13 - dyn_f) * 16 + dyn_f) ;
    } else if (i <= (int)dyn_f) {
        return ((long4)i);
    } else if (i < 14) {
        return ((i-(long4)dyn_f - 1) * 16 + (long4)getnyb() + dyn_f + 1);
    } else {
        if (i == 14) {
            repeatcount = (int) pk_packed_num() ;
        } else {
            repeatcount = 1 ;
        }
        /*     fprintf(stderr,"repeatcount = [%d]\n",repeatcount);    */
        return (pk_packed_num()) ;    /* tail end recursion !! */
    }
}
#endif  

#ifndef USEPXL
void bad_gf(n)
    int n;
{
    Fatal("Bad gf file, case %d\n",n);      /* See gf.c */
}
#endif
/*-->FormFeed*/
/**********************************************************************/
/*****************************  FormFeed ******************************/
/**********************************************************************/
void
FormFeed()
{

#ifdef IBM3812
    unsigned short pages;
    if ( (ndone == 0) && (FirstAlternate)){
        for (pages = 1; pages < ncopies; pages++) {
            PMPout(2, "\321\300"); /* PMP-command xD1C0 */
        }
        PMPout(2, "\321\100"); /* PMP-command xD140 */
    } else {
        for (pages = 1; pages < ncopies; pages++){
            PMPout(2, "\321\200"); /* PMP-command xD180 */
        }
        PMPout(2, "\321\0"); /* PMP-command xD100 */
    }
#endif
#ifdef LJ
    EMITC('\f');
#endif
}
