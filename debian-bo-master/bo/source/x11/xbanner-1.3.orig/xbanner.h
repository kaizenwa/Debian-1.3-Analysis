/* xbanner.h - header file for xbanner */

#define VERSION		"1.3"
#define DATE		"October 1st 1996"

/* default values in case no config file is present */

#define BANNER_DEFVAL		"L i n u x !"
#define XOFFSET_DEFVAL		25
#define YOFFSET_DEFVAL		25
#define FGCOL_DEFVAL		"black"
#define SHDCOL_DEFVAL		"darkblue"
#define LGHTCOL_DEFVAL		"cyan"
#define BGCOL_DEFVAL		"#00BFFF"	/* == DeepSkyBlue */
#define FONT_DEFVAL		"-bitstream-charter-bold-i-normal--150-*-100-100-*-*-iso8859-1"
#define SURMIN_DEFVAL		-3
#define SURMAX_DEFVAL		3
#define PLACEMENT_DEFVAL	TOPCENT
#define EFFECT_DEFVAL		SHADOUTL
#define SHDXOFFS_DEFVAL		12
#define SHDYOFFS_DEFVAL		12
#define SHD3DSHADOWS_DEFVAL	2
#define THICKNESS_DEFVAL	7
#define ULC_DEFVAL		"white"
#define UNDERLINED_DEFVAL	False
#define ULTHICK_DEFVAL		4
#define BGSTYLE_DEFVAL		NOBG
#define BARSIZE_DEFVAL          16
#define BARSIZE_MINVAL		2
#define SHOWSIZE_DEFVAL		False
#define PX_DEFVAL		0
#define PY_DEFVAL		0
#define DOPIX_DEFVAL		False
#define PIXNAME_DEFVAL		""
#define BGPIXFN_DEFVAL		""
#define CLOSENESS_DEFVAL	35000
#define XB_LINGER_DEFVAL	False
#define XB_GLINT_DEFVAL		False
#define COLORCYCLE_DEFVAL	CYCNONE
#define GLINTSIZE_MINVAL	56
#define GLINTSIZE_MAXVAL	120
#define GLINTSPEED_DEFVAL	-20
#define BLINK_SPEED_TIME	50000	/* usec */
#define GLINTMINTIME_DEFVAL	1000
#define GLINTMAXTIME_DEFVAL	3500
#define GLINTMINTIME_MINVAL	500
#define CYCFG_DEFVAL		False
#define DUMPRES_DEFVAL		False
#define DUMPRESFILE_DEFVAL	"XBanner.res"
#define FGCYC_NUMCOL_DEFVAL	16
#define FGGRAD_BARSIZE_DEFVAL	7
#define DO_FILL_DEFVAL		False
#define CYCLESPEED_DEFVAL	1
#define PLASMACOL_DEFVAL	76
#define PLASMA_GRAIN_DEFVAL	0.76
#define BGGRAD_REPEAT_DEFVAL	1
#define FGPL_NCOL_DEFVAL	32
#define FGPL_GRAIN_DEFVAL	4.76
#define AUTO_FILL_DEFVAL	True
#define SHOW_ERR_DEFVAL		False
#define RIPPLECOL_DEFVAL	32
#define RIPPLES_DEFVAL		2

/* misc defines */
/* Banner text length */
#define MAX_BANNER_LEN		256
/* ColorName Len */
#define CNLEN			40
/* Max. num of colors per color-gradient */
#define MAX_COLORS_PER_GRAD	8
/* Number of color grad structs */
#define NUM_GRADS		7
/* names lists */
#define MAX_CYCS	10
#define MAX_CYCNAME	40

/* Our special types - Option, etc. */
typedef enum {	X, Y, PLACE, EFFECT, FONT, FGC, SHDC, LABEL, XOFFS,
		YOFFS, SHADOWS, SURMIN, SURMAX, HICOLOR, SHDXOFFS,
		SHDYOFFS, THICKNESS, UNDERLINED, ULC, ULTHICKNESS,
		BGSTYLE, BARSIZE, CALC, PIXFIL, DOPIX, PX, PY, LINGER,
		GLINT, CYCLE, GLINTMIN, GLINTMAX, GLINTSPEED, GLINTMAXT,
		GLINTMINT, CORNERMASK, BGGRAD, BKLTGRAD, FADEGRAD,
		FATTXTGRAD, FGGRADGRAD, FGCYCGRAD, DUMPRES, DUMPRESFIL,
		FGCYCNUMCOL, FGGBARSIZ, BGC, BGFILL, CYCSPEED, PLASMA_NCOL,
		PLASMA_GRAIN, BGGRADREP, FGPL_NCOL, FGPL_GRAIN, FGPL_GRAD,
		AUTO_FILL, BGPIXFN, SHOWERR, CYCREV, CYCFOR, RIPPLES,
		RIPPLECOLORS,
		TERM } Option;

typedef enum {	NONE, SHADOW, OUTLINE, SHADOUTL, SHD3D, THICK,
		STANDOUT, STANDIN, POPART, COIN, FADE, BACKLIGHT,
		FATTEXT, STDOUT2, STDIN2, FUNNYOUTL, FGGRAD,
		FGPLASMA, SHAKE } Effect;

typedef enum {	TOPLEFT, TOPRIGHT, BOTLEFT, BOTRIGHT, 
		TOPCENT, BOTCENT, XY, CENTER, CTRONY } Placement;

typedef enum {  NOBG,   TOPDOWN,   DIAGL,      DIAGR, LEFTRIGHT, FILL,
		PLASMA, LEFTSPLIT, RIGHTSPLIT, FANBG, RIPPLESBG
#ifdef HAS_XPM
		,BGPIX
#endif
		} BGStyle;

typedef enum {  INWARDS, OUTWARDS } Zmethod;	/* which way to fill? */

		/* cycle what? */
typedef enum {  CYCNONE, CYCFGGRAD, CYCFADE, CYCBGFADE, CYCFAT, CYCBKLT,
                CYCFGC,  CYCFGPL } CycleType;

typedef enum {  CYC_FORWARD, CYC_REVERSE } CycleDirection;

#define UpLeft	  8
#define UpRight   4
#define DownLeft  2
#define DownRight 1

typedef enum {  C_ALL, C_DOWNLEFT, C_DOWNRIGHT, C_UPLEFT, C_UPRIGHT,
		C_NODL, C_NODR, C_NOUL, C_NOUR } CornerType;

typedef enum { OK=1, NOMEM, XIMAGE } Stat;

/* One resource */
typedef struct {
  Option	 opt;
  char		*option_keyword;
  char		*option_class;
} Resource;

/* definition of a Corner structure */
typedef struct {
  unsigned short x,y;
  unsigned char ctype; /* should have been CornerType but mem considerations */
} Corner;

/* define XColors */
typedef XColor * XColors;

/* Hold one gradient */
typedef struct {
  char     color_names[(CNLEN+1)*MAX_COLORS_PER_GRAD];
  XColor   xcolor[MAX_COLORS_PER_GRAD];
  int      xcolors;	/* number of colors used in xcolor[] */
  XColors  grad;	/* pointer to array of XColor's containing entire grad */
  int      gradcolors;	/* number of elements in grad[] */
  Bool     Used;	/* is this one in use? */
} GradSpec;

/* Information required to do a PlasmaCloud */
typedef struct {
  Drawable d;
  GC	   gc;
  int	   width;
  int	   height;
  int	   colors;
  GradSpec *grad;
  double   Grain;
} Plasma_Info;

/* Information required to do a Ripples */
typedef struct {
  Drawable d;
  GC	   gc;
  int	   width;
  int	   height;
  int	   colors;
  int	   ripples;
  GradSpec *grad;
} Ripple_Info;

/* information required for color-cycling more than 1 thing */
typedef struct {
  int  grad_num;	  /* number of grad to cycle */
  Bool only_first;	  /* for CYCFGC		     */
  CycleDirection dir;	  /* direction to cycle it   */
  int  redc,greenc,bluec; /* r,g,b component cycle factor - Future stuff */
  int  dir_steps,dir_count;/* for direction changes - Future stuff */
} Cycle_Info;

#define MAX_CYCLES 10
extern Cycle_Info cyc_inf[];
extern int num_used_cyc;

/* function prototypes */

/* From effect.c */
void DrawIt(void);	  /* draw the text with the effect                 */

/* From linger.c */
void DoLinger(void);				/* linger around ...	   */

/* From colors.c */
void AllocColors(void);				/* do the color allocation */
Bool AllocGrad(int grad_num,int steps,Bool RW);	/* allocate a gradient	   */
void DoColorCycling(Display *disp);		/* color cycling routine   */

/* From xres.c */
void DoResources(int *argc, char *argv[]);	/* the resources           */

/* From backg.c */
void DoBackground(void);			/* fading background       */

/* From plasma.c */
Stat DoPlasma(Display *disp,Plasma_Info *pi);	/* do plasma on drawable   */

/* From ripples.c */
Stat DoRipples(Display *disp,Ripple_Info *pi);	/* do ripples on drawable   */

/* From pix.c */
void DoPastePixmap(void);			/* paste a pixmap	   */
void DoTilePixmap(Display *disp, Window w);	/* tile the root window    */

/* From util.c */
int  split_names(char *line,char names[MAX_CYCS][MAX_CYCNAME]);/* split line */
void error(Display *disp,GC mgc,char *s,Bool Fatal);	/* error reporting  */
void display_errors(Display *disp,GC mgc);	/* display the errors	    */
Bool is_grad_line(char *s);			/* is it a grad line?       */
Bool XBPropExists(Display *disp,Window root,Atom xprop); /* check if given property exists on a window */
Bool emptyline(char *s);			/* check for empty line     */
int  get_keyword(char *line,char *keywords[],char *err); /* for xres	    */
Bool parse_env(char *line);		/* parse environment variables	    */


#ifndef HAS_USLEEP
void usleep(const unsigned long usec);
#endif
#ifndef HAS_STRCMPI
int strcasecmp(char *s1,char *s2);
int strncasecmp(char *s1,char *s2,int n);
#endif

/* definitions for the X Properties for Linger() */
#define XPROPERTY 	 "XBanner_v1.3_Linger_State"
#define LINGER_DELAY	 50000L		/* uSEC */

/* easier for me to remember them like this */
#define strcmpi(ARG1,ARG2) strcasecmp(ARG1,ARG2)
#define strncmpi(ARG1,ARG2,ARG3) strncasecmp(ARG1,ARG2,ARG3)

/* inline functions when compiled under gcc */
#if defined(__GNUC__) && !defined(__STRICT_ANSI__)
#define _inline inline
#else
#define _inline
#endif
/* pixel definition */
#if !defined(PIXEL_ALREADY_TYPEDEFED)
typedef unsigned long Pixel;
#define PIXEL_ALREADY_TYPEDEFED
#endif

/* allowed separators : Comma Space Bar Colon */
#if !defined(IS_SEP_DEFD)
#define is_sep(C) ((C==',')||(C==' ')||(C=='|')||(C==':'))
#define IS_SEP_DEFD
#endif

/* extern defines of global variables */
extern char BANNER[];		/* the label text */

extern int  XDEFOFFSET;
extern int  YDEFOFFSET;
extern int  SHD_X_OFFSET;
extern int  SHD_Y_OFFSET;
extern char FGCOLOR[];
extern char BGCOLOR[];
extern char SHDCOLOR[];
extern char LGHTCOLOR[];
extern char ULCOLOR[];

extern GradSpec grad[];

extern char FONTN[];

extern int SUR_MIN;
extern int SUR_MAX;

extern int Thickness;
extern int ULthickness;
extern int bar_size,FgGradBarSize;
extern int BgGrad,BkltGrad,FadeGrad,FatTextGrad,FgGradGrad;
extern int FgCycGrad,FgPlasmaGrad;
extern int BgGradRepeat;

extern CycleType BgCycDir,BkltCycDir,FatCycDir,FadeCycDir,FgGradCycDir,FgplCycDir;

extern int SHD3D_SHADOWS;

extern Display        *disp;
extern GC              mgc;
extern Colormap        cmap;
extern XColor          xfgc,xshdc,xlghtc,xexact,xffc,xftc,xulc;
extern XColor	       xexact,xbgc;
extern Pixmap          rootpix;
extern Window	       root;

extern Pixel color_cell;	/* fgc in a R/W color cell */

extern int sc_w,sc_h;                  /* screen width/height                */
extern int dir,fasc,fdsc,lbear;        /* font ascent/descent/dir/lbearing   */
extern int bwid;                       /* banner pixel width                 */
extern int final_x,final_y;            /* where the text will finally appear */
extern Placement placement;
extern Effect effect;
extern BGStyle bgstyle;

/* relevant area x,y,height,width */
extern int rel_x,rel_y,rel_h,rel_w;        

/* position of the pixmap */
extern int px,py;

/* things for lingering effects */
extern int GlintSizeMin,GlintSizeMax,glint_speed;
extern long GlintMaxTime,GlintMinTime;

/* corners stuff */
extern int NumCorners,CornerMask;

/* Cycle Colors information */
extern GradSpec grad[];
extern XColor *CycleColors;
extern int NumCycleColors,FgCycNumCols,CycleSpeed;

/* Plasma effect */
extern int num_plasma_colors,FgPlasma_ncol,num_ripple_colors,num_ripples;
extern double Grain,FgPlasmaGrain;

extern char OPTFILE[];
extern char PIXFILE[];
extern char BGPIXFNAME[];
extern char dispname[];
extern char errline[];

extern Corner *CornerList;
extern char *effect_keyword[];
extern char *placement_keyword[];

extern Bool Underlined;
extern Bool show_sizes;
extern Bool dopix;
extern Bool XB_Linger;
extern Bool XB_Glint;
extern Bool do_fill,auto_fill;
extern Bool show_errors;

/* color cycling Booleans */
extern Bool FgGradCyc,FadeGradCyc,BgGradCyc,FatGradCyc,BkltGradCyc,FgcGradCyc,FgplGradCyc;
