#ifndef XB_CHECK

#ifndef vms
#include <unistd.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>

#include "xb_config.h"
#include "xbanner.h"

/* this will be the option file name for -file <fname> */
char OPTFILE[256];

char PROGNAME[80];
#endif /* #ifndef XB_CHECK */

/* effect names - order must match the enumeration in xbanner.h */
char *effect_keyword[] =
{
  "None", "Shadow", "Outline", "Shadowed-Outline", "3D-Shadow", "Thick",
  "StandOut", "StandIn", "PopArt", "Coin", "Fade", "Backlight", "FatText",
  "StandOut2", "StandIn2", "FunnyOutline", "FgGrad", "FgPlasma", "Shake",
  NULL
};

/* placement names - order must match the enumeration in xbanner.h */
char *placement_keyword[] =
{
  "TopLeft", "TopRight", "BottomLeft", "BottomRight", "TopCenter",
  "BottomCenter", "XY", "Center", "CenteredOnY",
  NULL
};

char *background_keyword[] =
{
  "None", "TopDown", "LeftDiagonal", "RightDiagonal", "LeftRight", "Fill",
  "Plasma", "LeftSplit", "RightSplit", "Fan", "Ripples",
#ifdef HAS_XPM
  "BgPix",
#endif
  NULL
};

char *cycletype_keyword[] =
{
  "None", "FgGrad", "Fade", "BG", "FatText", "Backlight", "FG", "FgPlasma",
  NULL
};

char *cornermask_keyword[] =
{
  "All",         "DownLeft", "DownRight", "UpLeft", "UpRight", "NoDownLeft",
  "NoDownRight", "NoUpLeft", "NoUpRight",
  NULL
};

/* should we dump the resources somewhere? */
Bool DumpRes = DUMPRES_DEFVAL;
char DumpResFile[256] = DUMPRESFILE_DEFVAL;

/* The resources and their classes - order is same as enum Option ! */
static Resource XBres[]= {
/* general options */
  { LABEL,	"label",		"Label" },
  { EFFECT,	"effect",		"Effect" },
  { FONT,	"font",			"Font" },
  { PLACE,	"placement",		"Placement" },

/* debugging and information options */
  { SHOWERR,	"showErrors",		"ShowErrors" },
  { CALC,	"showCalc",		"ShowCalc" },
  { DUMPRES,	"dumpRes",		"DumpRes" },
  { DUMPRESFIL, "dumpResFile",		"DumpResFile" },

/* things specific to background effects */
  { BGSTYLE,	"bgStyle",		"BgStyle" },
  { BGC,	"bgFillColor",		"BgFillColor" },
  { BGGRAD,	"bgGrad",		"BgGrad" },
  { BGGRADREP,	"bgGradRepeat",		"BgGradRepeat" },
  { AUTO_FILL,  "autoFillBg",		"AutoFillBg" },
  { BGFILL,	"bgFill",		"BgFill" },
  { BARSIZE,	"barSize",		"BarSize" },
#ifdef HAS_XPM
  { BGPIXFN,	"bgPixFile",		"BgPixFile" },
#endif

/* things related to the underlining feature */
  { UNDERLINED,	"underlined",		"Underlined" },
  { ULC,	"underlineColor",	"UnderlineColor" },
  { ULTHICKNESS,"underlineThickness",	"UnderlineThickness" },

/* pixmap paste */
#ifdef HAS_XPM
  { DOPIX,	"doPixmap",		"DoPixmap" },
  { PIXFIL,	"pixFile",		"PixFile" },
  { PX,		"pixmapX",		"PixmapX" },
  { PY,		"pixmapY",		"PixmapY" },
#endif

/* glint */
  { GLINT,	"glint",		"Glint" },
  { GLINTSPEED,	"glintSpeed",		"GlintSpeed" },
  { GLINTMIN,	"glintMin",		"GlintMin" },
  { GLINTMAX,	"glintMax",		"GlintMax" },
  { GLINTMAXT,	"glintTimeMax",		"GlintTimeMax" },
  { GLINTMINT,	"glintTimeMin",		"GlintTimeMin" },
  { CORNERMASK, "cornerMask",		"CornerMask" },

/* color cycling options */
  { CYCLE,	"cycleColors",		"CycleColors" },
  { CYCREV,	"reverseCycle",		"ReverseCycle"},
  { CYCFOR,	"forwardCycle",		"ForwardCycle"},
  { CYCSPEED,	"cycleSpeed",		"CycleSpeed" },

/* plasma and ripple effects */
  { PLASMA_NCOL,"plasmaNumColors",	"PlasmaNumColors" },
  { PLASMA_GRAIN,"plasmaGraininess",	"PlasmaGraininess" },
  { FGPL_NCOL,  "fgPlasmaNCol",		"FgPlasmaNCol" },
  { FGPL_GRAIN, "fgPlasmaGrain",	"FgPlasmaGrain" },
  { FGPL_GRAD,	"fgPlasmaGrad",		"FgPlasmaGrad" },
  { RIPPLES,	"ripples",		"Ripples" },
  { RIPPLECOLORS,"rippleColors",	"RippleColors" },

/* effect specific things */
  { SHADOWS,	"shadows",		"Shadows" },
  { FGCYCNUMCOL,"fgCycColors",		"FgCycColors" },
  { BKLTGRAD,	"backlightGrad",	"BacklightGrad" },
  { FADEGRAD,	"fadeGrad",		"FadeGrad" },
  { FATTXTGRAD,	"fatTextGrad",		"FatTextGrad" },
  { FGGRADGRAD, "fgGradGrad",		"FgGradGrad" },
  { FGGBARSIZ,	"fgGradBarSize",	"FgGradBarSize" },
  { FGCYCGRAD,  "fgCycleGrad",		"FgCycleGrad" },

/* things affecting some or most effects */
  { FGC,	"foreground",		"Foreground" },
  { SHDC,	"shadowColor",		"ShadowColor" },
  { HICOLOR,	"hiColor",		"HiColor" },
  { THICKNESS,	"thickness",		"Thickness" },
  { SHDXOFFS,	"shadowXOffset",	"ShadowOffset" },
  { SHDYOFFS,	"shadowYOffset",	"ShadowOffset" },
  { SURMIN,	"surroundMin",		"Surround" },
  { SURMAX,	"surroundMax",		"Surround" },

/* miscellania */
  { X,		"x",			"X" },
  { Y,		"y",			"Y" },
  { XOFFS,	"defXOffset",		"DefOffset" },
  { YOFFS,	"defYOffset",		"DefOffset" },
  { LINGER,	"linger",		"Linger" },

  { TERM,"","" }
};

/* not needed for xb_check */
#ifndef XB_CHECK

/* The command line options */
static XrmOptionDescRec opTable[] = {
/* -v/-h/-H are implemented in a twist of a way and are not checked ... */
  {"-v",	".XBshowver",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-h",	".XBshowver",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-help",	".XBhelp",        XrmoptionNoArg,  (caddr_t) "True"},

/* general options */
  {"-display",	".display",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-label",	".label",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-effect",	".effect",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-font",	".font",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-fn",	".font",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-placement",".placement",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-file",	".file",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-xrm",	NULL,		  XrmoptionResArg, (caddr_t) NULL},

/* things related to the underlining feature */
  {"-underline",".underlined",   XrmoptionNoArg,  (caddr_t) "True"},
  {"-ul",	".underlined",    XrmoptionNoArg,  (caddr_t) "True"},
  {"-noul",	".underlined",    XrmoptionNoArg,  (caddr_t) "False"},
  {"-ulcolor",	".underlineColor",XrmoptionSepArg, (caddr_t) NULL},
  {"-ulthick",  ".underlineThickness",XrmoptionSepArg, (caddr_t) NULL},

/* effect specific things */
  {"-shadows",	".shadows",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-fgcyccols",".fgCycColors",   XrmoptionSepArg, (caddr_t) NULL},
  {"-fgcycgrad",".fgCycleGrad",   XrmoptionSepArg, (caddr_t) NULL},
  {"-bkltgrad",	".backlightGrad", XrmoptionSepArg, (caddr_t) NULL},
  {"-fadegrad",	".fadeGrad",      XrmoptionSepArg, (caddr_t) NULL},
  {"-ftgrad",	".fatTextGrad",   XrmoptionSepArg, (caddr_t) NULL},
  {"-fggrad",	".fgGradGrad",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-fggradbs",	".fgGradBarSize", XrmoptionSepArg, (caddr_t) NULL},

/* things affecting some or most effects */
  {"-fg",	".foreground",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-shadowcol",".shadowColor", XrmoptionSepArg, (caddr_t) NULL},
  {"-sc",	".shadowColor",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-hicolor",	".hiColor",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-hc",	".hiColor",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-thickness",".thickness",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-shdoffset",".ShadowOffset",  XrmoptionSepArg, (caddr_t) NULL},
  {"-surround",	".Surround",      XrmoptionSepArg, (caddr_t) NULL},

/* things specific to background effects */
  {"-bgstyle",	".bgStyle",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-bg",	".bgFillColor",   XrmoptionSepArg, (caddr_t) NULL},
  {"-bggrad",	".bgGrad",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-bggradrep",".bgGradRepeat",  XrmoptionSepArg, (caddr_t) NULL},
  {"-autofill", ".autoFillBg",    XrmoptionNoArg,  (caddr_t) "True"},
  {"-noautofill",".autoFillBg",   XrmoptionNoArg,  (caddr_t) "False"},
  {"-bgfill",	".bgFill",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-nobgfill",	".bgFill",	  XrmoptionNoArg,  (caddr_t) "False"},
  {"-barsize",	".barSize",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-bs",	".barSize",       XrmoptionSepArg, (caddr_t) NULL},
#ifdef HAS_XPM
  {"-bgpixfile",".bgPixFile",	  XrmoptionSepArg, (caddr_t) NULL},
#endif

/* pixmap paste */
#ifdef HAS_XPM
  {"-dopixmap",	".doPixmap",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-nopixmap",	".doPixmap",	  XrmoptionNoArg,  (caddr_t) "False"},
  {"-pixfile",	".pixFile",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-px",	".pixmapX",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-py",       ".pixmapY",       XrmoptionSepArg, (caddr_t) NULL},
#endif

/* plasma and ripple effects */
  {"-plncol",	".plasmaNumColors",XrmoptionSepArg,(caddr_t) NULL},
  {"-plgrain",	".plasmaGraininess",XrmoptionSepArg,(caddr_t)NULL},
  {"-fgplncol", ".fgPlasmaNCol",  XrmoptionSepArg, (caddr_t) NULL},
  {"-fgplgrain",".fgPlasmaGrain", XrmoptionSepArg, (caddr_t) NULL},
  {"-fgplgrad", ".fgPlasmaGrad",  XrmoptionSepArg, (caddr_t) NULL},
  {"-ripples",  ".ripples",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-ripplencol",".rippleColors", XrmoptionSepArg, (caddr_t) NULL},

/* glint */
  {"-glint",	".glint",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-noglint",	".glint",	  XrmoptionNoArg,  (caddr_t) "False"},
  {"-glintspeed",".glintSpeed",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-gmin",	".glintMin",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-gmax",	".glintMax",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-gtimemax", ".glintTimeMax",  XrmoptionSepArg, (caddr_t) NULL},
  {"-gtimemin", ".glintTimeMin",  XrmoptionSepArg, (caddr_t) NULL},
  {"-cornermask",".cornerMask",   XrmoptionSepArg, (caddr_t) NULL},

/* color cycling options */
  {"-cycle",	".cycleColors",	  XrmoptionSepArg, (caddr_t) NULL},
  {"-cycrev",	".reverseCycle",  XrmoptionSepArg, (caddr_t) NULL},
  {"-cycfor",	".forwardCycle",  XrmoptionSepArg, (caddr_t) NULL},
  {"-cycspeed",	".cycleSpeed",	  XrmoptionSepArg, (caddr_t) NULL},

/* debugging and information options */
  {"-showerr",	".showErrors",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-noshowerr",".showErrors",	  XrmoptionNoArg,  (caddr_t) "False"},
  {"-showcalc",	".showCalc",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-hidecalc",	".showCalc",      XrmoptionNoArg,  (caddr_t) "False"},
  {"-dumpres",  ".dumpRes",       XrmoptionNoArg,  (caddr_t) "True"},
  {"-nodumpres",".dumpRes",       XrmoptionNoArg,  (caddr_t) "False"},
  {"-dumpresfile",".dumpResFile", XrmoptionSepArg, (caddr_t) NULL},

/* miscellania */
  {"-x",	".x",		  XrmoptionSepArg, (caddr_t) NULL},
  {"-y",	".y",		  XrmoptionSepArg, (caddr_t) NULL},
  {"-linger",	".linger",	  XrmoptionNoArg,  (caddr_t) "True"},
  {"-nolinger",	".linger",	  XrmoptionNoArg,  (caddr_t) "False"},

/* awkward but it's ok - a terminator entry */
  {"-XBtermin",	".XBtermin",	  XrmoptionNoArg,  (caddr_t) "XBtermin"}
};

typedef struct {
  char *opt;
  char *desc;
  Bool cont;
} OptDesc;

static OptDesc optDesc[] = {
/* general options */
  {"-display",	"String    Display to open for drawing",False},
  {"-label",	"String    The text to render the effect on",False},
  {"-effect",	"Keyword   The type of effect to render the text with. Can be:",True},
  {" ",		"           None, Shadow, Outline, Shadowed-Outline, 3D-Shadow,",True},
  {" ",		"           Thick, StandOut, StandIn, PopArt, Coin, Fade,",True},
  {" ",		"           Backlight, FatText, StandOut2, StandIn2, FunnyOutline,",True},
  {" ",		"           FgGrad, FgPlasma or Shake",False},
  {"-font",	"String    Font string for label text",False},
  {"-fn",	"String    Same as -font",False},
  {"-placement","Keyword   Where to place the text on the screen. Can be:",True},
  {" ",		"           TopLeft, TopRight, TopCenter, BottomLeft, BottomRight,",True},
  {" ",		"           BottomCenter, Center, XY, CenteredOnY",False},
  {"-file",	"String    Read a specified resource file",False},
  {"-xrm",	"String    Any X resoruce string",False},

/* debugging and information options */
  {"-showerr",	"          Show errors and warnings by drawing a text line on",True},
  {" ",		"           the root window",False},
  {"-noshowerr","          Suppress showing errors on the root window",False},
  {"-showcalc",	"          Print position / size information to stderr",False},
  {"-hidecalc",	"          Suppress printing position / size info",False},
  {"-dumpres",  "          Dump the X11 resources as they are seen by XBanner",False},
  {"-nodumpres","          Suppress dumping the resources to a file",False},
  {"-dumpresfile","String    File to dump resources into",False},

/* things specific to background effects */
  {"-bgstyle",	"Keyword   Style to use in coloring the background. Can be:",True},
  {" ",		"           TopDown, LeftRight, LeftDiagonal, RightDiagonal,",True},
  {" ",		"           LeftSplit, RightSplit, Fan, Fill, BgPix or None",False},
  {"-bg",	"ColorSpec Background color for -bgfill or -bgstyle Fill",False},
  {"-bggrad",	"GradSpec  Color gradient for the BG styles",False},
  {"-bggradrep","Numeric   Repeat period of the BgGrad. This tells XBanner to",True},
  {" ",		"           allocate less colors, but to use them several times.",True},
  {" ",		"           Example: If you selected -bggrad red,yellow,red and also",True},
  {" ",		"           use -bggradrep 2, then a -bgstyle TopDown would seem to",True},
  {" ",		"           go from red to yellow to red to yellow back to red",False},
  {"-autofill", "          When using a -bgstyle TopDown, LeftRight and the two",True},
  {" ",		"           diagonal fills, this will color the background using",True},
  {" ",		"           the color that is half-way along the gradient. This",True},
  {" ",		"           does a nice trick on the eyes and makes the square",True},
  {" ",		"           uncovered when XDM removes his login box appear as if",True},
  {" ",		"           it has a reversed color-gradient",False},
  {"-noautofill","          Suppress filling BG to the median color",False},
  {"-bgfill",	"          Color-fill the BG before other BG effects. This is",True},
  {" ",		"           an option additional to the -bgstyle Fill option,",True},
  {" ",		"           and is also here to help make the uncovered area",True},
  {" ",		"           under XDM's login box look better",False},
  {"-nobgfill",	"          No color-fill BG before other BG effects",False},
  {"-barsize",	"Numeric   Size of bars in background effects. This is the",True},
  {" ",         "           number of pixels per color (more or less) of the",True},
  {" ",         "           gradient. Generally, smaller number means that the",True},
  {" ",         "           gradient looks smoother and uses more colors",False},
  {"-bs",	"Numeric   Short for -barsize",False},
#ifdef HAS_XPM
  {"-bgpixfile","String    Name of .XPM file for the -bgstyle BgPix option",False},
#endif
  {"-ripples",  "Numeric   Number of ripples for the Ripples background style",False},
  {"-ripplencol","Numeric   Number of colors to use for the Ripples BG style",False},

/* things related to the underlining feature */
  {"-underline","          Place an underline under the text",False},
  {"-ul",	"          Short for -underline",False},
  {"-noul",	"          Suppress underlining the text",False},
  {"-ulcolor",	"ColorSpec Color for the underline. If you use -ulcolor FGC",True},
  {" ",         "           then some of the effects for the main text get",True},
  {" ",         "           rendered on the underline as well. This is the",True},
  {" ",         "           case for the following effcts: Shadow, Thick,",True},
  {" ",         "            Shadowed-Outline, StandOut/In/Out2/In2, PopArt,",True},
  {" ",         "            Coin, Fade, Backlight, FatText, FunnyOutline,",True},
  {" ",		"            FgGrad, FgPlasma, Shake",False},
  {"-ulthick",  "Numeric   Thickness of the underline",False},

/* pixmap paste */
#ifdef HAS_XPM
  {"-dopixmap",	"          Read and paste a pixmap on the screen (.XPM)",False},
  {"-nopixmap",	"          Suppress pasting a pixmap on the screen",False},
  {"-pixfile",	"String    Pathame of the .XPM file to paste",False},
  {"-px",	"Numeric   X position of the pixmap",False},
  {"-py",       "Numeric   Y position of the pixmap",False},
#endif

/* glint */
  {"-glint",	"          Make the text glint. This will work only on:",True},
  {" ",		"           StandOut/In/Out2/In2, PopArt, Coin, Fade, Backlight,",True},
  {" ",         "           FatText, FunnyOutline, FgGrad, FgPlasma and Shake",False},
  {"-noglint",	"          Suppress glinting altogether",False},
  {"-glintspeed","Numeric   Speed of glinting stars (smaller=slower)",False},
  {"-gmin",	"Numeric   Smallest size of glinting stars (XBanner chooses",True},
  {" ",         "           the size as a random between this number and the",True},
  {" ",         "           largest size allowed)",False},
  {"-gmax",	"Numeric   Largest size of glint stars",False},
  {"-gtimemax", "Numeric   Shortest time between glitters (again, XBanner will",True},
  {" ",         "           wait a random time between glints)",False},
  {"-gtimemin", "Numeric   Longest time between glitters",False},
  {"-cornermask","Keyword   Comma separated list of keywords telling XBanner",True},
  {" ",         "           which corners should be glinted on. Can be any of:",True},
  {" ",         "            All, DownLeft, DownRight, UpLeft, UpRight,",True},
  {" ",         "            NoDownLeft, NoDownRight, NoUpLeft, NoUpRight",False},

/* color cycling options */
  {"-cycle",	"Keyword   Comma separated list of keywords telling which",True},
  {" ",         "           color gradients should be cycled. Can be:",True},
  {" ",         "            FgGrad, Fade, BG, FatText, Backlight, FG,",True}, 
  {" ",         "            FgPlasma or None to suppress all cycling",False},
  {"-cycrev",	"Keyword   List of keywords (same as for -cycle) telling which",True}, 
  {" ",         "           color gradients you want cycled in reverse dir",False},
  {"-cycfor",	"Keyword   List of keywords (same as for -cycle) telling which",True}, 
  {" ",         "           color gradients you want cycled in forward dir",False},
  {"-cycspeed",	"Numeric   Color cycling speed. Smaller numbers mean slower",True}, 
  {" ",         "           cycling. Negative number allowed and are really",True}, 
  {" ",         "           very slow. A certain speed around 10-20 is usually",True}, 
  {" ",         "           the fastest. To make things seem faster, use less",True}, 
  {" ",         "           colors or -bggradrep for BG styles",False},

/* plasma & ripple effects */
  {"-plncol",	"Numeric   Number of colors for the Plasma BG style",False},
  {"-plgrain",	"Float     Graininess factor for the Plasma BG style",False},
  {"-fgplncol", "Numeric   Number of colors to use in the FgPlasma effect",False},
  {"-fgplgrain","Float     Graininess factor for the FgPlasma effect",False},
  {"-fgplgrad", "GradSpec  Color gradient for the FgPlasma effect",False},
  {"-ripples",  "Numeric   Number of ripples for the Ripples background",False},
  {"-ripplencol","Numeric   Number of colors to use for the Ripples background",False},

/* effect specific things */
  {"-shadows",	"Numeric   For 3D-Shadow effect, number of shadows",False},
  {"-fgcyccols","Numeric   Number of colors to cycle the FG through. This ",True}, 
  {" ",         "           makes the whole area painted in the FG color",True}, 
  {" ",         "           change its color along a color gradient. The less",True}, 
  {" ",         "           colors, the more rapid the change would seem",False},
  {"-bkltgrad",	"GradSpec  Color gradient for the Backlight effect",False},
  {"-fadegrad",	"GradSpec  Color gradient for the Fade effect",False},
  {"-ftgrad",	"GradSpec  Color gradient for the FatText effect",False},
  {"-fggrad",	"GradSpec  Color gradient for the FgGrad effect",False},
  {"-fggradbs",	"Numeric   Bar-size for the FgGrad effect. Again, this is the",True}, 
  {" ",         "           requested number of pixels per color in the grad",False},
  {"-fgcycgrad","GradSpec  Color gradient for cycling the FG color",False},

/* things affecting some or most effects */
  {"-fg",	"ColorSpec Foreground color of the text (can be #rrggbb)",False},
  {"-shadowcol","ColorSpec Shadow color for the shadowed effects: Shadow, ",True}, 
  {" ",         "           Shadowed-Outline, FatText, FgGrad and FgPlasma",False},
  {"-sc",	"ColorSpec Short for -shadowcol",False},
  {"-hicolor",	"ColorSpec Highlight color needed for the following effects:",True}, 
  {" ",         "           StandOut/In/Out2/In2, and Coin",False},
  {"-hc",	"ColorSpec Short for -hicolor",False},
  {"-thickness","Numeric   Generally this is the thickness of the effcet on the",True}, 
  {" ",         "           text. This is relevant for the following effects:",True}, 
  {" ",         "            Thick, StandOut/In/Out2/In2, PopArt, Coin, Fade,",True}, 
  {" ",         "            Backlight, FatText, FunnyOutline",False},
  {"-shdoffset","Numeric   Number of pixels to the right and down from the",True}, 
  {" ",         "           location of the text where the shadow will be",False},
  {"-surround",	"Numeric   Thickness of outline in Outline effects except for",True}, 
  {" ",         "           the FunnyOutline which uses -thickness!",False},

/* miscellania */
  {"-x",	"Numeric   X pos in XY placement type",False},
  {"-y",	"Numeric   Y pos in XY and CenteredOnY placement type",False},
  {"-linger",	"          Leave a process with an open display after XBanner",True}, 
  {" ",         "           finishes. This happens automaticly if you use",True}, 
  {" ",         "           -glint or -cycle, and then you MUST run the",True}, 
  {" ",         "           freetemp program to make XBanner's child process",True}, 
  {" ",         "           terminate. See the docs about this",False},
  {"-nolinger",	"          Do not leave a process with an open display. This",True}, 
  {" ",         "           option WILL NOT suppress leaving a process for the",True}, 
  {" ",         "           -glint and -cycle options",False},

  {NULL,NULL}
};

static XrmDatabase appResDB=NULL;

char dispname[256];

/* this checks if it's a valid grad line, finds an available grad struct,
   and fills some information in it.
*/
static int ParseGrad(char *line)
{
  int i;

  if(is_grad_line(line))	/* is it a valid grad line??? */
  {
    for(i=0;i<NUM_GRADS;i++)
      if(grad[i].Used==False)
        break;
    if(i==NUM_GRADS)
      error(disp,mgc,"No avail. color gradient",True);
    else
    {
      grad[i].Used = True;        /* mark it as used */
      strcpy(grad[i].color_names,line);
      return i;
    }
  }
  else /* !is_grad_line() */
  {
    sprintf(errline,"Value '%s' is not a color grad",line);
    error(disp,mgc,errline,True);
  }
  return -1;
} /* ParseGrad */

static char *GetHomeDir(char *s)
{
#ifndef vms
  if(getenv("HOME")==NULL)
    *s=0;
  else
    strcpy(s,getenv("HOME"));
#else
  strcpy(s,"SYS$LOGIN:");
#endif
  return s;
}

static void Usage(void)
{
  fprintf(stderr,"XBanner %s - Written by Amit Margalit (%s)\n",VERSION,DATE);
  fprintf(stderr,"Usage: xbanner [option [option...]]\n\
  Common command line options are:\n\
    -display <disp>, -fn <font>, -file <resourcefile>, -xrm <resource>,\n\
    -placement <placement>, -effect <effect_type>, -label <text>,\n\
    -fg <color>, -sc <color>, -hc <color>, -[no]ul, -ulcolor <color>,\n\
  Type 'xbanner -help 'for more details, or you can read the\n\
  documentation for a complete description for all options\n");
  exit(1);
}

static void Help_Message(void)
{
  int i,j;

  fprintf(stderr,"XBanner %s - Written by Amit Margalit (%s)\n",VERSION,DATE);
  fprintf(stderr,"Usage: xbanner [option [option...]]\n");
  fprintf(stderr,"* ColorSpec can be color-name or #rrggbb\n");
  fprintf(stderr,"* GradSpec is a comma/colon separated list of ColorSpec's\n");
  fprintf(stderr,"* Command line options recognized are:\n\n");
  fprintf(stderr,"Option        Type      Description\n");
  fprintf(stderr,"------------- --------- ---------------------------------\n");
  for(i=0;strcmp(opTable[i].option,"-XBtermin");i++)
  {
    if(opTable[i].specifier!=NULL)
      if(strncmp(opTable[i].specifier,".XB",3)==0)
        continue;
    fprintf(stderr,"%-13s ",opTable[i].option);
    for(j=0;optDesc[j].opt!=NULL;j++)
      if(strcmp(optDesc[j].opt,opTable[i].option)==0)
      {
        fprintf(stderr,"%s\n",optDesc[j].desc);
        if(optDesc[j++].cont)
        {
          do
          {
            fprintf(stderr,"%-13s %s\n",optDesc[j].opt,optDesc[j].desc);
          } while(optDesc[j++].cont);
        }
        break;
      }
  }

  fprintf(stderr,"See the doc for more info...\n");
  exit(1);
}

/* Read the cornermask line, and evaluate it */
static void MakeCornerMask(char *s)
{
  int  i,j;
  CornerType k;
  char op[40];	/* 40 is enough */
  
  op[0]='\0';

  for(i=0,j=0;s[i];i++,j++)
  {
    if(is_sep(s[i]))
    {
      /* parse it */
      op[j]='\0';
      if(op[0]!='\0')
      {
        for(k=0;cornermask_keyword[k]!=NULL;k++)
          if(strcmpi(cornermask_keyword[k],op)==0)
            break;
        if(cornermask_keyword[k]==NULL)
        {
          fprintf(stderr,"%s: Unknown corner mask keyword '%s'\n",PRGCLASS,op);
          continue;
        }
	/* we have a corner keyword */
        switch(k)	/* do them corners... eh? */
        {
          case C_ALL:
            CornerMask |= UpLeft|UpRight|DownLeft|DownRight;
            break;
          case C_UPLEFT:
            CornerMask = UpLeft;
            break;
          case C_UPRIGHT:
            CornerMask = UpRight;
            break;
          case C_DOWNLEFT:
            CornerMask = DownLeft;
            break;
          case C_DOWNRIGHT:
            CornerMask = DownRight;
            break;
          case C_NODL:
            CornerMask &= ~DownLeft;
            break;
          case C_NODR:
            CornerMask &= ~DownRight;
            break;
          case C_NOUL:
            CornerMask &= ~UpLeft;
            break;
          case C_NOUR:
            CornerMask &= ~UpRight;
            break;
        } /* switch(k) */
      } /* if op[0] != 0 */
      /* and restart */
      op[0]='\0';
      j=0;
    }
    else /* not sep */
      op[j]=s[i];
  } /* for i,j */
}

/* generic func to check a boolean value and store it */
static _inline void DoBoolean(char *line, Bool *b)
{
  if( strcmpi(line,"True")==0 || 
      strcmpi(line,"Yes")==0  ||
      strcmpi(line,"On")==0   ||
      strcmp (line,"1")==0)
    *b = True;
  else
    *b = False;
}

/* take a string make it into int and check boundaries */
static _inline int atoi_bounds(char *line,Bool min,int minval,Bool max,int maxval,char *err)
{
  int i;
  
  i = atoi(line);
  if( ( (min==True) && (i < minval) ) ||
      ( (max==True) && (i < maxval) ) )
  {
    fprintf(stderr,"%s: %s (%s)\n",PRGCLASS,err,line);
    exit(1);
  }
  return i;
}

typedef enum { FlagCyc, FlagDirection } What2Flag;

/* This func will flag a cycle component to True or set its direction */
static void FlagCycle(CycleType ct, What2Flag w2f, void *value)
{
  switch(ct)
  {
    /* CYCNONE is only handled for flagging true/false */
    case CYCNONE:	/* this one disables all of them */
      FgGradCyc = FadeGradCyc = BgGradCyc = FatGradCyc = BkltGradCyc = 
      	FgcGradCyc = FgplGradCyc = False;
      break;
    case CYCFGGRAD:
      switch(w2f)
      {
	case FlagCyc:
          FgGradCyc = *((Bool*)value);
	  break;
	case FlagDirection:
	  FgGradCycDir = *((CycleType*)value);
	  break;
      }
      break;
    case CYCFADE:
      switch(w2f)
      {
	case FlagCyc:
          FadeGradCyc = *((Bool*)value);
	  break;
	case FlagDirection:
	  FadeCycDir = *((CycleType*)value);
	  break;
      }
      break;
    case CYCBGFADE:
      switch(w2f)
      {
	case FlagCyc:
          BgGradCyc = *((Bool*)value);
	  break;
	case FlagDirection:
	  BgCycDir = *((CycleType*)value);
	  break;
      }
      break;
    case CYCFAT:
      switch(w2f)
      {
	case FlagCyc:
          FatGradCyc = *((Bool*)value);
	  break;
	case FlagDirection:
	  FatCycDir = *((CycleType*)value);
	  break;
      }
      break;
    case CYCBKLT:
      switch(w2f)
      {
	case FlagCyc:
          BkltGradCyc = *((Bool*)value);
	  break;
	case FlagDirection:
	  BkltCycDir = *((CycleType*)value);
	  break;
      }
      break;
    case CYCFGC:
      switch(w2f)
      {
	case FlagCyc:
          FgcGradCyc = *((Bool*)value);
	  break;
	case FlagDirection:	/* nothing to do here, if you want other */
	  break;		/* direction change the grad definition  */
      }
      break;
    case CYCFGPL:
      switch(w2f)
      {
	case FlagCyc:
          FgplGradCyc = *((Bool*)value);
	  break;
	case FlagDirection:
	  FgplCycDir = *((CycleType*)value);
	  break;
      }
      break;
  }
} /* FlagCycle */

static void strip_trailing(char *s)
{
  char *p;
  
  p=s+strlen(s)-1;
  while(isspace(*p))
    p--;
  *(p+1)='\0';
}

/* Main resource parsing routine */
static void ParseResources(void)
{
  int k,j,i;		/* iter */
  char line[256];
  char resname[80],classname[80];
  char res_type[20];
  XrmValue value;
  char namelist[MAX_CYCS][MAX_CYCNAME];
  CycleType XB_ColorCycle=CYCNONE;

  /* to pass as parameters */
  Bool tr = True;
  CycleDirection cf = CYC_FORWARD, cr = CYC_REVERSE;

  for (j=0;XBres[j].opt!=TERM;j++)
  {
    /* make the resource name and class */
    strcpy(classname,PRGCLASS); strcat(classname,".");
    strcat(classname,XBres[j].option_class);

    strcpy(resname,PROGNAME); strcat(resname,".");
    strcat(resname,XBres[j].option_keyword);

    /* see if we have anything for this resource */
    if(XrmGetResource(appResDB,resname,classname,(char**)res_type,&value)==True)
    {
      /* guess we do, so take the value into line */
      strncpy(line,value.addr,(int)value.size);
      line[(int)value.size]='\0';
      strip_trailing(line);
      /* now let's do something with it */
      switch(XBres[j].opt)
      {
	case BGPIXFN:
	  strcpy(BGPIXFNAME,line);
	  break;
        case FGPL_NCOL:
          FgPlasma_ncol = atoi_bounds(line,True,8,False,0,"PlasmaCloud needs at least 8 colors.");
          break;
	case PLASMA_NCOL:
	  num_plasma_colors = atoi_bounds(line,True,8,False,0,"PlasmaCloud needs at least 8 colors.");
	  break;
	case RIPPLECOLORS:
	  num_ripple_colors = atoi_bounds(line,True,4,False,0,"Ripples needs at least 4 colors.");
	  break;
	case RIPPLES:
	  num_ripples = atoi_bounds(line,True,1,False,0,"You must specify a positive number of ripples.");
	case FGPL_GRAIN:
	  FgPlasmaGrain = atof(line);
	  if(Grain<=0.0)
	  {
	    fprintf(stderr,"%s: PlasmaCloud Graininess factor must be positive.\n",PRGCLASS);
	    exit(1);
	  }
	  break;
	case PLASMA_GRAIN:
	  Grain = atof(line);
	  if(Grain<=0.0)
	  {
	    fprintf(stderr,"%s: PlasmaCloud Graininess factor must be positive.\n",PRGCLASS);
	    exit(1);
	  }
	  break;
	case BGGRADREP:
	  BgGradRepeat = atoi_bounds(line,True,1,False,0,"BgGrad's Repeat count must be positive!");
	  break;
	case CYCSPEED:
	  CycleSpeed = atoi_bounds(line,True,1,False,0,"CycleSpeed must be positive!");
	  break;
	case SHOWERR:
	  DoBoolean(line,&show_errors);
	  break;
	case AUTO_FILL:
	  DoBoolean(line,&auto_fill);
	  break;
	case BGFILL:
	  DoBoolean(line,&do_fill);
	  break;
	case FGCYCNUMCOL:
	  FgCycNumCols = atoi_bounds(line,True,2,False,0,"# of FG Cycle colors must be >=2!");
	  break;
	case FGGBARSIZ:
	  FgGradBarSize = atoi_bounds(line,True,1,False,0,"FG Bar Size must be positive (>=1)!");
	  break;
	case FGCYCGRAD:
	  FgCycGrad = ParseGrad(line);
	  break;
	case FGGRADGRAD:
	  FgGradGrad = ParseGrad(line);
	  break;
        case BKLTGRAD:
          BkltGrad = ParseGrad(line);
          break;
        case FADEGRAD:
          FadeGrad = ParseGrad(line);
          break;
        case FATTXTGRAD:
          FatTextGrad = ParseGrad(line);
          break;
	case FGPL_GRAD:
	  FgPlasmaGrad = ParseGrad(line);
	  break;
        case BGGRAD:
          BgGrad = ParseGrad(line);
          break;
        case CORNERMASK:
	  strcat(line,",");	/* so we have a sep at the end */
	  MakeCornerMask(line);
          break;
        case GLINTMAXT:
	  GlintMaxTime = atoi(line);
	  break;
        case GLINTMINT:
          GlintMinTime = atoi(line);
          break;
        case GLINTSPEED:
          glint_speed = atoi(line);
	  break;
        case GLINTMAX:
	  GlintSizeMax = atoi(line);
          break;
        case GLINTMIN:
          GlintSizeMin = atoi(line);
          break;
	case DUMPRES:
	  DoBoolean(line,&DumpRes);
          break;
	case CYCLE:
	  i=split_names(line,namelist);
	  for(k=0;k<i;k++)
	  {
	    XB_ColorCycle = get_keyword(namelist[k],cycletype_keyword,"Unknown cycle type");
	    if(XB_ColorCycle == -1)
	      continue;
	    FlagCycle(XB_ColorCycle,FlagCyc,&tr);
	  }
	  break;
        case CYCREV:
	  i=split_names(line,namelist);
	  for(k=0;k<i;k++)
	  {
	    XB_ColorCycle = get_keyword(namelist[k],cycletype_keyword,"Reverse cycle unknown cycle type");
	    if(XB_ColorCycle == -1)
	      continue;
	    FlagCycle(XB_ColorCycle,FlagDirection,&cr);
	  }
          break;
        case CYCFOR:
	  i=split_names(line,namelist);
	  for(k=0;k<i;k++)
	  {
	    XB_ColorCycle = get_keyword(namelist[k],cycletype_keyword,"Forward cycle unknown cycle type");
	    if(XB_ColorCycle == -1)
	      continue;
	    FlagCycle(XB_ColorCycle,FlagDirection,&cf);
	  }
          break;
	case GLINT:
	  DoBoolean(line,&XB_Glint);
          break;
	case LINGER:
	  DoBoolean(line,&XB_Linger);
          break;
#ifdef HAS_XPM
        case PX:
          px = atoi_bounds(line,True,0,True,sc_w,"Pixmap X position out of screen!");
          break;
        case PY:
          py=atoi_bounds(line,True,0,True,sc_h,"Pixmap Y position out of screen!");
          break;
        case PIXFIL:
          strcpy(PIXFILE,line);
          break;
        case DOPIX:
	  DoBoolean(line,&dopix);
          break;
#endif
        case CALC:
	  DoBoolean(line,&show_sizes);
          break;
        case THICKNESS:
          Thickness = atoi_bounds(line,True,0,False,0,"Thickness < 0 is not allowed.");
          break;
        case X:
          final_x = atoi_bounds(line,True,0,True,sc_w,"Text X position out of screen!");
          break;
        case Y:
          final_y = atoi_bounds(line,True,0,True,sc_h,"Text Y position out of screen!");
          break;
        case PLACE:
	  placement = get_keyword(line,placement_keyword,"Unknown placement name");
          break;
        case EFFECT:
	  effect = get_keyword(line,effect_keyword,"Unknown effect name requested");
          break;
        case FONT:
          strcpy(FONTN,line);
          break;
	case DUMPRESFIL:
	  strcpy(DumpResFile,line);
	  break;
	case BGC:
	  strcpy(BGCOLOR,line);
	  break;
        case FGC:
          strcpy(FGCOLOR,line);
          break;
        case HICOLOR:
          strcpy(LGHTCOLOR,line);
          break;
        case SHDC:
          strcpy(SHDCOLOR,line);
          break;
        case LABEL:
          strcpy(BANNER,line);
          break;
        case XOFFS:
          XDEFOFFSET = atoi_bounds(line,True,0,False,0,"DefOffset must be positive.");
          break;
        case YOFFS:
          YDEFOFFSET = atoi_bounds(line,True,0,False,0,"DefOffset must be positive.");
          break;
        case SHADOWS:
          SHD3D_SHADOWS = atoi_bounds(line,True,1,False,0,"Can't do zero or less shadows.");
          break;
        case SURMIN:
          SUR_MIN = atoi(line);
          break;
        case SURMAX:
          SUR_MAX = atoi(line);
          break;
        case SHDXOFFS:
          SHD_X_OFFSET = atoi(line);
          break;
        case SHDYOFFS:
          SHD_Y_OFFSET = atoi(line);
          break;
        case UNDERLINED:
	  DoBoolean(line,&Underlined);
          break;
        case ULC:
          strcpy(ULCOLOR,line);
          break;
        case ULTHICKNESS:
          ULthickness=atoi_bounds(line,True,1,False,0,"Underline thickness must be positive.");
          break;
	case BGSTYLE:
	  bgstyle = get_keyword(line,background_keyword,
	  			"Unknown background type");
	  break;
	case BARSIZE:
	  bar_size = atoi_bounds(line,True,1,False,0,"BarSize must be positive.");
	  break;
        case TERM:
          /* this is here only to avoid gcc's warning */
          break;
      } /* switch */
    } /* if(Xrm...==True) */
  } /* for i */
  
  /* handle XBanner.Surround: <val> properly */
  if(SUR_MIN==SUR_MAX)
  {
    if(SUR_MAX<0)
      SUR_MAX= -SUR_MAX;
    SUR_MIN= -SUR_MAX;
  }

  /* parse environment variables within the label */
  parse_env(BANNER);	/* parse environment variables */

  /* miscellaneous checks... */
  if(XB_ColorCycle != CYCNONE || XB_Glint == True)
  /* we need to linger around for glints... */
    XB_Linger=True;

  /* warn about glint and color-cycling */
  if(XB_ColorCycle != CYCNONE && XB_Glint == True)
    error(disp,mgc,"Glint & Color Cycling do not go well together",False);

  /* we don't like the following options... */
  if(GlintSizeMax > GLINTSIZE_MAXVAL)
    GlintSizeMax = GLINTSIZE_MAXVAL;
  if(GlintSizeMin < GLINTSIZE_MINVAL)
    GlintSizeMin = GLINTSIZE_MINVAL;
  if(GlintSizeMax < GLINTSIZE_MINVAL)
    GlintSizeMax = GLINTSIZE_MINVAL;
  if(GlintSizeMin > GLINTSIZE_MAXVAL)
    GlintSizeMin = GLINTSIZE_MAXVAL;
  if(GlintSizeMax < GlintSizeMin)
  {
    error(disp,mgc,"Warning - GlintMax <= GlintMin, corrected",False);
    GlintSizeMin = GlintSizeMax;
  }
  GlintSizeMax++;
  
  /* check timing information */
  if(GlintMaxTime < GLINTMINTIME_MINVAL)
    GlintMaxTime = GLINTMINTIME_MINVAL + 1;
  if(GlintMinTime < GLINTMINTIME_MINVAL)
    GlintMinTime = GLINTMINTIME_MINVAL;
  if(GlintMaxTime <= GlintMinTime)
  {
    error(disp,mgc,"GlintMaxTime <= GlintMintime, corrected",False);
    GlintMinTime = GlintMaxTime - 1;
  }

  /* let's see if we have an empty label. If we do - no effect then */
  if(BANNER[0]=='\0')
  {
    BANNER[0]=' ';		/* make it a single space	*/
    BANNER[1]='\0';
    effect = None;		/* no effect - faster		*/
    strcpy(FONTN,"fixed");	/* don't load a scalable font	*/
  }

} /* ParseResources */

/* read the databases and then parse */
void DoResources(int *argc, char *argv[])
{
  XrmDatabase cmdlDB,appdefDB,rmDB,xenvDB,fileDB;
  XrmValue value;
  int len,i;
#ifdef vms
  int j;
#endif

  char *envi;
  char str_type[256];
  char line[1024];

  /* these must be nulls! */
  cmdlDB=appdefDB=rmDB=xenvDB=fileDB=NULL;

  /* Initialize Resource Manager Routines */
  XrmInitialize();

  /* parse our own name for instance name... */
#ifdef vms
  /* make the OpenVMS executable name */
  for(i=0,j=0;argv[0][i];i++)
    if(argv[0][i]==']')
      j=i;
  strcpy(line,argv[0]+j+1);
  for(i=0;line[i];i++)
    if(line[i]=='.')
    {
      line[i]='\0';
      break;
    }
  strcpy(argv[0],line);
#endif
  strcpy(line,argv[0]);
  len= -1;		/* note that this means it hasn't found any */
  for(i=0;line[i];i++)
    if(line[i]=='/')
      len=i;
  strcpy(PROGNAME,line+(len== -1?0:len+1));
  XrmParseCommand(&cmdlDB,opTable,sizeof(opTable)/sizeof(opTable[0]),PROGNAME,argc,argv);

  if(*argc>1)
  {
    fprintf(stderr,"%s: Following commandline options were not understood:\n",PRGCLASS);
    for(i=0;i<(*argc)-1;i++)
      fprintf(stderr,"%s%c ",argv[i+1],(i<(*argc)-2?',':' '));
    fprintf(stderr,"\n");
    Usage();
  }

  /* check for -v / -h */
  strcpy(line,PROGNAME); strcat(line,".XBshowver");
  if (XrmGetResource(cmdlDB,line,"XBanner.XBshowver",
  	(char**)str_type,&value)==True)
    Usage();

  /* check for -H or -help in OpenVMS */
  strcpy(line,PROGNAME); strcat(line,".XBhelp");
  if (XrmGetResource(cmdlDB,line,"XBanner.XBhelp",
  	(char**)str_type,&value)==True)
    Help_Message();

  /* get the display so we can get the XResourceManagerString() */
  strcpy(line,PROGNAME); strcat(line,".display");
  if (XrmGetResource(cmdlDB,line,"XBanner.Display",
  	(char**)str_type,&value)==True)
    {
      strncpy(dispname,value.addr,(int)value.size);
      dispname[(int)value.size]='\0';
    }

  /* get -file first */
  strcpy(line,PROGNAME); strcat(line,".file");
  if (XrmGetResource(cmdlDB,line,"XBanner.File",
  	(char**)str_type,&value)==True)
    {
      strncpy(OPTFILE,value.addr,(int)value.size);
      OPTFILE[(int)value.size]='\0';
      fileDB = XrmGetFileDatabase(OPTFILE);
    }

  /* actually open the display so we can read the resource manager string */
  if(dispname[0]=='\0')
    disp=XOpenDisplay(NULL);
  else
    disp=XOpenDisplay(dispname);
  if (disp == NULL)
  {
    fprintf(stderr,"%s: Could not open display...\n",PRGCLASS);
      exit(1);
  }
              
  /*
    order of reading the databases...
    cmdline		<-- done already
    app-defaults
    ResourceManagerString
    XENVIRONMENT
    XAPPLRESDIR
  */
  
  /* get the app-defaults */

#ifndef vms
  strcpy(line,"/usr/lib/X11/app-defaults/");
  strcat(line,PRGCLASS);
  appdefDB = XrmGetFileDatabase(line);
  if(appdefDB==NULL && getenv("XAPPLRESDIR")!=NULL)
  {
    strcpy(line,getenv("XAPPLRESDIR"));
    strcat(line,"/");
    strcat(line,PRGCLASS);
    appdefDB = XrmGetFileDatabase(line);
  }
#else
  strcpy(line,"DECW$SYSTEM_DEFAULTS:");
  strcat(line,PRGCLASS);
  strcat(line,".DAT");
  appdefDB = XrmGetFileDatabase(line);
#endif

  if(XResourceManagerString(disp) != NULL)
    rmDB = XrmGetStringDatabase(XResourceManagerString(disp));
  else
  {
    GetHomeDir(line);
#ifndef vms
    strcat(line,"/.Xdefaults");
#else
    strcat(line,"XBANNER.DAT");
#endif
    rmDB = XrmGetFileDatabase(line);
  }

#ifndef vms
  if((envi=getenv("XENVIRONMENT"))==NULL)
  {
    envi=GetHomeDir(line);
    strcat(line,"/.Xdefaults-");
    len=strlen(line);
    gethostname(envi+len,1024-len);
  }
#else
  strcpy(line,"DECW$XDEFAULTS.DAT");
#endif
  xenvDB = XrmGetFileDatabase(line);

  /* here I need to do things the correct order */
#ifdef R4_RESOURCES
  if(appdefDB!=NULL)
    XrmMergeDatabases(appdefDB,&appResDB);
  if(rmDB!=NULL)
    XrmMergeDatabases(rmDB,&appResDB);
  if(xenvDB!=NULL)
    XrmMergeDatabases(xenvDB,&appResDB);
  if(fileDB!=NULL)
    XrmMergeDatabases(fileDB,&appResDB);
  if(cmdlDB!=NULL)
    XrmMergeDatabases(cmdlDB,&appResDB);
#else
  if(cmdlDB!=NULL)
    XrmCombineDatabase(cmdlDB,&appResDB,False);
  if(fileDB!=NULL)
    XrmCombineDatabase(fileDB,&appResDB,False);
  if(xenvDB!=NULL)
    XrmCombineDatabase(xenvDB,&appResDB,False);
  if(rmDB!=NULL)
    XrmCombineDatabase(rmDB,&appResDB,False);
  if(appdefDB!=NULL)
    XrmCombineDatabase(appdefDB,&appResDB,False);
#endif

  /* now all we have left to do is read the resources and set the variables */
  ParseResources();

  /* dump resources to a file??? */
  if(DumpRes)
    XrmPutFileDatabase(appResDB,DumpResFile);
}

#endif /* #ifndef XB_CHECK */
