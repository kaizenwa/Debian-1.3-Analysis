
/*  @(#)calctool.c 1.12 89/12/07
 *
 *  A simple calculator program.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Basic algorithms, copyright (c) Ed Falk.
 *                Sun Microsystems, Mountain View.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <strings.h>
#include <math.h>
#include "patchlevel.h"
#include "color.h"
#include "calctool.h"

double powers[11][4] = {
         {    1.0,          1.0,           1.0,             1.0 },
         {    2.0,          8.0,          10.0,            16.0 },
         {    4.0,         64.0,         100.0,           256.0 },
         {    8.0,        512.0,        1000.0,          4096.0 },
         {   16.0,       4096.0,       10000.0,         65536.0 },
         {   32.0,      32768.0,      100000.0,       1048576.0 },
         {   64.0,     262144.0,     1000000.0,      16777216.0 },
         {  128.0,    2097152.0,    10000000.0,     268435456.0 },
         {  256.0,   16777216.0,   100000000.0,    4294967296.0 },
         {  512.0,  134217728.0,  1000000000.0,   68719476736.0 },
         { 1024.0, 1073741824.0, 10000000000.0, 1099511627776.0 }
} ;

double exp_p1[10][4] = {
        { 1.0,          1.0,             1.0,    1.0             },
        { 1.0,          1.0,             1.0,    1.0             },
        { 0.5,          0.125,           0.1,    0.0625          },
        { 0.25,         0.015625,        0.01,   3.90625e-03     },
        { 0.125,        1.953125e-03,    0.001,  2.44140625e-04  },
        { 0.0625,       2.44140625e-04,  1.0e-4, 1.525878906e-05 },
        { 0.03125,      3.051757813e-05, 1.0e-5, 9.536743164e-07 },
        { 0.015625,     3.814697266e-06, 1.0e-6, 5.960464478e-08 },
        { 0.0078125,    4.768371582e-07, 1.0e-7, 3.725290298e-09 },
        { 3.90625e-03,  5.960464478e-08, 1.0e-8, 2.328306437e-10 }
} ;

double max_fix[4] = {
          6.871947674e+10, 3.245185537e+32,
          1.000000000e+36, 2.230074520e+43
} ;

/* Selectable constants - these are the default selections. */

double con_vals[MAXREGS] = {
         0.621,                   /* kilometres per hour <=> miles per hour. */
         1.41421356237309504880,  /* square root of 2. */
         2.7182818284590452354,   /* e */
         3.14159265358979323846,  /* pi */
         2.54,                    /* centimetres <=> inch. */
         57.29577951308232,       /* degrees in a radian. */
         1048576.0,               /* 2 ^ 20. */
         0.0353,                  /* grams <=> ounce. */
         0.948,                   /* kilojoules <=> British thermal units. */
         0.0610,                  /* cubic cms <=> cubic inches. */
} ;

char con_names[MAXREGS][MAXLINE] = {
       "kilometres per hour <=> miles per hour.",
       "square root of 2.",
       "e.",
       "pi.",
       "centimetres <=> inch.",
       "degrees in a radian.",
       "2 ^ 20.",
       "grams <=> ounce.",
       "kilojoules <=> British thermal units.",
       "cubic cms <=> cubic inches.",
} ;

char num_names[MAXREGS][2] = {    /* Used by the popup menus. */
       "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
} ;

char base_str[4][4]  = { "BIN", "OCT", "DEC", "HEX" } ;
char dtype_str[2][4] = { "FIX", "SCI" } ;
char ttype_str[3][5] = { "DEG", "GRAD", "RAD" } ;

char digits[] = "0123456789abcdef" ;
int basevals[4] = {2, 8, 10, 16} ;

/* Length of display in characters for each base. */
int disp_length[4] = {32, 15, 12, 12} ;

/* X offset in pixels for various length button strings. */
int chxoff[5] = { 5, 9, 14, 16, 5 } ;

/* Valid keys when an error condition has occured. */
/*                            MEM  KEYS HELP   clr   QUIT OFF  REDRAW */
char validkeys[MAXVKEYS]  = { 'M', 'K', '?', '\177', 'q', 'o', '\f' } ;

/* Valid keys for which the right mouse button will get a popup menu. */
char validmenu[MAXMENUS]  = {
/*   ACC  CON    EXCH        FUN        <    >   RCL  STO  */
     'A', '#', CCTRL('e'), CCTRL('f'), '<', '>', 'r', 's'
} ;

double disp_val ;             /* Value of the current display. */
double last_input ;           /* Previous number input by user. */
double mem_vals[MAXREGS] ;    /* Memory register values. */
double result ;               /* Current calculator total value. */
double tresults[3] ;          /* Current trigonometric results. */

enum base_type base ;         /* Current base: BIN, OCT, DEC or HEX. */
enum gr_type gtype ;          /* What graphics system is being used. */
enum num_type dtype ;         /* Number display mode. */
enum trig_type ttype ;        /* Trigonometric type (deg, grad or rad). */

FILE *hfd ;         /* File descriptor for help information. */

int accuracy ;      /* Number of digits precision (Max 9). */
int color ;         /* Color being used for current raster operation. */
int column ;        /* Column number of current key/mouse press. */
int cur_ch ;        /* Current character if keyboard event. */
int curx ;          /* Current mouse X position. */
int cury ;          /* Current mouse Y position. */
int down ;          /* Indicates is a mouse button is down. */
int error ;         /* Indicates some kind of display error. */
int hyperbolic ;    /* If set, trig functions will be hyperbolic. */
int iconic ;        /* Set if window is currently iconic. */
int inv_video ;     /* Set if user wants inverse video mode. */
int inverse ;       /* If set, trig and log functions will be inversed. */
int iscolor ;       /* Set if this is a color screen. */
int ishelp ;        /* Set if there is a help file. */
int issel ;         /* Set if valid [Get] selection. */
int ix ;            /* Initial X position of the icon. */
int iy ;            /* Initial Y position of the icon. */
int key_exp ;       /* Set if entering exponent number. */
int new_input ;     /* New number input since last op. */
int nextc ;         /* Current event identifier. */
int nfont_width ;   /* Width of the normal font characters. */
int pending ;       /* Indicates command depending on multiple presses. */
int pending_op ;    /* Arithmetic operation for pending command. */
int pointed ;       /* Whether a decimal point has been given. */
int portion ;       /* Portion of button on current key/mouse press. */
int posspec ;       /* Set if -Wp or -g option is present (for X11) */
int row ;           /* Row number of current key/mouse press. */
int rstate ;        /* Indicates if memory register frame is displayed. */
int spaces ;        /* Number of spaces in current button string. */
int toclear ;       /* Indicates if display should be cleared. */
int tstate ;        /* Indicates current button set being displayed. */
int wx ;            /* Initial X position of the window. */
int wy ;            /* Initial Y position of the window. */
int x ;             /* X offset for text for button. */
int y ;             /* Y offset for text for button. */

/* Routines obeyed by mouse button or character presses. */
int close_frame(),    destroy_frame(),  do_base() ;
int do_calculation(), do_clear(),       do_constant(),  do_delete() ;
int do_expno(),       do_immediate(),   do_keys(),      do_number() ;
int do_pending(),     do_point(),       do_portion(),   do_repaint() ;
int do_set_mode(),    do_trig(),        do_trigtype(),  toggle_reg_canvas() ;

char cur_op ;                     /* Current arithmetic operation. */
char current ;                    /* Current button or character pressed. */
char *exp_posn ;                  /* Position of the exponent sign. */
char fnum[MAX_DIGITS+1] ;         /* Scratch area for fixed point numbers. */
char fun_names[MAXREGS][MAXLINE] ;  /* Function defs from .calctoolrc. */
char fun_vals[MAXREGS][MAXLINE] ;   /* Function names from .calctoolrc. */
char geometry[MAXLINE] ;          /* X11 geometry information. */
char old_cal_value ;              /* Previous calculation operator. */
char pstr[5] ;                    /* Current button text string. */
char *selection ;                 /* Current [Get] selection. */
char *shelf ;                     /* PUT selection shelf contents. */
char snum[MAX_DIGITS+1] ;         /* Scratch are for scientific numbers. */
char x11_display[MAXLINE] ;       /* X11 display information. */

struct iteminfo items[MAXITEMS] = {                    /* Panel items. */
  { SFONT, BBORDER,                 DISPLAY-3,  "" },  /* BASEITEM. */
  { NFONT, 0,                       DISPLAY-15, "" },  /* DISPLAYITEM. */
  { SFONT, BBORDER+1*(BWIDTH+BGAP), DISPLAY-3,  "" },  /* TTYPEITEM. */
  { SFONT, BBORDER+2*(BWIDTH+BGAP), DISPLAY-3,  "" },  /* NUMITEM. */
  { SFONT, BBORDER+3*(BWIDTH+BGAP), DISPLAY-3,  "" },  /* HYPITEM. */
  { SFONT, BBORDER+4*(BWIDTH+BGAP), DISPLAY-3,  "" },  /* INVITEM. */
  { SFONT, BBORDER+5*(BWIDTH+BGAP), DISPLAY-3,  "" },  /* OPITEM. */
} ;

/* Calculator button values. */

struct button buttons[TITEMS] = {
  { "EXCH", CCTRL('e'), OP_SET,   MAUVE,    do_pending },       /* Row 1. */
  { "CON ", '#',        OP_SET,   BLUE,     do_pending },
  { "BIN ", 'B',        OP_CLEAR, YELLOW,   do_base },
  { "MEM ", 'M',        OP_CLEAR, BLUE,     toggle_reg_canvas },
  { "OCT ", 'O',        OP_CLEAR, YELLOW,   do_base },
  { "D   ", 'd',        OP_NOP,   PINK,     do_number },
  { "DEC ", 'D',        OP_CLEAR, YELLOW,   do_base },
  { "E   ", 'e',        OP_NOP,   PINK,     do_number }, 
  { "HEX ", 'H',        OP_CLEAR, YELLOW,   do_base },
  { "F   ", 'f',        OP_NOP,   PINK,     do_number },  
  { "SCI ", CCTRL('n'), OP_CLEAR, BLUE,     do_set_mode },
  { "FUN ", CCTRL('f'), OP_SET,   BLUE,     do_pending },

  { "&32 ", '[',        OP_CLEAR, LGREY,    do_immediate },      /* Row 2. */
  { "STO ", 's',        OP_SET,   MAUVE,    do_pending },
  { "&16 ", ']',        OP_CLEAR, LGREY,    do_immediate },
  { "RCL ", 'r',        OP_SET,   MAUVE,    do_pending }, 
  { "<   ", '<',        OP_SET,   LGREY,    do_pending },
  { "A   ", 'a',        OP_NOP,   PINK,     do_number }, 
  { ">   ", '>',        OP_SET,   LGREY,    do_pending },
  { "B   ", 'b',        OP_NOP,   PINK,     do_number },
  { "%   ", '%',        OP_SET,   LPURPLE,  do_calculation },
  { "C   ", 'c',        OP_NOP,   PINK,     do_number },
  { "clr ", '\177',     OP_CLEAR, BLUE,     do_clear },
  { "bsp ", CCTRL('h'), OP_NOP,   BLUE,     do_delete },

  { "OR  ", '|',        OP_SET,   GREEN,    do_calculation },   /* Row 3. */
  { "AND ", '&',        OP_SET,   GREEN,    do_calculation },
  { "HYP ", 'h',        OP_CLEAR, BLUE,     do_set_mode },
  { "SIN ", CCTRL('s'), OP_CLEAR, LGREEN,   do_trig },
  { "e^x ", '{',        OP_CLEAR, ORANGE,   do_immediate },
  { "7   ", '7',        OP_NOP,   LBLUE,    do_number },
  { "10^x", '}',        OP_CLEAR, ORANGE,   do_immediate },
  { "8   ", '8',        OP_NOP,   LBLUE,    do_number },
  { "y^x ", 'Y',        OP_SET,   ORANGE,   do_calculation },
  { "9   ", '9',        OP_NOP,   LBLUE,    do_number }, 
  { "INT ", 'I',        OP_CLEAR, LGREY,    do_portion },
  { "X   ", 'x',        OP_SET,   LPURPLE,  do_calculation },

  { "XNOR", 'n',        OP_SET,   GREEN,    do_calculation },   /* Row 4. */
  { "XOR ", '^',        OP_SET,   GREEN,    do_calculation },
  { "INV ", 'i',        OP_CLEAR, BLUE,     do_set_mode },
  { "COS ", CCTRL('c'), OP_CLEAR, LGREEN,   do_trig },
  { "ln  ", 'N',        OP_CLEAR, ORANGE,   do_immediate },
  { "4   ", '4',        OP_NOP,   LBLUE,    do_number },
  { "log ", 'G',        OP_CLEAR, ORANGE,   do_immediate },
  { "5   ", '5',        OP_NOP,   LBLUE,    do_number },
  { "SQRT", 'S',        OP_CLEAR, ORANGE,   do_immediate },
  { "6   ", '6',        OP_NOP,   LBLUE,    do_number },
  { "FRAC", 'F',        OP_CLEAR, LGREY,    do_portion },
  { "/   ", '/',        OP_SET,   LPURPLE,  do_calculation },

  { "NOT ", '~',        OP_CLEAR, GREEN,    do_immediate },     /* Row 5. */
  { "ACC ", 'A',        OP_SET,   BLUE,     do_pending },
  { "EXP ", 'E',        OP_SET,   BLUE,     do_expno },
  { "TAN ", CCTRL('t'), OP_CLEAR, LGREEN,   do_trig },
  { "1/x ", 'R',        OP_CLEAR, ORANGE,   do_immediate },
  { "1   ", '1',        OP_NOP,   LBLUE,    do_number },
  { "x!  ", '!',        OP_CLEAR, ORANGE,   do_immediate },
  { "2   ", '2',        OP_NOP,   LBLUE,    do_number },
  { "x^2 ", '@',        OP_CLEAR, ORANGE,   do_immediate },
  { "3   ", '3',        OP_NOP,   LBLUE,    do_number },
  { "CHS ", 'C',        OP_CLEAR, LGREY,    do_immediate },
  { "-   ", '-',        OP_SET,   LPURPLE,  do_calculation },

  { "QUIT", 'q',        OP_CLEAR, BLUE,     destroy_frame },    /* Row 6. */
  { "OFF ", 'o',        OP_CLEAR, BLUE,     close_frame },
  { "KEYS", 'K',        OP_CLEAR, BLUE,     do_keys },
  { "?   ", '?',        OP_SET,   BLUE,     do_pending },
  { "DEG ", CCTRL('d'), OP_CLEAR, RED,      do_trigtype },
  { "0   ", '0',        OP_NOP,   LBLUE,    do_number },
  { "RAD ", CCTRL('r'), OP_CLEAR, RED,      do_trigtype },
  { ".   ", '.',        OP_NOP,   LPURPLE,  do_point },
  { "GRAD", CCTRL('g'), OP_CLEAR, RED,      do_trigtype },
  { "=   ", '=',        OP_CLEAR, LPURPLE,  do_calculation },
  { "ABS ", 'U',        OP_CLEAR, LGREY,    do_portion },
  { "+   ", '+',        OP_SET,   LPURPLE,  do_calculation },

/* Extra definitions. */

  { "X   ", 'X',        OP_SET,   WHITE,    do_calculation },
  { "X   ", '*',        OP_SET,   WHITE,    do_calculation },
  { "    ", CCTRL('m'), OP_CLEAR, WHITE,    do_calculation },
  { "QUIT", 'Q',        OP_CLEAR, WHITE,    destroy_frame },
  { "    ", '\f',       OP_NOP,   WHITE,    do_repaint },
} ;

/*  256-byte table for quickly reversing the bits in an unsigned 8-bit char,
 *  used to convert between MSBFirst and LSBFirst image formats.
 */

char revtable[256] = {
        0, -128,   64,  -64,   32,  -96,   96,  -32,
       16, -112,   80,  -48,   48,  -80,  112,  -16,
        8, -120,   72,  -56,   40,  -88,  104,  -24,
       24, -104,   88,  -40,   56,  -72,  120,   -8,
        4, -124,   68,  -60,   36,  -92,  100,  -28,
       20, -108,   84,  -44,   52,  -76,  116,  -12,
       12, -116,   76,  -52,   44,  -84,  108,  -20,
       28, -100,   92,  -36,   60,  -68,  124,   -4,
        2, -126,   66,  -62,   34,  -94,   98,  -30,
       18, -110,   82,  -46,   50,  -78,  114,  -14,
       10, -118,   74,  -54,   42,  -86,  106,  -22,
       26, -102,   90,  -38,   58,  -70,  122,   -6,
        6, -122,   70,  -58,   38,  -90,  102,  -26,
       22, -106,   86,  -42,   54,  -74,  118,  -10,
       14, -114,   78,  -50,   46,  -82,  110,  -18,
       30,  -98,   94,  -34,   62,  -66,  126,   -2,
        1, -127,   65,  -63,   33,  -95,   97,  -31,
       17, -111,   81,  -47,   49,  -79,  113,  -15,
        9, -119,   73,  -55,   41,  -87,  105,  -23,
       25, -103,   89,  -39,   57,  -71,  121,   -7,
        5, -123,   69,  -59,   37,  -91,  101,  -27,
       21, -107,   85,  -43,   53,  -75,  117,  -11,
       13, -115,   77,  -51,   45,  -83,  109,  -19,
       29,  -99,   93,  -35,   61,  -67,  125,   -3,
        3, -125,   67,  -61,   35,  -93,   99,  -29,
       19, -109,   83,  -45,   51,  -77,  115,  -13,
       11, -117,   75,  -53,   43,  -85,  107,  -21,
       27, -101,   91,  -37,   59,  -69,  123,   -5,
        7, -121,   71,  -57,   39,  -89,  103,  -25,
       23, -105,   87,  -41,   55,  -73,  119,   -9,
       15, -113,   79,  -49,   47,  -81,  111,  -17,
       31,  -97,   95,  -33,   63,  -65,  127,   -1,
} ;

char display[MAXLINE] ;     /* Current calculator display. */
char helpname[MAXLINE] ;    /* Filename for help file. */
char progname[MAXLINE] ;    /* Name of this program. */


main(argc,argv)
int argc ;
char *argv[] ;
{
  STRCPY(progname, argv[0]) ; /* Save this programs name. */
  get_options(argc, argv) ;   /* Get command line arguments. */
  get_helpfile(helpname) ;    /* Open helpfile if present. */
  read_rcfiles() ;            /* Read .calctoolrc's files. */
  if (init_ws_type())         /* Determine window system type. */
    {
      FPRINTF(stderr,"Error initialising window system.\n") ;
      exit(1) ;
    }
  init_fonts() ;              /* Open required fonts. */
  make_icon() ;               /* Set up the calctool window icon. */
  make_frames(argc, argv) ;   /* Create calctool window frames. */
  make_subframes() ;          /* Create panels and canvases. */
  make_menus() ;              /* Create menus for graphics versions. */
  if (gtype != X11) load_colors() ;  /* Load the initial calctool colormap. */

  shelf = NULL ;              /* No selection for shelf initially. */
  pending = 0 ;               /* No initial pending command. */
  rstate = 0 ;                /* No memory register frame display initially. */
  tstate = 0 ;                /* Button values displayed first. */
  hyperbolic = 0 ;            /* Normal trig functions initially. */
  inverse = 0 ;               /* No inverse functions initially. */
  base = DEC ;                /* Initial base. */
  ttype = DEG ;               /* Initial trigonometric type. */
  dtype = FIX ;               /* Initial number display mode. */
  down = 0 ;                  /* No mouse presses initially. */

  make_items() ;              /* Create panel items and cursors. */
  do_clear() ;                /* Initialise and clear display. */
  set_cursor(MAINCURSOR) ;    /* Initially display the arrow cursor. */
  start_tool() ;              /* Display the calculator. */
  exit(0) ;
}


/* Calctools' customised math library error-handling routine. */

doerr(funcname, etype, eval)
char *funcname, *etype ;
int eval ;
{
  SPRINTF(display, "%s: %s error", funcname, etype) ;
  set_item(DISPLAYITEM, display) ;
  error = eval ;
  set_item(OPITEM, "CLR") ;
}


/* Default math library exception handling routine. */

/*ARGSUSED*/
matherr(exc)
struct exception *exc ;
{
  STRCPY(display, "Error") ;
  set_item(DISPLAYITEM, display) ;
  error = 1 ;
  set_item(OPITEM, "CLR") ;
}
