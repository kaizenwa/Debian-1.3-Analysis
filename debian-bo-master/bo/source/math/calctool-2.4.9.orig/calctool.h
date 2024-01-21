
/*  @(#)calctool.h 1.10 89/11/06
 *
 *  Contains all the global definitions used by calctool.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

char *getenv(), *getwd();

#define  CLOSE          (void) close      /* To make lint happy. */
#define  FCLOSE         (void) fclose
#define  FFLUSH         (void) fflush
#define  FGETS          (void) fgets
#define  FPRINTF        (void) fprintf
#define  IOCTL          (void) ioctl
#define  PUTC           (void) putc
#define  READ           (void) read
#define  SELECT         (void) select
#define  SIGNAL         (void) signal
#define  SPRINTF        (void) sprintf
#define  SSCANF         (void) sscanf
#define  STRCAT         (void) strcat
#define  STRCPY         (void) strcpy
#define  STRNCAT        (void) strncat
#define  STRNCPY        (void) strncpy
#define  WRITE          (void) write

/* Various pseudo events used by the calctool program. */
#define  CFRAME_REPAINT   100    /* Main frame needs repainting. */
#define  RFRAME_REPAINT   101    /* Register frame needs repainting. */
#define  ENTER_WINDOW     102    /* Mouse has enter calctool window. */
#define  EXIT_WINDOW      103    /* Mouse has exited calctool window. */ 
#define  KEYBOARD         104    /* Keyboard character was pressed. */
#define  LEFT_DOWN        105    /* Left mouse button was depressed. */
#define  LEFT_UP          106    /* Left mouse button was debounced. */
#define  MIDDLE_DOWN      107    /* Middle mouse button was depressed. */
#define  MIDDLE_UP        108    /* Middle mouse button was debounced. */
#define  RIGHT_DOWN       109    /* Right mouse button was depressed. */
#define  RIGHT_UP         110    /* Right mouse button was debounced. */
#define  TAKE_FROM_SHELF  111    /* PUT function key was pressed. */
#define  PUT_ON_SHELF     112    /* GET function key was pressed. */
#define  DIED             113    /* Calctool window has been destroyed. */
#define  LASTEVENTPLUSONE 114    /* Not one of the above. */

#define  HELPCURSOR     0      /* Cursor types. */
#define  MAINCURSOR     1

enum base_type { BIN, OCT, DEC, HEX } ;      /* Base definitions. */

enum but_state { NORMAL, INVERTED } ;        /* Calculator button states. */
 
enum can_type { KEYCANVAS, REGCANVAS, PANELCANVAS } ;  /* Canvas types. */

enum font_type { SFONT, NFONT, BFONT } ;     /* Text font definitions. */

/* Graphics supported. */
enum gr_type  { MGR, NEWS, SVIEW, TTY, X11, XVIEW } ;

/* Pseudo panel items. */
enum item_type { BASEITEM, DISPLAYITEM, TTYPEITEM, NUMITEM,
                 HYPITEM,  INVITEM,     OPITEM } ;

/* Popup menu types. Used with some of the graphics versions. */
enum menu_type {
       M_ACC,    M_CON,   M_EXCH,   M_FUN,
       M_LSHIFT, M_RCL,   M_RSHIFT, M_STO
} ;

enum num_type { FIX, SCI } ;                 /* Number display mode. */

enum op_type { OP_SET, OP_CLEAR, OP_NOP } ;  /* Operation item settings. */

enum trig_type { DEG, GRAD, RAD } ;          /* Trigonometric types. */

/*  Mathematical constants used by the routines in functions.c
 *  These should be declared in math.h, but just in case....
 */

#ifndef  HUGE
#define  HUGE            1.701411733192644270e38
#endif /*HUGE*/

#ifndef  LN10
#define  LN10            2.30258509299404568402
#endif /*LN10*/

#ifndef  PI
#define  PI              3.14159265358979323846
#endif /*PI*/

#define  BBORDER        10     /* No of pixels in border. */
#define  BCOLS          6      /* No of columns of buttons. */
#define  BGAP           5      /* No of pixels between buttons. */
#define  BHEIGHT        52     /* Number of pixels for height. */
#define  BROWS          6      /* No of rows of buttons. */
#define  BWIDTH         44     /* No of pixels for width. */

#define  CCTRL(n)       n - 96     /* Generate control character value. */
#define  DISPLAY        30         /* Calculators numerical display. */

#define  EQUAL          !strcmp    /* For character comparisons. */
#define  EXTRA          5          /* Extra useful character definitions. */

#ifndef  HELPNAME
#define  HELPNAME       "./calctool.help"
#endif /*HELPNAME*/

#define  ICONHEIGHT     64         /* Height of calctool icon. */
#define  ICONWIDTH      42         /* Width of calctool icon. */
#define  INC            argc-- ; argv++ ;
#define  MAX_DIGITS     32         /* Maximum displayable number of digits. */
#define  MAXITEMS       7          /* Maximum number of panel items. */

#ifndef  MAXLINE
#define  MAXLINE        256        /* Length of character strings. */
#endif /*MAXLINE*/

#define  MAXMENUS       8          /* Maximum number of popup menus. */

#ifndef  MAXPATHLEN
#define  MAXPATHLEN     1024       /* Longest possible path length. */
#endif /*MAXPATHLEN*/

#define  MAXREGS        10         /* Maximum number of memory registers. */
#define  MAXVKEYS       7          /* Number of valid keys after an error. */

#ifndef  MIN
#define  MIN(x,y)       ((x) < (y) ? (x) : (y))
#endif /*MIN*/

#ifndef  NEWSFILE
#define  NEWSFILE       "calctool.ps"
#endif /*NEWSFILE*/

#define  NOBUTTONS      BROWS * BCOLS

#ifdef   NOINDEX
#define  index          strchr
#endif /*NOINDEX*/

#ifndef  RCNAME
#define  RCNAME         ".calctoolrc"
#endif /*RCNAME*/

#ifndef  NO_4_3SIGNAL
#define  SIGRET         void
#else
#define  SIGRET         int
#endif /*NO_4_3SIGNAL*/

#define  THEIGHT        (BROWS*BHEIGHT) + ((BROWS-1) * BGAP) + (2*BBORDER)
#define  TITEMS         NOBUTTONS*2 + EXTRA    /* Total definitions. */
#define  TWIDTH         (BCOLS*BWIDTH) + ((BCOLS-1) * BGAP) + (2*BBORDER)

typedef  unsigned long  BOOLEAN ;

struct iteminfo                  /* Panel item information record. */
  {
    enum font_type font ;        /* Font type for this panel item. */
    int x ;                      /* X position of item. */
    int y ;                      /* Y position of item. */
    char text[60] ;              /* Text string associated with this item. */
  } ;

struct button {
         char *str ;             /* Button display string. */
         char value ;            /* Unique button keyboard equivalent. */
         enum op_type opdisp ;   /* Display operation code during operation. */
         char color ;            /* Color of button portion. */
         int  (*func)() ;        /* Function to obey on button press. */
} ;
