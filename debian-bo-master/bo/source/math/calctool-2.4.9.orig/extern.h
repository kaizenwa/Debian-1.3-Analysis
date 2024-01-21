
/*  @(#)extern.h 1.9 89/11/01
 *
 *  Contains the external variable definitions used by calctool.
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

extern char *make_fixed(), *make_number(), *make_scientific() ;
extern char base_str[4][4] ;               /* Strings for each base value. */
extern char con_names[MAXREGS][MAXLINE] ;  /* .calctoolrc constant names. */
extern char cur_op ;            /* Current arithmetic operation. */
extern char current ;           /* Current button or character pressed. */
extern char digits[] ;          /* Valid numerical digits. */
extern char display[] ;         /* Current calculator display. */
extern char dtype_str[2][4] ;   /* Strings for each display mode value. */
extern char *exp_posn ;         /* Position of the exponent sign. */
extern char fnum[] ;            /* Scratch area for fixed point numbers. */
extern char fun_names[MAXREGS][MAXLINE] ; /* .calctoolrc function names. */
extern char fun_vals[MAXREGS][MAXLINE] ;  /* .calctoolrc function defs. */
extern char geometry[] ;        /* X11 geometry information. */
extern char helpname[] ;        /* Filename for helpfile. */
extern char num_names[MAXREGS][2] ;       /* Used by the popup menus. */
extern char old_cal_value ;     /* Previous calculation operator. */
extern char progname[] ;        /* Name of this program. */
extern char pstr[] ;            /* Current button text string. */
extern char revtable[] ;        /* Table for reversing the bits in a byte. */
extern char *selection ;        /* Current [Get] selection. */
extern char *shelf ;            /* PUT selection shelf contents. */
extern char snum[] ;            /* Scratch are for scientific numbers. */
extern char ttype_str[3][5] ;   /* Strings for each trig type value. */
extern char validkeys[] ;       /* Valid keys after an error condition. */
extern char validmenu[] ;       /* Valid keys with popup menus. */
extern char x11_display[] ;     /* X11 display information. */

extern double convert_display() ;
extern double con_vals[] ;        /* Selectable constants. */
extern double disp_val ;           /* Value of the current display. */
extern double exp_p1[10][4] ;
extern double last_input ;         /* Previous number input by user. */
extern double max_fix[] ;          /* Maximum showable fixed values. */
extern double mem_vals[] ;         /* Memory register values. */
extern double powers[11][4] ;      /* Table of power values for each base. */
extern double result ;             /* Current calculator total value. */
extern double tresults[] ;         /* Current trigonometric results. */

extern enum base_type base ;       /* Current base: BIN, OCT, DEC or HEX. */
extern enum gr_type gtype ;        /* What graphics system is being used. */
extern enum num_type dtype ;       /* Number display mode. */
extern enum trig_type ttype ;      /* Trigonometric type (deg, grad or rad). */

extern int accuracy ;       /* Number of digits precision (Max 9). */
extern int basevals[] ;     /* Supported arithmetic bases. */
extern int chxoff[] ;       /* X offset for various length button strings. */
extern int color ;          /* Color used for current raster operation. */
extern int column ;         /* Column number of current key/mouse press. */
extern int cur_ch ;         /* Current character if keyboard event. */
extern int curx ;           /* Current mouse X position. */
extern int cury ;           /* Current mouse Y position. */
extern int disp_length[] ;  /* Display length in characters for each base. */
extern int down ;           /* Indicates if mouse button is down. */
extern int error ;          /* Indicates some kind of display error. */
extern int hyperbolic ;     /* If set, trig functions will be hyperbolic. */
extern int iconic ;         /* Set if window is currently iconic. */
extern int inv_video ;      /* Set if user wants inverse video mode. */
extern int inverse ;        /* If set, trig & log functions will be inversed. */
extern int iscolor ;        /* Set if this is a color screen. */
extern int ishelp ;         /* Set if there is a help file. */
extern int issel ;          /* Set if valid [Get] selection. */
extern int ix ;             /* Initial X position of the icon. */
extern int iy ;             /* Initial Y position of the icon. */
extern int key_exp ;        /* Set if entering exponent number. */
extern int new_input ;      /* New number input since last op. */
extern int nextc ;          /* Current event identifier. */
extern int nfont_width ;    /* Width of the normal font characters. */
extern int pending ;        /* Set for command with on multiple presses. */
extern int pending_op ;     /* Arithmetic operation for pending command. */
extern int pointed ;        /* Whether a decimal point has been given. */
extern int portion ;        /* Button portion on current key/mouse press. */
extern int posspec ;        /* Set if -Wp or -g option is present (for X11) */
extern int row ;            /* Row number of current key/mouse press. */
extern int rstate ;         /* Is memory register frame displayed? */
extern int signgam ;
extern int spaces ;         /* Number of spaces in current button string. */
extern int toclear ;        /* Indicates if display should be cleared. */
extern int tstate ;         /* Current button set being displayed. */
extern int wx ;             /* Initial X position of the window. */
extern int wy ;             /* Initial Y position of the window. */
extern int x ;              /* X offset for text for button. */
extern int y ;              /* Y offset for text for button. */

extern FILE *hfd ;          /* File descriptor for help information. */

extern struct iteminfo items[] ;  /* Panel items. */
extern struct button buttons[] ;  /* Calculator button values. */
