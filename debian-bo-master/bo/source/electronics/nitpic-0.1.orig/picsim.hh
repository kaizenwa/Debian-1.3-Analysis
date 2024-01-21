/*													-*- c++ -*-
 * picsim.hh -- X PIC simulator
 */
#include "patchlevel.h"
#include <std.h>
#include <ctype.h>
#include <sys/types.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/TextSink.h>
#include "pu_defs.h"			/* defines the PIC definition structure */
#ifdef __cplusplus
}
#endif

typedef struct reg {			/* Info about each register in register file */
	char	*name;
	unsigned modified : 1;
	unsigned modified_last : 1;
	char	 redirect_page;
	char	 redirect_reg;
	char	 implemented;
	char	 value;
	char	(*hook)( struct reg *r, Boolean write, char v = 0 );
} reg;

typedef struct {
	char	*reg_name;
	char	*bit_names[8];
	char	 value;
	char	 old_value;
} info_reg;

typedef struct {
	char		*pin_name;
	unsigned	 io : 2;
	unsigned	 hi : 1;
} pin_info;
	
typedef struct {
	int			 npins;
	pin_info	*pins;
} pins_info;

/*
 * These are commands that the sim controller will send to the PIC execution
 * module.
 */
typedef enum {
	Init,						// Power-on reset
	Reset,						// MCLR reset
	GetPage,					// Return reg file page select bits
	GetInfo,					// Return current vals of regs that should
								// be displayed in the Info window
	GetPins,					// Get info for IC window
	Disassemble,				// Return a string for the Instr window
	Step						// Run a cycle
} SimFunc;

typedef union {					/* Simulator argument & return value */
	int		 i;
	void	*p;
} SimAR;

typedef struct {
	unsigned running:1;
	unsigned sleeping:1;
	u_long	 ticks;				/* clock cycles @ uinfo.clock */
	u_long	 millisec;			/* ms of realtime elapsed in simulation */
	int		 millisec_left;		/* clock ticks left before another ms */
	float	 wdt_min;			/* WDT min, nominal, and max timeout */
	float	 wdt_nominal;		/* values */
	float	 wdt_max;
	u_long	 wdt_left;			/* ticks left until WDT(max) timeout */
	int		 prescaler;
	int		 vreg;				/* Page number of reg file in reg window */ 
	int		 itop;				/* PC of top line in instr window */
	int		 pc;				/* simulator PC */
	int		 isize;				/* words of instruction memory */
	int		 nstack;
	int		 sp;
	int		 stack[32];
	int		 instr;
	int		 prefetch;
	int		 reg_pages;
	reg		 reg_file[4][128];
	int		 neeprom;
	char	 eeprom[128];
	SimAR	(*simulate)( SimFunc func, SimAR arg );
	PICDEFN	 uinfo;				/* User info (program, clock, etc) */
} PIC;

typedef struct {
	Boolean	 nitpic_resources;
	Pixel	 wdt_color_1;
	Pixel	 wdt_color_2;
	Pixel	 wdt_color_3;
	float	 wdt_min;
	float	 wdt_nom;
	float	 wdt_max;
	XFontStruct *info_bits;
	Pixel	 led_on;
	Pixel	 led_off;
	Pixmap	 left_pointer;
	Pixmap	 left_hollow;
	Pixmap	 right_pointer;
	Pixmap	 right_hollow;
} OptionsRec;

/*
 * Function declarations
 */
extern void		 ic_update( void );
extern void		 info_update( void );
extern void		 info_quick( void );
extern Boolean	 ipointer_erase( void );
extern void		 sim_init( const char *fname );
extern void		 redraw( void );
extern void		 reg_update( void );
extern int		 dialog( const char *text, int n, ... );

/*
 * Simulators
 */
extern SimAR	 pic16c84( SimFunc func, SimAR arg );

/*
 * Action routines
 */
#define	XtActionProcArgs Widget w, XEvent *ev, String *prms, Cardinal *nprm
extern void	 ic_refresh( XtActionProcArgs );
extern void	 info_refresh( XtActionProcArgs );
extern void	 ipointer_refresh( XtActionProcArgs );
extern void	 ipointer_seek( XtActionProcArgs );
extern void	 instr_refresh( XtActionProcArgs );
extern void	 load_file( XtActionProcArgs );
extern void	 reg_refresh( XtActionProcArgs );
extern void	 reg_switch( XtActionProcArgs );
extern void	 sim_step( XtActionProcArgs );
extern void	 sim_reset( XtActionProcArgs );
extern void	 sim_toggle_run( XtActionProcArgs );
#undef	XtActionProcArgs

/*
 * Callbacks
 */
extern void	 instr_scroll( Widget w, XtPointer client_data, XtPointer pos );
extern void	 instr_jump( Widget w, XtPointer client_data, XtPointer percent );

#ifdef __main__
#	define	GLOBAL
#else
#	define	GLOBAL	extern
#endif

GLOBAL	struct {
	Widget	 toplevel;
	Widget	 main;
	Widget		 title;
	Widget		 copyright;
	Widget		 info;
	Widget		 ic;
	Widget		 instr;
	Widget			 ipointer;
	Widget			 itext;
	Widget			 iscroll;
	Widget		 regs;
	Widget			 rprev;
	Widget			 rpage;
	Widget			 rnext;
	Widget			 rdata;
	Widget		 data;
	Widget		 reset;
	Widget		 step;
	Widget		 run;
	Widget		 interrupt;
	Widget		 file;
	Widget		 quit;
} W;

GLOBAL OptionsRec	 Options;
GLOBAL XtAppContext	 AppContext;

GLOBAL	PIC			 Pic;
