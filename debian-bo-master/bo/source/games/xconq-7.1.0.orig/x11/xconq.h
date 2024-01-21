/* Definitions for the X11 interface to Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Default color of text and icons - 0 is for white on black, 1
   is for black on white.  Should be set appropriately for the most
   common monochrome display (color displays always do white on black).
   This is also settable by the player, so the default is just for
   convenience of the majority. */

#define BLACKONWHITE 1

#define BARGRAPHS

/* The default fonts can be altered by users, so these are just hints. */

#define TEXTFONT "fixed"

#define ICONFONT "xconq"
#define STANDARD "standard"

/* Some X11 servers die if too much is written between output flushes. */

/* #define STUPIDFLUSH */

/* This is the name of a family of programs, so argv[0] inadequate. */

#define PROGRAMNAME "xconq"
#define PROGRAMCLASSNAME "Xconq"

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Panner.h>
#include <X11/Xaw/Porthole.h>
#include <X11/Shell.h>

#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/SmeBSB.h>

#include <math.h>

#include "imf.h"
#include "ximf.h"
#include "ui.h"
#include "print.h"

#ifndef IMFLIB
#ifdef VMS
#define IMFLIB "[-.lib-x11]"
#else /* not VMS */
#define IMFLIB "../lib-x11"
#endif /* VMS */
#endif /* IMFLIB */

enum grayshade {
  gray,
  darkgray,
  numgrays
};

/* Drawing techniques that may be used. */

enum whattouse {
  dontdraw,
  useblocks,
  usepictures,
  usefontchars,
  usepolygons
};

/* Types of controls. */

enum controltypeenum {
    LOOK = 0,
    MOVE,
    UNIT_MOVE,
    UNIT_SHOOT,
    UNIT_BUILD,
    SHOW_TERRAIN,
    SHOW_GRID,
    SHOW_UNITS,
    SHOW_NAMES,
    SHOW_FEATURE_NAMES,
    SHOW_FEATURE_BOUNDARIES,
    SHOW_NUMBERS,
    SHOW_PEOPLE,
    SHOW_ELEV,
    SHOW_TEMP,
    SHOW_ALL,
    SHOW_MORE,
    COLR_UNITS,
    COLR_TERR,
    COLR_EMBL,
    MONO_REVERSE,
    ZOOM_OUT,
    ZOOM_IN,
    POPUP_HELP,
    numcontrols
};

/* Tools govern the interpretation of mouse clicks in a map view. */

enum tooltypes {
  looktool,
  movetool,
  unitmovetool,
  unitshoottool,
  unitbuildtool,
#ifdef DESIGNERS
  cellpainttool,
  bordpainttool,
  connpainttool,
  unitaddtool,
  peoplepainttool,
  featurepainttool,
#endif /* DESIGNERS */
  numtools
};

struct a_movie {
  enum movie_type type;
  int args[5];
};

#define N_COMMAND  3
#define N_CHOICE   1
#define N_BUTTON   (N_COMMAND+N_CHOICE)
#define N_TOGGLE   6
#define N_DIALOG  12
#define N_DIMEN_D 10 
#define N_WIDGET (N_BUTTON+N_TOGGLE+N_DIALOG)

/* The user interface substructure.  This is only allocated for sides with
   attached displays. */

typedef struct a_ui {
    Display *dpy;		/* The X display used by this side */
    int active;			/* True when the display is usable */
    int screen;			/* The X screen in use */
    Widget shell;		/* Main shell for this side's displays */
    Window rootwin;		/* The root window */
    Pixel foreground;
    Pixel background;
    String geospec;
    int mpTime;			/* # of milliseconds to pause between mplayer moves */
    Atom kill_atom;
    short bonw;			/* true if display is black-on-white */
    short follow_action;	/* scroll to where something has occured */
    int sxdown, sydown;
    int	cellx, celly;
    Boolean cellxy_ok;		/* cellx & celly valid? */
    struct a_map *mapdown;
    int beepcount;		/* number of times we've been beeped */
    /* Constructed during display init. */
    short monochrome;		/* obvious */
    short fw, fh;		/* dimensions of text font (in pixels) */
    /* Working variables for the display. */
    long bgcolor;		/* background color */
    long fgcolor;		/* foreground (text) color */
    long whitecolor;		/* actual white for this display */
    long blackcolor;		/* actual black for this display */
    long enemycolor;		/* color for them (usually red) */
    long neutcolor;		/* color for fencesitters (usually gray) */
    long graycolor;		/* color for graying out (usually gray) */
    long diffcolor;		/* unusual/distinct color (usually maroon) */
    long goodcolor;		/* color for OKness (usually green) */
    long badcolor;		/* color for non-OKness (usually red) */
    long gridcolor;
    long unseencolor;
    long cellcolor[MAXTTYPES];	/* the color of each terrain type */
    long cellfgcolor[MAXTTYPES]; /* the "fg" color of each terrain type */
    long colors[MAXSIDES][3];
    int numcolors[MAXSIDES];
    long *unitcolors;
    long *numunitcolors;
    short dflt_color_unit_images;
    short dflt_color_terr_images;
    short dflt_color_embl_images;
    GC gc;			/* a tmp graphics context for this display */
    GC textgc;			/* foreground on background text */
    GC ltextgc;			/* foreground on background text */
    GC terrgc;			/* terrain display gc */
    GC unitgc;			/* unit display gc */
    GC emblgc;			/* emblem display gc */
    GC bdrygc;			/* country/feature boundary gc */
    XFontStruct *textfont;	/* Font for text display */
    /* Map-related slots. */
    struct a_map *maps;		/* Chain of maps that are up */
    /* Help-related slots. */
    Widget help_shell;
    Widget help_form;
    Widget help_topicPort;
    Widget help_topicList;
    Widget help_title;
    Widget help_text;
    Widget help_button_box;
    Widget help_next;
    Widget help_prev;
    Widget help_back;
    Widget help_close;
    short helpw, helph;
    struct a_helpnode *curhelpnode;
    struct a_helpnode **nodestack;
    int nodenumber, nodestackpos;
    /* Printing-related slots. */
    Widget print_shell;
    Widget print_help_shell;
    Widget print_cmds[N_WIDGET];
    PrintParameters *ps_pp;
    int choi[N_CHOICE];
    int flag[N_TOGGLE];
    double parm[N_DIALOG];
    int i_metric;
#ifdef DESIGNERS
    /* Design-related slots. */
    Widget design_shell;
    Widget design;
    Widget normal_button;
    Widget normal_label;
    Widget terrain_button;
    Widget terrain_label;
    Widget unit_button;
    Widget unit_label;
    Widget people_button;
    Widget people_label;
    Widget feature_button;
    Widget feature_label;
    Widget brush_radius_label;
    short curdesigntool;
    short curbrushradius;
    short curttype;
    short curbgttype;
    short curutype;
    short curusidenumber;
    short cursidenumber;
    short curfid;
#endif /* DESIGNERS */
    /* Arrays of image families for units, terrain, and sides. */
    ImageFamily **uimages;
    ImageFamily **timages;
    ImageFamily **eimages;
    /* Terrain drawing machinery */
    enum whattouse usewhat[NUMPOWERS][MAXTTYPES];
    /* (should use as cache of image in image family - calc as needed) */
    Pixmap terrpics[NUMPOWERS][MAXTTYPES];
    char terrchars[NUMPOWERS][MAXTTYPES];
    XFontStruct *terrfonts[NUMPOWERS][MAXTTYPES];
    Pixmap hexpics[NUMPOWERS], bhexpics[NUMPOWERS];
    Pixmap hexchars[NUMPOWERS], bhexchars[NUMPOWERS];
    XFontStruct *hexfonts[NUMPOWERS], *bhexfonts[NUMPOWERS];
    /* Unit drawing machinery */
    Pixmap unitpics[NUMPOWERS][MAXUTYPES];
    Pixmap unitmasks[NUMPOWERS][MAXUTYPES];
    char unitchars[NUMPOWERS][MAXUTYPES];
    XFontStruct *unitfonts[NUMPOWERS][MAXUTYPES];
    XFontStruct *unitfont;	/* font for unit characters */
    int unitw[NUMPOWERS][MAXUTYPES], unith[NUMPOWERS][MAXUTYPES];
    /* Emblem drawing machinery */
    Pixmap emblempics[MAXSIDES];
    Pixmap emblemmasks[MAXSIDES];
    int embw[MAXSIDES], embh[MAXSIDES];
    /* Map legend drawing machinery */
    XFontStruct *ulegendfonts[NUMPOWERS][MAXUTYPES];
    XFontStruct *flegendfonts[6];
    Font flegendfids[6];
    Legend *legends;
    int numfeatures;
    /* Side closeup list */
    struct a_side_closeup *sidecloseuplist;
    /* Unit list */
    struct a_unit_list *unitlistlist;
    /* Unit closeup list */
    struct a_unit_closeup *unitcloseuplist;
    /* Unit closeup summary */
    struct a_closeup_summary *closeupsummary;
    /* Orders interface */
    Widget orders_shell;
    Widget orders_label;
    Widget orders_form;
    Widget orders_help_shell;
    StandingOrder *sorder_edit;
    char *sorder_types_edit;
    Task *sorder_task_edit;
    struct a_order_interface *ordi_edit;
    struct a_order_interface *orderlist;
    int *grok_p1;
    int *grok_p2;
    int grok_size;
    /* Random stuff */
    Pixmap bombpics[4];
    Pixmap hitpics[3];
    Pixmap controlpics[numcontrols];
    Pixmap toolcursors[numtools];
    Pixmap unitcursors[MAXUTYPES];
    Pixmap boxcurs, boxmask;
    Pixmap grays[numgrays];
    int numscheduled;
    struct a_movie movies[10];
} UI;

/* A closer look at a side. */
typedef struct a_side_closeup {
    struct a_map *map;
    struct a_side *side;
    Widget shell, info, *filter, list_units;
    struct a_side_closeup *next;
} SideCloseup;

/* A list of units. */
typedef struct a_unit_list {
    struct a_map *map;
    int number;
    Unit **units;
    char **labels;
    Widget shell, label, list, close;
    struct a_unit_list *next;
} UnitList;

/* A closer look at a unit. */
typedef struct a_unit_closeup {
    struct a_map *map;
    struct a_unit *unit;
    Widget shell, info;
    struct a_unit_closeup *next;
} UnitCloseup;

/* List of unit closeups */
typedef struct a_closeup_summary {
    int number;
    UnitCloseup **unitcloseups;
    char **labels;
    Widget shell, label, list;
} CloseupSummary;
 
/* Orders interface */
typedef struct a_order_interface {
    StandingOrder *sorder;
    Widget form, toggle, types, etype, eparms, task, tparms;
    Pixel form_bg, form_fg;
    struct a_order_interface *next;
} OrderInterface;

/* Iteration over all of a side's unit closeups. */
#define for_all_unit_closeups(c,s)  \
  for ((c) = (s)->ui->unitcloseuplist; (c) != NULL; (c) = (c)->next)

/* Iteration over all of a side's unit lists. */
#define for_all_unit_lists(l,s)  \
  for ((l) = (s)->ui->unitlistlist; (l) != NULL; (l) = (l)->next)

/* Iteration over all of a side's side closeups. */
#define for_all_side_closeups(c,s)  \
  for ((c) = (s)->ui->sidecloseuplist; (c) != NULL; (c) = (c)->next)

/* Each side can open up any number and shape of maps. */

/* All the maps will share colors and bitmaps though. */

typedef struct a_map {
    /* Widgets that make up the map. */
    Widget mainwidget;
    Widget leftpane;		/* lefthand area of window */
    Widget leftform;		/*  */
    Widget rightpane;		/* righthand area of window */
    Widget infoform;		/* parts of leftform */
    Widget controlform;		/* parts of leftform */
    Widget *controls;		/* Array of controls */
    Widget mapform;		/* parts of leftform */
    Widget sideform;		/* parts of rightform */
    Widget info;		/* Info about current pos/unit */
    Widget porthole;
    Widget portlabel;
    Widget gamedate;		/* Overall game info */
    Widget gameclock;		/* overall game info */
    Widget msgarea;		/* miscellaneous info related to UI */
    Widget sides;		/* List of sides */
    Widget history;		/* Place for notices/warnings */
    Widget promptlabel;
    Widget listview;
    Widget listform;
    Widget *list_buttons;	/* numutypes+1 of these */
    Widget pannerbox;
    Widget panner;
    Widget help_pop;
    Widget ctrlpanel_pop;	/* View control popup */
    Widget ctrlpanel_shell;	/* View control popup's shell */
    Widget ctrlpanel_form;
    Widget *ctrlpanel_buttons;	/* Array of buttons */
    /* Raw windows that we draw into directly. */
    Window infowin;		/* info about a unit and/or cell */
    Pixmap viewwin;		/* actual graphical display of an area */
    Window sideswin;		/* list of sides */
    /* panner pixmap */
    Pixmap panner_pix;
    /* How to draw the map. */
    short seeall;		/* True if viewing world data directly */
    short drawterrain;		/* Display solid color terrain on the map? */
    short drawgrid;		/* Draw outlines around cells? */
    short drawcellpats;		/* Draw terrain patterns? */
    short drawunits;		/* Display units on the map? */
    short drawnames;		/* Display unit names/numbers on the map? */
    short drawfeatureboundaries;
    short drawfeaturenames;	/* true if displaying names of features */
    short oldesttoshow;		/* the relative time of the oldest data */
    short agetofade;		/* point at which to gray out the display */
    short newesttoshow;		/* the relative time of the newest data */
    short fadeterrain;		/* fade world features as well as units? */
    short lowestlevel;		/* the lowest level of units to display */
    short highestlevel;		/* the highest level of units to display */
    short drawpeople;		/* true if displaying people sides */
    short drawelevations;		/* true if display elevations */
    short drawresources[MAXMTYPES];
    short drawtemp;
    short drawweather;
    short use_color_unit_images;
    short use_color_terr_images;
    short use_color_embl_images;
    short fullpanel;
    short follow_action;
    /* Slots used for internal display calculations. */
    VP *vp;			/* This map's generic view parameters */
    short totalw, totalh;	/* Total size of window in pixels */
    short leftfrac;
    short leftw;
    short pxw, pxh;		/* Size of map subwindow in pixels */
    short toph;
    short infoh;
    short list1frac;
    short list1w, list1h;
    short list2w, list2h;
    short panw, panh;
    short vx, vy;		/* lower left of viewport in cell coords */
    short vw, vh;		/* Boundaries of viewport in cell coords */
    short vw2, vh2;		/* 1/2 (rounded down) of above values */
    short sidespacing;		/* Vertical space for each side in side list */
    short last_num_in_play[MAXUTYPES];
    short last_num_incomplete[MAXUTYPES];
    /* Interaction controls. */
    short curtool;		/* Interpretation of left-mouse clicks */
    short curx, cury;		/* Current spot being looked at */
    struct a_unit *curunit;	/* Unit under cursor */
    short savedcurx, savedcury;	/* Current spot being looked at (saved) */
    struct a_unit *savedcurunit;/* Unit under cursor (saved) */
    void (*modalhandler) PARAMS ((Side *side, struct a_map *map, int cancelled));
    char inpch;			/* Keyboard char */
    short inptype;		/* Unit type clicked in unit type list */
    short prefixarg;		/* numerical prefix argument */
    short argunitid;		/* is unit id, not ptr, for safety */
    short tmpt;
    struct a_side *argside;
    short uvec[MAXUTYPES];	/* vector of allowed unit types to input */
    char ustr[MAXUTYPES+1];	/* used in composing unit type hints */
    short tvec[MAXTTYPES];	/* vector of allowed terrain types to input */
    char tstr[MAXTTYPES+1];	/* used in composing terrain type hints */
    char prompt[BUFSIZE];	/* prompt for input */
    char answer[BUFSIZE];	/* string being typed in */
    int tmpint;
    short frombutton;		/* true if command was issued from button */
    /* Link to the next map. */
    struct a_map *next;
} Map;

/* Iteration over all of a side's map windows. */

#define for_all_maps(s,m)  \
  for ((m) = (s)->ui->maps; (m) != NULL; (m) = (m)->next)

#define num_unit_colors(s,s2,u)  \
  (((s)->ui->numunitcolors)[numutypes * (s2) + (u)])

#define unit_color(s,s2,u,n)  \
  (((s)->ui->numunitcolors)[3 * (numutypes * (s2) + (u)) + (n)])

/* Values shared by all displays and all sides. */

extern Widget thistoplevel;
extern XtAppContext thisapp;
extern int nargs;
extern Arg tmpargs[];

/* Declarations of globally visible functions. */

extern int ask_unit_type PARAMS ((Side *side, Map *map, char *prompt,
				 int *poss,
				 void (*handler)(Side *side, Map *map,
						 int cancel)));
extern int ask_terrain_type PARAMS ((Side *side, Map *map, char *prompt,
				    int *poss,
				    void (*handler)(Side *side, Map *map,
						    int cancel)));
extern void ask_side PARAMS ((Side *side, Map *map, char *prompt,
			     Side *dflt,
			     void (*handler)(Side *side, Map *map,
					     int cancel)));
extern void ask_position PARAMS ((Side *side, Map *map, char *prompt,
				 void (*handler)(Side *side, Map *map,
						 int cancel)));
extern void ask_bool PARAMS ((Side *side, Map *map, char *prompt,
			     int dflt, void (*handler)(Side *side, Map *map,
						       int cancel)));
extern void ask_string PARAMS ((Side *side, Map *map, char *prompt, char *dflt,
			       void (*handler)(Side *side, Map *map,
					       int cancel)));

extern int grok_unit_type PARAMS ((Side *side, Map *map, int *typep));
extern int grok_terrain_type PARAMS ((Side *side, Map *map, int *typep));
extern int grok_side PARAMS ((Side *side, Map *map, Side **side2p));
extern int grok_position PARAMS ((Side *side, Map *map, int *xp, int *yp));
extern int grok_bool PARAMS ((Side *side, Map *map));
extern int grok_string PARAMS ((Side *side, Map *map, char **strp));

extern int xform PARAMS ((Side *side, Map *map, int x, int y,
			 int *sxp, int *syp));
extern int x_xform_unit PARAMS ((Side *side, Map *map, Unit *unit,
				int *sxp, int *syp, int *swp, int *shp));
extern int x_xform_unit_self PARAMS ((Side *side, Map *map, Unit *unit,
				     int *sxp, int *syp, int *swp, int *shp));
extern int x_xform_occupant PARAMS ((Side *side, Map *map,
				    Unit *transport, Unit *unit,
				    int sx, int sy, int sw, int sh,
				    int *sxp, int *syp, int *swp, int *shp));
extern int x_nearest_cell PARAMS ((Side *side, Map *map, int sx, int sy,
				  int *xp, int *yp));
extern int x_nearest_boundary PARAMS ((Side *side, Map *map, int sx, int sy,
				      int *xp, int *yp, int *dirp));
extern int x_nearest_unit PARAMS ((Side *side, Map *map, int sx, int sy,
				  Unit **unitp));

extern void init_x_signal_handlers PARAMS ((void));

extern void popup_game_dialog PARAMS ((void));

extern void check_player_displays PARAMS ((void));
extern void init_all_displays PARAMS ((void));
extern void init_redraws PARAMS ((void));
extern void init_display PARAMS ((Side *side));
extern void set_colors PARAMS ((Side *side));
extern long request_color PARAMS ((Side *side, char *name));

extern XFontStruct *open_font PARAMS ((Side *side, char *name, char *xdefault,
				      XFontStruct *altfont, char *alttype,
				      Font *fid));
extern Cursor make_cursor PARAMS ((Display *dpy, Window win,
				  char *cursbits, char *maskbits,
				  unsigned long fg, unsigned long bg,
				  unsigned int x, unsigned int y));

extern void reset_color_state PARAMS ((Side *side));
extern void reset_window_colors PARAMS ((Side *side, Window win));

extern void execute_command PARAMS ((Side *side, Map *map));

extern void set_current_unit PARAMS ((Side *side, Map *map, Unit *unit));
extern void set_current_xy PARAMS ((Side *side, Map *map, int x, int y));
extern void clear_current PARAMS ((Side *side, Map *map));
extern void save_cur PARAMS ((Side *side, Map *map));
extern void restore_cur PARAMS ((Side *side, Map *map));

extern Map *create_map PARAMS ((Side *side, int power, char *geospec));

extern void add_map_actions PARAMS ((void));

extern void set_tool_cursor PARAMS ((Side *side, Map *map));

extern void set_map_power PARAMS ((Side *side, Map *map, int power));
extern void x_center_on_focus PARAMS ((Side *side, Map *map));

extern void handle_key_event PARAMS ((Side *side, Map *map, XEvent *evt));

extern int find_side_and_map PARAMS ((Widget w, Side **sidep, Map **mapp));
extern int find_side_and_map_via_control PARAMS ((Widget w,
						 Side **sidep, Map **mapp));
extern int find_side_and_map_via_listform PARAMS ((Widget w,
						  Side **sidep, Map **mapp));
extern int find_side_and_map_via_mapform PARAMS ((Widget w,
						 Side **sidep, Map **mapp));
extern int find_side_and_map_via_rightform PARAMS ((Widget w,
						   Side **sidep, Map **mapp));
extern int find_side_and_map_via_a_toplevel PARAMS ((Widget w,
						    Side **sidep, Map **mapp));
extern int find_side_and_map_via_porthole PARAMS ((Widget w,
						  Side **sidep, Map **mapp));
extern int find_side_via_widget PARAMS ((Widget w, Side **sidep));

extern void update_controls PARAMS ((Side *side, Map *map));

extern void draw_all_maps PARAMS ((Side *side));

extern void draw_map PARAMS ((Side *side, Map *map));
extern void draw_map_view PARAMS ((Side *side, Map *map));
extern void destroy_map PARAMS ((Side *side, Map *map));


extern void draw_row PARAMS ((Side *side, Map *map,
			     int x0, int y0, int len, int clearit));
extern void draw_unit_image PARAMS ((Side *side, Window win,
				    int sx, int sy, int sw, int sh,
				    int u, int s2, int fg, int bg));
extern void draw_side_emblem PARAMS ((Side *side, Window win,
				     int ex, int ey, int ew, int eh,
				     int s2, int style));
extern void draw_current PARAMS ((Side *side, Map *map));
extern void erase_current PARAMS ((Side *side, Map *map, int x, int y,
				  Unit *unit));

extern void draw_prompt PARAMS ((Side *side, Map *map));
extern void clear_prompt PARAMS ((Side *side, Map *map));
extern void draw_map_info PARAMS ((Side *side, Map *map));
extern void draw_game_state PARAMS ((Side *side, Map *map));
extern void draw_game_clocks PARAMS ((Side *side, Map *map));
extern void draw_map_sides PARAMS ((Side *side, Map *map));
extern void draw_side_info PARAMS ((Side *side, Map *map, Side *side2));
extern void draw_side_progress PARAMS ((Side *side, Map *map, Side *side2));

extern void put_on_screen PARAMS ((Side *side, Map *map, int x, int y));
extern int in_middle PARAMS ((Side *side, Map *map, int x, int y));

extern void move_the_selected_unit PARAMS ((Side *side, Map *map, Unit *unit,
					   int sx, int sy));

extern void create_help PARAMS ((Side *side));
extern void popup_help PARAMS ((Side *side));
extern void popdown_help PARAMS ((Side *side));

extern void recenter PARAMS ((Side *side, Map *map, int x, int y));

extern void draw_text PARAMS ((Side *side, Window win, int x, int y,
			      char *str,int color));
extern void draw_fg_text PARAMS ((Side *side, Window win, int x, int y,
				 char *str));

extern void textw_printf PARAMS ((const Widget w, const char *fmt, ...));

extern int font_width PARAMS ((XFontStruct *font));
extern int font_height PARAMS ((XFontStruct *font));

extern void close_display PARAMS ((Side *side));

extern void exit_xconq PARAMS ((Side *side));

extern void notify_all PARAMS ((char *fmt, ...));
extern void low_notify PARAMS ((Side *side, char *str));
extern void redraw PARAMS ((Side *side));
extern void flush_output PARAMS ((Side *side));
extern void beep PARAMS ((Side *side));

extern void flush_input PARAMS ((Side *side));

extern void build_name PARAMS ((char *name, char *first, char *second));

extern void draw_view_in_panner PARAMS ((Side *side, Map *map));

extern void set_message_area PARAMS ((Map *map, char *msg));

extern void add_map_actions PARAMS ((void));

extern void update_unit_type_list PARAMS ((Side *side, Map *map, int u));

extern XawTextPosition widget_text_length PARAMS ((Widget w));

extern void move_caret_to_end PARAMS ((Widget w));

extern int popup_print_setup_dialog PARAMS ((Side *side));

void place_legends PARAMS ((Side *side));


void enable_in_unit_type_list PARAMS ((Side *side, Map *map, int u, int flag));

extern int smallest_image PARAMS ((ImageFamily *imf, int *wp, int *hp));
extern void zoom_in_out PARAMS ((Side *side, Map *map, int which));

extern int find_side_and_map_via_ctrlpanel_form PARAMS ((Widget w,
							Side **sidep,
							Map **mapp));

extern int min_w_for_unit_image;
extern int min_h_for_unit_image;

extern time_t game_start_in_real_time;
extern time_t turn_play_start_in_real_time;

extern void handle_map_sides_events PARAMS ((Widget w, XtPointer clientdata,
					    XEvent *evt, Boolean *contdispatch));
extern void handle_map_info_events PARAMS ((Widget w, XtPointer clientdata,
					   XEvent *evt, Boolean *contdispatch));

extern void place_legends PARAMS ((Side *side));

extern void scroll_map_absolute PARAMS ((Side *side, Map *map, int sx, int sy));
extern void scroll_map_relative PARAMS ((Side *side, Map *map, int sx, int sy));

extern void draw_blast_image PARAMS ((Side *side, Map *map, int sx, int sy, int sw, int sh, int blasttype));
extern void invert_unit_subarea PARAMS ((Side *side, Map *map, int x, int y));

extern void update_help PARAMS ((Side *side));

/* Declarations of all the command functions. */

#undef DEF_CMD
#define DEF_CMD(letter,name,args,FN,help) \
  void FN PARAMS ((Side *side, Map *map));

#include "cmd.def"

#include "xcmd.def"

#ifdef DESIGNERS
extern void create_design PARAMS ((Side *side));
extern void popup_design PARAMS ((Side *side));
extern void update_curttype PARAMS ((Side *side));
extern void update_curutype PARAMS ((Side *side));
extern void update_cursidenumber PARAMS ((Side *side));
extern void update_curfeature PARAMS ((Side *side));
extern void update_curbrushradius PARAMS ((Side *side));
extern void set_designer_cur_from_map PARAMS ((Side *side, Map *map,
					      int sx, int sy));
extern void handle_designer_map_click PARAMS ((Side *side, Map *map,
					      int sx, int sy));
extern void popdown_design PARAMS ((Side *side));
#endif /* DESIGNERS */

extern UnitCloseup *find_unit_closeup PARAMS ((Unit *unit, Side *side));
extern UnitCloseup *find_unit_closeup_via_button PARAMS ((Widget w, Side *side));
extern UnitCloseup *create_unit_closeup PARAMS ((Unit *unit, Side *side, Map *map));
extern void draw_unit_closeup PARAMS ((UnitCloseup *unitcloseup, Side *side));
extern void destroy_unit_closeup PARAMS ((UnitCloseup *unitcloseup, Side *side));
extern SideCloseup *find_side_closeup PARAMS ((Side *side1, Side *side));
extern SideCloseup *find_side_closeup_via_button PARAMS ((Widget w, Side *side));
extern SideCloseup *create_side_closeup PARAMS ((Side *side1, Side *side, Map *map));
extern void draw_side_closeup PARAMS ((SideCloseup *sidecloseup, Side *side));
extern void destroy_side_closeup PARAMS ((SideCloseup *sidecloseup, Side *side));
extern Pixmap get_unit_picture PARAMS ((int utype, Side *side));
extern Pixmap get_side_picture PARAMS ((Side *side, Side *side1));
