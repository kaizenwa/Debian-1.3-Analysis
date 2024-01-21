/* Definitions for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* All Mac compilers are close enough to ANSI to include these,
   even though some don't define __STDC__. */

#include <stddef.h>
#include <stdarg.h>
#include <stdlib.h>

#ifdef THINK_C
#include <MacHeaders>
#include <Sound.h>
#endif /* THINK_C */

#ifdef MPW
#ifndef __MWERKS__ /* actually, "old headers" */
#include <Values.h>
#endif
#include <Types.h>
#include <Resources.h>
#include <QuickDraw.h>
#include <Fonts.h>
#include <Events.h>
#include <Windows.h>
#include <Menus.h>
#include <TextEdit.h>
#include <Dialogs.h>
#include <Desk.h>
#include <Scrap.h>
#include <ToolUtils.h>
#include <Memory.h>
#include <SegLoad.h>
#include <Files.h>
#include <Folders.h>
#include <OSUtils.h>
#include <OSEvents.h>
#include <DiskInit.h>
#include <Packages.h>
#include <Traps.h>
#include <Lists.h>
#include <StandardFile.h>
#include <Sound.h>
#include <Devices.h>
#endif /* MPW */

#include <GestaltEqu.h>
#include <PPCToolbox.h>
#include <AppleEvents.h>

/* Bring in image- and interface-related definitions. */

#include "imf.h"
#include "macimf.h"
#include "ui.h"

/* Bring in the definitions of resources. */

#include "macdefs.h"

#ifdef MPW_C
#define QD(whatever) (qd.##whatever)
#define QDPat(whatever) (&(qd.##whatever))
#endif
#ifdef THINK_C
#define QD(whatever) (whatever)
#define QDPat(whatever) (whatever)
#endif
#ifdef __MWERKS__
#define QD(whatever) (qd.##whatever)
#define QDPat(whatever) (&(qd.##whatever))
#endif

#ifdef MPW_C
/* dangerous way */
#define SET_PAT_ELT(pattern,i,val) ((pattern)[(i)] = (val))
#define PAT_ELT(pattern,i) ((pattern)[(i)])
#endif

#ifdef THINK_C
/* dangerous way */
#define SET_PAT_ELT(pattern,i,val) ((pattern)[(i)] = (val))
#define PAT_ELT(pattern,i) ((pattern)[(i)])
#endif

#ifdef __MWERKS__
#define SET_PAT_ELT(pattern,i,val) ((pattern).pat[(i)] = (val))
#define PAT_ELT(pattern,i) ((pattern).pat[(i)])
#endif

#ifdef THINK_C
/* From MPW, to replace missing definitions in Think C includes. */
enum {

 kOnSystemDisk = /* 0x8000 */ -32768


#define kCreateFolder true
#define kDontCreateFolder false

#define kSystemFolderType				'macs'		/* the system folder */
#define kDesktopFolderType				'desk'		/* the desktop folder; objects in this folder show on the desk top. */
#define kTrashFolderType				'trsh'		/* the trash folder; objects in this folder show up in the trash */
#define kWhereToEmptyTrashFolderType	'empt'		/* the "empty trash" folder; Finder starts empty from here down */

#define kPrintMonitorDocsFolderType		'prnt'		/* Print Monitor documents */

#define kStartupFolderType				'strt'		/* Finder objects (applications, documents, DAs, aliases, to...) to open at startup go here */
#define kAppleMenuFolderType			'amnu'		/* Finder objects to put into the Apple menu go here */
#define kControlPanelFolderType 		'ctrl'		/* Control Panels go here (may contain INITs) */
#define kExtensionFolderType			'extn'		/* Finder extensions go here */
#define	kFontsFolderType				'font'		/* Fonts go here */
#define kPreferencesFolderType			'pref'		/* preferences for applications go here */
#define kTemporaryFolderType			'temp'		/* temporary files go here (deleted periodically, but don't rely on it.) */
};

pascal OSErr FindFolder(short vRefNum,OSType folderType,Boolean createFolder,
 short *foundVRefNum,long *foundDirID)
 = {0x7000,0xA823}; 
#endif

/* This is a way - perhaps not the best way - to distinguish "old headers"
   from "universal headers". */

#ifndef __CONDITIONALMACROS__
#define SetMenuItemText SetItem
#define InsertMenuItem InsMenuItem
#define SndListHandle Handle
#define AEEventHandlerUPP EventHandlerProcPtr
#define ModalFilterUPP ModalFilterProcPtr
typedef pascal void (*UserItemProcPtr)(WindowPtr theWindow, short itemNo);
#define UserItemUPP UserItemProcPtr
#define ControlActionUPP ProcPtr
#endif

#ifndef NewAEEventHandlerProc
#define NewAEEventHandlerProc(fn) ((EventHandlerProcPtr) (fn))
#endif
#ifndef NewModalFilterProc
#define NewModalFilterProc(fn) ((ModalFilterProcPtr) (fn))
#endif
#ifndef NewUserItemProc
#define NewUserItemProc(fn) ((UserItemProcPtr) (fn))
#endif
#ifndef NewControlActionProc
#define NewControlActionProc(fn) ((ProcPtr) (fn))
#endif

enum grays {
	blackgray,
	darkgray,
	mediumgray,
	lightgray,
	whitegray,
	fullcolor
};

/* The types of available designer tools. */

enum tooltype {
	notool,
	terraintool,
	unittool,
	peopletool,
	featuretool,
	brushsizetool,

	materialtool,
	elevationtool,
	temperaturetool,
	cloudstool,
	windstool,
	viewtool
};

/* The user interface substructure.  This is only allocated for sides with
   attached displays. */

typedef struct a_ui {
	short followaction;	   /* move to where a change has occured */
	int itertime;
	int active;
} UI;

/* Each side can open up any number and shape of maps. */

#define MAXSELECTIONS 500

typedef struct a_map {
	VP *vp;                 /* This map's generic view parameters */
	short osx, osy;         /* Tracks shifted origin so can be restored */
	WindowPtr window;
	ControlHandle hscrollbar;
	ControlHandle vscrollbar;
	/* How to draw the map. */
	short conw;
	short toplineh;
	short topunith;
	short toph;
	short drawterrain;		/* Display solid color terrain on the map? */
	short auxterraintypes[MAXTTYPES];
	short drawauxterrain[MAXTTYPES];
	short drawgrid;		    /* Draw outlines around cells? */
	short drawcellpats;		/* Draw terrain patterns? */
	short drawothermaps;	/* Draw outlines of other maps? */
	short drawunits;		/* Draw units on the map? */
	short drawnames;		/* Draw unit names/numbers on the map? */
	short oldesttoshow;	    /* the relative time of the oldest data */
	short agetofade;		/* point at which to gray out the display */
	short newesttoshow;	    /* the relative time of the newest data */
	short fadeterrain;		/* fade world features as well as units? */
	short lowestaltitude;   /* the lowest altitude of units to display */
	short highestaltitude;	/* the highest altitude of units to display */
	short drawborders;
	short drawconnections;
	short drawpeople;		/* Draw people sides on the map? */
	short drawelevations;	/* Draw elevations on the map? */
	short drawmaterials[MAXMTYPES];
	short nummaterialstodraw;
	short drawlighting;		/* Draw day/night on the map? */
	short drawtemperature;	/* Draw temperatures on the map? */
	short drawwinds;
	short drawclouds;
	short drawstorms;
	short drawplans;
	short drawai;
	short drawcover;
	short autoselect;
	Unit *curunit;
	short moveonclick;
	short scrolltocurunit;
	short follow_action;	/* Scroll to scene of actions? */
	int numselections;
	int maxselections;
	Unit **selections;
	Rect contentrect;
	RgnHandle cellrgn;
	int cellrgnx, cellrgny;
	int maxdepth;           /* Largest # of bits/pixel of screens this map uses */
	struct a_map *next;		/* Link to the next map. */
} Map;

/* Each side can open up any number of lists of units. */

#define MAXINLIST 500

typedef struct a_list {
    SideMask sides;
	int mainsortmi;
	int listglimpsed;              /* list glimpsed units also? */
	enum sortkeys sortkeys[MAXSORTKEYS];  /* attributes to sort list elements on */
	UnitVector *contents;
	int numunits;
	int firstvisible;
	int lastvisible;
	int firstvisfield;
	int lastvisfield;
	int largeicons;
	int shouldreorg;
	int entryspacing;
	WindowPtr window;
	ControlHandle hscrollbar;
	ControlHandle vscrollbar;
	struct a_list *next;
} List;

/* A closer look at a unit. */

typedef struct a_unit_closeup {
	struct a_unit *unit;
	WindowPtr window;
	struct a_unit_closeup *next;
} UnitCloseup;

/* Iteration over all of a side's maps. */

#define for_all_maps(m)  \
  for ((m) = maplist; (m) != NULL; (m) = (m)->next)

/* Iteration over all of a side's lists. */

#define for_all_lists(l)  \
  for ((l) = listlist; (l) != NULL; (l) = (l)->next)

/* Iteration over all of a side's unit closeups. */

#define for_all_unit_closeups(c)  \
  for ((c) = unitcloseuplist; (c) != NULL; (c) = (c)->next)

/* Other useful macros. */

#define window_width(w) ((w)->portRect.right - (w)->portRect.left)
#define window_height(w) ((w)->portRect.bottom - (w)->portRect.top)

#define clip_to_limits(lo,x,hi) (max((lo), min((x), (hi))))

#define bords_to_draw(m) (numbordtypes > 0 && bwid[(m)->vp->power] > 0)

#define conns_to_draw(m) (numconntypes > 0 && cwid[(m)->vp->power] > 0)

#define draw_any_materials(m) (0)

#define any_borders_at(x, y, b) (aux_terrain_at(x, y, b) != 0)

#define any_connections_at(x, y, c) (aux_terrain_at(x, y, c) != 0)

enum {
	dontdraw,
	useblocks,
	usepictures,
	usepolygons
};

enum {
	plain_emblem,
	shadow_emblem
};

enum {
	NO_MODAL,
	MOVE_TO_MODAL,
	FIRE_MODAL,
	FIRE_INTO_MODAL,
	SET_FORMATION_MODAL,
	ADD_TERRAIN_MODAL,
	REMOVE_TERRAIN_MODAL,
	DISTANCE_MODAL,
	ZOOM_MODAL,
	GENERIC_MODAL
};

#ifndef c2p
#define c2p(STR,PBUF) \
  strcpy(((char *) PBUF) + 1, STR);  \
  PBUF[0] = strlen(STR);
#endif

#ifndef p2c
#define p2c(PSTR,BUF)  \
  strncpy(BUF, ((char *) (PSTR) + 1), PSTR[0]);  \
  BUF[PSTR[0]] = '\0';
#endif

#define top_left(rect) (*(Point *) &(rect.top))

#define bottom_right(rect) (*(Point *) &(rect.bottom))

/* The following macros require local vars named win, itemtype, itemhandle, itemrect. */

#define set_flag_from_ditem(di,place)  \
  GetDItem(win, (di), &itemtype, &itemhandle, &itemrect);  \
  (place) = GetCtlValue((ControlHandle) itemhandle); 

#define put_number_into_ditem(di,num)  \
  GetDItem(win, (di), &itemtype, &itemhandle, &itemrect);  \
  NumToString((num), tmpstr);  \
  SetIText(itemhandle, tmpstr);

#define get_number_from_ditem(di,place)  \
  GetDItem(win, (di), &itemtype, &itemhandle, &itemrect);  \
  GetIText(itemhandle, tmpstr);  \
  StringToNum(tmpstr, (long *) &(place));

/* (should understand nn:mm times) */

#define put_time_into_ditem(di,num)  \
  GetDItem(win, (di), &itemtype, &itemhandle, &itemrect);  \
  NumToString((num), tmpstr);  \
  SetIText(itemhandle, tmpstr);

#define get_time_from_ditem(di,place)  \
  GetDItem(win, (di), &itemtype, &itemhandle, &itemrect);  \
  GetIText(itemhandle, tmpstr);  \
  StringToNum(tmpstr, (long *) &(place));

#define get_string_from_ditem(di,buf)  \
  GetDItem(win, (di), &itemtype, &itemhandle, &itemrect);  \
  GetIText(itemhandle, tmpstr);  \
  p2c(tmpstr, (buf));

extern Side *dside;

extern int playsounds;

extern int nummaps;

extern ImageFamily **uimages;
extern ImageFamily **timages;
extern ImageFamily **eimages;

extern ImageColor **tcolors;

extern WindowPtr helpwin;
extern struct a_helpnode *curhelpnode;
extern char *helpstring;
extern TEHandle helptext;

extern WindowPtr instructionswin;

extern WindowPtr gamewin;
extern ControlHandle gamevscrollbar;

extern WindowPtr constructionwin;

extern WindowPtr designwin;
extern enum tooltype tooltype;
extern short curutype;
extern short curttype;
extern short cursidenumber;
extern short curfid;
extern Feature *curfeature;

extern WindowPtr historywin;

extern WindowPtr noticewin;
extern TEHandle noticetext;

extern WindowPtr scoreswin;
extern TEHandle scorestext;

extern MenuHandle sidemenu;
extern MenuHandle utypemenu;
extern MenuHandle mtypemenu;
extern MenuHandle ttypemenu;

extern int hasColorQD;

extern int minscreendepth;
extern int maxscreendepth;

extern int sbarwid;

extern WindowPtr *winmenuwins;

extern int default_draw_grid;		   /* Display grid on the map? */
extern int default_draw_names;		   /* Display unit names/numbers on the map? */

extern int map_modal;

extern int defaultmoveonclick;
extern int defaultautoselect;

extern PolyHandle polygons[];
extern int lastpolyx[], lastpolyy[];
extern PolyHandle gridpolygons[];
extern int lastgridpolyx[], lastgridpolyy[];

extern RgnHandle cellrgns[];
extern int lastcellrgnx[], lastcellrgny[];
extern RgnHandle gridcellrgns[];
extern int lastgridcellrgnx[], lastgridcellrgny[];

extern RgnHandle cellrgns30[];
extern int lastcellrgnx30[], lastcellrgny30[];
extern RgnHandle gridcellrgns30[];
extern int lastgridcellrgnx30[], lastgridcellrgny30[];

extern RgnHandle cellrgns15[];
extern int lastcellrgnx15[], lastcellrgny15[];
extern RgnHandle gridcellrgns15[];
extern int lastgridcellrgnx15[], lastgridcellrgny15[];

extern struct a_map *maplist;	   /* chain of maps that we're using */
extern struct a_list *listlist;	 /* chain of lists */
extern struct a_unit_closeup *unitcloseuplist;	 /* chain of unit closeups */

extern CursHandle paintcursors[];

extern CursHandle cellpaintor;
extern CursHandle bordpaintor;
extern CursHandle connpaintor;
extern CursHandle unitpaintor;
extern CursHandle peoplepaintor;
extern CursHandle featurepaintor;
extern CursHandle materialpaintor;
extern CursHandle elevpaintor;
extern CursHandle cloudpaintor;
extern CursHandle temppaintor;

extern CursHandle movecursors[];
extern CursHandle nomovecursor;
extern CursHandle allmovecursor;
extern CursHandle grayarrowcursor;
extern CursHandle opencrosscursor;
extern CursHandle watchcursor;
extern CursHandle firecursor;

extern char *curdatestr;

extern RGBColor graycolor, blackcolor;

extern enum grays gridgray;
extern enum grays unseengray;
extern enum grays bggray;

extern RGBColor gridcolor;
extern RGBColor unseencolor;
extern RGBColor blackcolor;

extern int conwid;
extern int tophgt;

extern int first_windows;

extern int daynight;
extern int grid_matches_unseen;

extern BitMap *bordbitmaps;
extern BitMap *connbitmaps;

extern Pattern *animation_patterns;

extern int animation_pattern_state;

extern WindowPtr playersetupwin;

extern int connection_method;
extern int hosting;
extern Rect dragrect;
extern int foundimagesfile;

extern int Profile;

extern TEHandle command_text;

/* Function declarations. */

/* Note that although the Xconq kernel must conditionalize its prototypes,
   all Mac C compilers are sufficiently standard C to handle them. */

extern int main(void);
extern int splash_dialog(void);
extern void connect_game_dialog(void);
extern void init_toolbox(void);
extern void recalc_depths(void);
extern void init_rects(void);
extern void init_ae(void);
extern void get_files(void);
extern int open_preferences(void);
extern void close_preferences(void);
extern void get_preferences(void);
extern void save_preferences(void);
extern void ui_update_state(void);
extern void event_loop(void);
extern void get_global_mouse(Point *mouse);
extern void adjust_cursor(Point mouse, RgnHandle region);
extern void grow_window(WindowPtr win, Point where);
extern void zoom_window(WindowPtr win, Point where, int part);
extern void close_window(WindowPtr win);
extern void do_mouse_down(WindowPtr window, EventRecord *event);
extern void activate_window(WindowPtr win, int activate);
extern void update_window(WindowPtr win);
extern void maybe_select_next_unit(void);

extern int is_da_window(WindowPtr win);
extern int is_app_window(WindowPtr win);

extern void won_game_dialog(void);
extern void lost_game_dialog(void);
extern void game_over_dialog(void);

extern int position_on_screen(int h, int v);
extern int position_already_used(int h, int v);
extern GDHandle best_zoom_screen(Rect *rectptr);

extern void force_update(WindowPtr win);
extern void force_overall_update(void);

extern void set_standard_state(WindowPtr win, int fullw, int fullh);
extern void get_main_screen_size(int *widp, int *hgtp);

extern void draw_default_button(DialogPtr dialog, short ditem);

extern void stagger_window(WindowPtr win, int *lasthp, int *lastvp);

extern void set_end_of_game_interaction_modes(void);
extern void draw_selection_animation(Map *map, Unit *unit);

/* Map-handling prototypes. */

extern int at_all_visible(Map *map, int x, int y);
extern int in_middle(Map *map, int x, int y);

extern void xform(Map *map, int x, int y, int *sxp, int *syp);
extern void m_xform_unit(Map *map, Unit *unit, int *sxp, int *syp, int *swp, int *shp);
extern void m_xform_unit_self(Map *map, Unit *unit, int *sxp, int *syp, int *swp, int *shp);
extern void m_xform_occupant(Map *map, Unit *transport, Unit *unit, int sx, int sy, int sw, int sh, int *sxp, int *syp, int *swp, int *shp);

extern Map *create_map(int power);
extern void set_content_rect(Map *map);
extern void m_focus_on_center(Map *map);
extern void m_center_on_focus(Map *map);
extern void set_map_scrollbars(Map *map);
extern void set_map_power(Map *map, int power);
extern void make_cell_clip(int power);
extern Map *map_from_window(WindowPtr window);

extern void grow_map(Map *map, int w, int h);
extern void zoom_map(Map *map, int part);

extern void adjust_map_decor(Map *map);

extern int m_nearest_cell(Map *map, int sx, int sy, int *xp, int *yp);
extern int m_nearest_boundary(Map *map, int sx, int sy, int *xp, int *yp, int *dirp);
extern int m_nearest_unit(Map *map, int sx, int sy, Unit **unitp);

extern void draw_map(Map *map);
extern void draw_window_background(Map *map);
extern void draw_map_content(Map *map);
extern void draw_area_background(Map *map);
extern void draw_control_panel(Map *map);
extern void draw_top_line(Map *map);
extern void draw_other_maps(Map *map);
extern void draw_related_maps(Map *map);
extern void draw_other_map(Map *map, Map *map2);
extern void draw_row(Map *map, int x0, int y0, int len, int clearit);
extern int cell_update(Map *map, int x, int y);
extern void draw_terrain_row(Map *map, int x0, int y0, int len);
extern void draw_legend(Map *map, int x, int y);

extern void draw_selections(Map *map);
extern void draw_selections_at(Map *map, int x, int y);
extern void draw_selected_unit_setport(Map *map, Unit *unit);
extern void draw_selected_unit(Map *map, Unit *unit);
extern void erase_selections(Map *map);
extern void erase_selection(Map *map, Unit *unit);
extern void draw_unselected_unit(Map *map, Unit *unit);

extern void force_map_update(Map *map);
extern void destroy_map(Map *map);
extern void activate_map(Map *map, int activate);
extern void print_map(Map *map);
extern void oneliner(Map *map, int sx, int sy);

extern void draw_unit_image(WindowPtr win, int sx, int sy, int sw, int sh, int u, int e, int mod);
extern void draw_side_emblem(WindowPtr win, int ex, int ey, int ew, int eh, int e, int style);
extern void draw_cell_block(int sx, int sy, int n, int power, int t, int t2, int angle);
extern void calc_best_terrain_images(void);
extern void draw_hex_region(int sx, int sy, int power, int dogrid, int t, int t2, int angle);
extern void draw_border_line_multiple(WindowPtr win, int sx, int sy, int bitmask, int power, int t, int angle);
extern void draw_connection_line_multiple(WindowPtr win, int sx, int sy, int bitmask, int power, int t, int angle);
extern void draw_terrain_sample(WindowPtr win, Rect tmprect, int t);
extern void draw_country_borders(WindowPtr win, int sx, int sy, int bitmask, int power, int shade, int angle);
extern void draw_ai_region_borders(WindowPtr win, int sx, int sy, int bitmask, int power);
extern int draw_elevation_here(int x, int y);
extern void draw_elevation(int sx, int sy, int power, int elev);
extern int draw_temperature_here(int x, int y);
extern void draw_temperature(int sx, int sy, int power, int temp);
extern int draw_winds_here(int x, int y);
extern void draw_winds(int sx, int sy, int power, int rawwind);
extern int draw_clouds_here(int x, int y);
extern void draw_clouds(int sx, int sy, int power, int cloudtype);
extern void draw_coverage(int sx, int sy, int power, int cov, int altcov);
extern void draw_unit_name(Unit *unit, int sx, int sy, int sw, int sh);
extern void draw_legend_text(int sx, int sy, int sh, char *legend, int just);
extern void draw_blast_image(WindowPtr win, int sx, int sy, int sw, int sh, int blasttype);
extern void clear_blast_image(WindowPtr win, int sx, int sy, int sw, int sh, int blasttype);
extern int picture_width(PicHandle pichandle);
extern int picture_height(PicHandle pichandle);
extern void plot_sicn(WindowPtr win, int sx, int sy, Handle sicnhandle, int n, int erase, int mode);
extern void gray_out_rect(Rect *rectptr);
extern void draw_unit_blast(Map *map, Unit *unit, int blast);
extern void clear_unit_blast(Map *map, Unit *unit, int blast);

extern void do_mouse_down_map(Map *map, Point mouse, int mods);
extern void do_mouse_down_map_control_panel(Map *map, int h, int v, int mods);
extern void toggle_survey(Map *map);
extern void magnify_map(Map *map, int inout);
extern void set_map_mag(Map *map, int newpower);
extern void toggle_map_grid(Map *map);
extern void toggle_map_topline(Map *map);
extern void toggle_map_topunit(Map *map);
extern void toggle_map_other_maps(Map *map);
extern void toggle_map_lighting(Map *map);
extern void toggle_map_coverage(Map *map);
extern void toggle_map_names(Map *map);
extern void toggle_map_people(Map *map);
extern void toggle_map_elevations(Map *map);
extern void toggle_map_materials(Map *map, int m);
extern void toggle_map_aux_terrain(Map *map, int t);
extern void toggle_map_temperature(Map *map);
extern void toggle_map_winds(Map *map);
extern void toggle_map_clouds(Map *map);
extern void toggle_map_storms(Map *map);
extern void toggle_map_plans(Map *map);
extern void toggle_map_ai(Map *map);
extern void do_mouse_down_map_content(Map *map, int h, int v, int mods);
extern void select_all_dragged_over(Map *map, int h0, int v0, int mods);
extern void select_area_and_zoom(Map *map, int h0, int v0, int mods);
extern void move_on_drag(Map *map, Unit *unit, int mods);
extern void unselect_all(Map *map);
extern void select_unit_on_map(Map *map, Unit *unit);
extern int unit_is_selected(Map *map, Unit *unit);
extern void unselect_unit_on_map(Map *map, Unit *unit);
extern void select_all_units_in_rect(Map *map, Rect *rectptr);
extern int move_the_selected_unit(Map *map, Unit *unit, int h, int v);
extern void fire_the_selected_unit(Map *map, Unit *unit, int h, int v);
extern void select_exactly_one_unit(Map *map, Unit *unit);
extern void select_next_unit(Map *map);
extern void select_previous_unit(Map *map);
extern void select_next_actor(Map *map);
extern void select_previous_actor(Map *map);
extern void select_next_mover(Map *map);
extern void select_previous_mover(Map *map);
extern void select_next_awake_mover(Map *map);
extern void select_previous_awake_mover(Map *map);
extern void select_another(Map *map, Unit *(*fn)(Side *side, Unit *unit));
extern void scroll_best_map_to_unit(Unit *unit);
extern void scroll_to_unit(Map *map, Unit *unit);
extern void magnify_to_fit(Map *map, int x1, int y1, int x2, int y2);

extern void create_list(void);
extern void init_list_contents(List *list);
extern void organize_list_contents(List *list);
extern void sort_list_contents(List *list);
extern void add_unit_to_list(List *list, Unit *unit);
extern void set_list_scrollbars(List *list);
extern List *list_from_window(WindowPtr window);
extern void draw_list(List *list);
extern void draw_list_contents(List *list);
extern void draw_list_headings(List *list);
extern void draw_unit_list_entry(List *list, int n, int clearfirst);
extern void grow_list(List *list, int w, int h);
extern void zoom_list(List *list, int part);
extern void adjust_list_decor(List *list);
extern void do_mouse_down_list(List *list, Point mouse, int mods);
extern void set_list_sorting(List *list, enum sortkeys newkey, int mi);
extern void toggle_list_large_icons(List *list);
extern void update_unit_in_lists(Unit *unit);
extern int unit_position_in_list(List *list, Unit *unit);
extern void reorganize_list(List *list);
extern void redraw_unit_list_entry(List *list, int n);
extern void clear_selections(List *list);
extern Unit *selected_unit_in_list(List *list);
extern void scroll_to_selected_unit_in_list(List *list);
extern void activate_list(List *list, int activate);
extern void print_list(List *list);
extern void destroy_list(List *list);

extern void scroll_best_map_to_unit(Unit *unit);

extern void init_patterns(void);
extern void init_icons(void);
extern void init_cursors(void);
extern int do_splash_box(void);
extern void new_game_dialog(void);
extern int start_new_game(void);
extern void update_new_game_list(void);
extern int variants_dialog(void);
extern int open_game_dialog(void);
extern int open_game_from_fsspec(FSSpec *fsspec);
extern int open_game_from_name_and_volume(Str255 name, int vrefnum);
extern int launch_game(void);
extern void handle_player_setup_event(EventRecord *event);
extern int hit_player_setup_dialog(int itemhit, Point mouse);
extern void launch_game_2(void);
extern void add_remote_player(char *name);
extern void check_for_missing_images(void);
extern void open_progress_dialog(void);
extern void close_progress_dialog(void);
extern void init_display(void);
extern void init_terrain_images(void);
extern void interp_named_color(char *name, enum grays *grayvar, RGBColor *colorvar);
extern void init_terrain_colors(void);
extern void init_unit_images(void);
extern void init_all_emblem_images(void);
extern void init_emblem_images(Side *side2);

extern int construction_ever_possible(void);

extern void init_menus(void);
extern void add_window_menu_item(char *name, WindowPtr win);
extern void remove_window_menu_item(WindowPtr win);
extern void build_side_menu(void);
extern void update_side_menu(Side *side2);
extern void build_unit_type_menu(void);
extern void build_material_type_menu(void);
extern void build_terrain_type_menu(void);
extern void build_ai_type_menu(void);
extern void build_feature_menu(void);
extern void build_optional_terrain_type_menu(void);
extern void update_feature_menu(Feature *feature);
extern void do_menu_command(long which);
extern void do_about_box(void);
extern void save_the_game(int askname, int quitting);
extern void set_preferences(void);
extern void maybe_init_print(void);
extern void quit_the_game(void);
extern void apply_to_all_selected(int (*fn)(), int beepfailure);
extern void do_reserve_command(int value, int radius, int recurse);
extern void do_sleep_command(int value, int radius, int recurse);
extern void do_attack_command(void);
extern void enable_construction(void);
extern int do_move_to_command(void);
extern int do_fire_command(void);
extern int do_fire_into_command(void);
extern void do_profile(void);
extern void do_trace(void);
extern void do_detonate_command(void);
extern void adjust_menus(void);

extern int add_unit_position(Unit *unit);
extern void set_focus(Map *map, int x, int y);
extern void message_dialog(void);
extern void command_dialog(void);
extern void do_keyboard_command(int key);
extern void execute_named_command(char *cmdstr);
extern void describe_commands(int arg, char *key, char *buf);
struct cmdtab;
extern void describe_command_table(int arg, char *key, char *buf, struct cmdtab *cmdtab);
extern void toggle_profiling(void);

extern char *get_string_from_item(Handle itemhandle);

extern void instructions_dialog(void);
extern void create_instructions_dialog(void);
extern int hit_instructions_dialog(DialogPtr dialog, int itemhit, EventRecord *evt);

extern void create_game_window(void);
extern void draw_game(void);
extern void draw_game_date(void);
extern void draw_game_clocks(void);
extern void draw_game_progress(void);
extern void draw_game_side(Side *side2);
extern int feeling_towards(Side *side, Side *side2);
extern void draw_side_status(Side *side2);
extern void do_mouse_down_game(Point mouse, int mods);

extern void create_construction_window(void);
extern void init_construction_lists(void);
extern void reinit_construction_lists(void);
extern void draw_construction(void);
extern void draw_construction_default(void);
extern void calc_construction_rects(void);
extern void activate_construction(int activate);
extern Unit *get_selected_construction_unit(void);
extern int get_selected_construction_type(void);
extern void scroll_to_selected_construction_unit(void);
extern void do_mouse_down_construction(Point mouse, int mods);
extern void select_unit_in_construction_window(Unit *unit);
extern void select_type_in_construction_window(int u);
extern void update_construction_unit_list(Unit *unit);
extern void maybe_add_unit_to_construction_list(Unit *unit);
extern void update_unit_list_for_type(int u);
extern void update_construction_type_list(void);
extern void update_type_list_for_unit(Unit *unit);
extern void adjust_construction_controls(void);
extern void grow_construction(int h, int v);
extern void zoom_construction(int part);
extern int do_key_down_construction(int key);

extern void side_rename_dialog(Side *side);

extern int unit_rename_dialog(Unit *unit);

extern UnitCloseup *find_unit_closeup(Unit *unit);
extern void create_unit_closeup(Unit *unit);
extern void preferred_closeup_size(int u, int *widp, int *hgtp);
extern void draw_unit_closeup(UnitCloseup *unitcloseup);
extern UnitCloseup *unit_closeup_from_window(WindowPtr win);
extern int do_mouse_down_unit_closeup(UnitCloseup *unitcloseup, Point mouse, int mods);
extern void destroy_unit_closeup(UnitCloseup *unitcloseup);

extern void create_history_window(void);
extern void calc_history_layout(void);
extern void update_total_hist_lines(void);
extern void set_history_scrollbar(void);
extern void draw_history(void);
extern void draw_historical_event(HistEvent *hevt, int y);
extern void draw_historical_date(HistEvent *hevt, int y);
extern void do_mouse_down_history(Point mouse, int mods);
extern void move_history_scrollbar(int h, int v);
extern void grow_history(int h, int v);
extern void zoom_history(int part);
extern void update_history_window(HistEvent *hevt);

extern void notice_dialog(void);
extern void create_notice_window(void);
extern void append_notice(char *str);
extern void draw_notice(void);
extern void adjust_notice_scrollbar(void);
extern void activate_notice(int activate);
extern void do_mouse_down_notice(Point mouse, int mods);
extern void grow_notice(int h, int v);
extern void zoom_notice(int part);

extern void scores_dialog(void);
extern void create_scores_window(void);
extern void append_scores(char *str);
extern void draw_scores(void);
extern void adjust_scores_scrollbar(void);
extern void activate_scores(int activate);
extern void do_mouse_down_scores(Point mouse, int mods);
extern void grow_scores(int h, int v);
extern void zoom_scores(int part);

extern WindowPtr commandwin;
extern void draw_command(void);
extern void activate_command(int activate);
extern int do_key_down_command(int key);
extern void do_mouse_down_command(Point mouse, int mods);

extern void help_dialog(HelpNode *helpnode);
extern void describe_menus(int arg, char *key, char *buf);
extern void describe_mouse(int arg, char *key, char *buf);
extern void describe_keyboard(int arg, char *key, char *buf);
extern void describe_help(int arg, char *key, char *buf);
extern void create_help_window(void);
extern void set_help_content(HelpNode *helpnode);
extern void draw_help(void);
extern void adjust_help_scrollbar(void);
extern void activate_help(int activate);
extern void do_mouse_down_help(Point mouse, int mods);
extern void grow_help(int h, int v);
extern void zoom_help(int part);

extern void enable_designing(int forsure);
extern void disable_designing(void);
extern void init_design_cursors(void);
extern CursPtr adjust_designer_cursor(Point mouse, RgnHandle region);
extern void create_design_window(void);
extern void position_design_window(void);
extern void draw_design_window(void);
extern void draw_design_window_tool(enum tooltype tool);
extern void do_mouse_down_design(Point mouse, int mods);
extern void mark_allowed_unit_types(void);
extern void mark_allowed_sides(void);
extern void feature_rename_dialog(Feature *feature);
extern void apply_designer_tool(Map *map, int h, int v, int mods);
extern void paint_on_drag(Map *map, int h0, int v0, int mods);
extern void border_on_drag(Map *map, int h0, int v0, int mods, int paintmode);
extern void connect_on_drag(Map *map, int h0, int v0, int mods, int paintmode);
extern void designer_save_dialog(void);
extern void designer_reshape_dialog(Module *module);

extern void update_unit_in_maps(Unit *unit);
extern void beep(void);
extern void set_game_file_type(char *name);

extern int get_a_position(Map **map, int *xp, int *yp, int *dirp);
extern int get_a_unit(Map **mapp, Unit **unitp);

extern void interp_mac_ui_data(Obj *uispec);

extern void draw_unit_info(Map *map);
extern void connection_method_dialog(void);
extern void init_connection_method(void);

extern void play_sound(char *soundname);

extern void query_position_modally(int mode);
extern void set_position_modally(void);

extern int gameinited;
extern int position_set_modally;
extern Point modal_point;
extern Map *modal_map;
