/* Definitions for graphics support not specific to any interface.
   Copyright (C) 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#define NUMPOWERS 8

typedef struct a_vp {
    short sx, sy;		/* lower left corner of the viewport, in pixels */
    int   totsw, totsh;		/* total size of the map in pixels */
    short pxw, pxh;		/* size of window in pixels */
    short power;		/* index to this map's magnification */
    short mag;			/* magnification of each cell (a power of 2) */
    short vcx, vcy;		/* center of the view */
    short hw, hh;		/* pixel dims of a cell */
    short hch;			/* height of cell cell between centers */
    short uw, uh;		/* pixel dims of unit subcell */
    short angle;		/* Angle of perspective view. */
    short cellwidth;
    short vertscale;
} VP;

typedef struct a_legend {
    int ox, oy;			/* starting point */
    int dx, dy;			/* displacement */
    float angle, dist;		/* polar displacement (redundant, but useful) */
} Legend;

#define hexagon_adjust(v) (area.xwrap ? 0 : (area.halfheight * (v)->hw) / 2)

#define REDRAW_CHAR '\001'
#define BACKSPACE_CHAR '\010'
#define ESCAPE_CHAR '\033'
#define DELETE_CHAR 0x7f

extern short mags[], hws[], hhs[], hcs[], uws[], uhs[];

extern short bwid[], bwid2[], cwid[];

extern short bsx[NUMPOWERS][7], bsy[NUMPOWERS][7];
extern short lsx[NUMPOWERS][6], lsy[NUMPOWERS][6];

extern short qx[NUMPOWERS][7], qy[NUMPOWERS][7];

extern char *dirchars;

extern ImageFamily *unseen_image;

extern char *terrchars;
extern char *unitchars;
extern char unseen_char_1, unseen_char_2;

extern Module **possible_games;

extern int numgames;

/* Function declarations. */

extern void collect_possible_games PARAMS ((void));
extern void add_to_possible_games PARAMS ((Module *module));

extern void parse_long_name_command PARAMS ((char *cmdstr, char **namep, char **argp, char *buf));

extern VP *new_vp PARAMS ((void));

extern void xform_cell PARAMS ((VP *vp, int x, int y, int *sxp, int *syp));
extern void xform_unit PARAMS ((VP *vp, Unit *unit, int *sxp, int *syp, int *swp, int *shp));
extern void xform_unit_self PARAMS ((VP *vp, Unit *unit, int *sxp, int *syp, int *swp, int *shp));
extern void xform_occupant PARAMS ((VP *vp, Unit *transport, Unit *unit, int sx, int sy, int sw, int sh, int *sxp, int *syp, int *swp, int *shp));

extern void scale_vp PARAMS ((VP *vp, VP *vp2, int *sxp, int *syp, int *swp, int *shp));

extern int nearest_cell PARAMS ((VP *vp, int sx, int sy, int *xp, int *yp));
extern int nearest_boundary PARAMS ((VP *vp, int sx, int sy, int *xp, int *yp, int *dirp));
extern int nearest_unit PARAMS ((VP *vp, int sx, int sy, Unit **unitp));

extern int cell_is_visible PARAMS ((VP *vp, int x, int y));
extern int unit_is_visible PARAMS ((VP *vp, Unit *unit));
extern int cell_is_in_middle PARAMS ((VP *vp, int x, int y));

extern int set_view_size PARAMS ((VP *vp, int w, int h));
extern int set_view_position PARAMS ((VP *vp, int sx, int sy));
extern int set_view_power PARAMS ((VP *vp, int power));
extern int set_view_angle PARAMS ((VP *vp, int angle));
extern int set_view_direction PARAMS ((VP *vp, int dir));
extern int set_view_focus PARAMS ((VP *vp, int x, int y));
extern void center_on_focus PARAMS ((VP *vp));
extern void focus_on_center PARAMS ((VP *vp));

extern void compute_fire_line_segment PARAMS ((int sx1, int sy1, int sx2, int sy2,
					       int i, int n,
					       int *xx, int *yy, int *dx, int *dy));

extern void place_feature_legends
  PARAMS ((Legend *leg, int nf, Side *side, int orient, int block));

extern Unit *find_unit_or_occ PARAMS ((VP *vp, Unit *unit, int usx, int usy, int usw, int ush, int sx, int sy));
extern Unit *find_unit_at PARAMS ((VP *vp, int x, int y, int sx, int sy));

extern void pick_a_focus PARAMS ((Side *side, int *xp, int *yp));

extern Unit *autonext_unit PARAMS ((Side *side, Unit *unit));
extern int in_box PARAMS ((int x, int y, int lx, int ly, int w, int h));
extern Unit *autonext_unit_inbox PARAMS ((Side *side, Unit *unit, VP *vp));
extern int could_be_next_unit PARAMS ((Unit *unit));

extern Unit *find_next_occupant PARAMS ((Unit *unit));

extern int num_active_displays PARAMS ((void));

extern int char_to_dir PARAMS ((int ch, int *dir1p, int *dir2p, int *modp));
extern int advance_into_cell PARAMS ((Side *side, Unit *unit, int x, int y,
				Unit *other));

extern int give_supplies PARAMS ((Unit *unit, short *amts, short *rslts));
extern int take_supplies PARAMS ((Unit *unit, short *amts, short *rslts));

extern int disband_unit PARAMS ((Side *side, Unit *unit));
extern int favored_type PARAMS ((Unit *unit));

extern ImageFamily *get_unit_type_images PARAMS ((Side *side, int u,
						 void (*interp_hook)(ImageFamily *imf),
						 void (*load_hook)(ImageFamily *imf),
						 void (*default_hook)(ImageFamily *imf, int u, char *name)));
extern ImageFamily *get_terrain_type_images PARAMS ((Side *side, int t,
						    void (*interp_hook)(ImageFamily *imf),
						    void (*load_hook)(ImageFamily *imf),
						    void (*default_hook)(ImageFamily *imf, int u, char *name)));
extern ImageFamily *get_unseen_images PARAMS ((Side *side,
						    void (*interp_hook)(ImageFamily *imf),
						    void (*load_hook)(ImageFamily *imf),
						    void (*default_hook)(ImageFamily *imf, char *name)));
extern ImageFamily *get_emblem_images PARAMS ((Side *side, Side *side2,
					      void (*interp_hook)(ImageFamily *imf),
					      void (*load_hook)(ImageFamily *imf),
						  void (*default_hook)(ImageFamily *imf, int u, char *name)));

extern void record_imf_get PARAMS ((ImageFamily *imf));

extern void init_ui_chars PARAMS ((void));
extern void dump_text_view PARAMS ((Side *side, int use_both));

extern int terrain_seen_at PARAMS ((Side *side, int x, int y));
extern Unit *unit_seen_at PARAMS ((Side *side, int x, int y));
extern int utype_seen_at PARAMS ((Side *side, int x, int y));
