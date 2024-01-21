#ifndef MODE_H
#define MODE_H
/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Parts Copyright (c) 1994 by Brian V. Smith
 * Parts Copyright (c) 1991 by Paul King
 *
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge, a
 * full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 * nonexclusive right and license to deal in this software and
 * documentation files (the "Software"), including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software subject to the restriction stated
 * below, and to permit persons who receive copies from any such party to
 * do so, with the only requirement being that this copyright notice remain
 * intact.
 * This license includes without limitation a license to do the foregoing
 * actions under any patents of the party supplying this software to the 
 * X Consortium.
 *
 * Restriction: The GIF encoding routine "GIFencode" in f_wrgif.c may NOT
 * be included if xfig is to be sold, due to the patent held by Unisys Corp.
 * on the LZW compression algorithm.
 */

#define		F_NULL			0
#define	    FIRST_DRAW_MODE	    F_CIRCLE_BY_RAD
#define		F_CIRCLE_BY_RAD		1
#define		F_CIRCLE_BY_DIA		2
#define		F_ELLIPSE_BY_RAD	3
#define		F_ELLIPSE_BY_DIA	4
#define		F_CIRCULAR_ARC		5
#define		F_POLYLINE		6
#define		F_BOX			7
#define		F_POLYGON		8
#define		F_TEXT			9
#define		F_APPROX_SPLINE		10
#define		F_CLOSED_APPROX_SPLINE	11
#define		F_INTERP_SPLINE		12
#define		F_CLOSED_INTERP_SPLINE	13
#define		F_ARC_BOX		14
#define		F_REGPOLY		15
#define		F_PICOBJ		16
#define	    FIRST_EDIT_MODE	    F_GLUE
#define		F_GLUE			30
#define		F_BREAK			31
#define		F_SCALE			32
#define		F_ADD			33
#define		F_COPY			34
#define		F_MOVE			35
#define		F_DELETE		36
#define		F_MOVE_POINT		37
#define		F_DELETE_POINT		38
#define		F_ADD_POINT		39
#define		F_DELETE_ARROW_HEAD	40
#define		F_ADD_ARROW_HEAD	41
#define		F_FLIP			42
#define		F_ROTATE		43
#define		F_AUTOARROW		44
#define		F_CONVERT		45
#define		F_CHANGE		46
#define		F_UPDATE		47
#define		F_ALIGN			48
#define		F_ZOOM			49
#define		F_LOAD			50
#define		F_EDIT			50
#define		F_ENTER_COMP		51
#define		F_EXIT_COMP		52
#define		F_EXIT_ALL_COMP		53
#define		F_OPEN_CLOSE		54

extern int	cur_mode;

/* alignment mode */
#define		ALIGN_NONE		0
#define		ALIGN_LEFT		1
#define		ALIGN_TOP		1
#define		ALIGN_CENTER		2
#define		ALIGN_RIGHT		3
#define		ALIGN_BOTTOM		3
#define		ALIGN_DISTRIB_C		4
#define		ALIGN_DISTRIB_E		5
#define		ALIGN_ABUT		6

extern int	cur_halign;
extern int	cur_valign;

/* angle geometry */
#define		L_UNCONSTRAINED		0
#define		L_LATEXLINE		1
#define		L_LATEXARROW		2
#define		L_MOUNTHATTAN		3
#define		L_MANHATTAN		4
#define		L_MOUNTAIN		5

extern int	manhattan_mode;
extern int	mountain_mode;
extern int	latexline_mode;
extern int	latexarrow_mode;

/* arrow mode */
#define		L_NOARROWS		0
#define		L_FARROWS		1
#define		L_FBARROWS		2
#define		L_BARROWS		3

extern int	autoforwardarrow_mode;
extern int	autobackwardarrow_mode;

/* grid mode */
#define		GRID_0			0
#define		GRID_1			1
#define		GRID_2			2
#define		GRID_3			3

extern int	cur_gridmode;
extern int	grid_fine[];
extern int	grid_coarse[];
extern char    *grid_name[];

/* point position */
#define		P_ANY			0
#define		P_MAGNET		1
#define		P_GRID1			2
#define		P_GRID2			3
#define		P_GRID3			4

extern int	cur_pointposn;
extern int	posn_rnd[];
extern int	posn_hlf[];

/* rotn axis */
#define		UD_FLIP			1
#define		LR_FLIP			2

extern int	cur_rotnangle;

/* smart link mode */
#define		SMART_OFF		0
#define		SMART_MOVE		1
#define		SMART_SLIDE		2

extern int	cur_linkmode;

/* misc */
extern int	action_on;
extern int	highlighting;
extern int	aborting;
extern int	anypointposn;
extern int	figure_modified;
extern int	cur_numsides;
extern int	cur_numcopies;
extern int	cur_numxcopies;
extern int	cur_numycopies;
extern char	cur_fig_units[32];
extern Boolean	warnexist;
extern Boolean  warninput;

extern void	reset_modifiedflag();
extern void	set_modifiedflag();
extern void	reset_action_on();
extern void	set_action_on();

/**********************	 global mode variables	************************/

extern int	num_point;
extern int	min_num_points;

/***************************  Export Settings  ****************************/

extern Boolean	export_flushleft;	/* flush left (true) or center (false) */

/* position of languages starting from 0 */
enum {
	LANG_BOX,
	LANG_LATEX,
	LANG_EPIC,
	LANG_EEPIC,
	LANG_EEPICEMU,
	LANG_PICTEX,
	LANG_IBMGL,
	LANG_EPS,
	LANG_PS,
	LANG_PSTEX,
	LANG_PXTEX_T,
	LANG_TEXTYL,
	LANG_TPIC,
	LANG_PIC,
	LANG_MF,

/* the bitmap formats follow LANG_MF (METAFONT) */

	LANG_PCX,
#ifdef USE_GIF
	LANG_GIF,
#endif
#ifdef USE_JPEG
	LANG_JPEG,
#endif
	LANG_XBM,
#ifdef USE_XPM
	LANG_XPM,
#endif
	END_OF_LANGS
};

/* IMPORTANT: Bitmap formats (e.g. GIF, JPEG) must follow BITMAP_FORMAT */

#define	BITMAP_FORMAT	LANG_MF+1

/* number of export languages */

#define NUM_EXP_LANG	END_OF_LANGS

extern int	cur_exp_lang;
extern char    *lang_items[NUM_EXP_LANG];
extern char    *lang_texts[NUM_EXP_LANG];
extern Boolean  batch_exists;
extern char     batch_file[];

/***************************  Mode Settings  ****************************/

extern int	cur_objmask;
extern int	cur_updatemask;
extern int	cur_depth;

/***************************  Text Settings  ****************************/

extern int	hidden_text_length;
extern float	cur_textstep;
extern int	cur_fontsize;
extern int	cur_latex_font;
extern int	cur_ps_font;
extern int	cur_textjust;
extern int	cur_textflags;

/***************************  Lines ****************************/

extern int	cur_linewidth;
extern int	cur_linestyle;
extern int	cur_joinstyle;
extern int	cur_capstyle;
extern float	cur_dashlength;
extern float	cur_dotgap;
extern float	cur_styleval;
extern Color	cur_pencolor;
extern Color	cur_fillcolor;
extern int	cur_boxradius;
extern int	cur_fillstyle;
extern int	cur_penstyle;
extern int	cur_arrowmode;
extern int	cur_arrowtype;
extern int	cur_arctype;
extern char	EMPTY_PIC[8];

/* Misc */
extern float	cur_elltextangle;	/* text/ellipse input angle */


/***************************  File Settings  ****************************/

extern char	cur_dir[];
extern char	cur_filename[];
extern char	save_filename[];	/* to undo load or "new" command */
extern char	file_header[];
extern char	cut_buf_name[];
#endif /* MODE_H */
