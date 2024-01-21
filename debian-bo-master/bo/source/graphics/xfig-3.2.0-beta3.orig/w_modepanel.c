/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1991 by Paul King
 * Parts Copyright (c) 1994 by Brian V. Smith
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

#include "fig.h"
#include "figx.h"
#include "resources.h"
#include "mode.h"
#include "object.h"
#include "paintop.h"
#include "w_drawprim.h"
#include "w_indpanel.h"
#include "w_util.h"
#include "w_mousefun.h"
#include "w_setup.h"
#include "d_spline.h"

/* from e_rotate.c */
extern int	setcenter;
extern int	setcenter_x;
extern int	setcenter_y;

/* from e_flip.c */
extern int	setanchor;
extern int	setanchor_x;
extern int	setanchor_y;

extern          finish_text_input();
extern          erase_objecthighlight();

extern          circlebyradius_drawing_selected();
extern          circlebydiameter_drawing_selected();
extern          ellipsebyradius_drawing_selected();
extern          ellipsebydiameter_drawing_selected();
extern          box_drawing_selected();
extern          arcbox_drawing_selected();
extern          line_drawing_selected();
extern          regpoly_drawing_selected();
extern          picobj_drawing_selected();
extern          text_drawing_selected();
extern          arc_drawing_selected();
extern          align_selected();
extern          compound_selected();
extern          open_compound();
extern          break_selected();
extern          scale_selected();
extern          point_adding_selected();
extern          delete_point_selected();
extern          move_selected();
extern          move_point_selected();
extern          delete_selected();
extern          copy_selected();
extern          rotate_cw_selected();
extern          rotate_ccw_selected();
extern          flip_ud_selected();
extern          flip_lr_selected();
extern          convert_selected();
extern          arrow_head_selected();
extern          edit_item_selected();
extern          update_selected();
static stub_circlebyradius_drawing_selected();
static stub_circlebydiameter_drawing_selected();
static stub_ellipsebyradius_drawing_selected();
static stub_ellipsebydiameter_drawing_selected();
static stub_box_drawing_selected();
static stub_arcbox_drawing_selected();
static stub_line_drawing_selected();
static stub_poly_drawing_selected();
static stub_regpoly_drawing_selected();
static stub_picobj_drawing_selected();
static stub_text_drawing_selected();
static stub_arc_drawing_selected();
static stub_spline_drawing_selected();
static stub_cl_spline_drawing_selected();
static stub_intspline_drawing_selected();
static stub_cl_intspline_drawing_selected();
static stub_align_selected();
static stub_compound_selected();
static stub_break_selected();
static stub_open_compound_selected();
static stub_scale_selected();
static stub_point_adding_selected();
static stub_delete_point_selected();
static stub_move_selected();
static stub_move_point_selected();
static stub_delete_selected();
static stub_copy_selected();
static stub_rotate_cw_selected();
static stub_rotate_ccw_selected();
static stub_flip_ud_selected();
static stub_flip_lr_selected();
static stub_convert_selected();
static stub_arrow_head_selected();
static stub_edit_item_selected();
static stub_update_selected();
static void stub_enter_mode_btn();

/**************	    local variables and routines   **************/

#define MAX_MODEMSG_LEN 80
typedef struct mode_switch_struct {
    icon_struct	   *icon;
    int		    mode;
    int		    (*setmode_func) ();
    int		    objmask;
    int		    indmask;
    char	    modemsg[MAX_MODEMSG_LEN];
    Widget	    widget;
    Pixmap	    pixmap, reversePM;
}               mode_sw_info;

#define		setmode_action(z)    (z->setmode_func)(z)

DeclareStaticArgs(13);
/* pointer to current mode switch */
static mode_sw_info *current = NULL;

/* button selection event handler */
static void     sel_mode_but();
static void     turn_on();

/* The M_XXX indicate which objects are selectable when the mode is on, and
   the I_XXX say which indicator buttons will appear on the indicator panel */

static mode_sw_info mode_switches[] = {
    {&cirrad_ic, F_CIRCLE_BY_RAD, circlebyradius_drawing_selected, M_NONE,
      I_CIRCLE, "CIRCLE drawing: specify RADIUS",},
    {&cirdia_ic, F_CIRCLE_BY_DIA, circlebydiameter_drawing_selected, M_NONE,
      I_CIRCLE, "CIRCLE drawing: specify DIAMETER",},
    {&ellrad_ic, F_ELLIPSE_BY_RAD, ellipsebyradius_drawing_selected, M_NONE,
      I_ELLIPSE, "ELLIPSE drawing: specify RADII",},
    {&elldia_ic, F_ELLIPSE_BY_DIA, ellipsebydiameter_drawing_selected, M_NONE,
      I_ELLIPSE, "ELLIPSE drawing: specify DIAMETERS",},
    {&c_spl_ic, F_CLOSED_APPROX_SPLINE, spline_drawing_selected, M_NONE,
      I_CLOSED, "CLOSED APPROXIMATED SPLINE drawing: specify control points",},
    {&spl_ic, F_APPROX_SPLINE, spline_drawing_selected, M_NONE,
      I_OPEN, "APPROXIMATED SPLINE drawing: specify control points",},
    {&c_intspl_ic, F_CLOSED_INTERP_SPLINE, spline_drawing_selected, M_NONE,
      I_CLOSED, "CLOSED INTERPOLATED SPLINE drawing: specify control points",},
    {&intspl_ic, F_INTERP_SPLINE, spline_drawing_selected, M_NONE,
      I_OPEN, "INTERPOLATED SPLINE drawing: specify control points",},
    {&polygon_ic, F_POLYGON, line_drawing_selected, M_NONE,
      I_CLOSED, "POLYGON drawing",},
    {&line_ic, F_POLYLINE, line_drawing_selected, M_NONE,
      I_LINE, "POLYLINE drawing",},
    {&box_ic, F_BOX, box_drawing_selected, M_NONE,
      I_BOX, "Rectangular BOX drawing",},
    {&arc_box_ic, F_ARC_BOX, arcbox_drawing_selected, M_NONE,
      I_ARCBOX, "Rectangular BOX drawing with ROUNDED CORNERS",},
    {&regpoly_ic, F_REGPOLY, regpoly_drawing_selected, M_NONE,
      I_REGPOLY, "Regular Polygon",},
    {&arc_ic, F_CIRCULAR_ARC, arc_drawing_selected, M_NONE,
      I_ARC, "ARC drawing: specify three points on the arc",},
    {&picobj_ic, F_PICOBJ, picobj_drawing_selected, M_NONE,
      I_PICOBJ, "Picture Object",},
    {&text_ic, F_TEXT, text_drawing_selected, M_TEXT_NORMAL,
      I_TEXT, "TEXT input (from keyboard)",},
    {&glue_ic, F_GLUE, compound_selected, M_ALL,
      I_MIN2, "GLUE objects into COMPOUND object",},
    {&break_ic, F_BREAK, break_selected, M_COMPOUND,
      I_MIN1, "BREAK COMPOUND object",},
    {&open_comp_ic, F_ENTER_COMP, open_compound, M_COMPOUND,
      I_MIN1, "OPEN COMPOUND object",},
    {&scale_ic, F_SCALE, scale_selected, M_NO_TEXT,
      I_MIN2, "SCALE objects",},
    {&align_ic, F_ALIGN, align_selected, M_COMPOUND,
      I_ALIGN, "ALIGN objects within a COMPOUND or to CANVAS",},
    {&movept_ic, F_MOVE_POINT, move_point_selected, M_NO_TEXT,
      I_ADDMOVPT, "MOVE POINTs",},
    {&move_ic, F_MOVE, move_selected, M_ALL,
      I_MIN3, "MOVE objects",},
    {&addpt_ic, F_ADD_POINT, point_adding_selected, M_VARPTS_OBJECT,
      I_ADDMOVPT, "ADD POINTs (to lines, polygons and splines)",},
    {&copy_ic, F_COPY, copy_selected, M_ALL,
      I_COPY, "COPY objects",},
    {&deletept_ic, F_DELETE_POINT, delete_point_selected, M_VARPTS_OBJECT,
      I_MIN1, "DELETE POINTs (from lines, polygons and splines)",},
    {&delete_ic, F_DELETE, delete_selected, M_ALL,
      I_MIN1, "DELETE objects",},
    {&update_ic, F_UPDATE, update_selected, M_ALL,
      I_OBJECT, "UPDATE object <-> current settings",},
    {&edit_ic, F_EDIT, edit_item_selected, M_ALL,
      I_MIN1, "CHANGE OBJECT via EDIT pane",},
    {&flip_x_ic, F_FLIP, flip_ud_selected, M_NO_TEXT,
      I_MIN2, "FLIP objects up or down",},
    {&flip_y_ic, F_FLIP, flip_lr_selected, M_NO_TEXT,
      I_MIN2, "FLIP objects left or right",},
    {&rotCW_ic, F_ROTATE, rotate_cw_selected, M_ALL,
      I_ROTATE, "ROTATE objects clockwise",},
    {&rotCCW_ic, F_ROTATE, rotate_ccw_selected, M_ALL,
      I_ROTATE, "ROTATE objects counter-clockwise",},
    {&convert_ic, F_CONVERT, convert_selected, M_VARPTS_OBJECT, 
      I_MIN1, "CONVERSION between lines, polygons and splines",},
    {&autoarrow_ic, F_AUTOARROW, arrow_head_selected, M_OPEN_OBJECT,
      I_MIN1 | I_LINEWIDTH | I_ARROWTYPE, "ADD/DELETE ARROWs",},
};

int	NUM_MODE_SW = (sizeof(mode_switches) / sizeof(mode_sw_info));

static Arg      button_args[] =
{
     /* 0 */ {XtNlabel, (XtArgVal) "    "},
     /* 1 */ {XtNwidth, (XtArgVal) 0},
     /* 2 */ {XtNheight, (XtArgVal) 0},
     /* 3 */ {XtNresizable, (XtArgVal) False},
     /* 4 */ {XtNborderWidth, (XtArgVal) 0},
     /* 5 */ {XtNresize, (XtArgVal) False},	/* keeps buttons from being
						 * resized when there are not
						 * a multiple of three of
						 * them */
     /* 6 */ {XtNbackgroundPixmap, (XtArgVal) NULL},
};

static void
stub_enter_mode_btn(widget, closure, event, continue_to_dispatch)
    Widget        widget;
    XtPointer     closure;
    XEvent       *event;
    Boolean      *continue_to_dispatch;
{
    draw_mousefun_mode();
}

static XtActionsRec mode_actions[] =
{
    {"EnterModeSw", (XtActionProc) stub_enter_mode_btn},
    {"LeaveModeSw", (XtActionProc) clear_mousefun},
    {"PressMiddle", (XtActionProc) notused_middle},
    {"ReleaseMiddle", (XtActionProc) clear_middle},
    {"PressRight", (XtActionProc) notused_right},
    {"ReleaseRight", (XtActionProc) clear_right},
    {"ModeCircleR", (XtActionProc) stub_circlebyradius_drawing_selected},
    {"ModeCircleD", (XtActionProc) stub_circlebydiameter_drawing_selected},
    {"ModeEllipseR", (XtActionProc) stub_ellipsebyradius_drawing_selected},
    {"ModeEllipseD", (XtActionProc) stub_ellipsebydiameter_drawing_selected},
    {"ModeBox", (XtActionProc) stub_box_drawing_selected},
    {"ModeArcBox", (XtActionProc) stub_arcbox_drawing_selected},
    {"ModeLine", (XtActionProc) stub_line_drawing_selected},
    {"ModePoly", (XtActionProc) stub_poly_drawing_selected},
    {"ModeRegPoly", (XtActionProc) stub_regpoly_drawing_selected},
    {"ModePIC", (XtActionProc) stub_picobj_drawing_selected},
    {"ModeText", (XtActionProc) stub_text_drawing_selected},
    {"ModeArc", (XtActionProc) stub_arc_drawing_selected},
    {"ModeSpline", (XtActionProc) stub_spline_drawing_selected},
    {"ModeClSpline", (XtActionProc) stub_cl_spline_drawing_selected},
    {"ModeIntSpline", (XtActionProc) stub_intspline_drawing_selected},
    {"ModeClIntSpline", (XtActionProc) stub_cl_intspline_drawing_selected},
    {"ModeAlign", (XtActionProc) stub_align_selected},
    {"ModeCompound", (XtActionProc) stub_compound_selected},
    {"ModeBreakCompound", (XtActionProc) stub_break_selected},
    {"ModeOpenCompound", (XtActionProc) stub_open_compound_selected},
    {"ModeScale", (XtActionProc) stub_scale_selected},
    {"ModeAddPoint", (XtActionProc) stub_point_adding_selected},
    {"ModeDeletePoint", (XtActionProc) stub_delete_point_selected},
    {"ModeMoveObject", (XtActionProc) stub_move_selected},
    {"ModeMovePoint", (XtActionProc) stub_move_point_selected},
    {"ModeDeleteObject", (XtActionProc) stub_delete_selected},
    {"ModeCopyObject", (XtActionProc) stub_copy_selected},
    {"ModeRotateObjectCW", (XtActionProc) stub_rotate_cw_selected},
    {"ModeRotateObjectCCW", (XtActionProc) stub_rotate_ccw_selected},
    {"ModeFlipObjectUD", (XtActionProc) stub_flip_ud_selected},
    {"ModeFlipObjectLR", (XtActionProc) stub_flip_lr_selected},
    {"ModeConvertObject", (XtActionProc) stub_convert_selected},
    {"ModeArrow", (XtActionProc) stub_arrow_head_selected},
    {"ModeEditObject", (XtActionProc) stub_edit_item_selected},
    {"ModeUpdateObject", (XtActionProc) stub_update_selected},
};

static String   mode_translations =
"<EnterWindow>:EnterModeSw()highlight()\n\
    <Btn1Down>:\n\
    <Btn1Up>:\n\
    <Btn2Down>:PressMiddle()\n\
    <Btn2Up>:ReleaseMiddle()\n\
    <Btn3Down>:PressRight()\n\
    <Btn3Up>:ReleaseRight()\n\
    <LeaveWindow>:LeaveModeSw()unhighlight()\n";

int
init_mode_panel(tool)
    Widget           tool;
{
    register int    i;
    register mode_sw_info *sw;

    FirstArg(XtNwidth, MODEPANEL_WD);
    NextArg(XtNhSpace, INTERNAL_BW);
    NextArg(XtNvSpace, INTERNAL_BW);
    NextArg(XtNtop, XtChainTop);
    NextArg(XtNbottom, XtChainTop);
    NextArg(XtNfromVert, msg_form);
    NextArg(XtNvertDistance, -INTERNAL_BW);
    NextArg(XtNleft, XtChainLeft);
    NextArg(XtNright, XtChainLeft);
    NextArg(XtNresizable, False);
    NextArg(XtNborderWidth, 0);
    NextArg(XtNmappedWhenManaged, False);

    mode_panel = XtCreateWidget("mode_panel", boxWidgetClass, tool,
				Args, ArgCount);

    XtAppAddActions(tool_app, mode_actions, XtNumber(mode_actions));

    for (i = 0; i < NUM_MODE_SW; ++i) {
	sw = &mode_switches[i];
	if (sw->mode == FIRST_DRAW_MODE) {
	    FirstArg(XtNwidth, MODE_SW_WD * SW_PER_ROW +
		     INTERNAL_BW * (SW_PER_ROW - 1));
	    NextArg(XtNborderWidth, 0);
	    NextArg(XtNresize, False);
	    NextArg(XtNheight, (MODEPANEL_SPACE + 1) / 2);
	    NextArg(XtNlabel, "Drawing\n modes");
	    d_label = XtCreateManagedWidget("label", labelWidgetClass,
					    mode_panel, Args, ArgCount);
	} else if (sw->mode == FIRST_EDIT_MODE) {
	    /* assume Args still set up from d_label */
	    ArgCount -= 2;
	    NextArg(XtNheight, (MODEPANEL_SPACE) / 2);
	    NextArg(XtNlabel, "Editing\n modes");
	    e_label = XtCreateManagedWidget("label", labelWidgetClass,
					    mode_panel, Args, ArgCount);
	}
	button_args[1].value = sw->icon->width;
	button_args[2].value = sw->icon->height;
	sw->widget = XtCreateManagedWidget("button", commandWidgetClass,
			    mode_panel, button_args, XtNumber(button_args));

	/* left button changes mode */
	XtAddEventHandler(sw->widget, ButtonPressMask, (Boolean) 0,
			  sel_mode_but, (XtPointer) sw);
	XtOverrideTranslations(sw->widget,
			       XtParseTranslationTable(mode_translations));
    }
    return;
}

/*
 * after panel widget is realized (in main) put some bitmaps etc. in it
 */

setup_mode_panel()
{
    register int    i;
    register mode_sw_info *msw;

    blank_gc = XCreateGC(tool_d, XtWindow(mode_panel), (unsigned long) 0, NULL);
    button_gc = XCreateGC(tool_d, XtWindow(mode_panel), (unsigned long) 0, NULL);
    FirstArg(XtNforeground, &but_fg);
    NextArg(XtNbackground, &but_bg);
    GetValues(mode_switches[0].widget);

    XSetBackground(tool_d, blank_gc, but_bg);
    XSetForeground(tool_d, blank_gc, but_bg);

    FirstArg(XtNfont, button_font);
    SetValues(d_label);
    SetValues(e_label);

    for (i = 0; i < NUM_MODE_SW; ++i) {
	msw = &mode_switches[i];
	/* create normal bitmaps */
	msw->pixmap = XCreatePixmapFromBitmapData(tool_d, XtWindow(msw->widget),
		       msw->icon->bits, msw->icon->width, msw->icon->height,
				   but_fg, but_bg, tool_dpth);

	FirstArg(XtNbackgroundPixmap, msw->pixmap);
	SetValues(msw->widget);

	/* create reverse bitmaps */
	msw->reversePM = XCreatePixmapFromBitmapData(tool_d, XtWindow(msw->widget),
		       msw->icon->bits, msw->icon->width, msw->icon->height,
				   but_bg, but_fg, tool_dpth);
    }

    XDefineCursor(tool_d, XtWindow(mode_panel), arrow_cursor);
    FirstArg(XtNmappedWhenManaged, True);
    SetValues(mode_panel);
}

/* come here when a button is pressed in the mode panel */

static void
sel_mode_but(widget, closure, event, continue_to_dispatch)
    Widget          widget;
    XtPointer	    closure;
    XEvent*	    event;
    Boolean*	    continue_to_dispatch;
{
    XButtonEvent    xbutton;
    mode_sw_info    *msw = (mode_sw_info *) closure;
    int             new_objmask;

    /* erase any existing anchor for flips */
    if (setanchor)
	center_marker(setanchor_x, setanchor_y);
    /* and any center for rotations */
    if (setcenter)
	center_marker(setcenter_x, setcenter_y);
    setcenter = 0;
    setanchor = 0;

    xbutton = event->xbutton;
    if (action_on) {
	if (cur_mode == F_TEXT)
	    finish_text_input();/* finish up any text input */
	else {
	    put_msg("Please finish (or cancel) the current operation before changing modes");
	    return;
	}
    } else if (highlighting)
	erase_objecthighlight();
    if (xbutton.button == Button1) {	/* left button */
	turn_off_current();
	turn_on(msw);
	if (msw->mode == F_UPDATE) {	/* map the set/clr/toggle button for update */
	    if (cur_mode != F_UPDATE) {
		update_indpanel(0);	/* first remove ind buttons */
		XtUnmanageChild(ind_panel);
		XtManageChild(upd_ctrl);
		/* get the width of the update control panel */
		if (UPD_CTRL_WD == 0) {
		    FirstArg(XtNwidth, &UPD_CTRL_WD);
		    GetValues(upd_ctrl);
		}
		/* now put the ind_panel to our right */
		FirstArg(XtNfromHoriz, upd_ctrl);
		NextArg(XtNwidth, INDPANEL_WD-UPD_CTRL_WD-2*INTERNAL_BW); /* resize it */
		SetValues(ind_panel);
		XtManageChild(ind_panel);
		update_indpanel(msw->indmask);	/* now manage the relevant buttons */
	    }
	} else { 	/* turn off the update boxes if not in update mode */
	    if (cur_mode == F_UPDATE) {	/* if previous mode is update and current */
		update_indpanel(0);	/* is not, first remove ind buttons */
		unmanage_update_buts();
		XtUnmanageChild(ind_panel);
		XtUnmanageChild(upd_ctrl);
		/* now put the ind_panel to the right of the canvas */
		FirstArg(XtNfromHoriz, NULL);
		NextArg(XtNwidth, INDPANEL_WD);	/* resize it */
		SetValues(ind_panel);
		XtManageChild(ind_panel);
		update_indpanel(msw->indmask);	/* now manage the relevant buttons */
	    } else {
		update_indpanel(msw->indmask);	/* just update indicator buttons */
	    }
	}
	put_msg(msw->modemsg);
	if ((cur_mode == F_GLUE || cur_mode == F_BREAK) &&
	    msw->mode != F_GLUE && msw->mode != F_BREAK) {
		/* reset tagged items when changing modes; perhaps this
		   is not really necessary */
		set_tags(&objects, 0);
	}
	cur_mode = msw->mode;
	anypointposn = !(msw->indmask & I_POINTPOSN);
	new_objmask = msw->objmask;
	if (cur_mode == F_ROTATE && cur_rotnangle != 90)
	    new_objmask = M_ROTATE_ANGLE;
	update_markers(new_objmask);
	current = msw;
	setmode_action(msw);
    }
}

void
force_positioning()
{
    update_indpanel(current->indmask | I_POINTPOSN);
    anypointposn = 0;
}

void
force_nopositioning()
{
    update_indpanel(current->indmask & ~I_POINTPOSN);
    anypointposn = 1;
}

void
force_anglegeom()
{
    update_indpanel(current->indmask | I_ANGLEGEOM);
}

void
force_noanglegeom()
{
    update_indpanel(current->indmask & ~I_ANGLEGEOM);
}

static void
turn_on(msw)
    mode_sw_info   *msw;
{
    FirstArg(XtNbackgroundPixmap, msw->reversePM);
    SetValues(msw->widget);
}

turn_on_current()
{
    if (current)
	turn_on(current);
}

turn_off_current()
{
    if (current) {
	XtOverrideTranslations(current->widget,
		XtParseTranslationTable(mode_translations));
	FirstArg(XtNbackgroundPixmap, current->pixmap);
	SetValues(current->widget);
    }
}

change_mode(icon)
icon_struct *icon;
{
    int i;
    XButtonEvent ev; /* To fake an event with */

    ev.button = Button1;
    for (i = 0; i < NUM_MODE_SW; ++i)
	if (mode_switches[i].icon == icon) {
	    sel_mode_but(0,&mode_switches[i],&ev,0);
	    break;
	}
    /* force update of mouse function window */
    draw_mousefun_canvas();
}

static
stub_circlebyradius_drawing_selected()
{
	change_mode(&cirrad_ic);
}

static
stub_circlebydiameter_drawing_selected()
{
	change_mode(&cirdia_ic);
}

static
stub_ellipsebyradius_drawing_selected()
{
	change_mode(&ellrad_ic);
}

static
stub_ellipsebydiameter_drawing_selected()
{
	change_mode(&elldia_ic);
}

static
stub_box_drawing_selected()
{
	change_mode(&box_ic);
}

static
stub_arcbox_drawing_selected()
{
	change_mode(&arc_box_ic);
}

static
stub_poly_drawing_selected()
{
	change_mode(&polygon_ic);
}

static
stub_line_drawing_selected()
{
	change_mode(&line_ic);
}

static
stub_regpoly_drawing_selected()
{
	change_mode(&regpoly_ic);
}

static
stub_picobj_drawing_selected()
{
	change_mode(&picobj_ic);
}

static
stub_text_drawing_selected()
{
	change_mode(&text_ic);
}

static
stub_arc_drawing_selected()
{
	change_mode(&arc_ic);
}

static
stub_cl_spline_drawing_selected()
{
	change_mode(&c_spl_ic);
}

static
stub_spline_drawing_selected()
{
	change_mode(&spl_ic);
}

static
stub_cl_intspline_drawing_selected()
{
	change_mode(&c_intspl_ic);
}

static
stub_intspline_drawing_selected()
{
	change_mode(&intspl_ic);
}

static
stub_align_selected()
{
	change_mode(&align_ic);
}

static
stub_compound_selected()
{
	change_mode(&glue_ic);
}

static
stub_break_selected()
{
	change_mode(&break_ic);
}

static
stub_open_compound_selected()
{
	change_mode(&open_comp_ic);
}

static
stub_scale_selected()
{
	change_mode(&scale_ic);
}

static
stub_point_adding_selected()
{
	change_mode(&addpt_ic);
}

static
stub_delete_point_selected()
{
	change_mode(&deletept_ic);
}

static
stub_move_selected()
{
	change_mode(&move_ic);
}

static
stub_move_point_selected()
{
	change_mode(&movept_ic);
}

static
stub_delete_selected()
{
	change_mode(&delete_ic);
}

static
stub_copy_selected()
{
	change_mode(&copy_ic);
}

static
stub_rotate_cw_selected()
{
	change_mode(&rotCW_ic);
}

static
stub_rotate_ccw_selected()
{
	change_mode(&rotCCW_ic);
}

static
stub_flip_ud_selected()
{
	change_mode(&flip_y_ic);
}

static
stub_flip_lr_selected()
{
	change_mode(&flip_x_ic);
}

static
stub_convert_selected()
{
	change_mode(&convert_ic);
}

static
stub_arrow_head_selected()
{
	change_mode(&autoarrow_ic);
}

static
stub_edit_item_selected()
{
	change_mode(&edit_ic);
}

static
stub_update_selected()
{
	change_mode(&update_ic);
}
