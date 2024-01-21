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

#include "fig.h"
#include "resources.h"
#include "mode.h"
#include "object.h"
#include "paintop.h"
#include "u_create.h"
#include "u_draw.h"
#include "u_elastic.h"
#include "u_list.h"
#include "u_search.h"
#include "w_canvas.h"
#include "w_mousefun.h"
extern void	force_positioning(), force_nopositioning();
extern void	force_anglegeom(), force_noanglegeom();

static int	init_point_adding();
static int	fix_linepoint_adding();
static int	fix_splinepoint_adding();
static int	init_linepointadding();
static int	init_splinepointadding();
static int	find_endpoints();

point_adding_selected()
{
    set_mousefun("break/add here", "", "", "", "", "");
    canvas_kbd_proc = null_proc;
    canvas_locmove_proc = null_proc;
    init_searchproc_left(init_point_adding);
    canvas_leftbut_proc = object_search_left;
    canvas_middlebut_proc = null_proc;
    canvas_rightbut_proc = null_proc;
    set_cursor(pick9_cursor);
    force_nopositioning();
    force_anglegeom();
    constrained = MOVE_ARB;
}

static int
init_point_adding(p, type, x, y, px, py)
    char	   *p;
    int		    type;
    int		    x, y;
    int		    px, py;
{
    set_action_on();
    set_mousefun("place new point", "", "cancel", "", "", "");
    draw_mousefun_canvas();
    set_temp_cursor(null_cursor);
    win_setmouseposition(canvas_win, px, py);
    switch (type) {
    case O_POLYLINE:
	cur_l = (F_line *) p;
	/* the search routine will ensure that we don't have a box */
	init_linepointadding(px, py);
	break;
    case O_SPLINE:
	cur_s = (F_spline *) p;
	init_splinepointadding(px, py);
	break;
    default:
	return;
    }
    force_positioning();
    /* draw in rubber-band line */
    elastic_linelink();

    if (left_point == NULL || right_point == NULL) {
	if (latexline_mode || latexarrow_mode) {
	    canvas_locmove_proc = latex_line;
	    return;
	}
	if (mountain_mode || manhattan_mode) {
	    canvas_locmove_proc = constrainedangle_line;
	    return;
	}
    } else {
	force_noanglegeom();
    }
    canvas_locmove_proc = reshaping_line;
}

static
wrapup_pointadding()
{
    reset_action_on();
    point_adding_selected();
    draw_mousefun_canvas();
}

static
cancel_pointadding()
{
    elastic_linelink();
    wrapup_pointadding();
}

static
cancel_line_pointadding()
{
    if (left_point != NULL && right_point != NULL)
	pw_vector(canvas_win, left_point->x, left_point->y,
		  right_point->x, right_point->y, PAINT,
		  cur_l->thickness, cur_l->style, cur_l->style_val,
		  cur_l->pen_color);
    cancel_pointadding();
}

/**************************  spline  *******************************/

static int
init_splinepointadding(px, py)
    int		    px, py;
{
    find_endpoints(cur_s->points, px, py, &left_point, &right_point);

    cur_x = fix_x = px;
    cur_y = fix_y = py;
    if (left_point == NULL && closed_spline(cur_s)) {
	/* The added_point is between the 1st and 2nd point. */
	left_point = right_point;
	right_point = right_point->next;
    }
    if (right_point == NULL && closed_spline(cur_s)) {
	/* The added_point is between the last and 1st point. */
	right_point = cur_s->points;
    }
    canvas_leftbut_proc = fix_splinepoint_adding;
    canvas_rightbut_proc = cancel_pointadding;
}

static
fix_splinepoint_adding(x, y)
    int		    x, y;
{
    F_point	   *p;

    (*canvas_locmove_proc) (x, y);
    if ((p = create_point()) == NULL) {
	wrapup_pointadding();
	return;
    }
    p->x = cur_x;
    p->y = cur_y;
    elastic_linelink();
    splinepoint_adding(cur_s, left_point, p, right_point,
		   approx_spline(cur_s) ? S_SPLINE_APPROX : S_SPLINE_INTERP);
    wrapup_pointadding();
}

/*
 * Added_point is always inserted between left_point and
 * right_point, except in two cases. (1) left_point is NULL, the added_point
 * will be prepended to the list of points. (2) right_point is NULL, the 
 * added_point will be appended to the end of the list.
 */

splinepoint_adding(spline, left_point, added_point, right_point, sfactor)
    F_spline	   *spline;
    F_point	   *left_point, *added_point, *right_point;
    double         sfactor;
{
    F_sfactor	   *c, *prev_sfactor;


    if ((c = create_sfactor()) == NULL)
	    return;
    set_temp_cursor(wait_cursor);
    mask_toggle_splinemarker(spline);
    /* delete it and redraw underlying objects */
    list_delete_spline(&objects.splines, spline);
    redisplay_spline(spline);
    if (left_point == NULL) {
	added_point->next = spline->points;
	spline->points = added_point;
	if (open_spline(spline)) {
	  c->s = S_SPLINE_ANGULAR;
	  spline->sfactors->s = sfactor;
    }
	else
	  c->s = sfactor;

	c->next = spline->sfactors;
	spline->sfactors = c;
      }
    else {
      prev_sfactor = search_sfactor(spline, left_point);
      if (open_spline(spline) && (right_point == NULL))
	{
	  c->s = S_SPLINE_ANGULAR;
	  prev_sfactor->s = sfactor;
	}
      else
	c->s = sfactor;
     
      c->next = prev_sfactor->next;
      prev_sfactor->next = c;
      added_point->next = left_point->next; /*right_point;*/
      left_point->next = added_point;
    }
    /* put it back in the list and draw the new spline */
    list_add_spline(&objects.splines, spline);
    /* redraw it and anything on top of it */
    redisplay_spline(spline);
    clean_up();
    set_modifiedflag();
    set_last_prevpoint(left_point);
    set_last_selectedpoint(added_point);
    set_action_object(F_ADD_POINT, O_SPLINE);
    set_latestspline(spline);
    reset_cursor();
}

/***************************  line  ********************************/

static int
init_linepointadding(px, py)
    int		    px, py;
{
    find_endpoints(cur_l->points, px, py, &left_point, &right_point);

    /* set cur_x etc at new point coords */
    cur_x = fix_x = px;
    cur_y = fix_y = py;
    if (left_point == NULL && cur_l->type == T_POLYGON) {
	left_point = right_point;
	right_point = right_point->next;
    }
    /* erase line segment where new point is */
    if (left_point != NULL && right_point != NULL)
	pw_vector(canvas_win, left_point->x, left_point->y,
		  right_point->x, right_point->y, ERASE,
		  cur_l->thickness, cur_l->style, cur_l->style_val,
		  cur_l->pen_color);

    canvas_leftbut_proc = fix_linepoint_adding;
    canvas_rightbut_proc = cancel_line_pointadding;
}

static
fix_linepoint_adding(x, y)
    int x, y;
{
    F_point	   *p;

    (*canvas_locmove_proc) (x, y);
    if ((p = create_point()) == NULL) {
	wrapup_pointadding();
	return;
    }
    p->x = cur_x;
    p->y = cur_y;
    elastic_linelink();
    linepoint_adding(cur_l, left_point, p);
    wrapup_pointadding();
}

linepoint_adding(line, left_point, added_point)
    F_line	   *line;
    F_point	   *left_point, *added_point;
{
    mask_toggle_linemarker(line);
    /* delete it and redraw underlying objects */
    list_delete_line(&objects.lines, line);
    redisplay_line(line);
    if (left_point == NULL) {
	added_point->next = line->points;
	line->points = added_point;
    } else {
	added_point->next = left_point->next;
	left_point->next = added_point;
    }
    /* put it back in the list and draw the new line */
    list_add_line(&objects.lines, line);
    /* redraw it and anything on top of it */
    redisplay_line(line);
    clean_up();
    set_action_object(F_ADD_POINT, O_POLYLINE);
    set_latestline(line);
    set_last_prevpoint(left_point);
    set_last_selectedpoint(added_point);
    set_modifiedflag();
}

/*******************************************************************/

/*
 * If (x,y) is close to a point, q, fp points to q and sp points to q->next
 * (right).  However if q is the first point, fp contains NULL and sp points
 * to q.
 */

static int
find_endpoints(p, x, y, fp, sp)
    F_point	   *p, **fp, **sp;
    int		    x, y;
{
    int		    d;
    F_point	   *a = NULL, *b = p;

    if (x == b->x && y == b->y) {
	*fp = a;
	*sp = b;
	return;
    }
    for (a = p, b = p->next; b != NULL; a = b, b = b->next) {
	if (x == b->x && y == b->y) {
	    *fp = b;
	    *sp = b->next;
	    return;
	}
	if (close_to_vector(a->x, a->y, b->x, b->y, x, y, 1, 1.0, &d, &d)) {
	    *fp = a;
	    *sp = b;
	    return;
	}
    }
    *fp = a;
    *sp = b;
}
