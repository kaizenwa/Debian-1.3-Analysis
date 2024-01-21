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
#include "u_elastic.h"
#include "u_list.h"
#include "w_canvas.h"
#include "w_mousefun.h"

extern int	latex_endpoint();

/*************************** locally global variables *********************/

static int	init_line_drawing();

int		create_lineobject();
int		get_intermediatepoint();

/**********************	 polyline and polygon section  **********************/

line_drawing_selected()
{
    canvas_kbd_proc = null_proc;
    canvas_locmove_proc = null_proc;
    canvas_leftbut_proc = init_line_drawing;
    canvas_rightbut_proc = null_proc;
    set_cursor(arrow_cursor);
    reset_action_on();
    if (cur_mode == F_POLYGON) {
	set_mousefun("first point", "", "", "", "", "");
	min_num_points = 3;
	canvas_middlebut_proc = null_proc;
    } else {
	set_mousefun("first point", "single point", "", "", "", "");
	min_num_points = 1;
	num_point = 0;
	fix_x = fix_y = -1;
	canvas_middlebut_proc = create_lineobject;
    }
}

static
init_line_drawing(x, y)
    int		    x, y;
{
    init_trace_drawing(x, y);
}

cancel_line_drawing()
{
    elastic_line();
    cur_x = fix_x;
    cur_y = fix_y;
    if (cur_point != first_point)
	elastic_moveline(first_point);	/* erase control vector */
    free_points(first_point);
    return_proc();
    draw_mousefun_canvas();
}

init_trace_drawing(x, y)
    int		    x, y;
{
    if ((first_point = create_point()) == NULL)
	return;

    cur_point = first_point;
    set_action_on();
    cur_point->x = fix_x = cur_x = x;
    cur_point->y = fix_y = cur_y = y;
    cur_point->next = NULL;
    length_msg(MSG_LENGTH);
    if (latexline_mode || latexarrow_mode) {
	canvas_locmove_proc = latex_line;
    } else if (manhattan_mode || mountain_mode) {
	canvas_locmove_proc = constrainedangle_line;
    } else {
	canvas_locmove_proc = freehand_line;
    }
    canvas_leftbut_proc = get_intermediatepoint;
    canvas_middlebut_save = create_lineobject;
    canvas_rightbut_proc = cancel_line_drawing;
    return_proc = line_drawing_selected;
    num_point = 1;
    set_mousefun("next point", "", "cancel", "del point", "", "");
    if (num_point >= min_num_points - 1) {
	set_mousefun("next point", "final point", "cancel", "del point", "", "");
	canvas_middlebut_proc = canvas_middlebut_save;
    }
    draw_mousefun_canvas();
    set_temp_cursor(null_cursor);
    cur_cursor = null_cursor;
    elastic_line();
}

get_intermediatepoint(x, y, shift)
    int		    x, y;
    int		    shift;
{
    (*canvas_locmove_proc) (x, y);
    num_point++;
    fix_x = cur_x;
    fix_y = cur_y;
    elastic_line();
    if (cur_cursor != null_cursor) {
	set_temp_cursor(null_cursor);
	cur_cursor = null_cursor;
    }
    win_setmouseposition(canvas_win, cur_x, cur_y);
    if (shift != 0 && num_point > 2) {
	F_point	*p;

	num_point -= 2;
	p = prev_point(first_point, cur_point);
	p->next = NULL;
	/* erase the newest segment */
	pw_vector(canvas_win, fix_x, fix_y, cur_point->x, cur_point->y,
		  INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	/* and segment drawn before */
	pw_vector(canvas_win, p->x, p->y, cur_point->x, cur_point->y,
		  INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	/* and draw new elastic segment */
	pw_vector(canvas_win, fix_x, fix_y, p->x, p->y,
		  PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	fix_x = p->x;
	fix_y = p->y;
	free_points(cur_point);
	cur_point = p;
    } else {
	append_point(fix_x, fix_y, &cur_point);
    }
    if (num_point == min_num_points - 1) {
	set_mousefun("next point", "final point", "cancel", "del point", "", "");
	draw_mousefun_canvas();
	canvas_middlebut_proc = canvas_middlebut_save;
    }
}

/* come here upon pressing middle button (last point of lineobject) */

create_lineobject(x, y)
    int		    x, y;
{
    F_line	   *line;
    int		    dot;

    if (num_point == 0) {
	if ((first_point = create_point()) == NULL) {
	    line_drawing_selected();
	    draw_mousefun_canvas();
	    return;
	}
	cur_point = first_point;
	first_point->x = fix_x = cur_x = x;
	first_point->y = fix_y = cur_y = y;
	first_point->next = NULL;
	num_point++;
    } else if (x != fix_x || y != fix_y) {
	get_intermediatepoint(x, y, 0);
    }
    dot = (num_point == 1);
    elastic_line();
    if ((line = create_line()) == NULL) {
	line_drawing_selected();
	draw_mousefun_canvas();
	return;
    }
    line->type = T_POLYLINE;
    line->style = cur_linestyle;
    line->thickness = cur_linewidth;
    line->pen_color = cur_pencolor;
    line->fill_color = cur_fillcolor;
    line->depth = cur_depth;
    line->pen_style = 0;
    line->join_style = cur_joinstyle;
    line->cap_style = cur_capstyle;
    line->fill_style = cur_fillstyle;
    line->style_val = cur_styleval * (cur_linewidth + 1) / 2;
    line->points = first_point;
    if (!dot) {
	if (cur_mode == F_POLYGON) {	/* close off polygon */
	    line->type = T_POLYGON;
	    num_point++;
	    append_point(first_point->x, first_point->y, &cur_point);
	    elastic_line();
	    fix_x = first_point->x;
	    fix_y = first_point->y;
	    elastic_line();	/* fix last elastic line */
	} else {		/* polyline; draw any arrows */
	    if (autoforwardarrow_mode)
		line->for_arrow = forward_arrow();
	    /* arrow will be drawn in draw_line below */
	    if (autobackwardarrow_mode)
		line->back_arrow = backward_arrow();
	    /* arrow will be drawn in draw_line below */
	}
	cur_x = fix_x;
	cur_y = fix_y;
	elastic_moveline(first_point);	/* erase temporary outline */
    }
    add_line(line);
    /* draw it and anything on top of it */
    redisplay_line(line);
    line_drawing_selected();
    draw_mousefun_canvas();
}
