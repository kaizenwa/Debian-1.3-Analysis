/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Parts Copyright (c) 1991 by Paul King
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
#include "resources.h"
#include "mode.h"
#include "object.h"
#include "paintop.h"
#include "u_elastic.h"
#include "w_canvas.h"
#include "w_setup.h"
#include "w_zoom.h"

extern double	compute_angle();

/********************** EXPORTS **************/

int		constrained;
int		fix_x, fix_y, work_numsides;
float		cur_angle;
int		x1off, x2off, y1off, y2off;
Cursor		cur_latexcursor;
int		from_x, from_y;
double		cosa, sina;
int		movedpoint_num;
F_point	       *left_point, *right_point;

/**************** LOCAL ***********/

static void	elastic_links();

/*************************** BOXES *************************/

elastic_box(x1, y1, x2, y2)
    int		    x1, y1, x2, y2;
{
    int		    wid, ht;

    set_line_stuff(1, RUBBER_LINE, 0.0, JOIN_MITER, CAP_BUTT,
		INV_PAINT, DEFAULT);
    wid = abs(x2-x1)+1;
    ht = abs(y2-y1)+1;
    zXDrawRectangle(tool_d, canvas_win, gccache[INV_PAINT],min2(x1,x2),min2(y1,y2),wid,ht);
}

elastic_movebox()
{
    register int    x1, y1, x2, y2;

    x1 = cur_x + x1off;
    x2 = cur_x + x2off;
    y1 = cur_y + y1off;
    y2 = cur_y + y2off;
    elastic_box(x1, y1, x2, y2);
    elastic_links(cur_x - fix_x, cur_y - fix_y, 1.0, 1.0);
}

moving_box(x, y)
    int		    x, y;
{
    elastic_movebox();
    adjust_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_movebox();
    length_msg(MSG_DIST);
}

resizing_box(x, y)
    int		    x, y;
{
    elastic_box(fix_x, fix_y, cur_x, cur_y);
    cur_x = x;
    cur_y = y;
    elastic_box(fix_x, fix_y, cur_x, cur_y);
    boxsize_msg(1);
}

constrained_resizing_box(x, y)
    int		    x, y;
{
    elastic_box(fix_x, fix_y, cur_x, cur_y);
    adjust_box_pos(x, y, from_x, from_y, &cur_x, &cur_y);
    elastic_box(fix_x, fix_y, cur_x, cur_y);
    boxsize_msg(1);
}

scaling_compound(x, y)
    int		    x, y;
{
    elastic_scalecompound(cur_c);
    adjust_box_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_scalecompound(cur_c);
}

elastic_scalecompound(c)
    F_compound	   *c;
{
    double	    newx, newy, oldx, oldy;
    double	    newd, oldd, scalefact;
    int		    x1, y1, x2, y2;

    newx = cur_x - fix_x;
    newy = cur_y - fix_y;
    newd = sqrt(newx * newx + newy * newy);
    oldx = from_x - fix_x;
    oldy = from_y - fix_y;
    oldd = sqrt(oldx * oldx + oldy * oldy);
    scalefact = newd / oldd;
    x1 = fix_x + round((c->secorner.x - fix_x) * scalefact);
    y1 = fix_y + round((c->secorner.y - fix_y) * scalefact);
    x2 = fix_x + round((c->nwcorner.x - fix_x) * scalefact);
    y2 = fix_y + round((c->nwcorner.y - fix_y) * scalefact);
    elastic_box(x1, y1, x2, y2);
    boxsize_msg(2);
}

/*************************** LINES *************************/

elastic_line()
{
    pw_vector(canvas_win, fix_x, fix_y, cur_x, cur_y,
	      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
}

freehand_line(x, y)
    int		    x, y;
{
    elastic_line();
    cur_x = x;
    cur_y = y;
    elastic_line();
    length_msg(MSG_LENGTH);
}

latex_line(x, y)
    int		    x, y;
{
    Cursor c;

    elastic_line();
    latex_endpoint(fix_x, fix_y, x, y, &cur_x, &cur_y, latexarrow_mode,
		   (cur_pointposn == P_ANY) ? 1 : posn_rnd[cur_pointposn]);
    elastic_line();
    length_msg(MSG_LENGTH);
    c = (x == cur_x && y == cur_y) ? null_cursor : crosshair_cursor;
    if (c != cur_cursor) {
	set_temp_cursor(c);
	cur_cursor = c;
    }
}

constrainedangle_line(x, y)
    int		    x, y;
{
    double	    angle, dx, dy;

    if (x == cur_x && y == cur_y)
	return;

    dx = x - fix_x;
    dy = fix_y - y;
    /* only move if the pointer has moved at least 2 pixels */
    if (sqrt(dx * dx + dy * dy) < 2.0)
	return;
    if (dx == 0)
	angle = -90.0;
    else
	angle = 180.0 * atan(dy / dx) / M_PI;

    elastic_line();
    if (manhattan_mode) {
	if (mountain_mode) {
	    if (angle < -67.5)
		angle90_line(x, y);
	    else if (angle < -22.5)
		angle135_line(x, y);
	    else if (angle < 22.5)
		angle0_line(x, y);
	    else if (angle < 67.5)
		angle45_line(x, y);
	    else
		angle90_line(x, y);
	} else {
	    if (angle < -45.0)
		angle90_line(x, y);
	    else if (angle < 45.0)
		angle0_line(x, y);
	    else
		angle90_line(x, y);
	}
    } else {
	if (angle < 0.0)
	    angle135_line(x, y);
	else
	    angle45_line(x, y);
    }
    elastic_line();
    length_msg(MSG_LENGTH);
}

angle0_line(x, y)
    int		    x, y;
{
    cur_x = x;
    cur_y = fix_y;
}

angle90_line(x, y)
    int		    x, y;
{
    cur_y = y;
    cur_x = fix_x;
}

angle45_line(x, y)
    int		    x, y;
{
    if (abs(x - fix_x) < abs(y - fix_y)) {
	cur_x = fix_x - y + fix_y;
	cur_y = y;
    } else {
	cur_y = fix_y + fix_x - x;
	cur_x = x;
    }
}

angle135_line(x, y)
    int		    x, y;
{
    if (abs(x - fix_x) < abs(y - fix_y)) {
	cur_x = fix_x + y - fix_y;
	cur_y = y;
    } else {
	cur_y = fix_y + x - fix_x;
	cur_x = x;
    }
}

reshaping_line(x, y)
    int		    x, y;
{
    elastic_linelink();
    adjust_pos(x, y, from_x, from_y, &cur_x, &cur_y);
    elastic_linelink();
    /* one or two lines moving with the move point? */
    if (left_point != NULL && right_point != NULL) {
	length_msg2(left_point->x,left_point->y,
		    right_point->x,right_point->y,cur_x,cur_y);
    } else if (left_point != NULL) {
	altlength_msg(MSG_LENGTH,left_point->x,left_point->y);
    } else if (right_point != NULL) {
	altlength_msg(MSG_LENGTH,right_point->x,right_point->y);
    }
}

elastic_linelink()
{
    if (left_point != NULL) {
	pw_vector(canvas_win, left_point->x, left_point->y,
	       cur_x, cur_y, INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
    }
    if (right_point != NULL) {
	pw_vector(canvas_win, right_point->x, right_point->y,
	       cur_x, cur_y, INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
    }
}

moving_line(x, y)
    int		    x, y;
{
    elastic_moveline(new_l->points);
    adjust_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_moveline(new_l->points);
    length_msg(MSG_DIST);
}

elastic_moveline(pts)
    F_point	   *pts;
{
    F_point	   *p;
    int		    dx, dy, x, y, xx, yy;

    p = pts;
    if (p->next == NULL) {	/* dot */
	pw_vector(canvas_win, cur_x, cur_y, cur_x, cur_y,
		  INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
    } else {
	dx = cur_x - fix_x;
	dy = cur_y - fix_y;
	x = p->x + dx;
	y = p->y + dy;
	for (p = p->next; p != NULL; x = xx, y = yy, p = p->next) {
	    xx = p->x + dx;
	    yy = p->y + dy;
	    pw_vector(canvas_win, x, y, xx, yy, INV_PAINT, 1,
		      RUBBER_LINE, 0.0, DEFAULT);
	}
    }
    elastic_links(dx, dy, 1.0, 1.0);
}

static void
elastic_links(dx, dy, sx, sy)
    int		    dx, dy;
    float	    sx, sy;
{
    F_linkinfo	   *k;

    if (cur_linkmode == SMART_OFF)
	return;

    for (k = cur_links; k != NULL; k = k->next)
	if (k->prevpt == NULL) {/* dot */
	    pw_vector(canvas_win, k->endpt->x, k->endpt->y,
		      k->endpt->x, k->endpt->y,
		      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	} else {
	    if (cur_linkmode == SMART_MOVE)
		pw_vector(canvas_win, k->endpt->x + dx, k->endpt->y + dy,
			  k->prevpt->x, k->prevpt->y,
			  INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	    else if (cur_linkmode == SMART_SLIDE) {
		if (k->endpt->x == k->prevpt->x) {
		    if (!k->two_pts)
			pw_vector(canvas_win, k->prevpt->x,
			      k->prevpt->y, k->prevpt->x + dx, k->prevpt->y,
			     INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
		    pw_vector(canvas_win, k->endpt->x + dx,
			  k->endpt->y + dy, k->prevpt->x + dx, k->prevpt->y,
			      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
		} else {
		    if (!k->two_pts)
			pw_vector(canvas_win, k->prevpt->x,
			      k->prevpt->y, k->prevpt->x, k->prevpt->y + dy,
			     INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
		    pw_vector(canvas_win, k->endpt->x + dx,
			  k->endpt->y + dy, k->prevpt->x, k->prevpt->y + dy,
			      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
		}
	    }
	}
}

scaling_line(x, y)
    int		    x, y;
{
    elastic_scalepts(cur_l->points);
    adjust_box_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_scalepts(cur_l->points);
    if (cur_l->type == T_BOX || cur_l->type == T_PICTURE)
	boxsize_msg(2);
}

scaling_spline(x, y)
    int		    x, y;
{
    elastic_scalepts(cur_s->points);
    adjust_box_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_scalepts(cur_s->points);
}

elastic_scalepts(pts)
    F_point	   *pts;
{
    F_point	   *p;
    double	    newx, newy, oldx, oldy;
    double	    newd, oldd, scalefact;
    int		    ox, oy, xx, yy;

    p = pts;
    newx = cur_x - fix_x;
    newy = cur_y - fix_y;
    newd = sqrt(newx * newx + newy * newy);

    oldx = from_x - fix_x;
    oldy = from_y - fix_y;
    oldd = sqrt(oldx * oldx + oldy * oldy);

    scalefact = newd / oldd;
    ox = fix_x + round((p->x - fix_x) * scalefact);
    oy = fix_y + round((p->y - fix_y) * scalefact);
    for (p = p->next; p != NULL; ox = xx, oy = yy, p = p->next) {
	xx = fix_x + round((p->x - fix_x) * scalefact);
	yy = fix_y + round((p->y - fix_y) * scalefact);
	pw_vector(canvas_win, ox, oy, xx, yy, INV_PAINT, 1,
		  RUBBER_LINE, 0.0, DEFAULT);
    }
}

elastic_poly(x1, y1, x2, y2, numsides)
    int		    x1, y1, x2, y2, numsides;
{
    register float  angle;
    register int    nx, ny, i; 
    double	    dx, dy;
    double	    init_angle, mag;
    int		    ox, oy;

    dx = x2 - x1;
    dy = y2 - y1;
    mag = sqrt(dx * dx + dy * dy);
    init_angle = compute_angle(dx, dy);
    ox = x2;
    oy = y2;

    /* now append numsides points */
    for (i = 1; i < numsides; i++) {
	angle = (float)(init_angle - M_2PI * (double) i / (double) numsides);
	if (angle < 0)
	    angle += M_2PI;
	nx = x1 + round(mag * cos((double) angle));
	ny = y1 + round(mag * sin((double) angle));
	pw_vector(canvas_win, nx, ny, ox, oy, INV_PAINT, 1,
		  RUBBER_LINE, 0.0, DEFAULT);
	ox = nx;
	oy = ny;
    }
    pw_vector(canvas_win, ox, oy, x2, y2, INV_PAINT, 1,
	      RUBBER_LINE, 0.0, DEFAULT);
}

resizing_poly(x, y)
    int		    x, y;
{
    elastic_poly(fix_x, fix_y, cur_x, cur_y, work_numsides);
    cur_x = x;
    cur_y = y;
    work_numsides = cur_numsides;
    elastic_poly(fix_x, fix_y, cur_x, cur_y, work_numsides);
    length_msg(MSG_LENGTH);
}

/*********************** ELLIPSES *************************/

elastic_ebr()
{
    register int    x1, y1, x2, y2;
    int		    rx, ry;

    rx = cur_x - fix_x;
    ry = cur_y - fix_y;
    if (cur_angle != 0.0) {
	angle_ellipse(fix_x, fix_y, rx, ry, cur_angle, INV_PAINT, 1, 
	     RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT);
    } else {
	x1 = fix_x + rx;
	x2 = fix_x - rx;
	y1 = fix_y + ry;
	y2 = fix_y - ry;
	pw_curve(canvas_win, x1, y1, x2, y2, INV_PAINT, 1,
	     RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT, CAP_BUTT);
    }
}

resizing_ebr(x, y)
    int		    x, y;
{
    elastic_ebr();
    cur_x = x;
    cur_y = y;
    elastic_ebr();
    length_msg(MSG_RADIUS);
}

constrained_resizing_ebr(x, y)
    int		    x, y;
{
    elastic_ebr();
    adjust_box_pos(x, y, from_x, from_y, &cur_x, &cur_y);
    elastic_ebr();
    length_msg(MSG_RADIUS);
}

elastic_ebd()
{
    int		    centx,centy;
    centx = (fix_x+cur_x)/2;
    centy = (fix_y+cur_y)/2;
    if (cur_angle != 0.0) {
	angle_ellipse(centx, centy, abs(cur_x-fix_x)/2, 
		  abs(cur_y-fix_y)/2, cur_angle,
		  INV_PAINT, 1, RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT);
    } else {
	pw_curve(canvas_win, fix_x, fix_y, cur_x, cur_y,
	     INV_PAINT, 1, RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT, CAP_BUTT);
    }
    length_msg(MSG_DIAM);
}

resizing_ebd(x, y)
    int		    x, y;
{
    elastic_ebd();
    cur_x = x;
    cur_y = y;
    elastic_ebd();
    length_msg(MSG_DIAM);
}

constrained_resizing_ebd(x, y)
    int		    x, y;
{
    elastic_ebd();
    adjust_box_pos(x, y, from_x, from_y, &cur_x, &cur_y);
    elastic_ebd();
    length_msg(MSG_DIAM);
}

elastic_cbr()
{
    register int    radius, x1, y1, x2, y2;
    double	    rx, ry;

    rx = cur_x - fix_x;
    ry = cur_y - fix_y;
    radius = round(sqrt(rx * rx + ry * ry));
    x1 = fix_x + radius;
    x2 = fix_x - radius;
    y1 = fix_y + radius;
    y2 = fix_y - radius;
    pw_curve(canvas_win, x1, y1, x2, y2, INV_PAINT, 1,
	     RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT, CAP_BUTT);
}

resizing_cbr(x, y)
    int		    x, y;
{
    elastic_cbr();
    cur_x = x;
    cur_y = y;
    elastic_cbr();
    length_msg(MSG_RADIUS);
}

elastic_cbd()
{
    register int    x1, y1, x2, y2;
    int		    radius;
    double	    rx, ry;

    rx = (cur_x - fix_x) / 2;
    ry = (cur_y - fix_y) / 2;
    radius = round(sqrt(rx * rx + ry * ry));
    x1 = fix_x + rx + radius;
    x2 = fix_x + rx - radius;
    y1 = fix_y + ry + radius;
    y2 = fix_y + ry - radius;
    pw_curve(canvas_win, x1, y1, x2, y2, INV_PAINT, 1,
	     RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT, CAP_BUTT);
}

resizing_cbd(x, y)
    int		    x, y;
{
    elastic_cbd();
    cur_x = x;
    cur_y = y;
    elastic_cbd();
    length_msg(MSG_DIAM);
}

constrained_resizing_cbd(x, y)
    int		    x, y;
{
    elastic_cbd();
    adjust_box_pos(x, y, from_x, from_y, &cur_x, &cur_y);
    elastic_cbd();
    length_msg(MSG_DIAM);
}

elastic_moveellipse()
{
    register int    x1, y1, x2, y2;

    x1 = cur_x + x1off;
    x2 = cur_x + x2off;
    y1 = cur_y + y1off;
    y2 = cur_y + y2off;
    if (cur_angle != 0.0) {
	angle_ellipse((x1+x2)/2, (y1+y2)/2, abs(x1-x2)/2, abs(y1-y2)/2, cur_angle,
		  INV_PAINT, 1, RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT);
    } else {
	pw_curve(canvas_win, x1, y1, x2, y2, INV_PAINT, 1,
	     RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT, CAP_BUTT);
    }
}

moving_ellipse(x, y)
    int		    x, y;
{
    elastic_moveellipse();
    adjust_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_moveellipse();
    length_msg(MSG_DIST);
}

elastic_scaleellipse(e)
    F_ellipse	   *e;
{
    register int    x1, y1, x2, y2;
    int		    rx, ry;
    double	    newx, newy, oldx, oldy;
    float	    newd, oldd, scalefact;

    newx = cur_x - fix_x;
    newy = cur_y - fix_y;
    newd = sqrt(newx * newx + newy * newy);

    oldx = from_x - fix_x;
    oldy = from_y - fix_y;
    oldd = sqrt(oldx * oldx + oldy * oldy);

    scalefact = newd / oldd;

    rx = round(e->radiuses.x * scalefact);
    ry = round(e->radiuses.y * scalefact);
    if (cur_angle != 0.0) {
	angle_ellipse(e->center.x, e->center.y, rx, ry, cur_angle,
		  INV_PAINT, 1, RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT);
    } else {
	x1 = fix_x + rx;
	x2 = fix_x - rx;
	y1 = fix_y + ry;
	y2 = fix_y - ry;
	pw_curve(canvas_win, x1, y1, x2, y2, INV_PAINT, 1,
	     RUBBER_LINE, 0.0, UNFILLED, DEFAULT, DEFAULT, CAP_BUTT);
    }
}

scaling_ellipse(x, y)
    int		    x, y;
{
    elastic_scaleellipse(cur_e);
    adjust_box_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_scaleellipse(cur_e);
    if (cur_e->type == 1 || cur_e->type == 3)
	length_msg(MSG_RADIUS);
    else
	length_msg(MSG_DIAM);
}

/*************************** ARCS *************************/

reshaping_arc(x, y)
    int		    x, y;
{
    elastic_arclink();
    adjust_pos(x, y, cur_a->point[movedpoint_num].x,
	       cur_a->point[movedpoint_num].y, &cur_x, &cur_y);
    elastic_arclink();
    if (movedpoint_num == 1) {
	/* middle point */
	length_msg2(cur_a->point[0].x, cur_a->point[0].y,
		    cur_a->point[2].x, cur_a->point[2].y,
		    cur_x, cur_y);
    } else {
	/* end point */
	altlength_msg(MSG_LENGTH,cur_a->point[1].x,cur_a->point[1].y);
    }
}

elastic_arclink()
{
    switch (movedpoint_num) {
    case 0:
	pw_vector(canvas_win, cur_x, cur_y,
		  cur_a->point[1].x, cur_a->point[1].y,
		  INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	break;
    case 1:
	pw_vector(canvas_win, cur_a->point[0].x, cur_a->point[0].y,
		  cur_x, cur_y, INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	pw_vector(canvas_win, cur_a->point[2].x, cur_a->point[2].y,
		  cur_x, cur_y, INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
	break;
    default:
	pw_vector(canvas_win, cur_a->point[1].x, cur_a->point[1].y,
		  cur_x, cur_y, INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
    }
}

moving_arc(x, y)
    int		    x, y;
{
    elastic_movearc(new_a);
    adjust_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_movearc(new_a);
    length_msg(MSG_DIST);
}

elastic_movearc(a)
    F_arc	   *a;
{
    int		    dx, dy;

    dx = cur_x - fix_x;
    dy = cur_y - fix_y;
    pw_vector(canvas_win, a->point[0].x + dx, a->point[0].y + dy,
	      a->point[1].x + dx, a->point[1].y + dy,
	      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
    pw_vector(canvas_win, a->point[1].x + dx, a->point[1].y + dy,
	      a->point[2].x + dx, a->point[2].y + dy,
	      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
}

scaling_arc(x, y)
    int		    x, y;
{
    elastic_scalearc(cur_a);
    adjust_box_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_scalearc(cur_a);
}

elastic_scalearc(a)
    F_arc	   *a;
{
    double	    newx, newy, oldx, oldy;
    double	    newd, oldd, scalefact;
    F_pos	    p0, p1, p2;

    newx = cur_x - fix_x;
    newy = cur_y - fix_y;
    newd = sqrt(newx * newx + newy * newy);

    oldx = from_x - fix_x;
    oldy = from_y - fix_y;
    oldd = sqrt(oldx * oldx + oldy * oldy);

    scalefact = newd / oldd;

    p0 = a->point[0];
    p1 = a->point[1];
    p2 = a->point[2];
    p0.x = fix_x + round((p0.x - fix_x) * scalefact);
    p0.y = fix_y + round((p0.y - fix_y) * scalefact);
    p1.x = fix_x + round((p1.x - fix_x) * scalefact);
    p1.y = fix_y + round((p1.y - fix_y) * scalefact);
    p2.x = fix_x + round((p2.x - fix_x) * scalefact);
    p2.y = fix_y + round((p2.y - fix_y) * scalefact);

    pw_vector(canvas_win, p0.x, p0.y, p1.x, p1.y,
	      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
    pw_vector(canvas_win, p1.x, p1.y, p2.x, p2.y,
	      INV_PAINT, 1, RUBBER_LINE, 0.0, DEFAULT);
    length_msg2(p0.x, p0.y, p2.x, p2.y, p1.x, p1.y);
}

/*************************** TEXT *************************/

moving_text(x, y)
    int		    x, y;
{
    elastic_movetext();
    adjust_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_movetext();
    length_msg(MSG_DIST);
}

/* use x1off, y1off so that the beginning of the text isn't
   shifted under the cursor */

elastic_movetext()
{
    pw_text(canvas_win, cur_x + x1off, cur_y + y1off, INV_PAINT,
	    new_t->fontstruct, new_t->angle, 
	    new_t->cstring, new_t->color);
}


/*************************** SPLINES *************************/

moving_spline(x, y)
    int		    x, y;
{
    elastic_moveline(new_s->points);
    adjust_pos(x, y, fix_x, fix_y, &cur_x, &cur_y);
    elastic_moveline(new_s->points);
    length_msg(MSG_DIST);
}

/*********** AUXILIARY FUNCTIONS FOR CONSTRAINED MOVES ******************/

adjust_box_pos(curs_x, curs_y, orig_x, orig_y, ret_x, ret_y)
    int		    curs_x, curs_y, orig_x, orig_y;
    int		   *ret_x, *ret_y;
{
    int		    xx, sgn_csr2fix_x, yy, sgn_csr2fix_y;
    double	    mag_csr2fix_x, mag_csr2fix_y;

    switch (constrained) {
    case MOVE_ARB:
	*ret_x = curs_x;
	*ret_y = curs_y;
	break;
    case BOX_HSTRETCH:
	*ret_x = curs_x;
	*ret_y = orig_y;
	break;
    case BOX_VSTRETCH:
	*ret_x = orig_x;
	*ret_y = curs_y;
	break;
    default:
	/* calculate where scaled and stretched box corners would be */
	xx = curs_x - fix_x;
	sgn_csr2fix_x = signof(xx);
	mag_csr2fix_x = (double) abs(xx);

	yy = curs_y - fix_y;
	sgn_csr2fix_y = signof(yy);
	mag_csr2fix_y = (double) abs(yy);

	if (mag_csr2fix_x * sina > mag_csr2fix_y * cosa) {	/* above diagonal */
	    *ret_x = curs_x;
	    if (constrained == BOX_SCALE) {
		if (cosa == 0.0) {
		    *ret_y = fix_y + sgn_csr2fix_y * (int) (mag_csr2fix_x);
		} else {
		    *ret_y = fix_y + sgn_csr2fix_y * (int) (mag_csr2fix_x * sina / cosa);
		}
	    } else {
		    *ret_y = fix_y + sgn_csr2fix_y * abs(fix_y - orig_y);
		 }
	} else {
	    *ret_y = curs_y;
	    if (constrained == BOX_SCALE) {
		if (sina == 0.0) {
		    *ret_x = fix_x + sgn_csr2fix_x * (int) (mag_csr2fix_y);
		} else {
		    *ret_x = fix_x + sgn_csr2fix_x * (int) (mag_csr2fix_y * cosa / sina);
		}
	    } else {
		*ret_x = fix_x + sgn_csr2fix_x * abs(fix_x - orig_x);
	    }
	}
    } /* switch */
}

adjust_pos(curs_x, curs_y, orig_x, orig_y, ret_x, ret_y)
    int		    curs_x, curs_y, orig_x, orig_y;
    int		   *ret_x, *ret_y;
{
    if (constrained) {
	if (abs(orig_x - curs_x) > abs(orig_y - curs_y)) {
	    *ret_x = curs_x;
	    *ret_y = orig_y;
	} else {
	    *ret_x = orig_x;
	    *ret_y = curs_y;
	}
    } else {
	*ret_x = curs_x;
	*ret_y = curs_y;
    }
}
