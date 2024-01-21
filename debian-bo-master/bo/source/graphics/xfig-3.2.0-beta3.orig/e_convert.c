/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Parts Copyright (c) 1994 by Brian V. Smith
 * Parts Copyright (c) 1991 by Paul King
 * Parts Copyright (c) 1995 by C. Blanc and C. Schlick
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
#include "e_convert.h"
#include "u_create.h"
#include "u_draw.h"
#include "u_list.h"
#include "u_search.h"
#include "w_canvas.h"
#include "w_mousefun.h"
#include "d_spline.h"

static void	init_convert_line_spline();
static void	init_convert_open_closed();

convert_selected()
{
    set_mousefun("spline<->line", "", "", "", "", "");
    canvas_kbd_proc = null_proc;
    canvas_locmove_proc = null_proc;
    init_searchproc_left(init_convert_line_spline);
    init_searchproc_right(init_convert_open_closed);
    canvas_leftbut_proc = object_search_left;
    canvas_middlebut_proc = null_proc;
    canvas_rightbut_proc = point_search_right;
    set_cursor(pick15_cursor);
}

static void
init_convert_open_closed(obj, type, x, y, p, q)
     char	   *obj;
     int	    type, x, y;
     F_point       *p, *q;
{
    switch (type) {
    case O_POLYLINE:
      cur_l = (F_line *) obj;
      toggle_polyline_polygon(cur_l, p, q);
	break;
    case O_SPLINE:
      cur_s = (F_spline *) obj;
      toggle_open_closed_spline(cur_s, p, q);
	break;
    default:
	return;
    }
}

static void
init_convert_line_spline(p, type, x, y, px, py)
    char	   *p;
    int		    type;
    int		    x, y;
    int		    px, py;
{
    static int flag = 0;

    switch (type) {
    case O_POLYLINE:
	cur_l = (F_line *) p;
	/* the search routine will ensure that we don't have a box */
	line_spline(cur_l, cur_l->type == T_POLYGON ?
		    flag ? T_CLOSED_APPROX : T_CLOSED_INTERP :
		    flag ? T_OPEN_APPROX : T_OPEN_INTERP);
	break;
    case O_SPLINE:
	cur_s = (F_spline *) p;
	flag = (cur_s->type==T_OPEN_INTERP) || (cur_s->type==T_CLOSED_INTERP);
	spline_line(cur_s);
	break;
    default:
	return;
    }
}

void
line_spline(l, type_value)
    F_line	   *l;
    int          type_value;
{
    F_spline	   *s;

    if (num_points(l->points) < CLOSED_SPLINE_MIN_NUM_POINTS) {
	put_msg("Not enough points for a spline");
	return;
    }

    if ((s = create_spline()) == NULL)
        return;
    s->type = type_value;

    if (l->type == T_POLYGON)
	s->points = l->points->next;
    else
	s->points = l->points;

    s->style = l->style;
    s->thickness = l->thickness;
    s->pen_color = l->pen_color;
    s->fill_color = l->fill_color;
    s->depth = l->depth;
    s->style_val = l->style_val;
    s->cap_style = l->cap_style;
    s->pen_style = l->pen_style;
    s->fill_style = l->fill_style;
    s->for_arrow = l->for_arrow;
    s->back_arrow = l->back_arrow;
    s->sfactors = NULL;
    s->next = NULL;

    /* A spline must have an s parameter for each point */
    if (!make_sfactors(s)) {
	free_spline(s);
	return;
    }

    /* Get rid of the line and draw the new spline */
    mask_toggle_linemarker(l);
    draw_line(l, ERASE);
    list_delete_line(&objects.lines, l);
    /* for spline we reuse the arrows and points, so 'detach' them from the line */
    l->for_arrow = l->back_arrow = NULL;
    l->points = NULL;
    /* now get rid of the rest */
    free_linestorage(l);

    /* now put back the new spline */
    mask_toggle_splinemarker(s);
    list_add_spline(&objects.splines, s);
    redisplay_spline(s);
    clean_up();
    set_action_object(F_CONVERT, O_POLYLINE);
    set_latestspline(s);
    set_modifiedflag();
}

void
spline_line(s)
    F_spline	   *s;
{
    F_line	   *l;
    F_point        *tmppoint;

    /* Now we turn s into a line */
    if ((l = create_line()) == NULL)
	return;

    if (open_spline(s)) {
	l->type = T_POLYLINE;
	l->points = s->points;
    } else {
	l->type = T_POLYGON;
	if ((l->points = create_point())==NULL)
	    return;
	tmppoint = last_point(s->points);
	l->points->x = tmppoint->x;
	l->points->y = tmppoint->y;
	l->points->next = s->points;
    }
    l->style = s->style;
    l->thickness = s->thickness;
    l->pen_color = s->pen_color;
    l->fill_color = s->fill_color;
    l->depth = s->depth;
    l->style_val = s->style_val;
    l->cap_style = s->cap_style;
    l->join_style = cur_joinstyle;
    l->pen_style = s->pen_style;
    l->radius = DEFAULT;
    l->fill_style = s->fill_style;
    l->for_arrow = s->for_arrow;
    l->back_arrow = s->back_arrow;

    /* now we have finished creating the line, we can get rid of the spline */
    /* first off the screen */
    mask_toggle_splinemarker(s);
    draw_spline(s, ERASE);
    list_delete_spline(&objects.splines, s);
    /* we reuse the arrows and points, so `detach' them from the spline */
    s->for_arrow = s->back_arrow = NULL;
    s->points = NULL;
    /* now get rid of the rest */
    free_splinestorage(s);

    /* and put in the new line */
    mask_toggle_linemarker(l);
    list_add_line(&objects.lines, l);
    redisplay_line(l);
    clean_up();
    set_action_object(F_CONVERT, O_SPLINE);
    set_latestline(l);
    set_modifiedflag();
    return;
}

void
toggle_polyline_polygon(line, previous_point, selected_point)
     F_line  *line;
     F_point *previous_point, *selected_point;
{
  F_point *point, *last_pt;
  
  last_pt = last_point(line->points);

  if (line->type == T_POLYLINE)
    {

      if ((point = create_point()) == NULL)
	return;
      
      point->x = last_pt->x;
      point->y = last_pt->y;
      point->next = line->points;
      line->points = point;
      
      line->type = T_POLYGON;
      clean_up();
      set_last_arrows(line->for_arrow, line->back_arrow);
      line->back_arrow = line->for_arrow = NULL;
    }
  else if (line->type == T_POLYGON)
    {
      point = line->points;
      line->points = point->next;           /* unchain the first point */
      free((char *) point);
            
      if ((line->points != selected_point) && (previous_point != NULL))
	{
	  last_pt->next = line->points;     /* let selected point become */
	  previous_point->next = NULL;      /* first point */
	  line->points = selected_point;
	}
      line->type = T_POLYLINE;
      clean_up();
    }
  redisplay_line(line);
  set_action_object(F_OPEN_CLOSE, O_POLYLINE);
  set_last_selectedpoint(line->points);
  set_last_prevpoint(NULL);
  set_latestline(line);
  set_modifiedflag();
}


void
toggle_open_closed_spline(spline, previous_point, selected_point)
     F_spline *spline;
     F_point  *previous_point, *selected_point;
{
  F_point *last_pt;
  F_sfactor *last_sfactor, *previous_sfactor, *selected_sfactor;

  last_pt = last_point(spline->points);
  last_sfactor = search_sfactor(spline, last_pt);

  if (previous_point == NULL)
    {
      previous_sfactor = NULL;
      selected_sfactor = spline->sfactors;
    }
  else
    {
      previous_sfactor = search_sfactor(spline, previous_point);
      selected_sfactor = previous_sfactor->next;
      set_last_tension(selected_sfactor->s, previous_sfactor->s);
    }

  draw_spline(spline, ERASE);

  if (closed_spline(spline))
    {      
      if (spline->points != selected_point)
	{
	  last_pt->next = spline->points;
	  last_sfactor->next = spline->sfactors;
	  previous_point->next = NULL;
	  previous_sfactor->next = NULL;
	  previous_sfactor->s = S_SPLINE_ANGULAR;
	  spline->points = selected_point;
	  spline->sfactors = selected_sfactor;
	}
      else
	{
	  last_sfactor->s = S_SPLINE_ANGULAR;
	}
      spline->sfactors->s = S_SPLINE_ANGULAR;
      spline->type = (x_spline(spline)) ? T_OPEN_XSPLINE :
	(int_spline(spline)) ? T_OPEN_INTERP : T_OPEN_APPROX;
      clean_up();
    }
  else
    {
      int type_tmp;
      double s_tmp;

      if(int_spline(spline))
	{
	  s_tmp = S_SPLINE_INTERP;
	  type_tmp = T_CLOSED_INTERP;
	}
      else
	if (x_spline(spline))
	  {
	      s_tmp = S_SPLINE_INTERP;
	      type_tmp = T_CLOSED_XSPLINE;
	    }
	else
	  {
	    s_tmp = S_SPLINE_APPROX;
	    type_tmp = T_CLOSED_APPROX;
	  }
      spline->sfactors->s = last_sfactor->s = s_tmp;
      spline->type = type_tmp;
      clean_up();
      set_last_arrows(spline->for_arrow, spline->back_arrow);
      spline->back_arrow = spline->for_arrow = NULL;
    }
  draw_spline(spline, PAINT);
  set_action_object(F_OPEN_CLOSE, O_SPLINE);
  set_last_selectedpoint(spline->points);
  set_last_prevpoint(NULL);
  set_latestspline(spline);
  set_modifiedflag();
}

