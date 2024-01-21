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

#include <stdlib.h>

#include "fig.h"
#include "resources.h"
#include "object.h"
#include "mode.h"
#include "paintop.h"
#include "u_bound.h"
#include "w_setup.h"
#include "w_zoom.h"

#define		Ninety_deg		M_PI_2
#define		One_eighty_deg		M_PI
#define		Two_seventy_deg		(M_PI + M_PI_2)
#define		Three_sixty_deg		(M_PI + M_PI)
#define		half(z1 ,z2)		((z1+z2)/2.0)
#define         MIN_MAX(X1, X2, Y1, Y2)  \
                *xmax = max2(*xmax, X1); \
         	*xmin = min2(*xmin, X2); \
	        *ymax = max2(*ymax, Y1); \
	        *ymin = min2(*ymin, Y2)

static void	points_bound();
static void	general_spline_bound();
static void	approx_spline_bound();

arc_bound(arc, xmin, ymin, xmax, ymax)
    F_arc	   *arc;
    int		   *xmin, *ymin, *xmax, *ymax;
{
    float	    alpha, beta;
    double	    dx, dy, radius;
    int		    bx, by, sx, sy;
    int		    half_wd;

    dx = arc->point[0].x - arc->center.x;
    dy = arc->center.y - arc->point[0].y;
    if (dx==0.0)
	alpha = (dy > 0? Ninety_deg: -Ninety_deg);
    else
	alpha = atan2(dy, dx);

    if (alpha < 0.0)
	alpha += Three_sixty_deg;

    radius = sqrt(dx * dx + dy * dy);

    dx = arc->point[2].x - arc->center.x;
    dy = arc->center.y - arc->point[2].y;
    if (dx==0.0)
	beta = (dy > 0? Ninety_deg: -Ninety_deg);
    else
	beta = atan2(dy, dx);

    if (beta < 0.0)
	beta += Three_sixty_deg;

    bx = max2(arc->point[0].x, arc->point[1].x);
    bx = max2(arc->point[2].x, bx);
    by = max2(arc->point[0].y, arc->point[1].y);
    by = max2(arc->point[2].y, by);
    sx = min2(arc->point[0].x, arc->point[1].x);
    sx = min2(arc->point[2].x, sx);
    sy = min2(arc->point[0].y, arc->point[1].y);
    sy = min2(arc->point[2].y, sy);

    if (arc->direction == 1) {	/* counter clockwise */
	if (alpha > beta) {
	    if (alpha <= 0 || 0 <= beta)
		bx = (int) (arc->center.x + radius + 1.0);
	    if (alpha <= Ninety_deg || Ninety_deg <= beta)
		sy = (int) (arc->center.y - radius - 1.0);
	    if (alpha <= One_eighty_deg || One_eighty_deg <= beta)
		sx = (int) (arc->center.x - radius - 1.0);
	    if (alpha <= Two_seventy_deg || Two_seventy_deg <= beta)
		by = (int) (arc->center.y + radius + 1.0);
	} else {
	    if (0 <= beta && alpha <= 0)
		bx = (int) (arc->center.x + radius + 1.0);
	    if (Ninety_deg <= beta && alpha <= Ninety_deg)
		sy = (int) (arc->center.y - radius - 1.0);
	    if (One_eighty_deg <= beta && alpha <= One_eighty_deg)
		sx = (int) (arc->center.x - radius - 1.0);
	    if (Two_seventy_deg <= beta && alpha <= Two_seventy_deg)
		by = (int) (arc->center.y + radius + 1.0);
	}
    } else {			/* clockwise	 */
	if (alpha > beta) {
	    if (beta <= 0 && 0 <= alpha)
		bx = (int) (arc->center.x + radius + 1.0);
	    if (beta <= Ninety_deg && Ninety_deg <= alpha)
		sy = (int) (arc->center.y - radius - 1.0);
	    if (beta <= One_eighty_deg && One_eighty_deg <= alpha)
		sx = (int) (arc->center.x - radius - 1.0);
	    if (beta <= Two_seventy_deg && Two_seventy_deg <= alpha)
		by = (int) (arc->center.y + radius + 1.0);
	} else {
	    if (0 <= alpha || beta <= 0)
		bx = (int) (arc->center.x + radius + 1.0);
	    if (Ninety_deg <= alpha || beta <= Ninety_deg)
		sy = (int) (arc->center.y - radius - 1.0);
	    if (One_eighty_deg <= alpha || beta <= One_eighty_deg)
		sx = (int) (arc->center.x - radius - 1.0);
	    if (Two_seventy_deg <= alpha || beta <= Two_seventy_deg)
		by = (int) (arc->center.y + radius + 1.0);
	}
    }
    half_wd = arc->thickness / 2 * ZOOM_FACTOR;
    *xmax = bx + half_wd;
    *ymax = by + half_wd;
    *xmin = sx - half_wd;
    *ymin = sy - half_wd;

    /* show the boundaries */
    if (appres.DEBUG) {
	pw_vector(canvas_win, *xmin, *ymin, *xmax, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, *xmax, *ymin, *xmax, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, *xmax, *ymax, *xmin, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, *xmin, *ymax, *xmin, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
    }

    /* now add in the arrow (if any) boundaries */
    arrow_bound(O_ARC, (F_line *)arc, xmin, ymin, xmax, ymax);
}

compound_bound(compound, xmin, ymin, xmax, ymax)
    F_compound	   *compound;
    int		   *xmin, *ymin, *xmax, *ymax;
{
    F_arc	   *a;
    F_ellipse	   *e;
    F_compound	   *c;
    F_spline	   *s;
    F_line	   *l;
    F_text	   *t;
    int		    bx, by, sx, sy, first = 1;
    int		    llx, lly, urx, ury;

    llx = lly = urx = ury = 0;

    for (a = compound->arcs; a != NULL; a = a->next) {
	arc_bound(a, &sx, &sy, &bx, &by);
	if (first) {
	    first = 0;
	    llx = sx;
	    lly = sy;
	    urx = bx;
	    ury = by;
	} else {
	    llx = min2(llx, sx);
	    lly = min2(lly, sy);
	    urx = max2(urx, bx);
	    ury = max2(ury, by);
	}
    }

    for (c = compound->compounds; c != NULL; c = c->next) {
	sx = c->nwcorner.x;
	sy = c->nwcorner.y;
	bx = c->secorner.x;
	by = c->secorner.y;
	if (first) {
	    first = 0;
	    llx = sx;
	    lly = sy;
	    urx = bx;
	    ury = by;
	} else {
	    llx = min2(llx, sx);
	    lly = min2(lly, sy);
	    urx = max2(urx, bx);
	    ury = max2(ury, by);
	}
    }

    for (e = compound->ellipses; e != NULL; e = e->next) {
	ellipse_bound(e, &sx, &sy, &bx, &by);
	if (first) {
	    first = 0;
	    llx = sx;
	    lly = sy;
	    urx = bx;
	    ury = by;
	} else {
	    llx = min2(llx, sx);
	    lly = min2(lly, sy);
	    urx = max2(urx, bx);
	    ury = max2(ury, by);
	}
    }

    for (l = compound->lines; l != NULL; l = l->next) {
	line_bound(l, &sx, &sy, &bx, &by);
	if (first) {
	    first = 0;
	    llx = sx;
	    lly = sy;
	    urx = bx;
	    ury = by;
	} else {
	    llx = min2(llx, sx);
	    lly = min2(lly, sy);
	    urx = max2(urx, bx);
	    ury = max2(ury, by);
	}
    }

    for (s = compound->splines; s != NULL; s = s->next) {
	spline_bound(s, &sx, &sy, &bx, &by);
	if (first) {
	    first = 0;
	    llx = sx;
	    lly = sy;
	    urx = bx;
	    ury = by;
	} else {
	    llx = min2(llx, sx);
	    lly = min2(lly, sy);
	    urx = max2(urx, bx);
	    ury = max2(ury, by);
	}
    }

    for (t = compound->texts; t != NULL; t = t->next) {
	int    dum;
	text_bound(t, &sx, &sy, &bx, &by,
		  &dum,&dum,&dum,&dum,&dum,&dum,&dum,&dum);
	if (first) {
	    first = 0;
	    llx = sx;
	    lly = sy;
	    urx = bx;
	    ury = by;
	} else {
	    llx = min2(llx, sx);
	    lly = min2(lly, sy);
	    urx = max2(urx, bx);
	    ury = max2(ury, by);
	}
    }

    /* round the corners to the current positioning grid */
    floor_coords(llx);
    floor_coords(lly);
    ceil_coords(urx);
    ceil_coords(ury);
    *xmin = llx;
    *ymin = lly;
    *xmax = urx;
    *ymax = ury;
    /* show the boundaries */
    if (appres.DEBUG) {
	pw_vector(canvas_win, *xmin, *ymin, *xmax, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, *xmax, *ymin, *xmax, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, *xmax, *ymax, *xmin, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, *xmin, *ymax, *xmin, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
    }
}

/* basically, use the code for drawing the ellipse to find its bounds */
/* From James Tough (see u_draw.c: angle_ellipse() */
/* include the bounds for the markers (even though we don't know if they
   are on or off now */

ellipse_bound(e, xmin, ymin, xmax, ymax)
    F_ellipse	   *e;
    int		   *xmin, *ymin, *xmax, *ymax;
{
	int	    half_wd;
	double	    c1, c2, c3, c4, c5, c6, v1, cphi, sphi, cphisqr, sphisqr;
	double	    xleft, xright, d, asqr, bsqr;
	int	    yymax, yy=0;
	float	    xcen, ycen, a, b;

	xcen = e->center.x;
	ycen = e->center.y;
	a = e->radiuses.x;
	b = e->radiuses.y;
	if (a==0 || b==0) {
	    *xmin = *xmax = xcen;
	    *ymin = *ymax = ycen;
	    return;
	}
	/* angle of 0 is easy */
	if (e->angle == 0) {
	    half_wd = e->thickness/2*ZOOM_FACTOR;
	    *xmin = xcen - a - half_wd;
	    *xmax = xcen + a + half_wd;
	    *ymin = ycen - b - half_wd;
	    *ymax = ycen + b + half_wd;
	    /* show the boundaries */
	    if (appres.DEBUG) {
		pw_vector(canvas_win, *xmin, *ymin, *xmax, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
		pw_vector(canvas_win, *xmax, *ymin, *xmax, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
		pw_vector(canvas_win, *xmax, *ymax, *xmin, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
		pw_vector(canvas_win, *xmin, *ymax, *xmin, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
	    }
	    return;
	}

	/* divide by ZOOM_FACTOR because we don't need such precision */
	a /= ZOOM_FACTOR;
	b /= ZOOM_FACTOR;
	xcen /= ZOOM_FACTOR;
	ycen /= ZOOM_FACTOR;

	cphi = cos((double)e->angle);
	sphi = sin((double)e->angle);
	cphisqr = cphi*cphi;
	sphisqr = sphi*sphi;
	asqr = a*a;
	bsqr = b*b;
	
	c1 = (cphisqr/asqr)+(sphisqr/bsqr);
	c2 = ((cphi*sphi/asqr)-(cphi*sphi/bsqr))/c1;
	c3 = (bsqr*cphisqr) + (asqr*sphisqr);
	yymax = sqrt(c3);
	c4 = a*b/c3;
	c5 = 0;
	v1 = c4*c4;
	c6 = 2*v1;
	c3 = c3*v1-v1;
	/* odd first points */
	*xmin = *ymin =  100000;
	*xmax = *ymax = -100000;
	if (yymax % 2) {
		d = sqrt(c3);
		*xmin = min2(*xmin,xcen-d);
		*xmax = max2(*xmax,xcen+d);
		*ymin = min2(*ymin,ycen);
		*ymax = max2(*ymax,ycen);
		c5 = c2;
		yy=1;
	}
	while (c3>=0) {
		d = sqrt(c3);
		xleft = c5-d;
		xright = c5+d;
		*xmin = min2(*xmin,xcen+xleft);
		*xmax = max2(*xmax,xcen+xleft);
		*ymax = max2(*ymax,ycen+yy);
		*xmin = min2(*xmin,xcen+xright);
		*xmax = max2(*xmax,xcen+xright);
		*ymax = max2(*ymax,ycen+yy);
		*xmin = min2(*xmin,xcen-xright);
		*xmax = max2(*xmax,xcen-xright);
		*ymin = min2(*ymin,ycen-yy);
		*xmin = min2(*xmin,xcen-xleft);
		*xmax = max2(*xmax,xcen-xleft);
		*ymin = min2(*ymin,ycen-yy);
		c5+=c2;
		v1+=c6;
		c3-=v1;
		yy=yy+1;
	}
	/* for simplicity, just add half the line thickness to xmax and ymax
	   and subtract half from xmin and ymin */
	half_wd = e->thickness/2;
	*xmax += half_wd;
	*ymax += half_wd;
	*xmin -= half_wd;
	*ymin -= half_wd;
	/* now include the markers because they could be outside the bounds of
	   the ellipse (+/-3 is (roughly) half the size of the markers (5)) */
	/* and multiply back to real coordinates */
	*xmax = max2(*xmax*ZOOM_FACTOR, max2(e->start.x, e->end.x)+3);
	*ymax = max2(*ymax*ZOOM_FACTOR, max2(e->start.y, e->end.y)+3);
	*xmin = min2(*xmin*ZOOM_FACTOR, min2(e->start.x, e->end.x)-3);
	*ymin = min2(*ymin*ZOOM_FACTOR, min2(e->start.y, e->end.y)-3);
	/* show the boundaries */
	if (appres.DEBUG) {
	    pw_vector(canvas_win, *xmin, *ymin, *xmax, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
	    pw_vector(canvas_win, *xmax, *ymin, *xmax, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
	    pw_vector(canvas_win, *xmax, *ymax, *xmin, *ymax, PAINT, 1, RUBBER_LINE, 0.0, RED);
	    pw_vector(canvas_win, *xmin, *ymax, *xmin, *ymin, PAINT, 1, RUBBER_LINE, 0.0, RED);
	}
}

line_bound(l, xmin, ymin, xmax, ymax)
    F_line	   *l;
    int		   *xmin, *ymin, *xmax, *ymax;
{
    points_bound(l->points, (l->thickness / 2), xmin, ymin, xmax, ymax);
    /* now add in the arrow (if any) boundaries */
    arrow_bound(O_POLYLINE, l, xmin, ymin, xmax, ymax);
}

spline_bound(s, xmin, ymin, xmax, ymax)
    F_spline	   *s;
    int		   *xmin, *ymin, *xmax, *ymax;
{
    if (approx_spline(s))
	approx_spline_bound(s, xmin, ymin, xmax, ymax);
    else
	general_spline_bound(s, xmin, ymin, xmax, ymax);

    *xmax += s->thickness>>1;
    *xmin -= s->thickness>>1;
    *ymax += s->thickness>>1;
    *ymin -= s->thickness>>1;

    /* now add in the arrow (if any) boundaries */
    arrow_bound(O_SPLINE, (F_line *)s, xmin, ymin, xmax, ymax);
}

static void
general_spline_bound(s, xmin, ymin, xmax, ymax)
    F_spline      *s;
    int	          *xmin, *ymin, *xmax, *ymax;
{
  F_point   *cur_point, *previous_point, *next_point;
  F_sfactor *cur_sfactor;     
  int       x0, y0, x1, y1, x2, y2 ,x ,y;
  
  previous_point = cur_point = s->points;
  cur_sfactor = s->sfactors;
  *xmin = *xmax = x0 = x1 = cur_point->x;
  *ymin = *ymax = y0 = y1 = cur_point->y;
  next_point = cur_point->next;
  x2 = next_point->x;
  y2 = next_point->y;
  
  while (1)
    {   
      previous_point = cur_point;
      cur_point = next_point;
      next_point = next_point->next;

      if (next_point == NULL)
	next_point = s->points;           /* usefull for closed splines,
					     no consequences on open splines */
      cur_sfactor = cur_sfactor->next;

      x0 = x1;
      y0 = y1;
      x1 = x2;
      y1 = y2;
      x2 = next_point->x;
      y2 = next_point->y;
      
      if (cur_sfactor->s < 0)
	{
	  x = abs(x2 - x0)>>2;
	  y = abs(y2 - y0)>>2;
	}
      else
	{
	  x = y = 0;
	}
      
      MIN_MAX((x1+x), (x1-x), (y1+y), (y1-y));
      if (cur_point->next==NULL)
	break;
    }
}

static void
approx_spline_bound(s, xmin, ymin, xmax, ymax)
    F_spline	   *s;
    int		   *xmin, *ymin, *xmax, *ymax;
{
    F_point	   *p;
    int		    px, py;

    p = s->points;
    *xmin = *xmax = p->x;
    *ymin = *ymax = p->y;

    for (p=p->next ; p!=NULL ; p=p->next)
      {
	px = p->x;
	py = p->y;
	MIN_MAX(px, px, py, py);
      }
}

/* This procedure calculates the bounding box for text.  It returns
   the min/max x and y coords of the enclosing HORIZONTAL rectangle.
   The actual corners of the rectangle are returned in (rx1,ry1)...(rx4,ry4)
 */

text_bound(t, xmin, ymin, xmax, ymax,
		  rx1, ry1, rx2, ry2, rx3, ry3, rx4, ry4)
    F_text	   *t;
    int		   *xmin, *ymin, *xmax, *ymax;
    int		   *rx1,*ry1, *rx2,*ry2, *rx3,*ry3, *rx4,*ry4;
{
    int		    h, l;
    int		    x1,y1, x2,y2, x3,y3, x4,y4;
    double	    cost, sint;
    double	    dcost, dsint, lcost, lsint, hcost, hsint;

    cost = cos((double)t->angle);
    sint = sin((double)t->angle);
    l = text_length(t);
    h = t->ascent+t->descent;
    lcost = round(l*cost);
    lsint = round(l*sint);
    hcost = round(h*cost);
    hsint = round(h*sint);
    dcost = round(t->descent*cost);
    dsint = round(t->descent*sint);
    x1 = t->base_x+dsint;
    y1 = t->base_y+dcost;
    if (t->type == T_CENTER_JUSTIFIED) {
	x1 = t->base_x+dsint - round((l/2)*cost);
	y1 = t->base_y+dcost + round((l/2)*sint);
	x2 = x1 + lcost;
	y2 = y1 - lsint;
    }
    else if (t->type == T_RIGHT_JUSTIFIED) {
	x1 = t->base_x+dsint - lcost;
	y1 = t->base_y+dcost + lsint;
	x2 = t->base_x+dsint;
	y2 = t->base_y+dcost;
    }
    else {
	x2 = x1 + lcost;
	y2 = y1 - lsint;
    }
    x4 = x1 - hsint;
    y4 = y1 - hcost;
    x3 = x2 - hsint;
    y3 = y2 - hcost;

    *xmin = min2(x1,min2(x2,min2(x3,x4)));
    *xmax = max2(x1,max2(x2,max2(x3,x4)));
    *ymin = min2(y1,min2(y2,min2(y3,y4)));
    *ymax = max2(y1,max2(y2,max2(y3,y4)));
    *rx1=x1; *ry1=y1;
    *rx2=x2; *ry2=y2;
    *rx3=x3; *ry3=y3;
    *rx4=x4; *ry4=y4;
}

static void
points_bound(points, half_wd, xmin, ymin, xmax, ymax)
    F_point	   *points;
    int		    half_wd;
    int		   *xmin, *ymin, *xmax, *ymax;
{
    int		    bx, by, sx, sy;
    F_point	   *p;

    bx = sx = points->x;
    by = sy = points->y;
    for (p = points->next; p != NULL; p = p->next) {
	sx = min2(sx, p->x);
	sy = min2(sy, p->y);
	bx = max2(bx, p->x);
	by = max2(by, p->y);
    }
    half_wd *= ZOOM_FACTOR;
    *xmin = sx - half_wd;
    *ymin = sy - half_wd;
    *xmax = bx + half_wd;
    *ymax = by + half_wd;
}

int
overlapping(xmin1, ymin1, xmax1, ymax1, xmin2, ymin2, xmax2, ymax2)
    int		    xmin1, ymin1, xmax1, ymax1, xmin2, ymin2, xmax2, ymax2;
{
    if (xmin1 < xmin2)
	if (ymin1 < ymin2)
	    return (xmax1 >= xmin2 && ymax1 >= ymin2);
	else
	    return (xmax1 >= xmin2 && ymin1 <= ymax2);
    else if (ymin1 < ymin2)
	return (xmin1 <= xmax2 && ymax1 >= ymin2);
    else
	return (xmin1 <= xmax2 && ymin1 <= ymax2);
}

/* extend xmin, ymin xmax, ymax by the arrow boundaries of obj (if any) */

arrow_bound(objtype, obj, xmin, ymin, xmax, ymax)
    int		    objtype;
    F_line	   *obj;
    int		   *xmin, *ymin, *xmax, *ymax;
{
    int		    fxmin, fymin, fxmax, fymax;
    int		    bxmin, bymin, bxmax, bymax;
    F_point	   *p, *q;
    F_arc	   *a;
    int		    p1x, p1y, p2x, p2y, opx, opy;
    int		    dum;
    zXPoint	    arrowpts[6];
    int		    npts, i, wd;

    if (obj->for_arrow) {
	if (objtype == O_ARC) {
	    a = (F_arc *) obj;
	    compute_arcarrow_angle(a->center.x, a->center.y, a->point[2].x,
		       a->point[2].y, a->direction, a->for_arrow, &p1x, &p1y);
	    p2x = a->point[2].x;	/* forward tip */
	    p2y = a->point[2].y;
	} else {
	    /* this doesn't work very well for a spline with few points 
		and lots of curvature */
	    /* locate last point (forward tip) and next-to-last point */
	    for (p = obj->points; p->next; p = p->next)
		q = p;
	    p1x = q->x;
	    p1y = q->y;
	    p2x = p->x;
	    p2y = p->y;
	}
	opx = p2x;
	opy = p2y;
	calc_arrow(p1x, p1y, p2x, p2y, &dum, &dum, &dum, &dum,
			objtype, obj->for_arrow, arrowpts, &npts);
	fxmin=fymin=100000;
	fxmax=fymax=-100000;
	for (i=0; i<npts; i++) {
	    fxmin = min2(fxmin, arrowpts[i].x);
	    fymin = min2(fymin, arrowpts[i].y);
	    fxmax = max2(fxmax, arrowpts[i].x);
	    fymax = max2(fymax, arrowpts[i].y);
	}
	/* the original endpoint of the line is further */
	wd = obj->for_arrow->thickness*ZOOM_FACTOR;
	fxmin = min2(fxmin, opx) - wd;
	fxmax = max2(fxmax, opx) + wd;
	fymin = min2(fymin, opy) - wd;
	fymax = max2(fymax, opy) + wd;
	*xmin = min2(*xmin, fxmin);
	*xmax = max2(*xmax, fxmax);
	*ymin = min2(*ymin, fymin);
	*ymax = max2(*ymax, fymax);
	if (appres.DEBUG) {
	  pw_vector(canvas_win,fxmin,fymin,fxmax,fymin,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	  pw_vector(canvas_win,fxmax,fymin,fxmax,fymax,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	  pw_vector(canvas_win,fxmax,fymax,fxmin,fymax,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	  pw_vector(canvas_win,fxmin,fymax,fxmin,fymin,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	}
    }
    if (obj->back_arrow) {
	if (objtype == O_ARC) {
	    a = (F_arc *) obj;
	    compute_arcarrow_angle(a->center.x, a->center.y, a->point[0].x,
		       a->point[0].y, a->direction ^ 1, a->back_arrow, &p1x, &p1y);
	    p2x = a->point[0].x;	/* backward tip */
	    p2y = a->point[0].y;
	} else {
	    p1x = obj->points->next->x;	/* second point */
	    p1y = obj->points->next->y;
	    p2x = obj->points->x;	/* first point (forward tip) */
	    p2y = obj->points->y;
	}
	opx = p2x;
	opy = p2y;
	calc_arrow(p1x, p1y, p2x, p2y, &dum, &dum, &dum, &dum,
			objtype, obj->back_arrow, arrowpts, &npts);
	bxmin=bymin=100000;
	bxmax=bymax=-100000;
	for (i=0; i<npts; i++) {
	    bxmin = min2(bxmin, arrowpts[i].x);
	    bymin = min2(bymin, arrowpts[i].y);
	    bxmax = max2(bxmax, arrowpts[i].x);
	    bymax = max2(bymax, arrowpts[i].y);
	}
	/* the original endpoint of the line is further */
	wd = obj->back_arrow->thickness*ZOOM_FACTOR;
	bxmin = min2(bxmin, opx) - wd;
	bxmax = max2(bxmax, opx) + wd;
	bymin = min2(bymin, opy) - wd;
	bymax = max2(bymax, opy) + wd;
	*xmin = min2(*xmin, bxmin);
	*xmax = max2(*xmax, bxmax);
	*ymin = min2(*ymin, bymin);
	*ymax = max2(*ymax, bymax);
	if (appres.DEBUG) {
	  pw_vector(canvas_win,bxmin,bymin,bxmax,bymin,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	  pw_vector(canvas_win,bxmax,bymin,bxmax,bymax,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	  pw_vector(canvas_win,bxmax,bymax,bxmin,bymax,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	  pw_vector(canvas_win,bxmin,bymax,bxmin,bymin,PAINT,1,RUBBER_LINE,0.0,MAGENTA);
	}
    }
}

/* calculate the width of an arrow in the direction going from (x1,y1) to (x2,y2) */

calc_arrow_width(type, wid, ht, x1, y1, x2, y2, xmin, ymin, xmax, ymax)
    int		    type, x1, y1, x2, y2;
    int		   *xmin, *ymin, *xmax, *ymax;
    float	    wid, ht;
{
    double	    l, sina, cosa, xb, yb, xc, yc, xd, yd;
    double	    x, y, dx, dy;

    dx = x2 - x1;
    dy = y1 - y2;
    l = sqrt(dx * dx + dy * dy);
    if (l == 0)
	return;
    sina = dy / l;
    cosa = dx / l;
    xb = x2 * cosa - y2 * sina;
    yb = x2 * sina + y2 * cosa;
    /* lengthen the "height" if type 2 */
    if (type == 2)
	x = xb - ht * 1.2;
    /* shorten the "height" if type 3*/
    else if (type == 3)
	x = xb - ht * 0.8;
    else
	x = xb - ht;
    y = yb - wid / 2;
    xc = round( x * cosa + y * sina);
    yc = round(-x * sina + y * cosa);
    y = yb + wid / 2;
    xd = round( x * cosa + y * sina);
    yd = round(-x * sina + y * cosa);

    *xmin = min3(xc, xd, x2);
    *xmax = max3(xc, xd, x2);
    *ymin = min3(yc, yd, y2);
    *ymax = max3(yc, yd, y2);
}
