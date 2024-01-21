/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Copyright (c) 1990 by Brian V. Smith
 * Copyright (c) 1992 by James Tough
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
#include "object.h"
#include "paintop.h"
#include "u_bound.h"
#include "u_create.h"
#include "u_draw.h"
#include "u_list.h"
#include "w_canvas.h"
#include "w_drawprim.h"
#include "w_setup.h"
#include "w_zoom.h"

extern PIX_FONT lookfont();
extern		X_error_handler();


/* declarations for splines */

#define         HIGH_PRECISION    0.5
#define         LOW_PRECISION     1.0
#define         ZOOM_PRECISION    5.0
#define         ARROW_START       4
#define         MAX_SPLINE_STEP   0.2


#define COPY_CONTROL_POINT(P0, S0, P1, S1) \
      P0 = P1; \
      S0 = S1

#define NEXT_CONTROL_POINTS(P0, S0, P1, S1, P2, S2, P3, S3) \
      COPY_CONTROL_POINT(P0, S0, P1, S1); \
      COPY_CONTROL_POINT(P1, S1, P2, S2); \
      COPY_CONTROL_POINT(P2, S2, P3, S3); \
      COPY_CONTROL_POINT(P3, S3, P3->next, S3->next)

#define INIT_CONTROL_POINTS(SPLINE, P0, S0, P1, S1, P2, S2, P3, S3) \
      COPY_CONTROL_POINT(P0, S0, SPLINE->points, SPLINE->sfactors); \
      COPY_CONTROL_POINT(P1, S1, P0->next, S0->next);               \
      COPY_CONTROL_POINT(P2, S2, P1->next, S1->next);               \
      COPY_CONTROL_POINT(P3, S3, P2->next, S2->next)

#define SPLINE_SEGMENT_LOOP(K, P0, P1, P2, P3, S1, S2, PREC) \
      step = step_computing(K, P0, P1, P2, P3, S1, S2, PREC);    \
      spline_segment_computing(step, K, P0, P1, P2, P3, S1, S2)



static Boolean       compute_closed_spline();

static Boolean       compute_open_spline();


static void          spline_segment_computing();

static float         step_computing();

static INLINE        point_adding();

static INLINE        point_computing();

static INLINE        negative_s1_influence();

static INLINE        negative_s2_influence();

static INLINE        positive_s1_influence();

static INLINE        positive_s2_influence();

static INLINE double f_blend();

static INLINE double g_blend();

static INLINE double h_blend();





/************** POLYGON/CURVE DRAWING FACILITIES ****************/

static int	npoints;
static zXPoint *points;
static int	max_points;
static int	allocstep;




static		Boolean
init_point_array(init_size, step_size)
    int		    init_size, step_size;
{
    npoints = 0;
    max_points = init_size;
    allocstep = step_size;
    if (max_points > MAXNUMPTS) {
	max_points = MAXNUMPTS;
    }
    if ((points = (zXPoint *) malloc(max_points * sizeof(zXPoint))) == 0) {
	fprintf(stderr, "xfig: insufficient memory to allocate point array\n");
	return False;
    }
    return True;
}


too_many_points()
{
	put_msg("Too many points, recompile with MAXNUMPTS > %d in w_drawprim.h",
		MAXNUMPTS);
}


static		Boolean
add_point(x, y)
    int		    x, y;
{
    if (npoints >= max_points) {
	zXPoint	       *tmp_p;

	if (max_points >= MAXNUMPTS) {
	    max_points = MAXNUMPTS;
	    return False;		/* stop; it is not closing */
	}
	max_points += allocstep;
	if (max_points >= MAXNUMPTS)
	    max_points = MAXNUMPTS;

	if ((tmp_p = (zXPoint *) realloc(points,
					max_points * sizeof(zXPoint))) == 0) {
	    fprintf(stderr,
		    "xfig: insufficient memory to reallocate point array\n");
	    return False;
	}
	points = tmp_p;
    }
    /* ignore identical points */
    if (npoints > 0 &&
	points[npoints-1].x == x && points[npoints-1].y == y)
		return True;
    points[npoints].x = x;
    points[npoints].y = y;
    npoints++;
    return True;
}

/*
   although the "pnts" array is usually just the global "points" array,
   draw_arc() and possibly others call this procedure with a pointer to
   other than the first point in that array.
   The real array points is freed at the end of this procedure
*/


static void
draw_point_array(w, op, line_width, line_style, style_val, 
	join_style, cap_style, fill_style, pen_color, fill_color)
    Window	    w;
    int		    op;
    int		    line_width, line_style, cap_style;
    float	    style_val;
    int		    join_style, fill_style;
    Color	    pen_color, fill_color;
{
    pw_lines(w, points, npoints, op, line_width, line_style, style_val,
		join_style, cap_style, fill_style, pen_color, fill_color);
    free((char *) points);
}

/* these are for the arrowheads */
static zXPoint	    farpts[6],barpts[6];
static int	    nfpts, nbpts;

/*********************** ARC ***************************/

draw_arc(a, op)
    F_arc	   *a;
    int		    op;
{
    double	    rx, ry;
    int		    radius;
    int		    xmin, ymin, xmax, ymax;
    int		    x, y;

    arc_bound(a, &xmin, &ymin, &xmax, &ymax);
    if (!overlapping(ZOOMX(xmin), ZOOMY(ymin), ZOOMX(xmax), ZOOMY(ymax),
		     clip_xmin, clip_ymin, clip_xmax, clip_ymax))
	return;

    rx = a->point[0].x - a->center.x;
    ry = a->center.y - a->point[0].y;
    radius = round(sqrt(rx * rx + ry * ry));

    /* fill points array but don't display the points yet */

    curve(canvas_win, round(a->point[0].x - a->center.x),
	  round(a->center.y - a->point[0].y),
	  round(a->point[2].x - a->center.x),
	  round(a->center.y - a->point[2].y),
	  False,
	  (a->type == T_PIE_WEDGE_ARC),
	  a->direction, 500, radius, radius,
	  round(a->center.x), round(a->center.y), op,
	  a->thickness, a->style, a->style_val, a->fill_style,
	  a->pen_color, a->fill_color, a->cap_style);

    /* setup clipping so that spline doesn't protrude beyond arrowhead */
    /* also create the arrowheads */
    clip_arrows(a,O_ARC,op);

    /* draw the arc itself */
    draw_point_array(canvas_win, op, a->thickness,
		     a->style, a->style_val, JOIN_BEVEL,
		     a->cap_style, a->fill_style,
		     a->pen_color, a->fill_color);

    /* restore clipping */
    set_clip_window(clip_xmin, clip_ymin, clip_xmax, clip_ymax);

    /* draw the arrowheads, if any */
    if (a->type != T_PIE_WEDGE_ARC) {
      if (a->for_arrow)
	    draw_arrow(a, a->for_arrow, farpts, nfpts, op);
      if (a->back_arrow)
	    draw_arrow(a, a->back_arrow, barpts, nbpts, op);
    }

}

/*********************** ELLIPSE ***************************/

draw_ellipse(e, op)
    F_ellipse	   *e;
    int		    op;
{
    int		    a, b, xmin, ymin, xmax, ymax;

    ellipse_bound(e, &xmin, &ymin, &xmax, &ymax);
    if (!overlapping(ZOOMX(xmin), ZOOMY(ymin), ZOOMX(xmax), ZOOMY(ymax),
		     clip_xmin, clip_ymin, clip_xmax, clip_ymax))
	return;

    if (e->angle != 0.0) {
	angle_ellipse(e->center.x, e->center.y, e->radiuses.x, e->radiuses.y,
		e->angle, op, e->thickness, e->style,
		e->style_val, e->fill_style, e->pen_color, e->fill_color);
    /* it is much faster to use curve() for dashed and dotted lines that to
       use the server's sloooow algorithms for that */
    } else if (op != ERASE && (e->style == DOTTED_LINE || e->style == DASH_LINE)) {
	a = e->radiuses.x;
	b = e->radiuses.y;
	curve(canvas_win, a, 0, a, 0, True, False, e->direction,
		(int)(7*max2(a,b)*zoomscale), (b * b), (a * a),
		e->center.x, e->center.y, op,
		e->thickness, e->style, e->style_val, e->fill_style,
		e->pen_color, e->fill_color, CAP_ROUND);
    /* however, for solid lines the server is muuuch faster even for thick lines */
    } else {
	xmin = e->center.x - e->radiuses.x;
	ymin = e->center.y - e->radiuses.y;
	xmax = e->center.x + e->radiuses.x;
	ymax = e->center.y + e->radiuses.y;
	pw_curve(canvas_win, xmin, ymin, xmax, ymax, op,
		 e->thickness, e->style, e->style_val, e->fill_style,
		 e->pen_color, e->fill_color, CAP_ROUND);
    }
}

/*
 *  An Ellipse Generator.
 *  Written by James Tough   7th May 92
 *
 *  The following routines displays a filled ellipse on the screen from the
 *    semi-minor axis 'a', semi-major axis 'b' and angle of rotation
 *    'phi'.
 *
 *  It works along these principles .....
 *
 *        The standard ellipse equation is
 *
 *             x*x     y*y
 *             ---  +  ---
 *             a*a     b*b
 *
 *
 *        Rotation of a point (x,y) is well known through the use of
 *
 *            x' = x*COS(phi) - y*SIN(phi)
 *            y' = y*COS(phi) + y*COS(phi)
 *
 *        Taking these to together, this gives the equation for a rotated
 *      ellipse centered around the origin.
 *
 *           [x*COS(phi) - y*SIN(phi)]^2   [x*SIN(phi) + y*COS(phi)]^2
 *           --------------------------- + ---------------------------
 *                      a*a                           b*b
 *
 *        NOTE -  some of the above equation can be precomputed, eg,
 *
 *              i = COS(phi)/a        and        j = SIN(phi)/b
 *
 *        NOTE -  y is constant for each line so,
 *
 *              m = -yk*SIN(phi)/a    and     n = yk*COS(phi)/b
 *              where yk stands for the kth line ( y subscript k)
 *
 *        Where yk=y, we get a quadratic,
 *
 *              (i*x + m)^2 + (j*x + n)^2 = 1
 *
 *        Thus for any particular line, y, there is two corresponding x
 *      values. These are the roots of the quadratic. To get the roots,
 *      the above equation can be rearranged using the standard method,
 *
 *          -(i*m + j*n) +- sqrt[i^2 + j^2 - (i*n -j*m)^2]
 *      x = ----------------------------------------------
 *                           i^2 + j^2
 *
 *        NOTE -  again much of this equation can be precomputed.
 *
 *           c1 = i^2 + j^2
 *           c2 = [COS(phi)*SIN(phi)*(a-b)]
 *           c3 = [b*b*(COS(phi)^2) + a*a*(SIN(phi)^2)]
 *           c4 = a*b/c3
 *
 *      x = c2*y +- c4*sqrt(c3 - y*y),    where +- must be evaluated once
 *                                      for plus, and once for minus.
 *
 *        We also need to know how large the ellipse is. This condition
 *      arises when the sqrt of the above equation evaluates to zero.
 *      Thus the height of the ellipse is give by
 *
 *              sqrt[ b*b*(COS(phi)^2) + a*a*(SIN(phi)^2) ]
 *
 *       which just happens to be equal to sqrt(c3).
 *
 *         It is now possible to create a routine that will scan convert
 *       the ellipse on the screen.
 *
 *        NOTE -  c2 is the gradient of the new ellipse axis.
 *                c4 is the new semi-minor axis, 'a'.
 *           sqr(c3) is the new semi-major axis, 'b'.
 *
 *         These values could be used in a 4WS or 8WS ellipse generator
 *       that does not work on rotation, to give the feel of a rotated
 *       ellipse. These ellipses are not very accurate and give visable
 *       bumps along the edge of the ellipse. However, these routines
 *       are very quick, and give a good approximation to a rotated ellipse.
 *
 *       NOTES on the code given.
 *
 *           All the routines take there parameters as ( x, y, a, b, phi ),
 *           where x,y are the center of the ellipse ( relative to the
 *           origin ), a and b are the vertical and horizontal axis, and
 *           phi is the angle of rotation in RADIANS.
 *
 *           The 'moveto(x,y)' command moves the screen cursor to the
 *               (x,y) point.
 *           The 'lineto(x,y)' command draws a line from the cursor to
 *               the point (x,y).
 *
 */


/*
 *  QuickEllipse, uses the same method as Ellipse, but uses incremental
 *    methods to reduce the amount of work that has to be done inside
 *    the main loop. The speed increase is very noticeable.
 *
 *  Written by James Tough
 *  7th May 1992
 *
 */

static int	x[MAXNUMPTS/4][4],y[MAXNUMPTS/4][4];
static int	nump[4];
static int	totpts,i,j;
static int	order[4]={0,1,3,2};

angle_ellipse(center_x, center_y, radius_x, radius_y, angle,
	      op, thickness, style, style_val, fill_style,
	      pen_color, fill_color)
    int		    center_x, center_y;
    int		    radius_x, radius_y;
    float	    angle;
    int		    op,thickness,style,fill_style;
    int		    pen_color, fill_color;
    float	    style_val;
{
	float	xcen, ycen, a, b;

	double	c1, c2, c3, c4, c5, c6, v1, cphi, sphi, cphisqr, sphisqr;
	double	xleft, xright, d, asqr, bsqr;
	int	ymax, yy=0;
	int	k,m,dir;
	float	savezoom;
	int	savexoff, saveyoff;
	zXPoint	*ipnts;

	/* clear any previous error message */
	put_msg("");
	if (radius_x == 0 || radius_y == 0)
		return;

	/* adjust for zoomscale so we iterate over zoomed pixels */
	xcen = ZOOMX(center_x);
	ycen = ZOOMY(center_y);
	a = radius_x*zoomscale;
	b = radius_y*zoomscale;
	savezoom = zoomscale;
	savexoff = zoomxoff;
	saveyoff = zoomyoff;
	zoomscale = 1.0;
	zoomxoff = zoomyoff = 0;

	cphi = cos((double)angle);
	sphi = sin((double)angle);
	cphisqr = cphi*cphi;
	sphisqr = sphi*sphi;
	asqr = a*a;
	bsqr = b*b;
	
	c1 = (cphisqr/asqr)+(sphisqr/bsqr);
	c2 = ((cphi*sphi/asqr)-(cphi*sphi/bsqr))/c1;
	c3 = (bsqr*cphisqr) + (asqr*sphisqr);
	ymax = sqrt(c3);
	c4 = a*b/c3;
	c5 = 0;
	v1 = c4*c4;
	c6 = 2*v1;
	c3 = c3*v1-v1;
	totpts = 0;
	for (i=0; i<=3; i++)
		nump[i]=0;
	i=0; j=0;
	/* odd first points */
	if (ymax % 2) {
		d = sqrt(c3);
		newpoint(xcen-d,ycen);
		newpoint(xcen+d,ycen);
		c5 = c2;
		yy=1;
	}
	while (c3>=0) {
		d = sqrt(c3);
		xleft = c5-d;
		xright = c5+d;
		newpoint(xcen+xleft,ycen+yy);
		newpoint(xcen+xright,ycen+yy);
		newpoint(xcen-xright,ycen-yy);
		newpoint(xcen-xleft,ycen-yy);
		c5+=c2;
		v1+=c6;
		c3-=v1;
		yy=yy+1;
	}
	dir=0;
	totpts++;	/* add another point to join with first */
	init_point_array(totpts, 0);
	ipnts = points;
	/* now go down the 1st column, up the 2nd, down the 4th
	   and up the 3rd to get the points in the correct order */
	for (k=0; k<=3; k++) {
	    if (dir==0)
		for (m=0; m<nump[k]; m++) {
		    if (!add_point(x[m][order[k]],y[m][order[k]]))
			break;
		}
	    else
		for (m=nump[k]-1; m>=0; m--) {
		    if (!add_point(x[m][order[k]],y[m][order[k]]))
			break;
		}
	    dir = 1-dir;
	} /* next k */
	/* add another point to join with first */
	if (!add_point(ipnts->x,ipnts->y))
		too_many_points();
	draw_point_array(canvas_win, op, thickness, style, style_val,
		 JOIN_BEVEL, CAP_ROUND, fill_style, pen_color, fill_color);

	zoomscale = savezoom;
	zoomxoff = savexoff;
	zoomyoff = saveyoff;
	return;
}

/* store the points across (row-wise in) the matrix */

newpoint(xp,yp)
    float	   xp,yp;
{
    if (totpts >= MAXNUMPTS/4) {
	if (totpts == MAXNUMPTS/4) {
	    put_msg("Too many points to fully display rotated ellipse. %d points max",
		MAXNUMPTS);
	    totpts++;
	}
	return;
    }
    x[i][j]=round(xp);
    y[i][j]=round(yp);
    nump[j]++;
    totpts++;
    if (++j > 3) {
	j=0;
	i++;
    }
}


/*********************** LINE ***************************/

draw_line(line, op)
    F_line	   *line;
    int		    op;
{
    F_point	   *point;
    int		    xx, yy, x, y;
    int		    xmin, ymin, xmax, ymax;
    char	   *string;
    F_point	   *p0, *p1, *p2;
    PR_SIZE	    txt;

    line_bound(line, &xmin, &ymin, &xmax, &ymax);
    if (!overlapping(ZOOMX(xmin), ZOOMY(ymin), ZOOMX(xmax), ZOOMY(ymax),
		     clip_xmin, clip_ymin, clip_xmax, clip_ymax))
	return;

    /* is it an arcbox? */
    if (line->type == T_ARC_BOX) {
	draw_arcbox(line, op);
	return;
    }
    /* is it a picture object? */
    if (line->type == T_PICTURE) {
	if (line->pic->bitmap != NULL) {
	    draw_pic_pixmap(line, op);
	    return;
	} else {		/* label empty pic bounding box */
	    if (line->pic->file[0] == '\0')
		string = EMPTY_PIC;
	    else {
		string = strrchr(line->pic->file, '/');
		if (string == NULL)
		    string = line->pic->file;
		else
		    string++;
	    }
	    p0 = line->points;
	    p1 = p0->next;
	    p2 = p1->next;
	    xmin = min3(p0->x, p1->x, p2->x);
	    ymin = min3(p0->y, p1->y, p2->y);
	    xmax = max3(p0->x, p1->x, p2->x);
	    ymax = max3(p0->y, p1->y, p2->y);
	    canvas_font = lookfont(0, 12);	/* get a size 12 font */
	    txt = textsize(canvas_font, strlen(string), string);
	    x = (xmin + xmax) / 2 - txt.length/display_zoomscale / 2;
	    y = (ymin + ymax) / 2;
	    pw_text(canvas_win, x, y, op, canvas_font, 0.0, string, DEFAULT);
	}
    }
    /* get first point and coordinates */
    point = line->points;
    x = point->x;
    y = point->y;

    /* is it a single point? */
    if (line->points->next == NULL) {
	/* draw but don't fill */
	pw_point(canvas_win, x, y, line->thickness,
		 op, line->pen_color, line->cap_style);
	return;
    }

    /* accumulate the points in an array - start with 50 */
    if (!init_point_array(50, 50))
	return;

    for (point = line->points; point != NULL; point = point->next) {
	xx = x;
	yy = y;
	x = point->x;
	y = point->y;
	if (!add_point(x, y)) {
	    too_many_points();
	    break;
	}
    }

    /* setup clipping so that spline doesn't protrude beyond arrowhead */
    /* also create the arrowheads */
    clip_arrows(line,O_POLYLINE,op);

    draw_point_array(canvas_win, op, line->thickness, line->style,
		     line->style_val, line->join_style,
		     line->cap_style, line->fill_style,
		     line->pen_color, line->fill_color);

    /* restore clipping */
    set_clip_window(clip_xmin, clip_ymin, clip_xmax, clip_ymax);

    /* draw the arrowheads, if any */
    if (line->for_arrow)
	draw_arrow(line, line->for_arrow, farpts, nfpts, op);
    if (line->back_arrow)
	draw_arrow(line, line->back_arrow, barpts, nbpts, op);
}

draw_arcbox(line, op)
    F_line	   *line;
    int		    op;
{
    F_point	   *point;
    int		    xmin, xmax, ymin, ymax;

    point = line->points;
    xmin = xmax = point->x;
    ymin = ymax = point->y;
    while (point->next) {	/* find lower left (upper-left on screen) */
	/* and upper right (lower right on screen) */
	point = point->next;
	if (point->x < xmin)
	    xmin = point->x;
	else if (point->x > xmax)
	    xmax = point->x;
	if (point->y < ymin)
	    ymin = point->y;
	else if (point->y > ymax)
	    ymax = point->y;
    }
    pw_arcbox(canvas_win, xmin, ymin, xmax, ymax, round(line->radius*ZOOM_FACTOR),
	      op, line->thickness, line->style, line->style_val, line->fill_style,
	      line->pen_color, line->fill_color);
}

draw_pic_pixmap(box, op)
    F_line	   *box;
    int		    op;
{
    int		    xmin, ymin;
    int		    xmax, ymax;
    int		    width, height, rotation;
    F_pos	    origin;
    F_pos	    opposite;

    origin.x = ZOOMX(box->points->x);
    origin.y = ZOOMY(box->points->y);
    opposite.x = ZOOMX(box->points->next->next->x);
    opposite.y = ZOOMY(box->points->next->next->y);

    xmin = min2(origin.x, opposite.x);
    ymin = min2(origin.y, opposite.y);
    xmax = max2(origin.x, opposite.x);
    ymax = max2(origin.y, opposite.y);
    if (op == ERASE) {
	clear_region(xmin, ymin, xmax, ymax);
	return;
    }
    /* width is upper-lower+1 */
    width = abs(origin.x - opposite.x) + 1;
    height = abs(origin.y - opposite.y) + 1;
    rotation = 0;
    if (origin.x > opposite.x && origin.y > opposite.y)
	rotation = 180;
    if (origin.x > opposite.x && origin.y <= opposite.y)
	rotation = 270;
    if (origin.x <= opposite.x && origin.y > opposite.y)
	rotation = 90;

    /* if something has changed regenerate the pixmap */
    if (box->pic->pixmap == 0 ||
	box->pic->color != box->pen_color ||
	box->pic->pix_rotation != rotation ||
	abs(box->pic->pix_width - width) > 1 ||		/* rounding makes diff of 1 bit */
	abs(box->pic->pix_height - height) > 1 ||
	box->pic->pix_flipped != box->pic->flipped)
	    create_pic_pixmap(box, rotation, width, height, box->pic->flipped);

    XCopyArea(tool_d, box->pic->pixmap, canvas_win, gccache[op],
	      0, 0, width, height, xmin, ymin);
    XFlush(tool_d);
}

/*
 * The input to this routine is the bitmap which is the "preview"
 * section of an encapsulated postscript file. That input bitmap
 * has an arbitrary number of rows and columns. This routine
 * re-samples the input bitmap creating an output bitmap of dimensions
 * width-by-height. This output bitmap is made into a Pixmap
 * for display purposes.
 */

create_pic_pixmap(box, rotation, width, height, flipped)
    F_line	   *box;
    int		    rotation, width, height, flipped;
{
    int		    cwidth, cheight;
    int		    i;
    int		    j;
    int		    k;
    unsigned char  *data;
    unsigned char  *tdata;
    int		    nbytes;
    int		    bbytes;
    int		    ibit, jbit, jnb;
    int		    wbit;
    int		    fg, bg;
    XImage	   *image;

    /* this could take a while */
    set_temp_cursor(wait_cursor);
    if (box->pic->pixmap != 0)
	XFreePixmap(tool_d, box->pic->pixmap);

    if (appres.DEBUG)
	fprintf(stderr,"Scaling pic pixmap to %dx%d pixels\n",width,height);

    cwidth = box->pic->bit_size.x;	/* current width, height */
    cheight = box->pic->bit_size.y;

    /* create a new bitmap at the specified size (requires interpolation) */
    /* XBM style *OR* EPS, PCX, XPM, GIF or JPEG on monochrome display */
    if (box->pic->numcols == 0) {
	    nbytes = (width + 7) / 8;
	    bbytes = (cwidth + 7) / 8;
	    data = (char *) malloc(nbytes * height);
	    tdata = (char *) malloc(nbytes);
	    bzero((char*)data, nbytes * height);	/* clear memory */
	    if ((!flipped && (rotation == 0 || rotation == 180)) ||
		(flipped && !(rotation == 0 || rotation == 180))) {
		for (j = 0; j < height; j++) {
		    jbit = cheight * j / height * bbytes;
		    for (i = 0; i < width; i++) {
			ibit = cwidth * i / width;
			wbit = (unsigned char) *(box->pic->bitmap + jbit + ibit / 8);
			if (wbit & (1 << (7 - (ibit & 7))))
			    *(data + j * nbytes + i / 8) += (1 << (i & 7));
		    }
		}
	    } else {
		for (j = 0; j < height; j++) {
		    ibit = cwidth * j / height;
		    for (i = 0; i < width; i++) {
			jbit = cheight * i / width * bbytes;
			wbit = (unsigned char) *(box->pic->bitmap + jbit + ibit / 8);
			if (wbit & (1 << (7 - (ibit & 7))))
			    *(data + (height - j - 1) * nbytes + i / 8) += (1 << (i & 7));
		    }
		}
	    }

	    /* horizontal swap */
	    if (rotation == 180 || rotation == 270)
		for (j = 0; j < height; j++) {
		    jnb = j*nbytes;
		    bzero((char*)tdata, nbytes);
		    for (i = 0; i < width; i++)
			if (*(data + jnb + (width - i - 1) / 8) & (1 << ((width - i - 1) & 7)))
			    *(tdata + i / 8) += (1 << (i & 7));
		    bcopy(tdata, data + j * nbytes, nbytes);
		}

	    /* vertical swap */
	    if ((!flipped && (rotation == 180 || rotation == 270)) ||
		(flipped && !(rotation == 180 || rotation == 270))) {
		for (j = 0; j < (height + 1) / 2; j++) {
		    jnb = j*nbytes;
		    bcopy(data + jnb, tdata, nbytes);
		    bcopy(data + (height - j - 1) * nbytes, data + jnb, nbytes);
		    bcopy(tdata, data + (height - j - 1) * nbytes, nbytes);
		}
	    }

	    if (writing_bitmap) {
		fg = x_fg_color.pixel;			/* writing xbm, 1 and 0 */
		bg = x_bg_color.pixel;
	    } else if (box->pic->subtype == T_PIC_XBM) {
		fg = x_color(box->pen_color);		/* xbm, use object pen color */
		bg = x_bg_color.pixel;
	    } else if (box->pic->subtype == T_PIC_EPS) {
		fg = black_color.pixel;			/* pbm from gs is inverted */
		bg = white_color.pixel;
	    } else {
		fg = white_color.pixel;			/* gif, xpm after map_to_mono */
		bg = black_color.pixel;
	    }
		
	    box->pic->pixmap = XCreatePixmapFromBitmapData(tool_d, canvas_win,
					data, width, height, fg,bg, tool_dpth);
	    free(data);
            free(tdata);

      /* EPS, PCX, XPM, GIF or JPEG on color display */
      /* It is important to note that the Cmap pixels are unsigned long. */
      /* Therefore all manipulation of the image data should be as unsigned long. */
      /* bpp = bytes per pixel */
      /* bpl = bytes per line */
      } else {
            unsigned char *pixel, *cpixel, *p1, *p2, tmp;
            int bpp, bpl, cbpp, cbpl;
            unsigned long *Lpixel;
            unsigned short *Spixel;
            unsigned char *Cpixel;
	    struct Cmap	*cmap = box->pic->cmap;

            cbpp = 1;
            cbpl = cwidth * cbpp;
	    if (tool_dpth == 24 || tool_dpth == 32)
                bpp = 4;
	    else if (tool_dpth == 16)
                bpp = 2;
	    else
                bpp = 1;
            bpl = width * bpp;
	    data = (char *) malloc(bpl * height);
	    bzero((char*)data, bpl * height);
	    if ((!flipped && (rotation == 0 || rotation == 180)) ||
		(flipped && !(rotation == 0 || rotation == 180))) {
                for( j=0; j<height; j++ ){
                  p1 = data + (j * bpl);
                  p2 = box->pic->bitmap + (j * cheight / height * cbpl);
                  for( i=0; i<width; i++ ){
                    pixel = p1 + (i * bpp);
                    cpixel = p2 + (i * cbpl / width );
		    if (bpp == 4) {
			Lpixel = (unsigned long *) pixel;
			*Lpixel = cmap[*cpixel].pixel;
		    } else if (bpp == 2) {
			Spixel = (unsigned short *) pixel;
			*Spixel = cmap[*cpixel].pixel;
		    } else {
			Cpixel = (unsigned char *) pixel;
			*Cpixel = cmap[*cpixel].pixel;
		    }
                  }
                }
	    } else {
                for( j=0; j<height; j++ ){
                  p1 = data + (j * bpl);
                  p2 = box->pic->bitmap + (j * cbpl / height);
                  for( i=0; i<width; i++ ){
                    pixel = p1 + (i * bpp);
                    cpixel = p2 + (i * cheight / width * cbpl);
		    if (bpp == 4) {
			Lpixel = (unsigned long *) pixel;
			*Lpixel = cmap[*cpixel].pixel;
		    } else if (bpp == 2) {
			Spixel = (unsigned short *) pixel;
			*Spixel = cmap[*cpixel].pixel;
		    } else {
			Cpixel = (unsigned char *) pixel;
			*Cpixel = cmap[*cpixel].pixel;
		    }
                  }
                }
	    }

	    /* horizontal swap */
	    if (rotation == 180 || rotation == 270){
                for( j=0; j<height; j++ ){
                  p1 = data + (j * bpl);
                  p2 = p1 + ((width - 1) * bpp);
                  for( i=0; i<width/2; i++, p2 -= 2*bpp ){
                    for( k=0; k<bpp; k++, p1++, p2++ ){
                      tmp = *p1;
                      *p1 = *p2;
                      *p2 = tmp;
                    }
                  }
                }
            }

	    /* vertical swap */
	    if ((!flipped && (rotation == 90 || rotation == 180)) ||
		( flipped && (rotation == 90 || rotation == 180))){
                for( i=0; i<width; i++ ){
                  p1 = data + (i * bpp);
                  p2 = p1 + ((height - 1) * bpl);
                  for( j=0; j<height/2; j++, p1 += (width-1)*bpp, p2 -= (width+1)*bpp ){
                    for( k=0; k<bpp; k++, p1++, p2++ ){
                      tmp = *p1;
                      *p1 = *p2;
                      *p2 = tmp;
                    }
                  }
                }
            }

	    image = XCreateImage(tool_d, tool_v, tool_dpth,
				ZPixmap, 0, data, width, height, 8*bpp, 0);
	    box->pic->pixmap = XCreatePixmap(tool_d, canvas_win,
				width, height, tool_dpth);
	    XPutImage(tool_d, box->pic->pixmap, gc, image, 0, 0, 0, 0, width, height);
	    XDestroyImage(image);
    }

    box->pic->color = box->pen_color;
    box->pic->pix_rotation = rotation;
    box->pic->pix_width = width;
    box->pic->pix_height = height;
    box->pic->pix_flipped = flipped;
    reset_cursor();
}

/*********************** SPLINE ***************************/

void
draw_spline(spline, op)
    F_spline	   *spline;
    int		    op;
{
    Boolean         success;
    int		    xmin, ymin, xmax, ymax;
    float           precision;

    spline_bound(spline, &xmin, &ymin, &xmax, &ymax);
    if (!overlapping(ZOOMX(xmin), ZOOMY(ymin), ZOOMX(xmax), ZOOMY(ymax),
		     clip_xmin, clip_ymin, clip_xmax, clip_ymax))
	return;

    precision = (display_zoomscale < ZOOM_PRECISION) ? LOW_PRECISION 
                                                     : HIGH_PRECISION;

    if (open_spline(spline))
	success = compute_open_spline(spline, precision);
    else
	success = compute_closed_spline(spline, precision);
    if (success) {
	/* setup clipping so that spline doesn't protrude beyond arrowhead */
	/* also create the arrowheads */
	clip_arrows(spline,O_SPLINE,op);

	draw_point_array(canvas_win, op, spline->thickness, spline->style,
		       spline->style_val, JOIN_MITER, spline->cap_style,
		       spline->fill_style, spline->pen_color,
		       spline->fill_color);
	/* restore clipping */
	set_clip_window(clip_xmin, clip_ymin, clip_xmax, clip_ymax);

	if (spline->back_arrow)	/* backward arrow  */
	    draw_arrow(spline, spline->back_arrow, barpts, nbpts, op);
	if (spline->for_arrow)	/* backward arrow  */
	    draw_arrow(spline, spline->for_arrow, farpts, nfpts, op);
    }
}

static Boolean
compute_open_spline(spline, precision)
     F_spline	   *spline;
     float         precision;
{
  int       k;
  float     step;
  F_point   *p0, *p1, *p2, *p3;
  F_sfactor *s0, *s1, *s2, *s3;

  if (!init_point_array(300, 200))
      return False;

  COPY_CONTROL_POINT(p0, s0, spline->points, spline->sfactors);
  COPY_CONTROL_POINT(p1, s1, p0, s0);
  /* first control point is needed twice for the first segment */
  COPY_CONTROL_POINT(p2, s2, p1->next, s1->next);

  if (p2->next == NULL) {
      COPY_CONTROL_POINT(p3, s3, p2, s2);
  } else {
      COPY_CONTROL_POINT(p3, s3, p2->next, s2->next);
  }


  for (k = 0 ;  ; k++) {
      SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
      if (p3->next == NULL)
	break;
      NEXT_CONTROL_POINTS(p0, s0, p1, s1, p2, s2, p3, s3);
  }
  /* last control point is needed twice for the last segment */
  COPY_CONTROL_POINT(p0, s0, p1, s1);
  COPY_CONTROL_POINT(p1, s1, p2, s2);
  COPY_CONTROL_POINT(p2, s2, p3, s3);
  SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
  
  if (!add_point(p3->x, p3->y))
    too_many_points();
  
  return True;
}


static Boolean
compute_closed_spline(spline, precision)
     F_spline	   *spline;
     float         precision;
{
  int k, npoints = num_points(spline->points), i;
  float     step;
  double    t;
  F_point   *p0, *p1, *p2, *p3, *first;
  F_sfactor *s0, *s1, *s2, *s3, *s_first;

  if (!init_point_array(300, 200))
      return False;

  INIT_CONTROL_POINTS(spline, p0, s0, p1, s1, p2, s2, p3, s3);
  COPY_CONTROL_POINT(first, s_first, p0, s0); 

  for (k = 0 ; p3 != NULL ; k++) {
      SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
      NEXT_CONTROL_POINTS(p0, s0, p1, s1, p2, s2, p3, s3);
  }
  /* when we are at the end, join to the beginning */
  COPY_CONTROL_POINT(p3, s3, first, s_first);
  SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);

  for (i=0; i<2; i++) {
      k++;
      NEXT_CONTROL_POINTS(p0, s0, p1, s1, p2, s2, p3, s3);
      SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, precision);
  }

  if (!add_point(points[0].x,points[0].y))
    too_many_points();

  return True;
}


void
quick_draw_spline(spline, operator)
     F_spline      *spline;
     int           operator;
{
  int        k;
  float     step;
  F_point   *p0, *p1, *p2, *p3;
  F_sfactor *s0, *s1, *s2, *s3;
  
  if (!init_point_array(300, 200))
    return;

  INIT_CONTROL_POINTS(spline, p0, s0, p1, s1, p2, s2, p3, s3);
 
  for (k=0 ; p3!=NULL ; k++) {
      SPLINE_SEGMENT_LOOP(k, p0, p1, p2, p3, s1->s, s2->s, LOW_PRECISION);
      NEXT_CONTROL_POINTS(p0, s0, p1, s1, p2, s2, p3, s3);
  }
  draw_point_array(canvas_win, operator, spline->thickness, spline->style,
		   spline->style_val, JOIN_MITER, spline->cap_style,
		   spline->fill_style, spline->pen_color, spline->fill_color);
}

/*********************** TEXT ***************************/

static char    *hidden_text_string = "<<>>";

draw_text(text, op)
    F_text	   *text;
    int		    op;
{
    PR_SIZE	    size;
    int		    x,y;
    int		    xmin, ymin, xmax, ymax;
    int		    x1,y1, x2,y2, x3,y3, x4,y4;

    if (text->zoom != zoomscale)
	reload_text_fstruct(text);
    text_bound(text, &xmin, &ymin, &xmax, &ymax,
	       &x1,&y1, &x2,&y2, &x3,&y3, &x4,&y4);

    if (!overlapping(ZOOMX(xmin), ZOOMY(ymin), ZOOMX(xmax), ZOOMY(ymax),
		     clip_xmin, clip_ymin, clip_xmax, clip_ymax))
	return;

    /* outline the text bounds in red if debug resource is set */
    if (appres.DEBUG) {
	pw_vector(canvas_win, x1, y1, x2, y2, op, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, x2, y2, x3, y3, op, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, x3, y3, x4, y4, op, 1, RUBBER_LINE, 0.0, RED);
	pw_vector(canvas_win, x4, y4, x1, y1, op, 1, RUBBER_LINE, 0.0, RED);
    }

    x = text->base_x;
    y = text->base_y;
    if (text->type == T_CENTER_JUSTIFIED || text->type == T_RIGHT_JUSTIFIED) {
	size = textsize(text->fontstruct, strlen(text->cstring),
			    text->cstring);
	size.length = size.length/display_zoomscale;
	if (text->type == T_CENTER_JUSTIFIED) {
	    x = round(x-cos(text->angle)*size.length/2);
	    y = round(y+sin(text->angle)*size.length/2);
	} else {	/* T_RIGHT_JUSTIFIED */
	    x = round(x-cos(text->angle)*size.length);
	    y = round(y+sin(text->angle)*size.length);
	}
    }
    if (hidden_text(text))
	pw_text(canvas_win, x, y, op, lookfont(0,12),
		text->angle, hidden_text_string, DEFAULT);
    else
	pw_text(canvas_win, x, y, op, text->fontstruct,
		text->angle, text->cstring, text->color);
}

/*********************** COMPOUND ***************************/

void
draw_compoundelements(c, op)
    F_compound	   *c;
    int		    op;
{
    F_line	   *l;
    F_spline	   *s;
    F_ellipse	   *e;
    F_text	   *t;
    F_arc	   *a;
    F_compound	   *c1;

    if (!overlapping(ZOOMX(c->nwcorner.x), ZOOMY(c->nwcorner.y),
		     ZOOMX(c->secorner.x), ZOOMY(c->secorner.y),
		     clip_xmin, clip_ymin, clip_xmax, clip_ymax))
	return;

    for (l = c->lines; l != NULL; l = l->next) {
	draw_line(l, op);
    }
    for (s = c->splines; s != NULL; s = s->next) {
	draw_spline(s, op);
    }
    for (a = c->arcs; a != NULL; a = a->next) {
	draw_arc(a, op);
    }
    for (e = c->ellipses; e != NULL; e = e->next) {
	draw_ellipse(e, op);
    }
    for (t = c->texts; t != NULL; t = t->next) {
	draw_text(t, op);
    }
    for (c1 = c->compounds; c1 != NULL; c1 = c1->next) {
	draw_compoundelements(c1, op);
    }
}

/*************************** ARROWS *****************************

 compute_arcarrow_angle - Computes a point on a line which is a chord
	to the arc specified by center (x1,y1) and endpoint (x2,y2),
	where the chord intersects the arc arrow->ht from the endpoint.

 May give strange values if the arrow.ht is larger than about 1/4 of
 the circumference of a circle on which the arc lies.

****************************************************************/

compute_arcarrow_angle(x1, y1, x2, y2, direction, arrow, x, y)
    float x1, y1;
    int x2, y2, direction, *x, *y;
    F_arrow *arrow;
{
    double	r, alpha, beta, dy, dx;
    double	lpt,h;

    dy=y2-y1;
    dx=x2-x1;
    r=sqrt(dx*dx+dy*dy);

    h = (double) arrow->ht;
    /* lpt is the amount the arrowhead extends beyond the end of the line */
    lpt = arrow->thickness*15/2.0/(arrow->wid/h/2.0);
    /* add this to the length */
    h += lpt;

    /* radius too small for this method, use normal method */
    if (h > 2.0*r) {
	compute_normal(x1,y1,x2,y2,direction,x,y);
	return;
    }

    beta=atan2(dy,dx);
    if (direction) {
	alpha=2*asin(h/2.0/r);
    } else {
	alpha=-2*asin(h/2.0/r);
    }

    *x=round(x1+r*cos(beta+alpha));
    *y=round(y1+r*sin(beta+alpha));
}

/* temporary error handler - see call to XSetRegion in clip_arrows below */

tempXErrorHandler (display, event)
    Display	*display;
    XErrorEvent	*event;
{
	return 0;
}


/****************************************************************

 clip_arrows - calculate a clipping region which is the current 
	clipping area minus the polygons at the arrowheads.

 This will prevent the object (line, spline etc.) from protruding
 on either side of the arrowhead Also calculate the arrowheads
 themselves and put the polygons in farpts[nfpts] for forward
 arrow and barpts[nbpts] for backward arrow.

 The points[] array must already have the points for the object
 being drawn (spline, line etc), and npoints, the number of points.

****************************************************************/

clip_arrows(obj, objtype, op)
    F_line	   *obj;
    int		    objtype;
    int		    op;
{
    Region	    mainregion, newregion;
    Region	    region;
    XPoint	    xpts[5];
    int		    fcx1, fcy1, fcx2, fcy2;
    int		    bcx1, bcy1, bcx2, bcy2;
    int		    x, y;
    int		    i, j;

    if (obj->for_arrow || obj->back_arrow) {
	/* start with current clipping area - maybe we won't have to draw anything */
	xpts[0].x = clip_xmin;
	xpts[0].y = clip_ymin;
	xpts[1].x = clip_xmax;
	xpts[1].y = clip_ymin;
	xpts[2].x = clip_xmax;
	xpts[2].y = clip_ymax;
	xpts[3].x = clip_xmin;
	xpts[3].y = clip_ymax;
	mainregion = XPolygonRegion(xpts, 4, WindingRule);
    }

    /* get points for any forward arrowhead */
    if (obj->for_arrow) {
	x = points[npoints-2].x;
	y = points[npoints-2].y;
	if (objtype == O_ARC) {
	    F_arc  *a = (F_arc *) obj;
	    compute_arcarrow_angle(a->center.x, a->center.y, a->point[2].x,
				a->point[2].y, a->direction,
				a->for_arrow, &x, &y);
	}
	calc_arrow(x, y, points[npoints-1].x, points[npoints-1].y,
		   &fcx1, &fcy1, &fcx2, &fcy2,
		   obj->thickness, obj->for_arrow, farpts, &nfpts);
	/* set clipping to the first three points of the arrowhead and
	   the box surrounding it */
	for (i=0; i<3; i++) {
	    xpts[i].x = ZOOMX(farpts[i].x);
	    xpts[i].y = ZOOMY(farpts[i].y);
	}
	xpts[3].x = ZOOMX(fcx2);
	xpts[3].y = ZOOMY(fcy2);
	xpts[4].x = ZOOMX(fcx1);
	xpts[4].y = ZOOMY(fcy1);
	/* draw the clipping area for debugging */
	if (appres.DEBUG) {
	  for (i=0; i<5; i++) {
	    if (i==4)
		j=0;
	    else
		j=i+1;
	    pw_vector(canvas_win,xpts[i].x,xpts[i].y,xpts[j].x,xpts[j].y,op,1,
		PANEL_LINE,0.0,RED);
	  }
	}
	region = XPolygonRegion(xpts, 5, WindingRule);
	newregion = XCreateRegion();
	XSubtractRegion(mainregion, region, newregion);
	XDestroyRegion(region);
	XDestroyRegion(mainregion);
	mainregion=newregion;
    }
	
    /* get points for any backward arrowhead */
    if (obj->back_arrow) {
	x = points[1].x;
	y = points[1].y;
	if (objtype == O_ARC) {
	    F_arc  *a = (F_arc *) obj;
	    compute_arcarrow_angle(a->center.x, a->center.y, a->point[0].x,
			       a->point[0].y, a->direction ^ 1,
			       a->back_arrow, &x, &y);
	}
	calc_arrow(x, y, points[0].x, points[0].y,
		   &bcx1, &bcy1, &bcx2, &bcy2,
		    obj->thickness, obj->back_arrow, barpts,&nbpts);
	/* set clipping to the first three points of the arrowhead and
	   the box surrounding it */
	for (i=0; i<3; i++) {
	    xpts[i].x = ZOOMX(barpts[i].x);
	    xpts[i].y = ZOOMY(barpts[i].y);
	}
	xpts[3].x = ZOOMX(bcx2);
	xpts[3].y = ZOOMY(bcy2);
	xpts[4].x = ZOOMX(bcx1);
	xpts[4].y = ZOOMY(bcy1);
	/* draw the clipping area for debugging */
	if (appres.DEBUG) {
	  int j;
	  for (i=0; i<5; i++) {
	    if (i==4)
		j=0;
	    else
		j=i+1;
	    pw_vector(canvas_win,xpts[i].x,xpts[i].y,xpts[j].x,xpts[j].y,op,1,
		PANEL_LINE,0.0,RED);
	  }
	}
	region = XPolygonRegion(xpts, 5, WindingRule);
	newregion = XCreateRegion();
	XSubtractRegion(mainregion, region, newregion);
	XDestroyRegion(region);
	XDestroyRegion(mainregion);
	mainregion=newregion;
    }
    /* now set the clipping region for the subsequent drawing of the object */
    if (obj->for_arrow || obj->back_arrow) {
	/* install a temporary error handler to ignore any BadMatch error
	   from the buggy R5 Xlib XSetRegion() */
	XSetErrorHandler (tempXErrorHandler);
	XSetRegion(tool_d, gccache[op], mainregion);
	/* restore original error handler */
	if (!appres.DEBUG)
	    XSetErrorHandler(X_error_handler);
	XDestroyRegion(mainregion);
    }
}

/****************************************************************

 calc_arrow - calculate points heading from (x1, y1) to (x2, y2)

 Must pass POINTER to npoints for return value and for c1x, c1y,
 c2x, c2y, which are two points at the end of the arrowhead such
 that xc, yc, c1x, c1y, c2x, c2y and xd, yd form the bounding
 rectangle of the arrowhead.

 Fills points array with npoints arrowhead coordinates

****************************************************************/

calc_arrow(x1, y1, x2, y2, c1x, c1y, c2x, c2y, objthick, arrow, points, npoints)
    int		    x1, y1, x2, y2;
    int		   *c1x, *c1y, *c2x, *c2y;
    int		    objthick;
    F_arrow	   *arrow;
    zXPoint	    points[];
    int		   *npoints;
{
    double	    x, y, xb, yb, dx, dy, l, sina, cosa;
    double	    xxb;
    double	    mx, my;
    double	    ddx, ddy, lpt;
    double	    alpha;
    int		    xc, yc, xd, yd, xs, ys;
    int		    xg, yg, xh, yh;
    float	    wid = arrow->wid;
    float	    ht = arrow->ht;
    int		    type = arrow->type;
    int		    style = arrow->style;

    *npoints = 0;
    dx = x2 - x1;
    dy = y1 - y2;
    if (dx==0 && dy==0)
	return;

    /* lpt is the amount the arrowhead extends beyond the end of the
       line because of the sharp point (miter join) */
    if (type == 2)
      lpt = arrow->thickness*15 / (2.0 * sin(atan(wid / (2.4 * ht))));
    else if (type == 3)
      lpt = arrow->thickness*15 / (2.0 * sin(atan(wid / (1.6 * ht))));
    else
      lpt = arrow->thickness*15 / (2.0 * sin(atan(wid / (2.0 * ht))));

    /* alpha is the angle the line is relative to horizontal */
    alpha = atan2(dy,-dx);

    /* ddx, ddy is amount to move end of line back so that arrowhead point
       ends where line used to */
    ddx = lpt * cos(alpha);
    ddy = lpt * sin(alpha);

    /* move endpoint of line back */
    mx = x2 + ddx;
    my = y2 + ddy;

    l = sqrt(dx * dx + dy * dy);
    sina = dy / l;
    cosa = dx / l;
    xb = mx * cosa - my * sina;
    yb = mx * sina + my * cosa;

    /* (xs,ys) are a point the length (height) of the arrowhead from
       the end of the shaft */
    xs =  (xb-ht) * cosa + yb * sina + .5;
    ys = -(xb-ht) * sina + yb * cosa + .5;

    /* lengthen the tail if type 2 */
    if (type == 2)
	x = xb - ht * 1.2;
    /* shorten the tail if type 3 */
    else if (type == 3)
	x = xb - ht * 0.8;
    else
	x = xb - ht;

    /* half the width of the arrowhead */
    y = yb - wid / 2;

    /* xc,yc is one point of arrowhead tail */
    xc =  x * cosa + y * sina + .5;
    yc = -x * sina + y * cosa + .5;

    /* the x component of the endpoint of the line */
    xxb = x2 * cosa - y2 * sina;

    /* xg,yg is one corner of the box enclosing the arrowhead */
    /* allow extra for a round line cap */
    xxb = xxb+objthick*ZOOM_FACTOR;

    xg =  xxb * cosa + y * sina + .5;
    yg = -xxb * sina + y * cosa + .5;

    y = yb + wid / 2;
    /* xd,yd is other point of arrowhead tail */
    xd =  x * cosa + y * sina + .5;
    yd = -x * sina + y * cosa + .5;

    /* xh,yh is the other corner of the box enclosing the arrowhead */
    /* allow extra for a round line cap */
    xh =  xxb * cosa + y * sina + .5;
    yh = -xxb * sina + y * cosa + .5;

    /* pass back these two corners to the caller */
    *c1x = xg;
    *c1y = yg;
    *c2x = xh;
    *c2y = yh;

    /* draw the box surrounding the arrowhead */
    if (appres.DEBUG) {
	pw_vector(canvas_win, xc, yc, xg, yg, PAINT, 1, RUBBER_LINE, 0.0, GREEN);
	pw_vector(canvas_win, xg, yg, xh, yh, PAINT, 1, RUBBER_LINE, 0.0, GREEN);
	pw_vector(canvas_win, xh, yh, xd, yd, PAINT, 1, RUBBER_LINE, 0.0, GREEN);
	pw_vector(canvas_win, xd, yd, xc, yc, PAINT, 1, RUBBER_LINE, 0.0, GREEN);
    }

    points[*npoints].x = xc; points[(*npoints)++].y = yc;
    points[*npoints].x = mx; points[(*npoints)++].y = my;
    points[*npoints].x = xd; points[(*npoints)++].y = yd;
    if (type != 0) {
	points[*npoints].x = xs; points[(*npoints)++].y = ys; /* add point on shaft */
	points[*npoints].x = xc; points[(*npoints)++].y = yc; /* connect back to first point */
    }
}

/* draw the arrowhead resulting from the call to calc_arrow() */

draw_arrow(obj, arrow, points, npoints, op)
    F_line	   *obj;
    F_arrow	   *arrow;
    zXPoint	    points[];
    int		    npoints;
    int		    op;
{
    int		    fill;

    if (obj->thickness == 0)
	return;
    if (arrow->type == 0)
	fill = UNFILLED;			/* old, boring arrow head */
    else if (arrow->style == 0)
	fill = NUMTINTPATS+NUMSHADEPATS-1;	/* "hollow", fill with white */
    else
	fill = NUMSHADEPATS-1;			/* "solid", fill with solid color */
    pw_lines(canvas_win, points, npoints, op, round(arrow->thickness),
		SOLID_LINE, 0.0, JOIN_MITER, CAP_BUTT,
		fill, obj->pen_color, obj->pen_color);
}

/********************* CURVES FOR ARCS AND ELLIPSES ***************

 This routine plot two dimensional curve defined by a second degree
 polynomial of the form : 2    2 f(x, y) = ax + by + g = 0

 (x0,y0) is the starting point as well as ending point of the curve. The curve
 will translate with the offset xoff and yoff.

 This algorithm is derived from the eight point algorithm in : "An Improved
 Algorithm for the generation of Nonparametric Curves" by Bernard W.
 Jordan, William J. Lennon and Barry D. Holm, IEEE Transaction on Computers
 Vol C-22, No. 12 December 1973.

 This routine is only called for ellipses when the andle is 0 and the line type
 is not solid.  For angles of 0 with solid lines, pw_curve() is called.
 For all other angles angle_ellipse() is called.

 Will fill the curve if fill_style is != UNFILLED (-1)
 Call with draw_points = True to display the points using draw_point_array
	Otherwise global points array is filled with npoints values but
	not displayed.
 Call with draw_center = True and center_x, center_y set to draw endpoints
	to center point (xoff,yoff) (arc type 2, i.e. pie wedge)

****************************************************************/

curve(window, xstart, ystart, xend, yend, draw_points, draw_center,
	direction, estnpts, a, b, xoff, yoff, op, thick,
	style, style_val, fill_style, pen_color, fill_color, cap_style)
    Window	    window;
    int		    xstart, ystart, xend, yend, a, b, xoff, yoff;
    Boolean	    draw_points, draw_center;
    int		    direction, estnpts, op, thick, style, fill_style;
    float	    style_val;
    Color	    pen_color, fill_color;
    int		    cap_style;
{
    register int    x, y;
    register double deltax, deltay, dfx, dfy;
    double	    dfxx, dfyy;
    double	    falpha, fx, fy, fxy, absfx, absfy, absfxy;
    int		    margin, test_succeed, inc, dec;
    float	    zoom;

    zoom = 1.0;
    /* if drawing on canvas (not in indicator button) adjust values by zoomscale */
    if (style != PANEL_LINE) {
	zoom = zoomscale;
	xstart = round(xstart * zoom);
	ystart = round(ystart * zoom);
	xend = round(xend * zoom);
	yend = round(yend * zoom);
	a = round(a * zoom);
	b = round(b * zoom);
	xoff = round(xoff * zoom);
	yoff = round(yoff * zoom);
    }

    if (a == 0 || b == 0)
	return;

    if (!init_point_array(estnpts,estnpts/2)) /* estimate of number of points */
	return;

    x = xstart;
    y = ystart;
    dfx = 2 * (double) a * (double) xstart;
    dfy = 2 * (double) b * (double) ystart;
    dfxx = 2 * (double) a;
    dfyy = 2 * (double) b;

    falpha = 0;
    if (direction) {
	inc = 1;
	dec = -1;
    } else {
	inc = -1;
	dec = 1;
    }
    if (xstart == xend && ystart == yend) {
	test_succeed = margin = 2;
    } else {
	test_succeed = margin = 3;
    }

    if (!add_point(round((xoff + x)/zoom), round((yoff - y)/zoom)))
	/* (error) */ ;
    else
      while (test_succeed) {
	deltax = (dfy < 0) ? inc : dec;
	deltay = (dfx < 0) ? dec : inc;
	fx = falpha + dfx * deltax + a;
	fy = falpha + dfy * deltay + b;
	fxy = fx + fy - falpha;
	absfx = fabs(fx);
	absfy = fabs(fy);
	absfxy = fabs(fxy);

	if ((absfxy <= absfx) && (absfxy <= absfy))
	    falpha = fxy;
	else if (absfy <= absfx) {
	    deltax = 0;
	    falpha = fy;
	} else {
	    deltay = 0;
	    falpha = fx;
	}
	x += deltax;
	y += deltay;
	dfx += (dfxx * deltax);
	dfy += (dfyy * deltay);
	if (!add_point(round((xoff + x)/zoom), round((yoff - y)/zoom)))
	    break;

	if ((abs(x - xend) < margin && abs(y - yend) < margin) &&
	    (x != xend || y != yend))
		test_succeed--;
    }

    if (xstart == xend && ystart == yend)	/* end points should touch */
	if (!add_point(round((xoff + xstart)/zoom),
			round((yoff - ystart)/zoom)))
		too_many_points();

    /* if this is arc type 2 then connect end points to center */
    if (draw_center) {
	if (!add_point(round(xoff/zoom),round(yoff/zoom)))
		too_many_points();
	if (!add_point(round((xoff + xstart)/zoom),round((yoff - ystart)/zoom)))
		too_many_points();
    }
	
    if (draw_points)
	draw_point_array(window, op, thick, style, style_val, JOIN_BEVEL,
			cap_style, fill_style, pen_color, fill_color);
}

/********************* CURVES FOR SPLINES *****************************

 The following spline drawing routines are from

    "X-splines : A Spline Model Designed for the End User"

    by Carole BLANC and Christophe SCHLICK,
    in Proceedings of SIGGRAPH ' 95

***********************************************************************/

#define Q(s)  (-(s)/2.0)
#define EQN_NUMERATOR(dim) \
  (A_blend[0]*p0->dim+A_blend[1]*p1->dim+A_blend[2]*p2->dim+A_blend[3]*p3->dim)

static INLINE double
f_blend(numerator, denominator)
     double numerator, denominator;
{
  double p = 2 * denominator * denominator;
  double u = numerator / denominator;
  double u2 = u * u;

  return (u * u2 * (10 - p + (2*p - 15)*u + (6 - p)*u2));
}

static INLINE double
g_blend(u, q)             /* p equals 2 */
     double u, q;
{
  return(u*(q + u*(2*q + u*(8 - 12*q + u*(14*q - 11 + u*(4 - 5*q))))));
}

static INLINE double
h_blend(u, q)
     double u, q;
{
  double u2=u*u;
   return (u * (q + u * (2 * q + u2 * (-2*q - u*q))));
}

static INLINE
negative_s1_influence(t, s1, A0, A2)
     double       t, s1, *A0 ,*A2;
{
  *A0 = h_blend(-t, Q(s1));
  *A2 = g_blend(t, Q(s1));
}

static INLINE
negative_s2_influence(t, s2, A1, A3)
     double       t, s2, *A1 ,*A3;
{
  *A1 = g_blend(1-t, Q(s2));
  *A3 = h_blend(t-1, Q(s2));
}

static INLINE
positive_s1_influence(k, t, s1, A0, A2)
     int          k;
     double       t, s1, *A0 ,*A2;
{
  double Tk;
  
  Tk = k+1+s1;
  *A0 = (t+k+1<Tk) ? f_blend(t+k+1-Tk, k-Tk) : 0.0;
  
  Tk = k+1-s1;
  *A2 = f_blend(t+k+1-Tk, k+2-Tk);
}

static INLINE
positive_s2_influence(k, t, s2, A1, A3)
     int          k;
     double       t, s2, *A1 ,*A3;
{
  double Tk;

  Tk = k+2+s2; 
  *A1 = f_blend(t+k+1-Tk, k+1-Tk);
  
  Tk = k+2-s2;
  *A3 = (t+k+1>Tk) ? f_blend(t+k+1-Tk, k+3-Tk) : 0.0;
}

static INLINE
point_adding(A_blend, p0, p1, p2, p3)
     F_point     *p0, *p1, *p2, *p3;
     double      *A_blend;
{
  double weights_sum;

  weights_sum = A_blend[0] + A_blend[1] + A_blend[2] + A_blend[3];
  if (!add_point(round(EQN_NUMERATOR(x) / (weights_sum)),
		 round(EQN_NUMERATOR(y) / (weights_sum))))
      too_many_points();
}

static INLINE
point_computing(A_blend, p0, p1, p2, p3, x, y)
     F_point     *p0, *p1, *p2, *p3;
     double      *A_blend;
     int         *x, *y;
{
  double weights_sum;

  weights_sum = A_blend[0] + A_blend[1] + A_blend[2] + A_blend[3];

  *x = round(EQN_NUMERATOR(x) / (weights_sum));
  *y = round(EQN_NUMERATOR(y) / (weights_sum));
}

static float
step_computing(k, p0, p1, p2, p3, s1, s2, precision)
     int     k;
     F_point *p0, *p1, *p2, *p3;
     double  s1, s2;
     float   precision;
{
  double A_blend[4];
  int    xstart, ystart, xend, yend, xmid, ymid, xlength, ylength;
  int    start_to_end_dist, number_of_steps;
  float  step, angle_cos, scal_prod, xv1, xv2, yv1, yv2, sides_length_prod;
  
  /* This function computes the step used to draw the segment (p1, p2)
     (xv1, yv1) : coordinates of the vector from middle to origin
     (xv2, yv2) : coordinates of the vector from middle to extremity */

  if ((s1 == 0) && (s2 == 0))
    return(1.0);              /* only one step in case of linear segment */

  /* compute coordinates of the origin */
  if (s1>0)
    {
      if (s2<0)
	{
	  positive_s1_influence(k, 0.0, s1, &A_blend[0], &A_blend[2]);
	  negative_s2_influence(0.0, s2, &A_blend[1], &A_blend[3]); 
	}
      else
	{
	  positive_s1_influence(k, 0.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.0, s2, &A_blend[1], &A_blend[3]); 
	}
      point_computing(A_blend, p0, p1, p2, p3, &xstart, &ystart);
    }
  else
    {
      xstart = p1->x;
      ystart = p1->y;
    }
  
  /* compute coordinates  of the extremity */
  if (s2>0)
    {
      if (s1<0)
	{
	  negative_s1_influence(1.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 1.0, s2, &A_blend[1], &A_blend[3]);
	}
      else
	{
	  positive_s1_influence(k, 1.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 1.0, s2, &A_blend[1], &A_blend[3]); 
	}
      point_computing(A_blend, p0, p1, p2, p3, &xend, &yend);
    }
  else
    {
      xend = p2->x;
      yend = p2->y;
    }

  /* compute coordinates  of the middle */
  if (s2>0)
    {
      if (s1<0)
	{
	  negative_s1_influence(0.5, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.5, s2, &A_blend[1], &A_blend[3]);
	}
      else
	{
	  positive_s1_influence(k, 0.5, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.5, s2, &A_blend[1], &A_blend[3]); 
	}
    }
  else if (s1<0)
    {
      negative_s1_influence(0.5, s1, &A_blend[0], &A_blend[2]);
      negative_s2_influence(0.5, s2, &A_blend[1], &A_blend[3]);
    }
  else
    {
      positive_s1_influence(k, 0.5, s1, &A_blend[0], &A_blend[2]);
      negative_s2_influence(0.5, s2, &A_blend[1], &A_blend[3]);
    }
  point_computing(A_blend, p0, p1, p2, p3, &xmid, &ymid);

  xv1 = xstart - xmid;
  yv1 = ystart - ymid;
  xv2 = xend - xmid;
  yv2 = yend - ymid;

  scal_prod = xv1*xv2 + yv1*yv2;
  
  sides_length_prod = sqrt((xv1*xv1 + yv1*yv1)*(xv2*xv2 + yv2*yv2));

  /* compute cosinus of origin-middle-extremity angle, which approximates the
     curve of the spline segment */
  angle_cos = scal_prod/sides_length_prod; 

  xlength = xend - xstart;
  ylength = yend - ystart;

  start_to_end_dist = sqrt(xlength*xlength + ylength*ylength);

  /* more steps if segment's origin and extremity are remote */
  number_of_steps = sqrt(start_to_end_dist)/2;

  /* more steps if the curve is high */
  number_of_steps += (int)((1 + angle_cos)*10);

  if (number_of_steps == 0)
    step = 1;
  else
    step = precision/number_of_steps;
  
  if ((step > MAX_SPLINE_STEP) || (step == 0))
    step = MAX_SPLINE_STEP;
  return (step);
}

static void
spline_segment_computing(step, k, p0, p1, p2, p3, s1, s2)
     float   step;
     F_point *p0, *p1, *p2, *p3;
     int     k;
     double  s1, s2;
{
  double A_blend[4];
  double t;
  
  if (s1<0)
    {  
     if (s2<0)
       {
	 for (t=0.0 ; t<1 ; t+=step)
	   {
	     negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	     negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
	   }
       }
     else
       {
	 for (t=0.0 ; t<1 ; t+=step)
	   {
	     negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	     positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
	   }
       }
   }
  else if (s2<0)
    {
      for (t=0.0 ; t<1 ; t+=step)
	   {
	     positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
	     negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
	   }
    }
  else
    {
      for (t=0.0 ; t<1 ; t+=step)
	   {
	     positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
	     positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, p0, p1, p2, p3);
	   } 
    }
}



/* redraw all the picture objects */

redraw_images(obj)
    F_compound	   *obj;
{
    F_line	   *l;
    F_compound	   *c;

    for (c = obj->compounds; c != NULL; c = c->next) {
	redraw_images(c);
    }
    for (l = obj->lines; l != NULL; l = l->next) {
	if (l->type == T_PICTURE && l->pic->numcols > 0)
		redisplay_line(l);
    }
}


