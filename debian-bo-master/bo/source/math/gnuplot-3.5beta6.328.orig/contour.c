#ifndef lint
static char *RCSid = "$Id: contour.c,v 1.25 1996/12/08 13:08:07 drd Exp $";
#endif


/* GNUPLOT - contour.c */
/*
 * Copyright (C) 1986 - 1993, 1996   Thomas Williams, Colin Kelley
 *
 * Permission to use, copy, and distribute this software and its
 * documentation for any purpose with or without fee is hereby granted, 
 * provided that the above copyright notice appear in all copies and 
 * that both that copyright notice and this permission notice appear 
 * in supporting documentation.
 *
 * Permission to modify the software is granted, but not the right to
 * distribute the modified code.  Modifications are to be distributed 
 * as patches to released version.
 *  
 * This software is provided "as is" without express or implied warranty.
 * 
 *
 * AUTHORS
 * 
 *   Original Software:
 *       Gershon Elber
 * 
 *   Improvements to the numerical algorithms:
 *        Hans-Martin Keller, Nov 1995 (hkeller@gwdg.de)
 *
 * There is a mailing list for gnuplot users. Note, however, that the
 * newsgroup 
 *	comp.graphics.apps.gnuplot 
 * is identical to the mailing list (they
 * both carry the same set of messages). We prefer that you read the
 * messages through that newsgroup, to subscribing to the mailing list.
 * (If you can read that newsgroup, and are already on the mailing list,
 * please send a message to majordomo@dartmouth.edu, asking to be
 * removed from the mailing list.)
 *
 * The address for mailing to list members is
 *	   info-gnuplot@dartmouth.edu
 * and for mailing administrative requests is 
 *	   majordomo@dartmouth.edu
 * The mailing list for bug reports is 
 *	   bug-gnuplot@dartmouth.edu
 * The list of those interested in beta-test versions is
 *	   info-gnuplot-beta@dartmouth.edu
 */

#include <math.h> /* get prototype for sqrt */
#include "plot.h"
#include "setshow.h"
#include <stdio.h>

#define DEFAULT_NUM_APPROX_PTS  5
#define DEFAULT_BSPLINE_ORDER  3
#define MAX_NUM_APPROX_PTS      100
#define MAX_BSPLINE_ORDER      10         /* ?? Not used ?? */

/* for some reason these symbols are also defined in plot.h under different */
/* names */
#define INTERP_NOTHING  CONTOUR_KIND_LINEAR     /* Kind of interpolations on contours. */
#define INTERP_CUBIC    CONTOUR_KIND_CUBIC_SPL  /* Cubic spline interp. */
#define APPROX_BSPLINE  CONTOUR_KIND_BSPLINE    /* Bspline interpolation. */

#define ACTIVE   1                    /* Status of edges at certain Z level. */
#define INACTIVE 2
#define INNER_MESH 1                             /* position of edge in mesh */
#define BOUNDARY 2
#define DIAGONAL 3

#define OPEN_CONTOUR     1                                 /* Contour kinds. */
#define CLOSED_CONTOUR   2

#define EPSILON  1e-5              /* Used to decide if two float are equal. */
#ifdef INFINITY
#undef INFINITY
#endif
#define INFINITY 1e10

#ifndef TRUE
#define TRUE     -1
#define FALSE    0
#endif

#define MAX_POINTS_PER_CNTR 	100
/* #define SHIFT_Z_EPSILON		0.000301060 */ /* Dec. change of poly bndry hit.*/

#define ABS(x)  ((x) > 0 ? (x) : (-(x)))
#define SQR(x)  ((x) * (x))

/* typedef double table_entry[4];	*/       /* Cubic spline interpolation 4 coef. */

struct vrtx_struct {
	double X, Y, Z;                       /* The coordinates of this vertex. */
	struct vrtx_struct *next;                             /* To chain lists. */
};

struct edge_struct {
	struct poly_struct *poly[2];   /* Each edge belongs to up to 2 polygons. */
	struct vrtx_struct *vertex[2];   /* The two extreme points of this edge. */
	struct edge_struct *next;                             /* To chain lists. */
	int status, /* Status flag to mark edges in scanning at certain Z level. */
	position;        /* position in mesh: INNER_MESH, BOUNDARY or DIAGONNAL. */
};

struct poly_struct {
	struct edge_struct *edge[3];           /* As we do triangolation here... */
	struct poly_struct *next;                             /* To chain lists. */
};

struct cntr_struct {           /* Contours are saved using this struct list. */
	double X, Y;                          /* The coordinates of this vertex. */
	struct cntr_struct *next;                             /* To chain lists. */
};

static int test_boundary;    /* If TRUE look for contours on boundary first. */
static struct gnuplot_contours *contour_list = NULL;
static double crnt_cntr[MAX_POINTS_PER_CNTR * 2];
static int crnt_cntr_pt_index = 0;
static double contour_level = 0.0;
/* static table_entry *hermit_table = NULL; */   /* Hold hermite table constants. */
static int num_approx_pts = DEFAULT_NUM_APPROX_PTS;/* # pts per approx/inter.*/
static int bspline_order = DEFAULT_BSPLINE_ORDER;   /* Bspline order to use. */
static int interp_kind = INTERP_NOTHING;  /* Linear, Cubic interp., Bspline. */


static void add_cntr_point __PROTO((double x, double y));
static void end_crnt_cntr __PROTO((void));
static void gen_contours __PROTO((struct edge_struct *p_edges, double z_level, double x_min, double x_max, double y_min, double y_max));
static int update_all_edges __PROTO((struct edge_struct *p_edges, double z_level));
static struct cntr_struct *gen_one_contour __PROTO((struct edge_struct *p_edges, double z_level, int *contr_kind, int *num_active));
static struct cntr_struct *trace_contour __PROTO((struct edge_struct *pe_start, double z_level, int *num_active, int contr_kind));
static struct cntr_struct *update_cntr_pt __PROTO((struct edge_struct *p_edge, double z_level));
static int fuzzy_equal __PROTO((double x, double y));
static void gen_triangle __PROTO((int num_isolines, struct iso_curve *iso_lines,
    struct poly_struct **p_polys, struct edge_struct **p_edges, struct
    vrtx_struct **p_vrts, double *x_min, double *y_min, double *z_min,
    double *x_max, double *y_max, double *z_max));
static struct vrtx_struct *gen_vertices __PROTO((int grid_x_max, struct coordinate
    GPHUGE *points, double *x_min, double *y_min, double *z_min, double *x_max,
    double *y_max, double *z_max));
static struct edge_struct *gen_edges __PROTO((int grid_x_max, struct vrtx_struct *p_vrtx, struct edge_struct **pe_tail));
static struct edge_struct *gen_edges_middle __PROTO((int grid_x_max, struct
    vrtx_struct *p_vrtx1, struct vrtx_struct *p_vrtx2, struct edge_struct **pe_tail));
static struct poly_struct *gen_polys __PROTO((int grid_x_max, struct edge_struct
    *p_edge1, struct edge_struct *p_edge_middle, struct edge_struct *p_edge2,
    struct poly_struct **pp_tail));
static void put_contour __PROTO((struct cntr_struct *p_cntr, double z_level,
     double x_min, double x_max, double y_min, double y_max, int contr_kind));
static void put_contour_nothing __PROTO((struct cntr_struct *p_cntr));
static void put_contour_cubic __PROTO((struct cntr_struct *p_cntr, double z_level,
     double x_min, double x_max, double y_min, double y_max, int contr_kind));
static void put_contour_bspline __PROTO((struct cntr_struct *p_cntr, double z_level,
     double x_min, double x_max, double y_min, double y_max, int contr_kind));
static void free_contour __PROTO((struct cntr_struct *p_cntr));
static int count_contour __PROTO((struct cntr_struct *p_cntr));
static int gen_cubic_spline __PROTO((int num_pts, struct cntr_struct *p_cntr, double d2x[],
     double d2y[], double delta_t[], int contr_kind, double unit_x, double unit_y));
static void intp_cubic_spline __PROTO((int n, struct cntr_struct *p_cntr, double d2x[],
     double d2y[], double delta_t[], int n_intpol));
static int solve_cubic_1 __PROTO((tri_diag m[], int n));
static void solve_cubic_2 __PROTO((tri_diag m[], double x[], int n));
/* static int solve_tri_diag __PROTO((tri_diag m[], double r[], double x[], int n));  see "protos.h" */
static void gen_bspline_approx __PROTO((struct cntr_struct *p_cntr, int num_of_points, int order, int contr_kind));
static void eval_bspline __PROTO((double t, struct cntr_struct *p_cntr,
     int num_of_points, int order, int j, int contr_kind, double *x, double *y));
static double fetch_knot __PROTO((int contr_kind, int num_of_points, int order, int i));

/*
 * Entry routine to this whole set of contouring module.
 */
struct gnuplot_contours *contour(num_isolines, iso_lines,
				 ZLevels, approx_pts, int_kind, order1,
				 levels_kind, levels_list)
int num_isolines;
struct iso_curve *iso_lines;
int ZLevels, approx_pts, int_kind, order1;
int levels_kind;
double *levels_list;
{
	int i;
	int num_of_z_levels;  /* # Z contour levels. */
	struct poly_struct *p_polys, *p_poly;
	struct edge_struct *p_edges, *p_edge;
	struct vrtx_struct *p_vrts, *p_vrtx;
	double x_min, y_min, z_min, x_max, y_max, z_max, z, dz;
	struct gnuplot_contours *save_contour_list;

	num_of_z_levels = ZLevels;
	num_approx_pts = approx_pts;
	bspline_order = order1 - 1;
	interp_kind = int_kind;

	contour_list = NULL;

	gen_triangle(num_isolines, iso_lines, &p_polys, &p_edges, &p_vrts,
		&x_min, &y_min, &z_min, &x_max, &y_max, &z_max);
	crnt_cntr_pt_index = 0;

	dz = (z_max - z_min) / (num_of_z_levels + 1);
	z = z_min;
	if(levels_kind == LEVELS_AUTO) {
		dz = fabs(z_max - z_min);
		if (dz == 0)
			return NULL; /* empty z range ? */
		dz = set_tic(log10(dz), ((int)ZLevels + 1)*2);
		z = floor(z_min/dz)*dz;
		num_of_z_levels = (int) floor((z_max - z)/dz);
	}
	for (i = 0; i < num_of_z_levels; i++) {
		switch(levels_kind) {
			case LEVELS_AUTO:
				z += dz;
				break;
			case LEVELS_INCREMENTAL:
				z = levels_list[0] + i * levels_list[1];
				break;
			case LEVELS_DISCRETE:
				z = is_log_z ? log(levels_list[i])/log_base_log_z : levels_list[i];
				break;
		}
		contour_level = z;
		save_contour_list = contour_list;
		gen_contours(p_edges, z, x_min, x_max, y_min, y_max);
		if(contour_list != save_contour_list) {
			contour_list->isNewLevel = 1;
			sprintf(contour_list->label, contour_format, is_log_z ? pow(base_log_z, z) : z);
		}
	}

	/* Free all contouring related temporary data. */
	while (p_polys) {
	p_poly = p_polys -> next;
	free (p_polys);
	p_polys = p_poly;
	}
	while (p_edges) {
	p_edge = p_edges -> next;
	free (p_edges);
	p_edges = p_edge;
	}
	while (p_vrts) {
	p_vrtx = p_vrts -> next;
	free (p_vrts);
	p_vrts = p_vrtx;
	}

    return contour_list;
}

/*
 * Adds another point to the currently build contour.
 */
static void add_cntr_point(x, y)
double x, y;
{
    int index;

    if (crnt_cntr_pt_index >= MAX_POINTS_PER_CNTR-1) {
	index = crnt_cntr_pt_index - 1;
	end_crnt_cntr();
	crnt_cntr[0] = crnt_cntr[index * 2];
	crnt_cntr[1] = crnt_cntr[index * 2 + 1];
	crnt_cntr_pt_index = 1; /* Keep the last point as first of this one. */
    }
    crnt_cntr[crnt_cntr_pt_index * 2] = x;
    crnt_cntr[crnt_cntr_pt_index * 2 + 1] = y;
    crnt_cntr_pt_index++;
}

/*
 * Done with current contour - create gnuplot data structure for it.
 */
static void end_crnt_cntr()
{
    int i;
    struct gnuplot_contours *cntr = (struct gnuplot_contours *)
        alloc((unsigned long)sizeof(struct gnuplot_contours), "gnuplot_contour");
    cntr->coords = (struct coordinate GPHUGE *)
       alloc( (unsigned long)sizeof(struct coordinate)
           * (unsigned long)crnt_cntr_pt_index, "contour coords" );

    for (i=0; i<crnt_cntr_pt_index; i++) {
	cntr->coords[i].x = crnt_cntr[i * 2];
	cntr->coords[i].y = crnt_cntr[i * 2 + 1];
	cntr->coords[i].z = contour_level;
    }
    cntr->num_pts = crnt_cntr_pt_index;

    cntr->next = contour_list;
    contour_list = cntr;
	contour_list->isNewLevel = 0;

    crnt_cntr_pt_index = 0;
}

/*
 * Generates all contours by tracing the intersecting triangles.
 */
static void gen_contours(p_edges, z_level, x_min, x_max, y_min, y_max)
struct edge_struct *p_edges;
double z_level, x_min, x_max, y_min, y_max;
{
    int num_active,                        /* Number of edges marked ACTIVE. */
	contr_kind;                  /* One of OPEN_CONTOUR, CLOSED_CONTOUR. */
    struct cntr_struct *p_cntr;

    num_active = update_all_edges(p_edges, z_level);           /* Do pass 1. */

    test_boundary = TRUE;        /* Start to look for contour on boundaries. */

    while (num_active > 0) {                                   /* Do Pass 2. */
        /* Generate One contour (and update MumActive as needed): */
	p_cntr = gen_one_contour(p_edges, z_level, &contr_kind, &num_active);
        /* Emit it in requested format: */
	put_contour(p_cntr, z_level, x_min, x_max, y_min, y_max, contr_kind);
    }
}

/*
 * Does pass 1, or marks the edges which are active (crosses this z_level)
 * as ACTIVE, and the others as INACTIVE:
 * Returns number of active edges (marked ACTIVE).
 */
static int update_all_edges(p_edges, z_level)
struct edge_struct *p_edges;
double z_level;
{
    int count = 0;

    while (p_edges) {
        /* use the same test at both vertices to avoid roundoff errors */
        if ( (p_edges -> vertex[0] -> Z >= z_level) !=
             (p_edges -> vertex[1] -> Z >= z_level) ) {
            p_edges -> status = ACTIVE;
            count++;
        }
	else p_edges -> status = INACTIVE;
	p_edges = p_edges -> next;
    }

    return count;
}

/*
 * Does pass 2, or find one complete contour out of the triangolation data base:
 * Returns a pointer to the contour (as linked list), contr_kind is set to
 * one of OPEN_CONTOUR, CLOSED_CONTOUR, and num_active is updated.
 */
static struct cntr_struct *gen_one_contour(p_edges, z_level, contr_kind, num_active)
struct edge_struct *p_edges;
double z_level;
int *contr_kind, *num_active;
{
    struct edge_struct *pe_temp;

    if (test_boundary) {    /* Look for something to start with on boundary: */
        pe_temp = p_edges;
        while (pe_temp) {
            if ( (pe_temp -> status == ACTIVE) && (pe_temp -> position == BOUNDARY) )
                break;
            pe_temp = pe_temp -> next;
        }
        if (!pe_temp) test_boundary = FALSE;/* No more contours on boundary. */
        else {
            *contr_kind = OPEN_CONTOUR;
            return trace_contour(pe_temp, z_level, num_active, *contr_kind);
        }
    }

    if (!test_boundary) {        /* Look for something to start with inside: */
        pe_temp = p_edges;
        while (pe_temp) {
            if ( (pe_temp->status == ACTIVE) && (!(pe_temp->position == BOUNDARY)) )
                break;
            pe_temp = pe_temp -> next;
        }
        if (!pe_temp) {
            *num_active = 0;
            return NULL;
        }
        else {
            *contr_kind = CLOSED_CONTOUR;
            return trace_contour(pe_temp, z_level, num_active, *contr_kind);
        }
    }
    return NULL;                     /* We should never be here, but lint... */
}

/*
 * Search the data base along a contour starts at the edge pe_start until
 * a boundary edge is detected or until we close the loop back to pe_start.
 * Returns a linked list of all the points on the contour
 * Also decreases num_active by the number of points on contour.
 */
static struct cntr_struct *trace_contour(pe_start, z_level, num_active,
                                                                contr_kind)
struct edge_struct *pe_start;
double z_level;
int *num_active, contr_kind;
{
    int i;
    struct cntr_struct *p_cntr, *pc_tail;
    struct edge_struct *p_edge = pe_start, *p_next_edge;
    struct poly_struct *p_poly, *PLastpoly = NULL;

    /* Generate the header of the contour - the point on pe_start. */
    if (contr_kind == OPEN_CONTOUR) pe_start -> status = INACTIVE;
    (*num_active)--;
    p_cntr = pc_tail = update_cntr_pt(pe_start, z_level);

    do {
        /* Find polygon to continue (Not where we came from - PLastpoly): */
        if (p_edge -> poly[0] == PLastpoly) p_poly = p_edge -> poly[1];
        else p_poly = p_edge -> poly[0];
        p_next_edge = NULL;               /* In case of error, remains NULL. */
        for (i=0; i<3; i++)              /* Test the 3 edges of the polygon: */
            if (p_poly -> edge[i] != p_edge)
                if (p_poly -> edge[i] -> status == ACTIVE)
                    p_next_edge = p_poly -> edge[i];
        if (!p_next_edge) {
            pc_tail -> next = NULL;
            free_contour(p_cntr);
            return NULL;
        }
        p_edge = p_next_edge;
        PLastpoly = p_poly;
        p_edge -> status = INACTIVE;
        (*num_active)--;

        /* Do not allocate contour points on diagonal edges */
        if ( p_edge->position != DIAGONAL ) {

            pc_tail -> next = update_cntr_pt(p_edge, z_level);

            /* Remove nearby points */
            if ( fuzzy_equal(pc_tail -> X, pc_tail -> next -> X)
                && fuzzy_equal(pc_tail -> Y, pc_tail -> next -> Y)) {

                free((char *) pc_tail -> next);
            }
            else pc_tail = pc_tail -> next;
        }
    }
    while ( (pe_start != p_edge) && (!(p_edge->position == BOUNDARY)) );
    pc_tail -> next = NULL;

    /* For CLOSED_CONTOUR the first and last point should be equal */
    if ( pe_start == p_edge ) {
      (p_cntr -> X) = (pc_tail -> X);
      (p_cntr -> Y) = (pc_tail -> Y);
    }
    return p_cntr;
}

/*
 * Allocates one contour location and update it to to correct position
 * according to z_level and edge p_edge.
 */
static struct cntr_struct *update_cntr_pt(p_edge, z_level)
struct edge_struct *p_edge;
double z_level;
{
    double t;
    struct cntr_struct *p_cntr;

    t = (z_level - p_edge -> vertex[0] -> Z) /
        (p_edge -> vertex[1] -> Z - p_edge -> vertex[0] -> Z);

    /* test if t is out of interval [0:1] (should not happen but who knows ...) */
    t = ( t < 0.0 ? 0.0 : t );
    t = ( t > 1.0 ? 1.0 : t );

    p_cntr = (struct cntr_struct *)
        alloc((unsigned long)sizeof(struct cntr_struct), "contour cntr_struct");

    p_cntr -> X = p_edge -> vertex[1] -> X * t +
                  p_edge -> vertex[0] -> X * (1-t);
    p_cntr -> Y = p_edge -> vertex[1] -> Y * t +
                  p_edge -> vertex[0] -> Y * (1-t);
    return p_cntr;
}

/*
 * Simple routine to decide if two real values are equal by simply
 * calculating the relative/absolute error between them (< EPSILON).
 */
static int fuzzy_equal(x, y)
double x, y;
{
    if (ABS(x) > EPSILON)                       /* Calculate relative error: */
        return (ABS((x - y) / x) < EPSILON);
    else                                        /* Calculate absolute error: */
        return (ABS(x - y) < EPSILON);
}

/*
 * Generate the triangles.
 * Returns the lists (vrtxs edges & polys) via pointers to their heads.
 */
static void gen_triangle(num_isolines, iso_lines, p_polys, p_edges,
        p_vrts, x_min, y_min, z_min, x_max, y_max, z_max)
int num_isolines;
struct iso_curve *iso_lines;
struct poly_struct **p_polys;
struct edge_struct **p_edges;
struct vrtx_struct **p_vrts;
double *x_min, *y_min, *z_min, *x_max, *y_max, *z_max;
{
    int i, grid_x_max = iso_lines->p_count;
    struct vrtx_struct *p_vrtx1, *p_vrtx2, *pv_temp;
    struct edge_struct *p_edge1, *p_edge2, *pe_tail1, *pe_tail2, *pe_temp,
                      *p_edge_middle, *pe_m_tail;
    struct poly_struct *p_poly, *pp_tail;

    *p_polys = NULL;
    *p_edges = NULL;
    *p_vrts = NULL;
    *z_min = INFINITY;
    *y_min = INFINITY;
    *x_min = INFINITY;
    *z_max = -INFINITY;
    *y_max = -INFINITY;
    *x_max = -INFINITY;

    /* Read 1st row. */
    p_vrtx1 = gen_vertices(grid_x_max, iso_lines->points,
                           x_min, y_min, z_min, x_max, y_max, z_max);
    *p_vrts = p_vrtx1;
    /* Gen. its edges.*/
    pe_temp = p_edge1 = gen_edges(grid_x_max, p_vrtx1, &pe_tail1);
    for (i = 1; i < grid_x_max; i++) {/* Mark one side of edges as boundary. */
        pe_temp -> poly[1] = NULL;
        pe_temp = pe_temp -> next;
    }
    for (i = 1; i < num_isolines; i++) { /* Read next column and gen. polys. */
        iso_lines = iso_lines->next;
        /* Get row into list. */
        p_vrtx2 = gen_vertices(grid_x_max, iso_lines->points,
                               x_min, y_min, z_min, x_max, y_max, z_max);
        /* Generate its edges. */
        p_edge2 = gen_edges(grid_x_max, p_vrtx2, &pe_tail2);
        /* Generate edges from one vertex list to the other one: */
        p_edge_middle = gen_edges_middle(grid_x_max, p_vrtx1, p_vrtx2,
                                                                 &pe_m_tail);

        /* Now we can generate the polygons themselves (triangles). */
        p_poly = gen_polys(grid_x_max, p_edge1, p_edge_middle, p_edge2,
                                                                 &pp_tail);
        pe_tail1 -> next = (*p_edges);      /* Chain new edges to main list. */
        pe_m_tail -> next = p_edge1;
        *p_edges = p_edge_middle;
        pe_tail1 = pe_tail2;
        p_edge1 = p_edge2;

        pv_temp = p_vrtx2;
        while (pv_temp -> next) pv_temp = pv_temp -> next;
        pv_temp -> next = *p_vrts;
        *p_vrts = p_vrtx1 = p_vrtx2;

        pp_tail -> next = (*p_polys);       /* Chain new polys to main list. */
        *p_polys = p_poly;
    }

    pe_temp = p_edge1;
    for (i = 1; i < grid_x_max; i++) {/* Mark one side of edges as boundary. */
        pe_temp -> poly[0] = NULL;
        pe_temp = pe_temp -> next;
    }

    pe_tail1 -> next = (*p_edges);    /* Chain last edges list to main list. */
    *p_edges = p_edge1;

    /* Update the boundary flag, saved in each edge, and update indexes: */
    pe_temp = (*p_edges);
    i = 1;

    while (pe_temp) {
        if ( (!(pe_temp -> poly[0])) || (!(pe_temp -> poly[1])) )
            (pe_temp -> position) = BOUNDARY;
        pe_temp = pe_temp -> next;
    }
}

/*
 * Handles grid_x_max 3D points (One row) and generate linked list for them.
 */
static struct vrtx_struct *gen_vertices(grid_x_max, points,
                                      x_min, y_min, z_min, x_max, y_max, z_max)
int grid_x_max;
struct coordinate GPHUGE *points;
double *x_min, *y_min, *z_min, *x_max, *y_max, *z_max;
{
    int i;
     /* avoid gcc -Wall warning about unused variables */
    struct vrtx_struct *p_vrtx=NULL, *pv_tail=NULL, *pv_temp;

    for (i=0; i<grid_x_max; i++) {/* Get a point and generate the structure. */
        pv_temp = (struct vrtx_struct *)
             alloc((unsigned long)sizeof(struct vrtx_struct), "contour vertex");
        pv_temp -> X = points[i].x;
        pv_temp -> Y = points[i].y;
        pv_temp -> Z = points[i].z;

        if (pv_temp -> X > *x_max) *x_max = pv_temp -> X; /* Update min/max. */
        if (pv_temp -> Y > *y_max) *y_max = pv_temp -> Y;
        if (pv_temp -> Z > *z_max) *z_max = pv_temp -> Z;
        if (pv_temp -> X < *x_min) *x_min = pv_temp -> X;
        if (pv_temp -> Y < *y_min) *y_min = pv_temp -> Y;
        if (pv_temp -> Z < *z_min) *z_min = pv_temp -> Z;

        if (i == 0)                                  /* First vertex in row: */
            p_vrtx = pv_tail = pv_temp;
        else {
            pv_tail -> next = pv_temp;      /* Stick new record as last one. */
            pv_tail = pv_tail -> next;       /* And continue to last record. */
        }
    }
    pv_tail -> next = NULL;

    return p_vrtx;
}

/*
 * Combines N vertices in pair to form N-1 edges.
 * Returns pointer to the edge list (pe_tail will point on last edge in list).
 */
static struct edge_struct *gen_edges(grid_x_max, p_vrtx, pe_tail)
int grid_x_max;
struct vrtx_struct *p_vrtx;
struct edge_struct **pe_tail;
{
    int i;
    struct edge_struct *p_edge=NULL, *pe_temp;

    for (i=0; i<grid_x_max-1; i++) {         /* Generate grid_x_max-1 edges: */
        pe_temp = (struct edge_struct *)
             alloc((unsigned long)sizeof(struct edge_struct), "contour edge");
        pe_temp -> vertex[0] = p_vrtx;              /* First vertex of edge. */
        p_vrtx = p_vrtx -> next;                     /* Skip to next vertex. */
        pe_temp -> vertex[1] = p_vrtx;             /* Second vertex of edge. */
        pe_temp -> position = INNER_MESH;                /* position in mesh */
        if (i == 0)                                    /* First edge in row: */
            p_edge = (*pe_tail) = pe_temp;
        else {
            (*pe_tail) -> next = pe_temp;   /* Stick new record as last one. */
            *pe_tail = (*pe_tail) -> next;   /* And continue to last record. */
        }
    }
    (*pe_tail) -> next = NULL;

    return p_edge;
}

/*
 * Combines 2 lists of N vertices each into edge list:
 * The dots (.) are the vertices list, and the              .  .  .  .
 *  edges generated are alternations of vertical edges      |\ |\ |\ |
 *  (|) and diagonal ones (\).                              | \| \| \|
 *  A pointer to edge list (alternate | , \) is returned    .  .  .  .
 * Note this list will have (2*grid_x_max-1) edges (pe_tail points on last
 * record).
 */
static struct edge_struct *gen_edges_middle(grid_x_max, p_vrtx1, p_vrtx2,
                                                                pe_tail)
int grid_x_max;
struct vrtx_struct *p_vrtx1, *p_vrtx2;
struct edge_struct **pe_tail;
{
    int i;
    struct edge_struct *p_edge, *pe_temp;

    /* Gen first (|). */
    pe_temp = (struct edge_struct *)
        alloc((unsigned long)sizeof(struct edge_struct), "contour edge");
    pe_temp -> vertex[0] = p_vrtx2;                 /* First vertex of edge. */
    pe_temp -> vertex[1] = p_vrtx1;                /* Second vertex of edge. */
    pe_temp -> position = INNER_MESH;                    /* position in mesh */
    p_edge = (*pe_tail) = pe_temp;

    /* Advance in vrtx list grid_x_max-1 times, and gen. 2 edges /| for each.*/
    for (i=0; i<grid_x_max-1; i++) {
        /* The / edge. */
        pe_temp = (struct edge_struct *)
            alloc((unsigned long)sizeof(struct edge_struct), "contour edge");
        pe_temp -> vertex[0] = p_vrtx1;             /* First vertex of edge. */
        pe_temp -> vertex[1] = p_vrtx2 -> next;    /* Second vertex of edge. */
        pe_temp -> position = DIAGONAL;                  /* position in mesh */
        (*pe_tail) -> next = pe_temp;       /* Stick new record as last one. */
        *pe_tail = (*pe_tail) -> next;       /* And continue to last record. */

        /* The | edge. */
        pe_temp = (struct edge_struct *)
            alloc((unsigned long)sizeof(struct edge_struct), "contour edge");
        pe_temp -> vertex[0] = p_vrtx2 -> next;     /* First vertex of edge. */
        pe_temp -> vertex[1] = p_vrtx1 -> next;    /* Second vertex of edge. */
        pe_temp -> position = INNER_MESH;                /* position in mesh */
        (*pe_tail) -> next = pe_temp;       /* Stick new record as last one. */
        *pe_tail = (*pe_tail) -> next;       /* And continue to last record. */

        p_vrtx1 = p_vrtx1 -> next;   /* Skip to next vertices in both lists. */
        p_vrtx2 = p_vrtx2 -> next;
    }
    (*pe_tail) -> next = NULL;

    return p_edge;
}

/*
 * Combines 3 lists of edges into triangles:
 * 1. p_edge1: Top horizontal edge list:        -----------------------
 * 2. p_edge_middge: middle edge list:         |\  |\  |\  |\  |\  |\  |
 *                                             |  \|  \|  \|  \|  \|  \|
 * 3. p_edge2: Bottom horizontal edge list:     -----------------------
 * Note that p_edge1/2 lists has grid_x_max-1 edges, while p_edge_middle has
 * (2*grid_x_max-1) edges.
 * The routine simple scans the two list    Upper 1         Lower
 * and generate two triangle upper one        ----         | \
 * and lower one from the lists:             0\   |2      0|   \1
 * (Nums. are edges order in polys)             \ |         ----
 * The routine returns a pointer to a                         2
 * polygon list (pp_tail points on last polygon).          1
 *                                                   -----------
 * In addition, the edge lists are updated -        | \   0     |
 * each edge has two pointers on the two            |   \       |
 * (one active if boundary) polygons which         0|1   0\1   0|1
 * uses it. These two pointer to polygons           |       \   |
 * are named: poly[0], poly[1]. The diagram         |    1    \ |
 * on the right show how they are used for the       -----------
 * upper and lower polygons.                             0
 */
static struct poly_struct *gen_polys(grid_x_max, p_edge1, p_edge_middle,
                                                        p_edge2, pp_tail)
int grid_x_max;
struct edge_struct *p_edge1, *p_edge_middle, *p_edge2;
struct poly_struct **pp_tail;
{
    int i;
    struct poly_struct *p_poly=NULL, *pp_temp;

    p_edge_middle -> poly[0] = NULL;                        /* Its boundary! */

    /* Advance in vrtx list grid_x_max-1 times, and gen. 2 polys for each. */
    for (i=0; i<grid_x_max-1; i++) {
        /* The Upper. */
        pp_temp = (struct poly_struct *)
            alloc((unsigned long)sizeof(struct poly_struct), "contour poly");
        /* Now update polys about its edges, and edges about the polygon. */
        pp_temp -> edge[0] = p_edge_middle -> next;
        p_edge_middle -> next -> poly[1] = pp_temp;
        pp_temp -> edge[1] = p_edge1;
        p_edge1 -> poly[0] = pp_temp;
        pp_temp -> edge[2] = p_edge_middle -> next -> next;
        p_edge_middle -> next -> next -> poly[0] = pp_temp;
        if (i == 0)                                /* Its first one in list: */
            p_poly = (*pp_tail) = pp_temp;
        else {
            (*pp_tail) -> next = pp_temp;
            *pp_tail = (*pp_tail) -> next;
        }

        /* The Lower. */
        pp_temp = (struct poly_struct *)
            alloc((unsigned long)sizeof(struct poly_struct), "contour poly");

        /* Now update polys about its edges, and edges about the polygon. */
        pp_temp -> edge[0] = p_edge_middle;
        p_edge_middle -> poly[1] = pp_temp;
        pp_temp -> edge[1] = p_edge_middle -> next;
        p_edge_middle -> next -> poly[0] = pp_temp;
        pp_temp -> edge[2] = p_edge2;
        p_edge2 -> poly[1] = pp_temp;
        (*pp_tail) -> next = pp_temp;
        *pp_tail = (*pp_tail) -> next;

        p_edge1 = p_edge1 -> next;
        p_edge2 = p_edge2 -> next;
        p_edge_middle = p_edge_middle -> next -> next;
    }
    p_edge_middle -> poly[1] = NULL;                        /* Its boundary! */
    (*pp_tail) -> next = NULL;

    return p_poly;
}

/*
 * Calls the (hopefully) desired interpolation/approximation routine.
 */
static void put_contour(p_cntr, z_level, x_min, x_max, y_min, y_max, contr_kind)
struct cntr_struct *p_cntr;
double z_level, x_min, x_max, y_min, y_max;
int contr_kind;
{
    if (!p_cntr) return;            /* Nothing to do if it is empty contour. */

    switch (interp_kind) {
        case INTERP_NOTHING:              /* No interpolation/approximation. */
            put_contour_nothing(p_cntr);
            break;
        case INTERP_CUBIC:                    /* Cubic spline interpolation. */
            put_contour_cubic(p_cntr, z_level, x_min, x_max, y_min, y_max,
                                                                contr_kind);
            break;
        case APPROX_BSPLINE:                       /* Bspline approximation. */
            put_contour_bspline(p_cntr, z_level, x_min, x_max, y_min, y_max,
                                                                contr_kind);
            break;
    }
    free_contour(p_cntr);
}

/*
 * Simply puts contour coordinates in order with no interpolation or
 * approximation.
 */
static void put_contour_nothing(p_cntr)
struct cntr_struct *p_cntr;
{
    while (p_cntr) {
        add_cntr_point(p_cntr -> X, p_cntr -> Y);
        p_cntr = p_cntr -> next;
    }
    end_crnt_cntr();
}

/*
 * Generate a cubic spline curve through the points (x_i,y_i) which are
 * stored in the linked list p_cntr.
 * The spline is defined as a 2d-function s(t) = (x(t),y(t)), where the
 * parameter t is the length of the linear stroke.
 */
static void put_contour_cubic(p_cntr, z_level, x_min, x_max, y_min, y_max,
                                                                 contr_kind)
struct cntr_struct *p_cntr;
double z_level, x_min, x_max, y_min, y_max;
int contr_kind;
{
    int num_pts, num_intpol;
    double unit_x, unit_y;                      /* To define norm (x,y)-plane */
    double *delta_t;                           /* Interval length t_{i+1}-t_i */
    double *d2x, *d2y;               /* Second derivatives x''(t_i), y''(t_i) */
    struct cntr_struct *pc_tail;

    num_pts = count_contour(p_cntr);          /* Number of points in contour. */

    pc_tail = p_cntr;                                      /* Find last point.*/
    while (pc_tail -> next) pc_tail = pc_tail -> next;

    if (contr_kind == CLOSED_CONTOUR) {
        /* Test if first and last point are equal (should be) */
        if ( !(fuzzy_equal(pc_tail -> X, p_cntr -> X)
            && fuzzy_equal(pc_tail -> Y, p_cntr -> Y)) ) {
            pc_tail -> next = p_cntr;  /* Close contour list - make it circular.*/
            num_pts++;
        }
    }

    delta_t= (double *)
        alloc((unsigned long) (sizeof(double) * num_pts), "contour delta_t");
    d2x= (double *)
        alloc((unsigned long) (sizeof(double) * num_pts), "contour d2x");
    d2y= (double *)
        alloc((unsigned long) (sizeof(double) * num_pts), "contour d2y");

    /* Width and hight of the grid is used at unit length (2d-norm) */
    unit_x= x_max - x_min;
    unit_y= y_max - y_min;
    unit_x = ( unit_x > EPSILON ? unit_x : EPSILON );   /* should not be zero */
    unit_y = ( unit_y > EPSILON ? unit_y : EPSILON );

    if (num_pts > 2) {
        /*
         * Calculate second derivatives d2x[], d2y[] and interval lengths delta_t[]:
         */
        if (!gen_cubic_spline(num_pts, p_cntr, d2x, d2y, delta_t,
                                             contr_kind, unit_x, unit_y)) {
            free((char *) delta_t);
            free((char *) d2x);  free((char *) d2y);
            if (contr_kind==CLOSED_CONTOUR) pc_tail->next = NULL; /* Un-circular list */
            return;
        }
    }
    /* If following (num_pts > 1) is TRUE then exactly 2 points in contour.  */
    else if (num_pts > 1) {
        /* set all second derivatives to zero, interval length to 1 */
        d2x[0] = 0.;  d2y[0] = 0.;
        d2x[1] = 0.;  d2y[1] = 0.;
        delta_t[0] = 1.;
    }
    else {                              /* Only one point ( ?? ) - ignore it. */
        free((char *) delta_t);
        free((char *) d2x);  free((char *) d2y);
        if (contr_kind==CLOSED_CONTOUR) pc_tail->next = NULL; /* Un-circular list */
        return;
    }

    /* Calculate "num_intpol" interpolated values */
    num_intpol = 1 + (num_pts-1) * num_approx_pts;   /* global: num_approx_pts */
    intp_cubic_spline(num_pts, p_cntr, d2x, d2y, delta_t, num_intpol);

    free((char *) delta_t);
    free((char *) d2x);  free((char *) d2y);

    if (contr_kind==CLOSED_CONTOUR) pc_tail->next = NULL; /* Un-circular list */

    end_crnt_cntr();
}


/*
 * Find Bspline approximation for this data set.
 * Uses global variable num_approx_pts to determine number of samples per
 * interval, where the knot vector intervals are assumed to be uniform, and
 * Global variable bspline_order for the order of Bspline to use.
 */
static void put_contour_bspline(p_cntr, z_level, x_min, x_max, y_min, y_max,
                                                                contr_kind)
struct cntr_struct *p_cntr;
double z_level, x_min, x_max,  y_min, y_max;
int contr_kind;
{
    int num_pts, order = bspline_order;

    num_pts = count_contour(p_cntr);         /* Number of points in contour. */
    if (num_pts < 2) return;     /* Can't do nothing if empty or one points! */
    /* Order must be less than number of points in curve - fix it if needed. */
    if (order > num_pts - 1) order = num_pts - 1;

    gen_bspline_approx(p_cntr, num_pts, order, contr_kind);
    end_crnt_cntr();
}

/*
 * Free all elements in the contour list.
 */
static void free_contour(p_cntr)
struct cntr_struct *p_cntr;
{
    struct cntr_struct *pc_temp;

    while (p_cntr) {
        pc_temp = p_cntr;
        p_cntr = p_cntr -> next;
        free((char *) pc_temp);
    }
}

/*
 * Counts number of points in contour.
 */
static int count_contour(p_cntr)
struct cntr_struct *p_cntr;
{
    int count = 0;

    while (p_cntr) {
        count++;
        p_cntr = p_cntr -> next;
    }
    return count;
}

/*
 * Find second derivatives (x''(t_i),y''(t_i)) of cubic spline interpolation
 * through list of points (x_i,y_i). The parameter t is calculated as the
 * length of the linear stroke. The number of points must be at least 3.
 * Note: For CLOSED_CONTOURs the first and last point must be equal.
 */
static int gen_cubic_spline(num_pts, p_cntr, d2x, d2y, delta_t,
                                                  contr_kind, unit_x, unit_y)
int num_pts;                /* Number of points (num_pts>=3),          input  */
struct cntr_struct *p_cntr; /* List of points (x(t_i),y(t_i)),         input  */
double d2x[], d2y[];        /* Second derivatives (x''(t_i),y''(t_i)), output */
double delta_t[];           /* List of interval lengths t_{i+1}-t_{i}, output */
int contr_kind;             /* CLOSED_CONTOUR or OPEN_CONTOUR,         input  */
double unit_x, unit_y;      /* Unit length in x and y (norm=1),        input  */
{
    int n, i;
    double norm;
    tri_diag *m;                    /* The tri-diagonal matrix is saved here. */
    struct cntr_struct *pc_temp;

    m = (tri_diag *)
        alloc((unsigned long) (sizeof(tri_diag)*num_pts), "contour tridiag m");

    /*
     * Calculate first differences in (d2x[i], d2y[i]) and interval lengths
     * in delta_t[i]:
     */
    pc_temp = p_cntr;
    for (i=0; i<num_pts-1; i++) {
        d2x[i] = pc_temp->next->X - pc_temp->X;
        d2y[i] = pc_temp->next->Y - pc_temp->Y;
        /*
         * The Norm of a linear stroke is calculated in "normal coordinates"
         * and used as interval length:
         */
        delta_t[i] = sqrt( SQR(d2x[i]/unit_x) + SQR(d2y[i]/unit_y) );

        d2x[i] /= delta_t[i];            /* first difference, with unit norm: */
        d2y[i] /= delta_t[i];            /*   || (d2x[i], d2y[i]) || = 1      */

        pc_temp = pc_temp -> next;
    }

    /*
     * Setup linear System:  M * x = b
     */
    n = num_pts - 2;                          /* Without first and last point */
    if( contr_kind == CLOSED_CONTOUR ) {
        /* First and last points must be equal for CLOSED_CONTOURs */
        delta_t[num_pts-1] = delta_t[0];
        d2x[num_pts-1] = d2x[0];
        d2y[num_pts-1] = d2y[0];
        n++;                                /* Add last point (= first point) */
    }
    for (i=0; i<n; i++) {
        /* Matrix M, mainly tridiagonal with cyclic second index ("j = j+n mod n") */
        m[i][0] = delta_t[i];               /* Off-diagonal element M_{i,i-1} */
        m[i][1] = 2. * ( delta_t[i] + delta_t[i+1] );              /* M_{i,i} */
        m[i][2] = delta_t[i+1];             /* Off-diagonal element M_{i,i+1} */

        /* Right side b_x and b_y */
        d2x[i] = ( d2x[i+1] - d2x[i] ) * 6.;
        d2y[i] = ( d2y[i+1] - d2y[i] ) * 6.;

        /*
         * If the linear stroke shows a cusps of more than 90 degree, the right
         * side is reduced to avoid oscillations in the spline:
         */
        norm = sqrt( SQR(d2x[i]/unit_x) + SQR(d2y[i]/unit_y) ) / 8.5;

        if ( norm > 1.) {
            d2x[i] /= norm;
            d2y[i] /= norm;
            /* The first derivative will not be continuous */
        }
    }

    if (contr_kind != CLOSED_CONTOUR) {
        /* Third derivative is set to zero at both ends */
        m[0][1] += m[0][0];                                    /* M_{0,0}     */
        m[0][0] = 0.;                                          /* M_{0,n-1}   */
        m[n-1][1] += m[n-1][2];                                /* M_{n-1,n-1} */
        m[n-1][2] = 0.;                                        /* M_{n-1,0}   */
    }

    /* Solve linear systems for d2x[] and d2y[] */


    if ( solve_cubic_1(m, n) )            /* Calculate Cholesky decomposition */
    {
        solve_cubic_2(m, d2x, n);                      /* solve M * d2x = b_x */
        solve_cubic_2(m, d2y, n);                      /* solve M * d2y = b_y */

    }
    else {                        /* Should not happen, but who knows ... */
        free((char *) m);
        return FALSE;
    }

    /* Shift all second derivatives one place right and abdate end points */
    for (i=n; i>0; i--) {
        d2x[i] = d2x[i-1];
        d2y[i] = d2y[i-1];
     }
    if (contr_kind == CLOSED_CONTOUR) {
        d2x[0] = d2x[n];
        d2y[0] = d2y[n];
    }
    else {
        d2x[0] = d2x[1];                       /* Third derivative is zero in */
        d2y[0] = d2y[1];                       /*     first and last interval */
        d2x[n+1] = d2x[n];
        d2y[n+1] = d2y[n];
    }

    free((char *) m);
    return TRUE;
}

/*
 * Calculate interpolated values of the spline function (defined via p_cntr
 * and the second derivatives d2x[] and d2y[]). The number of tabulated
 * values is n. On an equidistant grid n_intpol values are calculated.
 */
static void intp_cubic_spline(n, p_cntr, d2x, d2y, delta_t, n_intpol)
struct cntr_struct *p_cntr;
double d2x[], d2y[], delta_t[];
int n, n_intpol;
{
    double t, t_skip, t_max;
    double x0, x1, x, y0, y1, y;
    double d, hx, dx0, dx01, hy, dy0, dy01;
    int i;

    /* The length of the total interval */
    t_max = 0.;
    for (i=0; i<n-1; i++) t_max += delta_t[i];

    /* The distance between interpolated points */
    t_skip = ( 1. - 1e-7 ) * t_max / ( n_intpol - 1 );

    t = 0.;                                                /* Parameter value */
    x1 = p_cntr -> X;
    y1 = p_cntr -> Y;
    add_cntr_point(x1, y1);                                   /* First point. */
    t += t_skip;

    for (i=0; i<n-1; i++) {
        p_cntr = p_cntr -> next;

        d = delta_t[i];                                    /* Interval length */
        x0 = x1;                             y0 = y1;
        x1 = p_cntr -> X;                    y1 = p_cntr -> Y;
        hx = ( x1 - x0 ) / d;                hy = ( y1 - y0 ) / d;
        dx0 = ( d2x[i+1] + 2*d2x[i] ) /6.;   dy0 = ( d2y[i+1] + 2*d2y[i] ) /6.;
        dx01 = (d2x[i+1] - d2x[i]) /(6.*d);  dy01 = (d2y[i+1] - d2y[i]) /(6.*d);
        while (t <= delta_t[i]) {                 /* t in current interval ? */
            x = x0 + t * ( hx + (t-d) * ( dx0 + t * dx01 ) );
            y = y0 + t * ( hy + (t-d) * ( dy0 + t * dy01 ) );
            add_cntr_point(x, y);                              /* next point. */
            t += t_skip;
        }
        t -= delta_t[i];    /* Parameter t relative to start of next interval */
    }
}

/*
 * The following two procedures solve the special linear system which arise
 * in cubic spline interpolation. If x is assumed cyclic ( x[i]=x[n+i] ) the
 * equations can be written as (i=0,1,...,n-1):
 *     m[i][0] * x[i-1] + m[i][1] * x[i] + m[i][2] * x[i+1] = b[i] .
 * In matrix notation one gets M * x = b, where the matrix M is tridiagonal
 * with additional elements in the upper right and lower left position:
 *   m[i][0] = M_{i,i-1}  for i=1,2,...,n-1    and    m[0][0] = M_{0,n-1} ,
 *   m[i][1] = M_{i, i }  for i=0,1,...,n-1
 *   m[i][2] = M_{i,i+1}  for i=0,1,...,n-2    and    m[n-1][2] = M_{n-1,0}.
 * M should be symmetric (m[i+1][0]=m[i][2]) and positiv definite.
 * The size of the system is given in n (n>=1).
 *
 * In the first procedure the Cholesky decomposition M = C^T * D * C
 * (C is upper triangle with unit diagonal, D is diagonal) is calculated.
 * Return TRUE if decomposition exist.
 */
static int solve_cubic_1 (m, n)
tri_diag m[];
int n;
{
    int i;
    double m_ij, m_n, m_nn, d;

    if (n<1) return FALSE;                  /* Dimension should be at least 1 */

    d = m[0][1];                                         /* D_{0,0} = M_{0,0} */
    if ( d <= 0. ) return FALSE;       /* M (or D) should be positiv definite */
    m_n = m[0][0];                                             /*  M_{0,n-1}  */
    m_nn = m[n-1][1];                                          /* M_{n-1,n-1} */
    for (i=0; i<n-2; i++)
      {
        m_ij = m[i][2];                                          /*  M_{i,1}  */
        m[i][2] = m_ij / d;                                      /* C_{i,i+1} */
        m[i][0] = m_n / d;                                       /* C_{i,n-1} */
        m_nn -= m[i][0] * m_n;                          /* to get C_{n-1,n-1} */
        m_n = -m[i][2] * m_n;                           /* to get C_{i+1,n-1} */
        d = m[i+1][1] - m[i][2] * m_ij;                        /* D_{i+1,i+1} */
        if ( d <= 0. ) return FALSE;       /* Elements of D should be positiv */
        m[i+1][1] = d;
      }
    if (n>=2) {                                       /* Complete last column */
        m_n += m[n-2][2];                                  /* add M_{n-2,n-1} */
        m[n-2][0] = m_n / d;                                   /* C_{n-2,n-1} */
        m[n-1][1] = d = m_nn - m[n-2][0] * m_n;                /* D_{n-1,n-1} */
        if ( d <= 0. ) return FALSE;
    }
    return TRUE;
}

/*
 * The second procedure solves the linear system, with the Choleky decomposition
 * calculated above (in m[][]) and the right side b given in x[]. The solution x
 * overwrites the right side in x[].
 */
static void solve_cubic_2 (m, x, n)
tri_diag m[];
double x[];
int n;
{
    int i;
    double x_n;

    /* Division by transpose of C : b = C^{-T} * b */
    x_n = x[n-1];
    for (i=0; i<n-2; i++)
      {
        x[i+1] -= m[i][2] * x[i];                        /* C_{i,i+1} * x_{i} */
        x_n -= m[i][0] * x[i];                           /* C_{i,n-1} * x_{i} */
      }
    if (n>=2) x[n-1] = x_n - m[n-2][0] * x[n-2];     /* C_{n-2,n-1} * x_{n-1} */

    /* Division by D: b = D^{-1} * b */
    for (i=0; i<n; i++) x[i] /= m[i][1];

    /* Division by C: b = C^{-1} * b */
    x_n = x[n-1];
    if (n>=2) x[n-2] -= m[n-2][0] * x_n;             /* C_{n-2,n-1} * x_{n-1} */
    for (i=n-3; i>=0; i--)
      {
        /*      C_{i,i+1} * x_{i+1} + C_{i,n-1} * x_{n-1} */
        x[i] -=  m[i][2]  * x[i+1]  +  m[i][0]  * x_n;
      }
    return;
}

/*
 * Solve tri diagonal linear system equation. The tri diagonal matrix is
 * defined via matrix M, right side is r, and solution X i.e. M * X = R.
 * Size of system given in n. Return TRUE if solution exist.
 */
/* not used any more in "contour.c", but in "spline.c" (21. Dec. 1995) ! */

int solve_tri_diag(m, r, x, n)
tri_diag m[];
double r[], x[];
int n;
{
    int i;
    double t;

    for (i=1; i<n; i++) {   /* Eliminate element m[i][i-1] (lower diagonal). */
        if (m[i-1][1] == 0) return FALSE;
        t = m[i][0] / m[i-1][1];        /* Find ratio between the two lines. */
/*	m[i][0] = m[i][0] - m[i-1][1] * t; */
/* m[i][0] is not used any more (and set to 0 in the above line) */
        m[i][1] = m[i][1] - m[i-1][2] * t;
        r[i] = r[i] - r[i-1] * t;
    }
    /* Now do back subtitution - update the solution vector X: */
    if (m[n-1][1] == 0) return FALSE;
    x[n-1] = r[n-1] / m[n-1][1];                       /* Find last element. */
    for (i=n-2; i>=0; i--) {
        if (m[i][1] == 0) return FALSE;
        x[i] = (r[i] - x[i+1] * m[i][2]) / m[i][1];
    }
    return TRUE;
}

/*
 * Generate a Bspline curve defined by all the points given in linked list p:
 * Algorithm: using deBoor algorithm
 * Note: if Curvekind is OPEN_CONTOUR than Open end knot vector is assumed,
 *       else (CLOSED_CONTOUR) Float end knot vector is assumed.
 * It is assumed that num_of_points is at least 2, and order of Bspline is less
 * than num_of_points!
 */
static void gen_bspline_approx(p_cntr, num_of_points, order, contr_kind)
struct cntr_struct *p_cntr;
int num_of_points, order, contr_kind;
{
    int i, knot_index = 0, pts_count = 1;
    double dt, t, next_t, t_min, t_max, x, y;
    struct cntr_struct *pc_temp = p_cntr, *pc_tail=NULL;

    /* If the contour is Closed one we must update few things:               */
    /* 1. Make the list temporary circular, so we can close the contour.     */
    /* 2. Update num_of_points - increase it by "order-1" so contour will be */
    /*    closed. This will evaluate order more sections to close it!        */
    if (contr_kind == CLOSED_CONTOUR) {
        pc_tail = p_cntr;
        while (pc_tail -> next) pc_tail = pc_tail -> next;/* Find last point.*/

        /* test if first and last point are equal */
        if ( fuzzy_equal(pc_tail -> X, p_cntr -> X)
            && fuzzy_equal(pc_tail -> Y, p_cntr -> Y) ) {

            pc_tail -> next = p_cntr -> next; /* Close contour list - make it circular.*/
            num_of_points += order-1;
        }
        else {
            pc_tail -> next = p_cntr;
            num_of_points += order;
        }
    }

    /* Find first (t_min) and last (t_max) t value to eval: */
    t = t_min = fetch_knot(contr_kind, num_of_points, order, order);
    t_max = fetch_knot(contr_kind, num_of_points, order, num_of_points);
    next_t = t_min + 1.0;
    knot_index = order;
    dt = 1.0/num_approx_pts;            /* Number of points per one section. */


    while (t<t_max) {
        if (t > next_t) {
            pc_temp = pc_temp -> next;     /* Next order ctrl. pt. to blend. */
            knot_index++;
            next_t += 1.0;
        }
        eval_bspline(t, pc_temp, num_of_points, order, knot_index,
                                        contr_kind, &x, &y);   /* Next pt. */
        add_cntr_point(x, y);
        pts_count++;
        /* As we might have some real number round off problems we must      */
        /* test if we dont produce too many points here...                   */
        if (pts_count + 1 == num_approx_pts * (num_of_points - order) + 1)
                break;
        t += dt;
    }

    eval_bspline(t_max - EPSILON, pc_temp, num_of_points, order, knot_index,
                contr_kind, &x, &y);
    /* If from round off errors we need more than one last point: */
    for (i=pts_count; i<num_approx_pts * (num_of_points - order) + 1; i++)
        add_cntr_point(x, y);                       /* Complete the contour. */

    if (contr_kind == CLOSED_CONTOUR)     /* Update list - un-circular it. */
        pc_tail -> next = NULL;
}

/*
 * The routine to evaluate the B-spline value at point t using knot vector
 * from function fetch_knot(), and the control points p_cntr.
 * Returns (x, y) of approximated B-spline. Note that p_cntr points on the first control
 * point to blend with. The B-spline is of order order.
 */
static void eval_bspline(t, p_cntr, num_of_points, order, j, contr_kind, x, y)
double t;
struct cntr_struct *p_cntr;
int num_of_points, order, j, contr_kind;
double *x, *y;
{
    int i, p;
    double ti, tikp, *dx, *dy;      /* Copy p_cntr into it to make it faster. */

    dx = (double *)
        alloc((unsigned long) (sizeof(double) * (order+j)), "contour b_spline");
    dy = (double *)
        alloc((unsigned long) (sizeof(double) * (order+j)), "contour b_spline");

    /* Set the dx/dy - [0] iteration step, control points (p==0 iterat.): */
    for (i=j-order; i<=j; i++) {
        dx[i] = p_cntr -> X;
        dy[i] = p_cntr -> Y;
        p_cntr = p_cntr -> next;
    }

    for (p=1; p<=order; p++) {        /* Iteration (b-spline level) counter. */
        for (i=j; i>=j-order+p; i--) {           /* Control points indexing. */
            ti = fetch_knot(contr_kind, num_of_points, order, i);
            tikp = fetch_knot(contr_kind, num_of_points, order, i+order+1-p);
            if (ti == tikp) {   /* Should not be a problems but how knows... */
            }
            else {
                dx[i] = dx[i] * (t - ti)/(tikp-ti) +         /* Calculate x. */
                        dx[i-1] * (tikp-t)/(tikp-ti);
                dy[i] = dy[i] * (t - ti)/(tikp-ti) +         /* Calculate y. */
                        dy[i-1] * (tikp-t)/(tikp-ti);
            }
        }
    }
    *x = dx[j]; *y = dy[j];
    free((char *) dx);
    free((char *) dy);
}

/*
 * Routine to get the i knot from uniform knot vector. The knot vector
 * might be float (Knot(i) = i) or open (where the first and last "order"
 * knots are equal). contr_kind determines knot kind - OPEN_CONTOUR means
 * open knot vector, and CLOSED_CONTOUR selects float knot vector.
 * Note the knot vector is not exist and this routine simulates it existance
 * Also note the indexes for the knot vector starts from 0.
 */
static double fetch_knot(contr_kind, num_of_points, order, i)
int contr_kind, num_of_points, order, i;
{
    switch (contr_kind) {
        case OPEN_CONTOUR:
            if (i <= order) return 0.0;
            else if (i <= num_of_points) return (double) (i - order);
                 else return (double) (num_of_points - order);
        case CLOSED_CONTOUR:
            return (double) i;
        default: /* Should never happen */
            return 1.0;
    }
#ifdef sequent
        return 1.0;
#endif
}
