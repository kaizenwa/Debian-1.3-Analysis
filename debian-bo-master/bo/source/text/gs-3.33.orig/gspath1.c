/* Copyright (C) 1989, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gspath1.c */
/* Additional PostScript Level 1 path routines for Ghostscript library */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzpath.h"
#include "gscoord.h"            /* gs_itransform prototype */

/* Path enumeration structure */
struct gs_path_enum_s {
	const segment *pseg;
	const gs_state *pgs;
	gx_path *copied_path;	/* a copy of the path, release when done */
	bool moveto_done;	/* have we reported a final moveto yet? */
};
gs_private_st_ptrs3(st_gs_path_enum, gs_path_enum, "gs_path_enum",
  path_enum_enum_ptrs, path_enum_reloc_ptrs, pseg, pgs, copied_path);

/* ------ Arcs ------ */

/* Conversion parameters */
#define degrees_to_radians (M_PI / 180.0)

/* Forward declarations */
/*
 * Because of an obscure bug in the IBM RS/6000 compiler, the bool argument
 * for arc_either and arc_add must come before the floatp arguments.
 */
private int arc_either(P7(gs_state *, bool,
  floatp, floatp, floatp, floatp, floatp));
private int arc_add(P9(gs_state *, bool,
  floatp, floatp, floatp, floatp, floatp, floatp, floatp));

int
gs_arc(gs_state *pgs,
  floatp xc, floatp yc, floatp r, floatp ang1, floatp ang2)
{	return arc_either(pgs, 0, xc, yc, r, ang1, ang2);
}

int
gs_arcn(gs_state *pgs,
  floatp xc, floatp yc, floatp r, floatp ang1, floatp ang2)
{	return arc_either(pgs, 1, xc, yc, r, ang1, ang2);
}

private int
arc_either(gs_state *pgs, int clockwise,
  floatp axc, floatp ayc, floatp arad, floatp aang1, floatp aang2)
{	float ar = arad;
	fixed ang1 = float2fixed(aang1), ang2 = float2fixed(aang2), adiff;
	float ang1r;
	float x0, y0, sin0, cos0;
	float x3r, y3r;
	int first = true;
	int code;
#define fixed_90 int2fixed(90)
#define fixed_180 int2fixed(180)
#define fixed_360 int2fixed(360)
	if ( ar < 0 )
	   {	ang1 += fixed_180;
		ang2 += fixed_180;
		ar = - ar;
	   }
	ang1r = fixed2float(ang1 % fixed_360) * degrees_to_radians;
	sin0 = ar * sin(ang1r), cos0 = ar * cos(ang1r);
	x0 = axc + cos0, y0 = ayc + sin0;
	if ( clockwise )
	   {	/* Quadrant reduction */
		while ( ang1 < ang2 ) ang2 -= fixed_360;
		while ( (adiff = ang2 - ang1) < -fixed_90 )
		   {	float w = sin0; sin0 = -cos0; cos0 = w;
			x3r = axc + cos0, y3r = ayc + sin0;
			code = arc_add(pgs, first, ar, x0, y0, x3r, y3r,
				(x0 + cos0),
				(y0 + sin0));
			if ( code < 0 ) return code;
			x0 = x3r, y0 = y3r;
			ang1 -= fixed_90;
			first = false;
		   }
	   }
	else
	   {	/* Quadrant reduction */
		while ( ang2 < ang1 ) ang2 += fixed_360;
		while ( (adiff = ang2 - ang1) > fixed_90 )
		   {	float w = cos0; cos0 = -sin0; sin0 = w;
			x3r = axc + cos0, y3r = ayc + sin0;
			code = arc_add(pgs, first, ar, x0, y0, x3r, y3r,
				(x0 + cos0),
				(y0 + sin0));
			if ( code < 0 ) return code;
			x0 = x3r, y0 = y3r;
			ang1 += fixed_90;
			first = false;
		   }
	   }
	/* Compute the intersection of the tangents. */
	/* We define xt and yt as separate variables to work around */
	/* a floating point bug in one of the SPARC compilers. */
	/* We know that -fixed_90 <= adiff <= fixed_90. */
	   {	double trad =
		  tan(fixed2float(adiff) * (degrees_to_radians / 2));
		double ang2r = fixed2float(ang2) * degrees_to_radians;
		double xt = x0 - trad * sin0, yt = y0 + trad * cos0;
		code = arc_add(pgs, first, ar, x0, y0,
			       (axc + ar * cos(ang2r)),
			       (ayc + ar * sin(ang2r)),
			       xt, yt);
	   }
	return code;
}

int
gs_arcto(gs_state *pgs,
  floatp ax1, floatp ay1, floatp ax2, floatp ay2, floatp arad,
  float *retxy)			/* float retxy[4] */
{	float xt0, yt0, xt2, yt2;
	gs_point up0;
#define ax0 up0.x
#define ay0 up0.y
	int code;
	if ( arad < 0 )
		return_error(gs_error_undefinedresult);
	/* Transform the current point back into user coordinates */
	if ( (code = gs_currentpoint(pgs, &up0)) < 0 ) return code;
	   {	/* Now we have to compute the tangent points. */
		/* Basically, the idea is to compute the tangent */
		/* of the bisector by using tan(x+y) and tan(z/2) */
		/* formulas, without ever using any trig. */
		float dx0 = ax0 - ax1, dy0 = ay0 - ay1;
		float dx2 = ax2 - ax1, dy2 = ay2 - ay1;
		/* Compute the squared lengths from p1 to p0 and p2. */
		double sql0 = dx0 * dx0 + dy0 * dy0;
		double sql2 = dx2 * dx2 + dy2 * dy2;
		/* Compute the distance from p1 to the tangent points. */
		/* This is the only hairy part. */
		double num = dy0 * dx2 - dy2 * dx0;
		double denom = sqrt(sql0 * sql2) - (dx0 * dx2 + dy0 * dy2);
		/* Check for collinear points. */
		if ( fabs(num) < 1.0e-6 || fabs(denom) < 1.0e-6 )
		   {	gs_fixed_point pt;
			code = gs_point_transform2fixed(&pgs->ctm, ax1, ay1, &pt);
			if ( code >= 0 ) code = gx_path_add_line(pgs->path, pt.x, pt.y);
			xt0 = xt2 = ax1;
			yt0 = yt2 = ay1;
		   }
		else		/* not collinear */
		   {	double dist = fabs(arad * num / denom);
			double l0 = dist / sqrt(sql0), l2 = dist / sqrt(sql2);
			xt0 = ax1 + dx0 * l0;
			yt0 = ay1 + dy0 * l0;
			xt2 = ax1 + dx2 * l2;
			yt2 = ay1 + dy2 * l2;
			code = arc_add(pgs, true, arad, xt0, yt0, xt2, yt2, ax1, ay1);
		   }
	   }
	if ( retxy != 0 )
	   {	retxy[0] = xt0;
		retxy[1] = yt0;
		retxy[2] = xt2;
		retxy[3] = yt2;
	   }
	return code;
}

/* Internal routine for adding an arc to the path. */
private int
arc_add(gs_state *pgs, bool first,
  floatp r, floatp x0, floatp y0, floatp x3, floatp y3, floatp xt, floatp yt)
{	gx_path *path = pgs->path;
	floatp dx = xt - x0, dy = yt - y0;
	double dist = dx * dx + dy * dy;
	double r2 = r * r;
	floatp fraction;
	gs_fixed_point p0, p3, pt, cpt;
	int code;
	/* Compute the fraction coefficient for the curve. */
	/* See gx_path_add_arc for details. */
	if ( dist >= r2 * 1.0e8 )	/* almost zero radius; */
				/* the >= catches dist == r == 0 */
	  fraction = 0.0;
	else
	  fraction = (4.0/3.0) / (1 + sqrt(1 + dist / r2));
	if_debug8('r',
		  "[r]Arc f=%f p0=(%f,%f) pt=(%f,%f) p3=(%f,%f) first=%d\n",
		  fraction, x0, y0, xt, yt, x3, y3, (int)first);
	if (	(code = gs_point_transform2fixed(&pgs->ctm, x0, y0, &p0)) < 0 ||
		(code = gs_point_transform2fixed(&pgs->ctm, x3, y3, &p3)) < 0 ||
		(code = gs_point_transform2fixed(&pgs->ctm, xt, yt, &pt)) < 0 ||
		(first && (code = (gx_path_current_point(path, &cpt) >= 0 ?
			 gx_path_add_line(path, p0.x, p0.y) :
			 gx_path_add_point(path, p0.x, p0.y))) < 0)
	   )
		return code;
	return gx_path_add_arc(path, p0.x, p0.y, p3.x, p3.y, pt.x, pt.y, fraction);
}

/* ------ Path transformers ------ */

int
gs_flattenpath(gs_state *pgs)
{	gx_path fpath;
	int code;
	if ( !pgs->path->curve_count ) return 0;	/* no curves */
	code = gx_path_flatten(pgs->path, &fpath, pgs->flatness, 0);
	if ( code < 0 ) return code;
	gx_path_release(pgs->path);
	*pgs->path = fpath;
	return 0;
}

int
gs_reversepath(gs_state *pgs)
{	gx_path rpath;
	int code = gx_path_copy_reversed(pgs->path, &rpath, 1);
	if ( code < 0 ) return code;
	gx_path_release(pgs->path);
	*pgs->path = rpath;
	return 0;
}

/* ------ Accessors ------ */

int
gs_pathbbox(gs_state *pgs, gs_rect *pbox)
{	gs_fixed_rect fbox;		/* box in device coordinates */
	gs_rect dbox;
	int code = gx_path_bbox(pgs->path, &fbox);
	if ( code < 0 ) return code;
	/* Transform the result back to user coordinates. */
	dbox.p.x = fixed2float(fbox.p.x);
	dbox.p.y = fixed2float(fbox.p.y);
	dbox.q.x = fixed2float(fbox.q.x);
	dbox.q.y = fixed2float(fbox.q.y);
	return gs_bbox_transform_inverse(&dbox, &ctm_only(pgs), pbox);
}

/* ------ Enumerators ------ */

/* Allocate a path enumerator. */
gs_path_enum *
gs_path_enum_alloc(gs_memory_t *mem, client_name_t cname)
{	return gs_alloc_struct(mem, gs_path_enum, &st_gs_path_enum, cname);
}

/* Start enumerating a path */
int
gs_path_enum_init(gs_path_enum *penum, const gs_state *pgs)
{	gx_path *ppath = pgs->path;
	int code;
	penum->copied_path = gs_alloc_struct(pgs->memory, gx_path, &st_path,
					     "gs_path_enum_init");
	if ( penum->copied_path == 0 )
		return_error(gs_error_VMerror);
	code = gx_path_copy(ppath, penum->copied_path, 1);
	if ( code < 0 )
	{	gs_free_object(pgs->memory, penum->copied_path,
			       "gs_path_enum_init");
		return code;
	}
	penum->pgs = pgs;
	penum->pseg = (const segment *)penum->copied_path->first_subpath;
	penum->moveto_done = false;
	return 0;
}

/* Enumerate the next element of a path. */
/* If the path is finished, return 0; */
/* otherwise, return the element type. */
int
gs_path_enum_next(gs_path_enum *penum, gs_point ppts[3])
{	const segment *pseg = penum->pseg;
	const gs_state *pgs = penum->pgs;
	gs_point pt;
	int code;
	if ( pseg == 0 )
	{	/* We've enumerated all the segments, but there might be */
		/* a trailing moveto. */
		const gx_path *ppath = pgs->path;
		if ( ppath->subpath_open < 0 && !penum->moveto_done )
		{	/* Handle a trailing moveto */
			penum->moveto_done = true;
			if ( (code = gs_itransform((gs_state *)pgs,
					fixed2float(ppath->position.x),
					fixed2float(ppath->position.y),
					&ppts[0])) < 0
			   )
				return code;
			return gs_pe_moveto;
		}
		return 0;
	}
	penum->pseg = pseg->next;
	if ( pseg->type == s_line_close )
	  return gs_pe_closepath;
	if ( (code = gs_itransform((gs_state *)pgs, fixed2float(pseg->pt.x),
				   fixed2float(pseg->pt.y), &pt)) < 0 )
	  return code;
	switch ( pseg->type )
	   {
	case s_start:
	     ppts[0] = pt;
	     return gs_pe_moveto;
	case s_line:
	     ppts[0] = pt;
	     return gs_pe_lineto;
	case s_curve:
#define pcurve ((const curve_segment *)pseg)
	     if ( (code =
		   gs_itransform((gs_state *)pgs, fixed2float(pcurve->p1.x),
				 fixed2float(pcurve->p1.y), &ppts[0])) < 0 ||
		  (code =
		   gs_itransform((gs_state *)pgs, fixed2float(pcurve->p2.x),
				 fixed2float(pcurve->p2.y), &ppts[1])) < 0 )
	       return 0;
	     ppts[2] = pt;
	     return gs_pe_curveto;
#undef pcurve
	default:
	     lprintf1("bad type %x in gs_path_enum_next!\n", pseg->type);
	     return_error(gs_error_Fatal);
	   }
}

/* Clean up after a pathforall. */
void
gs_path_enum_cleanup(gs_path_enum *penum)
{	if ( penum->copied_path != 0 )		/* don't do it twice ... */
						/* shouldn't be needed! */
	{	gx_path_release(penum->copied_path);
		gs_free_object(penum->pgs->memory, penum->copied_path,
			       "gs_path_enum_cleanup");
		penum->copied_path = 0;
	}
}
