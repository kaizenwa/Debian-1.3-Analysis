/* Copyright (C) 1992, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gscie.c */
/* CIE color rendering for Ghostscript */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gscspace.h"
#include "gsmatrix.h"			/* for gscolor2.h */
#include "gscolor2.h"			/* for gs_set/currentcolorrendering */
#include "gscie.h"
#include "gxarith.h"
#include "gxdevice.h"			/* for gxcmap.h */
#include "gxcmap.h"
#include "gzstate.h"

/* Forward references */
private void cie_joint_caches_init(P3(gx_cie_joint_caches *,
  const gs_cie_common *, const gs_cie_render *));
private void cie_joint_caches_complete(P2(gx_cie_joint_caches *,
  const gs_cie_render *));
private void near cie_mult3(P3(const gs_vector3 *, const gs_matrix3 *,
  gs_vector3 *));
#define cie_vmult3(pv, pmat)\
  if ( !(pmat)->is_identity ) cie_mult3(pv, pmat, pv)
private void near cie_matrix_mult3(P3(const gs_matrix3 *, const gs_matrix3 *,
  gs_matrix3 *));
private void near cie_invert3(P2(const gs_matrix3 *, gs_matrix3 *));
private void near cie_lookup3(P2(gs_vector3 *, const gx_cie_cache *));
#define cie_vlookup3(pv, pcache)\
  if ( !((pcache)[0].is_identity & (pcache)[1].is_identity & (pcache)[2].is_identity) )\
    cie_lookup3(pv, pcache)
private void near cie_matrix_init(P1(gs_matrix3 *));

#define restrict_index(v, itemp)\
  ((uint)(itemp = (int)(v)) > gx_cie_cache_size ?\
   (itemp < 0 ? 0 : gx_cie_cache_size - 1) : itemp)
#define lookup(vin, pcache, valt, itemp)\
  ((pcache)->values.valt[restrict_index(((vin) - (pcache)->base) *\
					(pcache)->factor, itemp)])
#define vlookup(v, pcache, itemp)\
  if ( !(pcache)->is_identity ) v = lookup(v, pcache, floats, itemp)

#define if_restrict(v, range)\
  if ( (v) < (range).rmin ) v = (range).rmin;\
  else if ( (v) > (range).rmax ) v = (range).rmax

/* Define the template for loading a cache. */
/* If we had parameterized types, or a more flexible type system, */
/* this could be done with a single procedure. */
private void cie_cache_init3(P3(gx_cie_cache * /*[3]*/,
  gs_for_loop_params * /*[3]*/, const gs_range3 *));
#define CIE_LOAD_CACHE3_BODY(pcache, domains, rprocs, pcie)\
{	int i, j;\
	gs_for_loop_params lp[3];\
	cie_cache_init3(pcache, lp, domains);\
	for ( i = 0; i < gx_cie_cache_size; i++ )\
	  for ( j = 0; j < 3; lp[j].init += lp[j].step, j++ )\
	    pcache[j].values.floats[i] =\
	      (*(rprocs)->procs[j])(lp[j].init, pcie);\
}

/* ================ Color space definition ================ */

/* Allocator structure types */
private_st_joint_caches();
private_st_const_string();
public_st_const_string_element();
#define sptr ((gs_const_string *)vptr)
private ENUM_PTRS_BEGIN(const_string_enum_ptrs) return 0;
	case 0: *pep = (void *)sptr; return ptr_const_string_type;
ENUM_PTRS_END
private RELOC_PTRS_BEGIN(const_string_reloc_ptrs) {
	gs_reloc_const_string(sptr, gcst);
} RELOC_PTRS_END
#undef sptr

/* GC procedures for CIE color spaces */

#define pcs ((gs_color_space *)vptr)

private ENUM_PTRS_BEGIN(gx_enum_ptrs_CIEABC) return 0;
	ENUM_PTR(0, gs_color_space, params.abc);
ENUM_PTRS_END
private RELOC_PTRS_BEGIN(gx_reloc_ptrs_CIEABC) {
	RELOC_PTR(gs_color_space, params.abc);
} RELOC_PTRS_END

private ENUM_PTRS_BEGIN(gx_enum_ptrs_CIEA) return 0;
	ENUM_PTR(0, gs_color_space, params.a);
ENUM_PTRS_END
private RELOC_PTRS_BEGIN(gx_reloc_ptrs_CIEA) {
	RELOC_PTR(gs_color_space, params.a);
} RELOC_PTRS_END

#undef pcs

/* Define the CIE color space types. */
/* We use CIEA[BC] rather than CIEBasedA[BC] in some places because */
/* gcc under VMS only retains 23 characters of procedure names. */
private cs_proc_concrete_space(gx_concrete_space_CIEBased);
private cs_proc_remap_color(gx_remap_CIEABC);
cs_declare_procs(private, gx_concretize_CIEABC, gx_install_CIEABC,
  gx_adjust_cspace_CIEABC,
  gx_enum_ptrs_CIEABC, gx_reloc_ptrs_CIEABC);
cs_declare_procs(private, gx_concretize_CIEA, gx_install_CIEA,
  gx_adjust_cspace_CIEA,
  gx_enum_ptrs_CIEA, gx_reloc_ptrs_CIEA);
const gs_color_space_type
	gs_color_space_type_CIEBasedABC =
	 { gs_color_space_index_CIEBasedABC, 3, true,
	   gx_init_paint_3, gx_concrete_space_CIEBased,
	   gx_concretize_CIEABC, NULL,
	   gx_remap_CIEABC, gx_install_CIEABC,
	   gx_adjust_cspace_CIEABC, gx_no_adjust_color_count,
	   gx_enum_ptrs_CIEABC, gx_reloc_ptrs_CIEABC
	 },
	gs_color_space_type_CIEBasedA =
	 { gs_color_space_index_CIEBasedA, 1, true,
	   gx_init_paint_1, gx_concrete_space_CIEBased,
	   gx_concretize_CIEA, NULL,
	   gx_default_remap_color, gx_install_CIEA,
	   gx_adjust_cspace_CIEA, gx_no_adjust_color_count,
	   gx_enum_ptrs_CIEA, gx_reloc_ptrs_CIEA
	 };

/* ------ Default values for CIE dictionary elements ------ */

/* Default transformation procedures. */

private float
a_identity(floatp in, const gs_cie_a *pcie)
{	return in;
}
private float
abc_identity(floatp in, const gs_cie_abc *pcie)
{	return in;
}
private float
common_identity(floatp in, const gs_cie_common *pcie)
{	return in;
}
private float
render_identity(floatp in, const gs_cie_render *pcie)
{	return in;
}
private float
tpqr_identity(floatp in, const gs_cie_wbsd *pwbsd, const gs_cie_render *pcie)
{	return in;
}
private frac
render_table_identity(byte in, const gs_cie_render *pcie)
{	return byte2frac(in);
}

/* Default vectors and matrices. */

const gs_range3 Range3_default = { {0,1}, {0,1}, {0,1}, 1 /*true*/ };
const gs_cie_abc_proc3 DecodeABC_default =
 { abc_identity, abc_identity, abc_identity };
const gs_cie_common_proc3 DecodeLMN_default =
 { common_identity, common_identity, common_identity };
const gs_matrix3 Matrix3_default = { {1,0,0}, {0,1,0}, {0,0,1}, 1 /*true*/ };
const gs_range RangeA_default = {0,1};
const gs_cie_a_proc DecodeA_default = a_identity;
const gs_vector3 MatrixA_default = { 1, 1, 1 };
const gs_vector3 BlackPoint_default = { 0, 0, 0 };
const gs_cie_render_proc3 Encode_default =
 { render_identity, render_identity, render_identity };
const gs_cie_transform_proc3 TransformPQR_default =
 { tpqr_identity, tpqr_identity, tpqr_identity };
const gs_cie_render_table_procs RenderTableT_default =
 { render_table_identity, render_table_identity, render_table_identity,
   render_table_identity
 };

/* ------ Adjust reference counts for a CIE color space ------ */

private void
gx_adjust_cspace_CIEABC(const gs_color_space *pcs, gs_state *pgs, int delta)
{	rc_adjust_const(pcs->params.abc, delta, pgs->memory,
			"gx_adjust_cspace_CIEABC");
}

private void
gx_adjust_cspace_CIEA(const gs_color_space *pcs, gs_state *pgs, int delta)
{	rc_adjust_const(pcs->params.a, delta, pgs->memory,
			"gx_adjust_cspace_CIEA");
}

/* ------ Install a CIE color space ------ */

private int cie_load_common_cache(P3(gs_cie_common *, gs_state *,
  client_name_t));

private int
gx_install_CIEABC(gs_color_space *pcs, gs_state *pgs)
{	gs_cie_abc *pcie = pcs->params.abc;
	cie_matrix_init(&pcie->MatrixABC);
	CIE_LOAD_CACHE3_BODY(pcie->caches.DecodeABC, &pcie->RangeABC,
			     &pcie->DecodeABC, pcie);
	return cie_load_common_cache(&pcie->common, pgs, "gx_install_CIEABC");
}

private int
gx_install_CIEA(gs_color_space *pcs, gs_state *pgs)
{	gs_cie_a *pcie = pcs->params.a;
	int i;
	gs_for_loop_params lp;
	float in;
	gs_cie_cache_init(&pcie->caches.DecodeA, &lp, &pcie->RangeA);
	for ( i = 0, in = lp.init; i < gx_cie_cache_size; in += lp.step, i++ )
	  pcie->caches.DecodeA.values.floats[i] =
	    (*pcie->DecodeA)(in, pcie);
	return cie_load_common_cache(&pcie->common, pgs, "gx_install_CIEA");
}

/* Load the common caches when installing the color space. */
private int
cie_load_common_cache(gs_cie_common *pcie, gs_state *pgs, client_name_t cname)
{	cie_matrix_init(&pcie->MatrixLMN);
	CIE_LOAD_CACHE3_BODY(pcie->caches.DecodeLMN, &pcie->RangeLMN,
			     &pcie->DecodeLMN, pcie)
	if ( pgs->cie_render == 0 )
	  return 0;
	rc_unshare_struct(pgs->cie_joint_caches, gx_cie_joint_caches,
			  &st_joint_caches, pgs->memory,
			  return_error(gs_error_VMerror), cname);
	cie_joint_caches_init(pgs->cie_joint_caches, pcie, pgs->cie_render);
	cie_joint_caches_complete(pgs->cie_joint_caches, pgs->cie_render);
	return 0;
}

/* ================ Table setup ================ */

/* setcolorrendering */
private void cie_load_render_cache3(P4(gx_cie_cache * /*[3]*/,
  const gs_range3 *, const gs_cie_render_proc3 *, const gs_cie_render *));
int
gs_setcolorrendering(gs_state *pgs, gs_cie_render *pcie)
{	int code = gs_cie_render_init(pcie);
	if ( code < 0 )
	  return code;
	rc_assign(pgs->cie_render, pcie, pgs->memory,
		  "gs_setcolorrendering");
	/* Load the caches. */
	cie_load_render_cache3(pcie->caches.EncodeLMN, &pcie->DomainLMN,
			       &pcie->EncodeLMN, pcie);
	cie_load_render_cache3(pcie->caches.EncodeABC, &pcie->DomainABC,
			       &pcie->EncodeABC, pcie);
	if ( pcie->RenderTable.table != 0 )
	  { int i, j, m = pcie->RenderTable.m;
	    gs_for_loop_params flp;
	    for ( j = 0; j < m; j++ )
	      gs_cie_cache_init(&pcie->caches.RenderTableT[j], &flp,
				&Range3_default.u);
	    /****** ASSUMES gx_cie_cache_size >= 256 ******/
	    for ( i = 0; i < 256; i++ )
	      for ( j = 0; j < m; j++ )
		pcie->caches.RenderTableT[j].values.fracs[i] =
		  (*pcie->RenderTable.T.procs[j])((byte)i, pcie);
	  }
	code = gs_cie_render_complete(pcie);
	if ( code < 0 )
	  return code;
	/* Initialize the joint caches if needed. */
	{ const gs_color_space *pcs = pgs->color_space;
	  const gs_cie_common *common;
	  switch ( pcs->type->index )
	    {
	    case gs_color_space_index_CIEBasedABC:
	      common = &pcs->params.abc->common;
	      goto map;
	    case gs_color_space_index_CIEBasedA:
	      common = &pcs->params.a->common;
map:	      cie_joint_caches_init(pgs->cie_joint_caches, common, pcie);
	      cie_joint_caches_complete(pgs->cie_joint_caches, pcie);
	      break;
	    default:
	      ;
	    }
	}
	gx_unset_dev_color(pgs);
	return code;
}
/* Load one rendering cache. */
private void
cie_load_render_cache3(gx_cie_cache *pcache /*[3]*/, const gs_range3 *domain,
  const gs_cie_render_proc3 *rprocs, const gs_cie_render *pcie)
{	CIE_LOAD_CACHE3_BODY(pcache, domain, rprocs, pcie)
}

/* currentcolorrendering */
const gs_cie_render *
gs_currentcolorrendering(const gs_state *pgs)
{	return pgs->cie_render;
}

/* Get the joint caches, to avoid having to import gzstate.h */
gx_cie_joint_caches *
gx_currentciecaches(gs_state *pgs)
{	return pgs->cie_joint_caches;
}

/* ------ Compute the parameters for loading a cache. ------ */
/* Sets base and factor. */

void
gs_cie_cache_init(gx_cie_cache *pcache, gs_for_loop_params *pflp,
  const gs_range *domain)
{	/*
	 * We need to map the values in the range
	 * [domain->rmin..domain->rmax].  However, if neither rmin
	 * nor rmax is zero and the function is non-linear,
	 * this can lead to anomalies at zero, which is the
	 * default value for CIE colors.  The "correct" way to
	 * approach this is to run the mapping functions on demand;
	 * since we don't want to deal with the complexities of the
	 * callbacks this would involve (especially in the middle of
	 * rendering images), we adjust the range so that zero
	 * maps precisely to a cache slot.  Define:
	 *	a = domain->rmin;
	 *	b = domain->rmax;
	 *	R = b - a;
	 *	N = gx_cie_cache_size - 1;
	 *	f(v) = N(v-a)/R;
	 *	x = f(0).
	 * If x is not an integer, we can either increase b or
	 * decrease a to make it one.  In the former case, compute:
	 *	Kb = floor(x); R'b = N(0-a)/Kb; b' = a + R'b.
	 * In the latter case, compute:
	 *	Ka = ceiling(x-N); R'a = N(0-b)/Ka; a' = b - R'a.
	 * We choose whichever method stretches the range the least,
	 * i.e., the one whose R' value (R'a or R'b) is smaller.
	 */
	double a = domain->rmin, b = domain->rmax;
	double R = b - a;
#define N (gx_cie_cache_size - 1)
	double delta;
	/* Adjust the range if necessary. */
	if ( a < 0 && b >= 0 )
	  {	double x = -N * a / R;	/* must be > 0 */
		double Kb = floor(x);		/* must be >= 0 */
		double Ka = ceil(x) - N;	/* must be <= 0 */
		if ( Kb == 0 || (Ka != 0 && -b / Ka < -a / Kb) )	/* use R'a */
		  R = -N * b / Ka, a = b - R;
		else			/* use R'b */
		  R = -N * a / Kb, b = a + R;
	  }
	delta = R / N;
	pcache->base = a - delta / 2;	/* so lookup will round */
	pcache->factor = (delta == 0 ? 0 : N / R);
	if_debug3('c', "[c]cache 0x%lx base=%g, factor=%g\n",
		  (ulong)pcache, pcache->base, pcache->factor);
	pflp->init = a;
	pflp->step = delta;
	pflp->limit = b + delta / 2;
}
/* Compute parameters for 3 caches. */
private void
cie_cache_init3(gx_cie_cache *pcache /*[3]*/, gs_for_loop_params *plp /*[3]*/,
  const gs_range3 *domains)
{	gs_cie_cache_init(pcache, &plp[0], &domains->u);
	gs_cie_cache_init(pcache + 1, &plp[1], &domains->v);
	gs_cie_cache_init(pcache + 2, &plp[2], &domains->w);
}

/* ------ Complete a rendering structure ------ */

/* Compute values derived from the rendering structure parameters */
/* other than the cached procedure values.  This routine is idempotent. */
private void near cie_transform_range3(P3(const gs_range3 *,
  const gs_matrix3 *, gs_range3 *));
int
gs_cie_render_init(gs_cie_render *pcie)
{	gs_matrix3 PQR_inverse;
	cie_matrix_init(&pcie->MatrixLMN);
	cie_matrix_init(&pcie->MatrixABC);
	cie_matrix_init(&pcie->MatrixPQR);
	cie_invert3(&pcie->MatrixPQR, &PQR_inverse);
	cie_matrix_mult3(&PQR_inverse, &pcie->MatrixLMN,
			 &pcie->MatrixPQR_inverse_LMN);
	cie_transform_range3(&pcie->RangePQR, &pcie->MatrixPQR_inverse_LMN,
			     &pcie->DomainLMN);
	cie_transform_range3(&pcie->RangeLMN, &pcie->MatrixABC,
			     &pcie->DomainABC);
	cie_mult3(&pcie->points.WhitePoint, &pcie->MatrixPQR, &pcie->wdpqr);
	cie_mult3(&pcie->points.BlackPoint, &pcie->MatrixPQR, &pcie->bdpqr);
	return 0;
}

/* Transform a set of ranges. */
private void near
cie_transform_range(const gs_range3 *in, const gs_vector3 *col,
  gs_range *out)
{	float umin = col->u * in->u.rmin, umax = col->u * in->u.rmax;
	float vmin = col->v * in->v.rmin, vmax = col->v * in->v.rmax;
	float wmin = col->w * in->w.rmin, wmax = col->w * in->w.rmax;
	float temp;
#define swap(x, y) temp = x, x = y, y = temp
	if ( umin > umax ) swap(umin, umax);
	if ( vmin > vmax ) swap(vmin, vmax);
	if ( wmin > wmax ) swap(wmin, wmax);
	out->rmin = umin + vmin + wmin;
	out->rmax = umax + vmax + wmax;
#undef swap
}
private void near
cie_transform_range3(const gs_range3 *in, const gs_matrix3 *mat,
  gs_range3 *out)
{	cie_transform_range(in, &mat->cu, &out->u);
	cie_transform_range(in, &mat->cv, &out->v);
	cie_transform_range(in, &mat->cw, &out->w);
}

/* Complete the loading of the rendering caches. */
/* Note that this routine may make non-idempotent changes to */
/* the values in the caches. */
private void near cie_cache_restrict(P2(gx_cie_cache *, const gs_range *));
private void near cie_cache_restrict3(P2(gx_cie_cache * /*[3]*/,
  const gs_range3 *));
private void near cie_cache_to_fracs(P1(gx_cie_cache *));
int
gs_cie_render_complete(gs_cie_render *pcie)
{	/* Since range restriction happens immediately after */
	/* the cache lookup, we can save a step by restricting */
	/* the values in the cache entries. */
	cie_cache_restrict3(pcie->caches.EncodeLMN, &pcie->RangeLMN);
	cie_cache_restrict3(pcie->caches.EncodeABC, &pcie->RangeABC);
	/* If there is no lookup table, we want the final ABC values */
	/* to be fracs. */
	pcie->MatrixABCEncode = pcie->MatrixABC;
	if ( pcie->RenderTable.table == 0 )
	  {	int c;
		double f;
		for ( c = 0; c < 3; c++ )
		  {	cie_cache_restrict(&pcie->caches.EncodeABC[c],
					   &Range3_default.u);
			pcie->caches.EncodeABC[c].is_identity = false;
			cie_cache_to_fracs(&pcie->caches.EncodeABC[c]);
		      }
		/* Fold the scaling of the EncodeABC cache index */
		/* into MatrixABC. */
#define mabc(i, t)\
  f = pcie->caches.EncodeABC[i].factor;\
  pcie->MatrixABCEncode.cu.t *= f;\
  pcie->MatrixABCEncode.cv.t *= f;\
  pcie->MatrixABCEncode.cw.t *= f;\
  pcie->EncodeABC_base[i] = pcie->caches.EncodeABC[i].base * f
		mabc(0, u);
		mabc(1, v);
		mabc(2, w);
	  }
#undef mabc
	return 0;
}

/* Apply a range restriction to one cache or 3 caches. */
private void near
cie_cache_restrict(gx_cie_cache *pcache, const gs_range *prange)
{	int i;
	for ( i = 0; i < gx_cie_cache_size; i++ )
	  if_restrict(pcache->values.floats[i], *prange);
}
private void near
cie_cache_restrict3(gx_cie_cache *pcache3 /*[3]*/, const gs_range3 *pr3)
{	cie_cache_restrict(&pcache3[0], &pr3->u);
	cie_cache_restrict(&pcache3[1], &pr3->v);
	cie_cache_restrict(&pcache3[2], &pr3->w);
}

/* Convert a cache from floats to fracs. */
private void near
cie_cache_to_fracs(gx_cie_cache *pcache)
{	int i;
	for ( i = 0; i < gx_cie_cache_size; i++ )
	  pcache->values.fracs[i] = float2frac(pcache->values.floats[i]);
}

/* ------ Fill in the joint cache ------ */

/* Compute values derived from the color space and rendering parameters */
/* other than the cached PQR procedure values.  This routine is idempotent. */
private void
cie_joint_caches_init(gx_cie_joint_caches *pjc,
  const gs_cie_common *pcie, const gs_cie_render *pcier)
{	pjc->points_sd.ws.xyz = pcie->points.WhitePoint;
	cie_mult3(&pjc->points_sd.ws.xyz, &pcier->MatrixPQR,
		  &pjc->points_sd.ws.pqr);
	pjc->points_sd.bs.xyz = pcie->points.BlackPoint;
	cie_mult3(&pjc->points_sd.bs.xyz, &pcier->MatrixPQR,
		  &pjc->points_sd.bs.pqr);
	pjc->points_sd.wd.xyz = pcier->points.WhitePoint;
	pjc->points_sd.wd.pqr = pcier->wdpqr;
	pjc->points_sd.bd.xyz = pcier->points.BlackPoint;
	pjc->points_sd.bd.pqr = pcier->bdpqr;
	cie_matrix_mult3(&pcie->MatrixLMN, &pcier->MatrixPQR,
			 &pjc->MatrixLMN_PQR);
	/* Load the TransformPQR caches. */
	{ int i, j;
	  gs_for_loop_params lp[3];
	  cie_cache_init3(pjc->TransformPQR, lp, &pcier->RangePQR);
	  for ( i = 0; i < gx_cie_cache_size; i++ )
	    for ( j = 0; j < 3; lp[j].init += lp[j].step, j++ )
	      pjc->TransformPQR[j].values.floats[i] =
		(*pcier->TransformPQR.procs[j])(lp[j].init, &pjc->points_sd, pcier);
	}
}

/* Complete the loading of the joint caches.  Note that this routine */
/* may make non-idempotent changes to the values in the caches. */
private void
cie_joint_caches_complete(gx_cie_joint_caches *pjc, const gs_cie_render *pcier)
{	cie_cache_restrict3(pjc->TransformPQR, &pcier->RangePQR);
	/* If the PQR and LMN matrices are both identities, */
	/* we can combine the TransformPQR and EncodeLMN steps. */
	if ( pcier != 0 && pcier->MatrixPQR_inverse_LMN.is_identity )
	  {	int c, i;
		for ( c = 0; c < 3; c++ )
		  for ( i = 0; i < gx_cie_cache_size; i++ )
		    { float v = pjc->TransformPQR[c].values.floats[i];
		      int itemp;
		      pjc->TransformPQR[c].values.floats[i] = 
			lookup(v, &pcier->caches.EncodeLMN[c], floats, itemp);
		    }
	  }
}

/* ================ Color rendering (using the caches) ================ */

private int near cie_remap_finish(P4(const gs_vector3 *,
  const gs_cie_common *, frac *, const gs_state *));

/* Determine the concrete space underlying a CIEBased space. */
private const gs_color_space cie_rgb_space =
	{ &gs_color_space_type_DeviceRGB };
private const gs_color_space cie_cmyk_space =
	{ &gs_color_space_type_DeviceCMYK };
private const gs_color_space *
gx_concrete_space_CIEBased(const gs_color_space *pcs, const gs_state *pgs)
{	const gs_cie_render *pcie = pgs->cie_render;
	if ( pcie == 0 || pcie->RenderTable.table == 0 ||
	     pcie->RenderTable.m == 3
	   )
	  return &cie_rgb_space;
	else				/* pcie->RenderTable.m == 4 */
	  return &cie_cmyk_space;
}

/* Render a CIEBasedABC color. */
/* We provide both remap and concretize, but only the former */
/* needs to be efficient. */
private int
gx_remap_CIEABC(const gs_client_color *pc, const gs_color_space *pcs,
  gx_device_color *pdc, const gs_state *pgs)
{	frac conc[4];
	const gs_cie_abc *pcie = pcs->params.abc;
	gs_vector3 vec3;

	vec3.u = pc->paint.values[0];
	vec3.v = pc->paint.values[1];
	vec3.w = pc->paint.values[2];
	if ( !((pcie->RangeABC.is_unit & pcie->common.RangeLMN.is_unit &
		pcie->MatrixABC.is_identity) &&
	       (pcie->caches.DecodeABC[0].is_identity &
		pcie->caches.DecodeABC[1].is_identity &
		pcie->caches.DecodeABC[2].is_identity))
	   )
	  {
#define vabc vec3
			/* (*pcie->DecodeABC)(&vabc, pcie, &vabc); */
		cie_vlookup3(&vabc, &pcie->caches.DecodeABC[0]);
#define vlmn vec3
		cie_vmult3(&vabc /*&vlmn*/, &pcie->MatrixABC);
#undef vabc
	  }
	switch ( cie_remap_finish(&vlmn, &pcie->common, conc, pgs) )
	  {
	  case 3:
		gx_remap_concrete_rgb(conc[0], conc[1], conc[2], pdc, pgs);
		return 0;
	  case 4:
		gx_remap_concrete_cmyk(conc[0], conc[1], conc[2], conc[3], pdc, pgs);
		return 0;
	  }
	/* Can't happen. */
	return_error(gs_error_unknownerror);
#undef vlmn
}
private int
gx_concretize_CIEABC(const gs_client_color *pc, const gs_color_space *pcs,
  frac *pconc, const gs_state *pgs)
{	const gs_cie_abc *pcie = pcs->params.abc;
	gs_vector3 vec3;

	vec3.u = pc->paint.values[0];
	vec3.v = pc->paint.values[1];
	vec3.w = pc->paint.values[2];
#define vabc vec3
		/* (*pcie->DecodeABC)(&vabc, pcie, &vabc); */
	cie_vlookup3(&vabc, &pcie->caches.DecodeABC[0]);
#define vlmn vec3
	cie_vmult3(&vabc /*&vlmn*/, &pcie->MatrixABC);
#undef vabc
	cie_remap_finish(&vlmn, &pcie->common, pconc, pgs);
#undef vlmn
	return 0;
}

/* Render a CIEBasedA color. */
private int
gx_concretize_CIEA(const gs_client_color *pc, const gs_color_space *pcs,
  frac *pconc, const gs_state *pgs)
{	const gs_cie_a *pcie = pcs->params.a;
	const gx_cie_cache *pcache = &pcie->caches.DecodeA;
	float a = pc->paint.values[0];
	gs_vector3 vlmn;
	int itemp;

	vlookup(a, pcache, itemp);
	vlmn.u = a * pcie->MatrixA.u;
	vlmn.v = a * pcie->MatrixA.v;
	vlmn.w = a * pcie->MatrixA.w;
	return cie_remap_finish(&vlmn, &pcie->common, pconc, pgs);
}

/* Common rendering code. */
/* Return 3 if RGB, 4 if CMYK. */
private int near
cie_remap_finish(const gs_vector3 *plmn, const gs_cie_common *pcommon,
  frac *pconc, const gs_state *pgs)
{	const gs_cie_render *pcie = pgs->cie_render;
	const gx_cie_joint_caches *pjc = pgs->cie_joint_caches;
	gs_const_string *table;
	gs_vector3 vec3;
	int itemp;

	if ( pcie == 0 )
	  {	/* No rendering has been defined yet. */
		/* Just return black. */
		pconc[0] = pconc[1] = pconc[2] = frac_0;
		return 3;
	  }

		/* Finish decoding. */

#define vlmn vec3
	vlmn.u = plmn->u;
	vlmn.v = plmn->v;
	vlmn.w = plmn->w;
		/* (*pcommon->DecodeLMN)(&vlmn, pcommon, &vlmn); */
	cie_vlookup3(&vlmn, &pcommon->caches.DecodeLMN[0]);
	/*cie_vmult3(&vlmn/vxyz, &pcommon->MatrixLMN);*/

		/* Render. */

#define vpqr vec3
	/*cie_vmult3(&vxyz/vpqr, &pcie->MatrixPQR);*/
	cie_vmult3(&vlmn /*&vpqr*/, &pjc->MatrixLMN_PQR);
#undef vlmn
		/* (*pcie->TransformPQR)(&vpqr, &pjc->points_sd, pcie, &vpqr); */
	cie_vlookup3(&vpqr, &pjc->TransformPQR[0]);
#define vlmn vec3
	if ( !pcie->MatrixPQR_inverse_LMN.is_identity )
	  {	/* Do the LMN lookup in a separate step. */
		cie_vmult3(&vpqr /*&vlmn*/, &pcie->MatrixPQR_inverse_LMN);
			/* (*pcie->EncodeLMN)(&vlmn, pcie, &vlmn); */
		cie_vlookup3(&vlmn, &pcie->caches.EncodeLMN[0]);
		/*cie_restrict3(&vlmn, &pcie->RangeLMN, &vlmn);*/
	  }
#undef vpqr
#define vabc vec3
	cie_mult3(&vlmn, &pcie->MatrixABCEncode, &vabc);
#undef vlmn
		/* (*pcie->EncodeABC)(&vabc, pcie, &vabc); */
	/*cie_vlookup3(&vabc, &pcie->caches.EncodeABC[0]);*/
	/*cie_restrict3(&vabc, &pcie->RangeABC, &vabc);*/
	table = pcie->RenderTable.table;
	if ( table == 0 )
	{	/* No further transformation. */
		/* MatrixABCEncode includes the scaling of the cache index; */
		/* The final mapping step includes both restriction to */
		/* the range [0..1] and conversion to fracs. */
#define cabc(i, t)\
  pconc[i] =\
    pcie->caches.EncodeABC[i].values.fracs[\
      restrict_index(vabc.t - pcie->EncodeABC_base[i], itemp)]
		cabc(0, u);
		cabc(1, v);
		cabc(2, w);
#undef cabc
		return 3;
	}
	else
	{	/* Use the RenderTable. */
		/* We can speed this up a lot if we ever want to. */
		int m = pcie->RenderTable.m;
#define ri(s,n)\
  (int)((vabc.s - pcie->RangeABC.s.rmin) * (pcie->RenderTable.n - 1) /\
	(pcie->RangeABC.s.rmax - pcie->RangeABC.s.rmin) + 0.5)
		int ia = ri(u, NA);
		int ib = ri(v, NB);
		int ic = ri(w, NC);
		const byte *prtc =
		  table[ia].data + m * (ib * pcie->RenderTable.NC + ic);
			/* (*pcie->RenderTable.T)(prtc, m, pcie, pconc); */
#define shift_in(b) gx_cie_byte_to_cache_index(b)
#define rtc(i) (pcie->caches.RenderTableT[i])
/* The RenderTable cache is supposed to have been converted to fracs: */
#define rt_lookup(j, i) rtc(j).values.fracs[i]
/* but if it was loaded by the interpreter code, it still has floats: */
#undef rt_lookup
#define rt_lookup(j, i) float2frac(rtc(j).values.floats[i])
		pconc[0] = rt_lookup(0, shift_in(prtc[0]));
		pconc[1] = rt_lookup(1, shift_in(prtc[1]));
		pconc[2] = rt_lookup(2, shift_in(prtc[2]));
		if ( m != 3 )
		  pconc[3] = rt_lookup(3, shift_in(prtc[3]));
#undef rt_lookup
#undef rtc
#undef shift_in
		return m;
	}
}

/* ================ Utilities ================ */

#define if_debug_vector3(str, vec)\
  if_debug4('c', "%s[%g %g %g]\n", str, vec->u, vec->v, vec->w)
#define if_debug_matrix3(str, mat)\
  if_debug10('c', "%s[%g %g %g / %g %g %g / %g %g %g]\n", str,\
    mat->cu.u, mat->cu.v, mat->cu.w, mat->cv.u, mat->cv.v, mat->cv.w,\
    mat->cw.u, mat->cw.v, mat->cw.w)

/* Multiply a vector by a matrix. */
private void near
cie_mult3(const gs_vector3 *in, register const gs_matrix3 *mat,
  gs_vector3 *out)
{	if_debug_vector3("[c]mult", in);
	if_debug_matrix3("	*", mat);
	{	float u = in->u, v = in->v, w = in->w;
		out->u = (u * mat->cu.u) + (v * mat->cv.u) + (w * mat->cw.u);
		out->v = (u * mat->cu.v) + (v * mat->cv.v) + (w * mat->cw.v);
		out->w = (u * mat->cu.w) + (v * mat->cv.w) + (w * mat->cw.w);
	}
	if_debug_vector3("	=", out);
}

/* Multiply two matrices.  We assume the result is not an alias for */
/* either of the operands. */
private void near
cie_matrix_mult3(const gs_matrix3 *ma, const gs_matrix3 *mb, gs_matrix3 *mc)
{	gs_vector3 row_in, row_out;
	if_debug_matrix3("[c]matrix_mult", ma);
	if_debug_matrix3("             *", mb);
#define mult_row(e)\
  row_in.u = ma->cu.e, row_in.v = ma->cv.e, row_in.w = ma->cw.e;\
  cie_mult3(&row_in, mb, &row_out);\
  mc->cu.e = row_out.u, mc->cv.e = row_out.v, mc->cw.e = row_out.w
	mult_row(u);
	mult_row(v);
	mult_row(w);
#undef mult_row
	cie_matrix_init(mc);
	if_debug_matrix3("             =", mc);
}

/* Invert a matrix. */
/* The output must not be an alias for the input. */
private void near
cie_invert3(register const gs_matrix3 *in, register gs_matrix3 *out)
{	/* This is a brute force algorithm; maybe there are better. */
	/* We label the array elements */
	/*   [ A B C ]   */
	/*   [ D E F ]   */
	/*   [ G H I ]   */
#define A cu.u
#define B cv.u
#define C cw.u
#define D cu.v
#define E cv.v
#define F cw.v
#define G cu.w
#define H cv.w
#define I cw.w
	double coA = in->E * in->I - in->F * in->H; 
	double coB = in->F * in->G - in->D * in->I; 
	double coC = in->D * in->H - in->E * in->G;
	double det = in->A * coA + in->B * coB + in->C * coC;
	if_debug_matrix3("[c]invert", in);
	out->A = coA / det;
	out->D = coB / det;
	out->G = coC / det;
	out->B = (in->C * in->H - in->B * in->I) / det;
	out->E = (in->A * in->I - in->C * in->G) / det;
	out->H = (in->B * in->G - in->A * in->H) / det;
	out->C = (in->B * in->F - in->C * in->E) / det;
	out->F = (in->C * in->D - in->A * in->F) / det;
	out->I = (in->A * in->E - in->B * in->D) / det;
	if_debug_matrix3("        =", out);
#undef A
#undef B
#undef C
#undef D
#undef E
#undef F
#undef G
#undef H
#undef I
	out->is_identity = in->is_identity;
}

/* Look up 3 values in a cache. */
private void near
cie_lookup3(gs_vector3 *pv, const gx_cie_cache *pc /*[3]*/)
{	register int itemp;
	if_debug4('c', "[c]lookup 0x%lx [%g %g %g]\n", (ulong)pc,
		  pv->u, pv->v, pv->w);
	pv->u = lookup(pv->u, pc, floats, itemp);
	pv->v = lookup(pv->v, pc + 1, floats, itemp);
	pv->w = lookup(pv->w, pc + 2, floats, itemp);
	if_debug_vector3("        =", pv);
}

/* Set the is_identity flag that accelerates multiplication. */
private void near
cie_matrix_init(register gs_matrix3 *mat)
{	mat->is_identity =
	  mat->cu.u == 1.0 && is_fzero2(mat->cu.v, mat->cu.w) &&
	  mat->cv.v == 1.0 && is_fzero2(mat->cv.u, mat->cv.w) &&
	  mat->cw.w == 1.0 && is_fzero2(mat->cw.u, mat->cw.v);
}
