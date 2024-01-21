/* Copyright (C) 1992, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gscie.h */
/* Structures for CIE color algorithms */
/* (requires gscspace.h, gscolor2.h) */
#include "gsrefct.h"

/* ------ Common definitions ------ */

/* A 3-element vector. */
typedef struct gs_vector3_s {
	float u, v, w;
} gs_vector3;

/* A 3x3 matrix, stored in column order. */
typedef struct gs_matrix3_s {
	gs_vector3 cu, cv, cw;
	bool is_identity;
} gs_matrix3;

/* A 3-element vector of ranges. */
typedef struct gs_range_s {
	float rmin, rmax;
} gs_range;
typedef struct gs_range3_s {
	gs_range u, v, w;	/* must be first, for aliasing to float[] */
	bool is_unit;		/* true if all ranges are [0..1] */
} gs_range3;

/* Client-supplied transformation procedures. */
typedef struct gs_cie_common_s gs_cie_common;
typedef struct gs_cie_wbsd_s gs_cie_wbsd;
#define struct_procn(pn_s, p, n)\
struct pn_s { p procs[n]; }
typedef float (*gs_cie_a_proc)(P2(floatp, const gs_cie_a *));
typedef float (*gs_cie_abc_proc)(P2(floatp, const gs_cie_abc *));
typedef struct_procn(gs_cie_abc_proc3_s, gs_cie_abc_proc, 3)
  gs_cie_abc_proc3;
typedef float (*gs_cie_common_proc)(P2(floatp, const gs_cie_common *));
typedef struct_procn(gs_cie_common_proc3_s, gs_cie_common_proc, 3)
  gs_cie_common_proc3;
typedef float (*gs_cie_render_proc)(P2(floatp, const gs_cie_render *));
typedef struct_procn(gs_cie_render_proc3_s, gs_cie_render_proc, 3)
  gs_cie_render_proc3;
typedef float (*gs_cie_transform_proc)(P3(floatp, const gs_cie_wbsd *,
  const gs_cie_render *));
typedef struct_procn(gs_cie_transform_proc3_s, gs_cie_transform_proc, 3)
  gs_cie_transform_proc3;
typedef frac (*gs_cie_render_table_proc)(P2(byte, const gs_cie_render *));
typedef struct_procn(gs_cie_render_table_procs_s, gs_cie_render_table_proc, 4)
  gs_cie_render_table_procs;

/* CIE white and black points. */
typedef struct gs_cie_wb_s {
	gs_vector3 WhitePoint;
	gs_vector3 BlackPoint;
} gs_cie_wb;

/* ------ Caches ------ */

/*
 * Given that all the client-supplied procedures involved in CIE color
 * mapping and rendering are monotonic, and given that we can determine
 * the minimum and maximum input values for them, we can cache their values.
 * This takes quite a lot of space, but eliminates the need for callbacks
 * deep in the graphics code (particularly the image operator).
 *
 * The procedures, and how we determine their domains, are as follows:

Stage		Name		Domain determination
-----		----		--------------------
color space	DecodeA		RangeA
color space	DecodeABC	RangeABC
color space	DecodeLMN	RangeLMN
rendering	TransformPQR	RangePQR
  (but depends on color space White/BlackPoints)
rendering	EncodeLMN	RangePQR transformed by the inverse of
				  MatrixPQR and then by MatrixLMN
rendering	EncodeABC	RangeLMN transformed by MatrixABC
rendering	RenderTable.T	[0..1]*m

 * Note that we can mostly cache the results of the color space procedures
 * without knowing the color rendering parameters, and vice versa,
 * because of the range parameters supplied in the dictionaries.
 * We suspect it's no accident that Adobe specified things this way.  :-)
 * Unfortunately, TransformPQR is an exception.
 */

/* The index into a cache is (value - base) * factor, where */
/* factor is computed as (cie_cache_size - 1) / (rmax - rmin). */
#define gx_cie_log2_cache_size 8
#define gx_cie_cache_size (1 << gx_cie_log2_cache_size)
typedef struct gx_cie_cache_s {
	float base, factor;
	union _ccv {
	  float floats[gx_cie_cache_size];
	  frac fracs[gx_cie_cache_size];
	  int ints[gx_cie_cache_size];
	} values;
	bool is_identity;
} gx_cie_cache;
#if gx_cie_log2_cache_size < 8
#  define gx_cie_byte_to_cache_index(b)\
     ((b) >> (8 - gx_cie_log2_cache_size))
#else
#  if gx_cie_log2_cache_size > 8
#    define gx_cie_byte_to_cache_index(b)\
       (((b) << (gx_cie_log2_cache_size - 8)) +\
	((b) >> (16 - gx_cie_log2_cache_size)))
#  else
#    define gx_cie_byte_to_cache_index(b) (b)
#  endif
#endif

/* ------ Color space dictionaries ------ */

/* Elements common to ABC and A dictionaries. */
struct gs_cie_common_s {
	gs_range3 RangeLMN;
	gs_cie_common_proc3 DecodeLMN;
	gs_matrix3 MatrixLMN;
	gs_cie_wb points;
		/* Following are computed when structure is initialized. */
	struct {
		gx_cie_cache DecodeLMN[3];
	} caches;
};

/* A CIEBasedABC dictionary. */
struct gs_cie_abc_s {
	gs_cie_common common;		/* must be first */
	rc_header rc;
	gs_range3 RangeABC;
	gs_cie_abc_proc3 DecodeABC;
	gs_matrix3 MatrixABC;
		/* Following are computed when structure is initialized. */
	struct {
		gx_cie_cache DecodeABC[3];
	} caches;
};
#define private_st_cie_abc()	/* in zcie.c */\
  gs_private_st_simple(st_cie_abc, gs_cie_abc, "gs_cie_abc")

/* A CIEBasedA dictionary. */
struct gs_cie_a_s {
	gs_cie_common common;		/* must be first */
	rc_header rc;
	gs_range RangeA;
	gs_cie_a_proc DecodeA;
	gs_vector3 MatrixA;
		/* Following are computed when structure is initialized. */
	struct {
		gx_cie_cache DecodeA;
	} caches;
};
#define private_st_cie_a()	/* in zcie.c */\
  gs_private_st_simple(st_cie_a, gs_cie_a, "gs_cie_a")

/* Default values for components */
extern const gs_range3 Range3_default;
extern const gs_cie_abc_proc3 DecodeABC_default;
extern const gs_cie_common_proc3 DecodeLMN_default;
extern const gs_matrix3 Matrix3_default;
extern const gs_range RangeA_default;
extern const gs_cie_a_proc DecodeA_default;
extern const gs_vector3 MatrixA_default;
extern const gs_vector3 BlackPoint_default;
extern const gs_cie_render_proc3 Encode_default;
extern const gs_cie_transform_proc3 TransformPQR_default;
extern const gs_cie_render_table_procs RenderTableT_default;

/* ------ Rendering dictionaries ------ */

struct gs_cie_wbsd_s {
	struct { gs_vector3 xyz, pqr; } ws, bs, wd, bd;
};
/* The main dictionary */
struct gs_cie_render_s {
	rc_header rc;
	gs_matrix3 MatrixLMN;
	gs_cie_render_proc3 EncodeLMN;
	gs_range3 RangeLMN;
	gs_matrix3 MatrixABC;
	gs_cie_render_proc3 EncodeABC;
	gs_range3 RangeABC;
	gs_cie_wb points;
	gs_matrix3 MatrixPQR;
	gs_range3 RangePQR;
	gs_cie_transform_proc3 TransformPQR;
	struct {
		int NA, NB, NC;			/* >1 */
		int m;				/* 3 or 4 */
		/* It isn't really necessary to store the size */
		/* of each string, since they're all the same size, */
		/* but it makes things a lot easier for the GC. */
		gs_const_string *table;		/* [NA][m * NB * NC], */
						/* 0 means no table */
		gs_cie_render_table_procs T;	/* takes byte[m], */
						/* returns frac[m] */
	} RenderTable;
		/* Following are computed when structure is initialized. */
	gs_range3 DomainLMN;
	gs_range3 DomainABC;
	gs_matrix3 MatrixABCEncode;
	float EncodeABC_base[3];
	gs_matrix3 MatrixPQR_inverse_LMN;
	gs_vector3 wdpqr, bdpqr;
	struct {
		gx_cie_cache EncodeLMN[3];
		gx_cie_cache EncodeABC[3];
		gx_cie_cache RenderTableT[4];
	} caches;
};
#define private_st_cie_render()	/* in zcie.c */\
  gs_private_st_ptrs1(st_cie_render, gs_cie_render, "gs_cie_render",\
    cie_render_enum_ptrs, cie_render_reloc_ptrs, RenderTable.table)
/* RenderTable.table points to an array of st_const_string_elements. */
#define private_st_const_string()	/* in gscie.c */\
  gs_private_st_composite(st_const_string, gs_const_string, "gs_const_string",\
    const_string_enum_ptrs, const_string_reloc_ptrs)
extern_st(st_const_string_element);
#define public_st_const_string_element()	/* in gscie.c */\
  gs_public_st_element(st_const_string_element, gs_const_string,\
    "gs_const_string[]", const_string_elt_enum_ptrs,\
    const_string_elt_reloc_ptrs, st_const_string)

/* ------ Joint caches ------ */

/* This cache depends on both the color space and the rendering */
/* dictionary -- see above. */

typedef struct gx_cie_joint_caches_s {
	rc_header rc;
	gs_cie_wbsd points_sd;
	gs_matrix3 MatrixLMN_PQR;
	gx_cie_cache TransformPQR[3];
} gx_cie_joint_caches;
#define private_st_joint_caches() /* in gscie.c */\
  gs_private_st_simple(st_joint_caches, gx_cie_joint_caches,\
    "gx_cie_joint_caches")

/* Internal routines */
typedef struct gs_for_loop_params_s {
	float init, step, limit;
} gs_for_loop_params;
void gs_cie_cache_init(P3(gx_cie_cache *, gs_for_loop_params *, const gs_range *));
int gs_cie_render_init(P1(gs_cie_render *));
int gs_cie_render_complete(P1(gs_cie_render *));
