/* Copyright (C) 1989, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gsimage.c */
/* Image setup procedures for Ghostscript library */
#include "gx.h"
#include "math_.h"
#include "memory_.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gxfixed.h"
#include "gxfrac.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gsccolor.h"
#include "gspaint.h"
#include "gzstate.h"
#include "gxdevice.h"
#include "gzpath.h"
#include "gzcpath.h"
#include "gxdevmem.h"
#include "gximage.h"

/* Structure descriptor */
private_st_gs_image_enum();

/* Declare the 1-for-1 unpacking procedure so we can test for it */
/* in the GC procedures. */
extern iunpack_proc(image_unpack_copy);

/* GC procedures */
#define eptr ((gs_image_enum *)vptr)
private ENUM_PTRS_BEGIN(image_enum_enum_ptrs) {
	int bps;
	gs_ptr_type_t ret;
	/* Enumerate the data planes. */
	index -= 4 + image_scale_state_max_ptrs;
	if ( index < eptr->plane_index )
	  { *pep = (void *)&eptr->planes[index];
	    return ptr_string_type;
	  }
	/* Enumerate the used members of clues.dev_color. */
	index -= eptr->plane_index;
	bps = eptr->bps;
	if ( eptr->spp != 1 )
	  bps = 8;
	else if ( bps > 8 || eptr->unpack == image_unpack_copy )
	  bps = 1;
	if ( index >= (1 << bps) * st_device_color_max_ptrs ) /* done */
	  return 0;
	ret = (*st_device_color.enum_ptrs)
	  (&eptr->clues[(index / st_device_color_max_ptrs) *
			     (255 / ((1 << bps) - 1))].dev_color,
	   sizeof(eptr->clues[0].dev_color),
	   index % st_device_color_max_ptrs, pep);
	if ( ret == 0 )		/* don't stop early */
	  { *pep = 0;
	    break;
	  }
	return ret;
	}
	ENUM_PTR(0, gs_image_enum, pgs);
	ENUM_PTR(1, gs_image_enum, buffer);
	ENUM_PTR(2, gs_image_enum, line);
	ENUM_PTR(3, gs_image_enum, clip_dev);
	image_scale_state_ENUM_PTRS(4, gs_image_enum, scale_state.);
ENUM_PTRS_END
private RELOC_PTRS_BEGIN(image_enum_reloc_ptrs) {
	int i;
	RELOC_PTR(gs_image_enum, pgs);
	RELOC_PTR(gs_image_enum, buffer);
	RELOC_PTR(gs_image_enum, line);
	RELOC_PTR(gs_image_enum, clip_dev);
	image_scale_state_RELOC_PTRS(gs_image_enum, scale_state.);
	for ( i = 0; i < eptr->plane_index; i++ )
	  RELOC_CONST_STRING_PTR(gs_image_enum, planes[i]);
	{ int bps = eptr->bps;
	  if ( eptr->spp != 1 )
	    bps = 8;
	  else if ( bps > 8 || eptr->unpack == image_unpack_copy )
	    bps = 1;
	  for ( i = 0; i <= 255; i += 255 / ((1 << bps) - 1) )
	    (*st_device_color.reloc_ptrs)
	      (&eptr->clues[i].dev_color, sizeof(gx_device_color), gcst);
	}
} RELOC_PTRS_END
#undef eptr

/* Forward declarations */
private void image_init_map(P3(byte *, int, const float *));
private int image_init(P10(gs_image_enum *, int, int, int,
  int, int, bool, gs_matrix *, gs_state *, fixed));

/* Procedures for unpacking the input data into bytes or fracs. */
/*extern iunpack_proc(image_unpack_copy);*/	/* declared above */
extern iunpack_proc(image_unpack_1);
extern iunpack_proc(image_unpack_1_spread);
extern iunpack_proc(image_unpack_2);
extern iunpack_proc(image_unpack_2_spread);
extern iunpack_proc(image_unpack_4);
extern iunpack_proc(image_unpack_8);
extern iunpack_proc(image_unpack_8_spread);
extern iunpack_proc(image_unpack_12);

/* The image_render procedures work on fully expanded, complete rows. */
/* These take a height argument, which is an integer >= 0; */
/* they return a negative code, or the number of */
/* rows actually processed (which may be less than the height). */
/* height = 0 is a special call to indicated that the image has been */
/* fully processed; this is necessary because the last scan lines of */
/* the source data may not produce any output. */
extern irender_proc(image_render_skip);
extern irender_proc(image_render_simple);
extern irender_proc(image_render_landscape);
extern irender_proc(image_render_mono);
extern irender_proc(image_render_color);
extern irender_proc(image_render_frac);
extern irender_proc(image_render_interpolate);

/* Define 'strategy' procedures for selecting imaging methods. */
/* Strategies are called in a known order, so each one may assume */
/* that all the previous ones failed. */
/* If a strategy succeeds, it may update the enumerator structure */
/* as well as returning the rendering procedure. */
typedef irender_proc((*irender_proc_t));
#define image_strategy_proc(proc)\
  irender_proc_t proc(P1(gs_image_enum *penum))
private image_strategy_proc(image_strategy_skip);
private image_strategy_proc(image_strategy_interpolate);
private image_strategy_proc(image_strategy_simple);
private image_strategy_proc(image_strategy_frac);

/* Standard mask tables for spreading input data. */
/* Note that the mask tables depend on the end-orientation of the CPU. */
/* We can't simply define them as byte arrays, because */
/* they might not wind up properly long- or short-aligned. */
#define map4tox(z,a,b,c,d)\
	z, z^a, z^b, z^(a+b),\
	z^c, z^(a+c), z^(b+c), z^(a+b+c),\
	z^d, z^(a+d), z^(b+d), z^(a+b+d),\
	z^(c+d), z^(a+c+d), z^(b+c+d), z^(a+b+c+d)
#if arch_is_big_endian
private const bits32 map_4x1_to_32[16] =
   {	map4tox(0L, 0xffL, 0xff00L, 0xff0000L, 0xff000000L)	};
private const bits32 map_4x1_to_32_invert[16] =
   {	map4tox(0xffffffffL, 0xffL, 0xff00L, 0xff0000L, 0xff000000L)	};
#else					/* !arch_is_big_endian */
private const bits32 map_4x1_to_32[16] =
   {	map4tox(0L, 0xff000000L, 0xff0000L, 0xff00L, 0xffL)	};
private const bits32 map_4x1_to_32_invert[16] =
   {	map4tox(0xffffffffL, 0xff000000L, 0xff0000L, 0xff00L, 0xffL)	};
#endif

/* Allocate an image enumerator. */
gs_image_enum *
gs_image_enum_alloc(gs_memory_t *mem, client_name_t cname)
{	return gs_alloc_struct(mem, gs_image_enum, &st_gs_image_enum, cname);
}

/* Start processing an image */
int
gs_image_init(gs_image_enum *penum, gs_state *pgs,
  int width, int height, int bps,
  bool multi, const gs_color_space *pcs, const float *decode /* [spp*2] */,
  bool interpolate, gs_matrix *pmat)
{	const gs_color_space_type _ds *pcst = pcs->type;
	int spp = pcst->num_components;
	int ci;
	bool device_color =
		(*pcst->concrete_space)(pcs, pgs) == pcs;
	static const float default_decode[8] =
		{ 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 };

	if ( pgs->in_cachedevice )
		return_error(gs_error_undefined);
	if ( spp < 0 )			/* Pattern not allowed */
		return_error(gs_error_rangecheck);
	if ( decode == 0 )
		decode = default_decode;

	/* Initialize the color table */

#define ictype(i)\
  penum->clues[i].dev_color.type
	switch ( (spp == 1 ? bps : 8) )
	{
	case 8:			/* includes all color images */
	{	register gx_image_clue *pcht = &penum->clues[0];
		register int n = 64;
		do
		{	pcht[0].dev_color.type =
			  pcht[1].dev_color.type =
			  pcht[2].dev_color.type =
			  pcht[3].dev_color.type =
			    &gx_dc_none;
			pcht[0].key = pcht[1].key =
			  pcht[2].key = pcht[3].key = 0;
			pcht += 4;
		}
		while ( --n > 0 );
		penum->clues[0].key = 1;	/* guarantee no hit */
		break;
	}
	case 4:
		ictype(17) = ictype(2*17) = ictype(3*17) =
		  ictype(4*17) = ictype(6*17) = ictype(7*17) =
		  ictype(8*17) = ictype(9*17) = ictype(11*17) =
		  ictype(12*17) = ictype(13*17) = ictype(14*17) =
		  &gx_dc_none;
		/* falls through */
	case 2:
		ictype(5*17) = ictype(10*17) = &gx_dc_none;
#undef ictype
	}

	/* Initialize the maps from samples to intensities. */

	for ( ci = 0; ci < spp; ci++ )
	{	sample_map *pmap = &penum->map[ci];

		/* If the decoding is [0 1] or [1 0], we can fold it */
		/* into the expansion of the sample values; */
		/* otherwise, we have to use the floating point method. */

		const float *this_decode = &decode[ci * 2];
		const float *map_decode;	/* decoding used to */
				/* construct the expansion map */

		const float *real_decode;	/* decoding for */
				/* expanded samples */

		bool no_decode;

		map_decode = real_decode = this_decode;
		if ( map_decode[0] == 0.0 && map_decode[1] == 1.0 )
			no_decode = true;
		else if ( map_decode[0] == 1.0 && map_decode[1] == 0.0 )
			no_decode = true,
			real_decode = default_decode;
		else
			no_decode = false,
			device_color = false,
			map_decode = default_decode;
		if ( bps > 2 || multi )
		{	if ( bps <= 8 )
			  image_init_map(&pmap->table.lookup8[0], 1 << bps,
					 map_decode);
		}
		else
		{	/* The map index encompasses more than one pixel. */
			byte map[4];
			register int i;
			image_init_map(&map[0], 1 << bps, map_decode);
			switch ( bps )
			{
			case 1:
			{	register bits32 *p = &pmap->table.lookup4x1to32[0];
				if ( map[0] == 0 && map[1] == 0xff )
					memcpy((byte *)p, map_4x1_to_32, 16 * 4);
				else if ( map[0] == 0xff && map[1] == 0 )
					memcpy((byte *)p, map_4x1_to_32_invert, 16 * 4);
				else
				  for ( i = 0; i < 16; i++, p++ )
					((byte *)p)[0] = map[i >> 3],
					((byte *)p)[1] = map[(i >> 2) & 1],
					((byte *)p)[2] = map[(i >> 1) & 1],
					((byte *)p)[3] = map[i & 1];
			}	break;
			case 2:
			{	register bits16 *p = &pmap->table.lookup2x2to16[0];
				for ( i = 0; i < 16; i++, p++ )
					((byte *)p)[0] = map[i >> 2],
					((byte *)p)[1] = map[i & 3];
			}	break;
			}
		}
		pmap->decode_base /* = decode_lookup[0] */ = real_decode[0];
		pmap->decode_factor =
		  (real_decode[1] - real_decode[0]) /
		    (bps <= 8 ? 255.0 : (float)frac_1);
		pmap->decode_max /* = decode_lookup[15] */ = real_decode[1];
		if ( no_decode )
			pmap->decoding = sd_none;
		else if ( bps <= 4 )
		{	static const int steps[] = { 0, 15, 5, 0, 1 };
			int step = steps[bps];
			int i;
			pmap->decoding = sd_lookup;
			for ( i = 15 - step; i > 0; i -= step )
			  pmap->decode_lookup[i] = pmap->decode_base +
			    i * (255.0 / 15) * pmap->decode_factor;
		}
		else
			pmap->decoding = sd_compute;
		if ( spp == 1 )		/* and ci == 0 */
		{	/* Pre-map entries 0 and 255. */
			gs_client_color cc;
			cc.paint.values[0] = real_decode[0];
			(*pcst->remap_color)(&cc, pcs, &penum->icolor0, pgs);
			cc.paint.values[0] = real_decode[1];
			(*pcst->remap_color)(&cc, pcs, &penum->icolor1, pgs);
		}
	}

	penum->masked = 0;
	penum->device_color = device_color;
	return image_init(penum, width, height, bps, multi, spp, interpolate,
			  pmat, pgs, (fixed)0);
}
/* Construct a mapping table for sample values. */
/* map_size is 2, 4, 16, or 256.  Note that 255 % (map_size - 1) == 0. */
private void
image_init_map(byte *map, int map_size, const float *decode)
{	float min_v = decode[0], max_v = decode[1];
	byte *limit = map + map_size;
	uint value = min_v * 0xffffL;
	/* The division in the next statement is exact, */
	/* see the comment above. */
	uint diff = (max_v - min_v) * (0xffffL / (map_size - 1));
	for ( ; map != limit; map++, value += diff )
		*map = value >> 8;
}

/* Start processing a masked image */
int
gs_imagemask_init(gs_image_enum *penum, gs_state *pgs,
  int width, int height, bool invert, bool interpolate,
  gs_matrix *pmat, bool adjust)
{	gx_set_dev_color(pgs);
	/* Initialize color entries 0 and 255. */
	penum->icolor0.type = &gx_dc_pure;
	penum->icolor0.colors.pure = gx_no_color_index;
	penum->icolor1 = *pgs->dev_color;
	penum->masked = 1;
	memcpy(&penum->map[0].table.lookup4x1to32[0],
	       (invert ? map_4x1_to_32_invert : map_4x1_to_32),
	       16 * 4);
	penum->map[0].decoding = sd_none;
	return image_init(penum, width, height, 1, false, 1, interpolate,
			  pmat, pgs,
			  (adjust && (pgs->in_cachedevice > 1) ?
			   float2fixed(0.25) : (fixed)0));
}

/* Common setup for image and imagemask. */
/* Caller has set penum->masked, map, clues[]. */
private int
image_init(register gs_image_enum *penum, int width, int height, int bps,
  bool multi, int spp, bool interpolate, gs_matrix *pmat, gs_state *pgs,
  fixed adjust)
{	int code;
	int index_bps;
	gs_matrix mat;
	gs_fixed_rect clip_box;
	int log2_xbytes = (bps <= 8 ? 0 : arch_log2_sizeof_frac);
	int nplanes = (multi ? spp : 1);
	int spread = nplanes << log2_xbytes;
	uint bsize = (width + 8) * spp;	/* round up, +1 for end-of-run byte */
	byte *buffer;
	fixed mtx, mty;
	if ( width < 0 || height < 0 )
		return_error(gs_error_rangecheck);
	switch ( bps )
	   {
	case 1: index_bps = 0; break;
	case 2: index_bps = 1; break;
	case 4: index_bps = 2; break;
	case 8: index_bps = 3; break;
	case 12: index_bps = 4; break;
	default: return_error(gs_error_rangecheck);
	   }
	if ( width == 0 || height == 0 )
		return 1;	/* empty image */
	if (	(code = gs_matrix_invert(pmat, &mat)) < 0 ||
		(code = gs_matrix_multiply(&mat, &ctm_only(pgs), &mat)) < 0
	   )	return code;
	buffer = gs_alloc_bytes(pgs->memory, bsize, "image buffer");
	if ( buffer == 0 )
		return_error(gs_error_VMerror);
	penum->width = width;
	penum->height = height;
	penum->bps = bps;
	penum->log2_xbytes = log2_xbytes;
	penum->spp = spp;
	penum->num_planes = nplanes;
	penum->spread = spread;
	penum->matrix = mat;
#define fixed_rounding (fixed2float(fixed_epsilon) / 2)
	penum->fxx = float2fixed(mat.xx + fixed_rounding);
	penum->fxy = float2fixed(mat.xy + fixed_rounding);
	penum->fyx = float2fixed(mat.yx + fixed_rounding);
	penum->fyy = float2fixed(mat.yy + fixed_rounding);
#undef fixed_rounding
	penum->posture =
	  ((penum->fxy | penum->fyx) == 0 ? image_portrait :
	   (penum->fxx | penum->fyy) == 0 ? image_landscape : image_skewed);
	mtx = float2fixed(mat.tx);
	mty = float2fixed(mat.ty);
	penum->pgs = pgs;
	clip_box = pgs->clip_path->path.bbox;	/* box is known to be up to date */
	penum->clip_box = clip_box;
	penum->buffer = buffer;
	penum->buffer_size = bsize;
	penum->line = 0;
	penum->line_size = 0;
	penum->bytes_per_row =
		(uint)(((ulong)width * (bps * spp) / nplanes + 7) >> 3);
	penum->interpolate = interpolate;
	penum->slow_loop = 0;
	penum->clip_dev = 0;		/* in case we bail out */
	image_scale_state_clear_ptrs(&penum->scale_state);  /* for GC */
	penum->scale_state.memory = pgs->memory;
	/* If all four extrema of the image fall within the clipping */
	/* rectangle, clipping is never required. */
	   {	gs_fixed_rect cbox;
		fixed edx = float2fixed(mat.xx * width);
		fixed edy = float2fixed(mat.yy * height);
		fixed epx, epy, eqx, eqy;
		if ( edx < 0 ) epx = edx, eqx = 0;
		else epx = 0, eqx = edx;
		if ( edy < 0 ) epy = edy, eqy = 0;
		else epy = 0, eqy = edy;
		if ( penum->posture != image_portrait )
		{	edx = float2fixed(mat.yx * height);
			edy = float2fixed(mat.xy * width);
			if ( edx < 0 ) epx += edx; else eqx += edx;
			if ( edy < 0 ) epy += edy; else eqy += edy;
		}
		else
		{	/*
			 * If the image is only 1 sample wide or high,
			 * and is less than 1 device pixel wide or high,
			 * move it slightly so that it covers pixel centers.
			 * This is a hack to work around a bug in TeX/dvips,
			 * which uses 1-bit-high images to draw horizontal
			 * (and vertical?) lines without positioning them
			 * properly.
			 */
			fixed diff;
			if ( width == 1 && eqx - epx < fixed_1 )
				diff = arith_rshift_1(penum->fxx),
				mtx = (((mtx + diff) | fixed_half)
					& -fixed_half) - diff;
			if ( height == 1 && eqy - epy < fixed_1 )
				diff = arith_rshift_1(penum->fyy),
				mty = (((mty + diff) | fixed_half)
					& -fixed_half) - diff;
		}
		gx_cpath_inner_box(pgs->clip_path, &cbox);
		penum->never_clip =
			mtx + epx >= cbox.p.x && mtx + eqx <= cbox.q.x &&
			mty + epy >= cbox.p.y && mty + eqy <= cbox.q.y;
		if_debug7('b',
			  "[b]Image: cbox=(%g,%g),(%g,%g)\n	mt=(%g,%g) never_clip=%d\n",
			  fixed2float(cbox.p.x), fixed2float(cbox.p.y),
			  fixed2float(cbox.q.x), fixed2float(cbox.q.y),
			  fixed2float(mtx), fixed2float(mty),
			  penum->never_clip);
	   }
	penum->plane_index = 0;
	penum->byte_in_row = 0;
	penum->xcur = penum->mtx = mtx;
	penum->ycur = penum->mty = mty;
	penum->y = 0;
	penum->adjust = adjust;
	   {	static iunpack_proc((*procs[5])) = {
			image_unpack_1, image_unpack_2,
			image_unpack_4, image_unpack_8, image_unpack_12
		   };
		static iunpack_proc((*spread_procs[5])) = {
			image_unpack_1_spread, image_unpack_2_spread,
			image_unpack_4, image_unpack_8_spread,
			image_unpack_12
		   };
		if ( nplanes != 1 )
		  {	penum->unpack = spread_procs[index_bps];
			if_debug1('b', "[b]unpack=spread %d\n", bps);
		  }
		else
		  {	penum->unpack = procs[index_bps];
			if_debug1('b', "[b]unpack=%d\n", bps);
		  }
		penum->slow_loop |=
			/* Use slow loop for imagemask with a halftone */
			(penum->masked &&
			 !color_is_pure(pgs->dev_color));
		if ( (penum->render = image_strategy_skip(penum)) == 0 &&
		     (penum->render = image_strategy_interpolate(penum)) == 0 &&
		     (penum->render = image_strategy_simple(penum)) == 0 &&
		     (penum->render = image_strategy_frac(penum)) == 0
		   )
		  {	/* Use default logic. */
			penum->render =
			  (spp == 1 ? image_render_mono :
			   image_render_color);
			if_debug1('b', "[b]render=%s\n",
				  (spp == 1 ? "mono" : "color"));
		  }
	   }
	if ( !penum->never_clip )
	  {	/* Set up the clipping device. */
		gx_device_clip *cdev =
		  gs_alloc_struct(pgs->memory, gx_device_clip,
				  &st_device_clip, "image clipper");
		if ( cdev == 0 )
		  {	gs_image_cleanup(penum);
			return_error(gs_error_VMerror);
		  }
		gx_make_clip_device(cdev, cdev, &pgs->clip_path->list);
		penum->clip_dev = cdev;
		cdev->target = gs_currentdevice(pgs);
		(*dev_proc(cdev, open_device))((gx_device *)cdev);
	  }
	if_debug9('b', "[b]Image: w=%d h=%d %s\n   [%f %f %f %f %f %f]\n",
		 width, height,
		 (penum->never_clip ? "no clip" : "must clip"),
		 mat.xx, mat.xy, mat.yx, mat.yy, mat.tx, mat.ty);
	return 0;
}

/* Strategy procedures */

/* If we're in a charpath, don't image anything. */
private irender_proc_t
image_strategy_skip(gs_image_enum *penum)
{	if ( !penum->pgs->in_charpath )
	  return 0;
	if_debug0('b', "[b]render=skip\n");
	return image_render_skip;
}

/* If we're interpolating, use special logic. */
private irender_proc_t
image_strategy_interpolate(gs_image_enum *penum)
{	gs_state *pgs = penum->pgs;
	const gs_color_space *pcs = pgs->color_space;
	gs_point dst_xy;
	int code;

	if ( !penum->interpolate )
	  return 0;
	if ( penum->posture != image_portrait || penum->masked )
	  {	/* We can't handle these cases yet.  Punt. */
		penum->interpolate = false;
		return 0;
	  }
	/* Non-ANSI compilers require the following casts: */
	gs_distance_transform((float)penum->width, (float)penum->height,
			      &penum->matrix, &dst_xy);
	if ( penum->bps <= 8 && penum->device_color )
	  penum->scale_state.sizeofPixelIn = 1,
	  penum->scale_state.maxPixelIn = 0xff;
	else
	  penum->scale_state.sizeofPixelIn = sizeof(frac),
	  penum->scale_state.maxPixelIn = frac_1;
	penum->scale_state.sizeofPixelOut = sizeof(frac);
	penum->scale_state.maxPixelOut = frac_1;
	penum->scale_state.dst_width = (int)ceil(dst_xy.x);
	penum->scale_state.dst_height = (int)ceil(dst_xy.y);
	penum->scale_state.src_width = penum->width;
	penum->scale_state.src_height = penum->height;
	penum->scale_state.values_per_pixel =
	  cs_concrete_space(pcs, pgs)->type->num_components;
	code = gs_image_scale_init(&penum->scale_state,
				   true, true);
	if ( code < 0 )
	  {	/* Try again without interpolation. */
		penum->interpolate = false;
		return 0;
	  }
	if_debug0('b', "[b]render=interpolate\n");
	return image_render_interpolate;
}

/* Use special fast logic for portrait or landscape black-and-white images. */
private irender_proc_t
image_strategy_simple(gs_image_enum *penum)
{	irender_proc_t rproc;
	if ( !(penum->spp == 1 && penum->bps == 1 && !penum->slow_loop &&
	       (penum->masked ||
		(color_is_pure(&penum->icolor0) &&
		 color_is_pure(&penum->icolor1))))
	   )
	  return 0;
	switch ( penum->posture )
	  {
	  case image_portrait:
	  {	/* Use fast portrait algorithm. */
		long dev_width =
		  fixed2long_rounded(penum->mtx + penum->width * penum->fxx) -
		    fixed2long_rounded(penum->mtx);

		if ( dev_width != penum->width )
		  {	/* Add an extra align_bitmap_mod of padding so that */
			/* we can align scaled rows with the device. */
			long line_size =
			  bitmap_raster(any_abs(dev_width)) + align_bitmap_mod;
			if ( penum->adjust != 0 || line_size > max_uint )
			  return 0;
			/* Must buffer a scan line. */
			penum->line_width = any_abs(dev_width);
			penum->line_size = (uint)line_size;
			penum->line = gs_alloc_bytes(penum->pgs->memory,
					     penum->line_size, "image line");
			if ( penum->line == 0 )
			  {	gs_image_cleanup(penum);
				return 0;
			  }
		  }
		if_debug2('b', "[b]render=simple, unpack=copy; width=%d, dev_width=%ld\n",
			  penum->width, dev_width);
		rproc = image_render_simple;
		break;
	  }
#if 0		/****** DOESN'T WORK IN THIS RELEASE ******/
	  case image_landscape:
	  {	/* Use fast landscape algorithm. */
		long dev_width =
		  fixed2long_rounded(penum->mty + penum->width * penum->fxy) -
		    fixed2long_rounded(penum->mty);
		long line_size =
		  (dev_width = any_abs(dev_width),
		   bitmap_raster(dev_width) * 8 +
		   round_up(dev_width, 8) * align_bitmap_mod);

		if ( (dev_width != penum->width && penum->adjust != 0) ||
		     line_size > max_uint
		   )
		  return 0;
		/* Must buffer a group of 8N scan lines. */
		penum->line_width = dev_width;
		penum->line_size = (uint)line_size;
		penum->line = gs_alloc_bytes(penum->pgs->memory,
				     penum->line_size, "image line");
		if ( penum->line == 0 )
		  {	gs_image_cleanup(penum);
			return 0;
		  }
		penum->line_xy = fixed2int_var_rounded(penum->xcur);
		if_debug3('b', "[b]render=landscape, unpack=copy; width=%d, dev_width=%ld, line_size=%ld\n",
			  penum->width, dev_width, line_size);
		rproc = image_render_landscape;
		break;
	  }
#endif		/****** END *******/
	  default:
	    return 0;
	  }
	/* We don't want to spread the samples, */
	/* but we have to reset bps to prevent the buffer */
	/* pointer from being incremented by 8 bytes */
	/* per input byte. */
	penum->unpack = image_unpack_copy;
	penum->bps = 8;
	return rproc;
}

/* Use special (slow) logic for 12-bit source values. */
private irender_proc_t
image_strategy_frac(gs_image_enum *penum)
{	if ( penum->bps > 8 )
	  {	if_debug0('b', "[b]render=frac\n");
		return image_render_frac;
	  }
	return 0;
}
