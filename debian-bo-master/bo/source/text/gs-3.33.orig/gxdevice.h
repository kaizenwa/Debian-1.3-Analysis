/* Copyright (C) 1989, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gxdevice.h */
/* Device description structure for Ghostscript library */

#ifndef gxdevice_INCLUDED
#  define gxdevice_INCLUDED

#include "gsmatrix.h"
#include "gsxfont.h"
#include "gxbitmap.h"
#include "gxdcolor.h"

/* See drivers.doc for documentation of the driver interface. */
#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif

/* We need at least an abstract type for a graphics state, */
/* which is passed to the page device procedures. */
#ifndef gs_state_DEFINED
#  define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif

/* Define a type for telling get_alpha_bits what kind of object */
/* is being rendered. */
typedef enum {
	go_text,
	go_graphics
} graphics_object_type;

/* Define the type for gray or RGB values at the driver interface. */
typedef unsigned short gx_color_value;
#define arch_sizeof_gx_color_value arch_sizeof_short
/* We might use less than the full range someday. */
/* ...bits must lie between 8 and 16. */
#define gx_color_value_bits (sizeof(gx_color_value) * 8)
#define gx_max_color_value ((gx_color_value)((1L << gx_color_value_bits) - 1))
#define gx_color_value_to_byte(cv)\
  ((cv) >> (gx_color_value_bits - 8))
#define gx_color_value_from_byte(cb)\
  (((cb) << (gx_color_value_bits - 8)) + ((cb) >> (16 - gx_color_value_bits)))

/* Define the structure for device color capabilities. */
typedef struct gx_device_color_info_s {
	int num_components;		/* 1 = gray only, 3 = RGB, */
					/* 4 = CMYK */
	int depth;			/* # of bits per pixel */
	gx_color_value max_gray;	/* # of distinct gray levels -1 */
	gx_color_value max_color;	/* # of distinct color levels -1 */
					/* (only relevant if num_comp. > 1) */
	gx_color_value dither_grays;	/* size of gray ramp for dithering */
	gx_color_value dither_colors;	/* size of color cube ditto */
					/* (only relevant if num_comp. > 1) */
} gx_device_color_info;
#define dci_values(nc,depth,mg,mc,dg,dc) { nc, depth, mg, mc, dg, dc }
#define dci_std_color(color_bits)\
  dci_values(\
    (color_bits == 32 ? 4 : color_bits > 1 ? 3 : 1),\
    ((color_bits > 1) & (color_bits < 8) ? 8 : color_bits),\
    (color_bits >= 8 ? 255 : 1),\
    (color_bits >= 8 ? 255 : color_bits > 1 ? 1 : 0),\
    (color_bits >= 8 ? 5 : 2),\
    (color_bits >= 8 ? 5 : color_bits > 1 ? 2 : 0)\
  )
#define dci_black_and_white dci_std_color(1)
#define dci_black_and_white_() dci_black_and_white
#define dci_color(depth,maxv,dither)\
  dci_values(3, depth, maxv, maxv, dither, dither)
#define gx_device_has_color(dev) ((dev)->color_info.num_components > 1)

/* Structure for device procedures. */
typedef struct gx_device_procs_s gx_device_procs;

/* Structure for page device procedures. */
/* Note that these take the graphics state as a parameter. */
typedef struct gx_page_device_procs_s {

#define dev_page_proc_install(proc)\
  int proc(P2(gx_device *dev, gs_state *pgs))
	dev_page_proc_install((*install));

#define dev_page_proc_begin_page(proc)\
  int proc(P2(gx_device *dev, gs_state *pgs))
	dev_page_proc_begin_page((*begin_page));

#define dev_page_proc_end_page(proc)\
  int proc(P3(gx_device *dev, int reason, gs_state *pgs))
	dev_page_proc_end_page((*end_page));

} gx_page_device_procs;
/* Default procedures */
dev_page_proc_install(gx_default_install);
dev_page_proc_begin_page(gx_default_begin_page);
dev_page_proc_end_page(gx_default_end_page);

/* ---------------- Device structure ---------------- */

/*
 * Define the generic device structure.  The device procedures can
 * have two different configurations:
 * 
 *	- Statically initialized devices predating release 2.8.1
 *	set the static_procs pointer to point to a separate procedure record,
 *	and do not initialize std_procs.
 *
 *	- Statically initialized devices starting with release 2.8.1,
 *	and all dynamically created device instances,
 *	set the static_procs pointer to 0, and initialize std_procs.
 *
 * The gx_device_set_procs procedure converts the first of these to
 * the second, which is what all client code starting in 2.8.1 expects
 * (using the std_procs record, not the static_procs pointer, to call the
 * driver procedures).
 *
 * The choice of the name Margins (rather than, say, HWOffset), and the
 * specification in terms of a default device resolution rather than
 * 1/72" units, are due to Adobe.
 *
 * The xxx_set members are interpreted as follows:
 *	-1 : device does not support this parameter.
 *	0 : device supports the parameter, but no value has been specified.
 *	1 : a value has been specified for the parameter.
 */
#define gx_device_common\
	int params_size;		/* OBSOLETE */\
					/* size of this structure */\
	gx_device_procs *static_procs;	/* OBSOLETE */\
					/* pointer to std_procs */\
	const char *dname;		/* the device name */\
	gs_memory_t *memory;		/* (0 if statically allocated instance) */\
	bool is_open;			/* true if device has been opened */\
	struct {\
		gx_color_index white;	/* (cache) device white color */\
		gx_color_index black;	/* (cache) device black color */\
	} cached;\
	gx_device_color_info color_info;	/* color information */\
	int width;			/* width in pixels */\
	int height;			/* height in pixels */\
	float PageSize[2];		/* page dimensions in points */\
	float ImagingBBox[4];		/* imageable region in points */\
	  bool ImagingBBox_set;\
	float HWResolution[2];		/* resolution, dots per inch */\
	float Margins_HWResolution[2];	/* resolution for Margins */\
	float Margins[2];		/* offset of (0,0) from page corner, */\
					/* in units given by Margins_HWResolution */\
	float HWMargins[4];		/* margins around imageable area, */\
					/* in points */\
	long PageCount;			/* number of pages written */\
	long ShowpageCount;		/* number of calls on showpage */\
	int Orientation;\
	  int Orientation_set;\
	bool Duplex;\
	  int Duplex_set;\
	gx_page_device_procs page_procs;	/* must be last */\
		/* end of std_device_body */\
	gx_device_procs std_procs	/* standard procedures */
/* Define an enumerated type for the optional parameters. */
typedef enum {
	param_Orientation,
	param_Duplex
} param_enum;
#define no_options_(p) (-1)
#define x_pixels_per_inch HWResolution[0]
#define y_pixels_per_inch HWResolution[1]
#define offset_margin_values(x, y, left, bot, right, top)\
  {x, y}, {left, bot, right, top}
#define margin_values(left, bot, right, top)\
  offset_margin_values(0, 0, left, bot, right, top)
#define no_margins margin_values(0, 0, 0, 0)
#define no_margins_() no_margins
/* Define macros that give the page offset in inches. */
#define dev_x_offset(dev) ((dev)->Margins[0] / (dev)->Margins_HWResolution[0])
#define dev_y_offset(dev) ((dev)->Margins[1] / (dev)->Margins_HWResolution[1])
/* Note that left/right/top/bottom are defined relative to */
/* the physical paper, not the coordinate system. */
/* For backward compatibility, we define macros that give */
/* the margins in inches. */
#define dev_l_margin(dev) ((dev)->HWMargins[0] / 72.0)
#define dev_b_margin(dev) ((dev)->HWMargins[1] / 72.0)
#define dev_r_margin(dev) ((dev)->HWMargins[2] / 72.0)
#define dev_t_margin(dev) ((dev)->HWMargins[3] / 72.0)
/* The extra () are to prevent premature expansion. */
#define open_init_closed()\
  0 /*false*/, {gx_no_color_index, gx_no_color_index}
#define open_init_open()\
  1 /*true*/, {1, 0}
/* Accessors for device procedures */
#define dev_proc(dev, p) ((dev)->std_procs.p)
#define set_dev_proc(dev, p, proc) ((dev)->std_procs.p = (proc))
#define fill_dev_proc(dev, p, dproc)\
  if ( dev_proc(dev, p) == 0 ) set_dev_proc(dev, p, dproc)

/*
 * To insulate statically defined device templates from the
 * consequences of changes in the device structure, the following macros
 * must be used for generating initialized device structures.
 *
 * The computations of page width and height in pixels should really be
 *	((int)(page_width_inches*x_dpi))
 * but some compilers (the Ultrix 3.X pcc compiler and the HPUX compiler)
 * can't cast a computed float to an int.  That's why we specify
 * the page width and height in inches/10 instead of inches.
 *
 * In order to conditionally initialize an open-ended set of parameters,
 * we pass each optional parameter identifier (of type param_enum) to a
 * macro, which is defined as a conditional expression that returns either
 * a non-negative initial value or -1.
 *
 * Note that the macro is broken up so as to be usable for devices that
 * add further initialized state to the generic device.
 * Note also that the macro does not initialize std_procs, which is
 * the next element of the structure.
 */
#define std_device_part1_(devtype, ptr_procs, dev_name, open_init)\
	sizeof(devtype), ptr_procs, dev_name,\
	0 /*memory*/, open_init() /*is_open, cached*/
/* color_info goes here */
#define std_device_part2_(width, height, x_dpi, y_dpi)\
	width, height,\
	{ (width) * 72.0 / (x_dpi), (height) * 72.0 / (y_dpi) },\
	{ 0, 0, 0, 0 }, 0/*false*/, { x_dpi, y_dpi }, { x_dpi, y_dpi }
/* offsets and margins go here */
#define std_device_part3_(options_macro)\
	0, 0,\
	options_macro(param_Orientation),\
	  (options_macro(param_Orientation) >= 0 ? 0 : -1),\
	options_macro(param_Duplex) > 0,\
	  (options_macro(param_Duplex) >= 0 ? 0 : -1),\
	{ gx_default_install, gx_default_begin_page, gx_default_end_page }
/*
 * We need a number of different variants of the std_device_ macro simply
 * because we can't pass the color_info, offsets/margins, or optional
 * parameters as macro arguments, which in turn is because of the early macro
 * expansion issue noted in stdpre.h.  The basic variants are:
 *	...body_with_macros_, which uses 0-argument macros to supply
 *	  open_init, color_info, and offsets/margins, and a 1-argument macro
 *	  to supply the optional parameters;
 *	...full_[options_]body, which takes 12 values (6 for dci_values,
 *	  6 for offsets/margins);
 *	...color_full_body, which takes 9 values (3 for dci_color,
 *	  6 for margins/offset).
 *	...std_color_full_[options_]body, which takes 7 values (1 for
 *	  dci_std_color, 6 for margins/offset).
 *	
 */ 
#define std_device_body_with_macros_(dtype, pprocs, dname, w, h, xdpi, ydpi, open_init, dci_macro, margins_macro, options_macro)\
	std_device_part1_(dtype, pprocs, dname, open_init),\
	dci_macro(),\
	std_device_part2_(w, h, xdpi, ydpi),\
	margins_macro(),\
	std_device_part3_(options_macro)

#define std_device_std_body(dtype, pprocs, dname, w, h, xdpi, ydpi)\
	std_device_body_with_macros_(dtype, pprocs, dname,\
	  w, h, xdpi, ydpi,\
	  open_init_closed, dci_black_and_white_, no_margins_, no_options_)

#define std_device_std_body_open(dtype, pprocs, dname, w, h, xdpi, ydpi)\
	std_device_body_with_macros_(dtype, pprocs, dname,\
	  w, h, xdpi, ydpi,\
	  open_init_open, dci_black_and_white_, no_margins_, no_options_)

#define std_device_full_options_body(dtype, pprocs, dname, w, h, xdpi, ydpi, ncomp, depth, mg, mc, dg, dc, xoff, yoff, lm, bm, rm, tm, opt)\
	std_device_part1_(dtype, pprocs, dname, open_init_closed),\
	dci_values(ncomp, depth, mg, mc, dg, dc),\
	std_device_part2_(w, h, xdpi, ydpi),\
	offset_margin_values(xoff, yoff, lm, bm, rm, tm),\
	std_device_part3_(opt)

#define std_device_full_body(dtype, pprocs, dname, w, h, xdpi, ydpi, ncomp, depth, mg, mc, dg, dc, xoff, yoff, lm, bm, rm, tm)\
	std_device_full_options_body(dtype, pprocs, dname, w, h, xdpi, ydpi, ncomp, depth, mg, mc, dg, dc, xoff, yoff, lm, bm, rm, tm, no_options_)

#define std_device_dci_body(dtype, pprocs, dname, w, h, xdpi, ydpi, ncomp, depth, mg, mc, dg, dc)\
	std_device_full_body(dtype, pprocs, dname,\
	  w, h, xdpi, ydpi,\
	  ncomp, depth, mg, mc, dg, dc,\
	  0, 0, 0, 0, 0, 0)

#define std_device_color_full_body(dtype, pprocs, dname, w, h, xdpi, ydpi, depth, max_value, dither, xoff, yoff, lm, bm, rm, tm)\
	std_device_part1_(dtype, pprocs, dname, open_init_closed),\
	dci_color(depth, max_value, dither),\
	std_device_part2_(w, h, xdpi, ydpi),\
	offset_margin_values(xoff, yoff, lm, bm, rm, tm),\
	std_device_part3_(no_options_)

#define std_device_color_body(dtype, pprocs, dname, w, h, xdpi, ydpi, depth, max_value, dither)\
	std_device_color_full_body(dtype, pprocs, dname,\
	  w, h, xdpi, ydpi,\
	  depth, max_value, dither,\
	  0, 0, 0, 0, 0, 0)

#define std_device_std_color_full_options_body(dtype, pprocs, dname, w, h, xdpi, ydpi, depth, xoff, yoff, lm, bm, rm, tm, opt)\
	std_device_part1_(dtype, pprocs, dname, open_init_closed),\
	dci_std_color(depth),\
	std_device_part2_(w, h, xdpi, ydpi),\
	offset_margin_values(xoff, yoff, lm, bm, rm, tm),\
	std_device_part3_(opt)

#define std_device_std_color_full_body(dtype, pprocs, dname, w, h, xdpi, ydpi, depth, xoff, yoff, lm, bm, rm, tm)\
	std_device_std_color_full_options_body(dtype, pprocs, dname, w, h, xdpi, ydpi, depth, xoff, yoff, lm, bm, rm, tm, no_options_)

/* ---------------- Device procedures ---------------- */

/* Define an opaque type for parameter lists. */
#ifndef gs_param_list_DEFINED
#  define gs_param_list_DEFINED
typedef struct gs_param_list_s gs_param_list;
#endif

/*
 * Definition of device procedures.
 * Note that the gx_device * argument is not declared const,
 * because many drivers maintain dynamic state in the device structure.
 * Note also that the structure is defined as a template, so that
 * we can instantiate it with device subclasses.
 * Because C doesn't have real templates, we must do this with a macros.
 */

/* Define macros for declaring device procedures. */

#define dev_t_proc_open_device(proc, dev_t)\
  int proc(P1(dev_t *dev))
#define dev_proc_open_device(proc)\
  dev_t_proc_open_device(proc, gx_device)

#define dev_t_proc_get_initial_matrix(proc, dev_t)\
  void proc(P2(dev_t *dev, gs_matrix *pmat))
#define dev_proc_get_initial_matrix(proc)\
  dev_t_proc_get_initial_matrix(proc, gx_device)

#define dev_t_proc_sync_output(proc, dev_t)\
  int proc(P1(dev_t *dev))
#define dev_proc_sync_output(proc)\
  dev_t_proc_sync_output(proc, gx_device)

#define dev_t_proc_output_page(proc, dev_t)\
  int proc(P3(dev_t *dev, int num_copies, int flush))
#define dev_proc_output_page(proc)\
  dev_t_proc_output_page(proc, gx_device)

#define dev_t_proc_close_device(proc, dev_t)\
  int proc(P1(dev_t *dev))
#define dev_proc_close_device(proc)\
  dev_t_proc_close_device(proc, gx_device)

#define dev_t_proc_map_rgb_color(proc, dev_t)\
  gx_color_index proc(P4(dev_t *dev,\
    gx_color_value red, gx_color_value green, gx_color_value blue))
#define dev_proc_map_rgb_color(proc)\
  dev_t_proc_map_rgb_color(proc, gx_device)

#define dev_t_proc_map_color_rgb(proc, dev_t)\
  int proc(P3(dev_t *dev,\
    gx_color_index color, gx_color_value rgb[3]))
#define dev_proc_map_color_rgb(proc)\
  dev_t_proc_map_color_rgb(proc, gx_device)

#define dev_t_proc_fill_rectangle(proc, dev_t)\
  int proc(P6(dev_t *dev,\
    int x, int y, int width, int height, gx_color_index color))
#define dev_proc_fill_rectangle(proc)\
  dev_t_proc_fill_rectangle(proc, gx_device)

#define dev_t_proc_tile_rectangle(proc, dev_t)\
  int proc(P10(dev_t *dev,\
    const gx_tile_bitmap *tile, int x, int y, int width, int height,\
    gx_color_index color0, gx_color_index color1,\
    int phase_x, int phase_y))
#define dev_proc_tile_rectangle(proc)\
  dev_t_proc_tile_rectangle(proc, gx_device)

#define dev_t_proc_copy_mono(proc, dev_t)\
  int proc(P11(dev_t *dev,\
    const unsigned char *data, int data_x, int raster, gx_bitmap_id id,\
    int x, int y, int width, int height,\
    gx_color_index color0, gx_color_index color1))
#define dev_proc_copy_mono(proc)\
  dev_t_proc_copy_mono(proc, gx_device)

#define dev_t_proc_copy_color(proc, dev_t)\
  int proc(P9(dev_t *dev,\
    const unsigned char *data, int data_x, int raster, gx_bitmap_id id,\
    int x, int y, int width, int height))
#define dev_proc_copy_color(proc)\
  dev_t_proc_copy_color(proc, gx_device)

#define dev_t_proc_draw_line(proc, dev_t)\
  int proc(P6(dev_t *dev,\
    int x0, int y0, int x1, int y1, gx_color_index color))
#define dev_proc_draw_line(proc)\
  dev_t_proc_draw_line(proc, gx_device)

		/* Added in release 2.4 */

#define dev_t_proc_get_bits(proc, dev_t)\
  int proc(P4(dev_t *dev,\
    int y, unsigned char *data, unsigned char **actual_data))
#define dev_proc_get_bits(proc)\
  dev_t_proc_get_bits(proc, gx_device)

		/* Added in release 2.4, changed in 2.8, */
		/* renamed in 2.9.6 */

#define dev_t_proc_get_params(proc, dev_t)\
  int proc(P2(dev_t *dev, gs_param_list *plist))
#define dev_proc_get_params(proc)\
  dev_t_proc_get_params(proc, gx_device)

#define dev_t_proc_put_params(proc, dev_t)\
  int proc(P2(dev_t *dev, gs_param_list *plist))
#define dev_proc_put_params(proc)\
  dev_t_proc_put_params(proc, gx_device)

		/* Added in release 2.6 */

#define dev_t_proc_map_cmyk_color(proc, dev_t)\
  gx_color_index proc(P5(dev_t *dev,\
    gx_color_value cyan, gx_color_value magenta, gx_color_value yellow,\
    gx_color_value black))
#define dev_proc_map_cmyk_color(proc)\
  dev_t_proc_map_cmyk_color(proc, gx_device)

#define dev_t_proc_get_xfont_procs(proc, dev_t)\
  gx_xfont_procs *proc(P1(dev_t *dev))
#define dev_proc_get_xfont_procs(proc)\
  dev_t_proc_get_xfont_procs(proc, gx_device)

		/* Added in release 2.6.1 */

#define dev_t_proc_get_xfont_device(proc, dev_t)\
  gx_device *proc(P1(dev_t *dev))
#define dev_proc_get_xfont_device(proc)\
  dev_t_proc_get_xfont_device(proc, gx_device)

		/* Added in release 2.7.1 */

#define dev_t_proc_map_rgb_alpha_color(proc, dev_t)\
  gx_color_index proc(P5(dev_t *dev,\
    gx_color_value red, gx_color_value green, gx_color_value blue,\
    gx_color_value alpha))
#define dev_proc_map_rgb_alpha_color(proc)\
  dev_t_proc_map_rgb_alpha_color(proc, gx_device)

		/* Added in release 2.8.1 */

#define dev_t_proc_get_page_device(proc, dev_t)\
  gx_device *proc(P1(dev_t *dev))
#define dev_proc_get_page_device(proc)\
  dev_t_proc_get_page_device(proc, gx_device)

		/* Added in release 3.20 */

#define dev_t_proc_get_alpha_bits(proc, dev_t)\
  int proc(P2(dev_t *dev, graphics_object_type type))
#define dev_proc_get_alpha_bits(proc)\
  dev_t_proc_get_alpha_bits(proc, gx_device)

#define dev_t_proc_copy_alpha(proc, dev_t)\
  int proc(P11(dev_t *dev, const unsigned char *data, int data_x,\
    int raster, gx_bitmap_id id, int x, int y, int width, int height,\
    gx_color_index color, int depth))
#define dev_proc_copy_alpha(proc)\
  dev_t_proc_copy_alpha(proc, gx_device)

/* Define the device procedure vector template proper. */

#define gx_device_proc_struct(dev_t)\
{	dev_t_proc_open_device((*open_device), dev_t);\
	dev_t_proc_get_initial_matrix((*get_initial_matrix), dev_t);\
	dev_t_proc_sync_output((*sync_output), dev_t);\
	dev_t_proc_output_page((*output_page), dev_t);\
	dev_t_proc_close_device((*close_device), dev_t);\
	dev_t_proc_map_rgb_color((*map_rgb_color), dev_t);\
	dev_t_proc_map_color_rgb((*map_color_rgb), dev_t);\
	dev_t_proc_fill_rectangle((*fill_rectangle), dev_t);\
	dev_t_proc_tile_rectangle((*tile_rectangle), dev_t);\
	dev_t_proc_copy_mono((*copy_mono), dev_t);\
	dev_t_proc_copy_color((*copy_color), dev_t);\
	dev_t_proc_draw_line((*draw_line), dev_t);\
	dev_t_proc_get_bits((*get_bits), dev_t);\
	dev_t_proc_get_params((*get_params), dev_t);\
	dev_t_proc_put_params((*put_params), dev_t);\
	dev_t_proc_map_cmyk_color((*map_cmyk_color), dev_t);\
	dev_t_proc_get_xfont_procs((*get_xfont_procs), dev_t);\
	dev_t_proc_get_xfont_device((*get_xfont_device), dev_t);\
	dev_t_proc_map_rgb_alpha_color((*map_rgb_alpha_color), dev_t);\
	dev_t_proc_get_page_device((*get_page_device), dev_t);\
	dev_t_proc_get_alpha_bits((*get_alpha_bits), dev_t);\
	dev_t_proc_copy_alpha((*copy_alpha), dev_t);\
}

/* A generic device procedure record. */
struct gx_device_procs_s gx_device_proc_struct(gx_device);

/* A generic device */
struct gx_device_s {
	gx_device_common;
};
extern_st(st_device);
#define public_st_device()	/* in gsdevice.c */\
  gs_public_st_simple(st_device, gx_device, "gx_device")
#define st_device_max_ptrs 0

/* Enumerate or relocate a pointer to a device. */
/* These take the containing space into account properly. */
gx_device *gx_device_enum_ptr(P1(gx_device *));
gx_device *gx_device_reloc_ptr(P2(gx_device *, gc_state_t *));

/* Define typedefs for some of the device procedures, because */
/* ansi2knr can't handle dev_proc_xxx((*xxx)) in a formal argument list. */
typedef dev_proc_map_rgb_color((*dev_proc_map_rgb_color_t));
typedef dev_proc_map_color_rgb((*dev_proc_map_color_rgb_t));

/* A device that forwards non-display operations to another device */
/* called the "target".  This is used for clipping, banding, image, */
/* and null devices. */
#define gx_device_forward_common\
	gx_device_common;\
	gx_device *target
/* A generic forwarding device. */
typedef struct gx_device_forward_s {
	gx_device_forward_common;
} gx_device_forward;
extern_st(st_device_forward);
#define public_st_device_forward()	/* in gsdevice.c */\
  gs_public_st_composite(st_device_forward, gx_device_forward,\
    "gx_device_forward", device_forward_enum_ptrs, device_forward_reloc_ptrs)
#define st_device_forward_max_ptrs (st_device_max_ptrs + 1)

/* A null device.  This is used to temporarily disable output. */
typedef struct gx_device_null_s {
	gx_device_forward_common;
} gx_device_null;
extern_st(st_device_null);
#define public_st_device_null()	/* in gsdevice.c */\
  gs_public_st_suffix_add0(st_device_null, gx_device_null, "gx_device_null",\
    device_forward_enum_ptrs, device_forward_reloc_ptrs)
#define st_device_null_max_ptrs st_device_forward_max_ptrs
/* Make a null device. */
/* The gs_memory_t argument is 0 if the device is temporary and local, */
/* or the allocator that was used to allocate it if it is a real object. */
void	gs_make_null_device(P2(gx_device_null *, gs_memory_t *));

/* Calculate the raster (number of bytes in a scan line), */
/* with byte or word padding. */
uint	gx_device_raster(P2(const gx_device *dev, int pad_to_word));

/* Adjust the resolution for devices that only have a fixed set of */
/* geometries, so that the apparent size in inches remains constant. */
/* If fit=1, the resolution is adjusted so that the entire image fits; */
/* if fit=0, one dimension fits, but the other one is clipped. */
int	gx_device_adjust_resolution(P4(gx_device *dev, int actual_width, int actual_height, int fit));

/* Set the HWMargins to values defined in inches. */
/* If move_origin is true, also reset the Margins. */
void	gx_device_set_margins(P3(gx_device *dev, const float *margins /*[4]*/,
  bool move_origin));

/* Set the width and height, updating PageSize. */
void gx_device_set_width_height(P3(gx_device *dev, int width, int height));
/* Set the resolution, updating width and height. */
void gx_device_set_resolution(P3(gx_device *dev, floatp x_dpi, floatp y_dpi));
/* Set the PageSize, updating width and height. */
void gx_device_set_page_size(P3(gx_device *dev, floatp page_width, floatp page_height));

/*
 * Macros to help the drawing procedures clip coordinates to
 * fit into the drawing region.  Note that these may modify
 * x, y, w, h, data, data_x, and id.
 */

/* Macro for fill_rectangle and tile_rectangle. */
#define fit_fill_xw(dev, x, y, w, h)\
	if ( (x | y) < 0 )\
	{	if ( x < 0 ) w += x, x = 0;\
		if ( y < 0 ) h += y, y = 0;\
	}\
	if ( x > dev->width - w ) w = dev->width - x
#define fit_fill_xwh(dev, x, y, w, h)\
	fit_fill_xw(dev, x, y, w, h);\
	if ( w <= 0 || h <= 0 ) return 0
#define fit_fill(dev, x, y, w, h)\
	fit_fill_xw(dev, x, y, w, h);\
	if ( y > dev->height - h ) h = dev->height - y;\
	if ( w <= 0 || h <= 0 ) return 0

/* Macro for copy_mono and copy_color. */
#define fit_copy_xw(dev, data, data_x, raster, id, x, y, w, h)\
	if ( (x | y) < 0 )\
	{	if ( x < 0 ) w += x, data_x -= x, x = 0;\
		if ( y < 0 ) h += y, data -= y * raster, id = gx_no_bitmap_id, y = 0;\
	}\
	if ( x > dev->width - w ) w = dev->width - x
#define fit_copy_xwh(dev, data, data_x, raster, id, x, y, w, h)\
	fit_copy_xw(dev, data, data_x, raster, id, x, y, w, h);\
	if ( w <= 0 || h <= 0 ) return 0
#define fit_copy(dev, data, data_x, raster, id, x, y, w, h)\
	fit_copy_xw(dev, data, data_x, raster, id, x, y, w, h);\
	if ( y > dev->height - h ) h = dev->height - y;\
	if ( w <= 0 || h <= 0 ) return 0

/* Default implementations of optional procedures. */
/* Note that the default map_xxx_color routines assume white_on_black. */
dev_proc_open_device(gx_default_open_device);
dev_proc_get_initial_matrix(gx_default_get_initial_matrix);
dev_proc_get_initial_matrix(gx_upright_get_initial_matrix);
dev_proc_sync_output(gx_default_sync_output);
dev_proc_output_page(gx_default_output_page);
dev_proc_close_device(gx_default_close_device);
dev_proc_map_rgb_color(gx_default_w_b_map_rgb_color);
dev_proc_map_color_rgb(gx_default_w_b_map_color_rgb);
#define gx_default_map_rgb_color gx_default_w_b_map_rgb_color
#define gx_default_map_color_rgb gx_default_w_b_map_color_rgb
dev_proc_tile_rectangle(gx_default_tile_rectangle);
dev_proc_copy_color(gx_default_copy_color);
dev_proc_draw_line(gx_default_draw_line);
dev_proc_get_bits(gx_default_get_bits);
dev_proc_get_params(gx_default_get_params);
dev_proc_put_params(gx_default_put_params);
dev_proc_map_cmyk_color(gx_default_map_cmyk_color);
dev_proc_get_xfont_procs(gx_default_get_xfont_procs);
dev_proc_get_xfont_device(gx_default_get_xfont_device);
dev_proc_map_rgb_alpha_color(gx_default_map_rgb_alpha_color);
dev_proc_get_page_device(gx_default_get_page_device);	/* returns NULL */
dev_proc_get_page_device(gx_page_device_get_page_device);  /* returns dev */
dev_proc_get_alpha_bits(gx_default_get_alpha_bits);
dev_proc_copy_alpha(gx_default_copy_alpha);

/* Color mapping routines for black-on-white, gray scale, true RGB, */
/* and true CMYK color. */
dev_proc_map_rgb_color(gx_default_b_w_map_rgb_color);
dev_proc_map_color_rgb(gx_default_b_w_map_color_rgb);
dev_proc_map_rgb_color(gx_default_gray_map_rgb_color);
dev_proc_map_color_rgb(gx_default_gray_map_color_rgb);
dev_proc_map_rgb_color(gx_default_rgb_map_rgb_color);
dev_proc_map_color_rgb(gx_default_rgb_map_color_rgb);
dev_proc_map_cmyk_color(gx_default_cmyk_map_cmyk_color);

/* Default implementations for forwarding devices */
dev_proc_get_initial_matrix(gx_forward_get_initial_matrix);
dev_proc_map_rgb_color(gx_forward_map_rgb_color);
dev_proc_map_color_rgb(gx_forward_map_color_rgb);
dev_proc_get_params(gx_forward_get_params);
dev_proc_put_params(gx_forward_put_params);
dev_proc_map_cmyk_color(gx_forward_map_cmyk_color);
dev_proc_get_xfont_procs(gx_forward_get_xfont_procs);
dev_proc_get_xfont_device(gx_forward_get_xfont_device);
dev_proc_map_rgb_alpha_color(gx_forward_map_rgb_alpha_color);
dev_proc_get_page_device(gx_forward_get_page_device);
dev_proc_get_alpha_bits(gx_forward_get_alpha_bits);

/* Convert the device procedures to the proper form (see above). */
void	gx_device_set_procs(P1(gx_device *));

/* Fill in defaulted procedures in a device procedure record. */
void	gx_device_fill_in_procs(P1(gx_device *));
void	gx_device_forward_fill_in_procs(P1(gx_device_forward *));

/* Forward the color mapping procedures from a device to its target. */
void	gx_device_forward_color_procs(P1(gx_device_forward *));

/* Temporarily install a null device, or a special device such as */
/* a clipping device. */
void gx_device_no_output(P1(gs_state *));
void gx_set_device_only(P2(gs_state *, gx_device *));

/* Close a device. */
int gs_closedevice(P1(gx_device *));

/* ------ Device types ------ */

#define dev_type_proc_initialize(proc)\
  int proc(P1(gx_device *))

typedef struct gx_device_type_s {
	gs_memory_type_ptr_t stype;
	dev_type_proc_initialize((*initialize));
} gx_device_type;

#define device_type(dtname, stype, initproc)\
private dev_type_proc_initialize(initproc);\
const gx_device_type dtname = { &stype, initproc }

dev_type_proc_initialize(gdev_initialize);

#endif					/* gxdevice_INCLUDED */
