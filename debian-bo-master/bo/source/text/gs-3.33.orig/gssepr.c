/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gssepr.c */
/* Separation color handling for Ghostscript */

typedef ulong gs_separation;			/* BOGUS */
#define gs_no_separation ((gs_separation)(-1L))

#define dev_proc_lookup_separation(proc)\
  gs_separation proc(P4(gx_device *dev, const byte *sname, uint len,\
    gx_color_value *num_levels))

#define dev_proc_map_tint_color(proc)\
  gx_color_index proc(P4(gx_device *dev, gs_separation sepr, bool overprint,\
    gx_color_value tint))

/*
 * In principle, setting a Separation color space, or setting the device
 * when the current color space is a Separation space, calls the
 * lookup_separation device procedure to obtain the separation ID and
 * the number of achievable levels.  Currently, the only hooks for doing
 * this are unsuitable: gx_set_cmap_procs isn't called when the color
 * space changes, and doing it in gx_remap_Separation is inefficient.
 * Probably the best approach is to call gx_set_cmap_procs whenever the
 * color space changes.  In fact, if we do this, we can probably short-cut
 * two levels of procedure call in color remapping (gx_remap_color, by
 * turning it into a macro, and gx_remap_DeviceXXX, by calling the
 * cmap_proc procedure directly).  Some care will be required for the
 * implicit temporary resetting of the color space in [color]image.
 *
 * For actual remapping of Separation colors, we need cmap_separation_direct
 * and cmap_separation_halftoned, just as for the other device color spaces.
 * So we need to break apart gx_render_gray in gxdither.c so it can also
 * do the job for separations.
 */
