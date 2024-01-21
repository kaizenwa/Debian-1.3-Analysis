/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gsdevice.h */
/* Device and page control API */

#ifndef gsdevice_INCLUDED
#  define gsdevice_INCLUDED

#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef gs_param_list_DEFINED
#  define gs_param_list_DEFINED
typedef struct gs_param_list_s gs_param_list;
#endif
int gs_flushpage(P1(gs_state *));
int gs_copypage(P1(gs_state *));
int gs_output_page(P3(gs_state *, int, int));
int gs_copyscanlines(P6(gx_device *, int, byte *, uint, int *, uint *));
gx_device *gs_getdevice(P1(int));
int gs_copydevice(P3(gx_device **, const gx_device *, gs_memory_t *));
int gs_makeimagedevice(P7(gx_device **, const gs_matrix *, uint, uint,
			  const byte *, int, gs_memory_t *));
void gs_nulldevice(P1(gs_state *));
int gs_setdevice(P2(gs_state *, gx_device *));
int gs_setdevice_no_erase(P2(gs_state *, gx_device *)); /* returns 1 */
						/* if erasepage required */
gx_device *gs_currentdevice(P1(const gs_state *));
/* gzstate.h redefines the following: */
#ifndef gs_currentdevice_inline
#  define gs_currentdevice_inline(pgs) gs_currentdevice(pgs)
#endif
const char *gs_devicename(P1(const gx_device *));
void gs_deviceinitialmatrix(P2(gx_device *, gs_matrix *));
int gs_getdeviceparams(P2(gx_device *, gs_param_list *));
int gs_putdeviceparams(P2(gx_device *, gs_param_list *));
int gs_closedevice(P1(gx_device *));

#endif					/* gsdevice_INCLUDED */
