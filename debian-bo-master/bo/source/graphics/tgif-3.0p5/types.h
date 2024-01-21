/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/types.h,v 3.0 1996/05/06 16:12:41 william Exp $
 */

#ifndef _TGIF_TYPES_H_
#define _TGIF_TYPES_H_

#include "const.h"

typedef struct BBRec {
   int	ltx, lty, rbx, rby;
} * BBRecPtr;

typedef struct PtRec {
   int	x, y;
   struct PtRec	* next;
} * PtRecPtr;

typedef struct {
   int	x, y;
} IntPoint;

typedef struct XfrmMtrxRec {
   int	m[6];
} * XfrmMtrxPtr;

typedef struct ObjRec {
   int			x, y, type, color, id, dirty, hot_spot, invisible;
   int			rotation; /* degrees times 64 */
   short		marked, locked;
   struct BBRec		orig_obbox, obbox; /* object bounding box */
   struct BBRec		bbox; /* real bounding box */
   struct ObjRec	* next, * prev;
   struct AttrRec	* fattr, * lattr; /* first and last attributes */
   union {
      struct GroupRec	* r;
      struct PolyRec	* p;
      struct PolygonRec	* g;
      struct BoxRec	* b;
      struct OvalRec	* o;
      struct TextRec	* t;
      struct ArcRec	* a;
      struct RCBoxRec	* rcb;
      struct XBmRec	* xbm;
      struct XPmRec	* xpm;
   } detail;
   struct ObjRec	* tmp_child; /* used temporarily */
   struct ObjRec	* tmp_parent; /* used temporarily */
   struct XfrmMtrxRec	* ctm;
   char			color_str[40]; /* copy of the color name in file */
   XPoint		rotated_obbox[5]; /* offsets */
} * ObjRecPtr;

typedef struct DynStrRec {
   char	*s;
   int	sz; /* size of the buffer, should be len(s)+1 */
} * DynStrPtr;

typedef struct AttrRec {
   struct DynStrRec	attr_name; /* attribute name */
   struct DynStrRec	attr_value; /* attribute value */
   short		shown; /* TRUE if the attribute is shown */
   short		nameshown; /* TRUE if the attr name is also shown */
   short		inherited; /* TRUE if attr was inherited */
   struct ObjRec	* obj; /* the OBJ_TEXT object that represent the attr */
   struct ObjRec        * owner; /* ptr to owner obj of the record */
   struct AttrRec	* next, * prev; /* next and prev attributes */
} * AttrRecPtr;

typedef struct GroupRec {
   struct ObjRec	* first, * last;
   char			s[MAXPATHLENGTH+1];
   int			rotate, flip, deck_index;
} * GroupRecPtr;

typedef struct PolyRec {
   int			n;	/* number of points in the polyline */
   IntPoint		* vlist; /* absolute */
   char			* smooth;
   int			sn;	/* number of points in the spline polyline */
   XPoint		* svlist; /* offsets */
   int			asn;	/* spline polyline with arrows */
   XPoint		* asvlist; /* offsets */
   int			intn;	/* interpolated spline original spec */
   IntPoint		* intvlist; /* absolute */
   int			style, width, pen, curved, fill, dash, aw, ah, tension;
   char			width_spec[40], aw_spec[40], ah_spec[40];
   int			rotated_n, rotated_asn;
   XPoint		* rotated_vlist, * rotated_asvlist; /* offsets */
} * PolyRecPtr;

typedef struct PolygonRec {
   int			n;	/* number of points in the polygon */
   IntPoint		* vlist; /* absolute */
   char			* smooth;
   int			sn;	/* number of points in the spline polygon */
   XPoint		* svlist; /* offsets */
   int			intn;	/* interpolated spline original spec */
   IntPoint		* intvlist; /* absolute */
   int			fill, width, pen, curved, dash, tension;
   char			width_spec[40];
   int			rotated_n;
   XPoint		* rotated_vlist; /* offsets */
} * PolygonRecPtr;

typedef struct BoxRec {
   int		fill, width, pen, dash;
   char		width_spec[40];
} * BoxRecPtr;

typedef struct OvalRec {
   int		fill, width, pen, dash;
   char		width_spec[40];
   int		rotated_n;
   XPoint	* rotated_vlist; /* offsets */
} * OvalRecPtr;

typedef struct StrRec {
   struct DynStrRec	dyn_str;
   struct StrRec	* next, * prev;
} * StrRecPtr;

typedef struct TextRec {
   int			lines;
   int			just, font, style, size, rotate, pen, fill;
   int			asc, des, v_space;
   int			read_only, orig_w, orig_h; /* only used if read_only */
   int			underline_on, underline, min_lbearing, max_rextra;
   int			double_byte, direction;
   char			* font_name; /* only used in PRTGIF */
   char			* custom_screen_font_name;
   struct AttrRec       * attr; /* ptr to attr record if text obj is an attr */
   struct StrRec	* first, * last;
   XImage		* image;
   Pixmap		cached_bitmap;
   int			cached_zoom, cached_zoomed, cached_rotate;
   struct XfrmMtrxRec	cached_ctm;
   struct BBRec		orig_bbox;
} * TextRecPtr;

typedef struct SelRec {
   struct ObjRec	* obj;
   struct SelRec	* next, * prev;
} * SelRecPtr;

typedef struct VSelRec {
   struct ObjRec	* obj;
   int			n, max_v, * v_index, * x, * y;
   struct VSelRec	* next, * prev;
} * VSelRecPtr;

typedef struct SubCmdRec {
   union {
      struct MoveSubCmdRec	* mv;
   } detail;
} * SubCmdRecPtr;

typedef struct MoveSubCmdRec {
   int	dx, dy;
} * MoveSubCmdRecPtr;

typedef struct CmdRec {
   int			type, dx, dy, undone, include_tgif_obj, new_colormap;
   int			* pos_before, count_before;
   int			* pos_after, count_after;
   struct SelRec	* top_before, * bot_before;
   struct SelRec	* top_after, * bot_after;
   struct CmdRec	* next, * prev;
   struct CmdRec	* first, * last;
} * CmdRecPtr;

typedef struct PageRec {
   struct ObjRec	* top, * bot;
   struct PageRec	* next, * prev;
   char			* name;
   int			draw_orig_x, draw_orig_y, zoom_scale, zoomed_in;
   int			layer_on;
} * PageRecPtr;

typedef struct StkRec {
   struct ObjRec	* first, * last, * sel;
   struct StkRec	* next;
   struct CmdRec	* first_cmd, * last_cmd, * cur_cmd;
   int			history_count;
   int			name_valid, file_mod, id, page_style;
   int			orig_x, orig_y, zoom, zoomed;
   int			grid_system, english_grid, metric_grid, grid_on;
   int			color, h_align, v_align, line_w, line_s;
   int			fill, pen, dash, just, font, f_style, f_size;
   int			print_mag, v_space, grid_shown, move_mode;
   int			text_rotate, rcb_radius, underline_on, underline;
   char			name[MAXPATHLENGTH+1], domain[MAXPATHLENGTH+1];
   char			dir[MAXPATHLENGTH+1], sym_dir[MAXPATHLENGTH+1];
   char			* saved_comments;
   int			saved_comments_len;
   struct AttrRec	* first_file_attr, * last_file_attr;
   struct PageRec	* first_page, * last_page, * cur_page;
   int			cur_page_num, last_page_num, cols, rows;
   int			page_layout_mode, color_dump;
   int			one_page_width, one_page_height;
} * StkRecPtr;

typedef struct ArcRec {
   int		fill, width, pen, dash, style, aw, ah;
   char		width_spec[40], aw_spec[40], ah_spec[40];
   int		xc, yc, x1, y1, x2, y2, dir;
   int		ltx, lty, w, h, angle1, angle2;
   int		a_angle1, a_angle2;
   int		rotated_n, rotated_asn;
   XPoint	* rotated_vlist, * rotated_asvlist; /* offsets */
} * ArcRecPtr;

typedef struct RCBoxRec {
   int		fill, width, pen, dash, radius;
   char		width_spec[40];
   int		rotated_n;
   XPoint	* rotated_vlist; /* offsets */
} * RCBoxRecPtr;

typedef struct XBmRec {
   int			fill, real_type, flip, rotate, image_w, image_h;
   int			eps_w, eps_h;
   char			* data;
   char			* filename;
   char			* * epsflines;
   char			write_date[32];
   int			num_epsf_lines, epsf_level, save_epsf;
   int			llx, lly, urx, ury; /* these values are x1000 */
   XImage		* image;
   Pixmap		bitmap;
   Pixmap		cached_bitmap;
   int			cached_zoom, cached_zoomed, cached_rotate, cached_flip;
   int			cached_w, cached_h;
   struct XfrmMtrxRec	cached_ctm;
} * XBmRecPtr;

typedef struct XPmRec {
   int			fill, flip, rotate, image_w, image_h;
   int			ncolors;
   int			chars_per_pixel;
   int			first_pixel_is_bg;
   int			* pixels, * red, * green, * blue;
   char			* color_char;
   char			* * color_str;
   char			* data;
   XImage		* image, * bitmap_image;
   Pixmap		pixmap, bitmap;
   Pixmap		cached_pixmap, cached_bitmap;
   int			cached_zoom, cached_zoomed, cached_rotate, cached_flip;
   int			cached_w, cached_h, cached_color;
   Pixmap		clip_mask;
   struct XfrmMtrxRec	cached_ctm;
} * XPmRecPtr;

typedef struct WinInfoRec {
   Window	window;
   int		mapped;
   int		raise;
   int		(* ev_handler)();
   void		(* expose_handler)();
   void		(* cleanup)();
} * WinInfoPtr;

typedef struct MtrxRec {
   float	m[2][2], rev_m[2][2], h_scale, v_scale;
   float	image_w, image_h, w, h, transformed_w, transformed_h;
   float	dump_h_scale, dump_v_scale;
   int		rotate, flip, degree;
} * MtrxPtr;

typedef struct _DspList {
   char			itemstr[MAXPATHLENGTH+1];
   char			pathstr[MAXPATHLENGTH+1];
   int			directory;
   struct _DspList	* next;
} DspList;

typedef struct MouseStatusStrRec {
   char	* l, * m, * r;
} MouseStatusStrPtr;

typedef struct VRec {
   int vtype;
   union {
      int i;
      double d;
      char *s;
   } val;
} VRecPtr;

struct URLCacheRec {
   int remote_buf_sz, is_html;
   char *remote_buf, *content_type, *simple_url_name;
   struct URLCacheRec *next, *prev;
};

#endif /*_TGIF_TYPES_H_*/
