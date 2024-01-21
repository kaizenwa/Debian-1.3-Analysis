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
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/tgif_dbg.h,v 3.0 1996/05/06 16:12:30 william Exp $
 */

#ifndef _TGIF_DBG_H_

#ifdef _TGIF_DBG

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>

extern void tgif_dbg_add_to_log ARGS_DECL((char*));

extern void *tgif_malloc ARGS_DECL((size_t));
extern void *tgif_recalloc ARGS_DECL((void*, size_t));
extern void tgif_free ARGS_DECL((void*));

extern GC Tgif_XCreateGC ARGS_DECL((Display*, Drawable, unsigned long,
       XGCValues*));
extern void Tgif_XFreeGC ARGS_DECL((Display*, GC));

extern XImage *Tgif_XCreateImage ARGS_DECL((Display*, Visual*, unsigned int,
       int, int, char*, unsigned int, unsigned int, int, int));
extern XImage *Tgif_XGetImage ARGS_DECL((Display*, Drawable, int, int,
       unsigned int, unsigned int, unsigned long, int));
extern void Tgif_XDestroyImage ARGS_DECL((XImage*));

extern Cursor Tgif_XCreateFontCursor ARGS_DECL((Display*, unsigned int));
extern Cursor Tgif_XCreatePixmapCursor ARGS_DECL((Display*, Pixmap, Pixmap,
       XColor*, XColor*, unsigned int, unsigned int));
extern void Tgif_XFreeCursor ARGS_DECL((Display*, Cursor));

extern Pixmap Tgif_XCreatePixmap ARGS_DECL((Display*, Drawable, unsigned int,
       unsigned int, unsigned int));
extern Pixmap Tgif_XCreateBitmapFromData ARGS_DECL((Display*, Drawable,
       _Xconst char*, unsigned int, unsigned int));
extern void Tgif_XFreePixmap ARGS_DECL((Display*, Pixmap));

extern char *Tgif_XFetchBytes ARGS_DECL((Display*, int*));
extern Status Tgif_XQueryTree ARGS_DECL((Display*, Window, Window*, Window*,
       Window**, unsigned int*));
extern char *Tgif_XGetAtomName ARGS_DECL((Display*, Atom));
extern void Tgif_XFree ARGS_DECL((void*));

/* -------------------- defines -------------------- */

#ifndef PRTGIF_NO_TGIF_DBG

#ifdef malloc
#undef malloc
#endif /* malloc */
#ifdef realloc
#undef realloc
#endif /* realloc */
#ifdef free
#undef free
#endif /* free */

#define malloc tgif_malloc
#define realloc tgif_realloc
#define free tgif_free

#define XCreateGC Tgif_XCreateGC
#define XFreeGC Tgif_XFreeGC

#define XCreateImage Tgif_XCreateImage
#define XGetImage Tgif_XGetImage
#undef XDestroyImage
#define XDestroyImage Tgif_XDestroyImage

#define XCreateFontCursor Tgif_XCreateFontCursor
#define XCreatePixmapCursor Tgif_XCreatePixmapCursor
#define XFreeCursor Tgif_XFreeCursor

#define XCreatePixmap Tgif_XCreatePixmap
#define XCreateBitmapFromData Tgif_XCreateBitmapFromData
#define XFreePixmap Tgif_XFreePixmap

#define XFetchBytes Tgif_XFetchBytes
#define XQueryTree Tgif_XQueryTree
#define XGetAtomName Tgif_XGetAtomName
#define XFree Tgif_XFree

#endif /* ~PRTGIF_NO_TGIF_DBG */

#endif /* _TGIF_DBG */

#endif /* ~_TGIF_DBG_H_ */
