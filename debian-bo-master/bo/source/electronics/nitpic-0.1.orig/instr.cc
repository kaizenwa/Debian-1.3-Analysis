/*
 * instr.cc -- routines to handle the instr window
 */
#include "picsim.hh"

void
instr_refresh( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	GC			 gc;
	XGCValues	 xgcv;
	XFontStruct	*xfsp;
	Dimension	 w_height, w_width;
	Pixmap		 pointer_pixmap;
	char		 buf[32];
	int			 i, j, k, p, dv;

	XtVaGetValues( W.itext,
				  XtNforeground, &xgcv.foreground,
				  XtNbackground, &xgcv.background,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  XtNfont, &xfsp,
				  NULL );

	xgcv.font = xfsp->fid;
	gc = XCreateGC( XtDisplay(W.itext), XtWindow(W.itext),
				   GCForeground | GCBackground | GCFont,
				   &xgcv );

	p = Pic.itop;
	dv = xfsp->ascent + xfsp->descent;
	
	for (i = dv; i < w_height; i += dv) {
		SimAR	 a;
		
		if (p > Pic.isize) {
			XClearArea( XtDisplay(W.itext), XtWindow(W.itext),
					   0, i, w_width, w_height, False );
			break;
		}
		a.i = p++;
		a = Pic.simulate( Disassemble, a );

		j = strlen( (char *)a.p );
		k = XTextWidth( xfsp, (char *)a.p, j );
		
		XDrawImageString( XtDisplay(W.itext), XtWindow(W.itext), gc,
						 2, i, (char *)(a.p), j );
		XClearArea( XtDisplay(W.itext), XtWindow(W.itext),
				   2 + k, i - xfsp->ascent, w_width, dv, False );
	}

	XFreeGC( XtDisplay(W.itext), gc );
}

/*
 * Scroll the instruction area in response to a btn1 or btn3 click
 */
void
instr_scroll( Widget w, XtPointer client_data, XtPointer position )
{
	Dimension	 w_width, w_height;
	XFontStruct	*xfsp;
	int			 dv, top, lines;
	float		 thumb;

	XtVaGetValues( W.iscroll,
				  XtNheight, &w_height,
				  NULL );

	XtVaGetValues( W.itext,
				  XtNfont, &xfsp,
				  NULL );

	dv = xfsp->ascent + xfsp->descent;

	lines = w_height / dv;

	top = Pic.itop + lines * (int)position / w_height;
	if (top < 0)
		top = 0;
	else
	if (top + lines > Pic.isize)
		top = Pic.isize - lines;

	if (top == Pic.itop) return; /* nothing to do */

	thumb = (float)top / (float)(Pic.isize - lines);

	XawScrollbarSetThumb( W.iscroll, thumb, -1.0 );

	instr_jump( w, client_data, &thumb );
}

/*
 * Scroll the instruction area in response to btn2 in scroll bar
 */
void
instr_jump( Widget w, XtPointer client_data, XtPointer position )
{
	Dimension	 w_width, w_height;
	XFontStruct	*xfsp;
	int			 dv, top, bottom, lines, dlines;
	float		 thumb;

	XtVaGetValues( W.iscroll,
				  XtNheight, &w_height,
				  NULL );

	XtVaGetValues( W.itext,
				  XtNfont, &xfsp,
				  NULL );

	dv = xfsp->ascent + xfsp->descent;

	lines = w_height / dv;

	top = (int)((float)(Pic.isize - lines) * *(float *)position);

	ipointer_erase( );
	Pic.itop = top;
	ipointer_refresh( W.ipointer, NULL, NULL, NULL );
	instr_refresh( W.itext, NULL, NULL, NULL );
}

static unsigned	 ipointer_height = 16;

/*
 * Erase the instruction pointer (if it's visible)
 * Return whether or not it was visible.
 */
Boolean
ipointer_erase( void )
{
	Dimension	 w_width, w_height;
	XFontStruct	*xfsp;
	int			 dv, top, bottom;

	XtVaGetValues( W.itext,
				  XtNfont, &xfsp,
				  NULL );

	dv = xfsp->ascent + xfsp->descent;

	XtVaGetValues( W.ipointer,
				  XtNwidth, &w_width,
				  XtNheight, &w_height,
				  NULL );

	top = (Pic.pc - Pic.itop) * dv - 1;
	bottom = top + ipointer_height;

	if (bottom < 0 || top > w_height) return False;
	
	XClearArea( XtDisplay(W.ipointer), XtWindow(W.ipointer),
			   0, (Pic.pc - Pic.itop) * dv - 2, w_width, ipointer_height,
			   False );

	return True;
}

void
ipointer_refresh( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	GC			 gc;
	XGCValues	 xgcv;
	Dimension	 w_height, w_width;
	static Pixmap pointer_pixmap = None;
	XFontStruct	*xfsp;
	int			 dv;

	XtVaGetValues( W.itext,
				  XtNfont, &xfsp,
				  NULL );

	dv = xfsp->ascent + xfsp->descent;

	XtVaGetValues( W.ipointer,
				  XtNforeground, &xgcv.foreground,
				  XtNbackground, &xgcv.background,
				  NULL );

	if (pointer_pixmap == None) {
		XtVaGetValues( W.ipointer,
					  XtNbitmap, &pointer_pixmap,
					  NULL );
		XtVaSetValues( W.ipointer,
					  XtNbitmap, (XtPointer)None,
					  NULL );

		if (pointer_pixmap != None) {
			Window	 who_cares_win;
			int		 who_cares_int;
			u_int	 who_cares_unsigned;

			XGetGeometry( XtDisplay(W.ipointer), pointer_pixmap,
						 &who_cares_win,
						 &who_cares_int, &who_cares_int,
						 &who_cares_unsigned, &ipointer_height,
						 &who_cares_unsigned, &who_cares_unsigned );
		}
	}

	gc = XCreateGC( XtDisplay(W.ipointer), XtWindow(W.ipointer),
				   GCForeground | GCBackground,
				   &xgcv );

	XCopyPlane( XtDisplay(W.ipointer), pointer_pixmap, XtWindow(W.ipointer),
			  gc, 0, 0, 16, 16, 2, (Pic.pc - Pic.itop) * dv - 1, 1 );

	XFreeGC( XtDisplay(W.ipointer), gc );
}

/*
 * If the instruction pointer is off-screen, scroll so that it's about
 * 1/4 way down.
 */
void
ipointer_seek( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	Dimension	 w_width, w_height;
	XFontStruct	*xfsp;
	int			 dv, top, bottom, lines, dlines;
	float		 thumb;

	XtVaGetValues( W.iscroll,
				  XtNheight, &w_height,
				  NULL );

	XtVaGetValues( W.itext,
				  XtNfont, &xfsp,
				  NULL );

	dv = xfsp->ascent + xfsp->descent;

	lines = w_height / dv;

	if (Pic.pc >= Pic.itop && Pic.pc < Pic.itop + lines - 3) return;

	top = Pic.pc - lines / 4;
	if (top < 0)
		top = 0;
	else
	if (top + lines > Pic.isize)
		top = Pic.isize - lines;
	
	ipointer_erase( );
	Pic.itop = top;
	ipointer_refresh( W.ipointer, NULL, NULL, NULL );
	instr_refresh( W.itext, NULL, NULL, NULL );

	XawScrollbarSetThumb( W.iscroll, (float)top / (float)(Pic.isize - lines),
						 -1.0 );
}
