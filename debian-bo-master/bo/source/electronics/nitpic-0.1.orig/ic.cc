/*
 * ic.cc -- functions to handle stuff in the IC window
 */
#include "picsim.hh"

static GC			 ic_gc = None;
static XFontStruct	*ic_font;

void
ic_refresh( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	XGCValues	 xgcv;
	Dimension	 w_height, w_width;
	Dimension	 ic_height, ic_width, pin_len, pin_loc, pin_dv;
	int			 i, j, k;
	pins_info	*p;
	SimAR		 a;

	if (ic_gc == None) {
		Pixel		 fg, bg;
		XtVaGetValues( W.ic,
					  XtNforeground, &fg,
					  XtNbackground, &bg,
					  XtNfont, &ic_font,
					  NULL );

		xgcv.foreground = fg;
		xgcv.background = bg;
		xgcv.font = ic_font->fid;
	
		ic_gc = XCreateGC( XtDisplay(W.ic), XtWindow(W.ic),
						  GCForeground | GCBackground | GCFont,
						  &xgcv );

	}

	a = Pic.simulate( GetPins, a );
	p = (pins_info *)a.p;
	if (!p->npins) return;
	
	XtVaGetValues( W.ic,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  NULL );

	pin_dv = w_height / (p->npins / 2 + 2);
	if (pin_dv > 20) pin_dv = 20;
	ic_height = pin_dv * p->npins / 2;
	ic_width = ic_height * 2 / 3;
	if (ic_width < 8 || ic_width > w_width)
		ic_width = 4 * w_width / 5;

	XDrawRectangle( XtDisplay(W.ic), XtWindow(W.ic), ic_gc,
				   (w_width - ic_width) / 2,
				   (w_height - ic_height) / 2,
				   ic_width,
				   ic_height );

	pin_len = (w_width - ic_width) / 4;
	if (pin_len >16) pin_len = 16;
	pin_loc = (w_height - ic_height + pin_dv) / 2;

	for (i = 0; i < p->npins / 2; i++) {
		XDrawLine( XtDisplay(W.ic), XtWindow(W.ic), ic_gc,
				  (w_width - ic_width) / 2,
				  pin_loc,
				  (w_width - ic_width) / 2 - pin_len,
				  pin_loc );
		XDrawImageString( XtDisplay(W.ic), XtWindow(W.ic), ic_gc,
						 (w_width - ic_width) / 2 + 2,
						 pin_loc + ic_font->ascent / 2,
						 p->pins[i].pin_name,
						 strlen( p->pins[i].pin_name ) );
		XDrawLine( XtDisplay(w), XtWindow(w), ic_gc,
				  (w_width + ic_width) / 2,
				  pin_loc,
				  (w_width + ic_width) / 2 + pin_len,
				  pin_loc );
		j = strlen( p->pins[p->npins - i - 1].pin_name );
		k = XTextWidth( ic_font, p->pins[p->npins - i - 1].pin_name, j );
		XDrawImageString( XtDisplay(W.ic), XtWindow(W.ic), ic_gc,
						 (w_width + ic_width) / 2 - k,
						 pin_loc + ic_font->ascent / 2,
						 p->pins[p->npins - i - 1].pin_name, j );
		pin_loc += pin_dv;
	}

	ic_update( );
}

void
ic_update( void )
{
	XGCValues	 xgcv;
	Dimension	 w_height, w_width;
	Dimension	 ic_height, ic_width, pin_len, pin_loc, pin_dv;
	int			 i, j;
	pins_info	*p;
	SimAR		 a;
	Pixmap		 arrow;

	a = Pic.simulate( GetPins, a );
	p = (pins_info *)a.p;
	if (!p || !p->npins) return;
	
	XtVaGetValues( W.ic,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  NULL );

	pin_dv = w_height / (p->npins / 2 + 2);
	if (pin_dv > 20) pin_dv = 20;
	ic_height = pin_dv * p->npins / 2;
	ic_width = ic_height * 2 / 3;
	if (ic_width < 8 || ic_width > w_width)
		ic_width = 4 * w_width / 5;

	pin_len = (w_width - ic_width) / 4;
	if (pin_len >16) pin_len = 16;
	pin_loc = (w_height - ic_height + pin_dv) / 2;

	for (i = 0; i < p->npins / 2; i++) {
		if (p->pins[i].io == 0) arrow = Options.right_hollow;
		else
		if (p->pins[i].io == 1) arrow = Options.left_pointer;
		else
			arrow = None;

		if (arrow != None) {
			xgcv.foreground = ((p->pins[i].hi)
							   ? Options.led_on
							   : Options.led_off);
			XChangeGC( XtDisplay(W.ic), ic_gc, GCForeground, &xgcv );
			XCopyPlane( XtDisplay(W.ic), arrow, XtWindow(W.ic),
					   ic_gc, 0, 0, 8, 8,
					   (w_width - ic_width) / 2 - pin_len - 2,
					   pin_loc - 3, 1 );
		}

		j = p->npins - i - 1;
		if (p->pins[j].io == 0) arrow = Options.left_hollow;
		else
		if (p->pins[j].io == 1) arrow = Options.right_pointer;
		else
			arrow = None;

		if (arrow != None) {
			xgcv.foreground = ((p->pins[j].hi)
							   ? Options.led_on
							   : Options.led_off);
			XChangeGC( XtDisplay(W.ic), ic_gc, GCForeground, &xgcv );
			XCopyPlane( XtDisplay(W.ic), arrow, XtWindow(W.ic),
					   ic_gc, 0, 0, 8, 8,
					   (w_width + ic_width) / 2 + pin_len + 1,
					   pin_loc - 3, 1 );
		}

		pin_loc += pin_dv;
	}
}
