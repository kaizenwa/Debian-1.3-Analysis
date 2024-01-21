/*
 * info.cc -- handle the info window
 */
#include "picsim.hh"

static GC			 info_text_gc = None;
static GC			 info_wdt_gc = None;
static GC			 info_bits_gc = None;
static GC			 info_led_gc = None;
static XFontStruct	*info_big_font = None;
static XFontStruct	*info_little_font = None;

/*
 * Refresh the Info window -- display all the static text strings.
 * Call info_update to draw the variable portions.
 */
void
info_refresh( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	XGCValues	 xgcv;
	Dimension	 w_height, w_width;
	int			 i, j, k, bl, dv;

	if (info_text_gc == None) {	// allocate GCs the first time around
		XtVaGetValues( W.info,
					  XtNforeground, &xgcv.foreground,
					  XtNbackground, &xgcv.background,
					  XtNfont, &info_big_font,
					  NULL );
		xgcv.font = info_big_font->fid;
		info_text_gc = XtGetGC( W.info,
							   GCForeground | GCBackground | GCFont,
							   &xgcv );
		info_wdt_gc = XCreateGC( XtDisplay(W.info), XtWindow(W.info),
								GCBackground,
								&xgcv );
		info_little_font = Options.info_bits;
		xgcv.font = info_little_font->fid;
		info_bits_gc = XtGetGC( W.info,
							   GCForeground | GCBackground | GCFont,
							   &xgcv );
		xgcv.line_width = (  info_big_font->ascent
						   + info_big_font->descent
						   - info_little_font->ascent
						   - 2);
		if (xgcv.line_width < 0) xgcv.line_width = 0;
		info_led_gc = XCreateGC( XtDisplay(W.info), XtWindow(W.info),
								GCLineWidth, &xgcv );
	}
	
	XtVaGetValues( W.info,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  NULL );

	bl = 2 + info_big_font->ascent;
	dv = info_big_font->ascent + info_big_font->descent;

	XDrawImageString( XtDisplay(W.info), XtWindow(W.info), info_text_gc,
					 2, bl, "Time", 4 );
	bl += dv;
	XDrawImageString( XtDisplay(W.info), XtWindow(W.info), info_text_gc,
					 2, bl, "Ticks", 5 );
	bl += dv;
	XDrawImageString( XtDisplay(W.info), XtWindow(W.info), info_text_gc,
					 2, bl, "WDT", 3 );

	i = XTextWidth( info_big_font, "WDT", 3 ) + 2 + 10; // WDT bar start X
	j = w_width - i - 10;								// wdt bar length
	
	XDrawRectangle( XtDisplay(W.info), XtWindow(W.info), info_text_gc,
				   i, bl - info_big_font->ascent,
				   j, info_big_font->ascent + 1 );

	xgcv.foreground = Options.wdt_color_1;
	XChangeGC( XtDisplay(W.info), info_wdt_gc, GCForeground, &xgcv );
	XFillRectangle( XtDisplay(W.info), XtWindow(W.info), info_wdt_gc,
				   i + 1, bl - info_big_font->ascent + 1, 
				   j - 1, info_big_font->ascent );

	k = (int)((float)j * (Pic.wdt_max - Pic.wdt_min) / Pic.wdt_max);

	if (k) {
		xgcv.foreground = Options.wdt_color_2;
		XChangeGC( XtDisplay(W.info), info_wdt_gc, GCForeground, &xgcv );
		XFillRectangle( XtDisplay(W.info), XtWindow(W.info), info_wdt_gc,
					   i + 1, bl - info_big_font->ascent + 1,
					   k, info_big_font->ascent );
	}

	k = (int)((float)j * (Pic.wdt_max - Pic.wdt_nominal) / Pic.wdt_max);
	if (k) {
		xgcv.foreground = Options.wdt_color_3;
		XChangeGC( XtDisplay(W.info), info_wdt_gc, GCForeground, &xgcv );
		XFillRectangle( XtDisplay(W.info), XtWindow(W.info), info_wdt_gc,
					   i + 1, bl - info_big_font->ascent + 1,
					   k, info_big_font->ascent );
	}

	bl += dv;
	
	if (Pic.simulate != NULL) {
		info_reg	*regs;
		SimAR		 a = Pic.simulate( GetInfo, a );
		if ((regs = (info_reg *)a.p) != NULL) {
			char	*cp;
			while (cp = regs->reg_name) {
				regs->old_value = ~regs->value;
				XDrawImageString( XtDisplay(W.info), XtWindow(W.info),
								 info_text_gc, 2, bl, cp, strlen( cp ) );
				i = XTextWidth( info_big_font, cp, strlen( cp ) );
				j = bl - info_big_font->ascent * 2 / 3;
				XDrawLine( XtDisplay(W.info), XtWindow(W.info), info_text_gc,
						  i + 4, j, w_width - 10, j );
				i = (w_width - 10) / 8;
				for (j = 0; j < 8; j++) {
					k = XTextWidth( info_little_font,
								   regs->bit_names[j],
								   strlen( regs->bit_names[j] ) ) / 2;
					XDrawImageString( XtDisplay(W.info), XtWindow(W.info),
									 info_bits_gc,
									 i / 2 + i * j - k + 5,
									 (  bl
									  + info_big_font->descent
									  + info_little_font->ascent),
									 regs->bit_names[j],
									 strlen( regs->bit_names[j] ) );
				}
				bl += dv << 1;
				regs += 1;
			}
		}
	}

	info_update( );
}

void
info_update( void )
{
	XGCValues	 xgcv;
	Dimension	 w_height, w_width;
	int			 i, j, k, bl, dv;

	info_quick( );

	XtVaGetValues( W.info,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  NULL );

	dv = info_big_font->ascent + info_big_font->descent;
	bl = 2 + info_big_font->ascent + dv * 3;

	if (Pic.simulate != NULL) {
		info_reg	*regs;
		SimAR		 a = Pic.simulate( GetInfo, a );
		if ((regs = (info_reg *)a.p) != NULL) {
			char	*cp;
			i = (w_width - 10) / 8;
			while (cp = regs->reg_name) {
				bl += dv;

				xgcv.foreground = Options.led_on;
				XChangeGC(XtDisplay(W.info),info_led_gc,GCForeground,&xgcv);

				for (j = 0; j < 8; j++) {
					k = 0x80 >> j;
					if (!(regs->value & k) || (regs->old_value & k)) continue;
					k = i * 8 / 10;
					XDrawLine( XtDisplay(W.info), XtWindow(W.info),
							  info_led_gc,
							  i/2 + i*j - k/2 + 5, bl,
							  i/2 + i*j + k/2 + 5, bl );
				}

				xgcv.foreground = Options.led_off;
				XChangeGC(XtDisplay(W.info),info_led_gc,GCForeground,&xgcv);

				for (j = 0; j < 8; j++) {
					k = 0x80 >> j;
					if ((regs->value & k) || !(regs->old_value & k)) continue;
					k = i * 8 / 10;
					XDrawLine( XtDisplay(W.info), XtWindow(W.info),
							  info_led_gc,
							  i/2 + i*j - k/2 + 5, bl,
							  i/2 + i*j + k/2 + 5, bl );
				}

				regs->old_value = regs->value;
				bl += dv;
				regs += 1;
			}
		}
	}
}

/*
 * Update the elapsed time, ticks, and WDT items
 */
void
info_quick( void )
{
	XGCValues	 xgcv;
	Dimension	 w_height, w_width;
	int			 i, j, k, bl, dv;
	u_long		 ms, sec, min, hr;
	char		 buf[32];

	XtVaGetValues( W.info,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  NULL );

	bl = 2 + info_big_font->ascent;
	dv = info_big_font->ascent + info_big_font->descent;

	ms = Pic.millisec;
	hr = ms / (1000 * 60 * 60); ms -= hr * 100 * 60 * 60;
	min = ms / (1000 * 60); ms -= min * 1000 * 60;
	sec = ms / (1000); ms -= sec * 1000;
	sprintf( buf, "%lu:%02lu:%02lu.%03lu", hr, min, sec, ms );
	i = strlen( buf );
	j = XTextWidth( info_big_font, buf, i );
	XDrawImageString( XtDisplay(W.info), XtWindow(W.info), info_text_gc,
					 w_width - j - 10, bl, buf, i );
	bl += dv;

	sprintf( buf, "%lu", Pic.ticks );
	i = strlen( buf );
	j = XTextWidth( info_big_font, buf, i );
	XDrawImageString( XtDisplay(W.info), XtWindow(W.info), info_text_gc,
					 w_width - j - 10, bl, buf, i );
	bl += dv;

	i = XTextWidth( info_big_font, "WDT", 3 ) + 2 + 10; // WDT bar start X
	j = w_width - i - 10;								// wdt bar length
	k = (int)((float)j
			  * ((float)Pic.wdt_left * 4.0 / Pic.uinfo.clock)
			  / Pic.wdt_max) + 1;
	if (k < j) {
		XClearArea( XtDisplay(W.info), XtWindow(W.info),
				   i + k, bl - info_big_font->ascent + 1,
				   j - k, info_big_font->ascent, False );
	}
}
