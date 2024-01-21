/*
 * reg.cc -- handle events in the registers window
 */
#include "picsim.hh"

void
reg_refresh( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	GC			 gc;
	XGCValues	 xgcv;
	XFontStruct	*xfsp;
	Dimension	 w_height, w_width;
	short		 c_width;
	int			 a, b, i, j, k, dv;
	u_char		 v;
	char		 buf[32];

	XtVaGetValues( W.rdata,
				  XtNforeground, &xgcv.foreground,
				  XtNbackground, &xgcv.background,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  XtNfont, &xfsp,
				  NULL );

	xgcv.font = xfsp->fid;
	
	gc = XCreateGC( XtDisplay(W.rdata), XtWindow(W.rdata),
				   GCForeground | GCBackground | GCFont,
				   &xgcv );

	/*
	 * count the implemented regsters in the page we're looking at
	 */
	k = 0;
	for (i = 0; i < 128; i++) {
		a = Pic.reg_file[Pic.vreg][i].redirect_page;
		b = Pic.reg_file[Pic.vreg][i].redirect_reg;
		if (Pic.reg_file[a][b].implemented) ++k;
	}
	k = (k + 1) >> 1;

	dv = xfsp->ascent + xfsp->descent;
	c_width = xfsp->max_bounds.width;
	
	for (i = j = 0; i < 128; i++) {
		a = Pic.reg_file[Pic.vreg][i].redirect_page;
		b = Pic.reg_file[Pic.vreg][i].redirect_reg;
		if (!Pic.reg_file[a][b].implemented) continue;
		v = Pic.reg_file[a][b].value & Pic.reg_file[a][b].implemented;
		if (c_width * 16 * 2 <= w_width) {
			if (Pic.reg_file[a][b].name)
				sprintf( buf, "%.6s:0x%02X '%c'",
						Pic.reg_file[a][b].name,
						v,
						((v >= ' ' && v <= '~') ? v : '.'));
			else
				sprintf( buf, "   F%02X:0x%02X '%c'",
						i,
						v,
						((v >= ' ' && v <= '~') ? v : '.'));
		} else {
			if (Pic.reg_file[a][b].name)
				sprintf( buf, "%.6s:0x%02X", Pic.reg_file[a][b].name, v );
			else
				sprintf( buf, "   F%02X:0x%02X", i, v );
		}

		if (Pic.reg_file[a][b].modified_last) {
			Pixel	 temp = xgcv.foreground;
			xgcv.foreground = xgcv.background;
			xgcv.background = temp;
			XChangeGC( XtDisplay(W.rdata), gc,
					  GCForeground | GCBackground, &xgcv );
		}

		XDrawImageString( XtDisplay(W.rdata), XtWindow(W.rdata), gc,
						 (j < k) ? 2 : w_width / 2,
						 (j % k + 1) * dv,
						 buf, strlen( buf ) );

		if (Pic.reg_file[a][b].modified_last) {
			Pixel	 temp = xgcv.foreground;
			xgcv.foreground = xgcv.background;
			xgcv.background = temp;
			XChangeGC( XtDisplay(W.rdata), gc,
					  GCForeground | GCBackground, &xgcv );
		}

		++j;
	}

	XFreeGC( XtDisplay(W.rdata), gc );
}

/*
 * Display changed registers in reverse video, and reset all previously-
 * reversed registers to normal video (if they weren't modified again)
 */
void
reg_update( void )
{
	GC			 gc;
	XGCValues	 xgcv;
	XFontStruct	*xfsp;
	Dimension	 w_height, w_width;
	short		 c_width;
	int			 a, b, i, j, k, dv;
	u_char		 v;
	char		 buf[32];

	XtVaGetValues( W.rdata,
				  XtNforeground, &xgcv.foreground,
				  XtNbackground, &xgcv.background,
				  XtNheight, &w_height,
				  XtNwidth, &w_width,
				  XtNfont, &xfsp,
				  NULL );

	xgcv.font = xfsp->fid;
	
	gc = XCreateGC( XtDisplay(W.rdata), XtWindow(W.rdata),
				   GCForeground | GCBackground | GCFont,
				   &xgcv );

	/*
	 * count the implemented regsters in the page we're looking at
	 */
	k = 0;
	for (i = 0; i < 128; i++) {
		a = Pic.reg_file[Pic.vreg][i].redirect_page;
		b = Pic.reg_file[Pic.vreg][i].redirect_reg;
		if (Pic.reg_file[a][b].implemented) ++k;
	}
	k = (k + 1) >> 1;

	dv = xfsp->ascent + xfsp->descent;
	c_width = xfsp->max_bounds.width;
	
	for (i = j = 0; i < 128; i++) {
		a = Pic.reg_file[Pic.vreg][i].redirect_page;
		b = Pic.reg_file[Pic.vreg][i].redirect_reg;
		if (!Pic.reg_file[a][b].implemented) continue;
		if (!(Pic.reg_file[a][b].modified||Pic.reg_file[a][b].modified_last)) {
			++j;
			continue;
		}
		v = Pic.reg_file[a][b].value & Pic.reg_file[a][b].implemented;
		if (c_width * 16 * 2 <= w_width) {
			if (Pic.reg_file[a][b].name)
				sprintf( buf, "%.6s:0x%02X '%c'",
						Pic.reg_file[a][b].name,
						v,
						((v >= ' ' && v <= '~') ? v : '.'));
			else
				sprintf( buf, "   F%02X:0x%02X '%c'",
						i,
						v,
						((v >= ' ' && v <= '~') ? v : '.'));
		} else {
			if (Pic.reg_file[a][b].name)
				sprintf( buf, "%.6s:0x%02X", Pic.reg_file[a][b].name, v );
			else
				sprintf( buf, "   F%02X:0x%02X", i, v );
		}

		if (Pic.reg_file[a][b].modified) {
			Pixel	 temp = xgcv.foreground;
			xgcv.foreground = xgcv.background;
			xgcv.background = temp;
			XChangeGC( XtDisplay(W.rdata), gc,
					  GCForeground | GCBackground, &xgcv );
		}

		XDrawImageString( XtDisplay(W.rdata), XtWindow(W.rdata), gc,
						 (j < k) ? 2 : w_width / 2,
						 (j % k + 1) * dv,
						 buf, strlen( buf ) );

		if (Pic.reg_file[a][b].modified) {
			Pixel	 temp = xgcv.foreground;
			xgcv.foreground = xgcv.background;
			xgcv.background = temp;
			XChangeGC( XtDisplay(W.rdata), gc,
					  GCForeground | GCBackground, &xgcv );
		}

		Pic.reg_file[a][b].modified_last = Pic.reg_file[a][b].modified;
		Pic.reg_file[a][b].modified = 0;
		
		++j;
	}

	XFreeGC( XtDisplay(W.rdata), gc );
}

void
reg_switch( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	char	 buf[32];
	
	if (*nprm == 1) {
		if (!strcmp( *prms, "next" )) {
			Pic.vreg += 1;
			if (Pic.vreg >= Pic.reg_pages) Pic.vreg = 0;
		} else
		if (!strcmp( *prms, "prev" )) {
			Pic.vreg -= 1;
			if (Pic.vreg < 0) Pic.vreg = Pic.reg_pages - 1;
		} else
			goto reg_switch_usage;

		sprintf( buf, "Page %d", Pic.vreg );
		XtVaSetValues( W.rpage, XtNlabel, (XtPointer)buf, NULL );
		reg_refresh( W.rdata, ev, prms, nprm );
		return;
	}

  reg_switch_usage:
	fputs( "usage: reg-switch( [ next | prev ] )\n", stderr );
}
