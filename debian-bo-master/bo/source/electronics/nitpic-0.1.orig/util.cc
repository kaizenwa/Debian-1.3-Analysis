/*
 * util.cc -- various utility routines
 */
#include "picsim.hh"

void
redraw( void )
{
	ic_refresh( W.ic, NULL, NULL, NULL );
	info_refresh( W.info, NULL, NULL, NULL );
	instr_refresh( W.instr, NULL, NULL, NULL );
	reg_refresh( W.regs, NULL, NULL, NULL );
}

void
load_file( Widget w, XEvent *ev, String *prms, Cardinal *nprm )
{
	dialog( "Filename?", 1, "OK" );
}

static void		 dialog_format(char*text, XFontStruct*finfo, Dimension width);
static Widget	 dialog_box = None;
static Widget	 dialog_form;
static Widget	 dialog_text;
static Widget	*dialog_buttons = NULL;
static int		 dialog_nbuttons = 0;

static void
dialog_ok( Widget w, XtPointer closure, XtPointer call_data )
{
	int	 i;
	
	XtPopdown( dialog_box );

	XtUnrealizeWidget( dialog_box );

	XtUnmanageChildren( dialog_buttons, dialog_nbuttons );
	XtUnmanageChild( dialog_text );
	XtUnmanageChild( dialog_form );
	XtUnmanageChild( dialog_box );

	for (i = 0; i < dialog_nbuttons; i++) XtDestroyWidget( dialog_buttons[i] );

	XtFree( (char *)dialog_buttons );
	dialog_buttons = NULL;

	fprintf( stderr, "button %d\n", closure );
}

int
dialog( const char *text, int n, ... )
{
	va_list			 buttons;
	int				 i, j;
	Dimension		 bwidth;
	Dimension		 bheight;
	Dimension		 twidth;
	Dimension		 theight;
	int				 fgap;
	Widget			 tsink;
	Dimension		 width;
	Dimension		 height;
	Window			 root;
	Window			 child;
	int				 root_x, root_y;
	int				 win_x, win_y;
	unsigned int	 mask;
	char			*fmttext;
	XFontStruct		*finfo;

	fmttext = (char *)XtMalloc( strlen( text ) + 1 );
	strcpy( fmttext, text );

	va_start( buttons, n );

	if (dialog_box == None) {
		dialog_box = XtVaCreateWidget( "dialog",
									  overrideShellWidgetClass,
									  W.toplevel,
									  NULL );
		dialog_form = XtVaCreateWidget( "form", formWidgetClass,
									   dialog_box,
									   NULL );
		dialog_text = XtVaCreateWidget( "text", asciiTextWidgetClass,
									   dialog_form,
									   XtNautoFill, (XtPointer)True,
									   XtNscrollVertical,
									   (XtPointer)XawtextScrollWhenNeeded,
									   XtNeditType,
									   (XtPointer)XawtextRead,
									   XtNdisplayCaret,
									   (XtPointer)False,
									   XtNresizable,
									   (XtPointer)True,
									   NULL );
	}

	XawFormDoLayout( dialog_form, False );

	dialog_buttons = (Widget *)XtMalloc( n * sizeof(Widget) );

	for (i = 0; i < n; i++) {
		Widget	 b;
		char	*name;

		name = va_arg(buttons, char *);

		if (!i)
			b = XtVaCreateWidget( name, commandWidgetClass,
								 dialog_form,
								 XtNlabel, name,
								 XtNfromVert, dialog_text,
								 XtNresizable, (XtPointer)True,
								 NULL );
		else
			b = XtVaCreateWidget( name, commandWidgetClass,
								 dialog_form,
								 XtNlabel, name,
								 XtNfromVert, dialog_text,
								 XtNfromHoriz,
								 (XtPointer)dialog_buttons[i-1],
								 XtNresizable, (XtPointer)True,
								 NULL );
		
		XtAddCallback( b, "callback", dialog_ok, (XtPointer)i );
		dialog_buttons[i] = b;
	}

	dialog_nbuttons = n;

	XtVaGetValues( dialog_buttons[0],
				  XtNwidth, &bwidth,
				  XtNheight, &bheight,
				  NULL );

	XtVaGetValues( dialog_form,
				  XtNdefaultDistance, &fgap,
				  NULL );

	if (n == 1) {
		twidth = bwidth * 2;
		XtVaSetValues( dialog_buttons[0],
					  XtNhorizDistance, twidth / 4 + fgap,
					  NULL );
	} else {
		twidth = (bwidth + 4) * n + 4;
	}
	theight = twidth * 2 / 3;

	XtVaGetValues( dialog_text,
				  XtNtextSink, &tsink,
				  XtNfont, &finfo,
				  NULL );

	dialog_format( fmttext, finfo, twidth );

	for (i = 2, j = 0; fmttext[j]; j++) if (fmttext[j] == '\n') ++i;
	j = XawTextSinkMaxHeight( tsink, i );

	if (j < theight) {
		theight = j;
	} else {
		strcpy( fmttext, text );
		dialog_format( fmttext, finfo, twidth - 18 );
	}

	XtVaSetValues( dialog_text,
				  XtNwidth, twidth,
				  XtNheight, theight,
				  XtNstring, fmttext,
				  NULL );

	XtVaSetValues( dialog_form,
				  XtNwidth, twidth + fgap * 2,
				  XtNheight, theight + bheight + fgap * 3,
				  NULL );

	XawFormDoLayout( dialog_form, True );
	
	XtVaGetValues( dialog_form,
				  XtNwidth, &width,
				  XtNheight, &height,
				  NULL );

	XQueryPointer( XtDisplay(W.toplevel), XtWindow(W.toplevel),
				  &root, &child,
				  &root_x, &root_y, &win_x, &win_y, &mask );

	root_x = (root_x < width / 2) ? 0 : root_x - width / 2;
	root_y = (root_y < height - bheight / 2) ? 0 : root_y-height+bheight/2;

	XtVaSetValues( dialog_box,
				  XtNwidth, width,
				  XtNheight, height,
				  XtNx, root_x,
				  XtNy, root_y,
				  NULL );

	XtManageChild( dialog_text );
	XtManageChildren( dialog_buttons, dialog_nbuttons );
	XtManageChild( dialog_form );
	XtManageChild( dialog_box );
	
	XtRealizeWidget( dialog_box );

	XtPopup( dialog_box, XtGrabExclusive );

	XtFree( fmttext );

	return 0;
}

/*
 * format the text -- put in newlines whenever the current line gets
 * too long.
 */
static void
dialog_format( char *text, XFontStruct *finfo, Dimension width )
{
	int	 i, j, k;
	

	for (i = j = 0, k = 1; text[k]; ++k) {
		if (isspace(text[k])) {
			if (XTextWidth( finfo, text + i, k - i ) >= width) {
				if (isspace(text[j])) {
					text[j++] = '\n';
					i = j;
				} else {
					text[k] = '\n';
				}
			} else {
				j = k;
			}
			if (text[k] == '\n')  i = j = k + 1;
		}
	}
	if (XTextWidth( finfo, text + i, k - i ) >= width && isspace(text[j]))
		text[j] = '\n';
}
