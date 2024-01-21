/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module X-widget.c				     */
/*									     */
/*	The tableau widget for Xsok.					     */
/*	written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#include "X-sok.h"
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "TableauP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(TableauRec, tableau.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    {XtNruleset, XtCRuleset, XtRString,    sizeof(String),    offset(rules),    XtRString,    "Sokoban"},
    {XtNlevel,   XtCLevel,   XtRInt,       sizeof(int),       offset(level),    XtRImmediate, (XtPointer)1},
    {XtNusername,XtCUsername,XtRString,    sizeof(String),    offset(username), XtRString,    NULL},
    {XtNxsokdir, XtCXsokdir, XtRString,    sizeof(String),    offset(xsokdir),  XtRString,    XSOKDIR},
    {XtNxpmdir,  XtCXpmdir,  XtRString,    sizeof(String),    offset(xpmdir),   XtRString,    NULL},
    {XtNsavedir, XtCSavedir, XtRString,    sizeof(String),    offset(savedir),  XtRString,    XSOKSAVE },
    {XtNmessageFile,XtCMessageFile,XtRString,sizeof(String),  offset(messageFile), XtRString, "messages"},
    {XtNkeyboardFile,XtCKeyboardFile,XtRString,sizeof(String),offset(keyboardFile),XtRString, "keys"},
#undef offset
};


/* actions on the desktop area */
/*ARGSUSED*/
static void TableauKey(Widget w, XEvent *xev, String *s, Cardinal *num) {
    key_press((XKeyPressedEvent *)xev);
}
/*ARGSUSED*/
static void TableauBtn_down(Widget w, XEvent *xev, String *s, Cardinal *num) {
    button_press((XButtonPressedEvent *)xev);
}
/*ARGSUSED*/
static void Redisplay(Widget w, XEvent *xev, Region region) {
    redraw_table((XExposeEvent *)xev);
}

static XtActionsRec actions[] = {
    /* {name, procedure}, */
    { "tableau_k",	TableauKey },
    { "tableau_d",	TableauBtn_down },
};

static char translations[] = "\
<Key>:		tableau_k()	\n\
<BtnDown>:	tableau_d()	\n\
";


static Boolean SetValues(Widget current, Widget request, Widget desired,
			 ArgList args, Cardinal *num_args) {
    return FALSE;
}

static void Initialize(Widget request, Widget xnew, ArgList args, Cardinal *n);
static void Resize(Widget gw);


static void Realize(Widget w, XtValueMask *valuemask, XSetWindowAttributes *winattr) {
    *valuemask |= CWEventMask | CWBackingStore | CWBitGravity;
    winattr->backing_store = WhenMapped;
    winattr->bit_gravity = NorthWestGravity;
    winattr->event_mask = KeyPressMask | ExposureMask | ButtonPressMask |
            ButtonReleaseMask | StructureNotifyMask | Button3MotionMask;
   (*(tableauClassRec.core_class.superclass)->core_class.realize)(w, valuemask, winattr);
}

TableauClassRec tableauClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"Tableau",
    /* widget_size		*/	sizeof(TableauRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* tableau fields */
    /* empty			*/	0
  }
};

WidgetClass tableauWidgetClass = (WidgetClass)&tableauClassRec;

static Widget toplev = NULL;

static void Resize(Widget gw) {
    TableauWidget w = (TableauWidget) gw;
    /* printf("Parent wants me to have size %d,%d\n",
	   w->core.width, w->core.height); */
    resize_event(w->core.width, w->core.height);
    /* (*pileWidgetClass->core_class.superclass->core_class.resize)(gw); */
}

void AskWidgetForResize(XSize_t x, XSize_t y) {
    XtWidgetGeometry Geo;
    XtGeometryResult r;

    Geo.width = x;
    Geo.height = y;
    do {
	Geo.request_mode = CWWidth | CWHeight;
#ifdef LABER
	printf("resize to %d %d yielded ", Geo.width, Geo.height);
#endif
	r = XtMakeGeometryRequest(toplev, &Geo, &Geo);
#ifdef LABER
	switch (r) {
	case XtGeometryYes:	printf("YES!\n");break;
	case XtGeometryNo:	printf("NO!\n"); break;
	case XtGeometryAlmost:printf("Almost!\n"); break;
	case XtGeometryDone:	printf("Done!\n"); break;
	}
#endif
    } while (r == XtGeometryAlmost);
}

const char *keyfilename;

static void Initialize(Widget request, Widget xnew, ArgList args, Cardinal *n) {
    static int is_inited = 0;
    TableauWidget new = (TableauWidget)xnew;
    TableauPart *p = &new->tableau;
    toplev = xnew;
    if (is_inited) {
	fprintf(stderr, "Sorry, currently only one instance of Tableau is allowed\n");
	exit(EXIT_FAILURE);
    }

    xsokdir = p->xsokdir;
    savedir = p->savedir;
    setlangdir();
    if (!p->xpmdir)
	p->xpmdir = p->xsokdir;
    if (strlen(savedir) > MAXSAVEFILELEN-16 ||
	strlen(xsokdir) > MAXXSOKDIRLEN ||
	strlen(p->xpmdir) > MAXXSOKDIRLEN) {
	fprintf(stderr, "directory too long\n");
	exit(1);
    }
    if (strlen(p->keyboardFile) > MAXFILENAMELEN ||
	strlen(p->messageFile) > MAXFILENAMELEN) {
	fprintf(stderr, "filename too long\n");
	exit(1);
    }
    keyfilename = p->keyboardFile;
    read_message_file(p->messageFile);
    read_keyboard_file(p->keyboardFile);

    /* assign global data for old Xlib program */
    dpy    = XtDisplay(new);

    buildusername(p->username);
    /* gamegraphic = 1; */
    change_rules(p->rules);
    NewLevel(p->level);

    init_gfx(p->xpmdir);		/* make GCs, read xpm files */
    init_layout();
#ifdef LABER
    printf("Init widget: res %d grap %d\n", new->core.width, graphic.width);
#endif
    if (new->core.width < graphic.width)
	new->core.width = graphic.width;
    if (new->core.height < graphic.height)
	new->core.height = graphic.height;
}
