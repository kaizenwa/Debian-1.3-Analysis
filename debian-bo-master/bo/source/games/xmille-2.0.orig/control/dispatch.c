/*
 * dispatch.c
 *
 * pass out X events to appropriate recipiants
 */

# include	<X11/Xlib.h>

struct eventGroup {
	struct eventGroup	*prev;
	Window			w;
	unsigned long		e;
	int			(*f)();
};

int	eventToMask[] = {
	0,
	0,
	KeyPressMask,
	KeyReleaseMask,
	ButtonPressMask,
	ButtonReleaseMask,
	PointerMotionMask | Button1MotionMask | Button2MotionMask
		| Button3MotionMask | Button4MotionMask | Button5MotionMask
		| ButtonMotionMask,
	EnterWindowMask,
	LeaveWindowMask,
	FocusChangeMask,
	FocusChangeMask,
	KeymapStateMask,
	ExposureMask,
			0,
	VisibilityChangeMask,
	SubstructureNotifyMask,
	StructureNotifyMask | SubstructureNotifyMask,
	StructureNotifyMask | SubstructureNotifyMask,
	StructureNotifyMask | SubstructureNotifyMask,
	SubstructureRedirectMask,
	StructureNotifyMask | SubstructureNotifyMask,
	StructureNotifyMask | SubstructureNotifyMask,
	SubstructureRedirectMask,
	StructureNotifyMask | SubstructureNotifyMask,
	ResizeRedirectMask,
	StructureNotifyMask | SubstructureNotifyMask,
	SubstructureRedirectMask,
	PropertyChangeMask,
			0,
			0,
			0,
	ColormapChangeMask,
	OwnerGrabButtonMask,
			0,
			0
};


struct eventGroup	*eventStack, *allocGroup();
extern	Display		*dpy;

bindEvent (window, eventMask, func)
Window		window;
unsigned long	eventMask;
int		(*func)();
{
	struct eventGroup	*g;
	unsigned long		allEvents;

	g = allocGroup ();
	g->w = window;
	g->e = eventMask;
	g->f = func;
	g->prev = eventStack;
	eventStack = g;
	allEvents = 0;
	for (g = eventStack; g; g = g->prev)
		if (g->w == window)
			allEvents |= g->e;
	XSelectInput (dpy, window, allEvents);
}

unbindEvent (window, eventMask)
Window		window;
unsigned long	eventMask;
{
	struct eventGroup	*g, *n, *p;
	unsigned long		t;
	unsigned long		remainingEvents;

	n = 0;
	remainingEvents = 0;
	for (g = eventStack; g; g = p) {
		p = g->prev;
		if (g->w == window) {
 			if (g->e & eventMask) {
				t = eventMask;
				eventMask &= ~g->e;
				g->e &= ~t;
			}
			remainingEvents |= g->e;
			if (g->e == 0) {
				if (n)
					n->prev = p;
				else
					eventStack = p;
				freeGroup (g);
			}
		}
	}
	XSelectInput (dpy, window, remainingEvents);
}

sendEvent (rep)
XAnyEvent	*rep;
{
	struct eventGroup	*g;
	int	type;
	
	type = eventToMask[rep->type];
	for (g = eventStack; g; g = g->prev) {
		if (rep->window == g->w && (type & g->e)) {
			g->f (rep);
			return;
		}
	}
}

dispatch ()
{
	XEvent	event;

	XNextEvent (dpy, &event);
	sendEvent (&event);
}

static struct eventGroup *
allocGroup ()
{
	char	*malloc ();

	return (struct eventGroup *) malloc (sizeof (struct eventGroup));
}

freeGroup (g)
struct eventGroup	*g;
{
	free ((char *) g);
}
