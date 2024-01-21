/* 
 * tclMacNotify.c --
 *
 *	This file contains Macintosh-specific procedures for the notifier,
 *	which is the lowest-level part of the Tcl event loop.  This file
 *	works together with ../generic/tclNotify.c.
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclMacNotify.c 1.33 96/12/06 14:27:20
 */

#include "tclInt.h"
#include "tclPort.h"
#include "tclMacInt.h"
#include <signal.h> 
#include <Events.h>
#include <LowMem.h>
#include <Processes.h>
#include <Timer.h>

/*
 * The following variable is a backdoor for use by Tk.  It is set when
 * Tk needs to process events on the Tcl event queue without reentering
 * the system event loop.  Tk uses it to flush the Tcl event queue.
 */

static int ignoreEvents = 0;

static Point lastMousePosition;	/* The last known position of the cursor. */
TclMacConvertEventPtr convertEventProcPtr = NULL;
				/* This pointer holds the address of the
				 * function that will handle all incoming
				 * Macintosh events. */
static RgnHandle utilityRgn = NULL;
				/* Region used as the mouse region for 
				 * WaitNextEvent and the update region when 
				 * checking for events. */
				 
/*
 * Prototypes for procedures that are referenced only in this file:
 */

static int 		CheckEventsAvail _ANSI_ARGS_((void));
static void		EventCheckProc _ANSI_ARGS_((ClientData clientData,
			    int flags));
static void		EventSetupProc _ANSI_ARGS_((ClientData clientData,
			    int flags));
static int		HandleMacEvents _ANSI_ARGS_((int flags));

/*
 *----------------------------------------------------------------------
 *
 * TclMacFlushEvents --
 *
 *	This function is a special purpose hack to allow Tk to
 *	process queued Window events during a recursive event loop
 *	without looking for new events on the system event queue.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Services any pending Tcl events and calls idle handlers.
 *
 *----------------------------------------------------------------------
 */

void
TclMacFlushEvents(
    Tcl_Time *timePtr)		/* Specifies the maximum amount of time
				 * that this procedure should spend processing
				 * events.  The time is given as an
				 * interval, not an absolute wakeup time.
				 * NULL means return when queue is empty. */
{
    long ms;
    void * timerToken;

    ms = (timePtr->sec * 1000) + (timePtr->usec / 1000); 
    timerToken = TclMacStartTimer((long) ms);
    ignoreEvents = 1;

    while (Tcl_DoOneEvent(TCL_DONT_WAIT|TCL_WINDOW_EVENTS|
	    TCL_TIMER_EVENTS|TCL_IDLE_EVENTS)) {
	if (TclMacTimerExpired(timerToken)) {
	    break;
	}
    }

    TclMacRemoveTimer(timerToken);
    ignoreEvents = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * EventSetupProc --
 *
 *	This procedure is part of the event source for the Macintosh.
 *	It is invoked by Tcl_DoOneEvent before it calls select to check
 *	for events on all displays.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Tells the notifier which files should be waited for.
 *
 *----------------------------------------------------------------------
 */

static void
EventSetupProc(
    ClientData clientData,		/* Not used. */
    int flags)				/* Flags passed to Tk_DoOneEvent:
					 * if it doesn't include
					 * TCL_WINDOW_EVENTS then we do
					 * nothing. */
{
    static Tcl_Time dontBlock = {0, 0};

    if (!(flags & TCL_WINDOW_EVENTS)) {
	return;
    }

   if (CheckEventsAvail() == true) {
	Tcl_SetMaxBlockTime(&dontBlock);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * EventCheckProc --
 *
 *	This procedure is the second part of the "event source" for
 *	the Macintosh.  It is invoked by Tcl_DoOneEvent after it calls
 *	WaitNextEvent (or whatever it uses to wait for events).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Makes entries on the Tcl event queue for all the events 
 *	available on the Macintosh event queue.
 *
 *----------------------------------------------------------------------
 */

static void
EventCheckProc(
    ClientData clientData,		/* Not used. */
    int flags)				/* Flags passed to Tk_DoOneEvent:
					 * if it doesn't include 
					 * TCL_WINDOW_EVENTS then we do
					 * nothing. */
{
    if (!(flags & TCL_WINDOW_EVENTS)) {
	return;
    }

    if (CheckEventsAvail() == false) {
	return;
    }
    
    HandleMacEvents(TCL_DONT_WAIT);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_WatchFile --
 *
 *	Arrange for Tcl_DoOneEvent to include this file in the masks
 *	for the next call to select.  This procedure is invoked by
 *	event sources, which are in turn invoked by Tcl_DoOneEvent
 *	before it invokes select.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	
 *	The notifier will generate a file event when the I/O channel
 *	given by fd next becomes ready in the way indicated by mask.
 *	If fd is already registered then the old mask will be replaced
 *	with the new one.  Once the event is sent, the notifier will
 *	not send any more events about the fd until the next call to
 *	Tcl_NotifyFile. 
 *
 *----------------------------------------------------------------------
 */

void
Tcl_WatchFile(
    Tcl_File file,		/* Opaque identifier for a stream. */
    int mask)			/* OR'ed combination of TCL_READABLE,
				 * TCL_WRITABLE, and TCL_EXCEPTION:
				 * indicates conditions to wait for
				 * in select. */
{
    int fd, type;

    fd = (int) Tcl_GetFileInfo(file, &type);

    if (type == TCL_MAC_SOCKET) {
	TclMacWatchSocket(file, mask);
    } else if (type == TCL_MAC_FILE) {
	Tcl_Time timeout = { 0, 0 };

	/*
	 * Currently, files are always ready under the Macintosh,
	 * so we just set a 0 timeout.
	 */

	Tcl_SetMaxBlockTime(&timeout);
    } else if (type == TCL_UNIX_FD) {
	Tcl_Time timeout = { 0, 0 };

	/*
	 * The only types of files that should use TCL_UNIX_FD
	 * should be stdio files associated with a console.  For,
	 * example CodeWarrior's SIOUX.  In this case these files
	 * are non-blocking and always deemed ready.
	 */
	
	Tcl_SetMaxBlockTime(&timeout);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_FileReady --
 *
 *	Indicates what conditions (readable, writable, etc.) were
 *	present on a file the last time the notifier invoked select.
 *	This procedure is typically invoked by event sources to see
 *	if they should queue events.
 *
 * Results:
 *	The return value is 0 if none of the conditions specified by mask
 *	was true for fd the last time the system checked.  If any of the
 *	conditions were true, then the return value is a mask of those
 *	that were true.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_FileReady(
    Tcl_File file,		/* File handle for a stream. */
    int mask)			/* OR'ed combination of TCL_READABLE,
				 * TCL_WRITABLE, and TCL_EXCEPTION:
				 * indicates conditions caller cares about. */
{
    int type;
    int fd;

    fd = (int) Tcl_GetFileInfo(file, &type);

    if (type == TCL_MAC_SOCKET) {
	return TclMacSocketReady(file, mask);
    } else if (type == TCL_MAC_FILE) {
	/*
	 * Under the Macintosh, files are always ready, so we just 
	 * return the mask that was passed in.
	 */

	return mask;
    } else if (type == TCL_UNIX_FD) {
	/*
	 * Under the Macintosh, stdio files are always ready, 
	 * so we just return the mask that was passed in.
	 */

	return mask;
    }
    
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_WaitForEvent --
 *
 *	This procedure does the lowest level wait for events in a
 *	platform-specific manner.  It uses information provided by
 *	previous calls to Tcl_WatchFile, plus the timePtr argument,
 *	to determine what to wait for and how long to wait.
 *
 * Results:
 *	The return value is normally TCL_OK.  However, if there are
 *	no events to wait for (e.g. no files and no timers) so that
 *	the procedure would block forever, then it returns TCL_ERROR.
 *
 * Side effects:
 *	May put the process to sleep for a while, depending on timePtr.
 *	When this procedure returns, an event of interest to the application
 *	has probably, but not necessarily, occurred.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_WaitForEvent(
    Tcl_Time *timePtr)		/* Specifies the maximum amount of time
				 * that this procedure should block before
				 * returning.  The time is given as an
				 * interval, not an absolute wakeup time.
				 * NULL means block forever. */
{
    int numFound, notDone = true;
    EventRecord macEvent;
    long sleepTime = 5;
    long ms;
    Point currentMouse;
    void * timerToken;
    Rect mouseRect;

    /*
     * If we are ignoring events from the system, just return immediately.
     */

    if (ignoreEvents) {
	return TCL_OK;
    }

    /*
     * Calculate the end time, start the WaitNextEvent timer, and
     * create any other data structures we may need.
     */
    if (timePtr == NULL) {
	ms = INT_MAX;
    } else {
	ms = (timePtr->sec * 1000) + (timePtr->usec / 1000);
    }
    if (ms < 10) {
	notDone = true;
    }
    timerToken = TclMacStartTimer((long) ms);
    if (utilityRgn == NULL) {
	utilityRgn = NewRgn();
    }
    
    do {
	/*
	 * Poll for file events.
	 */
	numFound = TclMacNotifySocket();

	if (numFound > 0) {
	    notDone = false;
	}
	
	/*
	 * Check for time out.
	 */
	if (TclMacTimerExpired(timerToken)) {
	    notDone = false;
	}

	/*
	 * Check for mouse moved events.  We need to do this seprately
	 * from and before WaitNextEvent to avoid waiting.
	 */
	GetGlobalMouse(&currentMouse);
	if (!EqualPt(currentMouse, lastMousePosition)) {
	    lastMousePosition = currentMouse;
	    macEvent.what = nullEvent;
	    if (convertEventProcPtr != NULL) {
		if ((*convertEventProcPtr)(&macEvent) == true) {
		    notDone = false;
		}
	    }
	}
	
	/*
	 * Set up mouse region so we will wake if the mouse is moved.  We
	 * do this by defining the smallest possible region around the
	 * current mouse position.
	 */
	SetRect(&mouseRect, currentMouse.h, currentMouse.v,
	    currentMouse.h + 1, currentMouse.v + 1);
	RectRgn(utilityRgn, &mouseRect);
	
	/*
	 * Check for window events.  We may receive a NULL event for various
	 * reasons. 1) the timer has expired, 2) a mouse moved event is 
	 * occuring or 3) the os is giving us time for idle events.
	 */
	if ((notDone == true) || (CheckEventsAvail() == true)) {
	    WaitNextEvent(everyEvent, &macEvent, sleepTime, utilityRgn);
	    if (convertEventProcPtr != NULL) {
		if ((*convertEventProcPtr)(&macEvent) == true) {
		    notDone = false;
		}
	    }
	}
    } while(notDone == true);

    TclMacRemoveTimer(timerToken);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_Sleep --
 *
 *	Delay execution for the specified number of milliseconds.  This
 *	is not a very good call to make.  It will block the system -
 *	you will not even be able to switch applications.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Time passes.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_Sleep(
    int ms)			/* Number of milliseconds to sleep. */
{
    EventRecord dummy;
    int done = false;
    void *timerToken;
    
    if (ms <= 0) {
	return;
    }
    
    timerToken = TclMacStartTimer((long) ms);
    do {
	WaitNextEvent(0, &dummy, (ms / 16.66) + 1, NULL);
	
	if (TclMacTimerExpired(timerToken)) {
	    break;
	}
    } while(!done);
    TclMacRemoveTimer(timerToken);
}

/*
 *----------------------------------------------------------------------
 *
 * CheckEventsAvail --
 *
 *	Checks to see if events are available on the Macintosh queue.
 *	This function looks for both queued events (eg. key & button)
 *	and generated events (update & mouse moved).
 *
 * Results:
 *	True is events exist, false otherwise.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
CheckEventsAvail()
{
    QHdrPtr evPtr;
    WindowRef windowRef;
    Point currentMouse;
    
    evPtr = GetEvQHdr();
    if (evPtr->qHead != NULL) {
	return true;
    }
    
    if (utilityRgn == NULL) {
	utilityRgn = NewRgn();
    }
    
    windowRef = FrontWindow();
    while (windowRef != NULL) {
	GetWindowUpdateRgn(windowRef, utilityRgn);
	if (!EmptyRgn(utilityRgn)) {
	    return true;
	}
	windowRef = GetNextWindow(windowRef);
    }
    
    GetGlobalMouse(&currentMouse);
    if (!EqualPt(currentMouse, lastMousePosition)) {
	return true;
    }
    
    return false;
}

/*
 *----------------------------------------------------------------------
 *
 * TclCreateMacEventSource --
 *
 *	This procedure is called during Tcl initialization to create
 *	the event source for Macintosh window events.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A new event source is created.
 *
 *----------------------------------------------------------------------
 */

void
TclCreateMacEventSource()
{
    static int initialized = 0;

    if (!initialized) {
	Tcl_CreateEventSource(EventSetupProc, EventCheckProc,
		(ClientData) NULL);
	initialized = 1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * HandleMacEvents --
 *
 *	This function checks for events from the Macintosh event queue.
 *	It also is the point at which the Tcl application must provide
 *	cooprative multitasking with other Macintosh applications.  Mac
 *	events are then translated into the appropiate X events and
 *	placed on the Tk event queue.
 *
 * Results:
 *	Returns 1 if event found, 0 otherwise.
 *
 * Side effects:
 *	May change the grab module settings.
 *
 *----------------------------------------------------------------------
 */

int
HandleMacEvents(
    int flags)
{
    EventRecord theEvent;
    int eventFound = false;
    int eventToProcess = false;
    Point currentMouse;

    /*
     * If the TCL_DONT_WAIT flag is set then we first check to see
     * events are available and simple return if none will be found.
     * If the event in question is a motion event we need to handle
     * it first - otherwise, GetNextEvent may block.
     */
     
    GetGlobalMouse(&currentMouse);
    if (!EqualPt(currentMouse, lastMousePosition) &&
	    (convertEventProcPtr != NULL)) {
	lastMousePosition = currentMouse;
	theEvent.what = nullEvent;
	eventFound |= (*convertEventProcPtr)(&theEvent);
    }
    
    if (flags & TCL_DONT_WAIT) {
	if (CheckEventsAvail() == false) {
	    return eventFound;
	}
    }

    do {
	GetNextEvent(everyEvent, &theEvent);
	if (convertEventProcPtr != NULL) {
	    eventFound |= (*convertEventProcPtr)(&theEvent);
	}
    } while (CheckEventsAvail() == true);
    
    return eventFound;
}

/*
 *----------------------------------------------------------------------
 *
 * TclMacSetEventProc --
 *
 *	This function sets the event handling procedure for the 
 *	application.  This function will be passed all incoming Mac
 *	events.  This function usually controls the console or some
 *	other entity like Tk.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Changes the event handling function.
 *
 *----------------------------------------------------------------------
 */

void
TclMacSetEventProc(
    TclMacConvertEventPtr procPtr)
{
    convertEventProcPtr = procPtr;
}
