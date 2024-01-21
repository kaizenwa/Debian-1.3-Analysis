/*************************************************
*                  Exim Monitor                  *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "em_hdr.h"

/* This module contains the main program of the Exim monitor, which
sets up the world and then lets the XtAppMainLoop function
run things off X events. */


/*************************************************
*               Static variables                 *
*************************************************/

/* Fallback resources */

static String fallback_resources[] = {"eximon.geometry: +150+0", NULL};

/* X11 fixed argument lists */

static Arg quit_args[] = {
  {XtNfromVert, (XtArgVal) NULL},         /* must be first */
  {XtNlabel,    (XtArgVal) " Quit "},
  {"left",      XawChainLeft},
  {"right",     XawChainLeft},
  {"top",       XawChainTop},
  {"bottom",    XawChainTop}
};

static Arg resize_args[] = {
  {XtNfromVert, (XtArgVal) NULL},         /* must be first */
  {XtNfromHoriz,(XtArgVal) NULL},         /* must be second */
  {XtNlabel,    (XtArgVal) " Size "},
  {"left",      XawChainLeft},
  {"right",     XawChainLeft},
  {"top",       XawChainTop},
  {"bottom",    XawChainTop}
};

static Arg update_args[] = {
  {XtNfromVert, (XtArgVal) NULL},         /* must be first */
  {XtNlabel,    (XtArgVal) " Update "},
  {"left",      XawChainLeft},
  {"right",     XawChainLeft},
  {"top",       XawChainTop},
  {"bottom",    XawChainTop}
};

static Arg hide_args[] = {
  {XtNfromVert, (XtArgVal) NULL},         /* must be first */
  {XtNfromHoriz,(XtArgVal) NULL},         /* must be second */
  {XtNlabel,    (XtArgVal) " Hide "},
  {"left",      XawChainLeft},
  {"right",     XawChainLeft},
  {"top",       XawChainTop},
  {"bottom",    XawChainTop}
};

static Arg unhide_args[] = {
  {XtNfromVert, (XtArgVal) NULL},         /* must be first */
  {XtNfromHoriz,(XtArgVal) NULL},         /* must be second */
  {XtNlabel,    (XtArgVal) " Unhide "},
  {"left",      XawChainLeft},
  {"right",     XawChainLeft},
  {"top",       XawChainTop},
  {"bottom",    XawChainTop}
};

static Arg log_args[] = {
  {XtNfromVert, (XtArgVal) NULL},         /* must be first */
  {"editType",  XawtextEdit},
  {"useStringInPlace", (XtArgVal)TRUE},
  {"string",    (XtArgVal)""},            /* dummy to get it going */
  {"scrollVertical", XawtextScrollAlways},
  {"scrollHorizontal", XawtextScrollAlways},
  {"right",     XawChainRight},
  {"top",       XawChainTop},
  {"bottom",    XawChainTop}
};

static Arg queue_args[] = {
  {XtNfromVert, (XtArgVal) NULL},         /* must be first */
  {"editType",  XawtextEdit},
  {"string",    (XtArgVal)""},            /* dummy to get it going */
  {"scrollVertical", XawtextScrollAlways},
  {"right",     XawChainRight},
  {"top",       XawChainTop},
  {"bottom",    XawChainBottom}
};

static Arg get_size_args[] = {
  {"width",     (XtArgVal)NULL},
  {"height",    (XtArgVal)NULL}
};

static Arg get_sizepos_args[] = {
  {"width",     (XtArgVal)NULL},
  {"height",    (XtArgVal)NULL},
  {"x",         (XtArgVal)NULL},
  {"y",         (XtArgVal)NULL}
};

XtActionsRec menu_action_table[] = {
  { "menu-create",  menu_create } };

/* Types of non-message dialog action */

enum { da_hide };

/* Miscellaneous local variables */

static int dialog_action;
static int tick_stripchart_accumulator = 999999;
static int tick_interval = 2;
static int screenwidth, screenheight;
static int minposset = 0;
static Dimension minposx, minposy;
static Dimension maxwidth, maxheight;
static Widget outer_form_widget;
static Widget hide_widget;




/*************************************************
*               Debug print routine              *
*************************************************/

void debug(char *format, ...)
{
va_list ap;
va_start(ap, format);

if (DEBUG == NULL)
  {
  if ((DEBUG = fopen("DEBUG", "w")) == NULL)
    {
    printf("Failed to open DEBUG\n");
    exit(99);
    }
  else printf("Opened debugging file DEBUG\n");
  }

vfprintf(DEBUG, format, ap);
va_end(ap);
}


/*************************************************
*                SIGCHLD handler                 *
*************************************************/

/* Operations on messages are done in subprocesses; this handler
just catches them when they finish. It causes a queue display update
unless configured not to. */

static void sigchld_handler(int sig)
{
while (waitpid(-1, NULL, WNOHANG) > 0);
signal(sig, sigchld_handler);
if (action_queue_update) tick_queue_accumulator = 999999;
}



/*************************************************
*             Callback routines                  *
*************************************************/


void updateAction(Widget w, XtPointer client_data, XtPointer call_data)
{
scan_spool_input(TRUE, TRUE);
queue_display();
tick_queue_accumulator = 0;
}

void hideAction(Widget w, XtPointer client_data, XtPointer call_data)
{
actioned_message[0] = 0;
dialog_ref_widget = w;
dialog_action = da_hide;
create_dialog("Hide addresses ending with", "");
}

void unhideAction(Widget w, XtPointer client_data, XtPointer call_data)
{
skip_item *sk = queue_skip;
while (sk != NULL)
  {
  skip_item *next = sk->next;
  free(sk);
  sk = next;
  }
queue_skip = NULL;

XtDestroyWidget(unhide_widget);
unhide_widget = NULL;

scan_spool_input(TRUE, TRUE);
queue_display();
tick_queue_accumulator = 0;
}

void quitAction(Widget w, XtPointer client_data, XtPointer call_data)
{
exit(0);
}


/* Action when the "Size" button is pressed. This is a kludged up mess
that I made work after much messing around. Reading the position of the
toplevel widget gets the absolute position of the data portion of the window,
excluding the window manager's furniture. However, positioning the toplevel
widget's window seems to position the top corner of the furniture. */

void resizeAction(Widget button, XtPointer client_data, XtPointer call_data)
{
Dimension x, y;
Dimension width, height;
XWindowAttributes a;
Window w = XtWindow(toplevel_widget);

/* Get the position of the top level widget. */

get_sizepos_args[0].value = (XtArgVal)(&width);
get_sizepos_args[1].value = (XtArgVal)(&height);
get_sizepos_args[2].value = (XtArgVal)(&x);
get_sizepos_args[3].value = (XtArgVal)(&y);
XtGetValues(toplevel_widget, get_sizepos_args, 4);

/* Get the position of the widget's window relative to its parent; this
gives the thickness of the window manager's furniture. */

XGetWindowAttributes(X_display, XtWindow(toplevel_widget), &a);

/* If at maximum size, reduce to minimum and move back to where it was
when maximized, if that value is set, allowing for the furniture. */

if (width == maxwidth && height == maxheight)
  {
  if (minposset)
    {
    XMoveResizeWindow(X_display, w, minposx - a.x, minposy - a.y,
      min_width, min_height);
    minposset = 0;
    }
  else XResizeWindow(X_display, w, min_width, min_height);
  }

/* Else always expand to maximum. If currently at minimum size, and we have to
move it, remember where it was for coming back. */

else
  {
  int xx = x;
  int yy = y;
  if (x + maxwidth > screenwidth || y + maxheight + 10 > screenheight)
    {
    if (x + maxwidth > screenwidth) xx = 0;
    if (y + maxheight + 10 > screenheight) yy = 0;
    if (width == min_width && height == min_height)
      {
      minposx = x;
      minposy = y;
      minposset = 1;
      }
    XMoveResizeWindow(X_display, w, xx, yy, maxwidth, maxheight);
    }
  else XResizeWindow(X_display, w, maxwidth, maxheight);
  }

/* Ensure the window is at the top */

XRaiseWindow(X_display, w);
}




/*************************************************
*          Handle input from non-msg dialogue    *
*************************************************/

/* The various cases here are: hide domain, (no more yet) */

void NonMessageDialogue(char *s)
{
skip_item *sk;

switch(dialog_action)
  {
  case da_hide:

  /* Create the unhide button if not present */

  if (unhide_widget == NULL)
    {
    unhide_args[0].value = (XtArgVal) log_widget;
    unhide_args[1].value = (XtArgVal) hide_widget;
    unhide_widget = XtCreateManagedWidget("unhide", commandWidgetClass,
      outer_form_widget, unhide_args, XtNumber(unhide_args));
    XtAddCallback(unhide_widget, "callback",  unhideAction, NULL);
    }

  /* Add item to skip queue */

  sk = (skip_item *)malloc(sizeof(skip_item) + (int)strlen(s));
  sk->next = queue_skip;
  queue_skip = sk;
  strcpy(sk->text, s);
  sk->reveal = time(NULL) + 60 * 60;
  scan_spool_input(TRUE, TRUE);
  queue_display();
  tick_queue_accumulator = 0;
  break;
  }
}



/*************************************************
*              Regexp error handler              *
*************************************************/

/* The regexp routines require an error handler. The function
doesn't identify where the error happened, but since we
compile all the expressions at the start, we can detect whether
it's a compilation error or a runtime error. Give a message for
the former and collapse, but ignore the latter. We maintain a
pointer to regexps in the process of compilation so as to be
able to say which one it was that failed. */

void regerror(char *s)
{
if (!eximon_initialized)
  {
  printf("*** Error in regular expression: %s\n", s);
  if (regexp_compiling != NULL)
    printf("*** Failed expression: %s\n", regexp_compiling);
  exit(99);
  }
}



/*************************************************
*              Ticker function                   *
*************************************************/

/* This function is called initially to set up the starting data
values; it then sets a timeout so that it continues to be called
every 2 seconds. */

static void ticker(XtPointer pt, XtIntervalId *i)
{
pipe_item **pp = &pipe_chain;
pipe_item *p = pipe_chain;
tick_queue_accumulator += tick_interval;
tick_stripchart_accumulator += tick_interval;
read_log();

/* If we have passed the queue update time, we must do a full
scan of the queue, checking for new arrivals, etc. This will
as a by-product set the count of items for use by the stripchart
display. On some systems, SIGCHLD signals can get lost at busy times,
so just in case, clean up any completed children here. */

if (tick_queue_accumulator >= queue_update)
  {
  scan_spool_input(TRUE, FALSE);
  queue_display();
  tick_queue_accumulator = 0;
  if (tick_stripchart_accumulator >= stripchart_update)
    tick_stripchart_accumulator = 0;
  while (waitpid(-1, NULL, WNOHANG) > 0);
  }

/* Otherwise, if we have exceeded the stripchart interval,
do a reduced queue scan that simply provides the count for
the stripchart. */

else if (tick_stripchart_accumulator >= stripchart_update)
  {
  scan_spool_input(FALSE, FALSE);
  tick_stripchart_accumulator = 0;
  }

/* Scan any pipes that are set up for listening to delivery processes,
and display their output if their windows are still open. */

while (p != NULL)
  {
  int count;
  char buffer[256];

  while ((count = read(p->fd, buffer, 254)) > 0)
    {
    buffer[count] = 0;
    if (p->widget != NULL) text_show(p->widget, buffer);
    }

  if (count == 0)
    {
    close(p->fd);
    *pp = p->next;
    free(p);
    /* If configured, cause display update */
    if (action_queue_update) tick_queue_accumulator = 999999;
    }

  else pp = &(p->next);

  p = *pp;
  }

/* Reset the timer for next time */

XtAppAddTimeOut(X_appcon, tick_interval * 1000, ticker, 0);
}



void action_hook(Widget w, XtPointer client_data, String name,
  XEvent *event, String *string, Cardinal *n)
{
printf("action hook, name = %s Widget=%d queue_widget=%d\n",
  name, (int)w, (int)queue_widget);
}





/*************************************************
*               Initialize                       *
*************************************************/

int main(int argc, char **argv)
{
int i;
struct stat statdata;
Widget stripchart_form_widget,
       update_widget,
       quit_widget,
       resize_widget;


/* The exim global message_id needs to get set */

message_id_external = message_id_option + 1;
message_id = message_id_external + 1;

/* Some store needs getting for big_buffer, which is used for
constructing file names and things. */

big_buffer = (char *)malloc(1024);

/* Set up the version string and date and output them */

version_init();
printf("\nExim Monitor version %s (compiled %s) initializing\n",
  version_string, version_date);

/* Get the large buffer from free store */

queue_buffer = (char *)malloc(queue_buffer_size);

/* Initialize various things from the environment and arguments. */

init(argc, argv);

/* Set up the SIGCHLD handler */

signal(SIGCHLD, sigchld_handler);

/* Get the buffer for storing the string for the log display. */

log_display_buffer = (char *)malloc(log_buffer_size);
log_display_buffer[0] = 0;

/* Initialize the data structures for the stripcharts */

stripchart_init();

/* Open the log file and position to the end of it. Save its inode
so that we can detect when the file is switched to another one. However,
allow the monitor to start up without a log file (can happen if no
messages have been sent today.) */

LOG = fopen(log_file, "r");
if (LOG == NULL)
  {
  printf("*** eximon warning: can't open log file\n");
  }
else
  {
  fseek(LOG, 0, SEEK_END);
  log_position = ftell(LOG);
  fstat(fileno(LOG), &statdata);
  log_inode = statdata.st_ino;
  }


/* Now initialize the X world and create the top-level widget */

toplevel_widget = XtAppInitialize(&X_appcon, "Eximon", NULL, 0, &argc, argv,
  fallback_resources, NULL, 0);
X_display = XtDisplay(toplevel_widget);
xs_SetValues(toplevel_widget, 4,
  "title",     window_title,
  "iconName",  window_title,
  "minWidth",  min_width,
  "minHeight", min_height);


/* Create the action for setting up the menu in the queue display
window, and register the action for positioning the menu. */

XtAppAddActions(X_appcon, menu_action_table, 1);
XawSimpleMenuAddGlobalActions(X_appcon);

/* Set up translation tables for the text widgets we use. We don't
want all the generality of editing, etc. that the defaults provide.
This cannot be done before initializing X - the parser complains
about unknown events, modifiers, etc. in an unhelpful way... The
queue text widget has a different table which includes the button
for popping up the menu. Note that the order of things in these
tables is significant. Shift<thing> must come before <thing> as
otherwise it isn't noticed. */

/*
   <FocusIn>:      display-caret(on)\n\
   <FocusOut>:     display-caret(off)\n\
*/


sprintf(big_buffer,
   "%s:   menu-create() XawPositionSimpleMenu(menu) MenuPopup(menu)\n\
   <Btn1Down>:     select-start()\n\
   <Btn1Motion>:   extend-adjust()\n\
   <Btn1Up>:       extend-end(PRIMARY,CUT_BUFFER0)\n\
   <Btn3Down>:     extend-start()\n\
   <Btn3Motion>:   extend-adjust()\n\
   <Btn3Up>:       extend-end(PRIMARY,CUT_BUFFER0)\n\
   <Key>Up:        scroll-one-line-down()\n\
   <Key>Down:      scroll-one-line-up()\n\
   Ctrl<Key>R:     search(backward)\n\
   Ctrl<Key>S:     search(forward)\n\
   ", menu_event);

queue_trans = XtParseTranslationTable(big_buffer);

text_trans = XtParseTranslationTable(
  "<Btn1Down>:     select-start()\n\
   <Btn1Motion>:   extend-adjust()\n\
   <Btn1Up>:       extend-end(PRIMARY,CUT_BUFFER0)\n\
   <Btn3Down>:     extend-start()\n\
   <Btn3Motion>:   extend-adjust()\n\
   <Btn3Up>:       extend-end(PRIMARY,CUT_BUFFER0)\n\
   <Key>Up:        scroll-one-line-down()\n\
   <Key>Down:      scroll-one-line-up()\n\
   Ctrl<Key>R:     search(backward)\n\
   Ctrl<Key>S:     search(forward)\n\
  ");


/* Create a toplevel form widget to hold all the other things */

outer_form_widget = XtCreateManagedWidget("form", formWidgetClass,
  toplevel_widget, NULL, 0);

/* Now create an inner form to hold the stripcharts */

stripchart_form_widget = XtCreateManagedWidget("form", formWidgetClass,
  outer_form_widget, NULL, 0);
xs_SetValues(stripchart_form_widget, 5,
  "defaultDistance", 8,
  "left",            XawChainLeft,
  "right",           XawChainLeft,
  "top",             XawChainTop,
  "bottom",          XawChainTop);

/* Create the queue count stripchart and its label. */

create_stripchart(stripchart_form_widget, "queue");

/* If configured, create the size monitoring stripchart, but
only if the OS supports statfs(). */

if (size_stripchart != NULL)
  {
#ifdef HAVE_STATFS
  char *name = size_stripchart + (int)strlen(size_stripchart) - 1;
  while (name > size_stripchart && *name == '/') name--;
  while (name > size_stripchart && *name != '/') name--;
  create_stripchart(stripchart_form_widget, name);
#else
  printf("Can't create size stripchart: statfs() function not available\n");
#endif
  }

/* Now create the configured input/output stripcharts; note
the total number includes the queue stripchart. */

for (i = stripchart_varstart; i < stripchart_number; i++)
  create_stripchart(stripchart_form_widget, stripchart_title[i]);

/* Next in vertical order come the Resize & Quit buttons */

quit_args[0].value = (XtArgVal) stripchart_form_widget;
quit_widget = XtCreateManagedWidget("quit", commandWidgetClass,
  outer_form_widget, quit_args, XtNumber(quit_args));
XtAddCallback(quit_widget, "callback",  quitAction, NULL);

resize_args[0].value = (XtArgVal) stripchart_form_widget;
resize_args[1].value = (XtArgVal) quit_widget;
resize_widget = XtCreateManagedWidget("resize", commandWidgetClass,
  outer_form_widget, resize_args, XtNumber(resize_args));
XtAddCallback(resize_widget, "callback",  resizeAction, NULL);

/* Create an Ascii text widget for the log tail display. */

log_args[0].value = (XtArgVal) quit_widget;
log_widget = XtCreateManagedWidget("log", asciiTextWidgetClass,
  outer_form_widget, log_args, XtNumber(log_args));
XawTextDisplayCaret(log_widget, TRUE);
xs_SetValues(log_widget, 5,
  "translations", text_trans,
  "string",    log_display_buffer,
  "length",    log_buffer_size,
  "height",    log_depth,
  "width",     log_width);

if (log_font != NULL)
  {
  XFontStruct *f = XLoadQueryFont(X_display, log_font);
  if (f != NULL) xs_SetValues(log_widget, 1, "font", f);
  }


/* The update button */

update_args[0].value = (XtArgVal) log_widget;
update_widget = XtCreateManagedWidget("update", commandWidgetClass,
  outer_form_widget, update_args, XtNumber(update_args));
XtAddCallback(update_widget, "callback",  updateAction, NULL);

/* The hide button */

hide_args[0].value = (XtArgVal) log_widget;
hide_args[1].value = (XtArgVal) update_widget;
hide_widget = XtCreateManagedWidget("hide", commandWidgetClass,
  outer_form_widget, hide_args, XtNumber(hide_args));
XtAddCallback(hide_widget, "callback",  hideAction, NULL);

/* Create an Ascii text widget for the queue display. */

queue_args[0].value = (XtArgVal) update_widget;
queue_widget = XtCreateManagedWidget("queue", asciiTextWidgetClass,
  outer_form_widget, queue_args, XtNumber(queue_args));
XawTextDisplayCaret(queue_widget, TRUE);

xs_SetValues(queue_widget, 3,
  "height",    queue_depth,
  "width",     queue_width,
  "translations", queue_trans);

if (queue_font != NULL)
  {
  XFontStruct *f = XLoadQueryFont(X_display, queue_font);
  if (f != NULL) xs_SetValues(queue_widget, 1, "font", f);
  }

/* Call the ticker function to get the initial data set up. It
arranges to have itself recalled every 2 seconds. */

ticker(NULL, NULL);

/* Everything is now set up; this flag is used by the regerror
function and also by the queue reader. */

eximon_initialized = TRUE;
printf("\nExim Monitor running\n");

/* Realize the toplevel and thereby get things displayed */

XtRealizeWidget(toplevel_widget);

/* Find out the size of the initial window, and set that as its
maximum. */

get_size_args[0].value = (XtArgVal)(&maxwidth);
get_size_args[1].value = (XtArgVal)(&maxheight);
XtGetValues(toplevel_widget, get_size_args, 2);

xs_SetValues(toplevel_widget, 2,
  "maxWidth",  maxwidth,
  "maxHeight", maxheight);

/* Set up the size of the screen */

screenwidth = XDisplayWidth(X_display, 0);
screenheight= XDisplayHeight(X_display,0);

/* Register the action table */

XtAppAddActions(X_appcon, actionTable, actionTableSize);

/* Enter the application loop which handles things from here
onwards. The return statement is never obeyed, but is needed to
keep pedantic ANSI compilers happy. */

XtAppMainLoop(X_appcon);

return 0;
}

/* End of em_main.c */
