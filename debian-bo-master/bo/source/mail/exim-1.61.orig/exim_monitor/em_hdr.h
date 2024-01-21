/*************************************************
*                 Exim Monitor                   *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* This is the general header file for all the modules that comprise
the exim monitor program. */

/* System compilation parameters */

#define queue_index_size  10      /* Size of index into queue */
#define queue_buffer_size 10240   /* Big buffer for header handling */

/* Assume most systems have statfs() unless os.h undefines this macro */

#define HAVE_STATFS

/* Bring in the system-dependent stuff */

#include "os.h"


/* ANSI C includes */

#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Not-fully-ANSI systems (e.g. SunOS4 are missing some things) */

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/* Unix includes */

#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/* The new standard is statvfs; some OS have statfs. Also arrange
to be able to cut it out altogether for way-out OS that don't have
anything. */

#ifdef HAVE_STATFS
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>

#else
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h>
#endif
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#define statvfs statfs
#endif
#endif

#include <sys/wait.h>

/* Regular expression include */

#include "../regexp/regexp.h"

/* Includes from the main source of Exim */

#include "macros.h"
#include "config.h"
#include "structs.h"
#include "globals.h"
#include "functions.h"

/* The sys/resource.h header on SunOS 4 causes trouble with the gcc
compiler. Just stuff the bit we want in here; pragmatic easy way out. */

#ifdef NO_SYS_RESOURCE
#define RLIMIT_NOFILE   6               /* maximum descriptor index + 1 */
struct rlimit {
        int     rlim_cur;               /* current (soft) limit */
        int     rlim_max;               /* maximum value for rlim_cur */
};
#else
#include <sys/time.h>
#include <sys/resource.h>
#endif

/* X11 includes */

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Shell.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/TextSrc.h>
#include <X11/Xaw/TextSink.h>

/* These are required because exim monitor has its own munged
version of the stripchart widget. */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/XawInit.h>
#include <X11/Xaw/StripCharP.h>

extern WidgetClass mystripChartWidgetClass;



/*************************************************
*               Enumerations                     *
*************************************************/

/* Operations on the in-store message queue */

enum { queue_noop, queue_add };

/* Operations on the destinations queue */

enum { dest_noop, dest_add, dest_remove };


/*************************************************
*          Structure for destinations            *
*************************************************/

typedef struct dest_item {
  struct dest_item *next;
  struct dest_item *parent;
  char address[1];
} dest_item;



/*************************************************
*           Structure for queue items            *
*************************************************/

typedef struct queue_item {
  struct queue_item *next;
  struct queue_item *prev;
  struct dest_item  *destinations;
  int  input_time;
  int  update_time;
  int  size;
  char *sender;
  char name[17];
  char seen;
  char frozen;
} queue_item;


/*************************************************
*          Structure for queue skip items        *
*************************************************/

typedef struct skip_item {
  struct skip_item *next;
  time_t reveal;
  char text[1];
} skip_item;


/*************************************************
*           Structure for delivery displays      *
*************************************************/

typedef struct pipe_item {
  struct pipe_item *next;
  int fd;
  Widget widget;
} pipe_item;



/*************************************************
*                Global variables                *
*************************************************/

extern Display *X_display;         /* Current display */
extern XtAppContext X_appcon;      /* Application context */
extern XtActionsRec actionTable[]; /* Actions table */

extern XtTranslations queue_trans; /* translation table for queue text widget */
extern XtTranslations text_trans;  /* translation table for other text widgets */

extern Widget dialog_ref_widget;   /* for positioning dialog box */
extern Widget toplevel_widget;
extern Widget log_widget;          /* widget for tail display */
extern Widget queue_widget;        /* widget for queue display */
extern Widget unhide_widget;       /* widget for unhide button */


extern FILE  *DEBUG;
extern FILE  *LOG;

extern int    action_output;       /* TRUE when wanting action command output */
extern int    action_queue_update; /* controls auto updates */
extern int    actionTableSize;     /* # entries in actionTable */

extern char   actioned_message[];  /* For menu handling */
extern char  *action_required;

extern int    body_max;            /* Max size of body to display */

extern int    eximon_initialized;  /* TRUE when initialized */

extern int    log_buffer_size;     /* size of log buffer */
extern int    log_depth;           /* depth of log tail window */
extern char  *log_display_buffer;  /* to hold display text */
extern char  *log_file;            /* name of exim log file */
extern char  *log_font;            /* font for log display */
extern ino_t  log_inode;           /* the inode of the log file */
extern long int log_position;      /* position in log file */
extern int    log_width;           /* width of log tail window */

extern char  *menu_event;          /* name of menu event */
extern int    menu_is_up;          /* TRUE when menu displayed */
extern int    min_height;          /* min window height */
extern int    min_width;           /* min window width */

extern pipe_item *pipe_chain;      /* for delivery displays */

extern char  *qualify_domain;
extern char  *queue_buffer;        /* large buffer for header handling */
extern int    queue_depth;         /* depth of queue window */
extern char  *queue_font;          /* font for queue display */
extern int    queue_max_addresses; /* limit on per-message list */
extern skip_item *queue_skip;      /* for hiding bits of queue */
extern int    queue_update;        /* update interval */
extern int    queue_width;         /* width of queue window */

extern regexp *yyyymmdd_regexp;    /* for matching yyyy-mm-dd */

extern char  *size_stripchart;     /* path for size monitoring */
extern char  *spool_directory;     /* Name of exim spool directory */
extern int    stripchart_height;   /* height of stripcharts */
extern int    stripchart_number;   /* number of stripcharts */
extern regexp **stripchart_regexp; /* vector of regexps */
extern char **stripchart_title;    /* vector of titles */
extern int   *stripchart_total;    /* vector of accumulating values */
extern int    stripchart_update;   /* update interval */
extern int    stripchart_width;    /* width of stripcharts */
extern int    stripchart_varstart; /* starting number for variable charts */

extern int    tick_queue_accumulator; /* For timing next auto update */

extern char  *window_title;        /* title of the exim monitor window */


/*************************************************
*                Global functions                *
*************************************************/

extern XtActionProc dialogAction(Widget, XEvent *, String *, Cardinal *);

extern char *copystring(char *);
extern void  create_dialog(char *, char *);
extern void  create_stripchart(Widget, char *);
extern void  debug(char *, ...);
extern dest_item *find_dest(queue_item *, char *, int);
extern queue_item *find_queue(char *, int);
extern void  init(int, char **);
extern void  menu_create(Widget, XEvent *, String *, Cardinal *);
extern void  NonMessageDialogue(char *);
extern void  queue_display(void);
extern void  read_log(void);
extern int   read_spool(char *);
extern int   read_spool_init(char *);
extern void  read_spool_tidy(void);
extern int   repaint_window(StripChartWidget, int, int);
extern void  scan_spool_input(int, int);
extern void  stripchart_init(void);
extern int   strncmpic(char *, char *, int);
extern char *strstric(char *, char *, int);
extern void  str_remove(char *, char *, char *);
extern char *str_subfield(char *, char *, int);
extern char *substr(char *, int, int);
extern void  substr_init(void);
extern void  substr_reset(void);
extern void  text_empty(Widget);
extern void  text_show(Widget, char *);
extern void  text_showf(Widget, char *, ...);
extern void  text_position(Widget, int);
extern void  xs_SetValues(Widget, Cardinal, ...);

/* End of em_hdr.h */
