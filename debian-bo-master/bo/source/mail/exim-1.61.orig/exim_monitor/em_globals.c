/*************************************************
*                Exim Monitor                    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


#include "em_hdr.h"

/* This source module contains all the global variables used in
the exim monitor that are not in the general Exim globals header. For comments
on their usage, see em_hdr.h */

Display *X_display;
XtAppContext X_appcon;

XtActionsRec actionTable[] = {
  { "dialogAction",  (XtActionProc)dialogAction}};

int actionTableSize = sizeof(actionTable)/sizeof(XtActionsRec);

XtTranslations queue_trans;
XtTranslations text_trans;

Widget dialog_ref_widget;
Widget toplevel_widget;
Widget log_widget;
Widget queue_widget;
Widget unhide_widget = NULL;


FILE  *DEBUG = NULL;
FILE  *LOG;

int    action_output = TRUE;
int    action_queue_update = TRUE;

char   actioned_message[24];
char  *action_required;

int    body_max = 20000;

int    eximon_initialized = FALSE;

int    log_buffer_size = 10240;
int    log_depth = 150;
char  *log_display_buffer;
char  *log_file = NULL;
char  *log_font = NULL;
ino_t  log_inode;
long int log_position;
int    log_width = 600;

char  *menu_event = "Shift<Btn1Down>";
int    menu_is_up = FALSE;
int    min_height = 162;
int    min_width  = 103;

pipe_item *pipe_chain = NULL;

char  *qualify_domain = NULL;
char  *queue_buffer = NULL;
int    queue_depth = 200;
char  *queue_font = NULL;
int    queue_max_addresses = 10;
skip_item *queue_skip = NULL;
int    queue_update = 60;
int    queue_width = 600;

regexp *yyyymmdd_regexp;

char  *size_stripchart = NULL;
int    stripchart_height = 90;
int    stripchart_number = 1;
regexp **stripchart_regexp;
char **stripchart_title;
int   *stripchart_total;
int    stripchart_update = 60;
int    stripchart_width = 80;
int    stripchart_varstart = 1;

int    tick_queue_accumulator = 999999;

char  *window_title = "exim monitor";

/* End of em_globals.c */
