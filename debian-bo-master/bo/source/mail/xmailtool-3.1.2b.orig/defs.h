/*

Copyright 1990 by Cray Research, Inc.

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of Cray Research, Inc. not be used in
advertising or publicity pertaining to distribution of the software without
specific, written prior permission.  Cray Research, Inc. makes no
representations about the suitability of this software for any purpose.  It
is provided "as is" without express or implied warranty.

*/

static char defs_h_rcsid[]="$Id: defs.h,v 1.23 1995/05/12 19:16:06 bobo Exp $";

#include <X11/Intrinsic.h>

#if defined(SVR4) && !defined(SYSV)
#define SYSV
#endif

#define MAXHEADERS	200
#define MAXMESLEN	2024
#define MAXFILES	2024

#ifdef CRAY
#define MAXPATHLEN 160
#endif

#ifndef SIGCLD 
#define SIGCLD SIGCHLD
#endif

#ifndef DEFAULT_HELPFILE
#define DEFAULT_HELPFILE "/usr/lib/X11/xmailtool.help"
#endif

#define HEADER_END_MARK "------HEADER END-DONT REMOVE------\n"

#define GRIPE_MSG "To: bobo@cray.com\nSubject: Xmailtool Problem\n\
------HEADER END-DONT REMOVE------\n\n\
I would like to register the following complaint/suggestion/comment:\n\n\
============ E N T E R   M E S S A G E   H E R E ===========\n\
\n\
\n\
========= R E Q U I R E D    I N F O R M A T I O N =========\n\
!!! NOTE:  The following information is automaticly generated and inserted in\n\
           this message for support purposes.  Do not edit/delete this\n\
           information.\n\n\
UNAME STRING = %s\n\
EDITOR Value = %s\n\
hold Value   = %d\n"



#define asciiStringWidgetClass asciiTextWidgetClass

struct file_ent {
	char fname[30];
	long mtime,atime,size;
	struct file_ent *child,*next;
};

struct _app_resources {
	Boolean autoHelp;
	Boolean cmdBox;
	Boolean stateBox;
	int interval;
	char *patchLevel;
	char *file;
	char *helpFile;
	char *iconGeometry;
	char *newMailCmd;
	char *formatST;
	char *headerST;
	Cursor norm,wait,show,delete,save,preserve;
};

/* struct outbound_msg {
	Widget box,text;
	char *file;
}; */

struct proc_msg {
	char *file;
	int pid;
	Widget box,text;
	struct proc_msg *next,*prev;
	time_t mtime;
};

typedef struct {
	String string;
	int list_index;
	String data;
} myListReturnStruct;

#ifdef NEED_STRERROR
extern char *sys_errlist[];
#define strerror(a) (sys_errlist[(a)])
#endif

/* #define free(a) my_free(a)
#define realloc(a,b) my_realloc(a,b)
#define malloc(a) my_malloc(a)
*/

