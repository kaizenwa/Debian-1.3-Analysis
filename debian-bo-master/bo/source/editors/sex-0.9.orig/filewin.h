/*
 * File:	filewin.h
 * Purpose:	Declarations for the "enter filename" dialog box.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: filewin.h,v 1.4 1996/12/22 20:06:11 liw Exp $"
 */

#ifndef filewin_h
#define filewin_h

#include <X11/Intrinsic.h>

struct win;
struct filewin;

#include "cmd.h"

int filewin_create(struct filewin **, struct win *, char *, char *,
	cmd_function *);
void filewin_destroy(struct filewin *);
void filewin_set_filename(struct filewin *, char *);

void filewin_popup(struct filewin *);
void filewin_popdown(struct filewin *);

int filewin_contains(struct filewin *, Widget);
char *filewin_filename(struct filewin *);

int filewin_ok(struct filewin *);

struct filewin *filewin_find(Widget);

#endif
