/*
 * File:	msgwin.h
 * Purpose:	Declarations for the dialog box when exit without saves.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: msgwin.h,v 1.3 1996/12/22 20:06:12 liw Exp $"
 */

#ifndef msgwin_h
#define msgwin_h

struct msgwin;
struct win;

int msgwin_create(struct msgwin **, struct win *);
void msgwin_destroy(struct msgwin *);
int msgwin_contains(struct msgwin *, Widget);
void msgwin_popup(struct msgwin *);
void msgwin_popdown(struct msgwin *);
void msgwin_set(struct msgwin *, const char *);

#endif
