/*
 * File:	searchwin.h
 * Purpose:	Declarations for the "search and replace" dialog box.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: searchwin.h,v 1.7 1996/03/16 22:18:29 liw Exp $"
 */

#ifndef searchwin_h
#define searchwin_h

#include <X11/Intrinsic.h>

struct win;
struct searchwin;

int searchwin_create(struct searchwin **, struct win *);
void searchwin_destroy(struct searchwin *);

void searchwin_popup(struct searchwin *);
void searchwin_popdown(struct searchwin *);

int searchwin_contains(struct searchwin *, Widget);
char *searchwin_search_text(struct searchwin *);
char *searchwin_replace_text(struct searchwin *);

unsigned long searchwin_options(struct searchwin *);

void searchwin_set_search(struct searchwin *, char *);
void searchwin_set_replace(struct searchwin *, char *);

void searchwin_focus(struct searchwin *, int);

#endif
