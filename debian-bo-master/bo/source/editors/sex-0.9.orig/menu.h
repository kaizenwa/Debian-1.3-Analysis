/*
 * File:	menu.h
 * Purpose:	Declarations for menu creation.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: menu.h,v 1.4 1996/11/03 02:12:27 liw Exp $"
 */
 
#ifndef menu_h
#define menu_h

struct win;
struct menu_buf;

int menu_add(struct menu_buf **, Widget *, struct win *, Widget);

int menu_clear_bufs(struct menu_buf *);
int menu_add_buf(struct menu_buf *, Sbuf *);
void menu_done_adding(struct menu_buf *);

#endif
