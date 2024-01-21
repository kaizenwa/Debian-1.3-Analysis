/*
 * File:	win.h
 * Purpose:	Declarations for a top level window.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: win.h,v 1.24 1996/12/22 20:06:13 liw Exp $"
 */
 
#ifndef win_h
#define win_h

#include <publib.h>

struct editwin;
struct win;

#include <X11/Intrinsic.h>

enum win_filewin {
	WIN_LOAD, WIN_SAVE, WIN_INSERT, WIN_WRITE_TO, WIN_PIPE, WIN_LINENO,
	WIN_FILEWINS
};

int win_create(struct win **, Sbuf *);
void win_destroy(struct win *);
int win_destroy_all(struct win *);
int win_set_buf(struct win *, Sbuf *);
int win_change_buf_all(Sbuf *, Sbuf *);
void win_add_child(struct win *, Widget);
struct win *win_find(Widget);
void win_force_update(struct win *);
void win_update(struct win *);
void win_update_all(void);
void win_resize(struct win *);
void win_set_title(struct win *);
int win_build_buf_menu(struct win *);
void win_set_scrollbar(struct win *win);
void win_cache_stats(void);

int win_set_filename(struct win *, char *);

struct win *win_find_by_buf(Sbuf *);
struct win *win_ith_win(int);

void win_show(struct win *, long);
Sbufmark *win_selection(struct win *);
Sbuf *win_buf(struct win *);
struct editwin *win_editwin(struct win *);
struct searchwin *win_searchwin(struct win *);
struct filewin *win_filewin(struct win *, enum win_filewin);
Widget win_toplevel(struct win *);

void win_start_selection(struct win *, int, int);
void win_continue_selection(struct win *, int, int);
void win_extend_selection(struct win *, int, int);

void win_set_msg(struct win *, const char *);
void win_popdown_msg(struct win *);

long win_pixel_to_text(struct win *, int, int);

void win_get_dimensions(struct win *, int *, int *);
long win_get_top(struct win *);
void win_set_top(struct win *, long);
long win_get_bottom(struct win *);
long win_get_left(struct win *);
void win_set_left(struct win *, long);

#endif
