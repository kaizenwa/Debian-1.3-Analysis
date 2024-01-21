/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: udio.c
 * Abstract:    User Device Input/Output functions. Here are enclosed all
 *              non-portable functions.
 */

#include "yforth.h"
#ifdef HAVE_CONIO
#	include <conio.h>
#elifdef HAVE_CURSES
#	include <curses.h>
#endif
#include "udio.h"

/* d_clrscr: clear the screen */
void d_clrscr() {
#ifdef HAVE_CONIO
	clrscr();
#elifdef HAVE_CURSES
	clear();
#endif
}

/* d_clreol: clear to end of line */
void d_clreol() {
#ifdef HAVE_CONIO
	clreol();
#elifdef HAVE_CURSES
	clrtoeol();
#endif
}

/* d_setattr: set default attributes */
void d_setaddr(Cell attr) {
#ifdef HAVE_CONIO
	textattr(attr);
#elifdef HAVE_CURSES
#endif
}

/* d_getattr: get default attributes */
Cell d_getattr() {
#ifdef HAVE_CONIO
	struct text_info ti;
	gettextinfo(&ti);
	return (ti.attribute);
#elifdef HAVE_CURSES
#endif
}

/* d_gotoxy: move the cursor to the location (x, y) of the screen */
void d_gotoxy(Cell x, Cell y) {
#ifdef HAVE_CONIO
	gotoxy(x, y);
#elifdef HAVE_CURSES
	move(y, x);
#endif
}

/* d_wherex: current column position of the cursor */
Cell d_wherex() {
#ifdef HAVE_CONIO
	return (wherex());
#elifdef HAVE_CURSES
	int x, y;
	getyx(stdscr, y, x);
	return ((Cell) x);
#endif
}

/* d_wherey: current row position of the cursor */
Cell d_wherey() {
#ifdef HAVE_CONIO
	return (wherey());
#elifdef HAVE_CURSES
	int x, y;
	getyx(stdscr, y, x);
	return ((Cell) y);
#endif
}

/* d_getch: read a characted from the input device without displaying it and
 * return as soon as the character is enteres (i.e. no wait for Carriage 
 * Return
 */
Char d_getch() {
#ifdef HAVE_CONIO
	return (getch());
#elifdef HAVE_CURSES
	return (getch());
#endif 
}

/* d_kbhit: return True if a character is available */
Cell d_kbhit() {
#ifdef HAVE_CONIO
	return (kbhit());
#elifdef HAVE_CURSES
	return (1);
#endif 
}

/* d_open: Initialize the Input/Output device */
void d_open() {
#ifdef HAVE_CURSES
	initscr();
	cbreak();
	noecho();
	nonl();
	/* intrflush(stdscr, FALSE); */
	/* keypad(stdscr, TRUE); */
#endif
} 

/* d_close: make some work when program finish to restore Input/Output device */
void d_close() {
#ifdef HAVE_CURSES
	endwin();
#endif
}









