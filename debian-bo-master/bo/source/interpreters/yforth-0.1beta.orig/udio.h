/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name: udio.h
 * Abstract:    User device Input/Output functions.
 */

void d_open(void);
void d_close(void);
void d_clrscr(void);
void d_clreol(void);
void d_setattr(Cell attr);
Cell d_getattr(void);
void d_gotoxy(Cell x, Cell y);
Cell d_wherex(void);
Cell d_wherey(void);
Char d_getch(void);
Cell d_kbhit(void);
