/* Checker stubs for functions defined in curses.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_CURSES_H
#include <curses.h>
#include <stdarg.h>
#include "checker_api.h"
#include "check-printf.h"

#undef HAVE_fullname
#undef HAVE_getcap
#undef HAVE_longname
#undef HAVE_mvprintw
#undef HAVE_mvscanw
#undef HAVE_mvwprintw
#undef HAVE_mvwscanw
#undef HAVE_printw
#undef HAVE_scanw
#undef HAVE_setterm
#undef HAVE_sscans
#undef HAVE_wgetstr
#undef HAVE_touchline

#if 0
#define HAVE_box
#define HAVE_cbreak
#define HAVE_delwin
#define HAVE_echo
#define HAVE_endwin
#define HAVE_gettmode
#define HAVE_idlok
#define HAVE_initscr
#define HAVE_mvcur
#define HAVE_mvwin
#define HAVE_newwin
#define HAVE_nl
#define HAVE_nocbreak
#define HAVE_noecho
#define HAVE_nonl
#define HAVE_noraw
#define HAVE_overlay
#define HAVE_overwrite
#define HAVE_raw
#define HAVE_resetty
#define HAVE_savetty
#define HAVE_scroll
#define HAVE_subwin
/* #define HAVE_suspendwin */
#define HAVE_touchline
#define HAVE_touchoverlap
#define HAVE_touchwin
#define HAVE_vwprintw
#define HAVE_vwscanw
#define HAVE_waddch
#define HAVE_waddnstr
#define HAVE_wclear
#define HAVE_wclrtobot
#define HAVE_wclrtoeol
#define HAVE_wdelch
#define HAVE_wdeleteln
#define HAVE_werase
#define HAVE_wgetch
#define HAVE_winsch
#define HAVE_winsertln
#define HAVE_wmove
#define HAVE_wprintw
#define HAVE_wrefresh
#define HAVE_wscanw
#define HAVE_wstandend
#define HAVE_wstandout
#endif

#define CHECK_WIN(win)  chkr_check_addr (win, sizeof (WINDOW), CHKR_TW)

/* compiled from: . */
#ifdef HAVE_box
/* From `/usr/include/curses.h:255'.  */
int
chkr$box (WINDOW *win, int vert, int hor)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (box);
#else
  {
    int res;
    res = box (win, vert, hor);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_box */

#ifdef HAVE_cbreak
/* From `/usr/include/curses.h:256'.  */
int
chkr$cbreak (void)
{
#if USE_BI_JUMP
  __builtin_jump (cbreak);
#else
  return cbreak ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cbreak */

#ifdef HAVE_delwin
/* From `/usr/include/curses.h:257'.  */
int
chkr$delwin (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (delwin);
#else
  return delwin (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_delwin */

#ifdef HAVE_echo
/* From `/usr/include/curses.h:258'.  */
int
chkr$echo (void)
{
#if USE_BI_JUMP
  __builtin_jump (echo);
#else
  return echo ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_echo */

#ifdef HAVE_endwin
/* From `/usr/include/curses.h:259'.  */
int
chkr$endwin (void)
{
#if USE_BI_JUMP
  __builtin_jump (endwin);
#else
  return  endwin ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endwin */

#ifdef HAVE_fullname
/* From `/usr/include/curses.h:260'.  */
char *
chkr$fullname (char * arg0, char * arg1)
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (fullname);
#else
  {
    char * res;
    res = fullname (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fullname */

#ifdef HAVE_getcap
/* From `/usr/include/curses.h:261'.  */
char *
chkr$getcap (char * arg0)
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (getcap);
#else
  {
    char * res;
    res = getcap (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getcap */

#ifdef HAVE_gettmode
/* From `/usr/include/curses.h:262'.  */
int
chkr$gettmode (void)
{
#if USE_BI_JUMP
  __builtin_jump (gettmode);
#else
  return gettmode ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_gettmode */

#ifdef HAVE_idlok
/* From `/usr/include/curses.h:263'.  */
void
chkr$idlok (WINDOW *win, int bf)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (idlok);
#else
  idlok (win, bf);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_idlok */

#ifdef HAVE_initscr
/* From `/usr/include/curses.h:264'.  */
WINDOW *
chkr$initscr (void)
{
#if USE_BI_JUMP
  __builtin_jump (initscr);
#else
  return initscr ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_initscr */

#ifdef HAVE_longname
/* From `/usr/include/curses.h:265'.  */
char *
chkr$longname (char * arg0, char * arg1)
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (longname);
#else
  {
    char * res;
    res = longname (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_longname */

#ifdef HAVE_mvcur
/* From `/usr/include/curses.h:266'.  */
int
chkr$mvcur (int arg0, int arg1, int arg2, int arg3)
{
#if USE_BI_JUMP
  __builtin_jump (mvcur);
#else
  {
    int res;
    res = mvcur (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvcur */

#ifdef HAVE_mvprintw
/* From `/usr/include/curses.h:267'.  */
int
chkr$mvprintw (int arg0, int arg1, const char * arg2, ... )
{
  /* This function requires a stub */
  chkr_check_addr (arg2, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (mvprintw);
#else
  {
    int res;
    res = mvprintw (arg0, arg1, arg2, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvprintw */

#ifdef HAVE_mvscanw
/* From `/usr/include/curses.h:268'.  */
int
chkr$mvscanw (int arg0, int arg1, const char * arg2, ... )
{
  /* This function requires a stub */
  chkr_check_addr (arg2, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (mvscanw);
#else
  {
    int res;
    res = mvscanw (arg0, arg1, arg2, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvscanw */

#ifdef HAVE_mvwin
/* From `/usr/include/curses.h:269'.  */
int
chkr$mvwin (WINDOW *win, int bx, int by)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (mvwin);
#else
  return mvwin (win, bx, by);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvwin */

#ifdef HAVE_mvwprintw
/* From `/usr/include/curses.h:270'.  */
int
chkr$mvwprintw (WINDOW * arg0, int arg1, int arg2, const char * arg3, ... )
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  chkr_check_addr (arg3, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (mvwprintw);
#else
  {
    int res;
    res = mvwprintw (arg0, arg1, arg2, arg3, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvwprintw */

#ifdef HAVE_mvwscanw
/* From `/usr/include/curses.h:271'.  */
int
chkr$mvwscanw (WINDOW * arg0, int arg1, int arg2, const char * arg3, ... )
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  chkr_check_addr (arg3, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (mvwscanw);
#else
  {
    int res;
    res = mvwscanw (arg0, arg1, arg2, arg3, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvwscanw */

#ifdef HAVE_newwin
/* From `/usr/include/curses.h:272'.  */
WINDOW *
chkr$newwin (int l, int c, int y, int x)
{
#if USE_BI_JUMP
  __builtin_jump (newwin);
#else
  return newwin (l, c, y, x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_newwin */

#ifdef HAVE_nl
/* From `/usr/include/curses.h:273'.  */
int
chkr$nl (void)
{
#if USE_BI_JUMP
  __builtin_jump (nl);
#else
  return nl ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nl */

#ifdef HAVE_nocbreak
/* From `/usr/include/curses.h:274'.  */
int
chkr$nocbreak (void)
{
#if USE_BI_JUMP
  __builtin_jump (nocbreak);
#else
  return nocbreak ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nocbreak */

#ifdef HAVE_noecho
/* From `/usr/include/curses.h:275'.  */
int
chkr$noecho (void)
{
#if USE_BI_JUMP
  __builtin_jump (noecho);
#else
  return noecho ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_noecho */

#ifdef HAVE_nonl
/* From `/usr/include/curses.h:276'.  */
int
chkr$nonl (void)
{
#if USE_BI_JUMP
  __builtin_jump (nonl);
#else
  return nonl ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nonl */

#ifdef HAVE_noraw
/* From `/usr/include/curses.h:277'.  */
int
chkr$noraw (void)
{
#if USE_BI_JUMP
  __builtin_jump (noraw);
#else
  return noraw ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_noraw */

#ifdef HAVE_overlay
/* From `/usr/include/curses.h:278'.  */
int
chkr$overlay (WINDOW *win1, WINDOW *win2)
{
  CHECK_WIN (win1);
  CHECK_WIN (win2);
#if USE_BI_JUMP
  __builtin_jump (overlay);
#else
  return overlay (win1, win2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_overlay */

#ifdef HAVE_overwrite
/* From `/usr/include/curses.h:279'.  */
int
chkr$overwrite (WINDOW *win1, WINDOW *win2)
{
  CHECK_WIN (win1);
  CHECK_WIN (win2);
#if USE_BI_JUMP
  __builtin_jump (overwrite);
#else
  return overwrite (win1, win2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_overwrite */

#ifdef HAVE_printw
/* From `/usr/include/curses.h:280'.  */
int
chkr$printw (const char * arg0, ... )
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (printw);
#else
  {
    int res;
    res = printw (arg0, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_printw */

#ifdef HAVE_raw
/* From `/usr/include/curses.h:281'.  */
int
chkr$raw (void)
{
#if USE_BI_JUMP
  __builtin_jump (raw);
#else
  return raw ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_raw */

#ifdef HAVE_resetty
/* From `/usr/include/curses.h:282'.  */
int
chkr$resetty (void)
{
#if USE_BI_JUMP
  __builtin_jump (resetty);
#else
  return resetty ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_resetty */

#ifdef HAVE_savetty
/* From `/usr/include/curses.h:283'.  */
int
chkr$savetty (void)
{
#if USE_BI_JUMP
  __builtin_jump (savetty);
#else
  return savetty ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_savetty */

#ifdef HAVE_scanw
/* From `/usr/include/curses.h:284'.  */
int
chkr$scanw (const char * arg0, ... )
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (scanw);
#else
  {
    int res;
    res = scanw (arg0, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_scanw */

#ifdef HAVE_scroll
/* From `/usr/include/curses.h:285'.  */
int
chkr$scroll (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (scroll);
#else
  return scroll (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_scroll */

#ifdef HAVE_setterm
/* From `/usr/include/curses.h:286'.  */
int
chkr$setterm (char *type)
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (setterm);
#else
  {
    int res;
    res = setterm (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setterm */

#ifdef HAVE_sscans
/* From `/usr/include/curses.h:287'.  */
int
chkr$sscans (WINDOW * arg0, const char * arg1, ... )
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  chkr_check_addr (arg1, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (sscans);
#else
  {
    int res;
    res = sscans (arg0, arg1, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sscans */

#ifdef HAVE_subwin
/* From `/usr/include/curses.h:288'.  */
WINDOW *
chkr$subwin (WINDOW *win, int l, int c, int y, int x)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (subwin);
#else
  return subwin (win, l, c, y, x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_subwin */

#ifdef HAVE_suspendwin
/* From `/usr/include/curses.h:289'.  */
int
chkr$suspendwin (void)
{
#if USE_BI_JUMP
  __builtin_jump (suspendwin);
#else
  return suspendwin ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_suspendwin */

#ifdef HAVE_touchline
/* From `/usr/include/curses.h:290'.  */
int
chkr$touchline (WINDOW *win, int arg1, int arg2, int arg3)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (touchline);
#else
  return touchline (win, arg1, arg2, arg3);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_touchline */

#ifdef HAVE_touchoverlap
/* From `/usr/include/curses.h:291'.  */
int
chkr$touchoverlap (WINDOW *win1, WINDOW *win2)
{
  CHECK_WIN (win1);
  CHECK_WIN (win2);
#if USE_BI_JUMP
  __builtin_jump (touchoverlap);
#else
  return touchoverlap (win1, win2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_touchoverlap */

#ifdef HAVE_touchwin
/* From `/usr/include/curses.h:292'.  */
int
chkr$touchwin (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (touchwin);
#else
  return touchwin (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_touchwin */

#ifdef HAVE_vwprintw
/* From `/usr/include/curses.h:313'.  */
int
chkr$vwprintw (WINDOW *win, const char *format, va_list param)
{
  CHECK_WIN (win);
  chkr_check_str (format, CHKR_RO);
  check_printf_format ("vwprintw", format, param, TYPE_PRINTF, 1);
#if USE_BI_JUMP
  __builtin_jump (vwprintw);
#else
  return vwprintw (win, format, param);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_vwprintw */

#ifdef HAVE_vwscanw
/* From `/usr/include/curses.h:294'.  */
int
chkr$vwscanw (WINDOW *win, const char *format, va_list param)
{
  int n;
  
  CHECK_WIN (win);
  chkr_check_str (format, CHKR_RO);
  check_printf_format ("vwscanw", format, param, TYPE_PRESCANF, 1);
  n = vwscanw (win, format, param);
  if (n != EOF)
    check_printf_format ("vwscanw", format, param, n, 1);
  return n;
}
#endif /* HAVE_vwscanw */

#ifdef HAVE_waddch
/* From `/usr/include/curses.h:295'.  */
int
chkr$waddch (WINDOW *win, int ch)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (waddch);
#else
  return waddch (win, ch);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_waddch */

#ifdef HAVE_waddnstr
/* From `/usr/include/curses.h:296'.  */
int
chkr$waddnstr (WINDOW *win, const char *str, int len)
{
  CHECK_WIN (win);
  chkr_check_addr (str, len, CHKR_RO);
#if USE_BI_JUMP
  __builtin_jump (waddnstr);
#else
  return waddnstr (win, str, len);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_waddnstr */

#ifdef HAVE_wclear
/* From `/usr/include/curses.h:297'.  */
int
chkr$wclear (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wclear);
#else
  return wclear (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wclear */

#ifdef HAVE_wclrtobot
/* From `/usr/include/curses.h:298'.  */
int
chkr$wclrtobot (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wclrtobot);
#else
  return wclrtobot (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wclrtobot */

#ifdef HAVE_wclrtoeol
/* From `/usr/include/curses.h:299'.  */
int
chkr$wclrtoeol (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wclrtoeol);
#else
  return wclrtoeol (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wclrtoeol */

#ifdef HAVE_wdelch
/* From `/usr/include/curses.h:300'.  */
int
chkr$wdelch (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wdelch);
#else
  return wdelch (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wdelch */

#ifdef HAVE_wdeleteln
/* From `/usr/include/curses.h:301'.  */
int
chkr$wdeleteln (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wdeleteln);
#else
  return wdeleteln (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wdeleteln */

#ifdef HAVE_werase
/* From `/usr/include/curses.h:302'.  */
int
chkr$werase (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (werase);
#else
  return werase (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_werase */

#ifdef HAVE_wgetch
/* From `/usr/include/curses.h:303'.  */
int
chkr$wgetch (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wgetch);
#else
  return wgetch (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wgetch */

#ifdef HAVE_wgetstr
/* From `/usr/include/curses.h:304'.  */
int
chkr$wgetstr (WINDOW * arg0, char * arg1)
{
  /* This function requires a stub */
  chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wgetstr);
#else
  {
    int res;
    res = wgetstr (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wgetstr */

#ifdef HAVE_winsch
/* From `/usr/include/curses.h:305'.  */
int
chkr$winsch (WINDOW *win, int ch)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (winsch);
#else
  return winsch (win, ch);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_winsch */

#ifdef HAVE_winsertln
/* From `/usr/include/curses.h:306'.  */
int
chkr$winsertln (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (winsertln);
#else
  return winsertln (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_winsertln */

#ifdef HAVE_wmove
/* From `/usr/include/curses.h:307'.  */
int
chkr$wmove (WINDOW *win, int y, int x)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wmove);
#else
  return wmove (win, y, x);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wmove */

#ifdef HAVE_wprintw
/* From `/usr/include/curses.h:308'.  */
int
chkr$wprintw (WINDOW *win, const char *format, ... )
{
  va_list param;
  
  va_start (param, format);
  CHECK_WIN (win);
  chkr_check_str (format, CHKR_RO);
  check_printf_format ("wprintw", format, param, TYPE_PRINTF, 0);
  return vwprintw (win, format, param);
}
#endif /* HAVE_wprintw */

#ifdef HAVE_wrefresh
/* From `/usr/include/curses.h:309'.  */
int
chkr$wrefresh (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wrefresh);
#else
  return wrefresh (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wrefresh */

#ifdef HAVE_wscanw
/* From `/usr/include/curses.h:310'.  */
int
chkr$wscanw (WINDOW *win, const char *format, ... )
{
  va_list param;
  int n;
  
  va_start (param, format);
  CHECK_WIN (win);
  chkr_check_str (format, CHKR_RO);
  check_printf_format ("wscanw", format, param, TYPE_PRESCANF, 0);
  n = vwscanw (win, format, param);
  if (n != EOF)
    check_printf_format ("wscanw", format, param, n, 0);
  return n;
}
#endif /* HAVE_wscanw */

#ifdef HAVE_wstandend
/* From `/usr/include/curses.h:311'.  */
int
chkr$wstandend (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wstandend);
#else
  return wstandend (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wstandend */

#ifdef HAVE_wstandout
/* From `/usr/include/curses.h:312'.  */
int
chkr$wstandout (WINDOW *win)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wstandout);
#else
  return wstandout (win);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wstandout */

#endif /* HAVE_CURSES_H */
