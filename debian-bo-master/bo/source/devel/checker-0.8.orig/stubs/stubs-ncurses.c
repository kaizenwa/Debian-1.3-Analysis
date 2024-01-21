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

#ifdef HAVE_NCURSES_H
#include <ncurses.h>
#include <stdarg.h>
#include "checker_api.h"
#include "check-printf.h"

#define HAVE_beep
#define HAVE_wrefresh
#define HAVE_wmove
#define HAVE_wdeleteln
#define HAVE_winsertln
#define HAVE_wscrl
#define HAVE_wdelch
#define HAVE_waddch
#define HAVE_wclrtoeol
#define HAVE_waddnstr
#define HAVE_wclear
#define HAVE_werase
#define HAVE_winsch
#define HAVE_wgetch
#define HAVE_wattron
#define HAVE_raw
#define HAVE_noraw
#define HAVE_echo
#define HAVE_noecho
#define HAVE_nl
#define HAVE_nonl
#define HAVE_endwin
#define HAVE_newwin
#define HAVE_initscr
#define HAVE_scrollok
#define HAVE_keypad
#define HAVE_mvwprintw
#define HAVE_wprintw
#define HAVE_delwin
#define HAVE_mvwin
#define HAVE_subwin
#define HAVE_derwin
#define HAVE_reset_shell_mode
#define HAVE_reset_prog_mode

#undef HAVE_tgoto
#undef HAVE_tputs

#define CHECK_WIN(win)  stubs_chkr_check_addr (win, sizeof (WINDOW), CHKR_TW, "win")

/* compiled from: . */
#ifdef HAVE_unctrl
/* From `/usr/include/ncurses/unctrl.h:17'.  */
char *
chkr$unctrl (unsigned char arg0)
{
#if USE_BI_JUMP
  __builtin_jump (unctrl);
#else
  return unctrl (arg0);
  {
    char * res;
    res = unctrl (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_unctrl */

#ifdef HAVE_tigetflag
/* From `/usr/include/ncurses/ncurses.h:142'.  */
int
chkr$tigetflag (char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (tigetflag);
#else
  return tigetflag (arg0);
  {
    int res;
    res = tigetflag (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tigetflag */

#ifdef HAVE_tigetnum
/* From `/usr/include/ncurses/ncurses.h:143'.  */
int
chkr$tigetnum (char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (tigetnum);
#else
  return tigetnum (arg0);
  {
    int res;
    res = tigetnum (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tigetnum */

#ifdef HAVE_tigetstr
/* From `/usr/include/ncurses/ncurses.h:144'.  */
char *
chkr$tigetstr (char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (tigetstr);
#else
  return tigetstr (arg0);
  {
    char * res;
    res = tigetstr (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tigetstr */

#ifdef HAVE__init_trace
/* From `/usr/include/ncurses/ncurses.h:148'.  */
void
chkr$_init_trace (void )
{
#if USE_BI_JUMP
  __builtin_jump (_init_trace);
#else
  _init_trace ();
  _init_trace ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__init_trace */

#ifdef HAVE__tracef
/* From `/usr/include/ncurses/ncurses.h:149'.  */
void
chkr$_tracef (char * arg0, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (_tracef);
#else
  _tracef (arg0, );
  _tracef (arg0, );
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE__tracef */

#ifdef HAVE_traceon
/* From `/usr/include/ncurses/ncurses.h:150'.  */
void
chkr$traceon (void )
{
#if USE_BI_JUMP
  __builtin_jump (traceon);
#else
  traceon ();
  traceon ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_traceon */

#ifdef HAVE_traceoff
/* From `/usr/include/ncurses/ncurses.h:151'.  */
void
chkr$traceoff (void )
{
#if USE_BI_JUMP
  __builtin_jump (traceoff);
#else
  traceoff ();
  traceoff ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_traceoff */

#ifdef HAVE_baudrate
/* From `/usr/include/ncurses/ncurses.h:155'.  */
int
chkr$baudrate (void )
{
#if USE_BI_JUMP
  __builtin_jump (baudrate);
#else
  return baudrate ();
  {
    int res;
    res = baudrate ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_baudrate */

#ifdef HAVE_beep
/* From `/usr/include/ncurses/ncurses.h:156'.  */
int
chkr$beep (void)
{
#if USE_BI_JUMP
  __builtin_jump (beep);
#else
  return beep ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_beep */

#ifdef HAVE_cbreak
/* From `/usr/include/ncurses/ncurses.h:157'.  */
int
chkr$cbreak (void )
{
#if USE_BI_JUMP
  __builtin_jump (cbreak);
#else
  return cbreak ();
  {
    int res;
    res = cbreak ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_cbreak */

#ifdef HAVE_clearok
/* From `/usr/include/ncurses/ncurses.h:158'.  */
int
chkr$clearok (WINDOW * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (clearok);
#else
  return clearok (arg0, arg1);
  {
    int res;
    res = clearok (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_clearok */

#ifdef HAVE_copywin
/* From `/usr/include/ncurses/ncurses.h:159'.  */
int
chkr$copywin (WINDOW * arg0, WINDOW * arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (copywin);
#else
  return copywin (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  {
    int res;
    res = copywin (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_copywin */

#ifdef HAVE_crmode
/* From `/usr/include/ncurses/ncurses.h:160'.  */
int
chkr$crmode (void )
{
#if USE_BI_JUMP
  __builtin_jump (crmode);
#else
  return crmode ();
  {
    int res;
    res = crmode ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_crmode */

#ifdef HAVE_curs_set
/* From `/usr/include/ncurses/ncurses.h:161'.  */
int
chkr$curs_set (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (curs_set);
#else
  return curs_set (arg0);
  {
    int res;
    res = curs_set (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_curs_set */

#ifdef HAVE_def_prog_mode
/* From `/usr/include/ncurses/ncurses.h:162'.  */
int
chkr$def_prog_mode (void )
{
#if USE_BI_JUMP
  __builtin_jump (def_prog_mode);
#else
  return def_prog_mode ();
  {
    int res;
    res = def_prog_mode ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_def_prog_mode */

#ifdef HAVE_def_shell_mode
/* From `/usr/include/ncurses/ncurses.h:163'.  */
int
chkr$def_shell_mode (void )
{
#if USE_BI_JUMP
  __builtin_jump (def_shell_mode);
#else
  return def_shell_mode ();
  {
    int res;
    res = def_shell_mode ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_def_shell_mode */

#ifdef HAVE_delwin
/* From `/usr/include/ncurses/ncurses.h:164'.  */
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

#ifdef HAVE_derwin
/* From `/usr/include/ncurses/ncurses.h:165'.  */
WINDOW *
chkr$derwin (WINDOW *win, int nlines, int ncols, int begy, int begx)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (derwin);
#else
  return derwin (win, nlines, ncols, begy, begx);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_derwin */

#ifdef HAVE_doupdate
/* From `/usr/include/ncurses/ncurses.h:166'.  */
int
chkr$doupdate (void )
{
#if USE_BI_JUMP
  __builtin_jump (doupdate);
#else
  return doupdate ();
  {
    int res;
    res = doupdate ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_doupdate */

#ifdef HAVE_echo
/* From `/usr/include/ncurses/ncurses.h:167'.  */
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
/* From `/usr/include/ncurses/ncurses.h:168'.  */
int
chkr$endwin (void)
{
#if USE_BI_JUMP
  __builtin_jump (endwin);
#else
  return endwin ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_endwin */

#ifdef HAVE_erasechar
/* From `/usr/include/ncurses/ncurses.h:169'.  */
char
chkr$erasechar (void )
{
#if USE_BI_JUMP
  __builtin_jump (erasechar);
#else
  return erasechar ();
  {
    char res;
    res = erasechar ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_erasechar */

#ifdef HAVE_flash
/* From `/usr/include/ncurses/ncurses.h:170'.  */
int
chkr$flash (void )
{
#if USE_BI_JUMP
  __builtin_jump (flash);
#else
  return flash ();
  {
    int res;
    res = flash ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_flash */

#ifdef HAVE_flushinp
/* From `/usr/include/ncurses/ncurses.h:171'.  */
int
chkr$flushinp (void )
{
#if USE_BI_JUMP
  __builtin_jump (flushinp);
#else
  return flushinp ();
  {
    int res;
    res = flushinp ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_flushinp */

#ifdef HAVE_idlok
/* From `/usr/include/ncurses/ncurses.h:172'.  */
int
chkr$idlok (WINDOW * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (idlok);
#else
  return idlok (arg0, arg1);
  {
    int res;
    res = idlok (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_idlok */

#ifdef HAVE_initscr
/* From `/usr/include/ncurses/ncurses.h:173'.  */
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

#ifdef HAVE_is_linetouched
/* From `/usr/include/ncurses/ncurses.h:174'.  */
int
chkr$is_linetouched (WINDOW * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (is_linetouched);
#else
  return is_linetouched (arg0, arg1);
  {
    int res;
    res = is_linetouched (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_is_linetouched */

#ifdef HAVE_is_wintouched
/* From `/usr/include/ncurses/ncurses.h:175'.  */
int
chkr$is_wintouched (WINDOW * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (is_wintouched);
#else
  return is_wintouched (arg0);
  {
    int res;
    res = is_wintouched (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_is_wintouched */

#ifdef HAVE_isendwin
/* From `/usr/include/ncurses/ncurses.h:176'.  */
int
chkr$isendwin (void )
{
#if USE_BI_JUMP
  __builtin_jump (isendwin);
#else
  return isendwin ();
  {
    int res;
    res = isendwin ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_isendwin */

#ifdef HAVE_keypad
/* From `/usr/include/ncurses/ncurses.h:177'.  */
int
chkr$keypad (WINDOW *win, int fl)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (keypad);
#else
  return keypad (win, fl);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_keypad */

#ifdef HAVE_killchar
/* From `/usr/include/ncurses/ncurses.h:178'.  */
char
chkr$killchar (void )
{
#if USE_BI_JUMP
  __builtin_jump (killchar);
#else
  return killchar ();
  {
    char res;
    res = killchar ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_killchar */

#ifdef HAVE_leaveok
/* From `/usr/include/ncurses/ncurses.h:179'.  */
int
chkr$leaveok (WINDOW * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (leaveok);
#else
  return leaveok (arg0, arg1);
  {
    int res;
    res = leaveok (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_leaveok */

#ifdef HAVE_longname
/* From `/usr/include/ncurses/ncurses.h:180'.  */
char *
chkr$longname (void )
{
#if USE_BI_JUMP
  __builtin_jump (longname);
#else
  return longname ();
  {
    char * res;
    res = longname ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_longname */

#ifdef HAVE_meta
/* From `/usr/include/ncurses/ncurses.h:181'.  */
int
chkr$meta (WINDOW * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (meta);
#else
  return meta (arg0, arg1);
  {
    int res;
    res = meta (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_meta */

#ifdef HAVE_mvcur
/* From `/usr/include/ncurses/ncurses.h:182'.  */
int
chkr$mvcur (int arg0, int arg1, int arg2, int arg3)
{
#if USE_BI_JUMP
  __builtin_jump (mvcur);
#else
  return mvcur (arg0, arg1, arg2, arg3);
  {
    int res;
    res = mvcur (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvcur */

#ifdef HAVE_mvprintw
/* From `/usr/include/ncurses/ncurses.h:183'.  */
int
chkr$mvprintw (int arg0, int arg1, char * arg2, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg2, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (mvprintw);
#else
  return mvprintw (arg0, arg1, arg2, );
  {
    int res;
    res = mvprintw (arg0, arg1, arg2, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvprintw */

#ifdef HAVE_mvscanw
/* From `/usr/include/ncurses/ncurses.h:184'.  */
int
chkr$mvscanw (int arg0, int arg1, char * arg2, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg2, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (mvscanw);
#else
  return mvscanw (arg0, arg1, arg2, );
  {
    int res;
    res = mvscanw (arg0, arg1, arg2, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvscanw */

#ifdef HAVE_mvwin
/* From `/usr/include/ncurses/ncurses.h:185'.  */
int
chkr$mvwin (WINDOW *win, int by, int bx)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (mvwin);
#else
  return mvwin (win, by, bx);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvwin */

#ifdef HAVE_mvwprintw
/* From `/usr/include/ncurses/ncurses.h:186'.  */
int
chkr$mvwprintw (WINDOW *win, int y, int x, char *fmt, ... )
{
  va_list param;
  
  va_start (param, fmt);
  CHECK_WIN (win);
  stubs_chkr_check_str (fmt, CHKR_RO, "format");
  check_printf_format ("mvwprintw", fmt, param, TYPE_PRINTF, 0);
  
  wmove (win, y, x);
  return vwprintw (win, fmt, param);
}
#endif /* HAVE_mvwprintw */

#ifdef HAVE_mvwscanw
/* From `/usr/include/ncurses/ncurses.h:187'.  */
int
chkr$mvwscanw (WINDOW * arg0, int arg1, int arg2, char * arg3, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (mvwscanw);
#else
  return mvwscanw (arg0, arg1, arg2, arg3, );
  {
    int res;
    res = mvwscanw (arg0, arg1, arg2, arg3, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mvwscanw */

#ifdef HAVE_newpad
/* From `/usr/include/ncurses/ncurses.h:188'.  */
WINDOW *
chkr$newpad (int arg0, int arg1)
{
#if USE_BI_JUMP
  __builtin_jump (newpad);
#else
  return newpad (arg0, arg1);
  {
    WINDOW * res;
    res = newpad (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_newpad */

#ifdef HAVE_newterm
/* From `/usr/include/ncurses/ncurses.h:189'.  */
SCREEN *
chkr$newterm (char * arg0, FILE * arg1, FILE * arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (FILE), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (FILE), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (newterm);
#else
  return newterm (arg0, arg1, arg2);
  {
    SCREEN * res;
    res = newterm (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_newterm */

#ifdef HAVE_newwin
/* From `/usr/include/ncurses/ncurses.h:190'.  */
WINDOW *
chkr$newwin (int nlines, int ncols, int begy, int begx)
{
#if USE_BI_JUMP
  __builtin_jump (newwin);
#else
  return newwin (nlines, ncols, begy, begx);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_newwin */

#ifdef HAVE_nl
/* From `/usr/include/ncurses/ncurses.h:191'.  */
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
/* From `/usr/include/ncurses/ncurses.h:192'.  */
int
chkr$nocbreak (void )
{
#if USE_BI_JUMP
  __builtin_jump (nocbreak);
#else
  return nocbreak ();
  {
    int res;
    res = nocbreak ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nocbreak */

#ifdef HAVE_nocrmode
/* From `/usr/include/ncurses/ncurses.h:193'.  */
int
chkr$nocrmode (void )
{
#if USE_BI_JUMP
  __builtin_jump (nocrmode);
#else
  return nocrmode ();
  {
    int res;
    res = nocrmode ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nocrmode */

#ifdef HAVE_nodelay
/* From `/usr/include/ncurses/ncurses.h:194'.  */
int
chkr$nodelay (WINDOW * arg0, int arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (nodelay);
#else
  return nodelay (arg0, arg1);
  {
    int res;
    res = nodelay (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nodelay */

#ifdef HAVE_noecho
/* From `/usr/include/ncurses/ncurses.h:195'.  */
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
/* From `/usr/include/ncurses/ncurses.h:196'.  */
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
/* From `/usr/include/ncurses/ncurses.h:197'.  */
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
/* From `/usr/include/ncurses/ncurses.h:198'.  */
int
chkr$overlay (WINDOW * arg0, WINDOW * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (overlay);
#else
  return overlay (arg0, arg1);
  {
    int res;
    res = overlay (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_overlay */

#ifdef HAVE_overwrite
/* From `/usr/include/ncurses/ncurses.h:199'.  */
int
chkr$overwrite (WINDOW * arg0, WINDOW * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (overwrite);
#else
  return overwrite (arg0, arg1);
  {
    int res;
    res = overwrite (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_overwrite */

#ifdef HAVE_pnoutrefresh
/* From `/usr/include/ncurses/ncurses.h:200'.  */
int
chkr$pnoutrefresh (WINDOW * arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (pnoutrefresh);
#else
  return pnoutrefresh (arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  {
    int res;
    res = pnoutrefresh (arg0, arg1, arg2, arg3, arg4, arg5, arg6);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pnoutrefresh */

#ifdef HAVE_printw
/* From `/usr/include/ncurses/ncurses.h:201'.  */
int
chkr$printw (char * arg0, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (printw);
#else
  return printw (arg0, );
  {
    int res;
    res = printw (arg0, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_printw */

#ifdef HAVE_putp
/* From `/usr/include/ncurses/ncurses.h:202'.  */
int
chkr$putp (char * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (putp);
#else
  return putp (arg0);
  {
    int res;
    res = putp (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_putp */

#ifdef HAVE_raw
/* From `/usr/include/ncurses/ncurses.h:203'.  */
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

#ifdef HAVE_reset_prog_mode
/* From `/usr/include/ncurses/ncurses.h:204'.  */
int
chkr$reset_prog_mode (void)
{
  return reset_prog_mode ();
}
#endif /* HAVE_reset_prog_mode */

#ifdef HAVE_reset_shell_mode
/* From `/usr/include/ncurses/ncurses.h:205'.  */
int
chkr$reset_shell_mode (void)
{
  return reset_shell_mode ();
}
#endif /* HAVE_reset_shell_mode */

#ifdef HAVE_resetty
/* From `/usr/include/ncurses/ncurses.h:206'.  */
int
chkr$resetty (void )
{
#if USE_BI_JUMP
  __builtin_jump (resetty);
#else
  return resetty ();
  {
    int res;
    res = resetty ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_resetty */

#ifdef HAVE_savetty
/* From `/usr/include/ncurses/ncurses.h:207'.  */
int
chkr$savetty (void )
{
#if USE_BI_JUMP
  __builtin_jump (savetty);
#else
  return savetty ();
  {
    int res;
    res = savetty ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_savetty */

#ifdef HAVE_scanw
/* From `/usr/include/ncurses/ncurses.h:208'.  */
int
chkr$scanw (char * arg0, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (scanw);
#else
  return scanw (arg0, );
  {
    int res;
    res = scanw (arg0, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_scanw */

#ifdef HAVE_scrollok
/* From `/usr/include/ncurses/ncurses.h:209'.  */
int
chkr$scrollok (WINDOW *win, int fl)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (scrollok);
#else
  return scrollok (win, fl);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_scrollok */

#ifdef HAVE_set_term
/* From `/usr/include/ncurses/ncurses.h:210'.  */
SCREEN *
chkr$set_term (SCREEN * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (SCREEN), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (set_term);
#else
  return set_term (arg0);
  {
    SCREEN * res;
    res = set_term (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_set_term */

#ifdef HAVE_setupterm
/* From `/usr/include/ncurses/ncurses.h:211'.  */
int
chkr$setupterm (char * arg0, int arg1, int * arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (setupterm);
#else
  return setupterm (arg0, arg1, arg2);
  {
    int res;
    res = setupterm (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setupterm */

#ifdef HAVE_subwin
/* From `/usr/include/ncurses/ncurses.h:212'.  */
WINDOW *
chkr$subwin (WINDOW *win, int nlines, int ncols, int begy, int begx)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (subwin);
#else
  return subwin (win, nlines, ncols, begy, begx);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_subwin */

#ifdef HAVE_tgoto
/* From `/usr/include/ncurses/ncurses.h:213'.  */
char *
chkr$tgoto (char * arg0, int arg1, int arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (tgoto);
#else
  return tgoto (arg0, arg1, arg2);
  {
    char * res;
    res = tgoto (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tgoto */

#ifdef HAVE_tparm
/* From `/usr/include/ncurses/ncurses.h:214'.  */
char *
chkr$tparm (char * arg0, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (tparm);
#else
  return tparm (arg0, );
  {
    char * res;
    res = tparm (arg0, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tparm */

#ifdef HAVE_tputs
/* From `/usr/include/ncurses/ncurses.h:215'.  */
int
chkr$tputs (char * arg0, int arg1, int (*) (char) arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (tputs);
#else
  return tputs (arg0, arg1, arg2);
  {
    int res;
    res = tputs (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tputs */

#ifdef HAVE_ungetch
/* From `/usr/include/ncurses/ncurses.h:216'.  */
int
chkr$ungetch (char arg0)
{
#if USE_BI_JUMP
  __builtin_jump (ungetch);
#else
  return ungetch (arg0);
  {
    int res;
    res = ungetch (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ungetch */

#ifdef HAVE_vidattr
/* From `/usr/include/ncurses/ncurses.h:217'.  */
int
chkr$vidattr (chtype arg0)
{
#if USE_BI_JUMP
  __builtin_jump (vidattr);
#else
  return vidattr (arg0);
  {
    int res;
    res = vidattr (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_vidattr */

#ifdef HAVE_vidputs
/* From `/usr/include/ncurses/ncurses.h:218'.  */
int
chkr$vidputs (chtype arg0, int (*) (char) arg1)
{
#if USE_BI_JUMP
  __builtin_jump (vidputs);
#else
  return vidputs (arg0, arg1);
  {
    int res;
    res = vidputs (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_vidputs */

#ifdef HAVE_vwscanw
/* From `/usr/include/ncurses/ncurses.h:219'.  */
int
chkr$vwscanw (WINDOW * arg0, char * arg1, va_list arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (vwscanw);
#else
  return vwscanw (arg0, arg1, arg2);
  {
    int res;
    res = vwscanw (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_vwscanw */

#ifdef HAVE_vwprintw
/* From `/usr/include/ncurses/ncurses.h:220'.  */
int
chkr$vwprintw (WINDOW * arg0, char * arg1, va_list arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (vwprintw);
#else
  return vwprintw (arg0, arg1, arg2);
  {
    int res;
    res = vwprintw (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_vwprintw */

#ifdef HAVE_waddch
/* From `/usr/include/ncurses/ncurses.h:221'.  */
int
chkr$waddch (WINDOW *win, chtype c)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (waddch);
#else
  return waddch (win, c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_waddch */

#ifdef HAVE_waddchnstr
/* From `/usr/include/ncurses/ncurses.h:222'.  */
int
chkr$waddchnstr (WINDOW * arg0, chtype * arg1, int arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (chtype), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (waddchnstr);
#else
  return waddchnstr (arg0, arg1, arg2);
  {
    int res;
    res = waddchnstr (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_waddchnstr */

#ifdef HAVE_waddnstr
/* From `/usr/include/ncurses/ncurses.h:223'.  */
int
chkr$waddnstr (WINDOW *win, char *str, int n)
{
  CHECK_WIN (win);
  if (n == -1)
    stubs_chkr_check_str (str, CHKR_RO, "str");
  else
    stubs_chkr_check_addr (str, n, CHKR_RO, "str");
#if USE_BI_JUMP
  __builtin_jump (waddnstr);
#else
  return waddnstr (win, str, n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_waddnstr */

#ifdef HAVE_wattron
/* From `/usr/include/ncurses/ncurses.h:224'.  */
int
chkr$wattron (WINDOW *win, chtype c)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wattron);
#else
  return wattron (win, c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wattron */

#ifdef HAVE_wborder
/* From `/usr/include/ncurses/ncurses.h:225'.  */
int
chkr$wborder (WINDOW * arg0, chtype arg1, chtype arg2, chtype arg3, chtype arg4, chtype arg5, chtype arg6, chtype arg7, chtype arg8)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wborder);
#else
  return wborder (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  {
    int res;
    res = wborder (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wborder */

#ifdef HAVE_wclear
/* From `/usr/include/ncurses/ncurses.h:226'.  */
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
/* From `/usr/include/ncurses/ncurses.h:227'.  */
int
chkr$wclrtobot (WINDOW * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wclrtobot);
#else
  return wclrtobot (arg0);
  {
    int res;
    res = wclrtobot (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wclrtobot */

#ifdef HAVE_wclrtoeol
/* From `/usr/include/ncurses/ncurses.h:228'.  */
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
/* From `/usr/include/ncurses/ncurses.h:229'.  */
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
/* From `/usr/include/ncurses/ncurses.h:230'.  */
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
/* From `/usr/include/ncurses/ncurses.h:231'.  */
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
/* From `/usr/include/ncurses/ncurses.h:232'.  */
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
/* From `/usr/include/ncurses/ncurses.h:233'.  */
int
chkr$wgetstr (WINDOW * arg0, char * arg1)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wgetstr);
#else
  return wgetstr (arg0, arg1);
  {
    int res;
    res = wgetstr (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wgetstr */

#ifdef HAVE_whline
/* From `/usr/include/ncurses/ncurses.h:234'.  */
int
chkr$whline (WINDOW * arg0, chtype arg1, int arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (whline);
#else
  return whline (arg0, arg1, arg2);
  {
    int res;
    res = whline (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_whline */

#ifdef HAVE_winsch
/* From `/usr/include/ncurses/ncurses.h:235'.  */
int
chkr$winsch (WINDOW *win, chtype c)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (winsch);
#else
  return winsch (win, c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_winsch */

#ifdef HAVE_winsertln
/* From `/usr/include/ncurses/ncurses.h:236'.  */
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
/* From `/usr/include/ncurses/ncurses.h:237'.  */
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

#ifdef HAVE_wnoutrefresh
/* From `/usr/include/ncurses/ncurses.h:238'.  */
int
chkr$wnoutrefresh (WINDOW * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wnoutrefresh);
#else
  return wnoutrefresh (arg0);
  {
    int res;
    res = wnoutrefresh (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wnoutrefresh */

#ifdef HAVE_wprintw
/* From `/usr/include/ncurses/ncurses.h:239'.  */
int
chkr$wprintw (WINDOW *win, char *fmt, ... )
{
  va_list param;
  
  va_start (param, fmt);
  CHECK_WIN (win);
  stubs_chkr_check_str (fmt, CHKR_RO, "format");
  check_printf_format ("wprintw", fmt, param, TYPE_PRINTF, 0);
  
  return vwprintw (win, fmt, param);
}
#endif /* HAVE_wprintw */

#ifdef HAVE_wrefresh
/* From `/usr/include/ncurses/ncurses.h:240'.  */
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
/* From `/usr/include/ncurses/ncurses.h:241'.  */
int
chkr$wscanw (WINDOW * arg0, char * arg1, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (wscanw);
#else
  return wscanw (arg0, arg1, );
  {
    int res;
    res = wscanw (arg0, arg1, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wscanw */

#ifdef HAVE_wscrl
/* From `/usr/include/ncurses/ncurses.h:242'.  */
int
chkr$wscrl (WINDOW *win, int n)
{
  CHECK_WIN (win);
#if USE_BI_JUMP
  __builtin_jump (wscrl);
#else
  return wscrl (win, n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wscrl */

#ifdef HAVE_wsetscrreg
/* From `/usr/include/ncurses/ncurses.h:243'.  */
int
chkr$wsetscrreg (WINDOW * arg0, int arg1, int arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wsetscrreg);
#else
  return wsetscrreg (arg0, arg1, arg2);
  {
    int res;
    res = wsetscrreg (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wsetscrreg */

#ifdef HAVE_wtouchln
/* From `/usr/include/ncurses/ncurses.h:244'.  */
int
chkr$wtouchln (WINDOW * arg0, int arg1, int arg2, int arg3)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wtouchln);
#else
  return wtouchln (arg0, arg1, arg2, arg3);
  {
    int res;
    res = wtouchln (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wtouchln */

#ifdef HAVE_wvline
/* From `/usr/include/ncurses/ncurses.h:245'.  */
int
chkr$wvline (WINDOW * arg0, chtype arg1, int arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (WINDOW), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wvline);
#else
  return wvline (arg0, arg1, arg2);
  {
    int res;
    res = wvline (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wvline */

#ifdef HAVE_can_change_color
/* From `/usr/include/ncurses/ncurses.h:247'.  */
char
chkr$can_change_color (void )
{
#if USE_BI_JUMP
  __builtin_jump (can_change_color);
#else
  return can_change_color ();
  {
    char res;
    res = can_change_color ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_can_change_color */

#ifdef HAVE_color_content
/* From `/usr/include/ncurses/ncurses.h:248'.  */
int
chkr$color_content (short int arg0, short int * arg1, short int * arg2, short int * arg3)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg1, sizeof (short int), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (short int), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (short int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (color_content);
#else
  return color_content (arg0, arg1, arg2, arg3);
  {
    int res;
    res = color_content (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_color_content */

#ifdef HAVE_has_colors
/* From `/usr/include/ncurses/ncurses.h:249'.  */
int
chkr$has_colors (void )
{
#if USE_BI_JUMP
  __builtin_jump (has_colors);
#else
  return has_colors ();
  {
    int res;
    res = has_colors ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_has_colors */

#ifdef HAVE_init_color
/* From `/usr/include/ncurses/ncurses.h:250'.  */
int
chkr$init_color (short int arg0, short int arg1, short int arg2, short int arg3)
{
#if USE_BI_JUMP
  __builtin_jump (init_color);
#else
  return init_color (arg0, arg1, arg2, arg3);
  {
    int res;
    res = init_color (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_init_color */

#ifdef HAVE_init_pair
/* From `/usr/include/ncurses/ncurses.h:251'.  */
int
chkr$init_pair (short int arg0, short int arg1, short int arg2)
{
#if USE_BI_JUMP
  __builtin_jump (init_pair);
#else
  return init_pair (arg0, arg1, arg2);
  {
    int res;
    res = init_pair (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_init_pair */

#ifdef HAVE_pair_content
/* From `/usr/include/ncurses/ncurses.h:252'.  */
int
chkr$pair_content (short int arg0, short int * arg1, short int * arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg1, sizeof (short int), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (short int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (pair_content);
#else
  return pair_content (arg0, arg1, arg2);
  {
    int res;
    res = pair_content (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_pair_content */

#ifdef HAVE_start_color
/* From `/usr/include/ncurses/ncurses.h:253'.  */
int
chkr$start_color (void )
{
#if USE_BI_JUMP
  __builtin_jump (start_color);
#else
  return start_color ();
  {
    int res;
    res = start_color ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_start_color */

#ifdef HAVE_slk_init
/* From `/usr/include/ncurses/ncurses.h:255'.  */
int
chkr$slk_init (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (slk_init);
#else
  return slk_init (arg0);
  {
    int res;
    res = slk_init (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_init */

#ifdef HAVE_slk_set
/* From `/usr/include/ncurses/ncurses.h:256'.  */
int
chkr$slk_set (int arg0, char * arg1, int arg2)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (slk_set);
#else
  return slk_set (arg0, arg1, arg2);
  {
    int res;
    res = slk_set (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_set */

#ifdef HAVE_slk_refresh
/* From `/usr/include/ncurses/ncurses.h:257'.  */
int
chkr$slk_refresh (void )
{
#if USE_BI_JUMP
  __builtin_jump (slk_refresh);
#else
  return slk_refresh ();
  {
    int res;
    res = slk_refresh ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_refresh */

#ifdef HAVE_slk_noutrefresh
/* From `/usr/include/ncurses/ncurses.h:258'.  */
int
chkr$slk_noutrefresh (void )
{
#if USE_BI_JUMP
  __builtin_jump (slk_noutrefresh);
#else
  return slk_noutrefresh ();
  {
    int res;
    res = slk_noutrefresh ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_noutrefresh */

#ifdef HAVE_slk_label
/* From `/usr/include/ncurses/ncurses.h:259'.  */
char *
chkr$slk_label (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (slk_label);
#else
  return slk_label (arg0);
  {
    char * res;
    res = slk_label (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_label */

#ifdef HAVE_slk_clear
/* From `/usr/include/ncurses/ncurses.h:260'.  */
int
chkr$slk_clear (void )
{
#if USE_BI_JUMP
  __builtin_jump (slk_clear);
#else
  return slk_clear ();
  {
    int res;
    res = slk_clear ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_clear */

#ifdef HAVE_slk_restore
/* From `/usr/include/ncurses/ncurses.h:261'.  */
int
chkr$slk_restore (void )
{
#if USE_BI_JUMP
  __builtin_jump (slk_restore);
#else
  return slk_restore ();
  {
    int res;
    res = slk_restore ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_restore */

#ifdef HAVE_slk_touch
/* From `/usr/include/ncurses/ncurses.h:262'.  */
int
chkr$slk_touch (void )
{
#if USE_BI_JUMP
  __builtin_jump (slk_touch);
#else
  return slk_touch ();
  {
    int res;
    res = slk_touch ();
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_slk_touch */

#endif /* HAVE_NCURSES_H */