/* Checker stubs for functions defined in termcap.h
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
#include <available-stubs.h>

#ifdef HAVE_TERMCAP_H
#include <termios.h>
#include <termcap.h>
#endif
#include "checker_api.h"

#undef HAVE_tparam

/* compiled from: . */
#ifdef HAVE_tgetent
/* From `/usr/include/termcap.h:31'.  */
int
chkr$tgetent (void *buffer, const char *termtype)
{
  if (buffer)
    stubs_chkr_check_addr (buffer, 2048, CHKR_TW, "buffer");
  stubs_chkr_check_str (termtype, CHKR_RO, "termtype");
  return tgetent (buffer, termtype);
#if 0  
  {
    int res;
    res = tgetent (arg0, arg1);
    return res;
  }
#endif
}
#endif /* HAVE_tgetent */

#ifdef HAVE_tgetnum
/* From `/usr/include/termcap.h:33'.  */
int
chkr$tgetnum (const char *name)
{
  stubs_chkr_check_str (name, CHKR_RO, "name");
#if USE_BI_JUMP
  __builtin_jump (tgetnum);
#else
  return tgetnum (name);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tgetnum */

#ifdef HAVE_tgetflag
/* From `/usr/include/termcap.h:34'.  */
int
chkr$tgetflag (const char *name)
{
  stubs_chkr_check_str (name, CHKR_RO, "name");
#if USE_BI_JUMP
  __builtin_jump (tgetflag);
#else
  return tgetflag (name);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tgetflag */

#ifdef HAVE_tgetstr
/* From `/usr/include/termcap.h:35'.  */
char *
chkr$tgetstr (const char *name, char **area)
{
  char *res;
  
  stubs_chkr_check_str (name, CHKR_RO, "name");
  
  res = tgetstr (name, area);
  if (res)
    {
      stubs_chkr_check_addr (res, strlen (res) + 1, CHKR_WO, "return");
    }
  return res;
}
#endif /* HAVE_tgetstr */

#ifdef HAVE_tputs
/* From `/usr/include/termcap.h:43'.  */
void
chkr$tputs (const char *string, int nlines, int (*outfun)())
{
  stubs_chkr_check_str (string, CHKR_RO, "string");
  stubs_chkr_check_exec (outfun, "outfun");
#if USE_BI_JUMP
  __builtin_jump (tputs);
#else
  tputs (string, nlines, outfun);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tputs */

#ifdef HAVE_tparam
/* From `/usr/include/termcap.h:45'.  */
char *
chkr$tparam (const char * arg0, void * arg1, int arg2, ... )
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_RO);
  stubs_chkr_check_addr (arg1, sizeof (void), CHKR_XX);
  /* This function must be handled by the user */
#if USE_BI_JUMP
  __builtin_jump (tparam);
#else
  return tparam (arg0, arg1, arg2, );
  {
    char * res;
    res = tparam (arg0, arg1, arg2, );
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tparam */

#ifdef HAVE_tgoto
/* From `/usr/include/termcap.h:50'.  */
char *
chkr$tgoto (const char *cstring, int hpos, int vpos)
{
  stubs_chkr_check_str (cstring, CHKR_RO, "cstring");
#if USE_BI_JUMP
  __builtin_jump (tgoto);
#else
  return tgoto (cstring, hpos, vpos);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_tgoto */
