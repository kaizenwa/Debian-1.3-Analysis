/* Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

/* "readline.c" Scheme interface to readline library
   Author: Radey Shouman */

#include "scm.h"

char *readline   P((const char *prompt));
void add_history P((char *p));

             /* Reads on stdin/stdout only */
static char s_readline[] = "read-edited-line";
SCM lreadline(prompt)
     SCM prompt;
{
  SCM res;
  char *s;
  ASSERT(NIMP(prompt) && STRINGP(prompt), prompt, ARG1, s_readline);
  s = readline(CHARS(prompt));
  if (NULL == s) return EOF_VAL;
  NEWCELL(res);
  DEFER_INTS;
  SETCHARS(res,s);
  SETLENGTH(res,(sizet)strlen(s),tc7_string);
  ALLOW_INTS;
  return res;
}
static char s_add_history[] = "add-history";
SCM ladd_history(line)
     SCM line;
{
  ASSERT(NIMP(line) && STRINGP(line), line, ARG1, s_add_history);
  add_history(CHARS(line));
  return UNSPECIFIED;
}
static char s_def_inport[] = "default-input-port";
SCM def_inport()
{
  return def_inp;
}
static char s_def_outport[] = "default-output-port";
SCM def_outport()
{
  return def_outp;
}
static char s_Iedline[] = "Iedline.scm";
void init_edline()
{
  make_subr(s_def_inport, tc7_subr_0, def_inport);
  make_subr(s_def_outport, tc7_subr_0, def_outport);
  make_subr(s_readline, tc7_subr_1, lreadline);
  make_subr(s_add_history, tc7_subr_1, ladd_history);
  if (scm_ldprog(s_Iedline))
    wta(*loc_errobj, "couldn't init", s_Iedline);
}
