/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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


/* data initialization and C<->Scheme data conversion */

#include <stdio.h>

#include <gh.h>

/* data conversion C->scheme */
SCM gh_int2scmb(int x)
{
  return (x ? SCM_BOOL_T : SCM_BOOL_F);
}
SCM gh_int2scm(int x)
{
  return scm_long2num((long) x);
}
SCM gh_ulong2scm(unsigned long x)
{
  return scm_ulong2num(x);
}
SCM gh_long2scm(long x)
{
  return scm_long2num(x);
}
SCM gh_double2scm(double x)
{
  return scm_makdbl(x, 0.0);
}
SCM gh_char2scm(char c)
{
  return SCM_MAKICHR(c);
}
SCM gh_str2scm(char *s, int len)
{
  return scm_makfromstr(s, len, 0);
}
SCM gh_str02scm(char *s)
{
  return scm_makfrom0str(s);
}


/* data conversion scheme->C */
int gh_scm2bool(SCM obj)
{
  return ((obj) == SCM_BOOL_F) ? 0 : 1;
}
unsigned long gh_scm2ulong(SCM obj)
{
  return scm_num2ulong(obj, (char *) SCM_ARG1, "gh_scm2ulong");
}
long gh_scm2long(SCM obj)
{
  return scm_num2long(obj, (char *) SCM_ARG1, "gh_scm2long");
}
int gh_scm2int(SCM obj)
{
  /* NOTE: possible loss of precision here */
  return (int) scm_num2long(obj, (char *) SCM_ARG1, "gh_scm2int");
}
double gh_scm2double(SCM obj)
{
  return scm_num2dbl(obj, "gh_scm2double");
}
char gh_scm2char(SCM obj)
{
  return SCM_ICHR(obj);
}

/* takes a Scheme string in obj, and puts a C string in *return_str;
   the C string is *not* null-terminated, but its length is returned
   in *len */
void gh_scm2str(SCM obj, char **return_str, int *len)
{
  SCM_ASSERT (SCM_NIMP (obj) && SCM_STRINGP (obj), obj, SCM_ARG3,
	      "gh_scm2str");
  if (return_str)
    *return_str = SCM_CHARS (obj);
  if (len)
    *len = SCM_LENGTH (obj);
}
/* takes a Scheme string in obj, and puts a C string in return_str0;
   the C string *is* null-terminated; max_len is the maximum allowed
   length for the return_str0; extra chars will be discarded */
void gh_scm2str0(SCM obj, char *return_str0, int max_len)
{
  char *ret_str;
  int i, len;
  scm_protect_object(obj);
  gh_scm2str(obj, &ret_str, &len);
  for (i = 0; (i < len) && (i < (max_len-1)); ++i) {
    return_str0[i] = ret_str[i];
  }
  /* now make sure we null-terminate it */
  return_str0[i] = '\0';
/*   fprintf(stderr, "ERROR: gh_scm2str0() is not yet implemented\n"); */
/*   assert(0); */
  scm_unprotect_object(obj);
}

/* create a new vector of the given length, all initialized to the
   given value */
SCM gh_vector(SCM length, SCM val)
{
  return scm_make_vector(length, val, SCM_UNDEFINED);
}

/* set the given element of the given vector to the given value */
SCM gh_vset(SCM vec, SCM pos, SCM val)
{
  return scm_vector_set_x(vec, pos, val);
}

/* retrieve the given element of the given vector */
SCM gh_vref(SCM vec, SCM pos)
{
  return scm_vector_ref(vec, pos);
}

/* returns the length of the given vector */
unsigned long gh_vector_length(SCM v)
{
  return gh_scm2ulong(scm_vector_length(v));
}

/* takes a C string of the form "'identifier" and makes the lisp
   symbol 'identifier out of it */
SCM gh_symbol2scm(char *symbol_str)
{
  return SCM_CAR(scm_intern(symbol_str, strlen(symbol_str)));
}
