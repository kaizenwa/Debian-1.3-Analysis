/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
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

/* "rope.c" interface between C and SCM.
   Author: Aubrey Jaffer */

#include "scm.h"

				/* Numeric conversions */
				/* Convert longs to SCM */
SCM long2num(sl)
     long sl;
{
  if (!FIXABLE(sl)) {
# ifdef BIGDIG
    return long2big(sl);
# else
#  ifdef FLOATS
    return makdbl((double) sl, 0.0);
#  else
    return BOOL_F;
#  endif
# endif
  }
  return MAKINUM(sl);
}
SCM ulong2num(sl)
     unsigned long sl;
{
  if (!POSFIXABLE(sl)) {
#ifdef BIGDIG
    return ulong2big(sl);
#else
# ifdef FLOATS
    return makdbl((double) sl, 0.0);
# else
    return BOOL_F;
# endif
#endif
  }
  return MAKINUM(sl);
}

				/* Convert SCM to numbers */
unsigned char num2uchar(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  unsigned long res = INUM(num);
  ASSERT(INUMP(num) && (255L >= res),num,pos,s_caller);
  return (unsigned char) res;
}
unsigned short num2ushort(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  unsigned long res = INUM(num);
  ASSERT(INUMP(num) && (65535L >= res),num,pos,s_caller);
  return (unsigned short) res;
}
unsigned long num2ulong(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  unsigned long res;
  if INUMP(num) {
    ASRTGO(0 < num, errout);
    res = INUM((unsigned long)num);
    return res;
  }
  ASRTGO(NIMP(num), errout);
#ifdef FLOATS
  if REALP(num) {
    double u = REALPART(num);
    if ((0 <= u) && (u <= (unsigned long)~0L)) {
      res = u;
      return res;
    }
  }
#endif
#ifdef BIGDIG
  if (TYP16(num)==tc16_bigpos) {
    sizet l = NUMDIGS(num);
    ASRTGO(DIGSPERLONG >= l, errout);
    res = 0;
    for(;l--;) res = BIGUP(res) + BDIGITS(num)[l];
    return res;
  }
#endif
 errout: wta(num, pos, s_caller);
}
long num2long(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  long res;
  if INUMP(num) {
    res = INUM((long)num);
    return res;
  }
  ASRTGO(NIMP(num), errout);
# ifdef FLOATS
  if REALP(num) {
    double u = REALPART(num);
    if (((MOST_NEGATIVE_FIXNUM * 4) <= u)
	&& (u <= (MOST_POSITIVE_FIXNUM * 4 + 3))) {
      res = u;
      return res;
    }
  }
# endif
# ifdef BIGDIG
  if BIGP(num) {
    sizet l = NUMDIGS(num);
    ASRTGO(DIGSPERLONG >= l, errout);
    res = 0;
    for(;l--;) res = BIGUP(res) + BDIGITS(num)[l];
    ASRTGO(0<res, errout);
    return (tc16_bigpos==TYP16(num) ? res : -res);
  }
# endif
 errout: wta(num, pos, s_caller);
}
#ifdef FLOATS
double num2dbl(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  if INUMP(num) return (double)INUM(num);
  ASRTGO(NIMP(num), errout);
  if REALP(num) return REALPART(num);
#ifdef BIGDIG
  if BIGP(num) return big2dbl(num);
#endif
 errout: wta(num, pos, s_caller);
}
#endif


				/* Convert (arrays of) strings to SCM */
SCM makfromstr(src, len)
     char *src;
     sizet len;
{
  SCM s;
  register char *dst;
  s = makstr((long)len);
  dst = CHARS(s);
  while (len--) *dst++ = *src++;
  return s;
}
SCM makfrom0str(src)
     char *src;
{
  if (!src) return BOOL_F;
  return makfromstr(src, (sizet) strlen(src));
}
/* converts C array of strings to SCM list of strings. */
/* If argc < 0, a null terminated array is assumed. */
SCM makfromstrs(argc, argv)
     int argc;
     char **argv;
{
  int i = argc;
  SCM lst = EOL;
  if (0 > i) for(i = 0; argv[i]; i++);
  while (i--) lst = cons(makfrom0str(argv[i]), lst);
  return lst;
}
/* Converts SCM list of strings to NULL terminated array of strings. */
/* INTS must be DEFERed around this call and the use of the returned array. */
char **makargvfrmstrs(args, s_name)
     SCM args;
     char *s_name;
{
  char **argv;
  int argc = ilength(args);
  argv = (char **)must_malloc((1L+argc)*sizeof(char *), s_vector);
  for(argc = 0; NNULLP(args); args=CDR(args), ++argc) {
    ASSERT(NIMP(CAR(args)) && STRINGP(CAR(args)), CAR(args), ARG2, s_name);
    {
      sizet len = 1 + LENGTH(CAR(args));
      char *dst = (char *)must_malloc((long)len, s_string);
      char *src = CHARS(CAR(args));
      while (len--) dst[len] = src[len];
      argv[argc] = dst;
    }
  }
  argv[argc] = 0;
  return argv;
}
void must_free_argv(argv)
     char **argv;
{
  char **av = argv;
  while(!(*av)) free(*(av++));
  free(argv);
}

				/* Hooks to call SCM from C */
SCM scm_evstr(str)
     char *str;
{
  SCM lsym;
  NEWCELL(lsym);
  SETLENGTH(lsym, strlen(str)+0L, tc7_ssymbol);
  SETCHARS(lsym, str);
  return scm_eval_string(lsym);
}
void scm_ldstr(str)
     char *str;
{
  SCM lsym;
  NEWCELL(lsym);
  SETLENGTH(lsym, strlen(str)+0L, tc7_ssymbol);
  SETCHARS(lsym, str);
  scm_load_string(lsym);
}
int scm_ldfile(path)
     char *path;
{
  SCM name = makfrom0str(path);
  *loc_errobj = name;
  return BOOL_F==tryload(name);
}
int scm_ldprog(path)
     char *path;
{
  SCM name = makfrom0str(path);
  *loc_errobj = name;
  return
    BOOL_F==scm_evstr("(try-load (in-vicinity (program-vicinity) errobj))");
}

				/* Get byte address of SCM array */
#ifdef ARRAYS
long	aind P((SCM ra, SCM args, char *what));
unsigned long scm_addr(args, s_name)
     SCM args;
     char *s_name;
{
  long pos;
  unsigned long ptr = 0;	/* gratuitous assignment squelches cc warn. */
  SCM v;
  ASRTGO(NIMP(args), wna);
  v = CAR(args);
  args = CDR(args);
  if IMP(v) {goto badarg;}
  else if ARRAYP(v) {
    pos = aind(v, args, s_name);
    v = ARRAY_V(v);
  }
  else {
    if NIMP(args) {
      ASSERT(CONSP(args) && INUMP(CAR(args)), args, ARG2, s_name);
      pos = INUM(CAR(args));
      ASRTGO(NULLP(CDR(args)), wna);
    }
    else if NULLP(args) pos = 0;
    else {
      ASSERT(INUMP(args), args, ARG2, s_name);
      pos = INUM(args);
    }
    ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
  }
  switch TYP7(v) {
  case tc7_string:
    ptr = (unsigned long)&(CHARS(v)[pos]);
    break;
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_fvect:
    ptr = (unsigned long)&(((float *)CDR(v))[pos]);
    break;
#  endif
  case tc7_cvect: pos = 2 * pos;
  case tc7_dvect: ptr = (unsigned long)&(((double *)CDR(v))[pos]);
    break;
# endif
  case tc7_bvect: ASRTGO(0==(pos%LONG_BIT), outrng);
    pos = pos/LONG_BIT;
  case tc7_uvect:
  case tc7_ivect:
  case tc7_vector: ptr = (unsigned long)&(VELTS(v)[pos]);
    break;
  outrng: wta(MAKINUM(pos), (char *)OUTOFRANGE, s_name);
  default:
  badarg: wta(v, (char *)ARG1, s_name);
  wna: wta(UNDEFINED, (char *)WNA, s_name);
  }
  return ptr;
}
#endif /* ARRAYS */

void init_rope()
{
}
