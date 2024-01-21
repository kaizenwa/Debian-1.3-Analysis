/* Checker stubs for functions defined in stdlib.h
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

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#include "checker_api.h"

#undef HAVE_mblen
#undef HAVE_mbtowc
#undef HAVE_wctomb
#undef HAVE_mbstowcs
#undef HAVE_wcstombs
#undef HAVE_ecvt
#undef HAVE_fcvt
#undef HAVE_gcvt
#undef HAVE_insque
#undef HAVE_remque

#if 0
#define HAVE_exit
#define HAVE_abort
#define HAVE_getenv
#define HAVE_atoi
#define HAVE_atol
#define HAVE_atof
/* #define HAVE_atoq */
#define HAVE_strtof
#define HAVE_strtod
#define HAVE_strtold
#define HAVE_strtol
#define HAVE_strtoul
#define HAVE_strtoq
#define HAVE_strtouq
#define HAVE_qsort
#define HAVE_bsearch
#define HAVE_abs
#define HAVE_labs
#define HAVE_ldiv
#define HAVE_div
#define HAVE_unsetenv
#define HAVE_setenv
#define HAVE_putenv
#define HAVE_drand48
#define HAVE_erand48
#define HAVE_lrand48
#define HAVE_nrand48
#define HAVE_mrand48
#define HAVE_jrand48
#define HAVE_srand48
#define HAVE_seed48
#define HAVE_lcong48
#define HAVE_rand
#define HAVE_srand
#define HAVE_random
#define HAVE_srandom
#define HAVE_initstate
#define HAVE_setstate
#define HAVE_atexit
#define HAVE_on_exit
#define HAVE_system
#endif

/* compiled from: . */
#ifdef HAVE_atof
double
chkr$atof (const char *ptr)
{
  stubs_chkr_check_str (ptr, CHKR_RO, "ptr");
#if USE_BI_JUMP
  __builtin_jump (atof);
#else
  return atof (ptr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atof */

#ifdef HAVE_atoi
int
chkr$atoi (const char *ptr)
{
  stubs_chkr_check_str (ptr, CHKR_RO, "ptr");
#if USE_BI_JUMP
  __builtin_jump (atoi);
#else
  return atoi (ptr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atoi */

#ifdef HAVE_atol
long int
chkr$atol (const char *ptr)
{
  int len;
  const char *p = ptr;
  
  len = 1;
  if (*p == '+' || *p == '-')
    {
      p++;
      len++;
    }
  for (; *p >= '0' && *p <= '9'; p++)
    len++;
    
  stubs_chkr_check_addr (ptr, len, CHKR_RO, "ptr");
#if USE_BI_JUMP
  __builtin_jump (atol);
#else
  return atol (ptr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atol */

#ifdef HAVE_atoq
long long int
chkr$atoq (const char *ptr)
{
  stubs_chkr_check_str (ptr, CHKR_RO, "ptr");
#if USE_BI_JUMP
  __builtin_jump (atoq);
#else
  return atoq (ptr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atoq */

#ifdef HAVE_strtof
float
chkr$strtof (const char *nptr, char **endptr)
{
  stubs_chkr_check_str (nptr, CHKR_RO, "nptr");
  if (endptr)
    stubs_chkr_check_addr (endptr, sizeof (char *), CHKR_WO, "endptr");
#if USE_BI_JUMP
  __builtin_jump (strtod);
#else
  return strtof (nptr, endptr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtof */

#ifdef HAVE_strtod
double
chkr$strtod (const char *nptr, char **endptr)
{
  stubs_chkr_check_str (nptr, CHKR_RO, "nptr");
  if (endptr)
    stubs_chkr_check_addr (endptr, sizeof (char *), CHKR_WO, "endptr");
#if USE_BI_JUMP
  __builtin_jump (strtod);
#else
  return strtod (nptr, endptr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtod */

#ifdef HAVE_strtold
__long_double_t
chkr$strtold (const char *nptr, char **endptr)
{
  stubs_chkr_check_str (nptr, CHKR_RO, "nptr");
  if (endptr)
    stubs_chkr_check_addr (endptr, sizeof (char *), CHKR_WO, "endptr");
#if USE_BI_JUMP
  __builtin_jump (strtod);
#else
  return strtold (nptr, endptr);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtold */

#ifdef HAVE_strtol
long int
chkr$strtol (const char *nptr, char **endptr, int base)
{
  stubs_chkr_check_str (nptr, CHKR_RO, "nptr");
  if (endptr)
    stubs_chkr_check_addr (endptr, sizeof (char *), CHKR_WO, "endptr");
#if USE_BI_JUMP
  __builtin_jump (strtod);
#else
  return strtol (nptr, endptr, base);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtol */

#ifdef HAVE_strtoul
long unsigned int
chkr$strtoul (const char *nptr, char **endptr, int base)
{
  stubs_chkr_check_str (nptr, CHKR_RO, "nptr");
  if (endptr)
    stubs_chkr_check_addr (endptr, sizeof (char *), CHKR_WO, "endptr");
#if USE_BI_JUMP
  __builtin_jump (strtod);
#else
  return strtoul (nptr, endptr, base);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtoul */

#ifdef HAVE_strtoq
long long int
chkr$strtoq (const char *nptr, char **endptr, int base)
{
  stubs_chkr_check_str (nptr, CHKR_RO, "nptr");
  if (endptr)
    stubs_chkr_check_addr (endptr, sizeof (char *), CHKR_WO, "endptr");
#if USE_BI_JUMP
  __builtin_jump (strtod);
#else
  return strtoq (nptr, endptr, base);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtoq */

#ifdef HAVE_strtouq
long long unsigned int
chkr$strtouq (const char *nptr, char **endptr, int base)
{
  stubs_chkr_check_str (nptr, CHKR_RO, "nptr");
  if (endptr)
    stubs_chkr_check_addr (endptr, sizeof (char *), CHKR_WO, "endptr");
#if USE_BI_JUMP
  __builtin_jump (strtod);
#else
  return strtouq (nptr, endptr, base);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtouq */

#ifdef HAVE_rand
int
chkr$rand (void)
{
#if USE_BI_JUMP
  __builtin_jump (rand);
#else
  return rand ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rand */

#ifdef HAVE_srand
void
chkr$srand (unsigned int seed)
{
#if USE_BI_JUMP
  __builtin_jump (srand);
#else
  srand (seed);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_srand */

#ifdef HAVE_random
long int
chkr$random (void)
{
#if USE_BI_JUMP
  __builtin_jump (random);
#else
  return random ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_random */

#ifdef HAVE_srandom
void
chkr$srandom (unsigned int seed)
{
#if USE_BI_JUMP
  __builtin_jump (srandom);
#else
  srandom (seed);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_srandom */

#ifdef HAVE_initstate
void *
chkr$initstate (unsigned int seed, void *state, size_t n)
{
  stubs_chkr_check_addr (state, n, CHKR_RO, "state");
#if USE_BI_JUMP
  __builtin_jump (initstate);
#else
  return initstate (seed, state, n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_initstate */

#ifdef HAVE_setstate
void *
chkr$setstate (void *state)
{
#if USE_BI_JUMP
  __builtin_jump (setstate);
#else
  return setstate (state);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setstate */

#ifdef HAVE_abort
void
chkr$abort (void)
{
#if USE_BI_JUMP
  __builtin_jump (abort);
#else
  abort ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_abort */

#ifdef HAVE_atexit
int
chkr$atexit (void (*func) (void))
{
  stubs_chkr_check_exec (func, "func");
#if USE_BI_JUMP
  __builtin_jump (atexit);
#else
  return atexit (func);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_atexit */

#ifdef HAVE_on_exit
int
chkr$on_exit (void (*func) (int, void *), void *arg)
{
  stubs_chkr_check_exec (func, "func");
#if USE_BI_JUMP
  __builtin_jump (on_exit);
#else
  return on_exit (func, arg);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_on_exit */

#ifdef HAVE_exit
void
chkr$exit (int status)
{
#if USE_BI_JUMP
  __builtin_jump (exit);
#else
  exit (status);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_exit */

#ifdef HAVE_getenv
char *
chkr$getenv (const char *str)
{
  stubs_chkr_check_str (str, CHKR_RO, "str");
#if USE_BI_JUMP
  __builtin_jump (getenv);
#else
  {
    char * res;
    res = getenv (str);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getenv */

#ifdef HAVE_putenv
int
chkr$putenv (const char *name)
{
  stubs_chkr_check_str (name, CHKR_RO, "name");
#if USE_BI_JUMP
  __builtin_jump (putenv);
#else
  return putenv (name);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_putenv */

#ifdef HAVE_setenv
int
chkr$setenv (const char *name, const char *value, int overwrite)
{
  stubs_chkr_check_str (name, CHKR_RO, "name");
  stubs_chkr_check_str (value, CHKR_RO, "value");
#if USE_BI_JUMP
  __builtin_jump (setenv);
#else
  return setenv (name, value, overwrite);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setenv */

#ifdef HAVE_system
int
chkr$system (const char *command)
{
  stubs_chkr_check_str (command, CHKR_RO, "command");
#if USE_BI_JUMP
  __builtin_jump (system);
#else
  return system (command);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_system */

#ifdef HAVE_bsearch
void *
chkr$bsearch (const void *key, const void *base, size_t nmemb, size_t size,
              int (*compare)(const void *, const void *))
{
  stubs_chkr_check_exec (compare, "compare");
  if (nmemb * size)
    stubs_chkr_check_addr (base, nmemb * size, CHKR_TW, "base");
  if (size)
    stubs_chkr_check_addr (key, size, CHKR_TW, "key");
#if USE_BI_JUMP
  __builtin_jump (bsearch);
#else
  return bsearch (key, base, nmemb, size, compare);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_bsearch */

#ifdef HAVE_qsort
void
chkr$qsort (void *base, size_t nmemb, size_t size,
            int (*compare)(const void *, const void *))
{
  stubs_chkr_check_exec (compare, "compare");
  if (nmemb * size)
    stubs_chkr_check_addr (base, nmemb * size, CHKR_TW, "base");
  qsort (base, nmemb, size, compare);
}
#endif /* HAVE_qsort */

#ifdef HAVE_abs
int
chkr$abs (int j)
{
#if USE_BI_JUMP
  __builtin_jump (abs);
#else
  return abs (j);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_abs */

#ifdef HAVE_labs
long int
chkr$labs (long int j)
{
#if USE_BI_JUMP
  __builtin_jump (labs);
#else
  return labs (j);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_labs */

#ifdef HAVE_div
div_t
chkr$div (int numer, int denom)
{
#if USE_BI_JUMP
  __builtin_jump (div);
#else
  return div (numer, denom);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_div */

#ifdef HAVE_ldiv
ldiv_t
chkr$ldiv (long int numer, long int denom)
{
#if USE_BI_JUMP
  __builtin_jump (ldiv);
#else
  return ldiv (numer, denom);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ldiv */

#ifdef HAVE_mblen
int
chkr$mblen (const char * arg0, size_t arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (mblen);
#else
  {
    int res;
    res = mblen (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mblen */

#ifdef HAVE_mbtowc
int
chkr$mbtowc (wchar_t * arg0, const char * arg1, size_t arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (wchar_t), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (mbtowc);
#else
  {
    int res;
    res = mbtowc (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mbtowc */

#ifdef HAVE_wctomb
int
chkr$wctomb (char * arg0, wchar_t arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wctomb);
#else
  {
    int res;
    res = wctomb (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wctomb */

#ifdef HAVE_mbstowcs
size_t
chkr$mbstowcs (wchar_t * arg0, const char * arg1, size_t arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (wchar_t), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (mbstowcs);
#else
  {
    size_t res;
    res = mbstowcs (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mbstowcs */

#ifdef HAVE_wcstombs
size_t
chkr$wcstombs (char * arg0, const wchar_t * arg1, size_t arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (wchar_t), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (wcstombs);
#else
  {
    size_t res;
    res = wcstombs (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_wcstombs */

#ifdef HAVE_ecvt
char *
chkr$ecvt (double arg0, size_t arg1, int * arg2, int * arg3)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg2, sizeof (int), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (ecvt);
#else
  {
    char * res;
    res = ecvt (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ecvt */

#ifdef HAVE_fcvt
char *
chkr$fcvt (double arg0, size_t arg1, int * arg2, int * arg3)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg2, sizeof (int), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (int), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (fcvt);
#else
  {
    char * res;
    res = fcvt (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fcvt */

#ifdef HAVE_gcvt
char *
chkr$gcvt (double arg0, size_t arg1, char * arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg2, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (gcvt);
#else
  {
    char * res;
    res = gcvt (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_gcvt */

#ifdef HAVE_drand48
double
chkr$drand48 (void)
{
#if USE_BI_JUMP
  __builtin_jump (drand48);
#else
  return drand48 ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_drand48 */

#ifdef HAVE_erand48
double
chkr$erand48 (short unsigned int xsubi[3])
{
  stubs_chkr_check_addr (xsubi, 3 * sizeof (short unsigned int), CHKR_WO, "xsubi");
#if USE_BI_JUMP
  __builtin_jump (erand48);
#else
  return erand48 (xsubi);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_erand48 */

#ifdef HAVE_lrand48
long int
chkr$lrand48 (void)
{
#if USE_BI_JUMP
  __builtin_jump (lrand48);
#else
  return lrand48 ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_lrand48 */

#ifdef HAVE_nrand48
long int
chkr$nrand48 (short unsigned int xsubi[3])
{
  stubs_chkr_check_addr (xsubi, 3 * sizeof (short unsigned int), CHKR_WO, "xsubi");
#if USE_BI_JUMP
  __builtin_jump (nrand48);
#else
  return nrand48 (xsubi);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_nrand48 */

#ifdef HAVE_mrand48
long int
chkr$mrand48 (void)
{
#if USE_BI_JUMP
  __builtin_jump (mrand48);
#else
  return mrand48 ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_mrand48 */

#ifdef HAVE_jrand48
long int
chkr$jrand48 (short unsigned int xsubi[3])
{
  stubs_chkr_check_addr (xsubi, 3 * sizeof (short unsigned int), CHKR_WO, "xsubi");
#if USE_BI_JUMP
  __builtin_jump (jrand48);
#else
  return jrand48 (xsubi);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_jrand48 */

#ifdef HAVE_srand48
void
chkr$srand48 (long int seedval)
{
#if USE_BI_JUMP
  __builtin_jump (srand48);
#else
  srand48 (seedval);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_srand48 */

#ifdef HAVE_seed48
short unsigned int *
chkr$seed48 (short unsigned int seed16v[3])
{
  short unsigned int *res;
  stubs_chkr_check_addr (seed16v, 3 * sizeof (short unsigned int), CHKR_RO, "seed16v");
  res = seed48 (seed16v);
  if (res)
    stubs_chkr_set_right (seed16v, 3 * sizeof (short unsigned int), CHKR_RW);
  return res;
}
#endif /* HAVE_seed48 */

#ifdef HAVE_lcong48
void
chkr$lcong48 (short unsigned int param[7])
{
  stubs_chkr_check_addr (param, 7 * sizeof (short unsigned int), CHKR_RO, "param");
#if USE_BI_JUMP
  __builtin_jump (lcong48);
#else
  lcong48 (param);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_lcong48 */

#ifdef HAVE_unsetenv
void
chkr$unsetenv (const char *name)
{
  stubs_chkr_check_str (name, CHKR_RO, "name");
#if USE_BI_JUMP
  __builtin_jump (unsetenv);
#else
  unsetenv (name);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_unsetenv */

#ifdef HAVE_insque
void
chkr$insque (struct qelem * arg0, struct qelem * arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct qelem), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (struct qelem), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (insque);
#else
  insque (arg0, arg1);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_insque */

#ifdef HAVE_remque
void
chkr$remque (struct qelem * arg0)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (struct qelem), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (remque);
#else
  remque (arg0);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_remque */

#endif /* HAVE_STDLIB_H */
