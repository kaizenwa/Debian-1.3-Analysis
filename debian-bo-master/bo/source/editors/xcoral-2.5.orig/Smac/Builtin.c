/* ########################################################################

			       Builtin.c

   File: Builtin.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Builtin.c
   Description: 
   Created: Tue Feb 21 10:48:40 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:48:40 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */



#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <stdlib.h>
#include <sys/time.h>

#include "Builtin.h"
#include "Identifier.h"
#include "error.h"

/*******************
  
  Operateurs unaires

  *****************/

Function * Builtin_Not;
Function * Builtin_Mono_Minus;
Function * Builtin_Complement;

static Object builtin_not(a)
     Instruction ** a;
{
  return (! (Eval(*a)));
}

static Object builtin_mono_minus(a)
     Instruction ** a;
{
  return (- (Eval(*a)));
}

static Object builtin_complement(a)
     Instruction ** a;
{
  return (~ (Eval(*a)));
}


/********************
  
  Operateurs binaires

  *******************/

HashTable * binary_oper_hashtable;
HashTable * binary_oper_unique_hashtable;
HashTable * binary_special_oper_hashtable;

HashTable * binary_modifoper_hashtable;

/* + */

static Object builtin_int_plus_int(a)
     Instruction ** a;
{
  return Eval(a[0]) + Eval(a[1]);
}

static Object builtin_pvoid_plus_int(a)
     Instruction ** a;
{
  return Eval(a[0]) +
    Eval(a[1]) * Type__Sizeof(Type__Pointed_Type(GetExprType(a[0])));
}

static Object builtin_int_plus_pvoid(a)
     Instruction ** a;
{
  return Eval(a[0]) * Type__Sizeof(Type__Pointed_Type(GetExprType(a[1]))) +
    Eval(a[1]);
}

static Object builtin_modif_int_plus_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 + v2;
}

static Object builtin_modif_pvoid_plus_int(v1, v2, size)
     Object v1;
     Object v2;
     int size;
{
  return v1 + v2 * size;
}

/* builtin_modif_int_plus_pvoid impossible */

/* - */

static Object builtin_int_minus_int(a)
     Instruction ** a;
{
  return Eval(a[0]) - Eval(a[1]);
}

static Object builtin_pvoid_minus_int(a)
     Instruction ** a;
{
  return Eval(a[0]) -
    Eval(a[1]) * Type__Sizeof(Type__Pointed_Type(GetExprType(a[0])));
}

static Object builtin_pvoid_minus_pvoid(a)
     Instruction ** a;
{
  return (Eval(a[0]) - Eval(a[1]))
    / Type__Sizeof(Type__Pointed_Type(GetExprType(a[0])));
}

static Object builtin_modif_int_minus_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 - v2;
}

static Object builtin_modif_pvoid_minus_int(v1, v2, size)
     Object v1;
     Object v2;
     int size;
{
  return v1 - v2 * size;
}

static Object builtin_modif_pvoid_minus_pvoid(v1, v2, size)
     Object v1;
     Object v2;
     int size;
{
  return (v1 - v2) / size;
}


/* * */

static Object builtin_int_times_int(a)
     Instruction ** a;
{
  return Eval(a[0]) * Eval(a[1]);
}

static Object builtin_modif_int_times_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 * v2;
}


/* / */

static Object builtin_int_div_int(a)
     Instruction ** a;
{
#ifdef RUNTIMECHECK
  int div;
  
  return Eval(a[0]) / ((! (div = Eval(a[1])))
		       ? Error("zero divide"), 1
		       : div);
#else
  return Eval(a[0]) / Eval(a[1]);
#endif
}

static Object builtin_modif_int_div_int(v1, v2)
     Object v1;
     Object v2;
{
#ifdef RUNTIMECHECK
  if (! v2) Error("zero divide");
#endif

  return v1 / v2;
}


/* % */

static Object builtin_int_mod_int(a)
     Instruction ** a;
{
#ifdef RUNTIMECHECK
  int div;
  
  return Eval(a[0]) % ((! (div = Eval(a[1])))
		       ? Error("zero divide"), 1
		       : div);
#else
  return Eval(a[0]) % Eval(a[1]);
#endif
}

static Object builtin_modif_int_mod_int(v1, v2)
     Object v1;
     Object v2;
{
#ifdef RUNTIMECHECK
  if (! v2) Error("zero divide");
#endif

  return v1 % v2;
}


/* << */

static Object builtin_int_asl_int(a)
     Instruction ** a;
{
  return Eval(a[0]) << Eval(a[1]);
}

static Object builtin_modif_int_asl_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 << v2;
}


/* >> */

static Object builtin_int_asr_int(a)
     Instruction ** a;
{
  return Eval(a[0]) >> Eval(a[1]);
}

static Object builtin_modif_int_asr_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 >> v2;
}


/* & */

static Object builtin_int_arith_and_int(a)
     Instruction ** a;
{
  return Eval(a[0]) & Eval(a[1]);
}

static Object builtin_modif_int_arith_and_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 & v2;
}


/* | */

static Object builtin_int_arith_or_int(a)
     Instruction ** a;
{
  return Eval(a[0]) | Eval(a[1]);
}

static Object builtin_modif_int_arith_or_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 | v2;
}


/* ^ */

static Object builtin_int_arith_xor_int(a)
     Instruction ** a;
{
  return Eval(a[0]) ^ Eval(a[1]);
}

static Object builtin_modif_int_arith_xor_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 ^ v2;
}


/* && || */

Function * Builtin_cond_and;
Function * Builtin_cond_or;

static Object builtin_cond_and(a)
     Instruction ** a;
{
  return Eval(a[0]) && Eval(a[1]);
}

static Object builtin_modif_cond_and(v1, v2)
     Object v1;
     Object v2;
{
  return v1 && v2;
}

static Object builtin_cond_or(a)
     Instruction ** a;
{
  return Eval(a[0]) || Eval(a[1]);
}

static Object builtin_modif_cond_or(v1, v2)
     Object v1;
     Object v2;
{
  return v1 || v2;
}


/* == != */

Function * Builtin_egal;
Function * Builtin_negal;

static Object builtin_egal(a)
     Instruction ** a;
{
  return Eval(a[0]) == Eval(a[1]);
}

static Object builtin_modif_egal(v1, v2)
     Object v1;
     Object v2;
{
  return v1 == v2;
}


static Object builtin_negal(a)
     Instruction ** a;
{
  return Eval(a[0]) != Eval(a[1]);
}

static Object builtin_modif_negal(v1, v2)
     Object v1;
     Object v2;
{
  return v1 != v2;
}


/* < */

static Object builtin_int_lt_int(a)
     Instruction ** a;
{
  return Eval(a[0]) < Eval(a[1]);
}

static Object builtin_modif_int_lt_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 < v2;
}

static Object builtin_p_lt_p(a)
     Instruction ** a;
{
  return ((unsigned Object) Eval(a[0])) < ((unsigned Object) Eval(a[1]));
}

static Object builtin_modif_p_lt_p(v1, v2)
     unsigned Object v1;
     unsigned Object v2;
{
  return v1 < v2;
}

/* <= */

static Object builtin_int_le_int(a)
     Instruction ** a;
{
  return Eval(a[0]) <= Eval(a[1]);
}

static Object builtin_modif_int_le_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 <= v2;
}

static Object builtin_p_le_p(a)
     Instruction ** a;
{
  return ((unsigned Object) Eval(a[0])) <= ((unsigned Object) Eval(a[1]));
}

static Object builtin_modif_p_le_p(v1, v2)
     unsigned Object v1;
     unsigned Object v2;
{
  return v1 <= v2;
}

/* > */

static Object builtin_int_gt_int(a)
     Instruction ** a;
{
  return Eval(a[0]) > Eval(a[1]);
}

static Object builtin_modif_int_gt_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 > v2;
}

static Object builtin_p_gt_p(a)
     Instruction ** a;
{
  return ((unsigned Object) Eval(a[0])) > ((unsigned Object) Eval(a[1]));
}

static Object builtin_modif_p_gt_p(v1, v2)
     unsigned Object v1;
     unsigned Object v2;
{
  return v1 > v2;
}

/* <= */

static Object builtin_int_ge_int(a)
     Instruction ** a;
{
  return Eval(a[0]) >= Eval(a[1]);
}

static Object builtin_modif_int_ge_int(v1, v2)
     Object v1;
     Object v2;
{
  return v1 >= v2;
}

static Object builtin_p_ge_p(a)
     Instruction ** a;
{
  return ((unsigned Object) Eval(a[0])) >= ((unsigned Object) Eval(a[1]));
}

static Object builtin_modif_p_ge_p(v1, v2)
     unsigned Object v1;
     unsigned Object v2;
{
  return v1 >= v2;
}


/**********
  
  Fonctions

  *********/

/* Nombre d'arg min + 1 des fonctions builtin ayant un nombre variable
   de parametres, typiquement printf */

HashTable * builtin_fct_nbre_arg_min;


/* printf */

#ifdef RUNTIMECHECK

char * CHECK(pt)
     char * pt;
{
  if ((pt < Address_Min) || (pt > Address_Char_Max))
    Error("string access out of your memory area");

  return pt;
}

#else

#define CHECK(x)	(x)

#endif
  
static int builtin_printf(a)
     Instruction ** a;
{
  int result = 0;
  char * fmt = (char *) Eval(*a);

  a += 1;

  while (*CHECK(fmt)) {
    if (*fmt == '%')
      switch (fmt += 1, *CHECK(fmt)) {
      case 0 :
	return result;
	
      case '%' :
	putchar('%');
	fmt += 1;
	break;
	
      default :
	{
	  char * begin = fmt - 1;
	  char save;

	  while (! strchr("diopuxXcs", *CHECK(fmt)))
	    if (! *fmt++) return result;

	  fmt += 1;
	  save = *CHECK(fmt);
	  *fmt = 0;
#ifdef RUNTIMECHECK
	  if (*(fmt - 1) == 's') {
	    char * s = (char *)  Eval(*a);

	    if (s) {
	      char * p = s;
	      
	      while (*CHECK(p)) p += 1;
	    }
	    
	    result += printf(begin, s);
	  }
	  else
	    result += printf(begin, Eval(*a));
#else
	  result += printf(begin, Eval(*a));
#endif
	  *fmt = save;
	  a += 1;
	}
      }
    else
      putchar(*fmt++);
  }
  
  return result;
}

static Object builtin_sprintf(a)
     Instruction ** a;
{
  char * string = (char *) Eval(a[0]);
  char * result = string;
  char * fmt = (char *) Eval(a[1]);

#ifdef RUNTIMECHECK
  check_strlen(fmt);
#endif
  
  a += 2;

  while (*fmt) {
    if (*fmt == '%')
      switch (*++fmt) {
      case 0 :
	*CHECK(string) = 0;
	return (Object) result;
	
      case '%' :
	*CHECK(string) = '%';
	string +=1;
	fmt += 1;
	break;
	
      default :
	{
	  char * begin = fmt - 1;
	  char save;

	  while (! strchr("diopuxXcs", *fmt))
	    if (! *fmt++) {
	      *CHECK(string) = '%';
	      return (Object) result;
	    }

	  save = *++fmt;
	  *fmt = 0;
	  if (*(fmt - 1) == 's') {
	    char * s = (char *)  Eval(*a);

	    if (s) {
	      int strlg;
	      int lg = 0;

#ifdef RUNTIMECHECK
	      strlg = check_strlen(s);
#else
	      strlg = strlen(s);
#endif
	      sscanf(begin + 1, "%d", &lg);
	      while (lg-- > strlg) {
		*CHECK(string) = ' ';
		string += 1;
	      }
	    }
	    else
	      s = "(null)";
	    while (*s) {
	      *CHECK(string) = *s++;
	      string +=1;
	    }
	  }
	  else {
	    char str[128];
	    char * s = str;
	    
	    sprintf(s, begin, Eval(*a));
	    while (*s) {
	      *CHECK(string) = *s++;
	      string +=1;
	    }
	  }
	  result += 1;
	  *fmt = save;
	  a += 1;
	}
      }
    else {
      *CHECK(string) = *fmt++;
      string +=1;
    }
  }
  
  *CHECK(string) = 0;
  return (Object) result;
}

/* allocations / liberations memoire */

#ifdef RUNTIMECHECK

static Object builtin_RTCcalloc(a)
     Instruction ** a;
{
  return (Object) RTCCalloc((int) Eval(a[0]), (int) Eval(a[1]));
}

static Object builtin_RTCmalloc(a)
     Instruction ** a;
{
  return (Object) RTCMalloc((int) Eval(*a));
}

static Object builtin_RTCfree(a)
     Instruction ** a;
{
  RTCFree((char *) Eval(*a));
  return 0;
}

#else

static Object builtin_calloc(a)
     Instruction ** a;
{
  return (Object) calloc((int) Eval(a[0]), (int) Eval(a[1]));
}

static Object builtin_malloc(a)
     Instruction ** a;
{
  return (Object) malloc((int) Eval(*a));
}

static Object builtin_free(a)
     Instruction ** a;
{
  free((char *) Eval(*a));
  return 0;
}

#endif


/* chaines de caracteres */

#ifdef RUNTIMECHECK

static Object builtin_strcat(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  char * p = s1;
  
  while ((*CHECK(p)) != 0) p += 1;
  while ((*CHECK(p++) = *CHECK(s2++)) != 0);

  return (Object) s1;
}

static Object builtin_strchr(a)
     Instruction ** a;
{
  char * s = (char *) Eval(a[0]);
  int c = Eval(a[1]);

  while (*CHECK(s) != c)
    if (! *s++)
      return 0;

  return (Object) s;
}

static Object builtin_strcmp(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  
  while (*CHECK(s1++) == *CHECK(s2))
    if (! *s2++)
      return 0;

  return (*(s1-1) < *s2) ? -1 : 1;
}

static Object builtin_strcpy(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  char * p = s1;
  
  while ((*CHECK(p++) = *CHECK(s2++)) != 0);

  return (Object) s1;
}

static Object builtin_strcspn(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  char * p;
  int result = 0;
  char c;

  /* Verifie  une fois pour toute s2 */
  p = s2;
  while ((*CHECK(p++)) != 0);

  while ((c = *CHECK(s1++)) != 0) {
    for (p = s2; *p; )
      if (*p++ == c)
	return result;

    result += 1;
  }
  
  return result;
}

static Object builtin_strlen(a)
     Instruction ** a;
{
  char * s = (char *) Eval(*a);
  char * begin = s;

  while (*CHECK(s++));

  return (s - begin - 1);
}

int check_strlen(s)
     char *s;
{
  char * begin = s;

  while (*CHECK(s++));
  return (s - begin - 1);
}

static Object builtin_strdup(a)
     Instruction ** a;
{
  char * s = (char *) Eval(*a);
  char * result = (char *) RTCMalloc(check_strlen(s) + 1);

  if (result) {
    char * src = s;
    char * dest = result;

    while ((*dest++ = *src++) != 0);
  }

  return (Object) result;
}

static Object builtin_strncat(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  int n = Eval(a[2]);
  char * p = s1;
  
  while (*CHECK(p)) p += 1;
  while ((n-- > 0) && (*CHECK(p++) = *CHECK(s2++)));

  return (Object) s1;
}

static Object builtin_strncmp(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  int n = Eval(a[2]);
  
  while ((n-- > 0) && (*CHECK(s1++) == *CHECK(s2)))
    if (! *s2++)
      return 0;

  return (n < 0) ? 0 : (*(s1-1) < *s2) ? -1 : 1;
}

static Object builtin_strncpy(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  int n = Eval(a[2]);
  char * p = s1;
  
  while ((n-- > 0) && (*CHECK(p++) = *CHECK(s2++)));

  return (Object) s1;
}

static Object builtin_strpbrk(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  char * p;
  char c;

  /* Verifie  une fois pour toute s2 */
  p = s2;
  while ((*CHECK(p++)) != 0);

  while ((c = *CHECK(s1++)) != 0)
    for (p = s2; *p; )
      if (*p++ == c)
	return (Object) s1 - 1;
  
  return 0;
}

static Object builtin_strrchr(a)
     Instruction ** a;
{
  char * s = (char *) Eval(a[0]);
  int c = Eval(a[1]);
  char * result = 0;
  
  while (*CHECK(s))
    if (*s++ == c)
      result = s - 1;

  return (! c) ? (Object) s : (Object) result;
}

static Object builtin_strspn(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  char * p;
  int result = 0;
  char c;

  /* Verifie une fois pour toute s2 */
  p = s2;
  while ((*CHECK(p++)) != 0);

  while ((c = *CHECK(s1++)) != 0) {
    for (p = s2; *p != c; )
      if (! *p++)
	return result;

    result += 1;
  }

  return result;
}

static Object builtin_strstr(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  char * p1, * p2;
  int l1 = check_strlen(s1);
  int l2 = check_strlen(s2);

  while (l1 >= l2) {
    p1 = s1;
    p2 = s2;
    
    while (*CHECK(p1++) == *CHECK(p2))
      if (! *p2++) return (Object) s1;

    if (! *p2) return (Object) s1;

    s1 += 1;
    l1 -= 1;
  }

  return 0;
}

static Object builtin_strtok(a)
     Instruction ** a;
{
  static char * memo = 0;
  static removedchar = 0;
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  char * p, c;

  if (s1)
    memo = s1;
  else
    *CHECK(memo) = removedchar;

  /* Verifie  une fois pour toute s2 */
  p = s2;
  while ((*CHECK(p++)) != 0);

  /* saute les separateurs du debut */
  
  while ((c = *CHECK(memo)) != 0) {
    for (p = s2; (*p) && (*p != c); p += 1);
    if (! *p) break;
    memo += 1;
  }
   if (! c) return 0;

  /* le resultat */

  s1 = memo;

  /* Cherche la fin du mot */

  while ((c = *CHECK(++memo)) != 0) {
    for (p = s2; (*p) && (*p != c); p += 1);
    if (*p) {
      *memo = 0;
      break;
    }
  }

  removedchar = c;
  
  return (Object) s1;
}

static Object builtin_memcpy(a)
     Instruction ** a;
{
  char * s1 = (char *) Eval(a[0]);
  char * s2 = (char *) Eval(a[1]);
  int n = Eval(a[2]);
  char * p = s1;
  
  while (n-- > 0) *CHECK(p++) = *CHECK(s2++);

  return (Object) s1;
}

#else

static Object builtin_strcat(a)
     Instruction ** a;
{
  return (Object) strcat((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_strchr(a)
     Instruction ** a;
{
  return (Object) strchr((char *) Eval(a[0]), (int) Eval(a[1]));
}

static Object builtin_strcmp(a)
     Instruction ** a;
{
  return (Object) strcmp((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_strcpy(a)
     Instruction ** a;
{
  return (Object) strcpy((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_strcspn(a)
     Instruction ** a;
{
  return (Object) strcspn((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_strlen(a)
     Instruction ** a;
{
  return (Object) strlen((char *) Eval(*a));
}

static Object builtin_strdup(a)
     Instruction ** a;
{
  return (Object) stringdup((char *) Eval(*a));
}

static Object builtin_strncat(a)
     Instruction ** a;
{
  return (Object) strncat((char *) Eval(a[0]), (char *) Eval(a[1]),
			  (int) Eval(a[2]));
}

static Object builtin_strncmp(a)
     Instruction ** a;
{
  return (Object) strncmp((char *) Eval(a[0]), (char *) Eval(a[1]),
			  (int) Eval(a[2]));
}

static Object builtin_strncpy(a)
     Instruction ** a;
{
  return (Object) strncpy((char *) Eval(a[0]), (char *) Eval(a[1]),
			  (int) Eval(a[2]));
}

static Object builtin_strpbrk(a)
     Instruction ** a;
{
  return (Object) strpbrk((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_strrchr(a)
     Instruction ** a;
{
  return (Object) strrchr((char *) Eval(a[0]), (int) Eval(a[1]));
}

static Object builtin_strspn(a)
     Instruction ** a;
{
  return (Object) strspn((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_strstr(a)
     Instruction ** a;
{
  return (Object) strstr((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_strtok(a)
     Instruction ** a;
{
  return (Object) strtok((char *) Eval(a[0]), (char *) Eval(a[1]));
}

static Object builtin_memcpy(a)
     Instruction ** a;
{
  return (Object) memcpy((char *) Eval(a[0]), (char *) Eval(a[1]),
			 (int) Eval(a[2]));
}

#endif


/* interruption de l'execution */

static Object builtin_error(a)
     Instruction ** a;
{
  char * msg = (char *) Eval(*a);
  
#ifdef RUNTIMECHECK
  check_strlen(msg);
#endif

  Error(msg);
  
  return 0;			/* pour le compilo */
}

static Object builtin_showed_stack_size(a)
     Instruction ** a;
{
  Showed_Stack_Size = (int) Eval(*a);
  
  return 0;			/* pour le compilo */
}

/* chargement de fichier */

static Object builtin_load_file(a)
     Instruction ** a;
{
  extern FILE * yyin;
  extern int yyinflag;
  extern void load_file();
  extern void smac_read_new_file();
  
  extern int numlig;
  extern char * yyinfilename;
  FILE * previous_yyin = yyin;
  int previous_yyinflag = yyinflag;
  int previous_numlig = numlig;
  char * previous_yyinfilename = yyinfilename;
  char *smaclib = 0;
#ifdef XCORAL_LIB_DIR
  char * xcoral_lib = 0;
#endif
  yyinfilename = (char *) Eval(*a);
  
#ifdef RUNTIMECHECK
  check_strlen(yyinfilename);
#endif

  if (*yyinfilename == '~') {
    FCT(char *, ExpandTildeName,(char *));

    yyinfilename = ExpandTildeName(yyinfilename);
  }

/*  if (((yyin = fopen(yyinfilename, "r")) == 0) && (*yyinfilename != '~')) { */
  if ((yyin = fopen(yyinfilename, "r")) == 0) {
#ifdef XCORAL_LIB_DIR
    smaclib = (char *) getenv ("XCORAL_SMACLIB");
    if (!smaclib) {
      xcoral_lib = malloc(strlen(XCORAL_LIB_DIR) + strlen(yyinfilename) + 2);
      sprintf(xcoral_lib, "%s/%s", XCORAL_LIB_DIR, yyinfilename);
    }
    else {
      xcoral_lib = malloc(strlen(smaclib) + strlen(yyinfilename) + 2);
      sprintf(xcoral_lib, "%s/%s", smaclib, yyinfilename);
    }
    
    if ((yyin = fopen(xcoral_lib, "r")) != 0)
      yyinfilename = xcoral_lib;
    else {
      free(xcoral_lib);
#else
    {
#endif
      sprintf(err_msg, "cannot open %s", yyinfilename);
      yyin = previous_yyin;
      yyinfilename = previous_yyinfilename;
      Error(err_msg);
    }
  }
  yyinflag = 1;
  numlig = 0;
  {
    jmp_buf save_env;

    COPY_ENV(save_env, come_back);
    switch (setjmp(come_back)) {
    case NO_JMP:
      load_file();
      break;
    case JMP_ERROR:
      /* Erreur d'execution */
      fclose(yyin);
      yyin = previous_yyin;
      COPY_ENV(come_back, save_env);
      longjmp(come_back, JMP_ERROR);
    }
    COPY_ENV(come_back, save_env);
  }
#ifdef XCORAL_LIB_DIR
  if (xcoral_lib) free(xcoral_lib);
#endif
  fclose(yyin);
  yyin = previous_yyin;
  yyinflag = previous_yyinflag;
  numlig = previous_numlig;
  yyinfilename = previous_yyinfilename;

  if (*err_msg)
    /* Erreur de lecture */
    Error(err_msg);

#ifndef mylex
  smac_read_new_file();
#endif
  accept_ctrl_c();		/* retire par load_file */
  
  return 0;						/* pour le compilo */
}

/* rend une fonction indefinie comme si elle avait simplement ete declaree */

static Object builtin_remove_function_definition(a)
     Instruction ** a;
{
  Function * pf = (Function *) Eval(*a);
  
  /* Verifie qu'il s'agit bien d'une fonction de l'utilisateur */

  if (IsaPointerToFunction(pf) && (! Function__IsBuiltin(pf))) {

    /* Verifie que la fonction n'est pas en cours d'execution */
    Function__VerifyNotActive(pf);
    
    /* Liberer l'ancienne definition */
    pf->_def._user_def =
      (Instruction *) UndefinedFunctionCall__UndefinedFunctionCall(pf);
  }
  else
    Error("remove_function_definition argument is not a user function pointer\n");
  
  return 0;						/* pour le compilo */
}


/* le passage ou la sortie du mode debug */

static Object builtin_debug_mode(a)
     Instruction ** a;
{
  Debug_Mode = Eval(*a);
  
  return 0;						/* pour le compilo */
}


/* Pour avoir une idee des temps d'executions */

#include <signal.h>
#include <sys/time.h>

static int profile_count;
static int profiling;

void profile_tic(ignore)
     int ignore;
{
  if (Last_Function_Called) {
    profile_count += 1;
    ((Function *) Last_Function_Called->last_function_called)->_profile_count
      += 1;
  }
}

static Object builtin_start_profile(ignore)
     Instruction ** ignore;
{
  static struct itimerval prof;
  HashItem * item = 0;
  HashItem ** entry = 0;
  int result = 0;

  while (HashTable_Iter(IdentifierHashTable, &item, &entry), item) {
    Function * f;
    
    if ((f = Identifier__FunctionDef((Identifier *) HashItem__Data(item))) &&
	(! Function__IsBuiltin(f))) {
      result += 1;
      f->_profile_count = 0;
    }
  }
  
  prof.it_interval.tv_sec = prof.it_value.tv_sec = 0;
  prof.it_interval.tv_usec = prof.it_value.tv_usec = 1;

  /* May be a warning (prototype is not always the same), not important */
  signal(SIGVTALRM, profile_tic);
  setitimer(ITIMER_VIRTUAL, &prof, 0);

  profiling = 1;
  profile_count = 0;
  
  return result;
}

static Object builtin_stop_profile(ignore)
     Instruction ** ignore;
{
  signal(SIGVTALRM, SIG_IGN);
  profiling = 0;

  return profile_count;
}

static void err_pas_fonc(a, f)
     Instruction ** a;
     char * f;
{
  sprintf(err_msg, "%s argument type is ", f);
  Type__Print(GetExprType(*a), err_msg);
  strcat(err_msg, ", not a function");
  Error(err_msg);
}

static Object builtin_function_percent(a)
     Instruction ** a;
{
  Function * func = (Function *) Eval(*a);

  if (IsaFunction((Instruction *) func))
    return ((! Function__IsBuiltin(func)) && profile_count)
      ? (func->_profile_count * 100) / profile_count
      : 0;

  err_pas_fonc(a, "function_percent");
  return 0;	/* pour le compilo */
}

#ifdef XCORAL
  
/* pour que la mesure ne se fasse que sous smac */
  
void stop_profile()
{
  if (profiling)
    signal(SIGVTALRM, SIG_IGN);
}

void rerun_profile()
{
  if (profiling)
    /* May be a warning (prototype is not always the same), not important */
    signal(SIGVTALRM, profile_tic);
}

#endif

/* A propos des fonctions */

static Object builtin_function(a)
     Instruction ** a;
{
  char * name = (char *) Eval(*a);
  Identifier * id;
  
#ifdef RUNTIMECHECK
  check_strlen(name);
#endif
  return (name && (id = find_identifier(name)))
    ? (Object) Identifier__FunctionDef(id)
    : (Object) 0;
}

static Object builtin_function_name(a)
     Instruction ** a;
{
  Function * func = (Function *) Eval(*a);

  if (IsaFunction((Instruction *) func)) {
    char * func_name = Function__Name(func);
    char * result = (char *) RTCMalloc(strlen(func_name) + 1);
    
    if (result) {
      char * dest = result;
      
      while ((*dest++ = *func_name++) != 0);
    }
    return (Object) result;
  }
  
  err_pas_fonc(a, "function_name");
  return 0;	/* pour le compilo */
}

static Object builtin_function_arg_count(a)
     Instruction ** a;
{
  Function * func = (Function *) Eval(*a);

  if (IsaFunction((Instruction *) func))
    return Function__NbreArg(func);

  err_pas_fonc(a, "function_arg_count");
  return 0;	/* pour le compilo */
}

static Object builtin_function_is_builtin(a)
     Instruction ** a;
{
  Function * func = (Function *) Eval(*a);

  if (IsaFunction((Instruction *) func))
    return (Function__IsBuiltin(func)) ? 1 : 0;

  err_pas_fonc(a, "function_is_builtin");
  return 0;	/* pour le compilo */
}

static Object builtin_function_type(a)
     Instruction ** a;
{
  Function * func = (Function *) Eval(*a);

  if (IsaFunction((Instruction *) func)) {
    char * result;
    
    *err_msg = 0;
    Type__Print(GetExprType(func), err_msg);
    result = (char *) RTCMalloc(strlen(err_msg) + 1);
    
    if (result) {
      char * dest = result;
      char * dep = err_msg;
      
      while ((*dest++ = *dep++) != 0);
    }
    *err_msg = 0;
    return (Object) result;
  }

  err_pas_fonc(a, "function_type");
  return 0;	/* pour le compilo */
}
  
static HashItem * fi_item = 0;
static HashItem ** fi_entry = 0;
static int fi_end = 1;

static Object builtin_init_function_list(ignore)
     Instruction ** ignore;
{
  fi_item = 0;
  fi_entry = 0;
  fi_end = 0;

  return 0;
}

static Object builtin_function_list(ignore)
     Instruction ** ignore;
{
  if (! fi_end)
    while (HashTable_Iter(IdentifierHashTable, &fi_item, &fi_entry), fi_item) {
      Function * f;
      
      if (f = Identifier__FunctionDef((Identifier *) HashItem__Data(fi_item)))
	return (Object) f;
    }
  
  fi_end = 1;
  return 0;
}

/**/

Object new_builtin(name, typeresult, narg, argstype, fct)
     char * name;
     Type * typeresult;
     int narg;
     Type ** argstype;
     FCT (Object, (*fct),(Instruction **));
{
  Function * function =
      Function__Function(name, typeresult, narg, argstype, (Object) fct);

  Identifier__Identifier(name, function, 0);

  return (Object) function;
}


/* Pour eviter d'ecrire les cast des pointeurs de fonction en (void *) */

#define FUNCTION__FUNCTION(a, b, c,d ,e)	\
    Function__Function(a, b, c, d, (Object) e)
#define HASHTABLE__ADD(a, b, c)			\
    HashTable__Add(a, (Object) b, (Object) c)

    
void Init_Builtin()
{
  static Type * pvoid;

  pvoid = Type__Type(Type_Void, T_Pointer, sizeof(char *));
  
  Builtin_Not =
    FUNCTION__FUNCTION("!", Type_Int, 1, 0, builtin_not);
  
  Builtin_Mono_Minus =
    FUNCTION__FUNCTION("-", Type_Int, 1, 0, builtin_mono_minus);
  
  Builtin_Complement =
    FUNCTION__FUNCTION("~", Type_Int, 1, 0, builtin_complement);

  /* */

  binary_oper_hashtable = HashTable__HashTable(60, 1);
  binary_special_oper_hashtable = HashTable__HashTable(60, 1);
  binary_modifoper_hashtable = HashTable__HashTable(60, 0);

  /* + */
  
  {
    Function * Builtin_int_plus_int =
      FUNCTION__FUNCTION("+", Type_Int, 2, 0, builtin_int_plus_int);
    Function * Builtin_int_plus_pvoid =
      FUNCTION__FUNCTION("+", 0, 2, 0, builtin_int_plus_pvoid);
    Function * Builtin_pvoid_plus_int =
      FUNCTION__FUNCTION("+", 0, 2, 0, builtin_pvoid_plus_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int + int", Builtin_int_plus_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_plus_int,
		   builtin_modif_int_plus_int);
    
    HASHTABLE__ADD(binary_special_oper_hashtable, "<char *> + int",
		   Builtin_int_plus_int);
    HASHTABLE__ADD(binary_special_oper_hashtable, "int + <char *>",
		   Builtin_int_plus_int);
    
    HASHTABLE__ADD(binary_special_oper_hashtable, "int + <void *>",
		   Builtin_int_plus_pvoid);
    HASHTABLE__ADD(binary_special_oper_hashtable, "<void *> + int",
		   Builtin_pvoid_plus_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_pvoid_plus_int,
		   builtin_modif_pvoid_plus_int);
  }
  
  /* - */

  {
    Function * Builtin_int_minus_int =
      FUNCTION__FUNCTION("-", Type_Int, 2, 0, builtin_int_minus_int);
    Function * Builtin_pvoid_minus_pvoid =
      FUNCTION__FUNCTION("-", Type_Int, 2, 0, builtin_pvoid_minus_pvoid);
    Function * Builtin_pvoid_minus_int =
      FUNCTION__FUNCTION("-", 0, 2, 0, builtin_pvoid_minus_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int - int",
		   Builtin_int_minus_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_minus_int,
		   builtin_modif_int_minus_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "<char *> - <char *>",
		   Builtin_int_minus_int);
    HASHTABLE__ADD(binary_special_oper_hashtable, "<char *> - int",
		   Builtin_int_minus_int);
    
    HASHTABLE__ADD(binary_special_oper_hashtable, "<void *> - <void *>",
		   Builtin_pvoid_minus_pvoid);
    HASHTABLE__ADD(binary_special_oper_hashtable, "<void *> - int",
		   Builtin_pvoid_minus_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_pvoid_minus_pvoid,
		   builtin_modif_pvoid_minus_pvoid);
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_pvoid_minus_int,
		   builtin_modif_pvoid_minus_int);
  }
  
  /* * */

  {
    Function * Builtin_int_times_int =
      FUNCTION__FUNCTION("*", Type_Int,2 ,0, builtin_int_times_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int * int",
		   Builtin_int_times_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_times_int,
		   builtin_modif_int_times_int);
  }
  
  /* / */

  {
    Function * Builtin_int_div_int =
      FUNCTION__FUNCTION("/", Type_Int,2 ,0, builtin_int_div_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int / int", Builtin_int_div_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_div_int,
		   builtin_modif_int_div_int);
  }
  
  /* % */

  {
    Function * Builtin_int_mod_int =
      FUNCTION__FUNCTION("%", Type_Int,2 ,0, builtin_int_mod_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int % int", Builtin_int_mod_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_mod_int,
		   builtin_modif_int_mod_int);
  }
  
  /* << */

  {
    Function * Builtin_int_asl_int =
      FUNCTION__FUNCTION("<<", Type_Int,2 ,0, builtin_int_asl_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int << int", Builtin_int_asl_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_asl_int,
		   builtin_modif_int_asl_int);
  }  
  
  /* >> */

  {
    Function * Builtin_int_asr_int =
      FUNCTION__FUNCTION(">>", Type_Int,2 ,0, builtin_int_asr_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int >> int", Builtin_int_asr_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_asr_int,
		   builtin_modif_int_asr_int);
  }  

  /* & */

  {
    Function * Builtin_int_arith_and_int =
      FUNCTION__FUNCTION("&", Type_Int,2 ,0, builtin_int_arith_and_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int & int",
		   Builtin_int_arith_and_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_arith_and_int,
		   builtin_modif_int_arith_and_int);
  }
  
  /* | */

  {
    Function * Builtin_int_arith_or_int =
      FUNCTION__FUNCTION("|", Type_Int,2 ,0, builtin_int_arith_or_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int | int",
		   Builtin_int_arith_or_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_arith_or_int,
		   builtin_modif_int_arith_or_int);
  }

  /* ^ */

  {
    Function * Builtin_int_arith_xor_int =
      FUNCTION__FUNCTION("^", Type_Int,2 ,0, builtin_int_arith_xor_int);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int ^ int",
		   Builtin_int_arith_xor_int);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_arith_xor_int,
		   builtin_modif_int_arith_xor_int);
  }

  /* && || */

  Builtin_cond_and =
      FUNCTION__FUNCTION("&&", Type_Int,2 ,0, builtin_cond_and);
  Builtin_cond_or =
      FUNCTION__FUNCTION("||", Type_Int,2 ,0, builtin_cond_or);

  HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_cond_and,
		 builtin_modif_cond_and);
  HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_cond_or,
		 builtin_modif_cond_or);
  
  /* == != */

  Builtin_egal = FUNCTION__FUNCTION("==", Type_Int, 2, 0, builtin_egal);
  Builtin_negal = FUNCTION__FUNCTION("==", Type_Int, 2, 0, builtin_negal);

  HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_egal,
		 builtin_modif_egal);
  HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_negal,
		 builtin_modif_negal);
  /* < */

  {
    Function * Builtin_int_lt_int =
      FUNCTION__FUNCTION("<", Type_Int, 2, 0, builtin_int_lt_int);
    Function * Builtin_p_lt_p =
      FUNCTION__FUNCTION("<", Type_Int, 2, 0, builtin_p_lt_p);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int < int", Builtin_int_lt_int);
    HASHTABLE__ADD(binary_special_oper_hashtable, "<void *> < <void *>",
		   Builtin_p_lt_p);
    HASHTABLE__ADD(binary_oper_hashtable, "<char *> < <char *>",
		   Builtin_p_lt_p);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_lt_int,
		   builtin_modif_int_lt_int);
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_p_lt_p,
		   builtin_modif_p_lt_p);
  }
  
  /* <= */

  {
    Function * Builtin_int_le_int =
      FUNCTION__FUNCTION("<=", Type_Int, 2, 0, builtin_int_le_int);
    Function * Builtin_p_le_p =
      FUNCTION__FUNCTION("<=", Type_Int, 2, 0, builtin_p_le_p);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int <= int", Builtin_int_le_int);
    HASHTABLE__ADD(binary_special_oper_hashtable, "<void *> <= <void *>",
		   Builtin_p_le_p);
    HASHTABLE__ADD(binary_oper_hashtable, "<char *> <= <char *>",
		   Builtin_p_le_p);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_le_int,
		   builtin_modif_int_le_int);
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_p_le_p,
		   builtin_modif_p_le_p);
  }

  /* > */

  {
    Function * Builtin_int_gt_int =
      FUNCTION__FUNCTION(">", Type_Int, 2, 0, builtin_int_gt_int);
    Function * Builtin_p_gt_p =
      FUNCTION__FUNCTION(">", Type_Int, 2, 0, builtin_p_gt_p);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int > int", Builtin_int_gt_int);
    HASHTABLE__ADD(binary_special_oper_hashtable, "<void *> > <void *>",
		   Builtin_p_gt_p);
    HASHTABLE__ADD(binary_oper_hashtable, "<char *> > <char *>",
		   Builtin_p_gt_p);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_gt_int,
		   builtin_modif_int_gt_int);
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_p_gt_p,
		   builtin_modif_p_gt_p);
  }
  
  /* >= */

  {
    Function * Builtin_int_ge_int =
      FUNCTION__FUNCTION(">=", Type_Int, 2, 0, builtin_int_ge_int);
    Function * Builtin_p_ge_p =
      FUNCTION__FUNCTION(">=", Type_Int, 2, 0, builtin_p_ge_p);
    
    HASHTABLE__ADD(binary_oper_hashtable, "int >= int", Builtin_int_ge_int);
    HASHTABLE__ADD(binary_special_oper_hashtable, "<void *> >= <void *>",
		   Builtin_p_ge_p);
    HASHTABLE__ADD(binary_oper_hashtable, "<char *> >= <char *>",
		   Builtin_p_ge_p);
    
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_int_ge_int,
		   builtin_modif_int_ge_int);
    HASHTABLE__ADD(binary_modifoper_hashtable, Builtin_p_ge_p,
		   builtin_modif_p_ge_p);
  }
  
  /* printf */

  builtin_fct_nbre_arg_min = HashTable__HashTable(10, 0);
  
  HashTable__Add(builtin_fct_nbre_arg_min,
		 new_builtin("printf", Type_Int, 1, &Type_String,
			     builtin_printf),
		 1 + 1);

  {
    static Type * string_string[2];

    string_string[0] = Type_String;
    string_string[1] = Type_String;
    
    HashTable__Add(builtin_fct_nbre_arg_min,
		   new_builtin("sprintf", Type_String, 2,
			       string_string, builtin_sprintf),
		   2 + 1);
  }

  /* allocations / liberations memoire */
  
  {
    static Type * calloc_type_args[2];
    
    calloc_type_args[0] = calloc_type_args[1] = Type_Int;
#ifdef RUNTIMECHECK
    new_builtin("calloc", pvoid, 2, calloc_type_args, builtin_RTCcalloc);

    new_builtin("malloc", pvoid, 1, &Type_Int, builtin_RTCmalloc);
    
    new_builtin("free", Type_Void, 1, &pvoid, builtin_RTCfree);
#else
    new_builtin("calloc", pvoid, 2, calloc_type_args, builtin_calloc);
    
    new_builtin("malloc", pvoid, 1, &Type_Int, builtin_malloc);
    
    new_builtin("free", Type_Void, 1, &pvoid, builtin_free);
#endif
  }

  /* Chaine de caracteres */
  {
    static Type * ssi[3];

    ssi[0] = ssi[1] = Type_String;
    ssi[2] = Type_Int;
  
    new_builtin("strcat", Type_String, 2, ssi, builtin_strcat);
    new_builtin("strchr", Type_String, 2, ssi+1, builtin_strchr);
    new_builtin("strcmp", Type_Int, 2, ssi, builtin_strcmp);
    new_builtin("strcpy", Type_String, 2, ssi, builtin_strcpy);
    new_builtin("strcspn", Type_String, 2, ssi, builtin_strcspn);
    new_builtin("strdup", Type_String, 1, ssi, builtin_strdup);
    new_builtin("strlen", Type_Int, 1, ssi, builtin_strlen);
    new_builtin("strncat", Type_String, 3, ssi, builtin_strncat);
    new_builtin("strncmp", Type_Int, 3, ssi, builtin_strncmp);
    new_builtin("strncpy", Type_String, 3, ssi, builtin_strncpy);
    new_builtin("strpbrk", Type_String, 2, ssi, builtin_strpbrk);
    new_builtin("strrchr", Type_String, 2, ssi+1, builtin_strrchr);
    new_builtin("strspn", Type_Int, 2, ssi, builtin_strspn);
    new_builtin("strstr", Type_String, 2, ssi, builtin_strstr);
    new_builtin("strtok", Type_String, 2, ssi, builtin_strtok);
    new_builtin("index", Type_String, 2, ssi+1, builtin_strchr);
    new_builtin("rindex", Type_String, 2, ssi+1, builtin_strrchr);

    new_builtin("memcpy", Type_String, 3, ssi, builtin_memcpy);
  }

  /* error */

  new_builtin("error", Type_Void, 1, &Type_String, builtin_error);
  new_builtin("showed_stack_size", Type_Void, 1, &Type_Int,
	      (FCT(Object, (*),(Instruction**))) builtin_showed_stack_size);

  /* load_file */

  new_builtin("load_file", Type_Void, 1, &Type_String, builtin_load_file);

  /* remove_function_definition */

  new_builtin("remove_function_definition",
	      Type_Void, 1, &pvoid, builtin_remove_function_definition);

  /* debug_mode */

  new_builtin("debug_mode", Type_Void, 1, &Type_Int, builtin_debug_mode);

  /* fonctions */

  new_builtin("function", pvoid, 1, &Type_String, builtin_function);
  new_builtin("function_name", Type_String, 1, &pvoid, builtin_function_name);
  new_builtin("function_arg_count", Type_Int, 1, &pvoid,
	      builtin_function_arg_count);
  new_builtin("function_is_builtin", Type_Int, 1, &pvoid,
	      builtin_function_is_builtin);
  new_builtin("function_type", Type_String, 1, &pvoid, builtin_function_type);
  new_builtin("init_function_list", Type_Void, 0, 0,
	      builtin_init_function_list);
  new_builtin("function_list", pvoid, 0, 0, builtin_function_list);
  
  /* tps d'exec */
    
  new_builtin("start_profile", Type_Int, 0, 0, builtin_start_profile);
  new_builtin("stop_profile", Type_Int, 0, 0, builtin_stop_profile);
  new_builtin("function_percent", Type_Int, 1, &pvoid,
	      builtin_function_percent);
}

