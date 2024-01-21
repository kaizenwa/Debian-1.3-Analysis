/* @(#) strpbrk.c,v 1.2 1990/10/24 05:19:31 tron Exp */
/*
 * File   : strpbrk.c
 * Author : Richard A. O'Keefe.
 * Updated: 11 April 1984
 * Defines: strpbrk()
 *
 * strpbrk(s1, s2) returns NullS if no character of s2 occurs in s1, or
 * a pointer to the first character of s1 which occurs in s2  if  there
 * is one.  It generalises strchr (v7=index).  It wouldn't be useful to
 * consider NUL as part of s2, as that would occur in every s1.
 */

#include "strings.h"
#include "_str2set.h"

char *
strpbrk(str, set)
    register _char_ *str;
    char *set;
{
    _str2set(set);
    while (_set_vec[*str] != _set_ctr)
	if (!*str++) return NullS;
    return str;
}
