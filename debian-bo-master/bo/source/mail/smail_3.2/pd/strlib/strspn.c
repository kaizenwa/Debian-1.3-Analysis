/* @(#) strspn.c,v 1.2 1990/10/24 05:19:33 tron Exp */
/*
 * File   : strspn.c
 * Author : Richard A. O'Keefe.
 * Updated: 11 April 1984
 * Defines: strspn()
 *
 * strspn(s1, s2) returns the  length  of  the  longest  prefix  of  s1
 * consisting  entirely  of characters in s2.  NUL is not considered to
 * be in s2, and _str2set will not include it in the set.
 */

#include "strings.h"
#include "_str2set.h"

int
strspn(str, set)
    register _char_ *str;
    char *set;
{
    register int L;

    _str2set(set);
    for (L = 0; _set_vec[*str++] == _set_ctr; L++) ;
    return L;
}

