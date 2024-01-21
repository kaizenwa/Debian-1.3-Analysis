/* @(#) strcspn.c,v 1.2 1990/10/24 05:19:27 tron Exp */
/*
 * File   : strcspn.c
 * Author : Richard A. O'Keefe.
 * Updated: 11 April 1984
 * Defines: strspn()
 *
 * strcspn(s1, s2) returns the length  of  the  longest  prefix  of  s1
 * consisting  entirely  of  characters  which  are  NOT  in s2 ("c" is
 * "complement").  NUL is considered to be part  of  s2.   As  _str2set
 * will never include NUL in a set, we have to check for it explicitly.
 */

#include "strings.h"
#include "_str2set.h"

int strcspn(str, set)
    register _char_ *str;
    char *set;
{
    register int L;

    _str2set(set);
    for (L = 0; *str && _set_vec[*str++] != _set_ctr; L++) ;
    return L;
}
