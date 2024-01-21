/* ########################################################################

				const.c

   File: const.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/const.c
   Description: 
   Created: Tue Feb 21 12:51:18 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:51:19 MET 1995
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

#include "const.h"
#include "Const.h"
#include "list.h"
#include "mem.h"

/* Fonctions generant les bonnes constantes */


Instruction * make_integer(l)
     List * l;
{
  Int * result = Int__Int((int) l->info);

  free(l);

  return (Instruction *) result;
}

char lire_char(s)
     char * s;
{
  if (*++s == '\\')
    switch (s[1]) {
      
    case 'x' :
      {
	int code;
	
	sscanf(s + 2, "%x", &code);
	return ((char) code);
      }
      
    case 'n' :
      return('\n');
    case 't' :
      return('\t');
    case 'v' :
      return('\v');
    case 'b' :
      return('\b');
    case 'r' :
      return('\r');
    case 'f' :
      return('\f');
    case 'a' :
      /* some compilers produce a warning on '\a', not important */
      return('\a');
    case '\\' :
      return('\\');
    case '?' :
      return('\?');
    case '\'' :
      return('\'');
    case '\"' :
      return('\"');
      
    default :
      {
	int code;

	sscanf(s + 1, "%o", &code);
	return((char) code);
      }
  }

  return(*s);
}

Instruction * make_char(l)
     List * l;
{
  Char * result = Char__Char((int) l->info);

  free(l);

  return (Instruction *) result;
}

char * lire_string(str)
     char * str;
{
#ifdef RUNTIMECHECK
  char * decoded = (char *) RTCMalloc(strlen(str) - 1);
  char * pdecoded = decoded;

  forbit_RTCfree(decoded);
#else
  char * decoded = (char *) Malloc(strlen(str) - 1);
  char * pdecoded = decoded;
#endif

  while (*++str != '\"')
    if (*str == '\\') {
      switch (*++str) {
      case 'x' :
	{
	  int code;

	  sscanf(++str, "%x", &code);
	  *pdecoded++ = (char) code;
	  while (((*str >= 'a') && (*str <= 'f')) ||
		 ((*str >= 'A') && (*str <= 'F')) ||
		 ((*str >= '0') && (*str <= '9')))
	    str += 1;
	}
	break;
      case 'a' :
	 /* some compilers produce a warning on '\a', not important */
	*pdecoded++ = '\a';
	break;
      case 'b' :
	*pdecoded++ = '\b';
	break;
      case 'f' :
	*pdecoded++ = '\f';
	break;
      case 'n' :
	*pdecoded++ = '\n';
	break;
      case 'r' :
	*pdecoded++ = '\r';
	break;
      case 't' :
	*pdecoded++ = '\t';
	break;
      case 'v' :
	*pdecoded++ = '\v';
	break;
      case '\'' :
	*pdecoded++ = '\'';
	break;
      case '\"' :
	*pdecoded++ = '\"';
	break;
      case '\?' :
	*pdecoded++ = '\?';
	break;
      case '\\' :
	*pdecoded++ = '\\';
	break;
      default :
	{
	  int code = 0;
	  
	  while ((*str >= '0') && (*str <= '7'))
	    code = (code << 3) + *str++ - '0';
	  *pdecoded++ = (char) code;
	}
	str -= 1;
	break;
      }
    }
    else
      *pdecoded++ = *str;

  *pdecoded = 0;

  return decoded;
}
     
Instruction * make_string(l)
     List * l;
{
  char * str = (char *) l->info;

  free(l);
  
  return (Instruction *) String__String(str);
}

