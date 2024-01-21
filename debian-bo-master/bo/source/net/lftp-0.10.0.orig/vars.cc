/*
 * lftp and utils
 *
 * Copyright (c) 1996-1997 by Alexander V. Lukyanov (lav@yars.free.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>

#include <string.h>
#include <ctype.h>

#include "vars.h"

/*
 * lala_baba is equal to l-ba if no other matches
 */

typedef const char *cstr;
#define sep(c) (c=='-' || c=='_')

// returns:
//    0 - no match
//    1 - found, if *res==0 then ambiguous

int find_var_name(cstr unprec_name,cstr const *names,cstr *res)
{
   bool curr_unprec;
   cstr match=0;
   for( ; *names; names++)
   {
      cstr s,u;
      u=unprec_name;
      curr_unprec=false;
      for(s=*names; *s; s++)
      {
	 if(*u==0 || (sep(*u) && !sep(*s)))
	 {
	    curr_unprec=true;
	    continue;
	 }
	 if(sep(*s) && !sep(*u))
	    break;	// unmatch
	 if(!(sep(*s) && sep(*u)) && toupper(*u)!=toupper(*s))
	    break;
	 u++;
      }
      if(*s || *u)
	 continue;   // unmatch
      if(curr_unprec)
      {
	 if(match)
	 {
	    *res=0;
	    return 1;
	 }
	 match=*names;
      }
      else
      {
	 *res=*names;
	 return 1;
      }
   }
   if(match)
   {
      *res=match;
      return 1;
   }
   *res=0;
   return 0;
}

bool  var_str2bool(cstr s)
{
   return(strchr("TtYy1+",s[0])!=0 || !strcasecmp(s,"on"));
}

const char *var_bool2str(bool v)
{
   return v?"on":"off";
}
