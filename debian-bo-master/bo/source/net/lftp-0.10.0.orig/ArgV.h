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

#ifndef ARGV_H
#define ARGV_H

#include "getopt.h"

class ArgV
{
   char **v;
   int c;
   int ind;

   void Init(int,const char * const *);

public:
   ArgV() { Init(0,0); }
   void Empty();
   void Append(const char *);
   ArgV(const ArgV& a) { Init(a.c,a.v); }
   ArgV(int new_c,const char * const *new_v) { Init(new_c,new_v); }
   ~ArgV() { Empty(); }

   char *Combine(int start_index=0);

   int getopt(const char *opts)
      { optind=ind; int r=::getopt(c,v,opts); ind=optind; return r; }

   void rewind();
   char *getnext();

   char *getarg(int n)
   {
      if(n>=c)
	 return 0;
      return v[n];
   }
   void back();
   int count() { return c; }
};

#endif ARGV_H
