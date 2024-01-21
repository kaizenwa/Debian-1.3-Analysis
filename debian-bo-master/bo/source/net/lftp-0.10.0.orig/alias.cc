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

#include <stdlib.h>
#include <stdio.h>

#include "alias.h"
#include "xmalloc.h"

Alias *Alias::base;

Alias::Alias(const char *alias,const char *value,Alias *next)
{
   this->alias=xstrdup(alias);
   this->value=xstrdup(value);
   this->next=next;
}

Alias::~Alias()
{
   free(this->alias);
   free(this->value);
}

void Alias::Add(const char *alias,const char *value)
{
   Alias **scan=&base;
   while(*scan)
   {
      int dif=strcmp((*scan)->alias,alias);
      if(dif==0)
      {
	 free((*scan)->value);
	 (*scan)->value=xstrdup(value);
	 return;
      }
      if(dif>0)
	 break;
      scan=&((*scan)->next);
   }
   *scan=new Alias(alias,value,*scan);
}

void Alias::Del(const char *alias)
{
   Alias **scan=&base;
   while(*scan)
   {
      int dif=strcmp((*scan)->alias,alias);
      if(dif==0)
      {
	 Alias *tmp=(*scan)->next;
	 delete *scan;
	 *scan=tmp;
	 return;
      }
      scan=&((*scan)->next);
   }
}

const char *Alias::Find(const char *alias)
{
   Alias *scan=base;
   while(scan)
   {
      int dif=strcmp(scan->alias,alias);
      if(dif==0)
	 return(scan->value);
      if(dif>0)
	 break;
      scan=scan->next;
   }
   return 0;
}

void Alias::List()
{
   Alias *scan=base;
   while(scan)
   {
      printf("alias %s \"%s\"\n",scan->alias,scan->value);
      scan=scan->next;
   }
}
