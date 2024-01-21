/* cfengine for GNU
 
        Copyright (C) 1995/6
        Free Software Foundation, Inc.
 
   This file is part of GNU cfengine - written and maintained 
   by Mark Burgess, Dept of Computing and Engineering, Oslo College,
   Dept. of Theoretical physics, University of Oslo
 
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/


/*********************************************************************/
/*                                                                   */
/*  TOOLKIT: the "2Dlist" object library for cfengine                */
/*           uses (inherits) item.c                                  */
/*                                                                   */
/*********************************************************************/

#include "cf.defs.h"
#include "cf.extern.h"

/* private */

#define TD_wrapped   1
#define TD_nowrap    2

/*********************************************************************/
/* TOOLKIT : 2D list                                                 */
/*********************************************************************/

Set2DList(list)

struct TwoDimList *list;

{ struct TwoDimList *tp;

Debug1("Set2DLIst()\n");

for (tp = list; tp != NULL; tp=tp->next)
   {
   tp->current = tp->ilist;
   }
}

/*********************************************************************/

char *Get2DListEnt(list)

   /* return a path string in static data, like getent in NIS */

struct TwoDimList *list;

{ static char entry[bufsize];
  struct TwoDimList *tp;
  struct Item *ip;

Debug1("Get2DListEnt()\n");

if (EndOfTwoDimList(list))
   {
   return NULL;
   }

bzero(entry,bufsize);

for (tp = list; tp != NULL; tp=tp->next)
   {
   if (tp != list || list->hasslash)
      {
      strcat(entry,"/");
      }
   
   if (tp->current != NULL)
      {
      strcat(entry,(tp->current)->name);
      }
   }

Debug1("Get2DLIstEnt returns %s\n",entry);

IncrementTwoDimList(list,list);
return entry;
}

/*********************************************************************/

Build2DListFromVarstring(TwoDimlist,varstring,sep)

struct TwoDimList **TwoDimlist;
char *varstring, sep;
/* GLOBAL LISTSEPARATOR */

{ char format[6], *sp;
  char node[maxlinksize];
  short has_slash = true;

Debug1("Build2DListFromVarstring(%s)\n",varstring);

if (strcmp("/",varstring) == 0)
   {
   AppendTwoDimItem(TwoDimlist,SplitVarstring("",' '));
   return;
   }

if (*varstring != '/')
   {
   has_slash=false;
   }
  
sprintf(format,"%%[^%c]",sep);   /* set format string to search */

for (sp = varstring; *sp != '\0'; sp++)
   {
   bzero(node,maxlinksize);
   sscanf(sp,format,node);
   
   if (strlen(node) == 0)
      {
      continue;
      } 
   
   sp += strlen(node)-1;

   AppendTwoDimItem(TwoDimlist,SplitVarstring(node,LISTSEPARATOR),has_slash);

   if (*sp == '\0')
      {
      break;
      }
   }
}

/*********************************************************************/

IncrementTwoDimList (from,list)

struct TwoDimList *from, *list;

{ struct TwoDimList *tp;

Debug1("IncrementTwoDimList()\n");

for (tp = from; tp != NULL; tp=tp->next)
   {
   if (tp->is2d)
      {
      break;
      }
   }

if (tp == NULL)
   {
   return TD_wrapped;
   }

if (IncrementTwoDimList(tp->next,list) == TD_wrapped)
   {
   tp->current = (tp->current)->next;

   if (tp->current == NULL)
      {
      tp->current = tp->ilist;
      tp->rounds++;             /* count iterations to ident eolist*/
      return TD_wrapped;
      }
   else
      {
      return TD_nowrap;
      }
   }
}

/*********************************************************************/

EndOfTwoDimList(list)       /* bool */

struct TwoDimList *list;

   /* returns true if the leftmost list variable has cycled */
   /* i.e. rounds is > 0 for the first is-2d list item      */

{ struct TwoDimList *tp;

for (tp = list; tp != NULL; tp=tp->next)
   {
   if (tp->is2d)
       {
       break;
       }
   }

if (list == NULL)
   {
   return true;
   }

if (tp == NULL)             /* Need a case when there are no lists! */
   {
   if (list->rounds == 0)
      {
      list->rounds = 1;
      return false;
      }
   else
      {
      return true;
      }
   }

if (tp->rounds > 0)
    {
    return true;
    }
else
    {
    return false;
    }
}

/*********************************************************************/

AppendTwoDimItem(liststart,itemlist,slash)

struct TwoDimList **liststart;
struct Item *itemlist;
short slash;

{ struct TwoDimList *ip, *lp;

Debug1("AppendTwoDimItem(<item list>)\n");

if ((ip = (struct TwoDimList *)malloc(sizeof(struct TwoDimList))) == NULL)
   {
   perror("Can't allocate memory in AppendTwoDimItem()");
   FatalError("");
   }

if (*liststart == NULL)
   {
   *liststart = ip;
   }
else
   {
   for (lp = *liststart; lp->next != NULL; lp=lp->next)
      {
      }

   lp->next = ip;
   }

ip->ilist = itemlist;
ip->current = itemlist; /* init to start of list */
ip->next = NULL;
ip->rounds = 0;
ip->hasslash = slash;

if (itemlist == NULL || itemlist->next == NULL)
   {
   ip->is2d = false;
   }
else
   {
   ip->is2d = true;
   }
}

/*********************************************************************/

Delete2DList(item)

struct TwoDimList *item;

{
if (item != NULL)
   {
   Delete2DList(item->next);
   item->next = NULL;

   DeleteItemList(item->ilist);
   
   free((char *)item);
   }
}
