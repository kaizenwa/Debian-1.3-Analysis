/* ########################################################################

				 list.c

   File: list.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/list.c
   Description: 
   Created: Tue Feb 21 12:56:30 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:56:31 MET 1995
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



#include "list.h"

/* allocation */

List * List__List(fct, info)
     InstrFct fct;
     void * info;
{
  List * result = (List *) Malloc(sizeof(List));

  result->fct = fct;
  result->info = info;

  result->previous = 0;
  result->next = 0;

  return result;
}


/* Ajoute en fin de liste */

List * List__AddLast(endlist, newcell)
     List * endlist;
     List * newcell;
{
  if ((newcell->previous = endlist) != 0)
    endlist->next = newcell;

  return newcell;
}


/* Insere avant la cellule */

List * List__InsertBefore(list, newcell)
     List * list;
     List * newcell;
{
  if ((newcell->previous = list->previous) != 0)
    list->previous->next = newcell;
  list->previous = newcell;
  newcell->next = list;

  return newcell;
}


/* Insere apres la cellule */

List * List__InsertAfter(list, newcell)
     List * list;
     List * newcell;
{
  newcell->previous = list;
  newcell->next = list->next;
  list->next = newcell;

  return newcell;
}


/* Retire et memorise le dernier element */

List * List__PopLast(list, memolast)
     List *  list;
     List ** memolast;
{
  List * result = list->previous;
  
  *memolast = list;
  list->previous = 0;
  if (result)
    result->next = 0;
  
  return result;
}


/* Retire la liste du chainage qui l'entoure, retourne la derniere liste*/

List * List__Remove(list, last)
     List * list;
     List * last;
{
  if (list->previous)
    if ((list->previous->next = list->next) != 0)
      list->next->previous = list->previous;
    else
      return list->previous;
  else
    if (list->next)
      list->next->previous = 0;
    else
      return 0;

  return last;
}


int list_length(l)
     List * l;
{
  int result;

  for (result = 0; l; result += 1, l = l->next)
    ;

  return result;
}


/* */

List * Memo = 0;
