/*
 * SortedList.cc : Method definitions for class dndSortedList
 * Copyright (C) 1996 César Crusius
 *
 * This file is part of the DND Library.  This library is free
 * software; you can redistribute it and/or modify it under the terms of
 * the GNU Library General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your
 * option) any later version.  This library is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU Library General Public License for more details.
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */



#include "SortedList.h"


/**************************************************************************
*
* FUNCTION:	dndSortedList.add
*
* SYNTAX:	dndPointer dndSortedList.add(dndPointer obj);
*
* DESCRIPTION:	Inserts a new list node in the position determined by
*		the current criterion.
*
* RETURNS:	the object or NULL if errors.
*
* LAST UPDATED
* ------------
*
* Jun 15, 1994	Implemented.
*
***************************************************************************/

dndPointer dndSortedList::add(dndPointer object)
{
	if(!size() || SortFunc==NULL) return dndList::add(object);

	dndPointer SrchNode=getthis();
	if(SortFunc(object,SrchNode)<0)
	{	while(SrchNode!=NULL && SortFunc(object,SrchNode)<0)
			SrchNode=getprev();

		if(SrchNode==NULL)	goto befor;
		else			goto after;
	}
	else
	{	while(SrchNode!=NULL && SortFunc(object,SrchNode)>0)
			SrchNode=getnext();

		if(SrchNode==NULL)	goto after;
		else			goto befor;
	}

after:	return addafter(object);
befor:	return addbefore(object);
}

/**************************************************************************
*
* FUNCTION:	dndSortedList.chgsortcrit
*
* SYNTAX:	dndPointer dndSortedList.chgsortcrit(int(*func)(dndPointer,dndPointer));
*
* DESCRIPTION:	Changes the criterion function and automatically
*		re-sort the list.
*
* RETURNS:	None.
*
* LAST UPDATED
* ------------
*
* Jun 16, 1994	Implemented.
*
***************************************************************************/

void dndSortedList::chgsortcrit(int(*func)(dndPointer,dndPointer))
{
	SortFunc=func;
	if(func==NULL) return;
	sort(func);
}
