/*
 * SortedList.h : Definition of the dndSortedList class.
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

/*********************************************************************
*
* SORTEDLIST
*
* Automatically sorted list.
*
* Last Updated:
*
* Jun 16, 1994	Implemented.
*
**********************************************************************/

#ifndef __DNDSORTEDLISTh__
#define __DNDSORTEDLISTh__

#include "List.h"

class dndSortedList:public dndList
{
	int (*SortFunc)(dndPointer,dndPointer);

	public:

	dndSortedList(int(*func)(dndPointer,dndPointer)=NULL):dndList()
		{ SortFunc=func; }
	~dndSortedList(void) {}

	dndPointer add(dndPointer);

	void chgsortcrit(int(*func)(dndPointer,dndPointer));
};

#endif
