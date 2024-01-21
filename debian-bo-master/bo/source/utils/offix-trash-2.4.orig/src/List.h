/*
 * List.h - definition of the dndList class
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
* LIST
*
* Mother class for the list library. Implements a general purpose
* list class.
*
* LAST UPDATED:
*
* Jun 02, 1994	Constructors & destructors.
* Jun 05, 1994	Basic list class methods implemented.
* Jun 07, 1994	Operator() and replace methods implemented. getref
*		implemented for listnode class.
* Oct 08, 1995	Class name changed to dndList to avoid conflicts.
*			listndx  -> dndListIndex
*			listnode -> dndListNode
*			list     -> dndList
*		New data types
* 			dndPointer
*		New function
*			dndCast
* 
**********************************************************************/

#ifndef __DNDLISTh__
#define __DNDLISTh__

#ifndef NULL
#include <string.h>
#endif


typedef unsigned long	dndListIndex;
typedef void*		dndPointer;

class dndListNode
{
	dndPointer	NodeObject;
	dndListNode	*NextNode,*PrevNode;

	public:

	dndListNode(dndListNode *prev=NULL,dndListNode *next=NULL);
	~dndListNode();

	dndPointer	getobj(void)		{ return NodeObject; }
	void 		chgobj(dndPointer obj)	{ NodeObject=obj; }

	dndListNode *getprev(void)	{ return PrevNode; }
	dndListNode *getnext(void)	{ return NextNode; }

	void chgprev(dndListNode *node) { PrevNode=node; }
	void chgnext(dndListNode *node) { NextNode=node; }

	dndPointer &getref(void)	{ return NodeObject; }
};

class dndList
{
	dndListNode 	*FirstNode,*LastNode,*NodePointer;
	dndListIndex	 NumNodes,NodeNumber;

	dndListNode  *SecondaryPointer;
	dndListIndex  SecondaryNumber;

	public:

	dndList(void);
	virtual ~dndList(void);

	virtual char		verify(void);
	virtual dndListIndex	size(void)	{ return NumNodes; }
	virtual char		empty(void)	{ return !NumNodes; }

	dndPointer	getfirst(void);
	dndPointer	getnext(void);
	dndPointer	getlast(void);
	dndPointer	getprev(void);
	dndPointer	getthis(void)	{ return NodePointer->getobj(); }

	virtual dndPointer add(dndPointer obj)	{ return addafter(obj); }
	virtual dndPointer remove(void);

	dndPointer	addbefore(dndPointer);
	dndPointer	addafter(dndPointer);
	dndPointer	replace(dndPointer);

	dndPointer	findfirst(char (*func)(dndPointer));
	dndPointer	findlast(char (*func)(dndPointer));
	dndPointer	findnext(char (*func)(dndPointer));
	dndPointer	findprev(char (*func)(dndPointer));
	dndPointer	findequal(dndPointer);

	void	sort(int (*func)(dndPointer,dndPointer));

	void	swap(dndListIndex);
	void	swap(dndListIndex,dndListIndex);

	dndListIndex	pos(void)	{ return NodeNumber; }
	dndPointer	go(dndListIndex);
	dndPointer&	operator()(dndListIndex);
};

#define dndCast(pointer,newtype) ((newtype *)(pointer))

#endif
