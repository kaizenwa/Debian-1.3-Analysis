/*
 * List.cc - Definitions for the dndList class
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

/**************************************************************************
* LIST.CC -	DNDLIST class methods. Include some methods for
*		dndListNode class.
*
* A list contains at least three nodes: the first, the last and a
* "phantom" node if the list is empty. The current node (NodePointer)
* can never be the first or the last.
* 
***************************************************************************/

#include "List.h"

#ifndef max
#define max(x1,x2) ((x1)>(x2)?(x1):(x2))
#endif
#ifndef min
#define min(x1,x2) ((x1)>(x2)?(x2):(x1))
#endif

/**************************************************************************
*
* FUNCTION: 	dndListNode and dndList classes constructors and destructors.
*
* SYNTAX:       dndListNode(dndListNode *prev,dndListNode *next);
*		dndList(void)
*
* Jun 04,1994	Implemented.
*
***************************************************************************/

dndListNode::dndListNode(dndListNode *prev,dndListNode *next)
{
	NodeObject=NULL;
	PrevNode=prev;
	NextNode=next;
}

dndListNode::~dndListNode(void)
{
	if(PrevNode!=NULL)	PrevNode->chgnext(NextNode);
	if(NextNode!=NULL)	NextNode->chgprev(PrevNode);
}

dndList::dndList(void)
{
	NumNodes=NodeNumber=SecondaryNumber=0L;

	FirstNode=new dndListNode;
	if(FirstNode==NULL) return;

	LastNode=new dndListNode;
	if(LastNode==NULL) return;

	NodePointer=new dndListNode(FirstNode,LastNode);
	if(NodePointer==NULL) return;

	FirstNode->chgnext(NodePointer);
	LastNode->chgprev(NodePointer);
	SecondaryPointer=FirstNode;
}

dndList::~dndList(void)
{
	while(NumNodes) remove();
	delete FirstNode->getnext();
	delete FirstNode;
	delete LastNode;
}

/**************************************************************************
*
* FUNCTION: 	dndList.verify
*
* SYNTAX:	char dndList.verify(void);
*
* DESCRIPTION:	Test if the dndList is consistent.
*
* RETURNS:	0 if the dndList has errors.
*		1 if the dndList is Ok.
*
* LAST UPDATED
* ------------
*
* Jun 02,1994	Implemented.
* Jun 05,1994	Test the "phantom" node.
*
***************************************************************************/

char dndList::verify(void)
{
	return(FirstNode!=NULL && LastNode!=NULL && NodePointer!=NULL);
}


/**************************************************************************
*
* FUNCTION:	dndList.getfirst
*
* SYNTAX:	dndPointer dndList.getfirst(void);
*
* DESCRIPTION:  Go to the first node of the dndList.
*
* RETURNS:	The object of the node or NULL if the dndList is empty.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::getfirst(void)
{
	if(!NumNodes) return NULL;

	NodePointer=FirstNode->getnext();
	NodeNumber=1L;
	return NodePointer->getobj();
}

/**************************************************************************
*
* FUNCTION:	dndList.getlast
*
* SYNTAX:	dndPointer dndList.getlast(void);
*
* DESCRIPTION:  Go to the last node of the dndList.
*
* RETURNS:	The object of the node or NULL if the dndList is empty.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::getlast(void)
{
	NodePointer=LastNode->getprev();
	NodeNumber=NumNodes;
	return NodePointer->getobj();
}

/**************************************************************************
*
* FUNCTION:	dndList.getnext
*
* SYNTAX:	dndPointer dndList.getnext(void);
*
* DESCRIPTION:  Go to the next node of the dndList.
*
* RETURNS:	The object of the node or NULL if the end of the dndList
*		was reached.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::getnext(void)
{
	if(NodeNumber==NumNodes) return NULL;
	NodeNumber++;
	NodePointer=NodePointer->getnext();
	return NodePointer->getobj();
}

/**************************************************************************
*
* FUNCTION:	dndList.getprev
*
* SYNTAX:	dndPointer dndList.getprev(void);
*
* DESCRIPTION:  Go to the previous node of the dndList.
*
* RETURNS:	The object of the node or NULL if the beggining of the
*		dndList was reached.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
*
***************************************************************************/

dndPointer dndList::getprev(void)
{
	if(NodeNumber<=1L) return NULL;
	NodeNumber--;
	NodePointer=NodePointer->getprev();
	return NodePointer->getobj();
}

/**************************************************************************
*
* FUNCTION:	dndList.remove
*
* SYNTAX:	dndPointer dndList.remove(void);
*
* DESCRIPTION:	Removes the current node from the dndList. The current node
*		will switch to the next node of the dndList.
*
* RETURNS:	The object of the deleted node or NULL if the dndList was
*		empty.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::remove(void)
{
	if(!NumNodes)	return NULL;

	NumNodes--;
	NodeNumber--;

	dndPointer NodeObject=NodePointer->getobj();

	if(!NumNodes)	NodePointer->chgobj(NULL);
	else
	{	NodePointer=NodePointer->getprev();
		delete NodePointer->getnext();
		getnext();
	}

	return NodeObject;
}

/**************************************************************************
*
* FUNCTION:	dndList.addbefore
*
* SYNTAX:	dndPointer dndList.addbefore(dndPointer object);
*
*		dndPointer object;	object to be inserted
*
* DESCRIPTION:	Inserts an object before the actual node. The actual node
*		switches to the new one.
*
* RETURNS:	The object or NULL if could not add.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::addbefore(dndPointer object)
{
	if(!NumNodes)
	{	NodePointer->chgobj(object);
		NumNodes=NodeNumber=1L;
		return object;
	}

	dndListNode *NewNode=new dndListNode(NodePointer->getprev(),NodePointer);
	if(NewNode==NULL) return NULL;

	NumNodes++;
	NewNode->chgobj(object);
	NodePointer->getprev()->chgnext(NewNode);
	NodePointer->chgprev(NewNode);
	NodePointer=NewNode;

	return object;
}

/**************************************************************************
*
* FUNCTION:	dndList.addafter
*
* SYNTAX:	dndPointer dndList.addafter(dndPointer object);
*
*		dndPointer object;	object to be inserted
*
* DESCRIPTION:	Inserts an object after the actual node. The actual node
*		switches to the new one.
*
* RETURNS:	The object or NULL if could not add.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::addafter(dndPointer object)
{
	if(!NumNodes)
	{	NodePointer->chgobj(object);
		NumNodes=NodeNumber=1L;
		return object;
	}

	dndListNode *NewNode=new dndListNode(NodePointer,NodePointer->getnext());
	if(NewNode==NULL) return NULL;

	NumNodes++;
	NodeNumber++;
	NewNode->chgobj(object);
	NodePointer->getnext()->chgprev(NewNode);
	NodePointer->chgnext(NewNode);
	NodePointer=NewNode;

	return object;
}

/**************************************************************************
*
* FUNCTION:	dndList.swap
*
* SYNTAX:	void dndList.swap(dndListIndex node);
*
*		dndListIndex node;	index to the other node.
*
* DESCRIPTION:	Swap the contents of the current and the
*		nodeth nodes of the dndList. "node" range is
*		1...size.
*
* RETURNS:	None.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

void dndList::swap(dndListIndex node)
{
	if(node>NumNodes || !node) return;

	dndListNode *Srch=FirstNode;
	for(int i=0;i!=node;i++) Srch=Srch->getnext();

	dndPointer Backup=NodePointer->getobj();
	NodePointer->chgobj(Srch->getobj());
	Srch->chgobj(Backup);
}

/**************************************************************************
*
* FUNCTION:	dndList.swap
*
* SYNTAX:	void dndList.swap(dndListIndex node1,dndListIndex node2);
*
*		dndListIndex node1;	index to the first node.
*		dndListIndex node2;	index to the second node.
*
* DESCRIPTION:	Swap the contents of the node1th and the
*		node2th nodes of the dndList. node range is
*		1...size.
*
* RETURNS:	None.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

void dndList::swap(dndListIndex node1,dndListIndex node2)
{
	if(node1>NumNodes || node2>NumNodes || !node1 || !node2) return;

	dndListNode *Srch1=FirstNode;
	dndListIndex  DestNode=min(node1,node2);
	int i;
	for(i=0;i!=DestNode;i++) Srch1=Srch1->getnext();

	dndListNode *Srch2=Srch1;
	for(DestNode=max(node1,node2);i!=DestNode;i++) Srch2=Srch2->getnext();

	dndPointer Backup=Srch1->getobj();
	Srch1->chgobj(Srch2->getobj());
	Srch2->chgobj(Backup);
}

/**************************************************************************
*
* FUNCTION:	dndList.go
*
* SYNTAX:	dndPointer dndList.go(dndListIndex node);
*
*		dndListIndex node;	where to go.
*
* DESCRIPTION:	Go to the nodeth node of the dndList. "node" range is
*		1...size.
*
* RETURNS:	The object of the node or NULL if node is out of range.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
* Jun 06, 1994	Optimized.
*
***************************************************************************/

dndPointer dndList::go(dndListIndex node)
{
	if(node>NumNodes || !node) return NULL;

	if(node>NodeNumber)
		while(node!=NodeNumber)
		{ NodePointer=NodePointer->getnext(); NodeNumber++; }
	else
		while(node!=NodeNumber)
		{ NodePointer=NodePointer->getprev(); NodeNumber--; }

	return NodePointer->getobj();
}

/**************************************************************************
*
* FUNCTION:	dndList.findfirst
*
* SYNTAX:	dndPointer dndList.findfirst(char (*func)(dndPointer ));
*
*		char (*func)(dndPointer );	Criterion function.
*
* DESCRIPTION:	Go to the first node that satisfies the "func" criterion.
*
* RETURNS:	The object of the node or NULL if no node could satisfy.
*		If no object could satisfy the current node remains the
*		same.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::findfirst(char (*func)(dndPointer ))
{
	dndListNode *Srch=FirstNode;
	dndPointer SrchObj=0;
	char Flag=0;
	int i;
	for(i=0;!Flag;i++)
	{	Srch=Srch->getnext();
		SrchObj=Srch->getobj();
		if(SrchObj==NULL) break;
		Flag=(*func)(SrchObj);
	}

	if(!Flag) return NULL;
	NodePointer=Srch;
	NodeNumber=i;
	return SrchObj;
}

/**************************************************************************
*
* FUNCTION:	dndList.findnext
*
* SYNTAX:	dndPointer dndList.findnext(char (*func)(dndPointer ));
*
*		char (*func)(dndPointer );	Criterion function.
*
* DESCRIPTION:	Go to the next node that satisfies the "func" criterion.
*
* RETURNS:	The object of the node or NULL if no node could satisfy.
*		If no object could satisfy the current node remains the
*		same.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::findnext(char (*func)(dndPointer ))
{
	dndListNode *Srch=NodePointer;
	dndPointer SrchObj=0;
	char Flag=0;
	int i;
	for(i=NodeNumber;!Flag;i++)
	{	Srch=Srch->getnext();
		SrchObj=Srch->getobj();
		if(SrchObj==NULL) break;
		Flag=(*func)(SrchObj);
	}

	if(!Flag) return NULL;
	NodePointer=Srch;
	NodeNumber=i;
	return SrchObj;
}

/**************************************************************************
*
* FUNCTION:	dndList.findlast
*
* SYNTAX:	dndPointer dndList.findlast(char (*func)(dndPointer ));
*
*		char (*func)(dndPointer );	Criterion function.
*
* DESCRIPTION:	Go to the last node that satisfies the "func" criterion.
*
* RETURNS:	The object of the node or NULL if no node could satisfy.
*		If no object could satisfy the current node remains the
*		same.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::findlast(char (*func)(dndPointer ))
{
	dndListNode *Srch=LastNode;
	dndPointer SrchObj=0;
	char Flag=0;
	int i;
	for(i=NumNodes;!Flag;i--)
	{	Srch=Srch->getprev();
		SrchObj=Srch->getobj();
		if(SrchObj==NULL) break;
		Flag=(*func)(SrchObj);
	}

	if(!Flag) return NULL;
	NodePointer=Srch;
	NodeNumber=i+1;
	return SrchObj;
}

/**************************************************************************
*
* FUNCTION:	dndList.findprev
*
* SYNTAX:	dndPointer dndList.findprev(char (*func)(dndPointer ));
*
*		char (*func)(dndPointer );	Criterion function.
*
* DESCRIPTION:	Go to the previous node that satisfies the "func" criterion.
*
* RETURNS:	The object of the node or NULL if no node could satisfy.
*		If no object could satisfy the current node remains the
*		same.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::findprev(char (*func)(dndPointer ))
{
	dndListNode *Srch=NodePointer;
	dndPointer SrchObj=0;
	char Flag=0;
	int i;
	for(i=NodeNumber;!Flag;i--)
	{	Srch=Srch->getprev();
		SrchObj=Srch->getobj();
		if(SrchObj==NULL) break;
		Flag=(*func)(SrchObj);
	}

	if(!Flag) return NULL;
	NodePointer=Srch;
	NodeNumber=i;
	return SrchObj;
}

/**************************************************************************
*
* FUNCTION:	dndList.sort
*
* SYNTAX:	dndPointer dndList.sort(int (*func)(dndPointer node1,dndPointer node2));
*
*		char (*func)(dndPointer ,dndPointer );	Criterion function.
*
* DESCRIPTION:	Sort the dndList according to "func" criterion. "func"
*		must return the following values:
*
*		< 0 if node1 is less than node2
*		= 0 if node1 is equal to node2
*		> 0 if node1 is greater than node2
*
*		The dndList is sorted allways in ascending order.
*
* RETURNS:	None.
*
* LAST UPDATED
* ------------
*
* Jun 05, 1994	Implemented.
*
***************************************************************************/

void dndList::sort(int (*func)(dndPointer ,dndPointer ))
{
	if(NumNodes<=1) return;

	dndListNode *ithNode=FirstNode->getnext();
    	int i;
	for(i=1;i!=NumNodes;i++)
	{	dndListNode *jthNode=ithNode->getnext();
		for(int j=i+1;j<=NumNodes;j++)
		{	if((*func)(jthNode->getobj(),ithNode->getobj())<0)
			{	dndPointer Backup=ithNode->getobj();
				ithNode->chgobj(jthNode->getobj());
				jthNode->chgobj(Backup);
			}
			jthNode=jthNode->getnext();
		}
		ithNode=ithNode->getnext();
	}
}


/**************************************************************************
*
* FUNCTION:	dndList.replace
*
* SYNTAX:	dndPointer dndList.replace(dndPointer object);
*
* DESCRIPTION:	Replaces the object of the current node.
*
* RETURNS:	The old object or NULL if the dndList is empty.
*
* LAST UPDATED
* ------------
*
* Jun 07, 1994	Implemented.
*
***************************************************************************/

dndPointer dndList::replace(dndPointer object)
{
	if(!NumNodes)	return NULL;

	dndPointer OldObject=NodePointer->getobj();
	NodePointer->chgobj(object);
	return OldObject;
}

/**************************************************************************
*
* FUNCTION:	dndList.operator()
*
* SYNTAX:	dndPointer &dndList(dndListIndex node);
*
*		dndListIndex node;	where to go.
*
* DESCRIPTION:	Go to the nodeth node of the dndList. "node" range is
*		1...size. Don't update the current node. Instead, use
*		a secondary node pointer to do the task.
*
*		Don't use this method in an empty dndList !!!
*
* RETURNS:	A reference to the object of the node. Return a reference
*		to a "phantom" object if the dndList is empty.
*
* LAST UPDATED
* ------------
*
* Jun 07, 1994	Implemented.
*
***************************************************************************/

dndPointer &dndList::operator()(dndListIndex node)
{
	if(node>NumNodes || !node)
		return FirstNode->getref();

	if(SecondaryNumber>NumNodes)
	{	SecondaryPointer=FirstNode;
		SecondaryNumber=0L;
	}

	if(node>SecondaryNumber)
		while(node!=SecondaryNumber)
		{	SecondaryPointer=SecondaryPointer->getnext();
			SecondaryNumber++;
		}
	else
		while(node!=SecondaryNumber)
		{	SecondaryPointer=SecondaryPointer->getprev();
			SecondaryNumber--;
		}

	return SecondaryPointer->getref();
}

/**************************************************************************
*
* FUNCTION:	dndList.findequal
*
* SYNTAX:	dndPointer dndList.findfirst(dndPointer data);
*
*		dndPointer data;	what to find
*
* DESCRIPTION:	Go to the first node that have the "data" object.
*
* RETURNS:	The object of the node or NULL if no node could satisfy.
*		If no object could satisfy the current node remains the
*		same.
*
* LAST UPDATED
* ------------
*
* Feb 23, 1996	Implemented.
*
***************************************************************************/

dndPointer dndList::findequal(dndPointer data)
{
	dndListNode *Srch=FirstNode;
	dndPointer SrchObj=0;
	char Flag=0;
	int i;
	for(i=0;!Flag;i++)
	{	Srch=Srch->getnext();
		SrchObj=Srch->getobj();
		if(SrchObj==NULL) break;
		Flag=(data==SrchObj);
	}

	if(!Flag) return NULL;
	NodePointer=Srch;
	NodeNumber=i;
	return SrchObj;
}

