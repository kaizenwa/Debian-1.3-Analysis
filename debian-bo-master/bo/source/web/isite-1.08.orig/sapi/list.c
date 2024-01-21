#include <stdio.h>
#include <stdlib.h>
#include "list.h"

#define NEWSTRUCT(a) (a*)calloc(1,sizeof(a))

/*
PRE:	None.
POST:	Pointer to allocated LIST
*/
LIST * list_Create()
{
	LIST *l;
	if((l=NEWSTRUCT(LIST))==NULL)
		return NULL;

	l->Length=0;
	l->Head=NULL;
	l->Tail=NULL;

	return l;
}

/*
PRE:	l has been created.
POST:	Nonzero if empty
	Zero if not empty
*/
int list_Empty(LIST *l)
{
	if(l->Length==0)
		return 1;
	return 0;
}

/*
PRE:	l has been created.
	l contains at least one cell.
POST:	Pointer to first cell in list on success.
	NULL pointer on failure.
*/
CELL *list_First(LIST *l)
{
	return l->Head;
}

/*
PRE:	l has been created.
	l contains at least one cell.
POST:	Pointer to last cell in list on success.
	NULL pointer on failure.
*/
CELL *list_Last(LIST *l)
{
	if(l->Tail == NULL)
		/* Attempt to access first position of empty list */
		return NULL;

	return l->Tail;
}

/*
PRE:	l has been created.
	c is a pointer to a cell in l.
POST:	Pointer to next cell in list on success.
	NULL pointer if no more cells in list.
*/
CELL *list_Next(LIST *l, CELL *c)
{
	if(l->Tail == c)
		/* Attempt to access one position past end of list */
		return NULL;

	return c->Next;	
}

/*
PRE:	l has been created.
	c is a pointer to a cell in l.
POST:	Pointer to previous cell in list on success.
	NULL pointer if no more cells in list before c.
*/
CELL *list_Prev(LIST *l, CELL *c)
{
	if(l->Head == c)
		/* Attempt to access one position before start of list */
		return NULL;

	return c->Prev;
}

int list_InsertBefore(LIST *l, CELL *c, LIST_ATOM *a, int Type)
{
	LIST_ATOM *ta;

	if(list_Empty(l))
		list_InsertAfter(l,c,a,Type);
	else {
		list_InsertAfter(l,c,a,Type);
		ta = list_Retrieve(l,c);
		list_Update(l,c,a);
		list_Update(l, list_Next(l,c), ta);
	}	
	return 1;
}

void list_Update(LIST *l, CELL *c, LIST_ATOM *a)
{
	c->Atom = a;
}

/*
PRE:	l has been created.
	c points to a cell in l.
	a points to an allocated structure.
POST:	0 if out of memory condition.
	1 on success.  a is inserted in list after c.
*/
int list_InsertAfter(LIST *l, CELL *c, LIST_ATOM *a, int Type)
{
	CELL *nc, *tc;

	if(l->Head == NULL) {
		/* Inserting into empty list */
		if((l->Head = NEWSTRUCT(CELL)) == NULL)
			return 0;
		l->Head->Atom = a;
		l->Head->Type = Type;
		l->Head->Next = NULL;
		l->Head->Prev = NULL;
		l->Tail = l->Head;
	} else {
		/* Inserting into non-empty list */
		if((nc = NEWSTRUCT(CELL)) == NULL)
			return 0;
		nc->Atom = a;
		nc->Type = Type;
		tc = c->Next;
		c->Next = nc;
		nc->Prev = c;
		nc->Next = tc;
		if(c == l->Tail)
			/* Inserted after tail, move the pointer */
			l->Tail = nc;
	}
	l->Length=l->Length+1;

	return 1;
}

/*
PRE:	l has been created.
	c points to a cell in l.
POST:	Pointer to atom in c.
*/
LIST_ATOM *list_Retrieve(LIST *l, CELL *c)
{
	return c->Atom;
}

int list_Length(LIST *l)
{
	return l->Length;
}

/*
PRE:	l has been created.
	c points to a cell in l.
	Caller has freed memory allocated within the atom in c.
POST:	c is removed from l.
*/
void list_Delete(LIST *l, CELL *c)
{
	if(list_Empty(l))
		return;

	if(l->Head == c) {
		if(l->Tail == c) {
			/* singleton list */
			l->Head = NULL;
			l->Tail = NULL;
		} else {
			/* deleting first cell from non-singleton */
			l->Head = c->Next;
			c->Next->Prev = NULL;
		}
	} else {
		if(l->Tail == c) {
			/* deleting last cell from non-singleton */
			l->Tail = c->Prev;
			c->Prev->Next = NULL;
		} else {
			/* deleting cell from interior */
			c->Prev->Next = c->Next;
			c->Next = c->Prev;
		}
	}
	free(c);
	l->Length=l->Length - 1;

	return;
}

/*
PRE:	c is a valid cell.
POST:	Data type integer as defined in list.h
*/
int list_DataType(CELL *c)
{
	return(c->Type);
}


