/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

 /*
  *      Author: Wei Xu, Trusted Information Systems, Inc.
*/
static  char    RcsId[] = "Header: ";

#include "ulib.h"
#include "list.h"


list_t *mallocList(id,item)
int	 id;
void	*item;
{
	list_t	*plist;
	int	 z=sizeof(list_t);

	plist=(list_t*)malloc(z);
	bzero(plist,z);
	plist->id  =id;
	plist->item=item;

	return plist;
}

/* ************* findListItem  ***************************** *
 * return NULL if not find
 * ********************************************************* */
 list_t *findListItem( plist,id )
 list_t *plist;
 int     id;
 {
	list_t  *p= plist;

	if(!p)  return NULL;
	while( id != p->id && p->next ) p=p->next;
	if(id!=p->id) return NULL;

	return p;
 }

/* ************* add_list ************************************
 * return: a point to the list_t: if id is matched or
 *         added item to the list if(item). 
 *         NULL: neither match the id nor add to the list.
 * ********************************************************* */ 
list_t *setList( plist,id,item )
list_t *plist; /* the 1st list pointed to point */
int	id;     /* any number    */
void   *item;   /* any structure */
{
	list_t	*p= plist;

	if(!p)	return( mallocList(id,item) );
	if( p=findListItem( plist,id ) ) return p;

	if(!item) return (list_t*)NULL; /* nothing to add */

	p=mallocList(id,item);		/* not foud the same item and    */
	p->next= plist;		        /* add it to the top of the list */

	return p;
}

/* **************** deleteListItem ****************
 * delete it from the linked list if id is matched 
 ************************************************ */
list_t	*deleteListItem( plist,id,item )
list_t	*plist;
int	 id;
void   *item;	/* not in use yet */
{
	list_t  *p=plist, *pre=NULL;

	if(!p)  return plist;
	while( id!=p->id && p->next ) {
		pre=p;
		p=p->next;
	}
	if( id==p->id ) {
		if(pre) pre->next=p->next;
		else	plist=plist->next;
		free(p);
	}
	return plist;
}

/* **************** clearList ************
 * do a callback before free it
 *****************************************/
void	clearList(plist,cb,data)
list_t  *plist;
void   (*cb)();
void	*data;
{
	list_t  *p=plist, *tmp;
	while(p) {
		tmp=p->next;
		if(cb) cb(p->id,data,p->item);
		free(p);
		p=tmp;
	}
}
