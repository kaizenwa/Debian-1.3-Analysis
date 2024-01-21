/*
 * queue.c: Functions that handle the queue handlers
 * rspfd: Radio Shortest Path Daemon. A router for packet radio networks.
 * Copyright (C) 1995 Craig Small VK2XLZ
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#include "queue.h"
#include <stdio.h>
#include <stdlib.h>
#include <syslog.h> 
/*
 * create_queue()
 *
 * Creates the queue, even mallocing it up
 *
 * Returns:
 *	A pointer to a queue
 *
 * Arguments:
 *	Nothing
 */
struct queue *create_queue()
{
	struct queue *q;
	
	q = (struct queue*)malloc(sizeof(struct queue));
	q->head = (struct q_node*)NULL;
	q->tail = (struct q_node*)NULL;
	return q;
}


/*
 * add_qnode()
 *
 * Adds an item to a queue.  You may get more than one entry with the same
 * key, this is YOUR problem, not the functions.
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	struct queue: The queue this thing is going on
 *	void*:		The entry to append on the queue
 *	char*:		The key used for the finds
 */
void add_qnode(struct queue *q, void *ent, char *key)
{
	struct q_node *ptr;
	
	ptr = (struct q_node*)malloc(sizeof(struct q_node));
	ptr->next = NULL;
	if (q->head == NULL) 
		q->head = ptr;
	
	if (q->tail != NULL) 
		q->tail->next = ptr;
	q->tail = ptr;
		
	ptr->ent = ent;
	if (key == NULL)
		ptr->key[0] = '\0';
	else
		strcpy(ptr->key, key);
	if (q->tail != NULL && q->tail->next != NULL)
	{
		syslog(LOG_DAEMON | LOG_ERR, "add_qnode(): tail is not tail!");
		q->tail->next = NULL;
	}
}

/*
 * qfind_next()
 *
 * Finds the next item in the queue with the correct key
 *
 * Returns:
 *	Pointer to the entry or NULL if not found
 *
 * Arguments:
 *	struct queue:	The queue to look in
 *	char*:		The key we're looking for
 *	qmark		Current pointer to queue
 *
 */
void* qfind_next(struct queue *q, char *key, qmark *current)
{
	qmark ptr;
	
	if (q->head == NULL)
		return NULL;
		
	if (*current == NULL)
		*current = q->head;	
	else
	{
		/* Check that qmarker is in list */
		ptr = q->head;
		while(ptr != NULL)
		{
			if (ptr == *current)
				break;
			ptr = ptr->next;
		}
		if (ptr == NULL)
		{
			*current = NULL;
			return NULL;
		}
		*current = (*current)->next;
	}
	while(*current != NULL) {
	
		if (strcmp((*current)->key, key) == 0) {
			return (*current)->ent;
		}
		*current = (*current)->next;
	}
	return NULL;
}

void* qfind_first(struct queue *q, char *key, qmark *current)
{
	*current = NULL;
	return qfind_next(q, key, current);
}

/*
 * del_qnode()
 *
 * Deletes the node in a queue
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	struct queue*	The queue to manipulate
 *	qmark		Pointer to current item in queue
 *	int		Flag to free node entry to, 1 = yes do it
 */
void del_qnode(struct queue *q, qmark current, int nukenode)
{
	struct q_node *ptr;
	struct q_node *prevptr;
	
	/* We cannot use findq as we need to know who was before us */
	if (q->head == NULL)
		return;
		
	ptr = q->head;
	
	/* Make sure the qmark is valid */
	if (current == NULL)
		return;
		
	/* Make sure this node to be killed is empty */
	if (current->ent != NULL && nukenode) {
		free(current->ent);
		current->ent = NULL;
	}
	

	/* Special case for first entry */	
	if (ptr == current) {
		q->head = ptr->next;
		if (ptr->next == NULL)
			q->tail = (struct q_node*)NULL;
		free(current);
		current = NULL;
		if (q->tail != NULL && q->tail->next != NULL)
		{
			syslog(LOG_DAEMON | LOG_ERR, "del_qnode(): tail is not tail!");
			q->tail->next = NULL;
		}
		return;
	}
	prevptr = ptr;
	ptr = prevptr->next;
		
	while(ptr != NULL) {
		if (ptr == current) {
			/* Link over this (soon to be dead) node */
			prevptr->next = ptr->next;
			/* Fix up tail, if neccessary */
			if (q->tail == ptr)
				q->tail = prevptr;
			free(ptr);
			ptr = NULL;
			if (q->tail != NULL && q->tail->next != NULL)
			{
				syslog(LOG_DAEMON | LOG_ERR, "del_qnode(): tail is not tail!");
				q->tail->next = NULL;
			}
			return;
		}
		prevptr = ptr;
		ptr = prevptr->next;
	}
	syslog(LOG_DAEMON | LOG_ERR, "del_qnode(): Cannot find qmark!! Node not deleted.\n");
	
}			

/*
 * q_entry()
 *
 * Returns the current entry in the queue
 *
 * Returns:
 *	Current entry in queue or NULL if empty queue
 *
 * Arguments:
 *	Queue to look in
 */		
void *q_entry(struct queue *q, qmark current)
{		
	qmark ptr;
	
	if (current == NULL)
		return NULL;
	
	ptr = q->head;	
	while(ptr != NULL) 
	{
		if (ptr == current) 
			return ptr->ent;
		
		ptr = ptr->next;
	}
	syslog(LOG_DAEMON | LOG_ERR, "q_entry(): Cannot find qmark!!");
	return NULL;
}

/*
 * qmove_first()
 *
 * Go to top of the queue
 *
 * Returns:
 *	first item in the queue, if it exists
 *
 * Arguments:
 *	struct queue:	The queue to move in
 */
void* qmove_first(struct queue *q, qmark *current)
{
	*current = q->head;
	if (q->head == NULL)
		return NULL;
	return q->head->ent;
}

/*
 * qmove_last()
 *
 * Go to bottom of the queue
 *
 * Returns:
 *	Last queue entry
 *
 * Arguments:
 *	struct queue:	The queue to move in
 */
void* qmove_last(struct queue *q, qmark *current)
{
	*current = q->tail;
	if (q->tail == NULL)
		return NULL;
	return q->tail->ent;
}

/*
 * qmove_next()
 *
 * Move to the next item in the queue, if it exists
 *
 * Returns:
 *	next item in queue, if it exists
 *
 * Arguments:
 *	struct queue: 	The queue to move in
 *	qmark		current poiunter in queue
 */
void* qmove_next(struct queue *q, qmark *current) 
{
	qmark ptr;

	if (q == NULL)
	{
		*current = NULL;
		return NULL;
	}
	
/*	ptr = q->head;
	while (ptr != NULL)
	{
		if (ptr == *current)
			break;
		ptr = ptr->next;
	}*/
	ptr = *current;
	if (ptr == NULL)
	{
		*current = NULL;
		return NULL;
	}
	*current = (*current)->next;
	if (q->tail != NULL && q->tail->next != NULL)
	{
		syslog(LOG_DAEMON | LOG_ERR, "del_qnode(): tail is not tail!");
		q->tail->next = NULL;
	}
	if (*current == NULL)
		return NULL;
	return (*current)->ent;
}

/* 
 * nuke_queue()
 *
 * What program of mine wouldn't have a nuke_xx or two in it, eh Louis?
 * This destroys a given queue, which means it removes all the nodes in a
 * queue.
 *
 * Returns:
 *	Nothing
 *
 * Arguments:
 *	struct queue	The queue to be nuked
 *	int		Flag to also free() the queue entry, 1 = yes do it
 *
 * Caveats:
 *	Does nothing to the queue entries, so you'd better make sure you've
 *	cleaned them up yourself.  The reason is that we don't know if the 
 * 	entries have been malloc'ed or not
 */
void nuke_queue(struct queue *q, int nukenode)
{
	qmark qm;
	
	if (q == NULL)
		return;
		
	(void) qmove_first(q, &qm);
	
	while(qm != NULL) {
		del_qnode(q, qm, 1);
		(void) qmove_first(q, &qm);
	}
	free(q);
	q = NULL;
	
} /* nuke_queue() */