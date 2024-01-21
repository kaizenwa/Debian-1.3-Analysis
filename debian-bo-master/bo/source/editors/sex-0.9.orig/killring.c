/*
 * File:	killring.c
 * Purpose:	Impelement the kill ring.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: killring.c,v 1.10 1997/01/07 01:05:34 liw Exp $"
 */
 
#include <stdlib.h>
#include <publib.h>
#include "killring.h"
#include "error.h"
#include "config.h"



/*
 * Structure:	kill
 * Purpose:	Describe an entry in the kill ring.
 * Fields:	buf	the buffer containing the killed text
 *		mark	the mark marking the whole text in the buffer
 */
struct kill {
	Sbuf *buf;
	Sbufmark *mark;
	struct kill *next;
};



/*
 * Variables:	ring_head
 * Purpose:	Pointer to head of kill ring.
 */
static struct kill *ring_head = NULL;


/*
 * Variable:	current
 * Purpose:	Give the current position in the kill ring
 */
static struct kill *current = NULL;



/*
 * Prototypes for local functions.
 */
static struct kill *new_entry(void);
static struct kill *get_latest(void);
static struct kill *get_next(struct kill *);
static void remove_extra(void);
static void remove_entry(struct kill *);



/*
 * Function:	killring_add
 * Purpose:	Add a new entry to the kill ring.
 * Arguments:	mark	the deleted text
 * Return:	-1 for failure, 0 for success.
 */
int killring_add(Sbufmark *mark) {
	struct kill *k;

	k = new_entry();
	if (k == NULL)
		return -1;
	if (sbuf_change(k->mark, mark) == -1) {
		error(NULL, "error changing text (out of memory?)");
		remove_entry(k);
		return -1;
	}

	remove_extra();
	return 0;
}



/*
 * Function:	killring_prepend_to_latest
 * Purpose:	Prepend text to the latest entry in kill ring
 * Arguments:	mark	mark on the text to be prepended
 * Return:	-1 for failure, 0 for success.
 */
int killring_prepend_to_latest(Sbufmark *mark) {
	struct kill *k;
	
	k = get_latest();
	if (k == NULL)
		return killring_add(mark);
	sbuf_remark(k->mark, 0, 0);
	if (sbuf_change(k->mark, mark) == -1) {
		error(NULL, "error changing text (out of memory?)");
		return -1;
	}
	remove_extra();
	return 0;
}



/*
 * Function:	killring_append_to_latest
 * Purpose:	Append text to the latest entry in kill ring
 * Arguments:	mark	mark on the text to be appended
 * Return:	-1 for failure, 0 for success.
 */
int killring_append_to_latest(Sbufmark *mark) {
	struct kill *k;
	
	k = get_latest();
	if (k == NULL)
		return killring_add(mark);
	sbuf_remark(k->mark, sbuf_length(k->buf), 0);
	if (sbuf_change(k->mark, mark) == -1) {
		error(NULL, "error changing text (out of memory?)");
		return -1;
	}
	remove_extra();
	return 0;
}



/*
 * Function:	killring_get_first_mark
 * Purpose:	Return the latest entry in the kill ring as mark.
 * Arguments:	none.
 * Return:	Pointer to mark on first kill ring entry, NULL if none.
 */
Sbufmark *killring_get_first_mark(void) {
	current = get_latest();
	if (current == NULL)
		return NULL;
	sbuf_remark(current->mark, 0, sbuf_length(current->buf));
	return current->mark;
}



/*
 * Function:	killring_get_first
 * Purpose:	Return the latest entry in the kill ring.
 * Arguments:	mark	the mark to set to the contents of the kill ring entry
 * Return:	-1 for failure, 0 for success.
 * Description:	killring_get_first will set the mark that it is given as
 *		the argument to the contents of the latest kill ring entry.
 * Note:	The position in the kill ring is global, and will be reset
 *		whenever the kill ring is modified.
 */
int killring_get_first(Sbufmark *mark) {
	current = get_latest();
	if (current == NULL)
		return -1;
	sbuf_remark(current->mark, 0, sbuf_length(current->buf));
	if (sbuf_change(mark, current->mark) == -1) {
		error(NULL, "error changing text (out of memory?)");
		return -1;
	}
	return 0;
}



/*
 * Function:	killring_get_next
 * Purpose:	Return the next entry in the kill ring.
 * Arguments:	mark	the mark to set to the contents of the kill ring entry
 * Return:	-1 for failure, 0 for success.
 * Description:	killring_get_next will set the mark that it is given as
 *		the argument to the contents of the next kill ring entry.
 * Note:	The position in the kill ring is global, and will be reset
 *		whenever the kill ring is modified.
 */
int killring_get_next(Sbufmark *mark) {
	current = get_next(current);
	if (current == NULL)
		return -1;
	sbuf_remark(current->mark, 0, sbuf_length(current->buf));
	if (sbuf_change(mark, current->mark) == -1) {
		error(NULL, "error changing text (out of memory?)");
		return -1;
	}
	return 0;
}



/**********************************************************************
 * Local functions                                                    *
 **********************************************************************/



/*
 * Function:	new_entry
 * Purpose:	Create a new entry to kill ring.
 */
static struct kill *new_entry(void) {
	struct kill *k;
	
	k = malloc(sizeof(struct kill));
	if (k == NULL)
		return NULL;

	k->buf = sbuf_create();
	if (k->buf == NULL) {
		error(NULL, "error creating text buffer (out of memory?)");
		goto error;
	}
	k->mark = sbuf_mark(k->buf, 0, 0);
	if (k->mark == NULL) {
		error(NULL, "error creating text mark (out of memory?)");
		goto error;
	}

	k->next = ring_head;
	ring_head = k;

	return k;
	
error:
	if (k->buf != NULL)
		sbuf_destroy(k->buf);
	free(k);
	return NULL;
}


/*
 * Function:	get_latest
 * Purpose:	Return pointer to first entry in kill ring.
 */
static struct kill *get_latest(void) {
	return ring_head;
}


/*
 * Function:	get_next
 * Purpose:	Return pointer to the entry that follows k.
 */
static struct kill *get_next(struct kill *k) {
	if (k == NULL || k->next == NULL)
		return ring_head;
	return k->next;
}


/*
 * Function:	remove_extra
 * Purpose:	Make sure there is not too much stuff in the kill ring.
 */
static void remove_extra(void) {
	long size, ksize, max;
	struct kill *k;

	max = config_get_long(CONFIG_MAX_KILLRING);
	
	size = 0;
	for (k = ring_head; k != NULL; k = k->next) {
		ksize = sbuf_length(k->buf);
		if (size + ksize > max)
			break;
		size += ksize;
	}

	if (k == ring_head)
		k = k->next;
	if (k != NULL) {
		while (k->next != NULL)
			remove_entry(k->next);
	}
		
	current = NULL;
}


/*
 * Function:	remove_entry
 * Purpose:	Remove a specific entry from the kill ring.
 */
static void remove_entry(struct kill *k) {
	struct kill *kk;

	if (k == ring_head)
		ring_head = k->next;
	else {
		for (kk = ring_head; kk->next != k; kk = kk->next)
			continue;
		kk->next = k->next;
	}
	sbuf_destroy(k->buf);
	free(k);
}
