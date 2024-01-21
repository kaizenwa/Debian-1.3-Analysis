/*
 * File:	buflist.c
 * Purpose:	Definition of buffer list.
 */

#include <assert.h>
#include <stdlib.h>
#include <publib.h>

#include "buflist.h"
#include "error.h"


static struct dynarr list;
static int init = 0;


static int find_index(Sbuf *buf) {
	Sbuf **tab;
	int i;

	assert(init);
	tab = list.data;
	for (i = 0; i < list.used && buf != tab[i]; ++i)
		continue;
	if (i == list.used)
		return -1;
	return i;
}


int buflist_add(Sbuf *buf) {
	Sbuf **tab;

	if (!init) {
		dynarr_init(&list, sizeof(Sbuf *));
		init = 1;
	}
	
	if (dynarr_resize(&list, list.used + 1) == -1) {
		error(0, "Couldn't add buffer to list (out of memory?)");
		return -1;
	}

	tab = list.data;
	tab[list.used] = buf;
	++list.used;

	buflist_sort();
	
	return 0;
}


void buflist_remove(Sbuf *buf) {
	Sbuf **tab;
	int i;

	assert(init);
	tab = list.data;
	i = find_index(buf);
	assert(i != -1);
	for (; i+1 < list.used; ++i)
		tab[i] = tab[i+1];
	--list.used;
}


Sbuf *buflist_first(void) {
	Sbuf **tab;

	assert(init);
	if (list.used == 0)
		return NULL;
	tab = list.data;
	return tab[0];
}


Sbuf *buflist_next(Sbuf *buf) {
	Sbuf **tab;
	int i;

	assert(init);
	assert(list.used > 0);
	i = find_index(buf);
	assert(i != -1);
	tab = list.data;
	i = (i+1) % list.used;
	return tab[i];
}


Sbuf *buflist_prev(Sbuf *buf) {
	Sbuf **tab;
	int i;

	assert(init);
	assert(list.used > 0);
	i = find_index(buf);
	assert(i != -1);
	tab = list.data;
	i = (i + list.used - 1) % list.used;
	return tab[i];
}


Sbuf *buflist_by_name(const char *name) {
	Sbuf **tab;
	int i;
	char *p;

	assert(init);
	tab = list.data;
	for (i = 0; i < list.used; ++i) {
		p = sbuf_get_name(tab[i]);
		if (name == NULL && p == NULL)
			return tab[i];
		else if (name != NULL && p != NULL && strcmp(name, p) == 0)
			return tab[i];
	}
	return NULL;
}


static int compare(const void *a, const void *b) {
	char *aname = sbuf_get_name(*(Sbuf **) a);
	char *bname = sbuf_get_name(*(Sbuf **) b);
	return strcmp(aname ? aname : "", bname ? bname : "");
}

void buflist_sort(void) {
	assert(init);
	qsort(list.data, list.used, sizeof(Sbuf *), compare);
}
