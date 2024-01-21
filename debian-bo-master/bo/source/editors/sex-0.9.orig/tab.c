/*
 * File:	tab.c
 * Purpose:	Compute tab positions.
 */

#include <assert.h>
#include "tab.h"

#ifndef DEFAULT_TAB_WIDTH
#define DEFAULT_TAB_WIDTH	8
#endif

static long width = DEFAULT_TAB_WIDTH;


void tab_set_width(long w) {
	assert(w > 0);
	width = w;
}

long tab_width(void) {
	return width;
}

long tab_next(long col) {
	assert(col >= 0);
	return col + tab_next_distance(col);
}

long tab_next_distance(long col) {
	assert(col >= 0);
	return width - (col % width);
}
