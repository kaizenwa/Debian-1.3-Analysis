/* Copyright (c) 1995 by Sanjay Ghemawat */
// This file is designed to be included multiple times and therefore
// no header guards should be placed on it.

// "Template" for generating various kinds of hash tables.
//
// See "htable.h" for macros that should be defined before including this.

// Check that all of the macros are defined
#ifndef HTABLE
#error macro HTABLE not defined for hash table instantiation.
#endif
#ifndef HTYPE
#error macro HTYPE not defined for hash table instantiation.
#endif
#ifndef HCONTROL
#error macro HCONTROL not defined for hash table instantiation.
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef _HT_PRIV_H
#include "ht_priv.h"
#endif

HTABLE::HTABLE() {
    accesses = 0;
    collisions = 0;
    max_occupancy = htable_default_max_occupancy;
    init(1);
}

HTABLE::~HTABLE() {
    delete [] table;
}

HTABLE::HTABLE(HTABLE const& x) {
    accesses = 0;
    collisions = 0;
    max_occupancy = htable_default_max_occupancy;
    init(1);
    *this = x;
}

HTABLE& HTABLE::operator = (HTABLE const& x) {
    // This check is necessary to deal with aliasing between this and x.
    if (&x != this) {
	// Remove current contents
	clear();

	// Resize set to right size in one shot
	resize(x.size());

	// Populate with elements from x
	for (int i = 0; i < x.tsize; i++) {
	    if (HCONTROL::is_full(x.table[i])) {
		insert(x.table[i]);
	    }
	}
    }
    return *this;
}

int HTABLE::insert(HTYPE x) {
    int index = find_index(x);
    if (!HCONTROL::is_empty(table[index])) {
	// Just replace old entry
	table[index] = x;
	return 0;
    }

    // Need to insert new entry
    count++;
    table[index] = x;

    // Resize if table is full
    assert(count <= tsize);
    if ((count + delcount) >= enlarge_size)
	resize(count);
    return 1;
}

int HTABLE::remove(HTYPE x) {
    int index = find_index(x);
    if (HCONTROL::is_empty(table[index])) return 0;

    // Need to remove element
    count--;
    delcount++;
    HCONTROL::make_obsolete(table[index]);

    // XXX Automatically shrink?
    // if (count <= shrink_size) resize(count);

    return 1;
}

void HTABLE::clear() {
    for (int i = 0; i < tsize; i++)
	HCONTROL::make_empty(table[i]);
    count = 0;
    delcount = 0;
}

void HTABLE::set_occupancy(int x) {
    // Do not go outside range 50..90%
    if (x < 50) x = 50;
    if (x > 90) x = 90;

    if (x != max_occupancy) {
	max_occupancy = x;
	resize(count);
    }
}

int HTABLE::find_index(HTYPE elem) const {
    HTABLE_STAT(((HTABLE*) this)->accesses++);

    int h1 = htable_int_hash(HCONTROL::hash(elem));
    int first = htable_fix_hash(h1, slotBits);

    // First check without computing secondary hash value
    HTYPE v = table[first];
    if (HCONTROL::is_empty(v) ||
	(HCONTROL::is_full(v) && HCONTROL::equal(elem, v)))
	return first;

    // Now scan thru the table, incrementing the index by the secondary
    // hash value on each iteration.
    int h2 = htable_hash2(h1);
    int i = (first + h2) & mask;

    HTABLE_STAT(int xcount = 1);
    while (1) {
	v = table[i];
	if (HCONTROL::is_empty(v) ||
	    (HCONTROL::is_full(v) && HCONTROL::equal(elem, v))) {
	    HTABLE_STAT(((HTABLE*) this)->collisions += xcount);
	    return i;
	}

	i = (i + h2) & mask;
	HTABLE_STAT(xcount++);
    }

    fprintf(stderr, "hash loop: h1 = %4d, h2 = %4d, sz = %4d\n",h1,h2,tsize);
    exit(1);
}

int HTABLE::table_size(int n) const {
    // Search for the correct power of 2.
    int power = 1;
    while ((power <= n) || ((power*((double)max_occupancy)) <= ((n+1)*100.0)))
	power <<= 1;
    return power;
}

void HTABLE::init(int n) {
    count = 0;
    delcount = 0;
    tsize = table_size(n);
    mask = tsize-1;

    // Compute "lg(tsize)"
    slotBits = 0;
    int x = tsize;
    while (x > 1) {
	x >>= 1;
	slotBits++;
    }

    // Compute resize thresholds
    enlarge_size = (int) (((double)tsize) * (((double)max_occupancy) / 100.0));
    if (enlarge_size < 1) enlarge_size = 1;
    if (enlarge_size > tsize) enlarge_size = tsize;
    shrink_size = enlarge_size / htable_min_occupancy_divisor;
    if (shrink_size < 0) shrink_size = 0;
    assert(shrink_size < tsize);

    table = new HTYPE[tsize];
    for (int i = 0; i < tsize; i++)
	HCONTROL::make_empty(table[i]);
}

void HTABLE::resize(int n) {
    if (n < count) n = count;

    // Save old contents and make new correctly sized table
    HTYPE* old = table;
    int old_size = tsize;
    init(n);
    assert(tsize > n);

    // Now restore the old contents
    for (int i = 0; i < old_size; i++) {
	HTYPE elem = old[i];
	if (HCONTROL::is_full(elem))
	    insert(elem);
    }

    delete [] old;
}

void HTABLE::check() const {
    int i, j;

    /* Check for duplicates */
    for (i = 0; i < tsize; i++) {
	if (!HCONTROL::is_full(table[i])) continue;

	for (j = i+1; j < tsize; j++) {
	    if (HCONTROL::is_full(table[j]))
		assert(! HCONTROL::equal(table[i], table[j]));
	}
    }

    /* Check that hash values for all available elts do not wrap */
    int* marked = new int[tsize];
    for (i = 0; i < tsize; i++) {
	if (!HCONTROL::is_full(table[i])) continue;

	for (j = 0; j < tsize; j++) {
	    marked[j] = 0;
	}

	int h1 = htable_int_hash(HCONTROL::hash(table[i]));
	int h2 = htable_hash2(h1);
	int first = htable_fix_hash(h1, slotBits);

	int index = first;
	for (j = 0; j < tsize; j++) {
	    assert(! marked[index]);
	    marked[index] = 1;
	    index = (index + h2) & mask;
	}
    }
    delete [] marked;

    /* Check that there are no empty slots in hash sequence for each elt */
    for (i = 0; i < tsize; i++) {
	if (!HCONTROL::is_full(table[i])) continue;

	int h1 = htable_int_hash(HCONTROL::hash(table[i]));
	int h2 = htable_hash2(h1);
	int first = htable_fix_hash(h1, slotBits);

	/* Loop at most tsize times */
	int index = first;
	for (j = 0; j < tsize; j++) {
	    // Check for hole
	    assert(!HCONTROL::is_empty(table[index]));

	    if (index == i) break;
	    index = (index + h2) & mask;
	}
    }

    /* count */
    j = 0;
    for (i = 0; i < tsize; i++) {
	if (HCONTROL::is_full(table[i])) j++;
    }
    assert(count == j);

    /* tsize is power of two */
    i = 1;
    while (i < tsize) {
	i = i << 1;
    }
    assert(i == tsize);

    /* Check that at least one slot is empty */
    assert(tsize > (count + delcount));

    /* mask */
    assert(mask == (tsize - 1));

    /* slotBits */
    assert(tsize == (1 << slotBits));
}

void HTABLE::report_stats(char const* msg) const {
    if (accesses == 0) return;
    fprintf(stderr, "%s: %8d accesses, %6.3f collisions per access\n",
	    msg, accesses,
	    ((double) collisions) / ((double) accesses));
}

void HTABLE::Elements::del() {
    HTABLE* t = ((HTABLE*) table);

    assert((index-1) < t->tsize);
    assert(HCONTROL::is_full(t->table[index-1]));

    t->count--;
    t->delcount++;
    HCONTROL::make_obsolete(t->table[index-1]);
}

void HTABLE::Elements::skip_empty() {
    while ((index < table->tsize) && !HCONTROL::is_full(table->table[index]))
	index++;
}

// Remove the controlling macros now
#undef HTABLE
#undef HTYPE
#undef HCONTROL
