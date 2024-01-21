/*
 *  hash table management
 * 
 *  Copyright (c) 1997 Alfredo K. Kojima
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

#ifndef HASHTABLE_H_
#define HASHTABLE_H_


typedef struct HashEntry {
    struct HashEntry *nptr, *pptr;     /* linked list */
    unsigned long key;
    unsigned long data;
    struct HashEntry *next;	       /* next on the linked-list for 
					* collisions */
} HashEntry;


/*
 * A generic hash table structure
 */
typedef struct HashTable {
    int elements;		       /* elements stored in the table */
    int size;			       /* size of the table */
    HashEntry **table;
    HashEntry *last;		       /* last on the linked list */
} HashTable;



HashTable *table_init(HashTable *table);
void table_idestroy(HashTable *table);
void table_iput(HashTable *table, unsigned long key, unsigned long data);
int table_iget(HashTable *table, unsigned long key, unsigned long *data);
void table_idelete(HashTable *table, unsigned long key);


#endif
