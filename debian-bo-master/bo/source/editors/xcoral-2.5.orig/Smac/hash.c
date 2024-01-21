/* ########################################################################

				 hash.c

   File: hash.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/hash.c
   Description: 
   Created: Tue Feb 21 12:54:36 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 12:54:36 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */



/* Une table de hachage reliant un Object a un Object */


#include <string.h>

#include "hash.h"
#include "mem.h"

static unsigned almost_primify(i)
     unsigned i;
{
  if (! (i & 1)) i += 1;
  if (! (i % 3)) i += 2;
  if (! (i % 5)) do { i += 2; } while (! (i % 3));

  return i;
}

HashItem * HashItem__HashItem(key, data, next)
     Object key;
     Object data;
     HashItem * next;
{
  HashItem * result = (HashItem *) Malloc(sizeof(HashItem));
  
  result->_key = key;
  result->_data = data;
  result->_next = next;

  return result;
}


static unsigned int identity(x) Object x; { return ((unsigned int) x); }
static int negal(x, y) Object x; Object y; { return (x != y); }

static unsigned int strhash(s)
     char * s;
{
  unsigned int result = 0;

  while (*s) result = (result << 3) + *s++;

  return result;
}

HashTable * HashTable__HashTable(size, str)
     unsigned size;
     int str;
{
  HashTable * result = (HashTable *) Malloc(sizeof(HashTable));
  
  result->_size = almost_primify(size);
  result->_table = (HashItem**) Calloc(result->_size, sizeof(HashItem *));
  result->_n_entries = 0;
  result->_hfct = (str)
      ? (FCT(unsigned int, (*),(Object))) strhash
      : (FCT(unsigned int, (*),(Object))) identity;
  result->_cfct = (str)
      ? (FCT(int, (*),(Object, Object))) strcmp
      : (FCT(int, (*),(Object, Object))) negal;

  {
    HashItem ** p = result->_table;
    HashItem ** l = result->_table + size;
    
    while (p != l) *p++ = 0;
  }

  return result;
}



static void HashTable__Rehash(ht)
     HashTable * ht;
{
  unsigned new_size = almost_primify(ht->_size * 2 + 1);
  HashItem ** new_table = (HashItem **) Calloc(new_size, sizeof(HashItem));
  HashItem ** p;
  unsigned n_entries;

  p = new_table;
  {
    HashItem ** l = p + new_size;
  	
    while (p != l) *p++ = 0;
  }

  p = ht->_table;
  n_entries = ht->_n_entries;

  while (n_entries) {
    HashItem * item = *p++;
    
    while (item) {
      HashItem * next = item->_next;
      HashItem ** entry = &new_table[(*ht->_hfct)(item->_key) % new_size];

      item->_next = *entry;
      *entry = item;
      
      if (--n_entries)
	item = next;
      else
	break;
    }
  }

  free(ht->_table);
  ht->_table = new_table;
  ht->_size = new_size;
}


Object HashTable__Search(ht, key)
     HashTable * ht;
     Object key;
{
  HashItem * item;
  
  for (item = ht->_table[(*ht->_hfct)(key) % ht->_size];
       item;
       item = item->_next)
    if (! ht->_cfct(key, item->_key))
      return item->_data;

  return 0;
}


/* Ne regarde pas si la clef est deja la */

void HashTable__Add(ht, key, info)
     HashTable * ht;
     Object key;
     Object info;
{
  HashItem ** entry = &ht->_table[(*ht->_hfct)(key) % ht->_size];

  *entry = HashItem__HashItem(key, info, *entry);
  
  if (((++(ht->_n_entries)) >> 3) >= ht->_size)
    HashTable__Rehash(ht);
}


/* Suppose que la clef est presente */

Object HashTable__Remove(ht, key)
     HashTable * ht;
     Object key;
{
  HashItem ** entry = &ht->_table[(*ht->_hfct)(key) % ht->_size];

  while (ht->_cfct((*entry)->_key, key)) entry = &((*entry)->_next);

  {
    HashItem * item = *entry;
    Object result = item->_data;
  
    *entry = (*entry)->_next;
    HashItem__Delete(item);
    ht->_n_entries -= 1;

    return result;
  }
}


/* Vide la table de hachage */

void HashTable__Empty(ht)
     HashTable * ht;
{
  HashItem ** p;

  p = ht->_table;

  while (ht->_n_entries) {
    HashItem * item = *p;
    
    while (item) {
      HashItem * next = item->_next;

      free(item);
      ht->_n_entries -= 1;
      item = next;
    }
    *p++ = 0;
  }
}


/* Parcours une table de hash */

void HashTable_Iter(table, pitem, ptable)
     HashTable * table;
     HashItem ** pitem;
     HashItem *** ptable;
{
  if (! *ptable)
    *ptable = table->_table - 1;

  if (*pitem) *pitem = (*pitem)->_next;
  
  while (! *pitem)
    if (++(*ptable) >= (table->_table + table->_size)) {
      /* Tout a ete parcouru */
      *ptable = 0;
      *pitem = 0;
      break;
    }
    else
      *pitem = **ptable;
}
