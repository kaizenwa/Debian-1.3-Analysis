/* hashtab.c
 *
 * generic hash table routines */

#include "config.h"

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "hashtab.h"
#include "common.h"

/* Until I get off my ass, we'll only allocate fixed-sized hash
   tables.  It should be easy enough to grow tables and rehash
   everything later on (at insert time). */

static int hash_sizes[] = {
  7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521
};

static int num_hash_sizes = sizeof (hash_sizes) / sizeof (hash_sizes[0]);

/* A decent string hash function */

static
unsigned long
hash (const char *s, unsigned int len, unsigned long hashsize)
{
  unsigned long hashval;

  for (hashval = 0; len; s++, len--)
    hashval = *s + 31 * hashval;
  return hashval % hashsize;
}


/* Create and return a new hash table.  If NUMERIC is non-zero, items
   to be hashed are numeric and shouldn't be truncated like
   strings. */

struct hashtab *
hashtab_init (int numeric)
{
  struct hashtab *new;

  new = (struct hashtab *) xmalloc (sizeof (struct hashtab));
  memset (new, 0, sizeof (struct hashtab));
  new->numeric = numeric;
  
  return new;
}


/* Return the smaller of the length of a string or the number passed
   in.  If 0 is passed, return the string length unconditionally. */

static
unsigned int
get_key_len (char *s, unsigned int len, int numeric)
{
  if (numeric)
    {
      if (len == 0)
	fatal ("hashtab.c (get_key_len): hashing a num with len == 0");
      if (len != numeric)
	fatal ("hashtab.c (get_key_len): hashing a num with len == numeric");
      return len;
    }
  else
    {
      if (!len)
	return strlen (s);
      else
	{
	  unsigned int i;
	  for (i = len; i && (*s != '\0'); s++, i--)
	    ;
	  return len - i;
	}
    }
}


/* Create an entry for the given key.  If the key already exists,
   return the existing entry with the data cleared.  If LEN is 0,
   assume that the string is null-terminated.  Otherwise, only use LEN
   bytes. */

struct hashtab_elem *
hashtab_create (struct hashtab *ht, void *key, unsigned int len)
{
  unsigned long hashval;
  unsigned int key_len;
  struct hashtab_elem *he;
  
  if (ht->table == NULL)
    {
      /* Create a fresh table. */
      ht->table_size = hash_sizes[0]; /* start small */
      ht->table = (struct hashtab_elem **)
	xmalloc (sizeof (struct hashtab_elem *) * ht->table_size);
      memset (ht->table, 0, sizeof (struct hashtab_elem *) * ht->table_size);
    }

  /* Hash the key. */

  key_len = get_key_len (key, len, ht->numeric);
  hashval = hash (key, key_len, ht->table_size);

  /* We could use HASHTAB_FIND, but then we'd have to call STRLEN
     twice & etc. */

  for (he = ht->table[hashval]; he != NULL; he = he->next)
    {
      if ((he->key_len == key_len) && (memcmp (he->key, key, key_len) == 0))
	{
	  /* Clear the existing data and return the entry. */
	  he->data = NULL;
	  return he;
	}
    }

  /* Create a new entry, since we didn't find one. */

  he = (struct hashtab_elem *) xmalloc (sizeof (struct hashtab_elem));
  he->key_len = key_len;
  he->key = (char *) xmalloc (sizeof (char) * (he->key_len + 1));
  memcpy (he->key, key, key_len);

  /* Make sure the key is null-terminated -- this won't hurt if we're
     using a numeric hash value, since the extra zero will never get
     considered. */

  ((char *) he->key)[he->key_len] = '\0';
  he->ht = ht;
  he->prev = NULL;
  he->hashval = hashval;
  he->next = ht->table[hashval];
  if (ht->table[hashval])
    ht->table[hashval]->prev = he;
  ht->table[hashval] = he;

  return he;
}


/* Find KEY in HT and return the entry associated with it.  If LEN is
   0, assume that the string is null-terminated.  Otherwise, only use
   LEN bytes. */

struct hashtab_elem *
hashtab_find (struct hashtab *ht, void *key, unsigned int len)
{
  unsigned long hashval;
  unsigned int key_len;
  struct hashtab_elem *he;
  
  if (ht->table == NULL)
    return NULL;

  key_len = get_key_len (key, len, ht->numeric);
  hashval = hash (key, key_len, ht->table_size);
  
  for (he = ht->table[hashval]; he != NULL; he = he->next)
    if ((he->key_len == key_len) && (memcmp (he->key, key, key_len) == 0))
      return he;

  return NULL;
}


/* Return the key associated with HE. */

void *
hashtab_get_key (struct hashtab_elem *he)
{
  return he->key;
}


/* Return the data associated with HE. */

void *
hashtab_get_value (struct hashtab_elem *he)
{
  return he->data;
}


/* Set the data for HE. */

void
hashtab_set_value (struct hashtab_elem *he, void *v, unsigned int len)
{
  he->data = (void *) xmalloc (len);
  memcpy (he->data, v, len);
}


/* Return the first thing in the table. */

struct hashtab_elem *
hashtab_first (struct hashtab *ht, struct hashtab_order *ho)
{
  ho->which = 0;
  ho->elem = NULL;
  ho->ht = ht;

  return hashtab_next (ho);
}


/* Given HO, return the next entry in the hash table. */

struct hashtab_elem *
hashtab_next (struct hashtab_order *ho)
{
  unsigned long i;
  struct hashtab_elem *he;

  if (ho->elem)
    {
      he = ho->elem;
      ho->elem = he->next;
      if (he->next == NULL)
	ho->which++;
      return he;
    }

  for (i = ho->which; i < ho->ht->table_size; i++)
    for (he = ho->ht->table[i]; he != NULL; he = he->next)
      {
	ho->which = (he->next ? i : i + 1);
	ho->elem = he->next;
	return he;
      }

  return NULL;			/* nothing in the table */
}


void
hashtab_dump_keys (struct hashtab *ht, FILE *out)
{
  struct hashtab_order ho;
  struct hashtab_elem *he;
  
  for (he = hashtab_first (ht, &ho);
       he != NULL;
       he = hashtab_next (&ho))
    fprintf (stddebug, "%s (%d)\n", (char *) he->key, he->key_len);
}


/* Delete the given element from the hash table.  We want to be able
   to call this in between calls to HASHTAB_NEXT so we can use the
   obvious way to delete items from the table. */

void
hashtab_delete (struct hashtab_elem *he)
{
  if (he->prev)
    he->prev->next = he->next;
  else
    he->ht->table[he->hashval] = he->next;

  if (he->next)
    he->next->prev = he->prev;

  if (he->data)
    free (he->data);
  free (he->key);
  free (he);
}
