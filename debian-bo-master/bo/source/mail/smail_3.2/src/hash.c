/*
#ident	"@(#)smail/src:RELEASE-3_2:hash.c,v 1.7 1996/02/28 06:47:07 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * hash:
 *	perform a string hashing algorithm functions
 *
 * 	Hash tables are defined by their size.  It is suggested that the
 * 	size be a prime value, say:
 *	    13, 29, 47, 61, 113, 181, 251, 359, 509, 751, 1021, 1511,
 *	    2039, 3079, 4093, 6151, 8179, 12377, 16381, 24571, 32749,
 *	    49139, 65557, 98299, 132049, 180023, 216091, 321367, 521539, ...
 * 	An advantage with the above primes is that they are at last 3 less
 * 	than 2^n, and no closer than 3 away from 3*(2^m) for the larger 
 * 	values of m.  Most malloc systems are most efficient when one allocates
 * 	3 words less than 2^n bytes, i.e., 4*(2^n - 3) bytes.
 *
 *	external functions:
 *		add_to_hash, lookup_in_hash, delete_from_hash,
 *		delete_from_hash, store_in_hash, new_hash_table,
 *		write_hash_table, walk_hash, free_hash_element,
 *		free_hash_table
 */
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "smail.h"
#include "defs.h"
#include "exitcodes.h"
#include "hash.h"
#include "alloc.h"
#ifndef DEPEND
# include "extern.h"
# include "debug.h"
#endif

/* static functions used in this file */
static int hash_str();
static int hash_stric();
static struct hash *new_hash_element();

#ifdef STANDALONE
int debug = 0;	/* default debug level is NONE */
#define errfile stderr
void panic();
#define xmalloc(x) (malloc(x))	/* naive allocator */
#define bmalloc(x,y) (malloc(x))	/* naive allocator */
char *malloc();
#define xfree(x) (free(x))	/* naive deallocator */
#define bfree(x,y) (free(x))	/* naive deallocator */
void free();
#endif	/* STANDALONE */

/*
 * ETHEREAL_HASHDATA -
 *
 *    This file contains a nearly complete implementation of a general
 *    hashing system.  However it is likely that smail itself will
 *    never need to delete, replace, or store new data over old data.
 *    That is, smail will only create, add, lookup and perform hash
 *    table file operations.
 *
 *    The code ifdef-ed out does work and is tested by the STANDALONE code.
 *    You should define ETHEREAL_HASHDATA if you want to use it
 */


/*
 * hash_str - convert a trying into a hash value
 *
 * We build the hash value one character at a time by repeating the following
 * steps for each character:
 *
 *	1) The previous value is shifted up (by HASH_UP_SHIFT) and the
 *	   current character is added
 *	2) any upper level excess bits are fetched (by masking with
 *	   {H,L}_HASH_MASK) and xor-ed into bits near the bottom
 *	3) the upper level excess bits are cleared
 *
 * In the end, the hash value is taken modulo `mod' to produce a slot number.
 *
 * input:
 *	str	- string to hash
 *	mod	- number of hash table entries
 * output:
 *	the slot number on which `str' belongs
 *
 * NOTE: For more optimal hashing of smaller hash tables (entries < HASH_LEVEL)
 *       we L_ value constants.  This gives us a faster hash fold.  For larger
 *	 hash tables, this is not needed so H_ value constants are used.
 *
 * NOTE: mod should be a prime <= ~H_HASH_MASK
 */
static int
hash_str(str, mod)
    register char *str;			/* the string to hash */
    int mod;				/* prime modulus, size of hash table */
{
    register unsigned long val;		/* current hash value */
    register unsigned long excess;	/* upper/excess bits in val */
    register unsigned long c;		/* the current string character */

    /* firewall - bogus case, but be safe anyway */
    if (str == NULL) {
	return 0;
    }

    /*
     * hash each character in the string
     *
     * if our mod is small enough, then use L_ value constants so that
     * strings can fold into themselves faster.
     */
    if (mod < HASH_LEVEL) {
	/* hash each character using the L_ values */
	for (val = 0; c=(unsigned long)*str; ++str) {
	    val = (val << HASH_UP_SHIFT) + c;
	    val ^= ((excess = (val&L_HASH_MASK)) >> L_DOWN_SHIFT);
	    val ^= excess;
	}
    } else {
	/* hash each character using the H_ values */
	for (val = 0; c=(unsigned long)*str; ++str) {
	    val = (val << HASH_UP_SHIFT) + c;
	    val ^= ((excess = (val&H_HASH_MASK)) >> H_DOWN_SHIFT);
	    val ^= excess;
	}
    }
    
    return (int)(val%mod);    /* our hash value, mod the hash size */
}

/*
 * hash_stric - convert a trying into a hash value without regard to case
 *
 * We build the hash value one character at a time by repeating the following
 * steps for each character:
 *
 *	1) The previous value is shifted up (by HASH_UP_SHIFT) and the
 *	   current character is added
 *	2) any upper level excess bits are fetched (by masking with
 *	   {H,L}_HASH_MASK) and xor-ed into bits near the bottom
 *	3) the upper level excess bits are cleared
 *
 * In the end, the hash value is taken modulo `mod' to produce a slot number.
 *
 * input:
 *	str	- string to hash without regard to case
 *	mod	- number of hash table entries
 * output:
 *	the slot number on which `str' belongs
 *
 * NOTE: For more optimal hashing of smaller hash tables (entries < HASH_LEVEL)
 *       we L_ value constants.  This gives us a faster hash fold.  For larger
 *	 hash tables, this is not needed so H_ value constants are used.
 *
 * NOTE: mod should be a prime <= ~H_HASH_MASK
 */
static int
hash_stric(str, mod)
    register char *str;		/* the string to hash disregarding case */
    int mod;			/* prime modulus, size of hash table */
{
    register unsigned long val;		/* current hash value */
    register unsigned long excess;	/* upper/excess bits in val */
    register unsigned long c;		/* the current string character */

    /* firewall - bogus case, but be safe anyway */
    if (str == NULL) {
	return 0;
    }

    /*
     * hash each character in the string
     *
     * if our mod is small enough, then use L_ value constants so that
     * strings can fold into themselves faster.
     */
    if (mod < HASH_LEVEL) {
	/* hash each character using the L_ values */
	for (val = 0; c=(unsigned long)lowercase(*str); ++str) {
	    val = (val << HASH_UP_SHIFT) + c;
	    val ^= ((excess = (val&L_HASH_MASK)) >> L_DOWN_SHIFT);
	    val ^= excess;
	}
    } else {
	/* hash each character using the H_ values */
	for (val = 0; c=(unsigned long)lowercase(*str); ++str) {
	    val = (val << HASH_UP_SHIFT) + c;
	    val ^= ((excess = (val&H_HASH_MASK)) >> H_DOWN_SHIFT);
	    val ^= excess;
	}
    }
    
    return (int)(val%mod);    /* our hash value, mod the hash size */
}


#ifdef ETHEREAL_HASHDATA
/*
 * walk_hash - walk thru a hash table
 *
 * returns NULL if there is no next element in the hash table
 *
 * input:
 *	cur	- our current location, or NULL to start at the beginning
 *	table	- the table to be walked
 * output:
 *	a pointer to the next element, or NULL if no next element
 *
 * WARNING: results will be unpredictable or fatal if `cur' != NULL, and
 *	    `cur' != `the previous walk location', and `cur' is an element
 *	    that has been either deleted or replaced by another element.
 *	    It should be noted that this `cur' will never be the `previous
 *	    walk location' if our previous call ran off the end of the table.
 */
struct hash *
walk_hash(cur, table)
    struct hash *cur;		/* where we are now */
    struct hash_table *table;	/* hash table being walked */
{
    register struct hash *prev;	/* our previous walk location */
    register int indx;		/* our previous hash slot */
    register int len;		/* the table length */

    /*
     * firewall
     */
    if (table == NULL) {
	panic(EX_SOFTWARE, "walk_hash: table is NULL");
    }
    /* fetch these values for faster use */
    prev = table->prev;
    indx = table->indx;
    len = table->len;

    /*
     * find the first hash slot if cur is NULL (due to a restart request)
     */
    if (cur == NULL) {
	/* note that we really don't have a location yet */
	prev = NULL;

	/* find the first slot and return it */
	for (indx=0; indx < len; ++indx) {
	    if (full_slot(table->slot[indx])) {
		/* we found the first entry of the first non-empty chain */
		prev = table->slot[indx];
		break;
	    }
	}

    /*
     * walk from our current location to the next location
     */
    } else {

	/*
	 * if `cur' is not the previous `cur', then find `cur' and
	 * note where our hash table index now resides
	 */
	if (cur != prev) {
	    /* find the hash table index */
	    indx = hash_string(cur->keystr, len, table->flags&HASH_CASEFOLD);
	    /* if `cur' is an empty slot, panic */
	    if (empty_slot(table->slot[indx])) {
		    panic(EX_SOFTWARE, "walk_hash: <%s> hash slot is empty",
			  cur->keystr);
	    }
	
	    /* walk down the hash table chain looking for our entry */
	    for (prev = table->slot[indx];
		 cur != prev && prev != NULL;
		 prev = next_hash(prev,table)) {
	    }
	}
	/* if `cur' is not in the hash table, panic */
	if (prev == NULL) {
	    panic(EX_SOFTWARE, "walk_hash: <%s> is not in table", cur->keystr);
	}

	/*
	 * if we were at the end of a chain, then our successor will
	 * be the start of the next non-empty chain
	 */
	if ((prev = next_hash(prev,table)) == NULL) {
	    /* find the next non-empty chain */
	    for (++indx; indx < len; ++indx) {
		if (full_slot(table->slot[indx])) {
		    /* return first element of this chain */
		    prev = table->slot[indx];
		    break;
	        }
	    }
	}
    }

    /*
     * return the pointer the next element or NULL
     */
    /* remember our location for next time */
    table->prev = hash_addr(prev, table);
    table->indx = indx;
    return table->prev;
}
#endif	/* ETHEREAL_HASHDATA */


/*
 * new_hash_element - creat a new hash element
 *
 * return a malloced new hash element with the lengths correctly filled out
 *
 * inputs:
 *	keystr	- the key of this data element
 *	data	- the data to accompany `keystr', or NULL if no data
 *	datalen	- the length of `data' in bytes, or 0 is no data
 *	table	- the hash table which will get this element
 */
static struct hash *
new_hash_element(keystr, data, datalen, table)
    char *keystr;    			/* the keystring to be added */
    char *data;				/* the associated data if any */
    int datalen;			/* length of data,  0 ==> no data */
    struct hash_table *table;		/* hash table being added to */
{
    struct hash *new;		/* the new slot chain location */
    int keylen;			/* the length of the string, padded */
    unsigned long lk;		/* temp var for key length */

    /*
     * firewall - check for bad pointers and values
     */
    if (keystr == NULL || table == NULL) {
	panic(EX_SOFTWARE, "new_hash_element: NULL keystring or table");
    }
    lk = keystr_len(keystr);		/* compute padded key length */
    if (lk >= (unsigned long)(1L<<BITS_PER_SHORT)) {
	panic(EX_SOFTWARE, "new_hash_element: key too long");
    }
    keylen = (int)lk;		/* now we know it will fit in an int */
    /* firewall - check against bad data being passed to us */
    if (datalen < 0 || (datalen > 0 && data == NULL) || 
      (unsigned long)datalen >= (unsigned long)(1L<<BITS_PER_SHORT))  {
	panic(EX_SOFTWARE,
	    "new_hash_element: bad data passed with: <%s>  datalen: <%d>",
	    keystr, datalen);
    }

    /*
     * malloc the storage
     */
    new = (struct hash *)bmalloc( hashslot_size(keylen,datalen), table->life );
    /* firewall */
    if (is_odd(new)) {
	panic(EX_SOFTWARE,
	      "new_hash_element: malloc returned odd address: %lx",
	      (long)new);
    }

    /*
     * return the prebuild element
     */
    new->succ = NULL;
    new->keylen = keylen;
    strcpy(new->keystr, keystr);
    new->datalen = datalen;
    if (datalen > 0) {
	memcpy(hash_data(new), data, (size_t) datalen);
    }
    return new;
}

#ifdef ETHEREAL_HASHDATA
/*
 * free_hash_element - free an old hash element
 *
 * Frees a hash table element according to the life of the hash table.
 * Removes the hash table element if it in the hash table unless explicitly
 * told that the element is not in the table.
 *
 * inputs:
 *	cur	- the element which which we will free
 *	search	- non-zero means delete `cur' from table prior to the free
 *	table	- the table on which `cur' resides, or the table to which
 *		  `cur' would have been added
 *
 * WARNING: It is important that the `cur' element NOT be in a hash table
 *	    after the free.  Unpredictable results will happen otherwise.
 *	    If `search' is non-zero, we will first attempt to delete `cur'
 *	    from `table'.  It is a fatal error if `search' is non-zero and
 *	    `cur' is not in `table'.
 *
 * WARNING: It is important that `cur' was individually malloced (perhaps
 *	    by new_hash_element) so that the free of its address will
 *	    be valid.
 */
void
free_hash_element(cur, search, table)
    struct hash *cur;		/* what we will delete */
    int search;			/* non-zero => delete `cur' first */
    struct hash_table *table;	/* table `cur' does/would_have belonged to */
{
    /*
     * firewall - check for bad pointers and values
     */
    if (cur == NULL || table == NULL) {
	panic(EX_SOFTWARE, "free_hash_element: NULL cur or table");
    }

    /*
     * delete the element first if requested
     */
    if (search != 0 && delete_from_hash(cur->keystr, table) == NULL) {
	panic(EX_SOFTWARE,"free_hash_element: <%s> not in table",cur->keystr);
    }

    /*
     * free the storage
     */
    bfree(cur, table->life);
    return;
}
#endif	/* ETHEREAL_HASHDATA */

/*
 * new_hash_table - creat a new hash table
 *
 * return a malloced new hash table with correctly setup initial pointers
 *
 * input:
 *	tablelen - number of slots in the hash table
 *	life - the alloc block to which this is to be associated, or NULL
 *	       meaning the permanent block
 * output:
 *	a pointer to a malloced empty hash table
 */
struct hash_table *
new_hash_table(tablelen, life, flags)
    int tablelen;		/* number of slots in the hash table */
    struct block *life;		/* is the hash table permanent or temporary */
    int flags;			/* hash table flag as per struct hash_table */
{
    register int i;			/* index */
    struct hash_table *table;		/* the malloced hash table */

    /*
     * firewalls
     */
    if (tablelen <= 0) {
	panic(EX_SOFTWARE, "new_hash_table: tablelen: %d", tablelen);
    }
    DEBUG3(DBG_HASH_LO,
	   "new_hash_table: tablelen:%d life:%d flag:%d\n",tablelen,life,flags);

    /*
     * malloc the hash table
     */
    table = (struct hash_table *)bmalloc(table_size(tablelen), life);
    /* firewall */
    if (is_odd(table)) {
	panic(EX_SOFTWARE, "new_hash_table: malloc returned odd address: %ld", (long)table);
    }

    /*
     * initialize the table
     */
    table->len = tablelen;
    table->flags = flags & HASH_FLAGMASK;
    table->life = life;
    table->prev = NULL;		/* no current walk_hash() location */
    table->indx = 0;		/* no current walk_hash() slot index */
    for (i=0; i < tablelen; i++) {
	table->slot[i] = NULL;
    }
    return table;	/* return our new table */
}

#ifdef ETHEREAL_HASHDATA
/*
 * free_hash_table - free a hash table and its associated data
 *
 * Free all storage associated with a hash table.
 *
 * NOTE: any malloced elements should be freed prior to calling this routine.
 *
 * input:
 *	table	- the hash table to free
 */
void
free_hash_table(table)
    struct hash_table *table;	/* the hash table to free */
{
    struct hash *cur;		/* current element to delete */

    /*
     * firewalls
     */
    if (table == NULL ) {
	panic(EX_SOFTWARE, "free_hash_table: NULL table");
    }
    DEBUG(DBG_HASH_LO,"free_hash_table: start\n");

    /*
     * free the table slots
     */
    bfree(table, table->life);
    return;
}
#endif	/* ETHEREAL_HASHDATA */


/*
 * add_to_hash - add an element to the a hash table
 *
 * inputs:
 *	keystr	- the key of the data to add
 *	data	- the data to accompany `keystr', or NULL if no data
 *	datalen	- the length of `data' in bytes, or 0 if no data
 *	table	- a pointer to the hash table which is being modified
 * output:
 *	returns ALREADY_HASHED if `keystr' is already in the `table', or
 *	JUST_HASHED if we just added a no key.  The `table' is not modified
 *	if the key is already exists.
 */
int
add_to_hash(keystr, data, datalen, table)
    char *keystr;    			/* the keystring to be added */
    char *data;				/* the associated data if any */
    int datalen;			/* length of data,  0 ==> no data */
    struct hash_table *table;		/* the hash table to add it to */
{
    register struct hash *cur;		/* the current slot chain location */
    register struct hash *prev;		/* the previous slot chain location */
    register int cmp;			/* -1, 0, or 1 for < = > compare */
    register int caseflag;		/* 0 ==> use strcmp, 1 ==> strcmpic */
    int loc;				/* the hash slot to add onto */
    struct hash *new;			/* the new slot chain location */

    /*
     * firewall - watch for NULLs
     */
    if (keystr == NULL) {
	panic(EX_SOFTWARE, "add_to_hash: NULL keystr");
    }
    if (table == NULL) {
	panic(EX_SOFTWARE, "add_to_hash: NULL table");
    }

    /*
     * determine the slot on which this entry is to be added
     */
    caseflag = table->flags & HASH_CASEFOLD;
    loc = hash_string(keystr, table->len, caseflag);
    DEBUG2(DBG_HASH_LO, "add_to_hash: keystr: <%s> slot: %d\n", keystr, loc);

    /*
     * search the slot chain for our entry
     */
    /* special case for empty slot chains */
    if (empty_slot(table->slot[loc])) {
	DEBUG(DBG_HASH_MID, "add_to_hash: insert in NULL slot\n");
	new = new_hash_element(keystr, data, datalen, table);
	insert_hash(&table->slot[loc], new);
	return JUST_HASHED;
    }

    /* 
     * search the chain
     */
    DEBUG2(DBG_HASH_VHI, "add_to_hash: slot:0x%lx cur:0x%lx\n",
	   (long)table->slot[loc], (long)table->slot[loc]);
    for (prev = NULL, cur = table->slot[loc];
	 cur != NULL;
	 prev = cur, cur = next_hash(cur, table)) {
	/* 
	 * if we found the entry, stop
	 */
	DEBUG2(DBG_HASH_VHI, "add_to_hash: comparing <%s> to <%s>",
	       keystr, cur->keystr);
	if ((cmp = stringcmp(keystr, cur->keystr, caseflag)) == 0) {
	    DEBUG(DBG_HASH_MID, "add_to_hash: already hashed\n");
	    return ALREADY_HASHED;

	/* 
	 * we are past the insertion point, insert before here and stop
	 * note if we are inserting at the beginning a a chain or in the middle
	 */
	} else if (cmp < 0) {
	    new = new_hash_element(keystr, data, datalen, table);
	    if (prev == NULL) {
		DEBUG(DBG_HASH_MID, "add_to_hash: insert at front\n");
		insert_hash(&table->slot[loc], new);  /* insert at beginning */
	    } else {
		DEBUG(DBG_HASH_MID, "add_to_hash: insert in middle\n");
		insert_hash(prev, new);	/* insert in middle */
	    }
	    return JUST_HASHED;
	}
    }

    /* 
     * case: insertion at the end of the chain
     */
    DEBUG(DBG_HASH_MID, "add_to_hash: insert at END\n");
    new = new_hash_element(keystr, data, datalen, table);
    insert_hash(prev, new);
    return JUST_HASHED;
}

#ifdef ETHEREAL_HASHDATA
/*
 * replace_in_hash - replace an existing element in a hash table
 *
 * inputs:
 *	keystr	- the key of the data to replace
 *	data	- the data to accompany `keystr', or NULL if no data
 *	datalen	- the length of `data' in bytes, or 0 if no data
 *	table	- a pointer to the hash table which is being modified
 * output:
 *	returns a pointer to the element that was replaced, or NULL
 *	if no element was replaced due to `keystr' not in `table'
 */
struct hash *
replace_in_hash(keystr, data, datalen, table)
    char *keystr;    			/* the keystring to replace */
    char *data;				/* the associated data if any */
    int datalen;			/* length of data,  0 ==> no data */
    struct hash_table *table;		/* the hash table to add it to */
{
    register struct hash *cur;		/* the current slot chain location */
    register struct hash *prev;		/* the previous slot chain location */
    register int cmp;			/* -1, 0, or 1 for < = > compare */
    register int caseflag;		/* 0 ==> use strcmp, 1 ==> strcmpic */
    int loc;				/* the hash slot to add onto */
    struct hash *new;			/* the new slot chain location */

    /*
     * firewall - watch for NULLs
     */
    if (keystr == NULL) {
	panic(EX_SOFTWARE, "replace_in_hash: NULL keystr");
    }
    if (table == NULL) {
	panic(EX_SOFTWARE, "replace_in_hash: NULL table");
    }

    /*
     * determine the slot on which this entry is to be added
     */
    caseflag = table->flags & HASH_CASEFOLD;
    loc = hash_string(keystr, table->len, caseflag);
    DEBUG2(DBG_HASH_LO, "replace_in_hash: keystr: <%s> slot: %d\n",keystr,loc);

    /*
     * search the slot chain for our entry
     */
    /* special case for empty slow chains */
    if (empty_slot(table->slot[loc])) {
	DEBUG(DBG_HASH_MID, "replace_in_hash: slot NULL\n");
	return NULL;	/* no entry to replace */
    }

    /* 
     * search the chain
     */
    for (prev=NULL, cur=table->slot[loc]; cur != NULL; 
      prev=cur, cur=next_hash(cur, table)) {
	/* 
	 * if we found the entry, stop
	 */
	if ((cmp = stringcmp(keystr, cur->keystr, caseflag)) == 0) {
	    new = new_hash_element(keystr, data, datalen, table);
	    if (prev == NULL) {
		DEBUG(DBG_HASH_MID, "replace_in_hash: replaced at front\n");
		/* insert at beginning */
		replace_hash(table->slot[loc], cur, new);
	    } else {
		DEBUG(DBG_HASH_MID, "replace_in_hash: replaced in middle\n");
		replace_hash(prev->succ, cur, new);	/* insert in middle */
	    }
	    return cur;
	/* if we have gone past our entry, stop searching */
        } else if (cmp < 0) {
	    break;
	}
    }

    /* 
     * entry not found, nothing to replace
     */
    DEBUG(DBG_HASH_MID, "replace_in_hash: not found\n");
    return NULL;
}

/*
 * store_in_hash - store an existing element in a hash table
 *
 * inputs:
 *	keystr	- the key of the data to store
 *	data	- the data to accompany `keystr', or NULL if no data
 *	datalen	- the length of `data' in bytes, or 0 if no data
 *	table	- a pointer to the hash table which is being modified
 * output:
 *	returns a pointer to the element that was replaced, or NULL
 *	if no element was replaced.  In any case the element is added.
 */
struct hash *
store_in_hash(keystr, data, datalen, table)
    char *keystr;    			/* the keystring to replace */
    char *data;				/* the associated data if any */
    int datalen;			/* length of data,  0 ==> no data */
    struct hash_table *table;		/* the hash table to add it to */
{
    register struct hash *cur;		/* the current slot chain location */
    register struct hash *prev;		/* the previous slot chain location */
    register int cmp;			/* -1, 0, or 1 for < = > compare */
    register int caseflag;		/* 0 ==> use strcmp, 1 ==> strcmpic */
    int loc;				/* the hash slot to add onto */
    struct hash *new;			/* the new slot chain location */

    /*
     * firewall - watch for NULLs
     */
    if (keystr == NULL) {
	panic(EX_SOFTWARE, "store_in_hash: NULL keystr");
    }
    if (table == NULL) {
	panic(EX_SOFTWARE, "store_in_hash: NULL table");
    }

    /*
     * determine the slot on which this entry is to be added
     */
    caseflag = table->flags & HASH_CASEFOLD;
    loc = hash_string(keystr, table->len, caseflag);
    DEBUG2(DBG_HASH_LO, "store_in_hash: keystr: <%s> loc: %d\n", keystr, loc);

    /*
     * search the slot chain for our entry
     */
    /* special case for empty slow chains */
    if (empty_slot(table->slot[loc])) {
	DEBUG(DBG_HASH_MID, "store_in_hash: insert on NULL slot\n");
	new = new_hash_element(keystr, data, datalen, table);
	insert_hash(&table->slot[loc], new);
	return NULL;
    }

    /* 
     * search the chain
     */
    for (prev=NULL, cur=table->slot[loc]; cur != NULL; 
      prev=cur, cur=next_hash(cur, table)) {
	/* 
	 * if we found the entry, stop
	 */
	if ((cmp = stringcmp(keystr, cur->keystr, caseflag)) == 0) {
	    new = new_hash_element(keystr, data, datalen, table);
	    if (prev == NULL) {
		DEBUG(DBG_HASH_MID, "store_in_hash: replaced at front\n");
		/* insert at beginning */
		replace_hash(table->slot[loc], cur, new);
	    } else {
		DEBUG(DBG_HASH_MID, "store_in_hash: replaced in middle\n");
		replace_hash(prev->succ, cur, new);	/* insert in middle */
	    }
	    return cur;

	/* 
	 * we are past the insertion point, insert before here and stop
	 * note if we are inserting at the beginning a a chain or in the middle
	 */
	} else if (cmp < 0) {
	    new = new_hash_element(keystr, data, datalen, table);
	    if (prev == NULL) {
		DEBUG(DBG_HASH_MID, "store_in_hash: insert at front\n");
		insert_hash(&table->slot[loc], new);  /* insert at beginning */
	    } else {
		DEBUG(DBG_HASH_MID, "store_in_hash: insert in middle\n");
		insert_hash(&prev->succ, new);	/* insert in middle */
	    }
	    return NULL;
	}
    }

    /* 
     * case: insertion at the end of the chain
     */
    DEBUG(DBG_HASH_MID, "store_in_hash: insert at END\n");
    new = new_hash_element(keystr, data, datalen, table);
    insert_hash(&prev->succ, new);
    return NULL;
}
#endif	/* ETHEREAL_HASHDATA */

/*
 * lookup_in_hash - lookup an element in a hash table and return 
 *		    the associated data
 *
 * inputs:
 *	keystr	- the key of the data to add
 *	data	- pointer to a pointer, or 0 if no data is wanted
 *	datalen	- pointer to the data length, or NULL if no length is wanted
 *	table	- a pointer to the hash table which is being modified
 * output:
 *	returns ALREADY_HASHED if `keystr' is already in the `table', or
 *		NOT_HASHED the key was not found
 *	data	- if `data' was non-NULL points at the key's data or NULL
 *		  no data
 *	datalen	- if `datalen' was non-NULL, points at the length of the
 *		  key's data
 */
int
lookup_in_hash(keystr, data, datalen, table)
    char *keystr;			/* the key to lookup */
    char **data;			/* where to point at data, or NULL */
    int *datalen;			/* where to place data len or NULL */
    struct hash_table *table;		/* the hash table to add it to */
{
    register struct hash *cur;	/* the slot chain location */
    int loc;			/* the hash slot to add onto */
    int caseflag;		/* 0 ==> use strcmp, 1 ==> strcmpic */
    int cmp;			/* compare function result */

    /*
     * firewall - watch for NULLs
     */
    if (keystr == NULL) {
	panic(EX_SOFTWARE, "lookup_in_hash: NULL keystr");
    }
    if (table == NULL) {
	panic(EX_SOFTWARE, "lookup_in_hash: table is NULL");
    }

    /*
     * determine the hash slot to search on
     */
    caseflag = table->flags & HASH_CASEFOLD;
    loc = hash_string(keystr, table->len, caseflag);
    DEBUG2(DBG_HASH_LO, "lookup_in_hash: keystr: <%s> slot: %d\n",keystr,loc);
    /* watch out for empty chains, there is nothing on them */
    if (empty_slot(table->slot[loc])) {
	DEBUG(DBG_HASH_MID, "lookup_in_hash: found at slot END\n");
	return NOT_HASHED;
    }

    /* 
     * search the chain
     */
    for (cur=table->slot[loc]; cur != NULL; cur=next_hash(cur, table)) {
	/*
	 * if we found the entry, stop
	 */
	if ((cmp = stringcmp(keystr, cur->keystr, caseflag)) == 0) {
	    DEBUG(DBG_HASH_MID, "lookup_in_hash: found\n");
	    /*
	     * fill in the requested args
	     */
	    if (data != NULL) {
		*data = hash_data(cur);	
	    }
	    if (datalen != NULL) {
		*datalen = cur->datalen;
	    }
	    return ALREADY_HASHED;
	/* if we have gone past our entry, stop searching */
        } else if (cmp < 0) {
	    break;
	}
    }

    /* found nothing */
    DEBUG(DBG_HASH_MID, "lookup_in_hash: not found\n");
    return NOT_HASHED;
}

#ifdef ETHEREAL_HASHDATA
/*
 * delete_from_hash - delete an element in the hash table
 *
 * inputs:
 *	keystr	- the key of the data to add
 *	table	- a pointer to the hash table which is being modified
 * output:
 *	returns a pointer to the element that was deleted, or NULL
 *	if no element was deleted due to `keystr' not in `table'
 */
struct hash *
delete_from_hash(keystr, table)
    char *keystr;			/* the key to lookup */
    struct hash_table *table;		/* the hash table to add it to */
{
    register struct hash *cur;		/* the slot chain location */
    register struct hash *prev;		/* previous element */
    int loc;			/* the hash slot to add onto */
    int caseflag;		/* 0 ==> use strcmp, 1 ==> strcmpic */
    int cmp;			/* compare function result */

    /*
     * firewall - watch for NULLs
     */
    if (keystr == NULL) {
	panic(EX_SOFTWARE, "delete_from_hash: keystr is NULL");
    }
    if (table == NULL) {
	panic(EX_SOFTWARE, "delete_from_hash: table is NULL");
    }

    /*
     * determine the hash slot to search on
     */
    caseflag = table->flags & HASH_CASEFOLD;
    loc = hash_string(keystr, table->len, caseflag);
    DEBUG2(DBG_HASH_LO, "delete_from_hash: keystr: <%s> loc: %d\n",keystr,loc);
    /* watch out for empty chains, there is nothing on them */
    if (empty_slot(table->slot[loc])) {
	DEBUG(DBG_HASH_MID, "delete_from_hash: EMPTY slot\n");
	return NULL;	/* key is not in the table */
    }

    /* 
     * search the chain for the element to delete
     */
    for (prev=NULL, cur=table->slot[loc]; cur != NULL; 
      prev=cur, cur=next_hash(cur, table)) {
	/* 
	 * if we found the entry, stop
	 */
	if ((cmp = stringcmp(keystr, cur->keystr, caseflag)) == 0) {
	    if (prev == NULL) {
		DEBUG(DBG_HASH_MID, "delete_from_hash: delete at front\n");
		/* delete at the beginning */
		delete_hash(&table->slot[loc], cur);
	    } else {
		DEBUG(DBG_HASH_MID, "delete_from_hash: delete in middle\n");
		delete_hash(&prev->succ, cur);	/* delete in middle */
	    }
	    return cur;
	/*
	 * if we have gone past the entry, stop looking
	 */
        } else if (cmp < 0) {
	    DEBUG(DBG_HASH_MID, "delete_from_hash: past spot\n");
	    break;
        }
    }

    /* found nothing */
    DEBUG(DBG_HASH_MID, "delete_from_hash: not found\n");
    return NULL;		/* nothing was deleted */
}
#endif	/* ETHEREAL_HASHDATA */


/*
 * write_hash_table - write a hash table to a stream
 *
 * input:
 *	tab	- the hash table to write
 *	stream	- the stream on which to write
 */
void
write_hash_table(table, stream)
    struct hash_table *table;	/* the table to write */
    FILE *stream;		/* the stream on which to write table */
{
    register int i;		/* index */
    long tab_loc;		/* file location of hash_table start */
    long loc;			/* current location */
    int size;			/* size of the current element */
    long offset;                /* current offset within the file */
    long disc_succ;             /* cur->succ as written to disc */
    struct hash *cur;		/* the current hash element */
    struct hash_table *tab;	/* disk copy of table */

    /*
     * firewalls
     */
    if (table == NULL || stream == NULL) {
	panic(EX_SOFTWARE, "write_hash_table: NULL arguments");    
    }
    if (table->len <= 0) {
	panic(EX_SOFTWARE, "write_hash_table: table length: %d", table->len);
    }
    DEBUG2(DBG_HASH_LO, "write_hash_table: size: %d life: %d\n",
	   table->len, table->life);

    /*
     * allocate a temporary disk copy of the hash table
     */
    tab = (struct hash_table *)xmalloc(table_size(table->len));
    tab->life = table->life;		/* preserve life type */
    tab->len = table->len;		/* preserve table length */
    tab->flags = table->flags;		/* preserve flags - XXX all bits ??? */
    tab->prev = NULL;			/* clear walking location */
    tab->indx = 0;			/* clear walking slot index */

    /*
     * skip the hash table block
     */
    tab_loc = ftell(stream);
    if (fseek(stream, (long)table_size(table->len), 1) < 0) {
	panic(EX_IOERR, "write_hash_table: bad skip of %d over table",
	    table_size(table->len));
    }

    /*
     * write out each hash table chain
     */
    for (i=0; i < table->len; i++) {

	/* don't write out chains that don't exist */
	if (empty_slot(table->slot[i])) {
		/* slot is empty, deal with it quickly */
		set_ptr(tab->slot[i], (int)NULL);
		continue;
	}

	/* note starting offset of hash chain */
	offset = ftell(stream) - tab_loc;
	if (is_odd(offset)) {
		panic(EX_IOERR, "write_hash_table: slot %d offset is odd:%ld",
		      i, offset);
	}
	set_ptr(tab->slot[i], to_odd(offset));

	/* write up to the last chain element */
	for (cur=table->slot[i]; cur != NULL; cur=next_hash(cur, table)) {
	    /* compute the current element length */
	    size = hash_len(cur);
	    if (is_odd(size)) {
		    panic(EX_IOERR,
			  "write_hash_table: size is odd:%ld for <%s>",
			  offset, cur->keystr);
	    }
	    offset += size;		/* note file movement */
	    /* write the hash element disk successor element */
	    disc_succ = (cur->succ == NULL) ? 0 : to_odd(offset);
	    if (fwrite((char *)&disc_succ, sizeof(disc_succ), 1, stream) != 1) {
		panic(EX_IOERR,
		      "write_hash_table: bad succ write <%s> on slot %d",
		      cur->keystr, i);
	    }
	    /* write the rest of the hash element data */
	    if (fwrite((char *)cur+sizeof(cur->succ), size-sizeof(cur->succ),
		       1, stream) != 1) {
		panic(EX_IOERR,
		      "write_hash_table: bad write <%s> on slot %d",
		      cur->keystr, i);
	    }
	}
    }

    /*
     * write the hash table back in its place and return to our
     * current position
     */
    loc = ftell(stream);	/* remember our current location */
    if (fseek(stream, (long)tab_loc, 0) < 0) {
	panic(EX_IOERR, "write_hash_table: bad skip back to table at %d",
	    tab_loc);
    }
    if (fwrite((char *)tab, table_size(tab->len), 1, stream) != 1) {
	panic(EX_IOERR, "write_hash_table: bad table write");
    }
    if (fseek(stream, (long)loc, 0) < 0) {
	panic(EX_IOERR, "write_hash_table: bad end seek to %d", loc);
    }

    /*
     * free the temporary disk copy of the hash table
     */
    xfree((char *)tab);
    return;
}


#ifdef STANDALONE

#include <sys/types.h>
#include <sys/stat.h>

#define TABLE_LEN 3 /* use only a few slots to stress the chain code */
#define INPUT_SIZE (70*1024)	/* max input line */

char *tempname = "/tmp/hashtestXXXXXX";	/* tempory hash file name */

void
main(argc, argv)
    int argc;	/* arg count */
    char *argv[]; 	/* args */
{
    int i;			/* index */
    char buf[INPUT_SIZE+1];	/* the input buffer for stdin args */
    struct hash *cur;		/* pointer to walk the table */
    struct hash_table *table;	/* our allocated table */
    struct hash_table *tableic;	/* allocated table without regard to case */
    void test();		/* test an with an element */
    void dump_hash_table();	/* dump the contents of the hash table */
    void hash_file_test();	/* perform hash file testing */

    /*
     * establish debug level
     */
    DEBUG(DBG_HASH_LO, "main: start\n");
    if (argc > 1 && strncmp(argv[1], "-d", 2) == 0) {
	    /* we have a debug level, use it */
	    if (argv[1][2] != '\0') {
		    debug = atoi(&argv[1][2]);
		    --argc;
		    ++argv;
	    } else if (argc > 2) {
		    debug = atoi(argv[2]);
		    argc -= 2;
		    argv += 2;
	    }
    }
    DEBUG1(DBG_HASH_LO, "main: debug level: %d\n", debug);

    /*
     * setup a hash table
     */
    table = new_hash_table(TABLE_LEN, (struct block *)NULL, HASH_CASEFOLD);
    tableic = new_hash_table(TABLE_LEN, (struct block *)NULL, 0);

    /*
     * special case: no args means read one arg per line
     */
    if (argc == 1) {
	while(fgets(buf, INPUT_SIZE, stdin) != NULL) {
	    i = strlen(buf);
	    buf[i-1] = '\0';
	    DEBUG1(DBG_HASH_LO,
		   "main: testing <%s> -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n", buf);
	    test(buf, table);
	    if ( debug >= DBG_HASH_HI ) {
		dump_hash_table(table);
	    }
	    DEBUG1(DBG_HASH_LO,
		   "main: testing ignore case <%s> -*-*-*-*-*-*-*-*-*-\n", buf);
	    test(buf, tableic);
	    if ( debug >= DBG_HASH_HI ) {
		dump_hash_table(tableic);
	    }
	}

    /*
     * hash each argument
     */
    } else {
	for (i=1; i < argc; ++i) {
	    DEBUG1(DBG_HASH_LO,
		   "main: testing <%s> -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n", buf);
	    test(argv[i], table);
	    if ( debug >= DBG_HASH_HI ) {
		dump_hash_table(table);
	    }
	    DEBUG1(DBG_HASH_LO,
		   "main: testing ignore case <%s> -*-*-*-*-*-*-*-*-*-\n", buf);
	    test(argv[i], tableic);
	    if ( debug >= DBG_HASH_HI ) {
		dump_hash_table(tableic);
	    }
	}
    }

    /*
     * test the file operations
     */
    DEBUG(DBG_HASH_LO, "main: hash_file_test\n");
    hash_file_test(table);
    DEBUG(DBG_HASH_LO, "main: hash_file_test ignore case\n");
    hash_file_test(tableic);

    /*
     * final cleanup
     */
    DEBUG(DBG_HASH_LO, "main: free memory\n");
    /* free the hash table elements */
    for (cur=walk_hash((struct hash *)NULL,table);
	 cur != NULL;
	 cur=walk_hash(cur,table))
    {
	free_hash_element(cur, 0, table); /* free without deletion */
    }
    free_hash_table(table);	/* free up the memory */
    DEBUG(DBG_HASH_LO, "main: free memory ignore case\n");
    for (cur=walk_hash((struct hash *)NULL,tableic);
	 cur != NULL;
	 cur=walk_hash(cur,tableic))
    {
	free_hash_element(cur, 0, tableic); /* free without deletion */
    }
    free_hash_table(tableic);	/* free up the memory */
    DEBUG(DBG_HASH_LO, "main: end\n");
    exit(EX_OK);
}

/*
 * perform various tests on a string
 */
void
test(buf, table)
    char *buf;		/* the key to add */
    struct hash_table *table;	/* our allocated table */
{
    register struct hash *cur;	/* the current hash entry */
    char *data;			/* the data we stored */
    int datalen;		/* length of data */
    int buflen;			/* length of the buffer string + NULL */
    int i;			/* index */
    register int caseflag;	/* 0 ==> use strcmp, 1 ==> strcmpic */

    /* test adding */
    buflen = strlen(buf)+1;
    i = add_to_hash(buf, buf, buflen, table);
    DEBUG2(DBG_HASH_LO, "test: <%s> add: %d\n", buf, i);

    /* test the lookup function */
    DEBUG1(DBG_HASH_MID, "test: <%s> lookup\n", buf);
    caseflag = table->flags & HASH_CASEFOLD;
    if (lookup_in_hash(buf, &data, &datalen, table) != ALREADY_HASHED) {
	panic(EX_SOFTWARE,
	      "test: add_to_hash lost <%s>", buf);
    } else if (stringcmp(buf, data, caseflag) != 0 || buflen != datalen) {
	panic(EX_SOFTWARE,
	      "test: add_to_hash lookup <%s> != <%s>", buf, data);
    }
    i = add_to_hash(buf, buf, buflen, table);
    if (i != ALREADY_HASHED) {
	panic(EX_SOFTWARE, "test: add_to_hash returned: %d for #2\n", i);
    }

    /* test the delete function */
    DEBUG1(DBG_HASH_MID, "test: <%s> delete\n", buf);
    cur = delete_from_hash(buf, table);	/* delete something that exists */
    if (cur == NULL) {
	panic(EX_SOFTWARE,
	      "test: delete_from_hash unable to delete <%s>", buf);
    } else if (stringcmp(buf, hash_data(cur), caseflag) != 0 ||
	       buflen != cur->datalen) {
	panic(EX_SOFTWARE,
	      "test: delete_from_hash mis delete of <%s>", buf);
    } else {
	free_hash_element(cur, 0, table); /* free up memory */
    }
    DEBUG1(DBG_HASH_MID, "test: <%s> empty delete\n", buf);
    cur = delete_from_hash(buf, table);	/* delete a nothing */
    if (cur != NULL) {
	panic(EX_SOFTWARE,
	      "test: delete_from_hash ghost delete #2 of <%s>", buf);
    }

    /* test the store function */
    DEBUG1(DBG_HASH_MID, "test: <%s> store\n", buf);
    cur = store_in_hash(buf, buf, buflen, table);
    if (cur != NULL) {
	panic(EX_SOFTWARE,
	      "test: store_in_hash ghost store of <%s>", buf);
    }
    DEBUG1(DBG_HASH_MID, "test: <%s> store #2\n", buf);
    cur = store_in_hash(buf, buf, buflen, table);
    if (cur == NULL) {
	panic(EX_SOFTWARE,
	      "test: store_in_hash lost store #2 of <%s>", buf);
    } else if (stringcmp(buf, hash_data(cur), caseflag) != 0 ||
	       buflen != cur->datalen) {
	panic(EX_SOFTWARE,
	      "test: store_in_hash mis store #2 of <%s>", buf);
    } else {
	free_hash_element(cur, 0, table); /* free up memory */
    }
    
    /* test the replace function */
    DEBUG1(DBG_HASH_MID, "test: <%s> replace_in_hash\n", buf);
    cur = replace_in_hash(buf, buf, buflen, table);
    if (cur == NULL) {
	panic(EX_SOFTWARE,
	      "test: replace_in_hash lost <%s>", buf);
    } else if (stringcmp(buf, hash_data(cur), caseflag) != 0 ||
	       buflen != cur->datalen) {
	panic(EX_SOFTWARE,
	      "test: replace_in_hash mis replace of <%s>", buf);
    } else {
	free_hash_element(cur, 0, table); /* free up memory */
    }
    DEBUG1(DBG_HASH_MID, "test: <%s> replace_in_hash empty\n", buf);
    cur = delete_from_hash(buf, table);	/* delete something that exists */
    if (cur == NULL) {
	panic(EX_SOFTWARE,
	      "test: delete_from_hash unable to delete #3 <%s>", buf);
    } else {
	free_hash_element(cur, 0, table); /* free up memory */
    }
    cur = replace_in_hash(buf, buf, buflen, table);
    if (cur != NULL) {
	panic(EX_SOFTWARE,
	      "test: replace_in_hash ghost replace of <%s>", buf);
    } else {
	/* put it back for keeps */
	cur = store_in_hash(buf, buf, buflen, table);
	if (cur != NULL) {
	    panic(EX_SOFTWARE,
		  "test: store_in_hash store #3 lost <%s>", buf);
        }
    }
}

/*
 * dump_hash_table - dump the hash table
 */
void
dump_hash_table(table)
    struct hash_table *table;	/* our allocated table */
{
    int i;				/* index */
    struct hash *cur;		/* the current hash entry */

    /*
     * check the hash chains
     */
    for (i=0; i < TABLE_LEN; ++i) {
	DEBUG1(DBG_HASH_HI, "dump: slot[%d]:", i);
	/* check for empty slots */
	if (empty_slot(table->slot[i])) {
	    DEBUG(DBG_HASH_HI, "Empty\n");
	    continue;
	}
	for (cur=table->slot[i]; cur != NULL; cur=next_hash(cur, table)) {
	    if (cur->keylen == 0) {
		DEBUG2(DBG_HASH_HI, " 0x%lx: <%s> :EOC ",
		   (long)cur, cur->keystr);
	    } else {
		DEBUG3(DBG_HASH_HI, " 0x%lx: <%s> :0x%lx ",
		   (long)cur, cur->keystr, (long)cur->succ);
	    }
	}
	DEBUG(DBG_HASH_HI, "\n");
    }
}

/*
 * hash_file_test - test the hash table file operations
 */
void
hash_file_test(table)
    struct hash_table *table;		/* the hash table to operate on */
{
    char *filename;		/* the file to operate on */
    struct stat buf;		/* file status buffer */
    struct hash *cur;		/* current location in hash table */
    struct hash *cur2;		/* current location in hash table2 */
    struct hash_table *table2;	/* the hash table to operate on */
    int caseflag;		/* 0 ==> use strcmp, 1 ==> strcmpic */
    char *template;		/* template for mktemp */
    FILE *stream;		/* the file stream */
    char *mktemp();		/* form a temp filename */

    /*
     * open up a file to perform hash table operations into
     */
    template = xmalloc(strlen(tempname)+1);
    strcpy(template, tempname);
    filename = mktemp(template);
    DEBUG1(DBG_HASH_LO, "hash_file_test: using <%s>\n", filename);
    stream = fopen(filename, "w");
    if (stream == NULL) {
	panic(EX_CANTCREAT, "hash_file_test: can not creat <%s>", filename);
    }

    /*
     * write out the hash table
     */
    write_hash_table(table, stream);
    if (fclose(stream) != 0) {
	panic(EX_IOERR, "hash_file_test: can not fclose\n");
    }

    /*
     * reread the hash table into memory
     */
    DEBUG1(DBG_HASH_MID, "hash_file_test: rereading <%s>\n", filename);
    stream = fopen(filename, "r");
    if (stream == NULL) {
	panic(EX_CANTCREAT, "hash_file_test: can not reopen <%s>", filename);
    }
    if (fstat(fileno(stream), &buf) < 0) {
	panic(EX_IOERR, "hash_file_test: can not stat <%s>", filename);
    }
    table2 = (struct hash_table *)xmalloc(buf.st_size * sizeof(char));
    if (fread((char *)table2,sizeof(char),buf.st_size,stream) != buf.st_size) {
	panic(EX_IOERR, "hash_file_test: can not reread <%s>", filename);
    }

    /*
     * walk thru each hash table and verify string/data
     */
    caseflag = table->flags & HASH_CASEFOLD;
    DEBUG(DBG_HASH_MID, "hash_file_test: verify\n");
    for (cur=walk_hash((struct hash *)NULL,table),
	 	cur2=walk_hash((struct hash *)NULL,table2);
	 cur != NULL || cur2 != NULL;
	 cur=walk_hash(cur,table), cur2=walk_hash(cur2,table2))
    {
	    /* compare cur and cur2 */
	    if (stringcmp(cur->keystr,cur2->keystr,caseflag) != 0) {
		panic(EX_SOFTWARE,
		      "hash_file_test: key mismatch: <%s> != <%s>\n",
		      cur->keystr, cur2->keystr);
	    }
	    if (cur->datalen != cur2->datalen) {
		panic(EX_SOFTWARE,
		      "hash_file_test: key mismatch: %d != %d\n",
		      cur->datalen, cur2->datalen);
	    }
	    if (memcmp(hash_data(cur), hash_data(cur2), cur->datalen) != 0) {
		panic(EX_SOFTWARE,
		      "hash_file_test: data mismatch between <%s> and <%s>\n",
		      cur->keystr, cur2->keystr);
	    }
    }

    /*
     * cleanup
     */
    DEBUG(DBG_HASH_MID, "hash_file_test: cleanup\n");
    xfree(table2);
}

#endif	/* STANDALONE */
