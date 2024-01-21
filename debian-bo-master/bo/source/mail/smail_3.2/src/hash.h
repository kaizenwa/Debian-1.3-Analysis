/*
#ident	"@(#)smail/src:RELEASE-3_2:hash.h,v 1.7 1996/02/28 14:26:30 woods Exp"
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
 *	definitions for the support of hash.c
 */

/*
 * hash function values
 */
/* shift the hash up HASH_UP_SHIFT bits before adding the next character */
#define HASH_UP_SHIFT (3)
/* if hash mod < (1<<HASH_LEVEL_SHIFT), use L_ hash values, otherwise H_ */
#define HASH_LEVEL_SHIFT (17)
#define HASH_LEVEL (1<<HASH_LEVEL_SHIFT)
/* for hash sizes where the hash mod < HASH_LEVEL */
#define L_MASK_SHIFT (HASH_LEVEL_SHIFT)
#define L_HASH_MASK (~((1<<L_MASK_SHIFT)-1))     /* mask of excess bits */
#define L_DOWN_SHIFT (L_MASK_SHIFT-1)		 /* shift down excess */
/* for hash sizes where the hash mod >= HASH_LEVEL */
#define H_MASK_SHIFT (BITS_PER_LONG-HASH_UP_SHIFT-2)
#define H_HASH_MASK (~((1<<H_MASK_SHIFT)-1))	 /* mask of excess bits */
#define H_DOWN_SHIFT (H_MASK_SHIFT-2)		 /* shift down excess */

/*
 * what add_to_hash(), lookup_in_hash(), ... return
 */
#define NOT_HASHED (-1)		/* the key was not hashed */
#define JUST_HASHED (0)		/* the key was just hashed */
#define ALREADY_HASHED (1)	/* the key has been already hashed */

/*
 * hash_table - hash slots which map integers to mixed chains of hash elements
 *
 * A hash table consists of a ``struct hash_table'' and related hash slot
 * chains of ``struct hash''.  A hash table contains the number of slots,
 * and that number of slot pointers.  Each slot points to a slot chain
 * of ``struct hash'' elements.  Hash slot chains are kept in sorted order.
 *
 * The function hash_str() maps a string the index of one of the hash table
 * slot pointers.  A slot that does not have a chain has the value NULL.
 */
struct hash_table {
    int len;		/* the number of hash slots in this table */
    int flags;		/* see flags section, default == 0 */
    int indx;		/* the walk_hash() slot location */
    struct hash *prev;	/* the walk_hash() current element location */
    struct block *life;	/* the malloc block storage belongs to */
    struct hash *slot[1];	/* ``len'' consecutive slot chain pointers */
};
/* hash table entry size in bytes - given the number of slots */
#define table_size(len) \
    (((len)*sizeof(struct hash *)) + OFFSET(hash_table, slot[0]))
/* return TRUE if the hash table slot is empty, not-TRUE otherwise */
#define empty_slot(slot) ((struct hash *)(slot) == NULL)
/* return FALSE if the hash table slot is empty, not-FALSE otherwise */
#define full_slot(slot) ((struct hash *)(slot) != NULL)

/*
 * hash flags
 */
#define HASH_CASEFOLD	0x00000001	/* 0 ==> use strcmpic, 1 ==> a != A */
#define HASH_DISKDATA	0x00000002	/* 0 ==> data in mem, 1 ==> on disk */
#define HASH_DISKTBL	0x00000004	/* 0 ==> hashtbl in mem, 1 ==> error */
#define HASH_FLAGMASK	0x00000007	/* logical and of valid flags */
#define HASH_DEFAULT	HASH_CASEFOLD	/* use strcmpic, everything in memory */

/*
 * hash:
 *	variable length hash table structure found in hash slot chains
 *
 * Hash slot chains may be a mixture of arrays of ``struct hash'' elements
 * and linked lists of ``struct hash'' elements.  The reason for this mixture
 * is that some of the data is pre-constructed on disk by programs that
 * have no knowledge addresses, and thus simply stack data elements one after
 * another in an array.  Then again, some of the data is malloced and inserted 
 * into lists at run time, and thus must be linked in by pointers.
 *
 * To deal with this problem, the following methods are used:
 *
 *  hash entries in array form:
 *    is_odd(cur->succ) is true
 *    The next entry is hash_len(cur) bytes beyond `cur'
 *
 *  hash entries in queue form:
 *    is_even(cur->succ) is true
 *    cur->succ == NULL ==> end of chain
 *
 * A hash slot chain consists of a set of ``struct hash'' elements whose key
 * strings mapped onto the same hash table slot index.  All hash slot chain
 * elements are kept in sorted order as defined by memcmp().  (smail needs
 * to compare/hash without regard to case, so it uses strcmpic() instead)
 *
 * The location ``element.keystr'' is the starting location of the key string.
 * The length of the key string is ``element.keylen''.  Each key string must
 * be NULL byte terminated.
 *
 * One can may optionally associate data with the element.  The length of
 * this data is found in ``element.datalen''.  Extra bytes may be added to
 * pad the ``element'' to a BYTES_PER_ALIGN byte length.  The first byte of
 * the starting data is located at ``element.keystr[element.keylen]''.
 */
struct hash {
    /* NOTE: succ must be the first element */
    struct hash *succ; 	/* pointer to next hash entry, or NULL */
    short keylen;	/* length of key string + '\0' + word boundary pad */
    short datalen;	/* length in bytes of the data beyond the key string */
    char keystr[1];	/* padded key string, optionally followed by data */
};

/* hash_align aligns objects on optimal addresses */
#define hash_align(val) (((int)(val)+(BYTES_PER_ALIGN-1))&~(BYTES_PER_ALIGN-1))
/* correctly padded length of the key string - given the key string */
#define keystr_len(keystring) \
  hash_align(strlen((char *)(keystring))+1)
/* hash slot size in bytes - given lengths of the padded key string and data */
#define hashslot_size(keystrlen,datalen) \
 (hash_align(OFFSET(hash, keystr[0]) + \
	     hash_align((int)(keystrlen)+1) + \
	     hash_align((int)(datalen))))
/* hash slot length in bytes - given a ``struct hash'' element pointer */
#define hash_len(cur) \
 (OFFSET(hash, keystr[0]) + \
  (int)(((struct hash *)(cur))->keylen) + \
  hash_align((((struct hash *)(cur))->datalen)))

/* pointer to hash data - given a ``struct hash'' element pointer */
#define hash_data(cur) \
 ((((struct hash *)(cur))->datalen > 0) ? \
  ((char *)(((struct hash *)(cur))->keystr+((struct hash *)(cur))->keylen)) :\
  ((char *)NULL))

/*
 * stringcmp - compare two strings
 *
 * Some hash tables compare strings without regard to case while
 * others treat case as significant.  Returns <0, ==0, >0 if str1
 * is less than, equal to, or greater than str2.
 *
 * args:
 *	str1	- char *
 *		  first string to compare
 *	str2	- char *
 *		  second string to compare
 *	strcase	- int
 *		  case == 0 ==> use strcmp,  == 1 ==> use strcmpic
 */
#define stringcmp(str1, str2, strcase) \
    (strcase ? strcmpic(str1, str2) : strcmp(str1, str2))

/*
 * hash_string - hash string with or without regard to case
 *
 * args:
 *	str	- char *
 *		  the string to hash
 *	mod	- int
 *		  prime modulus used in hashing
 *	strcase	- int
 *		  == 0 ==> hash where a == A, == 1 ==> hash where a != A
 */
#define hash_string(str, mod, strcase) \
    (strcase ? hash_stric(str, mod) : hash_str(str, mod))

/*
 * insert_hash - insert an element before our current location in a slot chain
 *
 * Insert an element after an element (or hash slot head) in a chain.  We
 * pass `prev', a pointer to the `struct hash'-pointer that refers to 
 * our current chain location.  Our job is to have `prev' point to the
 * new element and our new element point to the current chain location.
 *
 * args:
 *	prev	- struct hash **
 *		  the entry before the place of insertion.  This pointer
 *		  may actually be the hash table slot pointer.
 *	item	- struct hash *
 *		  the item to insert
 */
#define insert_hash(prev, item) { \
    *(struct hash **)(item) = *((struct hash **)(prev)); \
    *(struct hash **)(prev) = (struct hash *)(item); \
}

/*
 * delete_hash - delete an element in the hash chain
 *
 * Given two ``struct hash'' elements `prev' and `item', delete_hash() will
 * remove `item' from the hash slot chain.
 *
 * input:
 *	prev	- struct hash **
 *		  pointer to the previous chain's forward pointer.  This
 *		  pointer may actually be the hash hash table slot pointer.
 *		  We will delete the element to which this pointer points.
 *	cur	- struct hash *
 *		  the `next' pointer of the item to delete
 */

#define delete_hash(prev, cur) { \
  *(struct hash **)(prev) = ((struct hash *)(cur))->succ; \
}

/*
 * replace_hash - replace an element in the hash chain
 *
 * Replace the element referred by `cur' and pointer at by `prev' with the
 * entry `item'
 *
 * input:
 *	prev	- struct hash *
 *		  the previous chain's forward pointer.  This pointer may
 *		  actually be the hash table slot pointer.  We will replace
 *		  the element to which this pointer points at.
 *	cur	- struct hash *
 *		  the element being replaced (i.e., deleted)
 *	item	- struct hash *
 *		  the element which is replacing `cur'
 */
#define replace_hash(prev, cur, item) { \
    ((struct hash *)(item))->succ = ((struct hash *)(cur))->succ; \
    (prev) = (struct hash *)(item); \
}

/*
 * hash_addr - return memory address of a 'succ' value
 *
 * If 'succ' is an array pointer form, hash_addr() will convert it to
 * a memory address, otherwise the queue pointer is returned.
 *
 * input:
 *	aqval	- struct hash *
 *		  the array or queue value tp be converted to a pointer
 *	table	- struct hash_table *
 *		  the hash table holding `cur'
 * output:
 *	a pointer to the object reference by succ
 */
#define hash_addr(aqval, table) \
  (is_odd((struct hash *)(aqval)) ? \
    (struct hash *)((char *)(table) + to_even((struct hash *)(aqval))) :\
    (struct hash *)(aqval))

/*
 * next_hash - return the next element in a hash slot chain
 *
 * returns NULL if the next element was beyond the end of the chain
 *
 * input:
 *	cur	- struct hash *
 *		  our current location
 *	table	- struct hash_table *
 *		  the hash table holding `cur'
 * output:
 *	a pointer to the next element, or NULL if no next element
 */
#define next_hash(cur, table) hash_addr(cur->succ, table)
