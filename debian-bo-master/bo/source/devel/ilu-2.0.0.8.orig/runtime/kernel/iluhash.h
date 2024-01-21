/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: iluhash.h,v 1.17 1996/05/15 00:02:27 janssen Exp $ */
/* Last tweaked by Mike Spreitzer February 9, 1995 12:32 pm PST */

#ifndef _ILUHASH
#define _ILUHASH

#include <iluxport.h>

/*L2, Main unconstrained*/

typedef struct hashTable *HashTable;

typedef struct {
  /*L1 >= {some mutex that protects the table}*/

  HashTable hn_ht;
  ilu_cardinal hn_index, hn_count;
} HashEnumerator;

/*L1 >= {some mutex that protects the table}*/

typedef 
ilu_cardinal(*ilu_hashfnptr) (ilu_refany /* key */ ,
			      ilu_cardinal /* modulus */ );

typedef 
ilu_boolean(*ilu_compfnptr) (ilu_refany /* key1 */ ,
			     ilu_refany /* key2 */ );

/*L1, L2, Main unconstrained*/

ilu_boolean     _ilu_hash_PointerCompare(ilu_refany, ilu_refany);
ilu_cardinal    _ilu_hash_HashPointer(ilu_refany, ilu_cardinal);

ilu_boolean     _ilu_hash_StringCompare(ilu_refany, ilu_refany);
ilu_cardinal    _ilu_hash_HashString(ilu_refany, ilu_cardinal);

HashTable _ilu_hash_MakeNewTable (ilu_cardinal size,
        ilu_hashfnptr hashfn, ilu_compfnptr compfn);

/*L1 >= {some mutex that protects the table}*/

ilu_refany _ilu_hash_FindInTable (HashTable ht, ilu_refany key);
/* Returns ILU_NIL if ht==ILU_NIL or key not found. */

ilu_boolean _ilu_hash_AddToTable (HashTable ht, ilu_refany key,
						ilu_refany obj);
/* If ht==ILU_NIL or key already in table, returns ilu_FALSE;
   otherwise adds <key, obj> to table and returns ilu_TRUE. */

ilu_refany _ilu_hash_RemoveFromTable (HashTable ht, ilu_refany key);
/* If ht==ILU_NIL or key not in table, returns ILU_NIL;
   otherwise removes key & returns its formerly associated datum. */

void _ilu_hash_FreeHashTable (HashTable ht, void (*freeKey)(ilu_refany),
					    void (*freeData)(ilu_refany));

ilu_cardinal _ilu_hash_PairsInTable (HashTable ht);

void 
_ilu_hash_TableEnumerate(HashTable ht,
			 void (*proc) (ilu_refany entry_data,
				       ilu_refany rock),
			 ilu_refany rock);

ilu_refany 
_ilu_hash_FindViaProc(HashTable ht,
		      ilu_boolean(*proc) (ilu_refany entry_data,
					  ilu_refany rock),
		      ilu_refany rock);

void _ilu_hash_BeginEnumeration (HashTable ht, HashEnumerator *he);
/* Initialize an enumeration state.  "he" must point to a HashEnumerator struct. */

ilu_boolean _ilu_hash_Next(HashEnumerator *he, ilu_refany *key,
					       ilu_refany *data);
/* If there's another pair to enumerate, stores the next pair to be enumerated in *key and *data, and returns TRUE; otherwise returns FALSE.  Unless the protecting mutex is held throughout the enumeration, pairs added or removed during the enumeration may or may not be enumerated, and if any pairs are removed, other pairs may be enumerated more than once.  A pair is in the table when it is enumerated. */


#endif
