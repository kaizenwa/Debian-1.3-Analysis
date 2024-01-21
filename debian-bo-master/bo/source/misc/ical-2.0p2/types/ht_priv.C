/* Copyright (c) 1995 by Sanjay Ghemawat */
#include "ht_priv.h"

int htable_hash2(int v) {
    // Table for secondary hash table.
    //
    // There are two important considerations for this table:
    //
    // 1. The entries are prime numbers so that the search process
    //    of incrementing the index by the secondary hash value module
    //    a power of 2 will end up scanning the entire table.
    // 2. The size of the table is a prime number to differentiate it
    //    from the primary hash function, which is used module a power of 2.

    static const int prime_count = 13;
    static const int primes[prime_count] = {
	3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43
    };

    return (primes[v % prime_count]);
}
