/* Copyright (c) 1995 by Sanjay Ghemawat */
#ifndef _HT_PRIV_H
#define _HT_PRIV_H

// Private routines for hash table code

// effects  Return secondary hash value for "v"
extern int htable_hash2(int v);

// Constants to fix up the hash value to the right range
static const int htable_precbits = (sizeof(int) << 3) - 2;
static const int htable_precmask = (1 << htable_precbits) - 1;

// effects  Convert an integer into a primary hash value
inline int htable_int_hash(int value) {
    // The multiplier used below is "int(PHI * 2^30)" where PHI denotes
    // the golden ratio.  This multiplier value is suggested as a good
    // one by Knuth.  We multiply by "2^30" instead of either "2^31" or
    // "2^32" because "2^30" is the largest possible power of two
    // representable in twos-complement arithmetic with 32 bits.
    //
    // The resulting hash value should be converted into the range
    // "[0..2^k-1]" by extracting the top "k" bits of the bottom 30 bits
    // of the hash value.  An easy way to do this is to right shift the
    // result by "30 - k".  The routine "htable_fix_hash" given below
    // does exactly that.

    return (int) ((((unsigned int) value) * 1737350766) & htable_precmask);
}

inline int htable_fix_hash(int value, int n) {
    return (value >> (htable_precbits - n));
}

// Default maximum occupancy of hash table in percent.
// If the table becomes more than this full, it is enlarged.
static const int htable_default_max_occupancy = 80;

// Divisor to obtain minimum occupancy from maximum occupancy.
// We set this to four so that the table does not keep bouncing
// back and forth between two contiguous sizes as a small number
// of elements are inserted and removed.
static const int htable_min_occupancy_divisor = 4;

// Collision count stats are only kept if assertion checking is on.
// All code that is stats related should be enclosed in "HTABLE_STAT"
// as follows:
//		HTABLE_STAT(collissions++);
#ifdef NDEBUG
#define HTABLE_STAT(x) 0
#else
#define HTABLE_STAT(x) x
#endif

#endif /* _HT_PRIV_H */
