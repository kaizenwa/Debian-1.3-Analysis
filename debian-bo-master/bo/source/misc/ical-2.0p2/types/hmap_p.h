/* Copyright (c) 1995 by Sanjay Ghemawat */
// This file is designed to be included multiple times and therefore
// no header guards should be placed on it.

// "Template" for generating various kinds of hash maps.
//
// See "hmap.h" for macros that should be defined before including this.

// Check that all of the macros are defined
#ifndef HMAP
#error macro HMAP not defined for hash table instantiation.
#endif
#ifndef HKEY
#error macro HKEY not defined for hash table instantiation.
#endif
#ifndef HVAL
#error macro HVAL not defined for hash table instantiation.
#endif
#ifndef HCONTROL
#error macro HCONTROL not defined for hash table instantiation.
#endif

// Do not need to use user-specified control here
#undef HCONTROL

#define HMAP_pair name2(HMAP,_pair)
#define HMAP_rep  name2(HMAP,_rep)

// Implement the underlying hash table
#define HTABLE   HMAP_rep
#define HTYPE    HMAP_pair
#define HCONTROL HMAP_pair
#include "htable_p.h"

// Remove the controlling macros now
#undef HMAP
#undef HKEY
#undef HVAL
#undef HMAP_pair
#undef HMAP_rep
