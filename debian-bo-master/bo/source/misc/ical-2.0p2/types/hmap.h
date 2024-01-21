/* Copyright (c) 1995 by Sanjay Ghemawat */
// This file is designed to be included multiple times and therefore
// no header guards should be placed on it.

// "Template" for generating various kinds of hash maps.
//
// This file should be included after defining the following macros
//
//	HMAP		Map class name
//	HKEY		Key type
//	HVAL		Value type
//	HCONTROL	Controlling class for keys.  Should have same
//			signature as the controlling class for hash tables
//			of HKEY.  (See htable.h)

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

#include <generic.h>
#define HMAP_pair name2(HMAP,_pair)
#define HMAP_rep  name2(HMAP,_rep)

// Internal struct
struct HMAP_pair {
    HKEY key;
    HVAL val;

    // Control ops go right here
    static int   is_full(HMAP_pair const&);
    static int   is_empty(HMAP_pair const&);
    static int   is_obsolete(HMAP_pair const&);
    static void  make_empty(HMAP_pair&);
    static void  make_obsolete(HMAP_pair&);
    static int   hash(HMAP_pair const&);
    static int   equal(HMAP_pair const&, HMAP_pair const&);
};

inline int HMAP_pair::is_full(HMAP_pair const& p) {
    return HCONTROL::is_full(p.key);
}

inline int HMAP_pair::is_empty(HMAP_pair const& p) {
    return HCONTROL::is_empty(p.key);
}

inline int HMAP_pair::is_obsolete(HMAP_pair const& p) {
    return HCONTROL::is_obsolete(p.key);
}

inline void HMAP_pair::make_empty(HMAP_pair& p) {
    HCONTROL::make_empty(p.key);
}

inline void HMAP_pair::make_obsolete(HMAP_pair& p) {
    HCONTROL::make_obsolete(p.key);
}

inline int HMAP_pair::hash(HMAP_pair const& p) {
    return HCONTROL::hash(p.key);
}

inline int HMAP_pair::equal(HMAP_pair const& a, HMAP_pair const& b) {
    return HCONTROL::equal(a.key, b.key);
}

// No need to use user-specified control routines any more
#undef HCONTROL

// Table of pairs
#define HTABLE   HMAP_rep
#define HTYPE    HMAP_pair
#define HCONTROL HMAP_pair
#include "htable.h"

class HMAP {
  public:
    // Default constructors, destructors, and assignment operators are okay
    // because the rep does not have direct pointers.

    int size() const;
    // effects	Returns number of mappings in the table.

    int contains(HKEY k) const;
    // effects	Returns true iff the table contains a mapping for "k"

    int find(HKEY key, HVAL& result) const;
    // modifies	"result"
    // effects	If a mapping for "key" exists in the table,
    //		store the corresponding value into "result"
    //		and return true.
    //		Else do not modify "result" and return false.

    int insert(HKEY k, HVAL v);
    // modifies	"this"
    // effects	If table contains a mapping for "k", replace that
    //		mapping with "k, v" and return false.  Else insert
    //		"k, v" table and return true.

    int remove(HKEY k);
    // modifies	"this"
    // effects	If table contains a mapping for "k", remove that
    //		mapping and return true.  Else return false.

    void clear();
    // modifies	"this"
    // effects	Removes all mappings from "this".

    void check() const;
    // effects	Nothing.
    //		Checks the table for internal consistency.  Dies on error.

    void report_stats(char const* msg) const;
    // effects	Reports collected table statistics prefixed with "msg".
    //		Report is sent to stderr.

    void predict(int n);
    // effects	Resizes table to an appropriate size to hold "n" mappings.

    void set_occupancy(int x);
    // effects	Set occupancy target to "x" percent.
    //		Values for "x" that are too small or too big to be
    //		beneficial are silently ignored.
    //		This is an O(n) operation.

    class Bindings {
      public:
	Bindings(HMAP const* table);
	// requires  "table" is not modified while this generator is active.
	// effects   Generate each table element exactly once.
	//	     Elements may be generated in arbitrary order.

	void operator = (HMAP const* table);
	// modifies  "this"
	// effects   Initializes "this" to generate elements from "table".

	int get(HKEY& k, HVAL& v);
	// modifies  "this", "k", "v"
	// effects    If more mappings can be generated from the table,
	//	      store the key of the next mapping in "k", its value
	//	      in "v", and return true.  Else return false.

	void del();
	// requires   Iterator has just yielded something
	// effects    Removes the last mapping yielded by iterator
      private:
	HMAP_rep::Elements rep;
    };
  private:
    friend Bindings;
    HMAP_rep rep;
};

inline int HMAP::size() const {
    return rep.size();
}

inline int HMAP::contains(HKEY k) const {
    HMAP_pair p;
    p.key = k;
    return rep.contains(p);
}

inline int HMAP::find(HKEY k, HVAL& v) const {
    HMAP_pair p1, p2;
    p1.key = k;
    if (rep.find(p1, p2)) {
	v = p2.val;
	return 1;
    }
    return 0;
}

inline int HMAP::insert(HKEY k, HVAL v) {
    HMAP_pair p;
    p.key = k;
    p.val = v;
    return rep.insert(p);
}

inline int HMAP::remove(HKEY k) {
    HMAP_pair p;
    p.key = k;
    return rep.remove(p);
}

inline void HMAP::clear() {
    rep.clear();
}

inline void HMAP::check() const {
    rep.check();
}

inline void HMAP::report_stats(char const* msg) const {
    rep.report_stats(msg);
}

inline void HMAP::predict(int n) {
    rep.predict(n);
}

inline void HMAP::set_occupancy(int n) {
    rep.set_occupancy(n);
}

inline HMAP::Bindings::Bindings(HMAP const* table) : rep(&table->rep) {
}

inline void HMAP::Bindings::operator = (HMAP const* table) {
    rep = &table->rep;
}

inline int HMAP::Bindings::get(HKEY& k, HVAL& v) {
    HMAP_pair p;
    if (rep.get(p)) {
	k = p.key;
	v = p.val;
	return 1;
    }
    return 0;
}

inline void HMAP::Bindings::del() {
    rep.del();
}

// Remove the controlling macros now
#undef HMAP
#undef HKEY
#undef HVAL
#undef HMAP_pair
#undef HMAP_rep
