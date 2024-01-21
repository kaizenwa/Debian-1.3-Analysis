/* Copyright (c) 1995 by Sanjay Ghemawat */
// This file is designed to be included multiple times and therefore
// no header guards should be placed on it.

// "Template" for generating various kinds of hash tables.
//
// This file should be included after defining the following macros
//
//	HTABLE		Table class name
//	HTYPE		Element type
//	HCONTROL	Controlling class name.  Has following signature.
//
//	class HCONTROL {
//	  public:
//	    static int   is_full(HTYPE);
//	    static int   is_empty(HTYPE);
//	    static int   is_obsolete(HTYPE);
//	    static void  make_empty(HTYPE&);
//	    static void  make_obsolete(HTYPE&);
//	    static int   hash(HTYPE);
//	    static int   equal(HTYPE, HTYPE);
//	};

// Check that all of the macros are defined
#ifndef HTABLE
#error macro HTABLE not defined for hash table instantiation.
#endif
#ifndef HTYPE
#error macro HTYPE not defined for hash table instantiation.
#endif
#ifndef HCONTROL
#error macro HCONTROL not defined for hash table instantiation.
#endif

// The cost-estimates for different operations are based on the
// assumption that the hash function is "good".

class HTABLE {
  public:
    HTABLE();
    HTABLE(HTABLE const&);
    ~HTABLE();

    HTABLE& operator = (HTABLE const&);

    int size() const;
    // effects	Returns number of entries in the table.
    //		This is an O(1) operation.

    int contains(HTYPE x) const;
    // effects	Returns true iff the table contains the specified element.
    //		This is an O(1) operation.

    int find(HTYPE key, HTYPE& result) const;
    // modifies	"result"
    // effects	If an element matching "key" exists in the table,
    //		store the table element into "result" and return true.
    //		Else do not modify "result" and return false.
    //		This is an O(1) operation.

    int insert(HTYPE x);
    // modifies	"this"
    // effects	If table contains an element matching "x", replace that
    //		element with "x" and return false.  Else insert "x" into
    //		table and return true.
    //		This operation has amortized cost O(log n).
    //		Individual operations may cost upto O(n).
    //		If the size has been predicted correctly, the cost is O(1).

    int remove(HTYPE x);
    // modifies	"this"
    // effects	If table contains an element matching "x", remove that
    //		element and return true.  Else return false.
    //		This is an O(1) operation.

    void clear();
    // modifies	"this"
    // effects	Removes all elements from "this".
    //		This is an O(n) operation.

    void check() const;
    // effects	Nothing.
    //		Checks the table for internal consistency.  Dies on error.

    void report_stats(char const* msg) const;
    // effects	Reports collected table statistics prefixed with "msg".
    //		Report is sent to stderr.

    void predict(int n);
    // effects	Resizes table to an appropriate size to hold "n" entries.
    //		This is an O(n) operation.

    void set_occupancy(int x);
    // effects	Set occupancy target to "x" percent.
    //		Values for "x" that are too small or too big to be
    //		beneficial are silently ignored.
    //		This is an O(n) operation.

    class Elements {
      public:
	Elements(HTABLE const* table);
	// requires  "table" is not modified while this generator is active.
	// effects   Generate each table element exactly once.
	//	     Elements may be generated in arbitrary order.

	void operator = (HTABLE const* table);
	// modifies  "this"
	// effects   Initializes "this" to generate elements from "table".

	int get(HTYPE& x);
	// modifies  "this", "x"
	// effects    If more elements can be generated from the table,
	//	      store the next element in "x" and return true.
	//	      Else return false.

	void del();
	// requires   Iterator has just yielded something
	// effects    Remove the last yielded element
      private:
	HTABLE const*	table;
	int		index;
	void		skip_empty();
    };

  private:
    friend Elements;

    // The table representation is an array of elements.
    // An element can have two special states: empty and obsolete.
    // An empty element indicates that the corresponding array slot
    // is empty.  An obsolete element indicates that the array slot
    // is empty, but used to contain a non-empty value.

    HTYPE*	table;		// The actual table of entries
    int		tsize;		// Size of the table (power of two)
    int		enlarge_size;	// Resize when count bigger or equal
    int		shrink_size;	// Resize when count bigger or equal
    int		count;		// Total # of non-empty/non-deleted entries
    int		delcount;	// Number of deleted entries
    int		accesses;	// Number of searches in table
    int		collisions;	// Number of collisions
    int		max_occupancy;	// Percent maximum occupancy in table

    // The following are maintained for fast arithmetic modulo "tsize"
    int		mask;		// mask == tsize-1
    int		slotBits;	// lg(tsize)

    void init(int s);
    // effects	ignores old state and sets new empty state to hold at
    //		least "s" elements.

    void resize(int n);
    // effects	Resizes the table to an appropriate size to hold "n" entries.

    int find_index(HTYPE elem) const;
    // effects	Find an index for "elem" and returns it.  If element is
    //		already in the table, returns its index.  Else returns
    //		an index for an empty slot.

    int table_size(int n) const;
    // effects	Return appropriate table size for "n" entries.
};

inline int HTABLE::size() const {
    return count;
}

inline int HTABLE::contains(HTYPE x) const {
    int index = find_index(x);
    return (HCONTROL::is_full(table[index]));
}

inline int HTABLE::find(HTYPE x, HTYPE& result) const {
    int index = find_index(x);
    if (HCONTROL::is_full(table[index])) {
	result = table[index];
	return 1;
    }
    else
	return 0;
}

inline void HTABLE::predict(int n) {
    resize(n);
}

inline HTABLE::Elements::Elements(HTABLE const* t) {
    table = t;
    index = 0;
}

inline void HTABLE::Elements::operator = (HTABLE const* t) {
    table = t;
    index = 0;
}

inline int HTABLE::Elements::get(HTYPE& x) {
    skip_empty();
    if (index < table->tsize) {
	x = table->table[index];
	index++;
	return 1;
    }
    else
	return 0;
}

// Remove the controlling macros now
#undef HTABLE
#undef HTYPE
#undef HCONTROL
