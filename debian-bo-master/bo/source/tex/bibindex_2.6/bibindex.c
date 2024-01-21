/* ================================================================= *\

   bibindex -- a program to index bibtex files, used in conjunction
	       with biblook

   This program was specifically developed for use with the
   computational geometry bibliographic database, and assumes the
   rules thereof.  The database may be obtained by anonymous ftp
   from cs.usask.ca in the file "pub/geometry/geombib.tar.Z".

   Version 1.0 written by Jeff Erickson <jeff@ics.uci.edu>, 27 Mar 92
   Version 2.0 written by Jeff Erickson <jeff@ics.uci.edu>, 17 Jun 92

   This program is in the public domain.  You may use it or modify
   it to your heart's content, at your own risk.  Bouquets, brickbats,
   and bug fixes may be sent to Jeff Erickson, jeffe@cs.berkeley.edu.

   %Make% gcc -O -o bibindex bibindex.c

   Usage: bibindex bibfile [-i field ...]

   -----------------------------------------------------------------
   HOW IT WORKS:

   The bibtex file is read field by field.  The file offset beginning
   each record and each record's citation key are recorded.  A list of
   words is extracted from each field.  These words are placed into
   tables, which remember which records contain them in their
   respective fields.  Once the file has been completely read, the
   hash tables are compacted and sorted.

   The hash tables are extensible, since we have to maintain one for
   each possible field type, and static tables would be way too big.
   Initially, each table holds 1K entries, and the tables are doubled
   whenever they reach 75% capacity.  Each table entry is at least 24
   bytes.  If the resulting hash tables use too much memory, the
   entries should be changed to pointers, allocated on the fly.

   The entry lists associated with each word are implemented as
   extensible arrays.  Initially, each list holds eight entries.  If a
   new entry is inserted into a full list, the list is doubled first.

   The index file has the following format (loosely):

	version info
	# entries
	array of offsets into bib file	-- one per entry
	# field types (incl. "@string")
	array of field names		-- one per field type
	array of			-- one per field type
	    # words
	    array of			-- one per word
		word			-- in alphabetical order
		# locations
		array of entry #s	-- one per location [compressed]
	# abbreviations
	array of abbreviations		-- in alphabetical order
	array of offsets into bib file	-- one per abbreviation

   There are advantages and disadvantages of having multiple hash
   tables instead of a single table.  I am starting with the premise
   that the lookup program should be very fast.  Consequently, I can't
   make it determine which fields contain a given word.  Doing so
   would require putting ALL of the entry-parsing code into the lookup
   program.  It would also mean potentially parsing a lot of
   extraneous entries to find relatively common words in relatively
   uncommon places (eg, "title edelsbrunner").

   If there were a single word table, each word list would have to
   include bitmasks to indicate the appropriate fields along with the
   entry numbers.  Assuming between 16 and 32 field types (the CG bib
   uses about 24), this would triple the size of each entry.  On the
   average, each word occurs in less than two field types.  The
   bitmask approach would also require knowledge of the field names in
   advance; the multiple table approach does not.

   -----------------------------------------------------------------
   VERSION HISTORY:

   1.0 <jge> 3/26/92	Initial version completed
   1.1 <jge> 3/27/92	Words restricted to letters only; special
			rules added for apostrophes, so that words
			like "didn't" and "O'Rourke" can be handled
			correctly.
   1.2 <jge> 3/30/92	Faster hash function; now compressing hash
			tables before sorting them.  Execution time on
			the CG bib reduced by a factor of thirty-five.
   1.3 <jge> 4/2/92	Toyed with the hash function some more, trying
			to reduce collisions, with limited success.
   1.4 <jge> 4/17/92	Added exit(0) at the end of main()  [I thought
			that was supposed to be automatic!]

   2.0 <jge> 6/12/92	First major revision completed.
	1. Major change in file format -- word tables for every
	   field instead of just author, title, and keywords.
	2. Word hash tables made extensible.
	3. Fixed bug in GetNextWord that would cause the function
	   to return inside nested braces.
	4. Fixed bug in MungeField that would kill everything in an
	   entry after an abbreviation.  Abbreviations now go into
	   the appropriate hash table with the other words.
	5. Made GetNextWord consider numbers and math expressions
	   to be words.
	6. Added WriteWord, resulting in 40% savings in disk space.
	7. Added -i flag and black holes.  Ignoring "oldlabel"
	   gives another 15% savings (6/92 version of CGbib).
   2.1 <jge> 7/9/92	Minor bug fixes.
   2.2 Nelson H. F. Beebe <beebe@math.utah.edu> 03-Oct-1992
	Testing with >100K lines of .bib files led to these changes:
	1. Add support for complete BibTeX keyword name syntax with
	   iskeychar() function.
	2. Add support for trailing comma after last key = "value" entry.
	3. Record input line number for display in error messages.
	4. Record initial line number of each BibTeX entry for
	   display in error messages to better localize error.
	5. Add test for word buffer overflow in MungeField() to prevent
	   run-time core dumps, and increase supported word size from
	   15 to 31 (a word-length histogram of a 116921-word dictionary
	   found words up to 28 characters long, with 1% longer than 15).
	   File version increased to 2 because of word size change.
	6. Add typecasts in calls to qsort() and comparisons of
	   unsigned short with short, change main() from void to int,
	   remove variable from initializer of msg[2], and add void to
	   IndexBibFile() definition to allow compilation with C++ as
	   well as C for better compile-time checking.
	7. In MungeEntry(), do an ungetc() after key name collection.
	   Otherwise, a key="value" pair without a blank before the =
	   will not be recognized because the = read on lookahead has
	   not been put back.
	8. Revise table compression code in OutputTables() and
	   code in FreeTables() to avoid duplicate frees, which is
	   a fatal error on many systems, and specified to result
	   in undefined behavior by the 1989 ANSI/ISO C Standard.
	9. Define bcopy() as a macro to invoke standard memcpy()
	   instead.
       10. Include time.h, unistd.h, and malloc.h to get proper
	   function declarations for library routines needed here.
       11. Add DEBUG_MALLOC support.
       12. Change several char* types to const char*.
       13. Correct some typographical errors in comment.
       14. Write UNIX manual pages for bibindex and biblook.
       15. Allow command-line to specify a filename with .bib extension.
       16. Add help support to biblook.
       17. Correct error in FreeTables() in biblook.c; inner loop
	   incremented i instead of j.
   2.3 Bill Jones <jones@cs.usask.ca> 93/01/29
	1. Declarations common to bibindex.c and biblook.c factored out
	   to new file biblook.h.
	2. Index type of (signed) short overflows early; created typedef
	   Index_t, defined as unsigned short.
	3. Changed hash tables to extend at 75% capacity rather than 50%.
   2.4 Nelson H. F. Beebe <beebe@math.utah.edu> [01-Jun-1993]
	1. Remove some mixed-mode arithmetic.
	2. Increase MAXFIELDS from 64 to 127 to deal with TeX User Group
	   bibliography collection
	3. Correct error in GetHashTable(); older versions got into an
	   infinite loop if MAXFIELDS field names were already stored, and
	   a new one was encountered.
   2.5 Erik Schoenfelder <schoenfr@ibr.cs.tu-bs.de> [14-Aug-1993]
	1. Add support for network byte order I/O, so that index files
	   can be shared between big-endian and little-endian systems.
	   This option is selected when HAVE_NETINET_IN_H is defined
	   at compile time (default on UNIX).
       Nelson H. F. Beebe <beebe@math.utah.edu> [14-Aug-1993]
	2. Add typecast (int) in OutputTables() array reference to eliminate
	   compiler warnings.
	3. Correct code in biblook:SetComplement() to check for zero
	   setmask; otherwise, a .bib file with a number of entries
	   which is an exact multiple of setsize (commonly 32) will
	   result in failing searches for the last setsize entries!
   2.6 Jeff Erickson <jeffe@cs.berkeley.edu> 8/19/93
	1. Made MungeWord() use iskeychar() instead of referring to
	   NONABBREV directly, and made iskeychar() use NONABBREV (now
	   called NONKEYCHARS).  No reason to spell it out twice!
	2. Correctly distinguish between abbreviations and numbers
	   not in quotes.
	3. 75% calculation for hash table capacity uses unsigned longs
	   instead of Index_t's, which might overflow if Index_t is
	   unsigned short.
	4. Added support for abbreviations (@strings).  Abbrevs are
	   stored in special "field" table.  HashCell data structure
	   modified to keep words in the expansion of any abbrev, along
	   with the offset at which the abbrev is defined.  Whenever an
	   abbrev is encountered, both the abbrev and its expansion are
	   indexed.  Warnings for multiple definitions of the same
	   abbrev or use of an undefined abbrev.  Automatic inclusion
	   of standard abbrevs.
	5. Abbrevs stored in index file to support biblook "whatis"
	   command.  File version changed to 3.
	6. Support for # concatenation added to MungeField.
	7. Brackets ignored in GetNextWord, so optional abbreviations
	   like "J[ohn]" are handled correctly.
	8. Elements of compound words, like "lower-bound" and "Fejes
	   Toth", are indexed both together and separately.
	9. Added support for BIBINDEXFLAGS environment variable, which
	   stores default set of command-line arguments
       10. No longer barfs on @comment and @preamble entries.
       11. Changed format of screen output.
       Nelson H. F. Beebe <beebe@math.utah.edu> [12-Sep-1993]
       12. Add some code fixes so compilation under C++ works again.
       13. Restore extended list of BADWORDS (defined in biblook.h).
	   Profiling showed that the sequential search through the
	   extended badwords[] table in IsRealWord() is a major
	   bottleneck with large input files.  Therefore, replace slow
	   sequential search by fast hash lookup, with new functions
	   InHashCell() and StandardBadWords(), and new initialization
	   code in InitTables(), FreeTables(), and GetHashCell(),
       14. Fix bug in GetNextWord() that produced string overflow in
	   word, and corrupt .bix file.  Add corresponding sanity
	   check in WriteWord().
       15. Increase MAXWORD from 31 to 255 to better handle compound word
	   support added with item 8 above.
       16. Fix long-standing bug in GetHashTable() that could produce
	   an infinite loop.  Add similar cautionary code in GetHashCell().
       17. Apply some spelling corrections to strings and comments.
       18. Prevent zero divide in statistics output in OutputTables().
       19. Use FILENAME_MAX+1 (defined in biblook.h) for size of filename
	   strings.
       20. Change INIT_HASH_SIZE to a prime number, and add comment that
	   MAXFIELDS must also be a prime.  Add next_prime() function
	   for calculating a prime number for hash table sizes.  When
	   the table size is not a prime, hashing performance can
	   degrade seriously, and secondary hashing will not probe the
	   full table (see Donald E. Knuth, The Art of Computer
	   Programming, Volume 3: Sorting and Searching,
	   Addison-Wesley, 1973).
       21. Change type of numfields from char to unsigned char, and
	   use sizeof(numfields) instead of sizeof(type of numfields).
       22. Change all exit() calls to use Standard C EXIT_xxx symbols.
       23. Put (void) casts on printf() family calls to avoid compiler
	   warnings about discarded returned values.
       24. Change type of loop index variables from "short" to "int",
	   since shorts carry a significant penalty on modern
	   RISC architectures.
       25. Change safemalloc() to request non-zero size when zero size
	   is requested; some systems reject a malloc(0) request (e.g.
	   IBM RS/6000 AIX 3.2).

\* ================================================================= */

#include "biblook.h"

#if vms
/* I get NULL pointer dereferencing in CompareCells from VMS qsort, so
here is a replacement borrowed from the Utah DVI driver development
package. */
#define qsort(b,n,s,c)	shsort((char*)(b),(n),(s),(c))
void			shsort(char *base_, size_t nmemb_, size_t size_,
			       int (*compar) (const void*, const void*));
#endif /* vms */

#define MIN_OFFSET_LIST_SIZE	128

#if DEBUG_MEMORY
#define MALLOC(x)	dbg_malloc(x)
#define FREE(x)		dbg_free(x)
#else
#define MALLOC(x)	malloc(x)
#define FREE(x)		free(x)
#endif /* DEBUG_MEMORY */

#if defined(USE_MEMIO)
/* We improve things even more by making safegetc() expand inline.
safegetc() will only result in a function call on a fatal error. */

static int safegetc_c_;
#define safegetc(fp_,what_) \
	(mfeof(fp_) ? (die("Unexpected end of file",what_), EOF) : \
	 (safegetc_c_ = mgetc(fp_), \
	  ((safegetc_c_ == '\n') ? line_number++ : 0), \
	  safegetc_c_))
#endif /* defined(USE_MEMIO) */

void * dbg_malloc(size_t n)
{
    void *p = malloc(n);

    (void)fprintf(stderr,"malloc:\t0x%08lx\t%8d\n",(unsigned long)p, (int)n);
    return (p);
}

void dbg_free(void *p)
{
    (void)fprintf(stderr,"free:\t0x%08lx\n",(unsigned long)p);
    free(p);
}

/* ======================= UTILITY FUNCTIONS ======================= */

static long line_number = 1L;		/* for debug messages */
static long initial_line_number = 1L;

/* ----------------------------------------------------------------- *\
|  void die(const char *msg1, const char *msg2)
|
|  Print an error message and die.
\* ----------------------------------------------------------------- */
void die(const char *msg1, const char *msg2)
{
    (void)fprintf(stderr, "\nError:\t in BibTeX entry starting at line %ld, ",
	    initial_line_number);
    (void)fprintf(stderr, "error detected at line %ld:\n",
	    line_number);
    (void)fprintf(stderr, "\t%s %s\n", msg1, msg2);
    exit(EXIT_FAILURE);
}

/* ----------------------------------------------------------------- *\
|  void diechar(const char *msg1, const char msg2)
|
|  Print an error message and die.  Called when second message is a
|  single character
\* ----------------------------------------------------------------- */
void diechar(const char *msg1, const char msg2)
{
    char tmp[2];
    tmp[0] = msg2;
    tmp[1] = 0;
    die(msg1, tmp);
}

/* ----------------------------------------------------------------- *\
|  void warn(const char *msg1, const char *msg2)
|
|  Print an error message and return.
\* ----------------------------------------------------------------- */
void warn(const char *msg1, const char *msg2)
{
    (void)fprintf(stderr, "\nWarning: %s %s (at line %ld)\n",
	    msg1, msg2, line_number);
}

/* ----------------------------------------------------------------- *\
|  char safegetc(FILE *fp, const char *what)
|
|  Get the next character safely.  Used by routines that assume that
|  they won't run into the end of file.
\* ----------------------------------------------------------------- */
char (safegetc)(FILE *fp, const char *what)
{
    char c;

    if (feof(fp))
	die("Unexpected end of file", what);

    c = getc(fp);
    if (c == '\n')
	line_number++;
    return (c);
}

/* ----------------------------------------------------------------- *\
|  void *safemalloc(size_t howmuch, const char *msg1, const char *msg2)
|
|  Allocate memory safely.  Used by routines that assume they won't
|  run out of memory.
\* ----------------------------------------------------------------- */
void *safemalloc(size_t howmuch, const char *msg1, const char *msg2)
{
    register void *tmp = NULL;

    tmp = (void*)MALLOC((howmuch > 0) ? howmuch : 1);
			/* some malloc's fail with zero size request */
    if (tmp == NULL)
	die(msg1, msg2);

    return tmp;
}


/* ====================== HASH TABLE FUNCTIONS ===================== *\

   The hash tables start small and double whenever they reach 75%
   capacity.  Hashing is performed by going through the string one
   character at a time, multiplying by a constant and adding in the
   new character value each time.  The constant is defined to give
   the same even spread (about size/sqrt(2)) between successive
   multiples, as long as the hash table size is a power of two.

   Collisions are resolved by double hashing.  Since the hash table
   size is always a power of two, the secondary hash value has to be
   odd to avoid loops.

   The field tables are non-extensible hash tables, otherwise handled
   the same way.  It is probably well worth the effort to fine tune
   the field table hash function in order to avoid collisions.

   The field tables associated with ignored fields are black holes.
   Everything is the same, except that InsertEntry doesn't actually
   DO anything.

\* ================================================================= */

#define MAXFIELDS	127	 	/* intentionally way too much */
					/* NB: limited by (possibly signed) */
					/* char type of numfields, and must */
					/* be a prime number (the next */
					/* larger prime will be used if it */
					/* is not already a prime) */
#define INIT_HASH_SIZE	257		/* prime number larger than expected */
					/* number of fields */
#define HASH_CONST   	1482907		/* prime close to 2^{20.5} */

typedef struct			/* Hash table entry for index/abbrev tables */
{
    Word 	theword;	/* the hashed word/abbreviation */
    Index_t 	number;		/* number of refs/words in the list */
    Index_t	size;		/* real size of reference/word list */

    /* --- Abbreviation table only --- */
    Word       *words;		/* list of words in expansion */
    Index_t	entry;		/* entry containing definition */

    /* --- Index tables only --- */
    Index_t    *refs;		/* actual list of references */
} HashCell, *HashPtr;

typedef struct			/* Extensible hash table */
{
    Word      thekey;		/* field name or "@string" */
    Index_t   number;		/* number of words in the table */
    Index_t   size;		/* real size of the table */
    HashPtr   words;		/* index hash table */
} ExHashTable;

static ExHashTable fieldtable[MAXFIELDS];	/* the field tables */
static unsigned char numfields;			/* number of fields */
static ExHashTable abbrevtable[1];		/* the abbrev table */
static ExHashTable badwordtable[1];		/* the badword table */


/* ----------------------------------------------------------------- *\
|  long next_prime(long n)
|
|  Return the next prime number after n.
\* ----------------------------------------------------------------- */

long next_prime(long n)
{
    long prime;			/* tentative prime */
    long factor;		/* prime factor */
    int is_prime;		/* 'prime' is a prime number */

    n = (n < 0L) ? -n : n;	/* be safe -- force n positive */
    prime = 2L*(n/2L) + 1L;	/* next odd number */
    is_prime = (prime <= 3L);
    while (!is_prime)
    {
	factor = 5L;
	is_prime = (prime % 2L) && (prime % 3L);
	while (is_prime && (factor*factor <= prime))
	{
	    if ((prime % factor) == 0L)
		is_prime = 0;
	    else if ((prime % (factor + 2L)) == 0L)
		is_prime = 0;
	    else	/* factor+4 is divisible by 3 (every 3rd odd is) */
		factor += 6L;
	}
	if (!is_prime)
		prime += 2L;
    }
    return (prime);
}

/* ----------------------------------------------------------------- *\
|  Word WordDup(const Word theword)
|
|  Return a pointer to a duplicate copy of theword (usually of
|  shorter length than MAXWORD)
\* ----------------------------------------------------------------- */
Word WordDup(const Word theword)
{
    Word newword;
    newword = (Word)safemalloc(strlen((const char*)theword) + 1,
			       "Can't allocate space for new word",
			       (const char*)theword);
    (void)strcpy((char*)newword,(const char*)theword);
    return (newword);
}

/* ----------------------------------------------------------------- *\
|  void InitOneField(ExHashTable *htable)
|
|  Initialize one field's hash table
\* ----------------------------------------------------------------- */
void InitOneField(register ExHashTable *htable)
{
    Index_t i;

    htable->number = 0;
    htable->size = (Index_t)next_prime((long)INIT_HASH_SIZE);

    htable->words = (HashPtr) safemalloc(htable->size*sizeof(HashCell),
					 "Can't create hash table for",
					 htable->thekey);
    for (i=0; i<INIT_HASH_SIZE; i++)
    {
	htable->words[i].theword = (Word)NULL;
	htable->words[i].number = 0;
	htable->words[i].size = 0;
	htable->words[i].refs = NULL;
	htable->words[i].entry = INDEX_NAN;
	htable->words[i].words = NULL;
    }
}

/* ----------------------------------------------------------------- *\
|  void InitTables(void)
|
|  Initialize the field tables
\* ----------------------------------------------------------------- */
void InitTables(VOID)
{
    register unsigned int i;

    numfields = 0;
    for (i=0; i<MAXFIELDS; i++)
    {
	fieldtable[i].thekey = (Word)NULL;
	fieldtable[i].number = 0;
	fieldtable[i].size = 0;
	fieldtable[i].words = NULL;
    }

    abbrevtable->thekey = WordDup("abbreviations");
    abbrevtable->number = 0;
    abbrevtable->size = 0;
    abbrevtable->words = NULL;

    InitOneField(abbrevtable);

    badwordtable->thekey = WordDup("bad words");
    badwordtable->number = 0;
    badwordtable->size = 0;
    badwordtable->words = NULL;

    InitOneField(badwordtable);
}

/* ----------------------------------------------------------------- *\
|  void FreeTables(void)
|
|  Free the tables
\* ----------------------------------------------------------------- */
void FreeTables(VOID)
{
    register int i;
    Index_t j;

    for (i=0; i < (int)numfields; i++)
    {
	if (fieldtable[i].words)
	{
	    for (j=0; j < fieldtable[i].number; j++)
	    {
		if (fieldtable[i].words[j].theword)
		    FREE(fieldtable[i].words[j].theword);
		if (fieldtable[i].words[j].refs)
		    FREE(fieldtable[i].words[j].refs);
	    }
	    FREE(fieldtable[i].words);
	}
    }

    if (abbrevtable->words)
    {
	for (j=0; j < abbrevtable->number; j++)
	    if (abbrevtable->words[j].words)
		FREE(abbrevtable->words[j].words);

	FREE(abbrevtable->words);
    }

    if (badwordtable->words)
    {
	for (j=0; j < badwordtable->number; j++)
	    if (badwordtable->words[j].words)
		FREE(badwordtable->words[j].words);

	FREE(badwordtable->words);
    }
}


/* ----------------------------------------------------------------- *\
|  ExHashTable *GetHashTable(const char *field)
|
|  Get the hash table associated with the given key -- field name or
|  "@string".  If the table is unclaimed, claim it and initialize it.
\* ----------------------------------------------------------------- */
ExHashTable *GetHashTable(const char *field)
{
    register unsigned long hash=0;	/* primary hash value	*/
    register unsigned long skip=1;	/* secondary hash value */
    register int i;

#if 0
    register unsigned long new_hash;	/* primary hash value	*/

    for (i=0; field[i]; i++)
    {
	hash = (hash*HASH_CONST + field[i]) % MAXFIELDS;
	skip += 2*hash;
    }

    while (fieldtable[hash].thekey &&		     /* cell not empty and  */
	   strcmp(fieldtable[hash].thekey, field))   /* not the right field */
    {
	new_hash = (hash+skip) % MAXFIELDS;
	/* Prior to version 2.6, this loop contained only one */
	/* statement.  However, an instance was found where hash == 117 */
	/* and skip == 1397, so (117 + 1397) % 127 -> 117 again, giving */
	/* an infinite loop. */
	hash = (new_hash == hash) ? (hash+1) % MAXFIELDS : new_hash;
    }
#else
    for (i=0; field[i]; i++)
    {
	hash = (hash ^ HASH_CONST + field[i]);
	skip += hash;
    }

    hash %= MAXFIELDS;
    skip %= MAXFIELDS;
    if (skip == 0)
	skip = 1;			/* need skip in 1..(MAXFIELDS-1) */

    while (fieldtable[hash].thekey &&		     /* cell not empty and  */
	   strcmp(fieldtable[hash].thekey, field))   /* not the right field */
    {
	hash += skip;		/* open addressing with double hashing */
	if (hash >= MAXFIELDS)
	    hash -= MAXFIELDS;		/* hash now in 0..MAXFIELDS */
    }
#endif
    if (!fieldtable[hash].thekey)
    {
	fieldtable[hash].thekey = WordDup((const Word)field);
	InitOneField(fieldtable+hash);
	numfields++;
	if (numfields >= MAXFIELDS)	/* NB: NOT > because that produces */
					/* an infinite while() loop above on */
					/* next entry! */
	    die("too many field names", field);
    }

    return fieldtable+hash;
}

/* ----------------------------------------------------------------- *\
|  void InitBlackHole(char *field)
|
|  Initialize a black hole for the given field
\* ----------------------------------------------------------------- */
void InitBlackHole(char *field)
{
    ExHashTable *hole;

    hole = GetHashTable(field);
    if (hole->words != NULL)
    {
	FREE(hole->words);
	hole->words = NULL;
    }
}

/* ----------------------------------------------------------------- *\
|  int IsBlackHole(ExHashTable *htable)
|
|  Is the given hash table a black hole?
\* ----------------------------------------------------------------- */
#define IsBlackHole(htable) ((htable)->words == NULL)


/* ----------------------------------------------------------------- *\
|  HashPtr GetHashCell(ExHashTable *table, const char *word)
|
|  Get the hash table cell associated with the given word/abbrev.
|  If the cell is unclaimed, claim it, initialize it, and update
|  the table's word count.
\* ----------------------------------------------------------------- */
HashPtr GetHashCell(ExHashTable *htable, const char *word)
{
    register HashPtr table, cell;
    register unsigned long hash=0;	/* primary hash value	*/
    register unsigned long skip=1;	/* secondary hash value */
    register int i;

#if 0
    register unsigned long new_hash;	/* primary hash value	*/

    table = htable->words;
    for (i=0; word[i]; i++)
    {
	hash = (hash*HASH_CONST + word[i]) % htable->size;
	skip += 2*hash;
    }

    while (table[hash].theword &&	/* cell not empty and */
	   strcmp((const char*)table[hash].theword, word))
					/* not the right word */
    {
	new_hash = (hash+skip) % htable->size;
	/* See remark in GetHashTable() above */
#if DEBUG
	if (hash == new_hash)
	    (void)printf(
		"hash = new_hash = %ld htable->size = %d field = [%s]\n",
		hash, htable->size, word);
#endif /* DEBUG */
	hash = (new_hash == hash) ? (hash+1) % htable->size : new_hash;
    }
#else
    table = htable->words;
    for (i=0; word[i]; i++)
    {
	hash = (hash ^ HASH_CONST + word[i]);
	skip += hash;
    }

    hash %= htable->size;
    skip %= htable->size;
    if (skip == 0)
	skip = 1;			/* need skip in 1..(htable->size-1) */

    while (table[hash].theword &&	/* cell not empty and */
	   strcmp((const char*)table[hash].theword, word))
					/* not the right word */
    {
	hash += skip;		/* open addressing with double hashing */
	if (hash >= htable->size)
	    hash -= htable->size;	/* hash now in 0..MAXFIELDS */
    }
#endif

    cell = table+hash;

    if (!cell->theword)			/* if cell isn't initialized yet... */
    {
	cell->theword = WordDup((const Word)word);
	cell->size = 8;

	if (htable == abbrevtable)
	{
	    cell->words = (Word *) safemalloc(cell->size * sizeof(Word),
					      "Can't store expansion for",
					      word);
	}
	else if (htable == badwordtable)
	{
	    cell->words = (Word *) safemalloc(cell->size * sizeof(Word),
					      "Can't store ignorable word",
					      word);
	}
	else
	{
	    cell->refs = (Index_t *) safemalloc(cell->size * sizeof(Index_t),
						"Can't create entry list for",
						word);
	}
	htable->number++;
    }
    return cell;
}
/* ----------------------------------------------------------------- *\
|  int InHashCell(ExHashTable *table, const char *word)
|
|  Return 1 if word is in table, and 0 otherwise.
\* ----------------------------------------------------------------- */
int InHashCell(ExHashTable *htable, const char *word)
{
    register HashPtr table;
    register unsigned long hash=0;	/* primary hash value	*/
    register unsigned long skip=1;	/* secondary hash value */
    register int i;

#if 0
    register unsigned long new_hash;	/* primary hash value	*/

    table = htable->words;

    for (i=0; word[i]; i++)
    {
	hash = (hash*HASH_CONST + word[i]) % htable->size;
	skip += 2*hash;
    }

    while (table[hash].theword &&	/* cell not empty and */
	   strcmp((const char*)table[hash].theword, word))
					/* not the right word */
    {
	new_hash = (hash+skip) % htable->size;
	/* See remark in GetHashTable() above */
#if DEBUG
	if (hash == new_hash)
	    (void)printf(
		"hash = new_hash = %ld htable->size = %d field = [%s]\n",
		hash, htable->size, word);
#endif /* DEBUG */
	hash = (new_hash == hash) ? (hash+1) % htable->size : new_hash;
    }
#else
    table = htable->words;
    for (i=0; word[i]; i++)
    {
	hash = (hash ^ HASH_CONST + word[i]);
	skip += hash;
    }

    hash %= htable->size;
    skip %= htable->size;
    if (skip == 0)
	skip = 1;			/* need skip in 1..(htable->size-1) */

    while (table[hash].theword &&	/* cell not empty and */
	   strcmp((const char*)table[hash].theword, word))
					/* not the right word */
    {
	hash += skip;		/* open addressing with double hashing */
	if (hash >= htable->size)
	    hash -= htable->size;	/* hash now in 0..MAXFIELDS */
    }
#endif

    return (table[hash].theword ? 1 : 0);
}

/* ----------------------------------------------------------------- *\
|  void ExtendHashTable(ExHashTable *htable)
|
|  Double the size of the hash table and rehash everything.
\* ----------------------------------------------------------------- */
void ExtendHashTable(ExHashTable *htable)
{
    register HashPtr newcell;
    register HashPtr oldtable;
    Index_t i;
    Index_t oldsize;

    oldsize  = htable->size;
    oldtable = htable->words;

    htable->number = 0;
    htable->size = (Index_t)next_prime(2L * (long)htable->size);
    htable->words = (HashPtr) safemalloc(sizeof(HashCell)*htable->size,
					 "Can't extend hash table for",
					 htable->thekey);

    for (i=0; i < htable->size; i++)
    {
	htable->words[i].theword = (Word)NULL;
	htable->words[i].number = 0;
	htable->words[i].size = 0;
	htable->words[i].refs = NULL;
	htable->words[i].entry = INDEX_NAN;
	htable->words[i].words = NULL;
    }

    for (i=0; i<oldsize; i++)
    {
	if (oldtable[i].theword)
	{
	    newcell = GetHashCell(htable, (const char*)oldtable[i].theword);
	    *newcell = oldtable[i];
	}
    }

    FREE(oldtable);
}


/* ----------------------------------------------------------------- *\
|  void InsertEntry(ExHashTable *htable, char *word, Index_t entry)
|
|  Insert the word/entry pair into the hash table, unless it's
|  already there.  Assumes htable is not the abbreviation table.
\* ----------------------------------------------------------------- */
void InsertEntry(ExHashTable *htable, char *word, Index_t entry)
{
    register HashPtr cell;
    Index_t *newlist;

    if (IsBlackHole(htable))
	return;

    if (htable->number * (unsigned long)4 >
	htable->size * (unsigned long)3)   /* unsigned long, NOT Index_t */
	ExtendHashTable(htable);

    cell = GetHashCell(htable, word);

    if (cell->number && (cell->refs[cell->number-1] == entry))
	return;

    if (cell->number == cell->size)	/* expand the array */
    {
	cell->size *= 2;
	newlist = (Index_t *) safemalloc(cell->size * sizeof(Index_t),
					 "Can't extend entry list for",
					 word);

	bcopy(cell->refs, newlist, cell->number * sizeof(Index_t));
	FREE(cell->refs);
	cell->refs = newlist;
    }
    cell->refs[cell->number++] = entry;
    if (cell->number <= 0)
	die("hash type overflow:", htable->thekey);
}

/* ----------------------------------------------------------------- *\
|  void InsertExpansion(HashPtr cell, const char *theword);
|
|  Insert a new word into the given abbreviation's expansion.
|  Doesn't bother checking for duplicate words [yet].
\* ----------------------------------------------------------------- */
void InsertExpansion(register HashPtr cell, const char *theword)
{
    Word *newlist;

    /* This should never happen... */
    if (cell->number == cell->size)	/* expand the array */
    {
	cell->size *= 2;
	newlist = (Word *) safemalloc(cell->size * sizeof(Word),
				      "Can't extend expansion list for",
				      (const char*)cell->theword);

	bcopy(cell->words, newlist, cell->number * sizeof(Word));
	FREE(cell->words);
	cell->words = newlist;
    }
    cell->words[cell->number] = WordDup((const Word)theword);
    cell->number++;

    if (cell->number <= 0)
	die("hash type overflow:", "abbreviations");
}

/* ----------------------------------------------------------------- *\
|  Standard bibtex abbreviations.  The journal list is taken from
|  "plain.bst", but the journal abbreviations conform (wherever
|  possible) to the computational geometry bibliography's standard.
|  In particular, these may disagree with the abbreviations used in
|  standard ".bst" files.  See the file "authority" for details
|  on the abbreviation standard.  Journal abbreviations are
|  included automatically iff the macro STD_J_ABBREVS is defined.
\* ----------------------------------------------------------------- */
typedef struct
{
    Word abbrev;
    short number;
    Word words[10];
} StdAbbrev;

#ifdef STD_J_ABBREVS
#define NUM_STD_ABBR 32
#else
#define NUM_STD_ABBR 12
#endif /* STD_J_ABBREVS */

const StdAbbrev std_abbr[] =
{
    {"jan",	1, {"january"}},
    {"feb",	1, {"february"}},
    {"mar",	1, {"march"}},
    {"apr",	1, {"april"}},
    {"may",	1, {"may"}},
    {"jun",	1, {"june"}},
    {"jul",	1, {"july"}},
    {"aug",	1, {"august"}},
    {"sep",	2, {"sept", "september"}},
    {"oct",	1, {"october"}},
    {"nov",	1, {"november"}},
    {"dec",	1, {"december"}},

#ifdef LOGABBREVS
    {"acmcs",	3, {"acm", "comput", "surv"}},
    {"acta",	2, {"acta", "inform"}},
    {"cacm",	2, {"commun", "acm"}},
    {"ibmjrd",	3, {"ibm", "res", "develop"}},
    {"ibmsj",	2, {"ibm", "syst"}},
    {"ieeese",	4, {"ieee", "trans", "softw", "eng"}},
    {"ieeetc",	3, {"ieee", "trans", "comput"}},
    {"ieeetcad",7, {"ieee", "trans", "comput", "aided", "design",
			"integrated", "circuits"},
    {"ipl",	3, {"inform", "process", "lett"}},
    {"jacm",	1, {"acm"}},
    {"jcss",	3, {"comput", "syst", "sci"}},
    {"scp",	3, {"sci", "comput", "program."}},
    {"sicomp",	2, {"siam", "comput"}},
    {"tocs",	4, {"acm", "trans", "comput", "syst"}},
    {"tods",	4, {"acm", "trans", "database", "syst"}},
    {"tog",	3, {"acm", "trans", "graph"}},
    {"toms",	4, {"acm", "trans", "math", "softw"}},
    {"toois",	5, {"acm", "trans", "office", "inform", "syst"}},
    {"toplas",	5, {"acm", "trans", "program", "lang", "syst"}},
    {"tcs",	3, {"theoret", "comput", "sci"}},
#endif /* LOGABBREVS */
};

/* ----------------------------------------------------------------- *\
|  void StandardAbbrevs(void)
|
|  Put the standard abbreviations into the abbreviation table.
\* ----------------------------------------------------------------- */
void StandardAbbrevs(VOID)
{
    register HashPtr thecell;
    register int i,j;

    for (i=0; i<NUM_STD_ABBR; i++)
    {
	thecell = GetHashCell(abbrevtable, std_abbr[i].abbrev);

	thecell->entry = INDEX_BUILTIN;
	for (j=0; j<std_abbr[i].number; j++)
	    InsertExpansion(thecell, std_abbr[i].words[j]);
    }
}

const char *badwords[] = BADWORDS;

/* ----------------------------------------------------------------- *\
|  void StandardBadWords(void)
|
|  Put the standard ignorable words into the badword table.
\* ----------------------------------------------------------------- */
void StandardBadWords(VOID)
{
    register int i;

    for (i=0; badwords[i] != (const char*)NULL; i++)
	(void)GetHashCell(badwordtable, badwords[i]);
}


/* ============================= INPUT ============================= *\

   I'd like this to work with more than just the CG bib, so I can't
   assume very much about the input.  In particular, all white space
   (blanks, tabs, and newlines) is identical most of the time.  On
   the other hand, it would be nice to include any "comments" that
   obviously go with an entry as part of that entry.  Consequently,
   two newlines in a row (possibly separated by other white space) is
   considered a break between entries.  This will give us bogus
   entries for stray "comments", but we can take care that easily
   enough -- an entry is only real if it contains a @ character.

\* ================================================================= */

/* ----------------------------------------------------------------- *\
|  unsigned long FindNextEntry(FILE *ifp)
|
|  Return the file offset to the next entry in the bib file.  On exit,
|  the file pointer is left just after the "@".  The entry officially
|  begins after the most recent blank line, the end  of the previous
|  entry, or the beginning of the file.  Returns "-1" if there is no
|  next entry.  It is the CALLER's responsibility to determine the type
|  of entry (normal or @string or @comment or @preamble).
\* ----------------------------------------------------------------- */
unsigned long FindNextEntry(FILE *ifp)
{
    char ch;
    char blank = 0;	  	/* 1 if current line is blank so far */
    unsigned long offset;

    offset = ftell(ifp);
    ch = safegetc(ifp,"reading next entry");
    if (ch == '\n')
    {
	line_number++;
	offset++;
    }
    initial_line_number = line_number;	/* record for errors */

    for (;;)
    {
	if (feof(ifp))
	    return (unsigned long) -1;

	if (ch == '@')			/* got an entry */
	    return offset;
	else if (ch == '\n')
	{
	    if (blank)
	    {
		offset = ftell(ifp);
		initial_line_number = line_number;
	    }
	    blank = 1;
	}
	else if (!isspace(ch))
	    blank = 0;

	ch = safegetc(ifp,"reading next entry");
	if (ch == '\n')
	    line_number++;
    }
}


/* ----------------------------------------------------------------- *\
|  int GetNextWord(FILE *ifp, char *word)
|
|  Get the next word in the current field.  A word is any contiguous
|  set of letters and numbers, AFTER the following steps:
|	1. Letters are folded to lower case.  Thus, "Voronoi" is
|	   returned as "voronoi"
|	2. All TeX commands, except those in math expressions, are
|	   removed, but their arguments are left behind.  Thus,
|	   "Erd{\H o}ss" is returned as "erdos".
|	3. All other non-word characters are removed.  Non-word
|	   characters inside {{possibly} nested} braces or dollar
|          signs do not delimit words, so they may cause unexpected
|	   results.  Thus, "{this example}" is returned as
|	   "thisexample".  (But see below.)
|	4. TeX commands in math expressions are considered normal
|	   text.  Thus, "$O(n\log^2 n)$" is returned as "onlog2n"
|	   instead of "onn".  (But see below.)  This occasionally
|	   gives unexpected or unreadable results.
|	5. Apostrophes, brackets, and hyphens are ignored (but see
|	   below).  Thus, "{\'O}'D{\'u}nlaing" as "odunlaing",
|	   "J[ohn]" as "john", and "half-space" as "halfspace".
|
|  Compound words are words with several components, like "half-space"
|  or "{van Dam}".  Compound words are written into the word buffer
|  with their components separated by null characters.  There are
|  three ways to get compound words:
|	1. Hyphens separate components.  Thus, "half-space" is really
|	   returned as "half\0space".  However, "18--21" is returned
|	   as two separate words.
|	2. Within braces, white space delimits components.  Thus,
|	   "{van Dam}" is really returned as "van\0dam".
|	3. Within math expressions, components are delimited by ANY
|	   string of non-alphanumeric characters.  For example,
|	   "$\Omega(n\log n)$" is really returned as
|	   "omega\0n\0log\0n".
|
|  Returns 0 if no word was found, 1 if a simple word was found, and
|  the number of components if a compound word was found.
|
|  The file pointer is left at the character immediately following the
|  word.  The input is assumed to be syntactically correct: unbalanced
|  braces, math delimiters, or quotation marks will cause errors.
\* ----------------------------------------------------------------- */
int GetNextWord(FILE *ifp, register char *word)
{
    register char ch = ' ';
    char braces = 0;		/* levels of indented braces */
    char math = 0;		/* 1 if in math expression */
    char incmd = 0;		/* 1 if reading TeX command */
    char done = 0;		/* 1 if word is complete */
    char btwn = 1;		/* 1 if "between" components */
    int nchars = 0;		/* how many characters in word? */
    int nwords = 0;		/* how many words have I seen? */

#if DEBUG
    char *start_word = word;		/* DEBUG */
#endif /* DEBUG */

    while (!done)
    {
	ch = safegetc(ifp, "reading next word");

#if DEBUG
	if ((int)(word - start_word) > (MAXWORD - 3))
	{
	    (void)printf("WARNING: word = %-.31s\n", start_word);
	}
#endif /* DEBUG */

	if (isalpha(ch))		/* letters */
	{
	    if (!incmd)			/* may be part of TeX command */
	    {
		if (btwn)
		{
		    nwords++;
		    btwn = 0;
		}
		if (++nchars <= MAXWORD) 	/* ignore overflow */
		    *word++ = tolower(ch);
	    }
	}
	else if (isdigit(ch))		/* digits */
	{
	    incmd = 0;
	    if (btwn)
	    {
		nwords++;
		btwn = 0;
	    }
	    if (++nchars <= MAXWORD)
		*word++ = ch;
	}
	else if (math)			/* other char in math mode */
	{
	    if (ch == '$')	/* end math mode */
	    {
		math = 0;
		braces--;
	    }
	    else if (!btwn)
	    {
		if (++nchars <= MAXWORD)
		    *word++ = 0;
		btwn = 1;
	    }
	}
	else if (ch == '\\') 		/* beginning of TeX command */
	{
	    ch = safegetc(ifp, "reading next word");
	    if (isalpha(ch))
		incmd = 1;
	}
	else if (ch == '{')		/* left brace */
	{
	    incmd = 0;
	    braces++;
	}
	else if (ch == '}')		/* right brace */
	{
	    incmd = 0;
	    if (!braces)
	    {
		ungetc(ch, ifp);
		done = 1;
	    }
	    else
		braces--;
	}
	else if (ch == '"')		/* double quote */
	{
	    incmd = 0;
	    if (!braces)
	    {
		ungetc(ch, ifp);
		done = 1;
	    }
	}
	else if (ch == '$')		/* begin math mode */
	{
	    incmd = 0;
	    math = 1;
	    braces++;
	}
	else if (ch == '-' && !btwn)	/* single hyphens */
	{
	    if (++nchars <= MAXWORD)
		*word++ = 0;
	    btwn = 1;
	}
	else if (isspace(ch) && braces)	/* white space */
	{
	    if (incmd)
		incmd = 0;
	    else if (!btwn)
	    {
		if (++nchars <= MAXWORD)
		    *word++ = 0;
		btwn = 1;
	    }
	}
	else if (incmd)			/* other characters */
	    incmd = 0;
	else if ((ch == '\'') || (ch == '[') || (ch == ']'))
	    /* do nothing */;
	else if ((nwords != 0) && !braces)
	    done = 1;

#if DEBUG
	if ((int)(word - start_word) > (MAXWORD - 3))
	{
	    (void)printf("WARNING: word = %-.31s\n", start_word);
	}
#endif /* DEBUG */

/*	(void)printf("%c {%d} $%d$ \\%d -%d- #%2d/%d\n",
	       ch, braces, math, incmd, btwn, nchars, nwords); */

	/* Two situations can produce an unusually long `word' that
	exceeds MAXWORD characters:
	   (a) a long compound word
	   (b) a long braced string (e.g. braces added to protect
	       a complete title from lower-casing in certain
	       bibliography styles)
	In either case, we bail out early, making sure to properly
	terminate the string. */

	if (nchars > MAXWORD)
	{
	    done = 1;		/* bail out early if word buffer overflows */
	    *word = 0;
	    while (braces > 0)	/* must flush rest of braced string */
	    {
		ch = safegetc(ifp, "reading next word");
		if (ch == '{')
		    braces++;
		else if (ch == '}')
		    braces--;
	    }
	}
    }

    if (!btwn)
	*word = 0;

#if DEBUG
    if (strlen(start_word) > (size_t)MAXWORD)
    {
	(void)printf("ERROR: word:%d = [%s]\n", strlen(start_word), start_word);
    }
#endif /* DEBUG */

    return nwords;
}

/* ----------------------------------------------------------------- *\
|  int iskeychar(char c, int first_char)
|
|  Return 1 if c is a valid keyword character, and 0 otherwise.
|  The rules are different for the first character, so first_char
|  must be set non-zero to select that case.
|
|  The code in bibtex.web in the section on character set dependencies
|  creates a character classification array, id_class[], which permits
|  the following characters (verified by experiment too) in field
|  names:
|      A-Z a-z 0-9
|      ! $ & * + - . / : ; < > ? @ [ \ ] ^ _ ` | ~ <DELete>
\* ----------------------------------------------------------------- */
int iskeychar(char c, int first_char)
{
    if (isalpha(c))
	return 1;
    else if (first_char)
	return 0;
    else if (isdigit(c))
	return 1;
    else if (iscntrl(c))
	return 0;
    else
	return (strchr(NONKEYCHARS, (int)c) == (char *)NULL);
}


/* ----------------------------------------------------------------- *\
|  int IsRealWord(char *theword)
|
|  Is this a word I really want to index?
\* ----------------------------------------------------------------- */
int IsRealWord(register char *theword)
{
#if 0
    register int i;
#endif

    if (!theword[0] || !theword[1])	/* empty or 1 character */
	return 0;

#if 0
    for (i=0; badwords[i]; i++)
	if (!strcmp(theword, badwords[i]))
	    return 0;
    return 1;
#else
    return (!InHashCell(badwordtable,theword));
#endif

}

/* ----------------------------------------------------------------- *\
|  void MungeField(FILE *ifp, void (*action)(char *, void *, void *)),
|		   void *arg1, void *arg2)
|
|  Munge the current field.  For every word in the field, call
|  (*action), passing in the word and the args.  If the word is an
|  abbreviation, call (*action) on the words in its expansion.
|
|  On entrance, the file pointer is just after the field name.  On
|  exit, the file pointer is on the comma or closing character for
|  the entry.
\* ----------------------------------------------------------------- */
void MungeField(FILE *ifp, void (*action)(char *, void *, void *),
		void *arg1, void *arg2)
{
    register char ch;
    register int i, nwords;
    register char *tmp, *tmp2;
    Index_t k;
    char nextword[MAXWORD+1];
    HashPtr abbrevcell;

    ch = safegetc(ifp, "looking for =");
    while (isspace(ch))
	ch = safegetc(ifp, "looking for =");

    if (ch != '=')
	diechar("Illegal character: ", ch);

    for (;;)
    {
	ch = safegetc(ifp, "looking for open quote/brace");
	while (isspace(ch))
	    ch = safegetc(ifp, "looking for open quote/brace");

	if ((ch == '{') || (ch == '"'))
	{
	    nwords = GetNextWord(ifp, nextword);
	    while (nwords != 0)
	    {
		if (nwords != 1)	/* compound word */
		{
		    tmp = nextword;
		    for (i=0; i<nwords; i++)
		    {
			if (IsRealWord(tmp))
			    (*action)(tmp, arg1, arg2);
			tmp += strlen(tmp) + 1;
		    }

		    tmp = nextword + strlen(nextword);
		    tmp2 = tmp + 1;
		    while (nwords > 1)	/* recombine the components */
		    {
			if (!*tmp2)
			{
			    tmp2++;
			    nwords--;
			}
			else
			    *tmp++ = *tmp2++;
		    }
		    *tmp = 0;
		}

		if (IsRealWord(nextword))
		    (*action)(nextword, arg1, arg2);

		nwords = GetNextWord(ifp, nextword);
	    }
	    ch = safegetc(ifp, "reading close quote/brace");
	    ch = safegetc(ifp, "looking for comma or close brace");
	}
	else if (isalpha(ch))	/* abbreviation */
	{
	    for (i=0; iskeychar(ch, i==0); i++)
	    {
		if (i >= MAXWORD)
		{
		    nextword[MAXWORD] = 0;
		    die("word buffer overflow", nextword);
		}
		nextword[i] = tolower(ch);
		ch = safegetc(ifp, "reading abbreviation");
	    }
	    nextword[i] = 0;
	    nextword[MAXWORD] = 0;

	    (*action)(nextword, arg1, arg2);

	    /* --- Munge the abbreviation's expansion, too --- */

	    abbrevcell = GetHashCell(abbrevtable, nextword);
	    if (abbrevcell->entry == INDEX_NAN)
		warn("Undefined abbreviation:", nextword);

	    for (k=0; k<abbrevcell->number; k++)
		(*action)(abbrevcell->words[k], arg1, arg2);
	}
	else if (isdigit(ch))	/* number */
	{
	    for (i=0; isdigit(ch); i++)
	    {
		if (i >= MAXWORD)
		{
		    nextword[MAXWORD] = 0;
		    die("word buffer overflow", nextword);
		}
		nextword[i] = tolower(ch);
		ch = safegetc(ifp, "reading number");
	    }
	    nextword[i] = 0;
	    nextword[MAXWORD] = 0;
	    (*action)(nextword, arg1, arg2);
	}
	else
	    diechar("Illegal character:", ch);

	while (isspace(ch))
	    ch = safegetc(ifp, "looking for comma, close brace, or #");

	if ((ch == ',') || (ch == '}') || (ch == ')'))
	{
	    ungetc(ch, ifp);
	    return;
	}
	else if (ch == '#')
	    continue;
	else
	    diechar("Illegal character:", ch);
    }
}

/* ----------------------------------------------------------------- *\
|  void MF_InsertExpansion(char *word, ExHashTable *htable, HashPtr cell)
|
|  Version of InsertEntry for passing to MungeField
\* ----------------------------------------------------------------- */
void MF_InsertExpansion(char *word, ExHashTable *htable, HashPtr cell)
{
    InsertEntry(htable, word, cell->entry);
    InsertExpansion(cell, word);
}

/* ----------------------------------------------------------------- *\
|  void MF_InsertEntry(char *word, ExHashTable *htable, Index_t *entry)
|
|  Version of InsertEntry for passing to MungeField
\* ----------------------------------------------------------------- */
void MF_InsertEntry(char *word, ExHashTable *htable, Index_t *entry)
{
    InsertEntry(htable, word, *entry);
}


/* ----------------------------------------------------------------- *\
|  void MungeAbbrev(FILE *ifp, Index_t entry)
|
|  Wander though the abbreviation, putting the words into the
|  abbreviation table.  Looks a lot like MungeField, doesn't it?
\* ----------------------------------------------------------------- */
void MungeAbbrev(FILE *ifp, Index_t entry)
{
    register char ch;
    register int i;
    char theabbrev_chars[MAXWORD+1];
    Word theabbrev = (Word)&theabbrev_chars[0];
    HashPtr thecell;
    ExHashTable *htable;

    ch = safegetc(ifp, "looking for abbreviation");
    while (isspace(ch))
	ch = safegetc(ifp, "looking for abbreviation");

    if (!isalpha(ch))
	diechar("Non-letter starting abbreviation:", ch);

    for (i=0; iskeychar(ch, i==0); i++)
    {
	if (i >= MAXWORD)
	{
	    theabbrev[i] = 0;
	    die("abbreviation buffer overflow", theabbrev);
	}
	theabbrev[i] = tolower(ch);
	ch = safegetc(ifp, "reading abbreviation");
    }
    ungetc(ch, ifp);		/* put back lookahead char */
    theabbrev[i] = 0;

    if (abbrevtable->number * (unsigned long)4 >
	abbrevtable->size * (unsigned long)3)
	ExtendHashTable(abbrevtable);

    htable = GetHashTable("@string");

    thecell = GetHashCell(abbrevtable, theabbrev);
    if (thecell->entry != INDEX_NAN)
	warn("Multiply-defined abbreviation:", theabbrev);
    thecell->entry = entry;

    MungeField(ifp, (void (*)(char *, void *, void *))MF_InsertExpansion,
	       (void *)htable, (void *)thecell);

    ch = safegetc(ifp, "trying to read close brace");
}


/* ----------------------------------------------------------------- *\
|  void MungeRealEntry(FILE *ifp, Index_t entry)
|
|  Wander though the entry, mungeing each field.  On entry, the file
|  pointer is just after the opening brace/paren.
\* ----------------------------------------------------------------- */
void MungeRealEntry(FILE *ifp, Index_t entry)
{
    register char ch;
    char thefield_chars[MAXWORD+1];
    Word thefield = (Word)&thefield_chars[0];
    int i;
    ExHashTable *htable;

    ch = safegetc(ifp, "looking for citekey");
    while (isspace(ch))
	ch = safegetc(ifp, "looking for citekey");

    /* Pretty much anything can go in a bibtex key, including braces, */
    /* parens, quotes, and even chars that are illegal ANYWHERE else! */
    while (ch != ',')
	ch = safegetc(ifp, "reading citekey");

    while (ch == ',')
    {
	ch = safegetc(ifp, "looking for field descriptor");
	while (isspace(ch))
	    ch = safegetc(ifp, "looking for field descriptor");

	if ((ch == '}') || (ch == ')'))	/* allow trailing comma after */
	    return;			/* last key = "value" entry */

	if (!isalpha(ch))
	    diechar("Non-letter starting field descriptor:", ch);

	for (i=0; iskeychar(ch, i==0); i++)
	{
	    if (i >= MAXWORD)
	    {
		thefield[i] = 0;
		die("field name buffer overflow", thefield);
	    }
	    thefield[i] = tolower(ch);
	    ch = safegetc(ifp, "reading field descriptor");
	}
	ungetc(ch, ifp);		/* put back lookahead char */
	thefield[i] = 0;

	htable = GetHashTable(thefield);
	MungeField(ifp, (void (*)(char*, void*, void*))MF_InsertEntry,
		   (void *)htable, (void *)&entry);

	ch = safegetc(ifp, "trying to read comma or close brace");
    }
}

/* ----------------------------------------------------------------- *\
|  void SkipEntry(FILE *ifp)
|
|  Skip the current entry, since it's a @comment or @premable.
\* ----------------------------------------------------------------- */
void SkipEntry(FILE *ifp)
{
    char ch;
    int braces;
    int quotes;

    quotes = 0;
    braces = 0;
    ch = safegetc(ifp, "skipping false entry");

    while (quotes || braces || ((ch != '}') && (ch != ')')))
    {
	if (ch == '{')
	    braces++;
	else if (ch == '}')
	    braces--;
	else if ((ch == '"') && !braces)
	    quotes = !quotes;

	ch = safegetc(ifp, "skipping false entry");
    }
}


/* ----------------------------------------------------------------- *\
|  int MungeEntry(FILE *ifp, Index_t entry)
|
|  Determine whether the current entry is real or @comment or
|  @preamble, and munge the entry appropriately.  Return 1 if the
|  entry was real, or 0 if it was a "comment".
\* ----------------------------------------------------------------- */
int MungeEntry(FILE *ifp, Index_t entry)
{
    register char ch;
    char therecord_chars[MAXWORD+1];
    Word therecord = (Word)&therecord_chars[0];
    int i;

    ch = safegetc(ifp, "looking for record descriptor");
    while (isspace(ch))
	ch = safegetc(ifp, "looking for record descriptor");

    if (!isalpha(ch))
	diechar("Non-letter starting record descriptor:", ch);

    for (i=0; iskeychar(ch, i==0); i++)
    {
	if (i >= MAXWORD)
	{
	    therecord[i] = 0;
	    die("record name buffer overflow", therecord);
	}
	therecord[i] = tolower(ch);
	ch = safegetc(ifp, "recording record descriptor");
    }
    therecord[i] = 0;

    while (isspace(ch))
	ch = safegetc(ifp, "looking for open brace");

    if ((ch != '(') && (ch != '{'))
	diechar("Open brace expected after record descriptor:", ch);

    if (!strcmp(therecord, "string"))
    {
	MungeAbbrev(ifp, entry);
	return 1;
    }
    else if (!strcmp(therecord, "comment"))
    {
	SkipEntry(ifp);
	return 0;
    }
    else if (!strcmp(therecord, "preamble"))
    {
	SkipEntry(ifp);
	return 0;
    }
    else
    {
	MungeRealEntry(ifp, entry);
	return 1;
    }
}


/* ============================= OUTPUT ============================ */

/* ----------------------------------------------------------------- *\
|  void WriteWord(FILE *ofp, Word word)
|
|  Output the word in "Pascal" string format -- length byte followed
|  by characters.  This saves some disk space over writing MAXWORD+1
|  bytes in all cases.
\* ----------------------------------------------------------------- */
void WriteWord(FILE *ofp, Word word)
{
    unsigned char length = (unsigned char)strlen((const char*)word);

    /* Apply a sanity check: had this been here in the first place,
       a nasty bug introduced with compound word support would have
       been caught many hours sooner. */
    if (length > MAXWORD)
	die("Writing word to index: compile-time buffer overflow","");
    fwrite((void *) &length, sizeof(length), 1, ofp);
    fwrite((void *) word, sizeof(char), (size_t)length, ofp);
}

/* ----------------------------------------------------------------- *\
|  void NetOrderFwrite(void *vbuf, int s, int n, FILE *fp)
|
|  Write n elements of size s in network byteorder
\* ----------------------------------------------------------------- */
static void NetOrderFwrite(void *vbuf, size_t s, size_t n, FILE *fp)
{
    char *buf = (char *)vbuf;

    while (n-- > 0)
    {
	size_t rc;

	if (s == sizeof (short))
	{
	    short x = htons(* (short *) buf);
	    rc = fwrite(&x, s, 1, fp);
	}
	else				/* assume s == sizeof (long) */
	{
	    long x = htonl(* (long *) buf);
	    rc = fwrite(&x, s, 1, fp);
	}

	if (rc != 1)
	{
	    perror("bibindex: cannot write; reason");
	    exit(EXIT_FAILURE);
	}
					/* advance and continue: */
	buf += s;
    }
}

int CompareCells(const void *a, const void *b)
{
    HashPtr ha = (HashPtr)a;
    HashPtr hb = (HashPtr)b;
    return (strcmp(ha->theword,hb->theword));
}

int CompareFields(const void *a, const void *b)
{
    ExHashTable *af = (ExHashTable *)a;
    ExHashTable *bf = (ExHashTable *)b;
    return (strcmp(((ExHashTable*)a)->thekey,((ExHashTable*)b)->thekey));
}

/* ----------------------------------------------------------------- *\
|  void SortTable(ExHashTable* htable)
|
|  Compress and sort a hash table.
\* ----------------------------------------------------------------- */
void SortTable(register ExHashTable* htable)
{
    register HashPtr words;
    Index_t m, n;

    words = htable->words;

    for (m = 0, n = 0; m < htable->size; m++)
    {
	if (words[m].theword)
	{
	    if (m > n)
	    {
		words[n++] = words[m];	 /* copy mth table to nth */
		words[m].number = 0;	 /* then clear mth table */
		words[m].size = 0;	 /* to avoid duplicate free() later */
		words[m].refs = (Index_t*)0;
	    }
	}
    }
    htable->number = n;		/* new size is needed in OutputTables */
    qsort(words, (size_t)htable->number, sizeof(HashCell),
	  (int (*)(const void*,const void*))CompareCells);
}

/* ----------------------------------------------------------------- *\
|  void OutputTables(FILE *ofp)
|
|  Compress and output the tables, with lots of user feedback.
\* ----------------------------------------------------------------- */
void OutputTables(FILE *ofp)
{
    register HashPtr words;
    register int i, k, count;
    Index_t m;
    int numwords, numrefs;

    numwords = numrefs = 0;

    (void)printf("Writing index tables...");
    fflush(stdout);

    numfields = 0;		/* recount, ignoring black holes */
    for (i = 0; i < MAXFIELDS; i++)
    {
	if (fieldtable[i].words)
	{
	    if (i > (int)numfields)
	    {
		fieldtable[(int)numfields++] = fieldtable[i];/* copy table */
		fieldtable[i].number = 0; /* then clear old one to */
		fieldtable[i].size = 0;	/* avoid duplicate free() later */
		fieldtable[i].words = NULL;
	    }
	}
    }
    qsort(fieldtable, (size_t)numfields, sizeof(ExHashTable),
	  (int (*)(const void*,const void*))CompareFields);

    fwrite((void *) &numfields, sizeof(numfields), 1, ofp);
    for (i=0; i < (int)numfields; i++)
	WriteWord(ofp, fieldtable[i].thekey);

    (void)printf("%d fields\n", (int)numfields);

    for (k=0; k < (int)numfields; k++)
    {
	(void)printf("%2d. %-12s ", k+1, (const char*)fieldtable[k].thekey);
	fflush(stdout);

	SortTable(&(fieldtable[k]));
	NetOrderFwrite((void *) &(fieldtable[k].number),
		       sizeof(Index_t), 1, ofp);
	count = 0;
	words = fieldtable[k].words;
	for (m=0; m<fieldtable[k].number; m++)
	{
	    WriteWord(ofp, words[m].theword);
	    NetOrderFwrite((void *) &(words[m].number),
			   sizeof(Index_t), 1, ofp);
	    NetOrderFwrite((void *) words[m].refs, sizeof(Index_t),
			   words[m].number, ofp);
	    count += words[m].number;
	}

	(void)printf("%6d words,%8d references,%6.2f refs/word\n",
		     (int)fieldtable[k].number, count,
		     (double)count/((fieldtable[k].number == 0) ?
				    1.0 : (double)fieldtable[k].number));
	numwords += fieldtable[k].number;
	numrefs += count;
    }

    (void)printf("--- TOTAL ---   %7d words,%8d references,%6.2f refs/word\n",
	   numwords, numrefs,
	   (double)numrefs/(double)((numwords == 0) ? 1 : numwords));

    (void)printf("Writing abbrev table...");
    fflush(stdout);

    SortTable(abbrevtable);
    NetOrderFwrite((void *) &(abbrevtable->number),
		   sizeof(Index_t), 1, ofp);

    words = abbrevtable->words;
    for (m=0; m<abbrevtable->number; m++)
	WriteWord(ofp, words[m].theword);
    for (m=0; m<abbrevtable->number; m++)
	NetOrderFwrite((void *) &(words[m].entry), sizeof(Index_t), 1, ofp);

    (void)printf("%d+%d abbreviations\n",
	   abbrevtable->number - NUM_STD_ABBR, NUM_STD_ABBR);
}


/* ========================== MAIN PROGRAM ========================= */

/* ----------------------------------------------------------------- *\
|  IndexBibFile(FILE *ifp, FILE *ofp, char *filename)
|
|  Index a bibtex file.  Input comes from ifp; output goes to ofp.
|  Filename is the name of the bibliography, with no prefix.
\* ----------------------------------------------------------------- */
void IndexBibFile(FILE *ifp, FILE *ofp, char *filename)
{
    Index_t count=0;
    long curoffset;
    long *offsets;
    long *oldoff;
    Index_t offsize;
    time_t now = time(0);

    (void)printf("Indexing %s.bib.", filename);
    fflush(stdout);

    offsize = MIN_OFFSET_LIST_SIZE;
    offsets = (long *) safemalloc(offsize * sizeof(long),
				  "Can't allocate offsets list", "");

    while (!feof(ifp))
    {
	curoffset = FindNextEntry(ifp);
	if (curoffset == (unsigned long) -1)
	    break;

	if (MungeEntry(ifp, count))
	{
	    offsets[count++] = curoffset;

	    if (count == offsize)	/* expand full offset array */
	    {
		oldoff = offsets;
		offsize *= 2;
		offsets = (long *) safemalloc(offsize * sizeof(long),
					      "Can't extend offsets list", "");
		bcopy(oldoff, offsets, count * sizeof(long));
		FREE(oldoff);
	    }

	    if (!(count % 200))		/* feedback */
	    {
		if (count % 1000)
		    putchar('.');
		else
		    (void)printf("%d.", count);
		fflush(stdout);
	    }
	}
    }

    (void)printf("done.\n");

    (void)fprintf(ofp, "bibindex %d %d %d %s", FILE_VERSION, MAJOR_VERSION,
	    MINOR_VERSION, ctime(&now));

    (void)printf("Writing offset table...");
    fflush(stdout);
    NetOrderFwrite((void *) &count, sizeof(Index_t), 1, ofp);
    NetOrderFwrite((void *) offsets, sizeof(long), count, ofp);
    FREE(offsets);
    (void)printf("%d entries\n", count);

    OutputTables(ofp);

    (void)printf("All done!\n");
}

/* ----------------------------------------------------------------- *\
|  The main program
\* ----------------------------------------------------------------- */

int main(int argc, char **argv)
{
    FILE *ifp;
    FILE *ofp;
    char infile[FILENAME_MAX+1];
    char outfile[FILENAME_MAX+1];
    char *p, *opts;
    int i, inopt;

#if DEBUG_MALLOC
    malloc_debug(2);
#endif /* DEBUG_MALLOC */

    if (argc < 2)
	die("Usage: bibindex bib [-i field...]", "");

    if (((p = strrchr(argv[1],'.')) != (char*)NULL) &&
	(strcmp(p, ".bib") == 0))
    {
	*p = '\0';			/* remove any .bib extension */
    }

    (void)sprintf(infile, "%s.bib", argv[1]);
    (void)sprintf(outfile, "%s.bix", argv[1]);

    ifp = fopen(infile, "r");
    if (!ifp)
	die("Can't read", infile);

    ofp = (fopen)(outfile, "w");
    if (!ofp)
	die("Can't write", outfile);

    InitTables();
    StandardAbbrevs();
    StandardBadWords();

    if ((argc > 2) && (!strcmp(argv[2], "-i")))
    {
	for (i=3; i<argc; i++)
	    InitBlackHole(argv[i]);
    }
    else
    {
	opts = (char*)getenv("BIBINDEXFLAGS");
	if (opts != NULL)
	{
	    p = opts;
	    inopt = 0;

	    while (*p != 0)
	    {
		if (isspace(*p))
		{
		    if (inopt)
		    {
			inopt = 0;
			*p = 0;
			if (strcmp(opts, "-i"))
			    InitBlackHole(opts);
			opts = p+1;
		    }
		}
		else
		    inopt = 1;

		p++;
	    }

	    if (inopt && strcmp(opts, "-i"))
		InitBlackHole(opts);
	}
    }

    IndexBibFile(ifp, ofp, argv[1]);

    FreeTables();
    fclose(ifp);
    (fclose)(ofp);

    exit(EXIT_SUCCESS);			/* Argh! */
    return (0);		/* keep compilers happy */
}
#if vms
/* This code is a replacement for the VAX VMS qsort() routine, which
gives unexplained NULL pointer dereferencing with this program.  The
comments below about the need for a stable sorting algorithm for
TeXindex are not relevant here. */

/* -*-C-*- shsort.c */
/*-->shsort*/
/**********************************************************************/
/****************************** shsort ********************************/
/**********************************************************************/

/***********************************************************************
TeXindex used the standard  UNIX library  function  qsort()   for record
sorting.  Unfortunately, qsort() is not a stable  sorting  algorithm, so
input order is not  necessarily preserved for  equal sort keys.  This is
important,  because the  sorting is case-independent,   while the actual
entries may not be.  For example, the input

\entry{i}{22}{{\CODE{i}}}
\entry{i}{42}{{\CODE{i}}}
\entry{I}{41}{{\CODE{I}}}
\entry{I}{42}{{\CODE{I}}}

produces

\initial {I}
\entry {{\CODE{i}}}{22}
\entry {{\CODE{I}}}{41--42}
\entry {{\CODE{i}}}{42}

instead of the correct

\initial {I}
\entry {{\CODE{i}}}{22, 42}
\entry {{\CODE{I}}}{41--42}

We therefore provide this stable shellsort replacement for qsort() based
on  the code  given   on p.   116 of   Kernighan  and  Ritchie,  ``The C
Programming  Language'',  Prentice-Hall (1978).    This has order N**1.5
average  performance,  which is  usually   slower  than qsort().  In the
interests of simplicity, we make no attempt to handle short sequences by
alternative methods.

[16-Dec-1988]
***********************************************************************/

#define BASE(i)	&base[(i)*width]

#define STDC (__STDC__ || defined(__cplusplus))

void
#if STDC
shsort(
char*			base,		/* start of data in memory */
size_t			nel,		/* number of elements to be sorted */
size_t			width,		/* size (in bytes) of each element */
int			(*compar) (const void*, const void*)
)
#else /* NOT STDC */
shsort(base, nel, width, compar)
char*			base;		/* start of data in memory */
size_t			nel;		/* number of elements to be sorted */
size_t			width;		/* size (in bytes) of each element */
int			(*compar) ();
#endif /* STDC */
{
    register char	c;
    int			gap;
    int			i;
    int			j;
    register size_t	k;	/* inner exchange loop parameters */
    register char	*p;
    register char	*q;

    for (gap = (int)nel/2; gap > 0; gap /= 2)
    {
	for (i = gap; i < (int)nel; i++)
	{
	    for (j = i-gap; j >= 0; j -= gap)
	    {
		p = BASE(j);
		q = BASE(j+gap);
		if ((*compar)(p,q) <= 0)
		    break;	/* exit j loop */
		else
		{
		    for (k = 0; k < width; (++p, ++q, ++k))
		    {
			c = *q;
			*q = *p;
			*p = c;
		    }
		}
	    }
	}
    }
}
#endif /* vms */
