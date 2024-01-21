/* ================================================================= *\

   biblook -- look up references in a bibindexed BibTeX file

   This program was specifically developed for use with the
   computational geometry bibliographic database.  The database
   can be obtained by anonymous ftp from cs.usask.ca in the file
   `pub/geometry/geombib.tar.Z'.

   Version 1.0 written by Jeff Erickson <jeff@ics.uci.edu>, 27 Mar 92
   Version 2.0 written by Jeff Erickson <jeff@ics.uci.edu>, 17 Jun 92

   This program is in the public domain.  You may use it or modify
   it to your heart's content, at your own risk.  Bouquets, brickbats,
   and bug fixes may be sent to Jeff Erickson, jeffe@cs.berkeley.edu.

   %Make% gcc -O -o biblook biblook.c

   Usage: biblook bibfile [savefile]

   -----------------------------------------------------------------

   HOW IT WORKS:

   The user can enter any of the following commands:

   f[ind] [not] <field> <words>
	Find the entries containing the given words in any field
	with a prefix matching the <field> argument.  For example,
	`a' matches both `author' and `address', and `au' matches
	`author' only.  If the <field> argument is `-' (or any
	string with no letters or numbers), match any field.

	If `not' appears before the <field>, the sense of the search
	is reversed.  The symbols `~' and `!' can be used in place
	of `not'.

	Each word is a contiguous sequence of letters and digits.
	Case is ignored; accents should be omitted; apostrophes are
	not required.  Single characters and a few common words are
	also ignored.  Any word ending with an asterisk is treated
	as a prefix.  Thus, `point*' matches `point', `points',
	`pointer', etc.

   and [not] <field> <words>
   or [not] <field> <words>
	Intersect (resp. union) the results of the given search
	with the previous search.  Several of these commands may be
	combined on a single line.  Commands are handled in the order
	in which they appear; there is no precedence.  Unlike other
	commands, and like `not', these must be spelled out
	completely.  `&' can be used in place of `and', and `|' can
	be used in place of `or'.

   d[isplay]
	Display the results of the previous search.

   s[ave] [<filename>]
	Save the results of the previous results into the specified
	file.  If <filename> is omitted, the previous save file is
	used.  If no save file has ever been specified, results are
	saved in the file specified on the command line.  If no such
	file is specified, `save.bib' is used.  If the save file
	exists, results are appended to it.

   w[hatis] <abbrev>
	Display the definition of the abbreviation <abbrev>.

   q[uit]/EOF
	Quit.

   Several commands can be combined on a single line by separating
   them with semicolons.  For example, the following command displays
   all STOC papers cowritten by Erdo"s without `Voronoi diagrams' in
   the title:

   f b stoc* | b symp* theory comp* & au erdos & ~t voronoi diagrams ; d

   -----------------------------------------------------------------
   Version history

   1.0 <jge> 3/29/92	Initial version complete
   1.1 <jge> 4/3/92	Fixed GetToken bug.
			Prompts and feedback messages sent to stderr
			instead of stdout, so results can be
			redirected to a file.

   2.0 <jge> 6/17/92	Major change in file format and commands.
	1. Allow searching on any field or all fields.
	2. More extensive boolean queries (and, or, not)
	3. New command to save results to a file
	4. New command to display results, rather than displaying
	   them automatically.
	5. Allow searching for prefixes
	6. Pipe display results through $PAGER or /usr/ucb/more
   2.1 <jge> 7/8/92	Minor bug fixes.
   2.3 Bill Jones <jones@cs.usask.ca> 93/01/29
	1. Declarations common to bibindex.c and biblook.c factored out
	   to new file biblook.h.
	2. Index type of (signed) short overflows early; created typedef
	   Index_t, defined as unsigned short.
   2.4 Nelson H. F. Beebe <beebe@math.utah.edu> [01-Jun-1993]
	1. Remove some mixed-mode arithmetic.
	2. Add cast to return value of fork().
	3. Correct use and type of numoffsets so that code works
	   if Index_t is "unsigned int" or "unsigned long".
   2.5 Erik Schoenfelder <schoenfr@ibr.cs.tu-bs.de> [14-Aug-1993]
	1. Add support for network byte order I/O, so that index files
	   can be shared between big-endian and little-endian systems.
	   This option is selected when HAVE_NETINET_IN_H is defined
	   at compile time (default on UNIX).
       Nelson H. F. Beebe <beebe@math.utah.edu> [14-Aug-1993]
	2. Add typecast (int) in bibindex:OutputTables() array reference
	   to eliminate compiler warnings.
	3. Correct code in biblook:SetComplement() to check for zero
	   setmask; otherwise, a .bib file with a number of entries
	   which is an exact multiple of setsize (commonly 32) will
	   result in failing searches for the last setsize entries!
   2.6 <jge> 8/29/93
	1. Simplified "help" output so it fits in one screen.
	2. Made help command act like a COMMAND -- "find help" no
	   longer displays the help message followed by an error
	   message.
	3. Fixed error with not -- "f not" followed by "f a erdos"
	   no longer finds all entries NOT written by Erdos!
	4. Added "safemalloc" routine from bibindex.
	5. Added "whatis" command to look up @strings.  Since @strings
	   are now indexed as if "@string" were a field, it's now
	   trivial to look up all abbreviations containing a given set
	   of words.
	6. Index lists are now read in only when they are required for
	   a search.  Only the CACHESIZE most recently accessed lists
	   are kept; older ones are freed.
	7. Added support for BIBLOOK environment variable, which
	   stores a search path for bib/bix files.  Defaults to
	   BIBINPUTS, or just the current directory if neither of
	   those exist.
       Nelson H. F. Beebe <beebe@math.utah.edu> [12-Sep-1993]
	8. Restore long form of help, offering short form first,
	   then long form if help is asked for a second time.  The
	   "telnet biblio" service on siggraph.org needs the long
	   form, since remote users cannot be expected to have a
	   biblook manual page.  Telling users to go read the manual
	   is unfriendly.
	9. Change "/usr/ucb/more" and "more" to compile-time settable
	   MOREPATH and MORE, and set Makefile to use "less" when available.
       10. Change type of numfields from char to unsigned char, and
           use sizeof(numfields) instead of sizeof(type of numfields).
       11. Change all exit() calls to use Standard C EXIT_xxx symbols.
       12. Put (void) casts on printf() family calls to avoid compiler
           warnings about discarded returned values.
       13. Change safemalloc() to request non-zero size when zero size
           is requested; some systems reject a malloc(0) request (e.g.
	   IBM RS/6000 AIX 3.2).
       14. Change &abbrevlocs to abbrevlocs in GetTables().  This error
           went undetected on 23 O/S-compiler combinations, but was
	   caught on DECstation ULTRIX with both gcc and g++, the only
	   one of these systems which is little-endian.  Network byte
	   order is big-endian, so ConvertToHostOrder() is a no-op
	   on such systems.

\* ================================================================= */


#include "biblook.h"

#if vms
#if 0
/* I don't know why tmpnam() doesn't work properly, sigh... */
#define tempnam(dir,prefix)	(dir,prefix,strdup(tmpnam((char*)NULL)))
#else
#define tempnam(dir,prefix)	(dir,prefix,strdup("biblook.tmp"))
#endif
char* strdup(char *s)
{
    char *p;
    if (s == (char*)NULL)
	s = "";
    p = malloc(strlen(s) + 1);
    if (p != (char*)NULL)
	strcpy(p,s);
    return (p);
}

#if defined(__ALPHA)
#include <unixio.h>
#endif
#define unlink(filename)	delete(filename)
#endif /* vms */

#define MAX_TEXTBUF	256

static char bibfile[FILENAME_MAX+1];
static char bixfile[FILENAME_MAX+1];

#if defined(USE_MEMIO)
/* We improve things even more by making saferead() and safegetc()
expand inline.  safegetc() will only result in a function call on a
fatal error. */

#define safefread(buf_,size_,count_,fp_) \
		if (fread(buf_,size_,count_,fp_) < count_) \
		    pdie("Error reading", bixfile)

static int safegetc_;
#define safegetc(fp_)		(safegetc_ = getc(fp_),\
				((safegetc_ == EOF) \
				 ? (pdie("Error reading", bibfile), EOF) \
				 : safegetc_))
#endif /* defined(USE_MEMIO) */

/* ======================= UTILITY FUNCTIONS ======================= */

/* ----------------------------------------------------------------- *\
|  void die(const char *msg1, const char *msg2)
|
|  print an error message and die
\* ----------------------------------------------------------------- */
void die(const char *msg1, const char *msg2)
{
    (void)fprintf(stderr, "Error:  %s %s\n", msg1, msg2);
    exit(EXIT_FAILURE);
}

/* ----------------------------------------------------------------- *\
|  void pdie(const char *msg1, const char *msg2)
|
|  Print a custom error message and a system error message, then die.
\* ----------------------------------------------------------------- */
void pdie(const char *msg1, const char *msg2)
{
    char msg[MAX_TEXTBUF];
    (void)sprintf(msg, "%s %s", msg1, msg2);
    perror(msg);
    exit(EXIT_FAILURE);
}

/* ----------------------------------------------------------------- *\
|  void safefread(void *ptr, size_t size, size_t num, FILE *fp)
|
|  Read from the file, but die if there's an error.
\* ----------------------------------------------------------------- */
void (safefread)(void *ptr, size_t size, size_t num, FILE *fp)
{
    if (fread(ptr, size, num, fp) < num)
	pdie("Error reading", bixfile);
}

/* ----------------------------------------------------------------- *\
|  char safegetc(FILE *fp)
|
|  Get the next character safely.  Used by routines that assume that
|  they won't run into the end of file.
\* ----------------------------------------------------------------- */
char (safegetc)(FILE *fp)
{
    if (feof(fp))
	pdie("Error reading", bibfile);
    return getc(fp);
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

    tmp = (void*)malloc((howmuch > 0) ? howmuch : 1);
			/* some malloc's fail with zero size request */
    if (tmp == NULL)
	pdie(msg1, msg2);

    return tmp;
}

/* ----------------------------------------------------------------- *\
|  void ConvertToHostOrder(int n, int s, void *xx)
|
|  Convert n elements of size s to host-byteorder
\* ----------------------------------------------------------------- */
static void ConvertToHostOrder(size_t n, size_t s, void *xx)
{
    short *x = (short *)xx;
    long *y = (long *)xx;

    if (s == sizeof (short))
    {
	while (n-- > 0)
	{
	    *x = ntohs(*x);
	    x++;
	}
    }
    else			/* assume s == sizeof (long) */
    {
	while (n-- > 0)
	{
	    *y = ntohl(*y);
	    y++;
	}
    }
}

#ifndef SCREEN_LINES
#define SCREEN_LINES	24	/* typical screen size */
#endif

#if defined(unix) && (SCREEN_LINES > 0)
#if !defined(HAVE_SGTTY_H) && !defined(HAVE_TERMIO_H) && !defined(HAVE_TERMIOS_H)
#if _AIX || __AIX370 || ardent || __hppa || __mips || sun || __sgi || ultrix
#define HAVE_TERMIO_H	1
#else
#if __NeXT__
#define HAVE_SGTTY_H	1
#endif
#endif
#endif /* defined(unix) && (SCREEN_LINES > 0) */

#if HAVE_SGTTY_H
#include <sgtty.h>
#else /* NOT HAVE_SGTTY_H */
#if HAVE_TERMIO_H
#include <sys/termio.h>
#else /* NOT HAVE_TERMIO_H */
#if HAVE_TERMIOS_H
#include <sys/termios.h>
#endif /* HAVE_TERMIOS_H */
#endif /* HAVE_TERMIO_H */
#endif /* HAVE_SGTTY_H */
#endif /* defined(unix) */

#if __sgi
#if __cplusplus
extern "C" {
#endif /* __cplusplus */
int ioctl(int, int, ...);
#if __cplusplus
};
#endif /* __cplusplus */
#endif /* __sgi */

int get_screen_lines(void)
{
    int lines = SCREEN_LINES;

#if defined(unix)
#if defined(TIOCGWINSZ)
    struct winsize window_size;

    (void)ioctl((int)(fileno(stdin)),(int)TIOCGWINSZ,&window_size);
    if (window_size.ws_row > 0)
        lines = (int)window_size.ws_row;
#else /* defined(TIOCGWINSZ) */
    /* some systems store screen lines in environment variables  */
    char *p;

    if (((p = getenv("ROWS")) != (char*)NULL) ||
	((p = getenv("LINES")) != (char*)NULL))
	lines = (int)atoi(p);
#endif /* defined(TIOCGWINSZ) */
#endif /* defined(unix) */

#if defined(vms)
    short flags;
    short dvtype;
    short ncols;
    short nrows = 0;

#if defined(__ALPHA)
    /* I don't know what the OpenVMS replacement for lib$screen_info() is yet */
    ncols = 80;
    nrows = 24;
#else
    (void)lib$screen_info(&flags,&dvtype,&ncols,&nrows);
#endif
    lines = (int)((nrows > 0) ? nrows : SCREEN_LINES);
#endif /* defined(vms) */

    return (lines);
}

/* ============================== CACHE ============================ *\

   In the interest of saving memory, starting with version 2.6,
   index lists are now cached.  The cache is a priority queue,
   implemented as a simple binary heap, stored in an array.  The cache
   contains pointers to the real lists, and each list contains its
   rank in the heap.  The heap is prioritized by least recent use.

   Making the rank field an unsigned char almost certainly doesn't
   save anything, since the data will almost certainly have to be
   word- or long-aligned.  [2.6 [NHFB]: change rank type to int, and
   reorder struct to have largest items first, because of
   memory-alignment requirements of many architectures.]

\* ================================================================= */

#define CACHESIZE 127

typedef struct
{
    long          offset;	/* offset into index file  */
    Index_t       length;	/* length of the list	   */
    Index_t      *list; 	/* actual list or NULL     */
    int		  rank; 	/* back pointer into cache */
} CachedList;

typedef struct
{
    long	stamp;	/* time stamp */
    CachedList *clist;  /* the cached list */
} CacheElement;

CacheElement cache[CACHESIZE];
long curstamp;		/* current "time stamp" */
int cachenum;		/* number of elements in the cache */
char cachefile[FILENAME_MAX+1];	/* name of the index file */
			/* (Should I just keep it open all the time?) */

/* ----------------------------------------------------------------- *\
|  void InitCache(VOID)
|
|  Initialize the cache; remember the file name.
\* ----------------------------------------------------------------- */
void InitCache(VOID)
{
    int i;

    for (i=0; i<CACHESIZE; i++)
    {
	cache[i].stamp = -1;
	cache[i].clist = NULL;
    }
    curstamp = 0;
}

/* ----------------------------------------------------------------- *\
|  void InitCachedList(CachedList *clist, FILE *ifp)
|
|  Initialize the cached list.  Read the length of the list from the
|  file, record the position of the list within the file, and skip
|  over the list itself.
\* ----------------------------------------------------------------- */
void InitCachedList(CachedList *clist, FILE *ifp)
{
    Index_t num;

    clist->list = NULL;

    safefread((void *) &num, sizeof(Index_t), 1, ifp);
    ConvertToHostOrder(1, sizeof (Index_t), &num);
    clist->length = num;

    clist->offset = ftell(ifp);
    if (fseek(ifp, (long)(num*sizeof(Index_t)), SEEK_CUR) != 0)
	pdie("Error reading", bixfile);
}

/* ----------------------------------------------------------------- *\
|  void FreeCache(void)
|
|  Free everything in the cache.
\* ----------------------------------------------------------------- */
void FreeCache(VOID)
{
    int i;

    for (i=0; i<cachenum; i++)
	free(cache[i].clist->list);
}

/* ----------------------------------------------------------------- *\
|  void CheckStamp(void)
|
|  Avoid emacs's timestamp wraparound bug!!
\* ----------------------------------------------------------------- */
void CheckStamp(VOID)
{
    int i;

    if (curstamp > 32767)
    {
	for (i=0; i<cachenum; i++)
	    cache[i].stamp = i;		/* Changes time stamp order! */
	curstamp = cachenum;
	(void)printf("You've been running biblook a long time, haven't you?\n");
    }
}

/* ----------------------------------------------------------------- *\
|  void HeapBubble(int which)
|
|  Bubble the given element down into its place in the heap.
\* ----------------------------------------------------------------- */
void HeapBubble(int which)
{
    register int i;
    CacheElement tmp;

    i = which;
    tmp = cache[i];

    while (2*i+1 < cachenum)		/* while I still have children */
    {
	if ((2*i+2 == cachenum) ||
	    (cache[2*i+1].stamp < cache[2*i+2].stamp))
	{
	    cache[i] = cache[2*i+1];	/* go to left child */
	    cache[i].clist->rank = i;
	    i = 2*i+1;
	}
	else
	{
	    cache[i] = cache[2*i+2];	/* go to right child */
	    cache[i].clist->rank = i;
	    i = 2*i+2;
	}
    }

    cache[i] = tmp;
    cache[i].clist->rank = i;
}

/* ----------------------------------------------------------------- *\
|  void Access(CachedList *clist)
|
|  Make sure clist is in memory.  Make sure the given element is in
|  its rightful place in the cache.  If it's already there, just move
|  it.  Otherwise, insert it into the heap, deleting the oldest
|  element if the cache is already full.
\* ----------------------------------------------------------------- */
void Access(CachedList *clist)
{
    FILE *ifp;

    if (clist->list == NULL)
    {
	ifp = fopen(bixfile, "r");
	if (!ifp)
	    pdie("Can't open", bixfile);

	if (fseek(ifp, clist->offset, SEEK_SET) != 0)
	    pdie("Error reading", bixfile);

	clist->list =
	    (Index_t *) safemalloc(clist->length * sizeof(Index_t),
				   "Can't allocate index list.", "");

	safefread((void *)clist->list, sizeof(Index_t),
		  clist->length, ifp);
	ConvertToHostOrder(clist->length, sizeof(Index_t),
			   clist->list);
	fclose(ifp);

	if (cachenum == CACHESIZE)		/* if cache is full... */
	{
	    free(cache[0].clist->list);		/* delete oldest element */
	    cache[0].clist->list = NULL;

	    cache[0] = cache[CACHESIZE-1];
	    cache[CACHESIZE-1].clist = NULL;

	    cachenum--;
	    HeapBubble(0);
	}

	cache[cachenum].stamp = curstamp++;
	cache[cachenum].clist = clist;
	clist->rank = cachenum++;
    }
    else
    {
	cache[clist->rank].stamp = curstamp++;
	HeapBubble(clist->rank);
    }

    CheckStamp();
}


/* ========================== INDEX TABLES ========================= */

typedef struct
{
    Word       theword;
    CachedList refs;
} Index, *IndexPtr;

typedef struct
{
    Word     thefield;
    Index_t  numwords;
    IndexPtr words;
} IndexTable;

unsigned char numfields;
IndexTable *fieldtable;

Index_t numabbrevs;
Word *abbrevs;
Index_t *abbrevlocs;

Index_t numoffsets;
long *offsets;

/* ----------------------------------------------------------------- *\
|  void ReadWord(FILE *ifp, Word *word)
|
|  Read a "pascal" string into the given buffer
\* ----------------------------------------------------------------- */
void ReadWord(FILE *ifp, Word *word)
{
    unsigned char length;

    safefread((void *) &length, sizeof(length), 1, ifp);
    if (length > MAXWORD)
	die("Index file is corrupt", "(word too long).");

    *word = (Word)safemalloc((size_t)length + 1,
				"Can't allocate space for new word","");


    safefread((void *)*word, sizeof(char), length, ifp);
    (*word)[length] = 0;

#if DEBUG
    (void)printf("word:%02d = [%s]\n", (int)length, (const char*)*word);
#endif /* DEBUG */

}

/* ----------------------------------------------------------------- *\
|  void GetOneTable(FILE *ifp, IndexTable *table)
|
|  Get one index table from the file
\* ----------------------------------------------------------------- */
void GetOneTable(FILE *ifp, register IndexTable *table)
{
    Index_t i;

    safefread((void *) &table->numwords, sizeof(Index_t), 1, ifp);
    ConvertToHostOrder(1, sizeof (Index_t), &table->numwords);
    table->words =
	(IndexPtr) safemalloc(table->numwords*sizeof(Index),
			      "Can't create index table for",
			      table->thefield);

    for (i=0; i<table->numwords; i++)
    {
	ReadWord(ifp, &table->words[i].theword);
	InitCachedList(&(table->words[i].refs), ifp);
    }
}

/* ----------------------------------------------------------------- *\
|  void GetTables(VOID)
|
|  Get the tables from the index file.
\* ----------------------------------------------------------------- */
void GetTables(VOID)
{
    int version, i;
    Index_t k;
    FILE *ifp;

    ifp = fopen(bixfile, "r");
    if (!ifp)
	pdie("Can't open", bixfile);
    if (fscanf(ifp, "bibindex %d %*[^\n]%*c", &version) < 1)
	die(bixfile, "is not a bibindex file!");

#if defined(USE_MEMIO)
    /* Because the memio package doesn't have fscanf(), we must reposition */
    /* the  memory-mapped file explicitly, using parenthesized (ftell) to */
    /* get the original one. */
    fseek(ifp,(long)(ftell)(ifp),SEEK_SET);
#endif /* defined(USE_MEMIO) */

    if (version < FILE_VERSION)
	die(bixfile, "is the wrong version.\n\tPlease rerun bibindex.");
    if (version > FILE_VERSION)
	die(bixfile, "is the wrong version.\n\tPlease recompile biblook.");

    InitCache();

    safefread((void *) &numoffsets, sizeof(Index_t), 1, ifp);
    ConvertToHostOrder(1, sizeof(Index_t), &numoffsets);
    offsets = (long *) safemalloc(numoffsets * sizeof(long),
				  "Can't create offset table", "");

    safefread((void *) offsets, sizeof(long), numoffsets, ifp);
    ConvertToHostOrder(numoffsets, sizeof(long), offsets);

    safefread((void *) &numfields, sizeof(numfields), 1, ifp);
    fieldtable = (IndexTable *) safemalloc(numfields * sizeof(IndexTable),
					   "Can't create field table", "");

    for (i=0; i < (int)numfields; i++)
	ReadWord(ifp, &fieldtable[i].thefield);
    for (i=0; i < (int)numfields; i++)
	GetOneTable(ifp, fieldtable+i);

    safefread((void *) &numabbrevs, sizeof(Index_t), 1, ifp);
    ConvertToHostOrder(1, sizeof(Index_t), &numabbrevs);

    abbrevs = (Word *) safemalloc(numabbrevs * sizeof(Word),
				  "Can't create abbreviation table", "");
    for (k=0; k<numabbrevs; k++)
	ReadWord(ifp, &abbrevs[k]);

    abbrevlocs =
	(Index_t *) safemalloc(numabbrevs * sizeof(Index_t),
			       "Can't create abbrev offset table", "");
    safefread((void *) abbrevlocs, sizeof(Index_t), numabbrevs, ifp);
    ConvertToHostOrder(numabbrevs, sizeof(Index_t), abbrevlocs);

    fclose(ifp);
}

/* ----------------------------------------------------------------- *\
|  void FreeTables(void)
|
|  Free the index tables.
\* ----------------------------------------------------------------- */
void FreeTables(VOID)
{
    register int i;

    FreeCache();	/* free all index lists in memory */

    for (i=0; i < (int)numfields; i++)
	free(fieldtable[i].words);

    free(fieldtable);
    free(offsets);
}


/* ----------------------------------------------------------------- *\
|  Index_t FindIndex(IndexTable table, char *word, char prefix)
|
|  Find the index of a word in a table.  Return INDEX_NAN if the word
|  isn't there.  If prefix is true, return the index of the first
|  matching word.
\* ----------------------------------------------------------------- */
Index_t FindIndex(IndexTable table, char *word, char prefix)
{
    register IndexPtr words = table.words;
    register int hi, lo, mid;	/* must be signed */
    register int cmp;

    hi = table.numwords-1;
    lo = 0;

    while (hi>=lo)
    {
	mid = (hi+lo)/2;
	cmp = strcmp(word, (const char*)words[mid].theword);

	if (cmp == 0)
	    return (Index_t)mid;
	else if (cmp < 0)
	    hi = mid-1;
	else if (cmp > 0)
	    lo = mid+1;
    }

    if (prefix && !strncmp(word, (const char*)words[lo].theword, strlen(word)))
	return (Index_t)lo;
    else
	return (Index_t)INDEX_NAN;
}


/* ----------------------------------------------------------------- *\
|  Index_t FindAbbrev(char *word)
|
|  Find the index of an abbrev in the abbrev table.  Return INDEX_NAN
|  if the abbrev isn't there.
\* ----------------------------------------------------------------- */
Index_t FindAbbrev(register char *word)
{
    register int hi, lo, mid;	/* must be signed */
    register int cmp;

    hi = numabbrevs-1;
    lo = 0;

    while (hi>=lo)
    {
	mid = (hi+lo)/2;
	cmp = strcmp(word, (const char*)abbrevs[mid]);

	if (cmp == 0)
	    return (Index_t)mid;
	else if (cmp < 0)
	    hi = mid-1;
	else if (cmp > 0)
	    lo = mid+1;
    }

    return (Index_t)INDEX_NAN;
}


/* =================== SET MANIPULATION ROUTINES =================== */

#define SETSCALE (sizeof(unsigned long)*8)

static Index_t setsize;
static unsigned long setmask;	   /* used to erase extra bits */
typedef unsigned long *Set;

/* ----------------------------------------------------------------- *\
|  Set NewSet(void)
|
|  Get a new variable to hold sets of integers in the range
|  [0, numoffsets].  Set setsize and setmask.
\* ----------------------------------------------------------------- */
Set NewSet(VOID)
{
    setsize = (numoffsets + SETSCALE - 1)/SETSCALE;	/* HACK */
    setmask = (1<<(numoffsets%SETSCALE)) - 1;		/* KLUDGE */

    return (Set) safemalloc(setsize * SETSCALE,
			    "Can't create new result list", "");
}

/* ----------------------------------------------------------------- *\
|  void EmptySet(Set theset)
|
|  Empty the set.
\* ----------------------------------------------------------------- */
void EmptySet(Set theset)
{
    register Index_t i;
    for (i=0; i<setsize; i++)
	theset[i] = 0L;
}

/* ----------------------------------------------------------------- *\
|  void SetUnion(Set src1, Set src2, Set result)
|
|  Get the union of two sets
\* ----------------------------------------------------------------- */
void SetUnion(Set src1, Set src2, Set result)
{
    register Index_t i;
    for (i=0; i<setsize; i++)
	result[i] = src1[i] | src2[i];
}

/* ----------------------------------------------------------------- *\
|  void SetIntersection(Set src1, Set src2, Set result)
|
|  Get the intersection of two sets
\* ----------------------------------------------------------------- */
void SetIntersection(Set src1, Set src2, Set result)
{
    register Index_t i;
    for (i=0; i<setsize; i++)
	result[i] = src1[i] & src2[i];
}

/* ----------------------------------------------------------------- *\
|  void SetComplement(Set src, Set result)
|
|  Get the complement of a set
\* ----------------------------------------------------------------- */
void SetComplement(Set src, Set result)
{
    register Index_t i;
    for (i=0; i<setsize; i++)
	result[i] = ~src[i];
    /* Bug fixed at version 2.5: setmask == 0 if numoffsets is  */
    /* a multiple of setsize, so the set contains no partial words, */
    /* The clearing of trailing bits must then be omitted. */
    if (setmask)
	result[setsize-1] &= setmask;	/* clear those last few bits */
}

/* ----------------------------------------------------------------- *\
|  void CopySet(Set src, Set result)
|
|  Copy one set into another
\* ----------------------------------------------------------------- */
void CopySet(Set src, Set result)
{
    register Index_t i;
    for (i=0; i<setsize; i++)
	result[i] = src[i];
}


/* ----------------------------------------------------------------- *\
|  int CountSet(Set theset)
|
|  Get the cardinality of the set
\* ----------------------------------------------------------------- */
int CountSet(Set theset)
{
    register int i, j, count;

    count = 0;
    for (i=0; i<(int)setsize; i++)
	for (j=0; j<(int)SETSCALE; j++)
	    if (theset[i] & (1<<j))
		count++;

    return count;
}

/* ----------------------------------------------------------------- *\
|  void BuildSet(Set theset, Index_t *thelist, Index_t length)
|
|  Build a set out of a list of integers
\* ----------------------------------------------------------------- */
void BuildSet(Set theset, Index_t *thelist, Index_t length)
{
    register unsigned i;

    EmptySet(theset);
    for (i=0; i<(unsigned)length; i++)
	theset[thelist[i]/SETSCALE] |= 1 << (thelist[i] % SETSCALE);
}

/* ----------------------------------------------------------------- *\
|  void DoForSet(Set theset, void (*action)(int, void *), void *arg)
|
|  Do something to every element in a set
\* ----------------------------------------------------------------- */
void DoForSet(Set theset, void (*action)(int, void *), void *arg)
{
    register int i, j;

    for (i=0; i<(int)setsize; i++)
	for (j=0; j<(int)SETSCALE; j++)
	    if (theset[i] & (1<<j))
		(*action)((int)(SETSCALE*i + j), arg);
}


/* ======================== SEARCH ROUTINES ======================== */

Set results, oldresults, oneword, onefield;
short firstfield, lastfield;		/* indices into fieldtable */

/* ----------------------------------------------------------------- *\
|  void InitSearch(void)
|
|  Initialize the search lists
\* ----------------------------------------------------------------- */
void InitSearch(VOID)
{
    results = NewSet();
    oldresults = NewSet();
    oneword = NewSet();
    onefield = NewSet();
    firstfield = lastfield = -1;
}

/* ----------------------------------------------------------------- *\
|  void FreeSearch(void)
|
|  Free the search list
\* ----------------------------------------------------------------- */
void FreeSearch(VOID)
{
    free(results);
    free(oldresults);
    free(oneword);
    free(onefield);
}

/* ----------------------------------------------------------------- *\
|  void ClearResults(void)
|
|  Clear the current and old results
\* ----------------------------------------------------------------- */
void ClearResults(VOID)
{
    EmptySet(results);
    SetComplement(results, results);
    CopySet(results, oldresults);
}

/* ----------------------------------------------------------------- *\
|  void SaveResults(void)
|
|  Save and clear the current results
\* ----------------------------------------------------------------- */
void SaveResults(VOID)
{
    CopySet(results, oldresults);
    EmptySet(results);
    SetComplement(results, results);
}

/* ----------------------------------------------------------------- *\
|  void CombineResults(char invert, char intersect)
|
|  Combine current results with old results
\* ----------------------------------------------------------------- */
void CombineResults(char invert, char intersect)
{
    if (invert)
	SetComplement(results, results);
    if (intersect)
	SetIntersection(results, oldresults, results);
    else
	SetUnion(results, oldresults, results);
}


/* ----------------------------------------------------------------- *\
|  char SetUpField(char *field)
|
|  Set up the search fields.  Return the number of searchable fields.
\* ----------------------------------------------------------------- */
char SetUpField(char *field)
{
    int i, len;

    firstfield = -1;
    len = strlen(field);

    for (i=0; i < (int)numfields; i++)
    {
	if (!strncmp(field, (const char*)fieldtable[i].thefield, len))
	{
	    if (firstfield == -1)
		firstfield = i;
	    lastfield = i;
	}
    }

    if (firstfield == -1)
    {
	(void)printf("\tNo searchable fields matching \"%s\".\n", field);
	return 0;
    }
    else
	return lastfield - firstfield + 1;

}

const char *badwords[] = BADWORDS;
/* ----------------------------------------------------------------- *\
|  void FindWord(char *word, char prefix)
|
|  Find a word in the currently active field and update `results'.
|  If the prefix flag is set, find all words having the given prefix.
\* ----------------------------------------------------------------- */
void FindWord(register char *word, char prefix)
{
    register IndexPtr words;
    Index_t win, len;
    int i;

    if (!prefix)
    {
	if (!word[0])
	{
	    (void)printf("\t[ignoring empty string]\n");
	    return;
	}
	if (!word[1])
	{
	    (void)printf("\t[ignoring single letter \"%s\"]\n", word);
	    return;
	}
	for (i=0; badwords[i]; i++)
	{
	    if (!strcmp(badwords[i], word))
	    {
		(void)printf("\t[ignoring common word \"%s\"]\n", word);
		return;
	    }
	}
    }

    EmptySet(oneword);
    len = strlen(word);

    for (i=firstfield; i<=lastfield; i++)
    {
	words = fieldtable[i].words;
	win = FindIndex(fieldtable[i], word, prefix);

	if (win != INDEX_NAN)
	{
	    if (prefix)
	    {
		while ((win < fieldtable[i].numwords) &&
		       !strncmp((const char*)words[win].theword, word, len))
		{
		    Access(&(words[win].refs));
		    BuildSet(onefield, words[win].refs.list,
			     words[win].refs.length);
		    SetUnion(oneword, onefield, oneword);
		    win++;
		}
	    }
	    else
	    {
		Access(&(words[win].refs));
		BuildSet(onefield, words[win].refs.list,
			 words[win].refs.length);
		SetUnion(oneword, onefield, oneword);
	    }
	}
    }

    SetIntersection(oneword, results, results);
}


/* ============================= OUTPUT ============================ */
FILE *bibfp;

/* ----------------------------------------------------------------- *\
|  void ReportResults(void)
|
|  Report the results of the previous search.
\* ----------------------------------------------------------------- */
void ReportResults(VOID)
{
    int numresults;

    numresults = CountSet(results);

    if (numresults == 0)
	(void)printf("\tNo matches found.\n");
    else if (numresults == 1)
	(void)printf("\t1 match found.\n");
    else
	(void)printf("\t%d matches found.\n", numresults);
}

/* ----------------------------------------------------------------- *\
|  void PrintEntry(int entry, FILE *ofp)
|
|  Print the entry.
\* ----------------------------------------------------------------- */
void PrintEntry(int entry, FILE *ofp)
{
    char ch;
    char braces;
    char quotes;

    if (entry >= (int)numoffsets)	/* extra bits might be set */
	return;

    putc('\n', ofp);
    if (fseek(bibfp, offsets[entry], 0))
	die("Index file is corrupt.", "");

    ch = safegetc(bibfp);

    while (ch != '@')
    {
	putc(ch, ofp);
	ch = safegetc(bibfp);
    }
	while ((ch != '{') && (ch != '('))
    {
	putc(ch, ofp);
	ch = safegetc(bibfp);
    }

    braces = quotes = 0;

    putc(ch, ofp);
    ch = safegetc(bibfp);
    while (braces || quotes || ((ch != '}') && (ch != ')')))
    {
	if (ch == '{')
	    braces++;
	else if (ch == '}')
	    braces--;
	else if ((ch == '"') && !braces)
	    quotes = !quotes;
	putc(ch, ofp);
	ch = safegetc(bibfp);
    }

    putc(ch, ofp);
    putc('\n', ofp);
}

/* ----------------------------------------------------------------- *\
|  void PrintResults(char *filename)
|
|  Print the current search results into the given file.  If the
|  filename is NULL, pipe the output through $PAGER.
\* ----------------------------------------------------------------- */
void PrintResults(char *filename)
{
    int numresults;
    FILE *ofp;
    char *pager;
    char *the_tmpfile = (char*)NULL;
    int childpid;

    numresults = CountSet(results);
    if (numresults == 0)
	(void)printf("\tNothing to display!\n");
    else if (numresults > MAXRESULTS)
	(void)printf("\tI can't display that many results!\n");
    else
    {
	if (filename)
	{
	    ofp = (fopen)(filename, "a"); /* need original fopen */
	    if (!ofp)
	    {
		(void)printf("\tCan't open %s: ", filename);
		perror(NULL);
		return;
	    }
	}
	else
	{
	    the_tmpfile = (char*)tempnam(NULL, "bibl.");
	    ofp = (fopen)(the_tmpfile, "w"); /* need original fopen */
	    if (!ofp)
	    {
		perror("\tCan't open temp file");
		return;
	    }
	}

	if (filename)
	{
	    time_t now = time(0);
	    (void)fprintf(ofp, "%% Retrieved by biblook %d.%d at %s",
		    MAJOR_VERSION, MINOR_VERSION, ctime(&now));
	}

	DoForSet(results, (void (*)(int, void *)) PrintEntry,
		 (void *) ofp);

	fflush(ofp);	/* Apparent Solaris 2.2 bug: last buffer lost
			without this fflush() */
	fclose(ofp);
	if (filename)
	    (void)printf("\tResults saved in \"%s\"\n", filename);
	else
	{
	    pager = (char*)getenv("PAGER");
#if vms
	    {
	        char message[11 + FILENAME_MAX + 1];
		(void)sprintf(message, "%s %s", 
			      (pager == (char*)NULL) ? "type /page" : pager,
			      the_tmpfile);
		system(message);
	    }
#else /* NOT vms */
	    if ((childpid = (int)fork()) != 0)
		waitpid(childpid, (int *) 0, 0);
	    else if (pager)
	    {
		execlp(pager, pager, the_tmpfile, (char *) 0);
		perror(pager);		/* should never get here! */
		exit(EXIT_SUCCESS);
	    }
	    else
	    {
		/* try absolute path first */
		execl(MOREPATH, MORE, the_tmpfile, (char *) 0);
		/* next try to find it in PATH list */
		execlp(MORE, MORE, the_tmpfile, (char *) 0);
		/* no pager available, so give up */
		perror(MOREPATH);
		exit(EXIT_SUCCESS);
	    }
#endif /* vms */

	    unlink(the_tmpfile);
	    free(the_tmpfile);	    	/* malloc'ed by tempnam() */
	    putchar('\n');
	}
    }
}

/* ----------------------------------------------------------------- *\
|  void DisplayAbbrev(char *theabbrev)
|
|  Display the definition of the given abbreviation.
\* ----------------------------------------------------------------- */
void DisplayAbbrev(char *theabbrev)
{
    Index_t the_index = FindAbbrev(theabbrev);

    if (the_index == INDEX_NAN)
	(void)printf("\tThe abbreviation \"%s\" is not defined.\n", theabbrev);
    else if (abbrevlocs[the_index] == INDEX_BUILTIN)
	(void)printf("\tThe abbreviation \"%s\" is builtin.\n", theabbrev);
    else
    {
	PrintEntry((int)abbrevlocs[the_index], stdout);
	putchar('\n');
    }
}


/* ======================== USER INTERFACE ========================= */

typedef enum {
    T_Find, T_Whatis, T_Display, T_Save, T_Quit, T_Word,
    T_And, T_Or, T_Not, T_Semi, T_Return, T_Help
} Token;

/* ----------------------------------------------------------------- *\
|  Token GetToken(char *tokenstr)
|
|  Get the next input token.
\* ----------------------------------------------------------------- */
Token GetToken(char *tokenstr)
{
    static char line[MAX_TEXTBUF];
    static short pos;
    static char neednew = 1;
    short tlen = 0;

    *tokenstr = 0;

    if (neednew)
    {
	(void)printf("biblook: ");
	if (!fgets(line, sizeof(line)-2, stdin)) /* leave 2 slots empty */
	    return T_Quit;

	pos = 0;
	neednew = 0;
    }

    while ((line[pos] == ' ') || (line[pos] == '\t'))
	pos++;

    switch (line[pos])
    {
	case '\n':
	    pos++;
	    neednew = 1;
	    return T_Return;

	case '&':
	    pos++;
	    return T_And;

	case '|':
	    pos++;
	    return T_Or;

	case '~':
	case '!':
	    pos++;
	    return T_Not;

	case ';':
	    pos++;
	    return T_Semi;

	case '?':
	    pos++;
	    return T_Help;

	default:
	    tokenstr[tlen++] = tolower(line[pos++]);
	    while (!isspace(line[pos]) && (line[pos] != ';') &&
		   (line[pos] != '&') && (line[pos] != '|'))
	    {
		tokenstr[tlen++] = tolower(line[pos++]);
	    }
	    tokenstr[tlen] = 0;

	    /* I really ought to use a hash table here. */

	    if (!strncmp(tokenstr, "find", tlen))
		return T_Find;
	    else if (!strncmp(tokenstr, "display", tlen))
		return T_Display;
	    else if (!strncmp(tokenstr, "help", tlen))
		return T_Help;
	    else if (!strncmp(tokenstr, "save", tlen))
		return T_Save;
	    else if (!strncmp(tokenstr, "whatis", tlen))
		return T_Whatis;
	    else if (!strncmp(tokenstr, "quit", tlen))
		return T_Quit;
	    else if (!strcmp(tokenstr, "and"))
		return T_And;
	    else if (!strcmp(tokenstr, "or"))
		return T_Or;
	    else if (!strcmp(tokenstr, "not"))
		return T_Not;
	    else
		return T_Word;
    }
}


/* ----------------------------------------------------------------- *\
|  char Strip(char *string)
|
|  Strip all but alphanumeric characters out of the string.  Return
|  true if the original string ended with the prefix character '*'.
\* ----------------------------------------------------------------- */
char Strip(char *string)
{
    char prefix = 0;
    char *src = string;

    while (*src)
    {
	prefix = (*src == '*');
	if (isalnum(*src))
	    *string++ = *src;
	src++;
    }
    *string = 0;
    return prefix;
}

/* ----------------------------------------------------------------- *\
|  void CmdError(void)
|
|  Print syntax error message
\* ----------------------------------------------------------------- */
void CmdError(VOID)
{
    (void)printf("\t?? Syntax error ??\n");
}

static const char* shorthelplines[] =
{
    "------------------------------------------------------------",
    "help			Print this message",
    "find <field> <words>	Find entries with <words> in <field>",
    "and  <field> <words>	Narrow search",
    "or   <field> <words>	Widen search",
    "display			Display search results",
    "save <file>		Save search results to <file>",
    "whatis <abbrev>		Find and display an abbreviation",
    "quit			Quit biblook",
    "------------------------------------------------------------",
    "Type `help' or `?' again for more details.",
    (const char *) NULL,
};

static const char* longhelplines[] =
{
    "biblook permits rapid lookup in a BibTeX bibliography data",
    "base, using a compact binary index file prepared by bibindex(1).",
    "",
    "Available commands:",
    "? or h[elp]",
    "     Display this help message.",
    "",
    "f[ind] [not] <field> <words>",
    "     Find the entries containing the given words in any",
    "     field with a prefix matching the <field> argument.  For",
    "     example, `a' matches both `author' and `address', and",
    "     `au' matches `author' only.  If the <field> argument is",
    "     `-' (or any string with no letters or numbers), match",
    "     any field.",
    "",
    "     If `not' appears before the <field>, the sense of the",
    "     search is reversed.  The symbols `~' and `!' can be",
    "     used in place of `not'.",
    "",
    "     Each word is a contiguous sequence of letters and",
    "     digits.  Case is ignored; accents should be omitted;",
    "     apostrophes are not required.  Single characters and a",
    "     few common words are also ignored.  Any word ending",
    "     with an asterisk is treated as a prefix.  Thus,",
    "     `point*' matches `point', `points', `pointer', etc.",
    "",
    "and [not] <field> <words>",
    "or [not] <field> <words>",
    "     Intersect (resp. union) the results of the given search",
    "     with the previous search.  Several of these commands",
    "     may be combined on a single line.  Commands are handled",
    "     in the order in which they appear; there is no pre-",
    "     cedence.  Unlike other commands, and like `not', these",
    "     must be spelled out completely.  `&' can be used in",
    "     place of `and', and `|' can be used in place of `or'.",
    "",
    "d[isplay]",
    "     Display the results of the previous search.",
    "",
    "s[ave] [<filename>]",
    "     Save the results of the previous results into the",
    "     specified file.  If <filename> is omitted, the previous",
    "     save file is used.  If no save file has ever been",
    "     specified, results are saved in the file specified on",
    "     the command line.  If no such file is specified,",
    "     `save.bib' is used.  If the save file exists, results",
    "     are appended to it.",
    "",
    "w[hatis] <abbrev>",
    "     Display the definition of the abbreviation <abbrev>.",
    "",
    "q[uit]/EOF",
    "     Quit.",
    "",
    "Several commands can be combined on a single line by",
    "separating them with semicolons.  For example, the following",
    "command displays all STOC papers cowritten by Erdo\"s",
    "without `Voronoi diagrams' in the title:",
    "",
    "f b stoc* | b symp* theory comp* & au erdos & ~t voronoi diagrams ; d",
    "",
    (const char*)NULL,
};

/* ----------------------------------------------------------------- *\
|  void GiveHelp(int verbose)
|
|  Print a help message.  Lines are stored as separate strings to
|  avoid hitting compiler limits.
\* ----------------------------------------------------------------- */
void GiveHelp(int verbose)
{
    int c;
    int k;
    int lines;
    int done = 0;
#if 0
    /* This fails on IBM RS/6000 AIX 3.2 cc with bogus error:
       "Operands must be pointers to compatible types." */
    const char **help = verbose ? &longhelplines[0] : &shorthelplines[0];
#else
    const char **help;

    if (verbose)
        help = &longhelplines[0];
    else
	help = &shorthelplines[0];
#endif
    lines = get_screen_lines() - 1;
    if (lines <= 0)
	lines = INT_MAX;
    for (k = 0; help[k] && !done; ++k)
    {
	(void)printf("\t%s\n", help[k]);
	if (((k + 1) % lines) == 0)
	{
	    (void)printf("more? (n to stop) ");
	    (void)fflush(stdout);
	    while ((c = getchar()) != '\n')
	    {
		if ((c == 'n') || (c == 'N'))
		    done = 1;
	    }
	}
    }
}

/* ----------------------------------------------------------------- *\
|  States for Lookup()
\* ----------------------------------------------------------------- */
typedef enum {
    Wait,	/* nothing yet 			*/
    Find,	/* "find"			*/
    FindN,	/* "find not"			*/
    FindF,	/* "find [not] <field>"		*/
    FindW,	/* "find [not] <field> <words>"	*/
    Display,	/* "display"			*/
    Save,	/* "save"			*/
    SaveF,	/* "save <file>"		*/
    Whatis,	/* "whatis"			*/
    WhatisA,	/* "whatis <abbrev>"		*/
    Help,	/* "help"			*/
    Error	/* anything else		*/
} CmdState;

/* ----------------------------------------------------------------- *\
|  void Lookup(const char *defsave)
|
|  Execute commands until the user quits.  Defsave is the default
|  save file name.  This is one big finite state machine.  It's long
|  and boring, but that's interface code for ya!
\* ----------------------------------------------------------------- */
void Lookup(const char *defsave)
{
    char tokenstr[MAX_TEXTBUF];
    char savestr[MAX_TEXTBUF];
    CmdState state = Wait;
    static CmdState last_state = Wait;

    Token thetoken;
    char intersect = 1;		/* 1 = intersect, 0 = union */
    char invert = 0;		/* 1 = invert */
    char prefix;		/* 1 = word is really a prefix */

    ClearResults();
    strcpy(savestr, defsave);

    for (;;)
    {
	thetoken = GetToken(tokenstr);

	if ((thetoken == T_Quit) && !tokenstr[0])
	    return;

	switch (state)
	{
	    case Wait:
		switch (thetoken)
		{
		    case T_Quit:
			return;
		    case T_Find:
			state = Find;
			invert = 0;
			ClearResults();
			break;
		    case T_And:
			state = Find;
			invert = 0;
			SaveResults();
			break;
		    case T_Or:
			state = Find;
			invert = 0;
			intersect = 0;
			SaveResults();
			break;
		    case T_Display:
			state = Display;
			break;
		    case T_Save:
			state = Save;
			break;
		    case T_Help:
			state = Help;
			break;
		    case T_Whatis:
			state = Whatis;
			break;
		    case T_Return:
		    case T_Semi:
			break;
		    default:
			state = Error;
			CmdError();
			break;
		}
		break;

	    case Find:
		if (thetoken == T_Not)
		{
		    last_state = state;
		    state = FindN;
		    invert = 1;
		}
		else
		{
		    if (tokenstr[0])
		    {
			state = FindF;
			Strip(tokenstr);
			if (!SetUpField(tokenstr))
			    state = Error;
			else
			    last_state = Find;
		    }
		    else
		    {
			state = (thetoken == T_Return) ? Wait : Error;
			CmdError();
		    }
		}
		break;

	    case FindN:
		if (tokenstr[0])
		{
		    state = FindF;
		    Strip(tokenstr);
		    if (!SetUpField(tokenstr))
			state = Error;
		    else
			last_state = FindN;

		}
		else
		{
		    state = (thetoken == T_Return) ? Wait : Error;
		    CmdError();
		}
		break;

	    case FindF:
		if (tokenstr[0])
		{
		    last_state = state;
		    state = FindW;
		    prefix = Strip(tokenstr);
		    FindWord(tokenstr, prefix);
		}
		else
		{
		    state = (thetoken == T_Return) ? Wait : Error;
		    CmdError();
		}
		break;

	    case FindW:
		switch (thetoken)
		{
		    case T_And:
		        last_state = state;
			state = Find;
			CombineResults(invert, intersect);
			SaveResults();
			invert = 0;
			intersect = 1;
			break;
		    case T_Or:
			last_state = state;
			state = Find;
			CombineResults(invert, intersect);
			SaveResults();
			invert = 0;
			intersect = 0;
			break;
		    case T_Semi:
			last_state = state;
			state = Wait;
			CombineResults(invert, intersect);
			invert = 0;
			intersect = 1;
			break;
		    case T_Return:
			last_state = state;
			state = Wait;
			CombineResults(invert, intersect);
			ReportResults();
			invert = 0;
			intersect = 1;
			break;
		    default:
			if (tokenstr[0])
			{
			    last_state = state;
			    state = FindW;
			    prefix = Strip(tokenstr);
			    FindWord(tokenstr, prefix);
			}
			else
			{
			    state = Error;
			    CmdError();
			}
			break;
		}
		break;

	    case Display:
		if ((thetoken == T_Semi) || (thetoken == T_Return))
		{
		    last_state = state;
		    state = Wait;
		    PrintResults(NULL);
		}
		else
		{
		    state = Error;
		    CmdError();
		}
		break;

	    case Save:
		if (tokenstr[0])
		{
		    last_state = state;
		    state = SaveF;
		    strcpy(savestr, tokenstr);
		}
		else if ((thetoken == T_Semi) || (thetoken == T_Return))
		{
		    state = Wait;
		    PrintResults(savestr);
		}
		else
		{
		    state = Error;
		    CmdError();
		}
		break;

	    case SaveF:
		if ((thetoken == T_Semi) || (thetoken == T_Return))
		{
		    last_state = state;
		    state = Wait;
		    PrintResults(savestr);
		}
		else
		{
		    state = Error;
		    CmdError();
		}
		break;

	    case Whatis:
		if (tokenstr[0])
		{
		    last_state = state;
		    state = WhatisA;
		    strcpy(savestr, tokenstr);
		}
		else
		{
		    state = (thetoken == T_Return) ? Wait : Error;
		    CmdError();
		}
		break;

	    case WhatisA:
		if ((thetoken == T_Semi) || (thetoken == T_Return))
		{
		    last_state = state;
		    state = Wait;
		    DisplayAbbrev(savestr);
		}
		else
		{
		    state = Error;
		    CmdError();
		}
		break;

	    case Help:
		if ((thetoken == T_Semi) || (thetoken == T_Return))
		{
		    state = Wait;
		    GiveHelp(last_state == Help);
		    last_state = Help;
		}
		break;

	    case Error:
		switch (thetoken)
		{
		    case T_Quit:
			return;
		    case T_Return:
			state = Wait;
			break;
		    default:
			break;
		}
		break;
	}				/* end switch(state) */
    }					/* end for(;;) */
}


/* ================================================================= *\
|  The main program
\* ================================================================= */
int main(int argc, char **argv)
{
    register char *path, *tmp;
    char gotit;
    struct stat bibstat, bixstat;
    char *p;

    (void)printf("biblook version %d.%d  file version %d\n",
	   (int)MAJOR_VERSION, (int)MINOR_VERSION, (int)FILE_VERSION);
    (void)printf("Type ? or h for help\n");

    if ((argc != 2) && (argc != 3))
    {
	(void)fprintf(stderr, "Usage: biblook bib [savefile]\n");
	exit(EXIT_FAILURE);
    }

    if (((p = strrchr(argv[1],'.')) != (char*)NULL) &&
	((strcmp(p, ".bib") == 0) || (strcmp(p, ".bix") == 0)))
    {
	*p = '\0';		/* remove any .bib or .bix extension */
    }

    /* ---- Search BIBLOOKPATH or BIBINPUTS for the files ---- */

    path = (char *)getenv("BIBLOOKPATH");
    if (path == NULL)
	path = (char *)getenv("BIBINPUTS");
    if ((path == NULL) || (argv[1][0] == '/'))
    {
	(void)sprintf(bibfile, "%s.bib", argv[1]);
	(void)sprintf(bixfile, "%s.bix", argv[1]);
    }
    else
    {
	tmp = path;
	gotit = 0;

	while ((tmp != NULL) && !gotit)
	{
	    tmp = strchr(path, ':');
	    if (tmp != NULL)
		*tmp = 0;

	    if (strcmp(path, "."))
	    {
		(void)sprintf(bibfile, "%s/%s.bib", path, argv[1]);
		(void)sprintf(bixfile, "%s/%s.bix", path, argv[1]);
	    }
	    else
	    {
		(void)sprintf(bibfile, "%s.bib", argv[1]);
		(void)sprintf(bixfile, "%s.bix", argv[1]);
	    }

	    if (stat(bibfile, &bibstat) != 0)
	    {
		if (errno != ENOENT)
		    pdie("Can't open", bibfile);
	    }
	    else
		gotit = 1;

	    if (tmp != NULL)
		path = tmp + 1;
	}

	if (!gotit)
	{
	    (void)sprintf(bibfile, "%s.bib", argv[1]);
	    pdie("Can't find", bibfile);
	}
    }

    /* ---- Now that we've found the files, open them and do the job ---- */

    if (stat(bibfile, &bibstat) != 0)
	pdie("Can't open", bibfile);

    if (stat(bixfile, &bixstat) != 0)
	pdie("Can't open", bixfile);

    if (bibstat.st_mtime > bixstat.st_mtime)
	die(bixfile, "is out of date.\n\tPlease rerun bibindex.");

    bibfp = fopen(bibfile, "r");
    if (!bibfp)
	pdie("Can't read", bibfile);

    GetTables();
    InitSearch();

    if (argc == 3)
	Lookup(argv[2]);
    else
	Lookup("save.bib");

    FreeSearch();
    FreeTables();

    fclose(bibfp);
    exit(EXIT_SUCCESS);
    return(0);
}
