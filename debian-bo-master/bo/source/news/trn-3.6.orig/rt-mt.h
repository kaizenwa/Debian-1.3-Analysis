/* $Id: rt-mt.h,v 3.0 1992/12/14 00:14:09 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

bool mt_init _((void));
bool mt_data _((void));

/* Stuff internal to rt-mt.c */

#ifdef DOINIT

#define DB_VERSION	2

typedef char		BYTE;
typedef short		WORD;
#ifndef __alpha
typedef long		LONG;
#else
typedef int		LONG;
#endif

#define ROOT_ARTICLE	0x0001		/* article flag definitions */
#define HAS_XREFS	0x0004		/* article has an xref line */

typedef struct {
    LONG root_num;
    WORD articles;
    WORD article_cnt;
    WORD subject_cnt;
    WORD pad_hack;
} PACKED_ROOT;

typedef struct {
    LONG num;
    LONG date;
    WORD subject, author;
    WORD flags;
    WORD child_cnt;
    WORD parent;
    WORD padding;
    WORD sibling;
    WORD root;
} PACKED_ARTICLE;

typedef struct Total {
    LONG first, last;
    LONG string1;
    LONG string2;
    WORD root;
    WORD article;
    WORD subject;
    WORD author;
    WORD domain;
    WORD pad_hack;
} TOTAL;

typedef struct {
    BYTE l[sizeof (LONG)];
    BYTE w[sizeof (WORD)];
    BYTE version;
    BYTE pad_hack;
} BMAP;

# ifndef USE_XTHREAD
static char *mt_name _((char*));
# endif
static int read_authors _((void));
static int read_subjects _((void));
static int read_roots _((void));
static int read_articles _((void));
static int read_ids _((void));
static void tweak_data _((void));
static int read_item();
static void safefree _((char**));
static void mybytemap _((BMAP*));
static void wp_bmap(), lp_bmap();

#endif
