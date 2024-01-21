/*
	System: Structured text retrieval tool sgrep.
	Module: defines.h
	Author: Pekka Kilpeläinen & Jani Jaakkola
	Description: Common data structures, definitions & macros for
		     all modules.
	Version history: Original version February 1995 by JJ & PK
	Copyright: University of Helsinki, Dept. of Computer Science
		   Distributed under GNU General Public Lisence
		   See file COPYING for details
*/

#include "config.h"

/*
 * If this is defined we try to optimize away some sort operations 
 */
#define OPTIMIZE_SORTS

/* 
 * This turns on debugging output for all modules. You really don't want
 * to that. Instead define it in the place which you are debugging.
 */
/*#define DEBUG*/

#ifdef ASSERT
#include <assert.h>
#endif

#define TRUE 1
#define FALSE 0

/* These are the constant labels */
#define LABEL_NOTKNOWN -1
#define LABEL_START 0
#define LABEL_END 1
#define LABEL_CONS 2
#define LABEL_CHARS 3
#define LABEL_PHRASE 4
#define LABEL_FIRST 5

#define GC_NODE_BITS 7		/* Size of a gc node in bits */

/* Sice GC_NODE_SIZE must be power of 2 it's calculated from GC_NODEBITS
 * as follows
 */
#define GC_NODE_SIZE ( 1 << GC_NODE_BITS )

/*
 * All operators. These are used in the parse tree 
 */
enum opers { IN,NOT_IN,CONTAINING,NOT_CONTAINING,OR,
		ORDERED,L_ORDERED,R_ORDERED,LR_ORDERED,
		EXTRACTING,
		QUOTE,L_QUOTE,R_QUOTE,LR_QUOTE,
		EQUAL, NOT_EQUAL,		/* PK Febr 12 '96 */
		OUTER,INNER,CONCAT,JOIN,PHRASE,INVALID };

/* 
 * These are used to identify reserved words. The order must be the same as
 * in r_word_str in parser.c
 */
enum symbols { W_IN,W_NOT,W_CONTAINING,W_OR,
	W_ORDERED,W_L_ORDERED,W_R_ORDERED,W_LR_ORDERED,
	W_EXTRACTING,
	W_QUOTE,W_L_QUOTE,W_R_QUOTE,W_LR_QUOTE,
	W_EQUAL,		/* PK Febr 12 '96 */
	W_OUTER,W_INNER,W_CONCAT,W_START,W_LAST,W_CHARS,
	W_JOIN,
	R_WORDS, /* This is the number of reserved words */
/* 
 * In the command line can also be '('..')' , '['..']' , a phrase ( "phrase" ) ,
 * number or it might just end 
 */
	W_LPAREN,W_RPAREN,W_LBRACK,W_RBRACK,W_COMMA,W_PHRASE,W_NUMBER,W_END };

/* 
 * Struct for non 0 terminating strings. ( which means we need length ) 
 * We need these for phrases with zero characters in them
 * ( Won't work yet (maybe never. Dont think it's really useful))
 */
struct STR_STRUCT {
#ifdef ASSERT
	int size;
#endif
	int length;
	unsigned char s[1]; /* Phrases may be 8-bit chars! */
};

typedef struct STR_STRUCT string;

/*
 * One region has a start point and a end point 
 */
struct REGION {
	int start;
	int end;
};

/* 
 * One gc node has table for regions and a pointers to next and previous
 * nodes 
 */
struct GC_NODE {
	struct REGION list[GC_NODE_SIZE];
	struct GC_NODE *next;
	struct GC_NODE *prev;
};

/*
 * A pointer to a GC_NODE in a gc list. Used for scanning gc lists.
 */
struct GC_POINTER {
	struct GC_LIST *list;	/* gc_list, which we are scanning */
	struct GC_NODE *node;	/* Points out the node */
	int ind;		/* Index into a node */
};

/*
 * Structure for whole gc list 
 */
struct GC_LIST {
	struct GC_NODE *first; 		/* First node in the gc list
					   Null means we have an optimized list
					   ( chars have been used ) */
	int chars;			/* When we have chars list, this
					   tells from how many characters
					   it is created */
	struct GC_NODE *last;		/* Last node */
	int nodes;			/* how many nodes there are */
	int length;			/* How many regions in last node */
	int end_sorted;			/* If this list is sorted by regions
					   end points */
	int sorted;			/* Is this list sorted or not */
#ifdef OPTIMIZE_SORTS
	int nested;			/* This list maybe nested */
#endif
	int refcount;			/* How many times this list is referenced */
	struct GC_LIST *next;		/* We may need to make lists out of gc lists */
};

/*
 * Leaves of parse tree are always phrases. 
 */
struct PHRASE_NODE {
	struct PHRASE_NODE *next;	/* Phrase nodes are kept in list for
					   the creation of AC automate */
	string *phrase;			/* The phrase string */ 
	struct TREE_NODE *parent;
	struct GC_LIST *GC_list;	/* gc_list of this phrase */ 
};

/*
 * Node of a parse tree 
 */
struct TREE_NODE {
	struct TREE_NODE *left;		/* left subtree */
	struct TREE_NODE *right;	/* right subtree */
	struct TREE_NODE *parent;	/* parent node */
	int oper;			/* operand. see enum opers above */
	int number;			/* Functions may have int parameters */
	struct PHRASE_NODE *leaf;	/* Points to PHRASE_node if this is
					   a leaf */
	int label_left;			/* Needed for optimizing */
	int label_right;		/* ditto */
	int refcount;			/* How many times this node is referenced in the tree */
	struct GC_LIST *GC_list;	/* This nodes gc_list */
};

/*
 * There is one of these for every input file 
 */
struct INPUT_FILE {
	int start;	/* Start index of a file */
	int length;	/* Length of a file */
	char *name;	/* Name of the file, NULL if stdin */
};

/*
 * Struct for gathering statistical information 
 */
struct STATS {
	int e_mallocs;		/* How much memory was allocated */
	int phrases;		/* How many phrases found */
	int order;		/* How many operations */
	int or;
	int in;
	int not_in;
	int equal;		/* PK Febr 12, '96 */
	int not_equal;		/* PK Febr 12, '96 */
	int containing;
	int not_containing;	
	int extracting;
	int quote;
	int inner;
	int outer;
	int concat;
	int join;
	int gc_lists;		/* Number of gc lists used */
	int constant_lists;	/* How many gc_lists were constant */
	int gc_lists_allocated;	/* Number of gc lista malloced */
	int gc_nodes;		/* Number of gc_nodes used */
	int gc_nodes_allocated; /* Number of gc_nodes malloced */
	int regions;		/* Number of regions created */
	int longest_list;	/* Longest gc list used */
	int output;		/* Size of output list */
	int scans;		/* Number of started scans */
	int scanned_regions;	/* How many regions were scanned */
	int sorts_by_start;	/* Number of sorts by start points */
	int sorts_by_end;	/* Number of sorts by end points */
#ifdef OPTIMIZE_SORTS
	int sorts_optimized;	/* How many sorts we could optimize away */
#endif
	int remove_duplicates;	/* Number of remove_duplicates operations */
	int reallocs;		/* how many times memory has been reallocated */
	int inner_tablesize;	/* What is the size of inner table now */
	int nest_stacksize;	/* What is the size of nesting stack */
	int skipped_phrases;	/* How many times we had same phrase */			   
	int tree_size;		/* Parse tree size */
	int opt_nodes;		/* How many parse tree nodes optimized */
	int input_size;		/* Size of given input in bytes */
};

/* 
 * Some handy macros
 */

/*
 * Macro for finding out gc list size 
 */
#define LIST_SIZE(LIST)	(((LIST)->nodes-1)*GC_NODE_SIZE+(LIST)->length)

/*
 * Macro for input size. This could actually be removed
 */
#define LAST (end_list->first->list[0].end)

/* 
 * These are for speeding up list scanning and creation.
 * get_region, add_region and prev_region are the most used
 * functions.
 */
#ifdef NO_MACROS
#define add_region(L,S,E)	do_add_region(L,S,E)
#define get_region(P,R)	do_get_region(P,R)
#define prev_region(P,R)	do_prev_region(P,R)
#else 
#define get_region(handle,reg)	\
do { \
	stats.scanned_regions++; \
	if ( (handle)->node==(handle)->list->last ) \
	{ \
		if ((handle)->ind==(handle)->list->length) \
		{ \
			(reg)->start=-1; \
			(reg)->end=-1; \
			break; \
		} \
	 	if ((handle)->list->last==NULL) \
		{ \
			(reg)->start=(handle)->ind; \
			(reg)->end=(handle)->ind+(handle)->list->chars; \
			(handle)->ind++; \
			break; \
		} \
	} \
	if ( (handle)->ind==GC_NODE_SIZE ) \
	{ \
		(handle)->node=(handle)->node->next; \
		(handle)->ind=0; \
	} \
	*(reg)=(handle)->node->list[(handle)->ind++]; \
} while(0)

#define prev_region(handle,reg) \
do { \
	stats.scanned_regions++; \
	if ( (handle)->node==(handle)->list->first ) \
	{ \
		if ((handle)->ind==0) \
		{ \
			(reg)->start=-1; \
			(reg)->end=-1; \
			break; \
		} \
		if ((handle)->list->first==NULL) \
		{ \
			(handle)->ind--; \
			(reg)->start=(handle)->ind; \
			(reg)->end=(reg)->start+(handle)->list->chars; \
			break; \
		} \
	} \
	if ( (handle)->ind==0 ) \
	{ \
		(handle)->node=(handle)->node->prev; \
		(handle)->ind=GC_NODE_SIZE; \
	} \
	*(reg)=(handle)->node->list[--(handle)->ind]; \
} while(0)
#define add_region(L,S,E)	do { \
 		if ( (L)->length==GC_NODE_SIZE ) \
 			do_add_region(L,S,E); \
 		else \
 		{ \
 			(L)->last->list[(L)->length].start=(S); \
 			(L)->last->list[(L)->length].end=(E); \
 			(L)->length++; \
 		} \
 		stats.regions++; \
 	} while (0)
#endif

/*
 * Global variables for all modules
 */
extern char *output_style;		/* String containing the output style
					   default is DEFAULT_OUTPUT */
extern int open_failure;		/* So if file that can't be opened
					   is considered to be fatal
					   defaults to OPEN_FAILURE (above)*/
extern struct STATS stats;		/* here we gather statistical
					   information */
extern int print_newline;		/* Shall we print newline at the end of output */
extern int stdin_fd;			/* This is the file descriptor of temp
					   file, if we have one */
extern int print_all;			/* If sgrep is used as a filter */
extern int progress_output;		/* Shall we print progression output ? */
extern int gc_lists_now;		/* We keep track of how many gc_lists 
					   we are using at a time */
extern char *r_word_str[];		/* Names of operators (constant from parser.c) */
extern int stream_mode; 	         /* Input files considered a stream (-S) */
extern int ignore_case;                  /* Ignore case distinctions in phrases */

/* The constant lists, start, end and chars */
extern struct GC_LIST *end_list;               
extern struct GC_LIST *start_list;                       
extern struct GC_LIST *chars_list; 

/*
 * global function prototypes 
 */

void *e_malloc(size_t size);
void *e_realloc(void *, size_t, size_t);
struct TREE_NODE *parse_string(const char *,struct PHRASE_NODE **);
void ACsearch(struct PHRASE_NODE *,struct INPUT_FILE *,int);
string *new_string(int);
string *init_string(int size,char *src);
struct GC_LIST* new_gclist();
void do_add_region(struct GC_LIST *, int s, int e);
void start_region_search(struct GC_LIST *, struct GC_POINTER *);
void do_get_region(struct GC_POINTER *, struct REGION *);
void do_prev_region(struct GC_POINTER *, struct REGION *);
void free_gclist(struct GC_LIST *);
void show_gc_list(struct GC_LIST *, struct INPUT_FILE *,int);
struct GC_LIST *eval(struct TREE_NODE *);
struct GC_LIST *sort_by_end(struct GC_LIST *);
struct GC_LIST *sort_by_start(struct GC_LIST *);
void remove_duplicates(struct GC_LIST *);
int preprocess(char *,char *,char *,int);
void to_chars(struct GC_LIST *,int);
void optimize_tree(struct TREE_NODE **, struct PHRASE_NODE **);
const char* give_oper_name(int oper);
