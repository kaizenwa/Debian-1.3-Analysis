/* -*- mode: C; mode: fold; -*- */
#include "config.h"
#include "jed-feat.h"
/*{{{ Include Files */

#include <stdio.h>
#include <string.h>

#include "buffer.h"
#include "abbrev.h"
#include "text.h"
#include "ledit.h"
#include "ins.h"
#include "cmds.h"
#include "misc.h"


/*}}}*/

#if JED_HAS_ABBREVS
/*{{{ Static Variables */

#ifdef pc_system
#define MAX_ABBREVS 48
#else 
#define MAX_ABBREVS 256
#endif

#define MAX_ABBREV_LEN 48
#define MAX_ABBREV_TABLES 10
typedef struct Abbrev_Table_Type /*{{{*/
{
   int len;			       /* current length of abbrev */
   int empty;			       /* number of cell known to be empty */
   char abbrevs [MAX_ABBREVS][MAX_ABBREV_LEN];
				       /* formatted as abbrev\0expansion */
   char word_chars[256];	       /* word delimiters */
   char name[16];		       /* name of table */
}

/*}}}*/

Abbrev_Table_Type;

static Abbrev_Table_Type *Global_Abbrev_Table;
static Abbrev_Table_Type *Abbrev_Tables [MAX_ABBREV_TABLES];

/*}}}*/

int expand_abbrev (unsigned char ch) /*{{{*/
{
   Abbrev_Table_Type *tbl;
   int len, i;
   unsigned char *p;
   char *abbrev;
   
   if ((i = CBuf->abbrev_table_handle) == -1) tbl = NULL;
   else tbl = Abbrev_Tables [i];

   if (tbl == NULL) 
     {
	tbl = Global_Abbrev_Table;
	if (tbl == NULL) return 0;
     }
   
   if (tbl->len == 0) return 0;
   
   
   if (tbl->word_chars[(int) ch]) return 0;   /* not a delimiter */

   p = CLine->data + (Point - 1);
   
   while ((p >= CLine->data) && (tbl->word_chars[(int) *p]))
     {
	p--;
     }
   p++;
   
   len = (int) ((CLine->data + Point) - p);
   if ((len == 0) || (len >= MAX_ABBREV_LEN / 2)) return 0;
   /* It makes no sense to have an abbrev larger than its expansion! */
   
   ch = *p;
   for (i = 0; i < tbl->len; i++)
     {
	abbrev = tbl->abbrevs [i];
	if ((*abbrev == (char) ch) && (abbrev[len] == 0) && 
	    (0 == strncmp (abbrev, (char *) p, len)))
	  {
	     Point -= len;
	     abbrev += len + 1;
	     deln (&len);
	     len = strlen (abbrev);
	     if (CBuf->flags & OVERWRITE_MODE)
	       {
		  deln (&len);	       /* this does not delete across lines */
	       }
	     ins_chars ((unsigned char *) abbrev, len);
	     return 1;
	  }
     }
   return 0;
}

/*}}}*/

static Abbrev_Table_Type *find_table (char *name, int *loc, int err) /*{{{*/
{
   Abbrev_Table_Type **tbl, **max;
   Abbrev_Table_Type **empty_tbl = NULL;
   
   tbl = Abbrev_Tables;
   max = tbl + MAX_ABBREV_TABLES;
   
   while (tbl < max)
     {
	if (*tbl == NULL)
	  {
	     if (empty_tbl == NULL) empty_tbl = tbl;
	  }
	else if (strcmp (name, (*tbl)->name) == 0)
	  {
	     break;
	  }
	tbl++;
     }
   *loc = (int) (tbl - Abbrev_Tables);
   if (tbl == max) 
     {
	if (empty_tbl != NULL) *loc = (int) (empty_tbl - Abbrev_Tables);
	if (err) msg_error ("Table does not exist.");
	return NULL;
     }
   return *tbl;
}

/*}}}*/

void create_abbrev_table (char *name, char *word_chars) /*{{{*/
{
   Abbrev_Table_Type *tbl;
   int loc;
   
   tbl = find_table (name, &loc, 0);
   if (tbl == NULL)
     {
	if (loc == MAX_ABBREV_TABLES) 
	  {
	     msg_error ("Abbrev Table Quota reached.");
	     return;
	  }
	
	tbl = (Abbrev_Table_Type *) SLMALLOC (sizeof (Abbrev_Table_Type));
	if (tbl == NULL)
	  {
	     msg_error ("Malloc error.");
	     return;
	  }
	
	Abbrev_Tables [loc] = tbl;
     }
   
   SLMEMSET ((char *) tbl, 0, sizeof (Abbrev_Table_Type));
   
   if (*word_chars == 0) word_chars = Jed_Word_Range;
   
   SLmake_lut ((unsigned char *) tbl->word_chars, (unsigned char *) word_chars, 0);
   
   strncpy (tbl->name, name, 15);      /* The null char is here because of
					* the MEMSET
					*/
   
   if (strcmp (name, "Global") == 0) Global_Abbrev_Table = tbl;
}

/*}}}*/

void delete_abbrev_table (char *name) /*{{{*/
{
   Abbrev_Table_Type *tbl;
   int n;
   Buffer *b;
   
   tbl = find_table (name, &n, 1);
   if (tbl == NULL) return;
   
   SLFREE (tbl);
   Abbrev_Tables[n] = NULL;
   if (tbl == Global_Abbrev_Table) Global_Abbrev_Table = NULL;
   b = CBuf;
   do
     {
	if (b->abbrev_table_handle == n) b->abbrev_table_handle = -1;
	b = b->next;
     }
   while (b != CBuf);
}

/*}}}*/

void define_abbrev (char *table, char *abbrev, char *expans) /*{{{*/
{
   Abbrev_Table_Type *tbl;
   int len, lena;
   int i;
   char *a;
   
   tbl = find_table (table, &len, 1);
   if (tbl == NULL) return;
   
   if (tbl->len == MAX_ABBREVS) return; /* silently return */
   
   lena = strlen (abbrev);
   len = lena + strlen (expans) + 1;
   
   if (len >= MAX_ABBREV_LEN)
     {
	msg_error ("Expansion is too long.");
     }
   
   for (i = 0; i < MAX_ABBREVS; i++)
     {
	a = tbl->abbrevs[i];
	if ((*a == 0) 
	    || ((a[lena] == 0) && (0 == strcmp (a, abbrev))))
	  {
	     strcpy (a, abbrev);
	     strcpy (a + lena + 1, expans);
	     tbl->len++;
	     return;
	  }
     }
}

/*}}}*/

void use_abbrev_table (char *name) /*{{{*/
{
   Abbrev_Table_Type *tbl;
   int loc;
   
   tbl = find_table (name, &loc, 1);
   if (tbl == NULL) return;
   CBuf->abbrev_table_handle = loc;
}

/*}}}*/

int abbrev_table_p (char *name) /*{{{*/
{
   int loc;
   if (find_table (name, &loc, 0) != NULL) return 1;
   return 0;
}

/*}}}*/

static void push_word (Abbrev_Table_Type *tbl) /*{{{*/
{
   char buf[256], *b, *w;
   int i, in_range;
   
   b = buf;
   w = tbl->word_chars;
   
   if (w[(unsigned char) '-'] != 0) *b++ = '-';
   in_range = 0;
   
   for (i = 33; i < 256; i++)
     {
	if ((i != '-') && (0 != w[i]))
	  {
	     if (i == '\\') *b++ = (char) i;
	     *b = (char) i;
	     if (in_range == 0) 
	       {
		  *(b + 1) = '-';
		  b += 2;  *b = 0;
		  in_range = 1;
	       }
	  }
	else 
	  {
	     if (in_range) 
	       {
		  if (*b == 0) b--; else b++;
		  in_range = 0;
	       }
	  }
     }
   *b = 0;
   SLang_push_string (buf);
}

/*}}}*/

void what_abbrev_table (void) /*{{{*/
{
   Abbrev_Table_Type *tbl;
   int i;
   char *w = "";
   
   
   if ((i = CBuf->abbrev_table_handle) == -1) tbl = NULL;
   else tbl = Abbrev_Tables [i];
   
   if (tbl == NULL) tbl = Global_Abbrev_Table;
   if (tbl == NULL)
     {
	SLang_push_string (w);
	SLang_push_string (w);
     }
   else 
     {
	SLang_push_string (tbl->name);
	push_word (tbl);
     }
}

/*}}}*/

void dump_abbrev_table (char *name) /*{{{*/
{
   Abbrev_Table_Type *tbl;
   char *abbrev;
   int i, len;
      
   if (NULL == (tbl = find_table (name, &len, 1))) return;
   
   len = tbl->len;
   for (i = 0; i < len; i++)
     {
	abbrev = tbl->abbrevs [i];
	if (*abbrev)
	  {
	     insert_string (abbrev);
	     ins ('\t');
	     insert_string (abbrev + (1 + strlen (abbrev)));
	     newline ();
	  }
     }
   push_word (tbl);
}

/*}}}*/

int list_abbrev_tables (void) /*{{{*/
{
   int n = 0;	     
   Abbrev_Table_Type **tbl, **max;
   
   tbl = Abbrev_Tables;
   max = tbl + MAX_ABBREV_TABLES;
   
   while ((tbl < max) && (*tbl != NULL))
     {
	n++;
	SLang_push_string ((*tbl)->name);
	tbl++;
     }

   return n;
}

/*}}}*/
#endif   /* JED_HAS_ABBREVS */
