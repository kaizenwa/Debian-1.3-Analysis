/* -*- mode: C; mode: fold -*- */

/*  Copyright (c) 1995, 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"
#include "slrnfeat.h"

/*{{{ Include files */

#include <stdio.h>
#include <string.h>
#include <time.h>
#ifndef VMS
# include <sys/types.h>
# include <sys/stat.h>
#else
# include "vms.h"
#endif

#include <signal.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif


#include <slang.h>
#include "jdmacros.h"

#include "slrn.h"
#include "group.h"
#include "art.h"
#include "misc.h"
#include "post.h"
#include "server.h"
#include "hash.h"
#include "score.h"
#include "menu.h"
#include "util.h"
#include "startup.h"

/*}}}*/

/*{{{ Global Variables */

int Slrn_Query_Group_Cutoff = 100;
int Slrn_Groups_Dirty;	       /* greater than 0 if need to write newsrc */
int Slrn_List_Active_File = 0;
int Slrn_Use_Xgtitle = 0;
int Slrn_Write_Newsrc_Flags = 0;       /* if 1, do not save unsubscribed 
					* if 2, do not save new unsubscribed.
					*/

int Slrn_Group_Description_Column = 40;/* column where group descr start */
char *Slrn_Group_Help_Line;

SLscroll_Window_Type Slrn_Group_Window;
Slrn_Group_Type *Slrn_Group_Current_Group;
static Slrn_Group_Type *Groups;

int Slrn_Group_Display_Descriptions = 1;
int Slrn_No_Backups = 0;
int Slrn_Prompt_Next_Group = 1;

int Slrn_Unsubscribe_New_Groups = 0;

SLKeyMap_List_Type *Slrn_Group_Keymap;
int *Slrn_Prefix_Arg_Ptr;

/*}}}*/
/*{{{ Static Variables */

#define GROUP_HASH_TABLE_SIZE 1250
static Slrn_Group_Type *Group_Hash_Table [GROUP_HASH_TABLE_SIZE];

static unsigned int Last_Cursor_Row;
static int Groups_Hidden;	       /* if true, hide groups with no arts */

typedef struct Unsubscribed_Slrn_Group_Type
{
   char name[MAX_GROUP_NAME_LEN + 1];
   struct Unsubscribed_Slrn_Group_Type *next;
}
Unsubscribed_Slrn_Group_Type;

static Unsubscribed_Slrn_Group_Type *Unsubscribed_Groups;

/*}}}*/

/*{{{ Forward Function Declarations */

static void group_update_screen (void);
static void group_quick_help (void);
static void save_newsrc_cmd (void);

/*}}}*/

/*{{{ Functions that deal with Group Range */


/* Note: This routine is NOT very robust.  It assumes that this function
 * is called in an ordered way such that the implied range is always increasing.
 * This is why the range is re-built in art.c:update_ranges.  Yes, it is ugly.
 * See also slrn_group_mark_article_as_read for something more random.
 */
void slrn_add_group_ranges (Slrn_Group_Type *g, int min, int max) /*{{{*/
{
   Slrn_Range_Type *r, *next;
   int unread;
   
   if ((max < min) || (g == NULL)) return;
   
   /* The first one is range of articles on server so expand max to cover
    * the range of articles nolonger available.
    */
   next = &g->range;
   if (max < next->min) 
     {
	/* If we have already expanded the range up to range currently available
	 * at server and we are now trying to add another range below available
	 * range, do not bother.
	 */
	if (next->next != NULL) return;
	max = next->min - 1;
     }
   
   /* Count number unread */
   unread = next->max;
   while (next->next != NULL)
     {
	next = next->next;
	unread -= next->max - next->min + 1;
     }
   
   /* check to see if a merge is possible */
   if ((min <= next->max + 1)
       && (next != &g->range))
     {
	next->max = max;
     }
   else
     {
	r = (Slrn_Range_Type *) slrn_safe_malloc (sizeof(Slrn_Range_Type));
	
	r->next = next->next;
	next->next = r;
	r->prev = next;
	
	r->min = min;
	r->max = max;
	
	/* For this case, min should be 1 */
	if (next == &g->range)
	  {
	     min = r->min = 1;
	  }
     }
   
   unread -= max - min + 1;
   
   if (unread < 0) unread = 0;
   g->unread = unread;
   Slrn_Groups_Dirty = 1;
}

/*}}}*/

static void group_mark_article_as_read (Slrn_Group_Type *g, long num) /*{{{*/
{
   Slrn_Range_Type *r, *r1, *newr;
   
   r1 = &g->range;
   if (r1->max < num)  /* not at server yet so update our data */
     {
	r1->max = num;
	g->unread += 1;
     }
   
   r = r1->next;
   
   while (r != NULL)
     {
	/* Already read */
	if ((num <= r->max) && (num >= r->min)) return;
	if (num < r->min) break;
	r1 = r;
	r = r->next;
     }
   
   if (g->unread > 0) g->unread -= 1;
   Slrn_Groups_Dirty = 1;
   if ((r != NULL) && (r->min == num + 1))
     {
	r->min = num;
	return;
     }
   
   if ((r1->max + 1 == num) && (r1 != &g->range))
     {
	r1->max = num;
	return;
     }
   
   newr = (Slrn_Range_Type *) slrn_safe_malloc (sizeof (Slrn_Range_Type));
   
   newr->min = newr->max = num;
   newr->next = r;
   if (r != NULL) r->prev = newr;
   newr->prev = r1;
   r1->next = newr;
}

/*}}}*/

void slrn_mark_article_as_read (char *group, long num) /*{{{*/
{
   Slrn_Group_Type *g;
   unsigned long hash;
   
   if (group == NULL)
     {
	group_mark_article_as_read (Slrn_Group_Current_Group, num);
	return;
     }
   
   hash = slrn_compute_hash ((unsigned char *) group,
			     (unsigned char *) group + strlen (group));
   
   g = Group_Hash_Table[hash % GROUP_HASH_TABLE_SIZE];
   
   while (g != NULL)
     {
	if ((g->hash == hash) && !strcmp (group, g->name))
	  {
	     /* If it looks like we have read this group, mark it read. */
	     if (((g->flags & GROUP_UNSUBSCRIBED) == 0)
		 || (g->range.next != NULL))
	       group_mark_article_as_read (g, num);
	     
	     break;
	  }
	g = g->hash_next;
     }
}

/*}}}*/

static int group_sync_group_with_server (Slrn_Group_Type *g, int *minp, int *maxp) /*{{{*/
{
   int min, max, n, max_available;
   char *group;
   Slrn_Range_Type *r;
   int status;
   
   if (g == NULL) return -1;
   
   group = g->name;
   
   slrn_message_now ("Selecting %s ...", group);
   
   status = Slrn_Server_Obj->sv_select_group (group, minp, maxp);
   if (status == -1)
     return -1;
   
   if (status != OK_GROUP)
     {
	g->flags &= ~GROUP_UNSUBSCRIBED;
	slrn_error ("This group appears to be bogus.");
	return -1;
     }

   min = *minp;
   max = *maxp;
   
   if (max == 0)
     {
	int nmax, nmin;
	
	nmax = g->range.max;
	nmin = g->range.min;
	
	/* Server database inconsistent. */
	for (n = nmin; n <= nmax; n++)
	  group_mark_article_as_read (g, n);
	
	/* g->unread = 0; */
	slrn_message ("No articles to read.");
	Slrn_Full_Screen_Update = 1;
	Slrn_Groups_Dirty = 1;
	return -1;
     }
   
   g->range.min = min;
   if (max < g->range.max)
     {
	/* There is only one way for this to happen that I am aware of:
	 * an article has been cancelled-- update the ranges.
	 */
	int nmax = g->range.max;
	for (n = max + 1; n <= nmax; n++)
	  group_mark_article_as_read (g, n);
     }
   else g->range.max = max;

   /* In case more articles arrived at the server between the time that 
    * slrn was first started and when the server was just queried, update
    * the ranges of read/unread articles.
    */
   
   max_available = g->range.max - g->range.min + 1;
   
   r = &g->range;
   if (r->next != NULL)
     {
	if (r->next->min <= r->min)
	  r->next->min = 1;
	
	n = r->max;
	while (r->next != NULL)
	  {
	     r = r->next;
	     n -= r->max - r->min + 1;
	  }
	if (n < 0) n = 0;
	if (n > max_available) n = max_available;
	g->unread = n;
     }
   
   return 0;
}



/*}}}*/

static void free_group_ranges (Slrn_Group_Type *g) /*{{{*/
{
   Slrn_Range_Type *r, *rnext;
   
   if (g == NULL) return;
   
   r = g->range.next;
   while (r != NULL)
     {
	rnext = r->next;
	SLFREE (r);
	r = rnext;
     }
   g->range.next = NULL;
}

/*}}}*/

void slrn_catchup_group (void) /*{{{*/
{
   if (Slrn_Group_Current_Group == NULL) return;
   free_group_ranges (Slrn_Group_Current_Group);
   slrn_add_group_ranges (Slrn_Group_Current_Group, 1, Slrn_Group_Current_Group->range.max);
   Slrn_Group_Current_Group->flags |= GROUP_TOUCHED;
}

/*}}}*/

void slrn_uncatchup_group (void) /*{{{*/
{
   if (Slrn_Group_Current_Group == NULL)
     return;
   free_group_ranges (Slrn_Group_Current_Group);
   slrn_add_group_ranges (Slrn_Group_Current_Group, 1, 1);
   Slrn_Group_Current_Group->flags |= GROUP_TOUCHED;
}

/*}}}*/

/*}}}*/

/*{{{ Misc Utility Functions */

static Slrn_Group_Type *find_group_entry (char *name, unsigned int len) /*{{{*/
{
   int hash_index;
   unsigned long hash;
   Slrn_Group_Type *g;
   
   hash = slrn_compute_hash ((unsigned char *) name,
			     (unsigned char *) name + len);
   
   hash_index = hash % GROUP_HASH_TABLE_SIZE;
   g = Group_Hash_Table[hash_index];
   
   while (g != NULL)
     {
	if ((g->hash == hash) && !strncmp (name, g->name, len))
	  {
	     if (len == strlen (g->name)) break;;
	  }
	g = g->hash_next;
     }
   return g;
}

/*}}}*/
static Slrn_Group_Type *create_group_entry (char *name, unsigned int len, /*{{{*/
					    int min, int max, int query_server,
					    int skip_find)
{
   int hash_index;
   unsigned long hash;
   Slrn_Group_Type *g;
   
   if (skip_find == 0)
     {
	g = find_group_entry (name, len);
	if (g != NULL) return g;
     }
   
   g = (Slrn_Group_Type *) slrn_safe_malloc (sizeof (Slrn_Group_Type));

   if (len > MAX_GROUP_NAME_LEN) len = MAX_GROUP_NAME_LEN;
   strncpy (g->name, name, len);
   g->name [len] = 0;
   
   if (query_server)
     {
	int status;
	
	status = Slrn_Server_Obj->sv_select_group (g->name, &min, &max);
	if (status != OK_GROUP)
	  {
	     if (status != -1)
	       {
		  Slrn_Groups_Dirty = 1;
		  slrn_error ("Group %s may be bogus.", g->name);
	       }
	     SLFREE (g);
	     return NULL;
	  }
     }
   g->range.min = min;
   g->range.max = max;
   if (max > 0)
     g->unread = max - min + 1;
   else g->unread = 0;
   
   g->flags = (GROUP_UNSUBSCRIBED | GROUP_HIDDEN);
   
   hash = slrn_compute_hash ((unsigned char *) name,
			     (unsigned char *) name + len);
   hash_index = hash % GROUP_HASH_TABLE_SIZE;
   
   g->hash = hash;
   g->hash_next = Group_Hash_Table[hash_index];
   Group_Hash_Table[hash_index] = g;
   
   if (Groups == NULL)
     {
	Slrn_Group_Current_Group = Groups = g;
     }
   else
     {
	if (Slrn_Group_Current_Group == NULL) Slrn_Group_Current_Group = Groups;
	g->next = Slrn_Group_Current_Group->next;
	if (g->next != NULL) g->next->prev = g;
	Slrn_Group_Current_Group->next = g;
	g->prev = Slrn_Group_Current_Group;
     }
   Slrn_Group_Current_Group = g;
   return g;
}

/*}}}*/
static int add_group (char *name, unsigned int len, /*{{{*/
		      unsigned int subscribe_flag, int create_flag)
{
   char ch;
   Slrn_Group_Type *g;
   
   g = find_group_entry (name, len);
   if (g == NULL)
     {
	if (Slrn_List_Active_File)
	  {
	     char namebuf[MAX_GROUP_NAME_LEN + 1];
	     if (len > MAX_GROUP_NAME_LEN) len = MAX_GROUP_NAME_LEN;
	     strncpy (namebuf, name, len);
	     namebuf[len] = 0;
	     fprintf (stderr, "Group %s is bogus,  Ignoring it.\r\n", namebuf);
	     return -1;
	  }
	else g = create_group_entry (name, len, -1, -1,
				     !(subscribe_flag & GROUP_UNSUBSCRIBED),
				     0);
	if (g == NULL) return -1;
     }
   Slrn_Groups_Dirty = 1;
   
   /* If we have already processed this, then the group is duplicated in
    * the newsrc file.  Throw it out now.
    */
   if (g->flags & GROUP_PROCESSED) return -1;
   
   Slrn_Group_Current_Group = g;
   g->flags = subscribe_flag;
   g->flags |= GROUP_PROCESSED;
   
   if (subscribe_flag & GROUP_UNSUBSCRIBED)
     {
	g->flags |= GROUP_HIDDEN;
	
	if (create_flag) return 0;
	g->unread = 0;
	/* if (Slrn_List_Active_File == 0) return 0; */
     }
   
   if (create_flag) return 0;
   
   /* find ranges for this */
   name += len;			       /* skip past name */
   if (*name) name++;			       /* skip colon */
   while (1)
     {
	int min, max;
	/* skip white space and delimiters */
	while (((ch = *name) != 0) && ((ch <= ' ') || (ch == ','))) name++;
	if ((ch < '0') || (ch > '9')) break;
	min = atoi (name++);
	while (((ch = *name) != 0) && (ch >= '0') && (ch <= '9')) name++;
	if (ch == '-')
	  {
	     name++;
	     max = atoi (name);
	     while (((ch = *name) != 0) && (ch >= '0') && (ch <= '9')) name++;
	  }
	else max = min;
	
	slrn_add_group_ranges (Slrn_Group_Current_Group, min, max);
     }
   return 0;
}

/*}}}*/

static void find_line_num (void) /*{{{*/
{
   Slrn_Group_Window.lines = (SLscroll_Type *) Groups;
   Slrn_Group_Window.current_line = (SLscroll_Type *) Slrn_Group_Current_Group;
   (void) SLscroll_find_line_num (&Slrn_Group_Window);
}

/*}}}*/
static void init_group_win_struct (void) /*{{{*/
{
   Slrn_Group_Window.nrows = SLtt_Screen_Rows - 3;
   Slrn_Group_Window.hidden_mask = GROUP_HIDDEN;
   Slrn_Group_Window.current_line = (SLscroll_Type *) Slrn_Group_Current_Group;
   Slrn_Group_Window.cannot_scroll = SLtt_Term_Cannot_Scroll;
   Slrn_Group_Window.lines = (SLscroll_Type *) Groups;
   Slrn_Group_Window.border = 1;
   if (Slrn_Scroll_By_Page)
     {
	/* Slrn_Group_Window.border = 0; */
	Slrn_Group_Window.cannot_scroll = 2;
     }
   find_line_num ();
}

/*}}}*/
static void free_all_groups (void) /*{{{*/
{
   Slrn_Group_Type *g = Groups;
   Slrn_Group_Type *nextg;
   int i;
   
   while (g != NULL)
     {
	nextg = g->next;
	slrn_free (g->descript);
	free_group_ranges (g);
	SLFREE(g);
	g = nextg;
     }
   Groups = NULL;
   Slrn_Group_Current_Group = NULL;
   SLMEMSET((char *) &Slrn_Group_Window, 0, sizeof (SLscroll_Window_Type));
   
   for (i = 0; i < GROUP_HASH_TABLE_SIZE; i++) Group_Hash_Table[i] = NULL;
}

/*}}}*/

static int find_group (char *name) /*{{{*/
{
   Slrn_Group_Type *g = find_group_entry (name, strlen (name));
   if (g == NULL) return 0;
   
   g->flags &= ~GROUP_HIDDEN;
   Slrn_Group_Current_Group = g;
   find_line_num ();
   return 1;
}

/*}}}*/

static SLRegexp_Type *read_group_regexp (char *prompt, char *origpat) /*{{{*/
{
   static char pattern[256];
   
   if (slrn_read_input (prompt, NULL, pattern, 1) <= 0) return NULL;
   
   if (origpat != NULL)
     strcpy (origpat, pattern);

   return slrn_compile_regexp_pattern (slrn_fix_regexp (pattern));
}

/*}}}*/
static Slrn_Group_Type *process_xgtitle_info (void) /*{{{*/
{
   char buf [NNTP_BUFFER_SIZE];
   Slrn_Group_Type *first = NULL, *save = Slrn_Group_Current_Group;
   
   while (Slrn_Server_Obj->sv_read_line(buf, sizeof(buf)) != NULL)
     {
	char *b, ch;
	unsigned int len;
	Slrn_Group_Type *g;
	
	b = buf;
	while (((ch = *b) != 0)
	       && (ch != ' ') 
	       && (ch != '\n') 
	       && (ch != '\t'))
	  b++;
	
	len = (unsigned int) (b - buf);
	if (len == 0) continue;
	*b = 0;
	
	g = create_group_entry (buf, len,
				-1, -1, 0, 0);

	if (g != NULL)
	  {
	     g->flags &= ~GROUP_HIDDEN;
	     if ((first == NULL) && (g->flags & GROUP_UNSUBSCRIBED))
	       first = g;
	  }
     }
   
   if (save != Slrn_Group_Current_Group)
     {
	Slrn_Group_Current_Group = save;
	find_line_num ();
     }
   return first;
}

/*}}}*/

static void add_unsubscribed_group (char *name) /*{{{*/
{
   Unsubscribed_Slrn_Group_Type *g;
   char *p;
   unsigned int len;
   
   g = (Unsubscribed_Slrn_Group_Type *) slrn_safe_malloc (sizeof (Unsubscribed_Slrn_Group_Type));
   
   g->next = Unsubscribed_Groups;
   Unsubscribed_Groups = g;
   
   p = name;
   while (*p > ' ') p++;
   *p = 0;
   len = p - name;
   
   if (len > MAX_GROUP_NAME_LEN) len = MAX_GROUP_NAME_LEN;
   strncpy (g->name, name, len);
   g->name[len] = 0;
}

/*}}}*/

/*}}}*/

int slrn_group_search (char *str) /*{{{*/
{
   SLsearch_Type st;
   Slrn_Group_Type *g;
   
   g = Slrn_Group_Current_Group;
   if (g == NULL) return 0;
   
   SLsearch_init (str, 1, 0, &st);
   
   do
     {
	g = g->next;
	if (g == NULL)
	  {
	     g = Groups;
	  }
	
	if ((g->flags & GROUP_HIDDEN) == 0)
	  {
	     if ((NULL != SLsearch ((unsigned char *) g->name,
				   (unsigned char *) g->name + strlen (g->name),
				   &st))
		 || ((NULL != g->descript)
		     && (NULL != SLsearch ((unsigned char *) g->descript,
					   (unsigned char *) g->descript + strlen (g->descript),
					   &st))))
		  break;
	  }
     }
   while (g != Slrn_Group_Current_Group);
   
   if (g == Slrn_Group_Current_Group)
     return 0;
   
   Slrn_Group_Current_Group = g;
   find_line_num ();
   return 1;
}

/*}}}*/
unsigned int slrn_group_up_n (unsigned int n) /*{{{*/
{
   n = SLscroll_prev_n (&Slrn_Group_Window, n);
   Slrn_Group_Current_Group = (Slrn_Group_Type *) Slrn_Group_Window.current_line;
   return n;
}

/*}}}*/
unsigned int slrn_group_down_n (unsigned int n) /*{{{*/
{
   n = SLscroll_next_n (&Slrn_Group_Window, n);
   Slrn_Group_Current_Group = (Slrn_Group_Type *) Slrn_Group_Window.current_line;
   return n;
}

/*}}}*/

int slrn_group_select_group (void) /*{{{*/
{
   int min, max, n, max_available;
   int ret;
   Slrn_Range_Type *r;
   int prefix;
   
   if (Slrn_Prefix_Arg_Ptr != NULL)
     {
	prefix = *Slrn_Prefix_Arg_Ptr;
	Slrn_Prefix_Arg_Ptr = NULL;
     }
   else prefix = 0;
   
   if (-1 == group_sync_group_with_server (Slrn_Group_Current_Group, &min, &max))
     return -1;
   
   n = Slrn_Group_Current_Group->unread;
   max_available = Slrn_Group_Current_Group->range.max - Slrn_Group_Current_Group->range.min + 1;

   if ((prefix & 1) || (n == 0))
     n = max_available;

   if ((prefix & 1) || 
       ((n > Slrn_Query_Group_Cutoff)
	&& (Slrn_Query_Group_Cutoff > 0)))
     {
	char int_prompt_buf[256];
	sprintf (int_prompt_buf, "%s: Read how many?", Slrn_Group_Current_Group->name);
	if ((-1 == slrn_read_integer (int_prompt_buf, &n, &n))
	    || (n <= 0))
	  {
	     slrn_clear_message ();
	     Slrn_Full_Screen_Update = 1;
	     return 0;
	  }
	
	if ((0 == (prefix & 1))
	    && (Slrn_Group_Current_Group->unread != 0))
	  {
	     r = Slrn_Group_Current_Group->range.next;
	     if (r != NULL)
	       {
		  while (r->next != NULL) r = r->next;
		  if (r->max + n > max)
		    n = -n;	       /* special treatment in article mode
					* because we will need to query the 
					* server about articles in a group 
					* that we have already read.
					*/
	       }
	  }
     }
   else if ((0 == (prefix & 1)) && (Slrn_Group_Current_Group->unread != 0))
     n = 0;
   
   ret = slrn_select_article_mode (Slrn_Group_Current_Group, n, 
				   ((prefix & 2) == 0));
   
   if (ret == -2) 
     slrn_catchup_group ();
   
   if (ret == 0) return 0;
   return -1;
}

/*}}}*/
void slrn_select_next_group (void) /*{{{*/
{
   if (Slrn_Group_Current_Group == NULL) 
     return;
   
   while ((SLang_Error == 0) && (1 == slrn_group_down_n (1)))
     {
	if (Slrn_Group_Current_Group->unread == 0) 
	  continue;
	
	if (0 == slrn_group_select_group ())
	  break;
     }
}

/*}}}*/
void slrn_select_prev_group (void) /*{{{*/
{
}

/*}}}*/


/*{{{ Interactive commands */

void slrn_group_quit (void) /*{{{*/
{
   if (Slrn_User_Wants_Confirmation
       && (Slrn_Batch == 0)
       && (slrn_get_yesno (1, "Do you really want to quit") <= 0)) return;

   if ((Slrn_Groups_Dirty) && (-1 == slrn_write_newsrc ()))
     {
	if (Slrn_Batch)
	  slrn_quit (1);

	slrn_smg_refresh ();
	if (Slrn_Batch == 0) sleep (2);
	if (slrn_get_yesno (0, "Write to newsrc file failed.  Quit anyway") <= 0)
	  return;
     }
   slrn_quit (0);
}

/*}}}*/

static void group_pagedown (void) /*{{{*/
{
   Slrn_Full_Screen_Update = 1;
   
   if (-1 == SLscroll_pagedown (&Slrn_Group_Window))
     slrn_error ("End of Buffer.");
   Slrn_Group_Current_Group = (Slrn_Group_Type *) Slrn_Group_Window.current_line;
}

/*}}}*/

static void group_pageup (void) /*{{{*/
{
   Slrn_Full_Screen_Update = 1;
   
   if (-1 == SLscroll_pageup (&Slrn_Group_Window))
     slrn_error ("Top of Buffer.");
   
   Slrn_Group_Current_Group = (Slrn_Group_Type *) Slrn_Group_Window.current_line;
}

/*}}}*/

static void group_up (void) /*{{{*/
{
   if (0 == slrn_group_up_n (1))
     {
	slrn_error ("Top of buffer.");
     }
}

/*}}}*/

static void refresh_groups_cmd (void) /*{{{*/
{
   char name[MAX_GROUP_NAME_LEN + 1];
   
   *name = 0;
   if (Slrn_Group_Current_Group != NULL) 
     strcpy (name, Slrn_Group_Current_Group->name);
   
   /* slrn_set_suspension (1); */
   save_newsrc_cmd ();
   free_all_groups ();
   /*
    * Groups_Hidden gets toggled in slrn_read_newsrc, which gets called
    * in slrn_get_new_news.  Therefore, pre-toggle Groups_Hidden ahead of
    * time.
    */
   Groups_Hidden = !Groups_Hidden;
   if (-1 == slrn_get_new_news (1, 0))
     {
	slrn_quit (0);
     }
   if (*name)
     (void) find_group (name);
   /* slrn_set_suspension (0); */

   init_group_win_struct ();
   group_quick_help ();
}

/*}}}*/

static void group_search_cmd (void) /*{{{*/
{
   static char search_str[256];
   Slrn_Group_Type *g;
   unsigned int n;
   
   g = Slrn_Group_Current_Group;
   if (g == NULL) return;
   
   if (slrn_read_input ("Search", search_str, NULL, 1) <= 0) return;
   
   n = Slrn_Group_Window.line_num;
   if (0 == slrn_group_search (search_str))
     {
        slrn_error ("Not found.");
	return;
     }
   
   if (n > Slrn_Group_Window.line_num)
     slrn_message ("Search wrapped.");
}

/*}}}*/

static void add_group_cmd (void) /*{{{*/
{
   char group[256];
   
   *group = 0;
   if (slrn_read_input ("Add group", NULL, group, 1) > 0)
     {
	if (!find_group (group)
	    && (Slrn_List_Active_File == 0))
	  add_group (group, strlen (group), 0, 0);
	Slrn_Groups_Dirty = 1;
	Slrn_Full_Screen_Update = 1;
	find_line_num ();
     }
}

/*}}}*/

static void group_down (void) /*{{{*/
{
   if (1 != slrn_group_down_n (1))
     {
	slrn_error ("End of Buffer.");
     }
}

/*}}}*/

static void transpose_groups (void) /*{{{*/
{
   Slrn_Group_Type *g, *g1, *tmp;
   
   if (NULL == (g = Slrn_Group_Current_Group))
     return;
   
   if (1 != slrn_group_up_n (1))
     return;
   
   g1 = Slrn_Group_Current_Group;
   tmp = g1->next;
   
   g1->next = g->next;
   if (g1->next != NULL) g1->next->prev = g1;
   g->next = tmp;
   tmp->prev = g;		       /* tmp cannot be NULL but it can be 
					* equal to g.  This link is corrected
					* below
					*/
	
   tmp = g1->prev;
   g1->prev = g->prev;
   g1->prev->next = g1;		       /* g1->prev cannot be NULL */
   g->prev = tmp;
   if (tmp != NULL) tmp->next = g;

   if (g1 == Groups) Groups = g;
   
   find_line_num ();
   
   (void) slrn_group_down_n (1);
   
   Slrn_Full_Screen_Update = 1;
   Slrn_Groups_Dirty = 1;
}

/*}}}*/

static void move_group_cmd (void) /*{{{*/
{
   SLang_Key_Type *key;
   void (*f)(void);
   Slrn_Group_Type *g;
   
   if (Slrn_Batch) return;
   if (Slrn_Group_Current_Group == NULL) return;
   g = Slrn_Group_Current_Group;
   
   while (1)
     {
	slrn_message_now ("Moving %s. Press RETURN when finished.", Slrn_Group_Current_Group->name);
	
	key = SLang_do_key (Slrn_Group_Keymap, (int (*)(void)) SLang_getkey);
	
	if ((key == NULL) 
	    || (key->type == SLKEY_F_INTERPRET))
	  f = NULL; 
	else f = (void (*)(void)) key->f.f;
     
	if (f == group_up)
	  {
	     transpose_groups ();
	     if (2 != slrn_group_up_n (2))
	       break;
	  }
	else if (f == group_down)
	  {
	     if (1 != slrn_group_down_n (1))
	       break;
	     
	     transpose_groups ();
	     
	     if (1 != slrn_group_up_n (1))
	       break;
	  }
	else break;
	
	if (g != Slrn_Group_Current_Group)
	  {
	     Slrn_Group_Current_Group = g;
	     find_line_num ();
	  }

	/* For a recenter if possible. */
	/* if (Slrn_Group_Window.top_window_line == Slrn_Group_Window.current_line) */
	Slrn_Group_Window.top_window_line = NULL;
	
	slrn_update_screen (1);
     }
}

/*}}}*/

static void subscribe (void) /*{{{*/
{
   SLRegexp_Type *re;
   Slrn_Group_Type *g;
   
   if (Slrn_Group_Current_Group == NULL) return;
   
   if (Slrn_Prefix_Arg_Ptr == NULL)
     {
	Slrn_Group_Current_Group->flags &= ~GROUP_UNSUBSCRIBED;
	Slrn_Group_Current_Group->flags |= GROUP_TOUCHED;
	slrn_group_down_n (1);
	Slrn_Groups_Dirty = 1;
	return;
     }
   
   Slrn_Prefix_Arg_Ptr = NULL;
   	
   if (NULL == (re = read_group_regexp ("Subscribe pattern", NULL)))
     return;

   g = Groups;
   while (g != NULL)
     {
	if (g->flags & GROUP_UNSUBSCRIBED)
	  {
	     if (NULL != slrn_regexp_match (re, g->name))
	       {
		  g->flags &= ~GROUP_HIDDEN;
		  g->flags &= ~GROUP_UNSUBSCRIBED;
		  g->flags |= GROUP_TOUCHED;
	       }
	  }
	g = g->next;
     }
   find_line_num ();
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void catch_up (void) /*{{{*/
{
   if ((Slrn_Group_Current_Group == NULL)
       || (Slrn_User_Wants_Confirmation
	   && (Slrn_Batch == 0)
	   && slrn_get_yesno(1, "Mark %s as read", Slrn_Group_Current_Group->name) <= 0))
     return;
   
   slrn_catchup_group ();
   slrn_message ("Group marked as read.");
   (void) slrn_group_down_n (1);
}

/*}}}*/

static void uncatch_up (void) /*{{{*/
{   
   if ((Slrn_Group_Current_Group == NULL)
       || (Slrn_User_Wants_Confirmation 
	   && (Slrn_Batch == 0)
	   && slrn_get_yesno(1, "Mark %s as un-read", Slrn_Group_Current_Group->name) <= 0))
     return;
 
   slrn_uncatchup_group ();   
   slrn_message ("Group marked as un-read.");
   (void) slrn_group_down_n (1);
}

/*}}}*/

static void unsubscribe (void) /*{{{*/
{
   SLRegexp_Type *re;
   Slrn_Group_Type *g;

   if (Slrn_Group_Current_Group == NULL) return;
   
   if (Slrn_Prefix_Arg_Ptr == NULL)
     {
	Slrn_Group_Current_Group->flags |= GROUP_UNSUBSCRIBED | GROUP_TOUCHED;
	slrn_group_down_n (1);
	Slrn_Groups_Dirty = 1;
	return;
     }
   
   Slrn_Prefix_Arg_Ptr = NULL;
   	
   if (NULL == (re = read_group_regexp ("Un-Subscribe pattern", NULL)))
     return;

   g = Groups;
   while (g != NULL)
     {
	if ((g->flags & GROUP_UNSUBSCRIBED) == 0)
	  {
	     if (NULL != (slrn_regexp_match (re, g->name)))
	       {
		  g->flags &= ~GROUP_HIDDEN;
		  g->flags |= (GROUP_TOUCHED | GROUP_UNSUBSCRIBED);
	       }
	  }
	g = g->next;
     }
   find_line_num ();
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void group_bob (void)
{
   while (slrn_group_up_n (1000));
}

static void group_eob (void)
{
   while (slrn_group_down_n (1000));
}


static void toggle_list_all_groups1 (int hide_flag) /*{{{*/
{
   Slrn_Group_Type *g, *first_found = NULL;
   static int all_hidden = 1;
   
   g = Groups;
   
   if (hide_flag != -1)
     {
	all_hidden = hide_flag;
     }
   else all_hidden = !all_hidden;
   
   if (all_hidden)
     {
	while (g != NULL)
	  {
	     if (g->flags & GROUP_UNSUBSCRIBED) g->flags |= GROUP_HIDDEN;
	     g = g->next;
	  }
     }
   else
     {
	SLRegexp_Type *re;
	char origpat[256];
	
	if (NULL == (re = read_group_regexp ("List Groups (e.g., comp*unix*)",
					     origpat)))
	  {
	     all_hidden = 1;
	     return;
	  }
	
	if ((Slrn_List_Active_File == 0)
	    && Slrn_Use_Xgtitle
	    && (OK_XGTITLE == Slrn_Server_Obj->sv_xgtitle_cmd (origpat)))
	  {
	     first_found = process_xgtitle_info ();
	  }
	else while (g != NULL)
	  {
	     if (g->flags & GROUP_UNSUBSCRIBED)
	       {
		  if (NULL != slrn_regexp_match (re, g->name))
		    {
		       if (first_found == NULL) first_found = g;
		       g->flags &= ~GROUP_HIDDEN;
		    }
	       }
	     g = g->next;
	  }
     }
   
   g = Slrn_Group_Current_Group;
   if (first_found != NULL)
     g = first_found;
   else
     {
	while ((g != NULL) && (g->flags & GROUP_HIDDEN)) g = g->next;
	if ((g == NULL) && (Slrn_Group_Current_Group != NULL))
	  {
	     g = Slrn_Group_Current_Group -> prev;
	     while ((g != NULL) && (g->flags & GROUP_HIDDEN)) g = g->prev;
	  }
     }
   Slrn_Group_Current_Group = g;
   
   Slrn_Full_Screen_Update = 1;
   
   if ((all_hidden == 0) && (Slrn_Group_Current_Group == NULL))
     {
	Slrn_Group_Current_Group = Groups;
	if ((Slrn_Group_Current_Group != NULL)
	    && (Slrn_Group_Current_Group->flags & GROUP_HIDDEN))
	  {
	     Slrn_Group_Current_Group = NULL;
	  }
     }
   
   find_line_num ();
}

/*}}}*/

static void toggle_list_all_groups (void) /*{{{*/
{
   int mode;
   if (Slrn_Prefix_Arg_Ptr != NULL)
     mode = 1;
   else mode = -1;
   
   toggle_list_all_groups1 (mode);
}

/*}}}*/

static void toggle_group_display (void) /*{{{*/
{
   Slrn_Group_Display_Descriptions = !Slrn_Group_Display_Descriptions;
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/


static void toggle_hide_groups (void) /*{{{*/
{
   Slrn_Group_Type *g;
   
   Groups_Hidden = !Groups_Hidden;
   
   g = Groups;
   
   if (Groups_Hidden)
     {
	while (g != NULL)
	  {
	     if ((g->unread == 0)
		 && ((g->flags & GROUP_UNSUBSCRIBED) == 0))
	       g->flags |= GROUP_HIDDEN;
	     
	     g = g->next;
	  }
     }
   else
     {
	while (g != NULL)
	  {
	     if ((g->unread == 0)
		 && ((g->flags & GROUP_UNSUBSCRIBED) == 0))
	       g->flags &= ~GROUP_HIDDEN;
	     
	     g = g->next;
	  }
     }
   
   g = Slrn_Group_Current_Group;
   if (g == NULL) g = Groups;
   
   while ((g != NULL) && (g->flags & GROUP_HIDDEN)) g = g->next;
   if ((g == NULL) && (Slrn_Group_Current_Group != NULL))
     {
	g = Slrn_Group_Current_Group -> prev;
	while ((g != NULL) && (g->flags & GROUP_HIDDEN)) g = g->prev;
     }
   Slrn_Group_Current_Group = g;
   
   find_line_num ();
   
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/


static void select_group_cmd (void)
{
   if (-1 == slrn_group_select_group ())
     slrn_error ("No unread articles.");
}

void slrn_post_cmd (void) /*{{{*/
{
   int ret;
   char *name, *dist = NULL;	       /* "world"; */
   char group[256];
   char subj[256];
   
   if (Slrn_Post_Obj->po_can_post == 0)
     {
	slrn_error ("Posting not allowed.");
	return;
     }
   if (Slrn_User_Wants_Confirmation && (Slrn_Batch == 0) &&
       (slrn_get_yesno (1, "Are you sure that you want to post") <= 0))
     return;
   
   if (Slrn_Group_Current_Group == NULL) name = ""; else name = Slrn_Group_Current_Group->name;
   strcpy (group, name);
   if (slrn_read_input ("Newsgroup", NULL, group, 1) <= 0) return;
   *subj = 0; if (slrn_read_input ("Subject", NULL, subj, 1) <= 0) return;
   
   ret = slrn_post (group, subj, dist);
   if (ret < 0)
     {
	slrn_error ("Posting failed.");
     }
}

/*}}}*/

static void toggle_scoring (void) /*{{{*/
{
   if (-1 == slrn_check_batch ())
     return;

   switch (slrn_get_response ("FfSsNnCc\007",
			      "Select scoring mode: F-ull, S-imple, N-one, C-ancel"))
     {
      case 'F':
      case 'f':
	Slrn_Perform_Scoring = SLRN_XOVER_SCORING | SLRN_EXPENSIVE_SCORING;
	slrn_message ("Full Header Scoring enabled.");
	break;
	
      case 'S':
      case 's':
	Slrn_Perform_Scoring = SLRN_XOVER_SCORING;
	slrn_message ("Expensive Scoring disabled.");
	break;
	
      case 'N':
      case 'n':
	Slrn_Perform_Scoring = 0;
	slrn_message ("Scoring disabled.");
	break;
	
      default:
	slrn_clear_message ();
	break;
     }
}

/*}}}*/

static void save_newsrc_cmd (void) /*{{{*/
{
   if (Slrn_Groups_Dirty)
     {
	slrn_write_newsrc ();
     }
   else
     {
	slrn_message ("No changes need to be saved.");
     }
   slrn_smg_refresh ();
}

/*}}}*/

/*}}}*/

/*{{{ Group Mode Initialization/Keybindings */

static void slrn_group_hup (int sig)
{
   slrn_write_newsrc ();
   slrn_quit (sig);
}

static void enter_group_mode_hook (void)
{
#if SLRN_HAS_SLANG
   SLang_run_hooks ("group_mode_hook", NULL, NULL);
#endif
}


static Slrn_Mode_Type Group_Mode_Cap =
{
   NULL,
   group_update_screen,		       /* redraw */
   NULL,			       /* sig winch hook */
   slrn_group_hup,		       /* hangup hook */
   enter_group_mode_hook,	       /* enter_mode_hook */
   SLRN_GROUP_MODE
};

/*{{{ Group Mode Keybindings */

#define A_KEY(s, f)  {s, (int (*)(void)) f}
static SLKeymap_Function_Type Group_Functions [] = /*{{{*/
{
   A_KEY("digit_arg", slrn_digit_arg),
   A_KEY("move_group", move_group_cmd),
   A_KEY("uncatch_up", uncatch_up),
   A_KEY("toggle_scoring", toggle_scoring),
   A_KEY("toggle_group_display", toggle_group_display),
   A_KEY("refresh_groups", refresh_groups_cmd),
   A_KEY("save_newsrc", save_newsrc_cmd),
   A_KEY("group_search", group_search_cmd),
   A_KEY("group_search_forward", group_search_cmd),
   A_KEY("toggle_list_all", toggle_list_all_groups),
   A_KEY("add_group", add_group_cmd),
   A_KEY("group_bob", group_bob),
   A_KEY("bob", group_bob),
   A_KEY("catchup", catch_up),
   A_KEY("down", group_down),
   A_KEY("eob", group_eob),
   A_KEY("group_eob", group_eob),
   A_KEY("help", slrn_group_help),
   A_KEY("pagedown", group_pagedown),
   A_KEY("pageup", group_pageup),
   A_KEY("post", slrn_post_cmd),
   A_KEY("quit", slrn_group_quit),
   A_KEY("redraw", slrn_redraw),
   A_KEY("select_group", select_group_cmd),
   A_KEY("subscribe", subscribe),
   A_KEY("suspend", slrn_suspend_cmd),
   A_KEY("toggle_hidden", toggle_hide_groups),
   A_KEY("unsubscribe", unsubscribe),
   A_KEY("up", group_up),
   A_KEY("repeat_last_key", slrn_repeat_last_key),
   A_KEY("transpose_groups", transpose_groups),
   A_KEY(NULL, NULL)
};

/*}}}*/

/*{{{ Mouse Functions*/


/* actions for different regions:
 *	- top status line (help)
 *	- normal region
 *	- bottom status line
 */
static void group_mouse (void (*top_status)(void),
			 void (*bot_status)(void),
			 void (*normal_region)(void)
			 )
{
   int r,c;
   
   slrn_get_mouse_rc (&r, &c);
   
   /* take top status line into account */
   if (r == 1)
     {
	if (Slrn_Use_Mouse)
	  slrn_execute_menu (c);
	else
	  if (NULL != top_status) (*top_status) ();
 	return;
     }
   
   if (r >= SLtt_Screen_Rows)
     return;
   
   /* bottom status line */
   if (r == SLtt_Screen_Rows - 1)
     {
	if (NULL != bot_status) (*bot_status) ();
	return;
     }
   
   r -= (1 + Last_Cursor_Row);
   if (r < 0)
     {
	r = -r;
	if (r != (int) slrn_group_up_n (r)) return;
     }
   else if (r != (int) slrn_group_down_n (r)) return;
   
   if (NULL != normal_region) (*normal_region) ();
}

static void group_mouse_left (void)
{
   group_mouse (slrn_group_help, group_pagedown, select_group_cmd);
}

static void group_mouse_middle (void)
{
   group_mouse (toggle_group_display, toggle_hide_groups, select_group_cmd);
#if 1
   /* Make up for buggy rxvt which have problems with the middle key. */
   if (NULL != getenv ("COLORTERM"))
     {
	if (SLang_input_pending (7))
	  {
	     while (SLang_input_pending (0)) SLang_getkey ();
	  }
     }
#endif
}

static void group_mouse_right (void)
{
   group_mouse (slrn_group_help, group_pageup, select_group_cmd);
}


/*}}}*/

/*}}}*/

void slrn_init_group_mode (void) /*{{{*/
{
   char  *err = "Unable to create group keymap!";
   
   if (NULL == (Slrn_Group_Keymap = SLang_create_keymap ("group", NULL)))
     slrn_exit_error (err);
   
   Group_Mode_Cap.keymap = Slrn_Group_Keymap;
   
   Slrn_Group_Keymap->functions = Group_Functions;
   
   SLkm_define_key ("\0331", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0332", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0333", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0334", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0335", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0336", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0337", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0338", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0339", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key ("\0330", (FVOID_STAR) slrn_digit_arg, Slrn_Group_Keymap);
   SLkm_define_key  ("^K\033[A", (FVOID_STAR) group_bob, Slrn_Group_Keymap);
   SLkm_define_key  ("^K\033OA", (FVOID_STAR) group_bob, Slrn_Group_Keymap);
   SLkm_define_key  ("^K\033[B", (FVOID_STAR) group_eob, Slrn_Group_Keymap);
   SLkm_define_key  ("^K\033OB", (FVOID_STAR) group_eob, Slrn_Group_Keymap);
   SLkm_define_key  ("\033A", (FVOID_STAR) toggle_group_display, Slrn_Group_Keymap);
   SLkm_define_key  ("\033>", (FVOID_STAR) group_eob, Slrn_Group_Keymap);
   SLkm_define_key  ("\033<", (FVOID_STAR) group_bob, Slrn_Group_Keymap);
   SLkm_define_key  ("^D", (FVOID_STAR) group_pagedown, Slrn_Group_Keymap);
   SLkm_define_key  ("^V", (FVOID_STAR) group_pagedown, Slrn_Group_Keymap);
#ifdef __os2__
   SLkm_define_key  ("^@Q", (FVOID_STAR) group_pagedown, Slrn_Group_Keymap);
   SLkm_define_key  ("\xE0Q", (FVOID_STAR) group_pagedown, Slrn_Group_Keymap);
   SLkm_define_key  ("^@I", (FVOID_STAR) group_pageup, Slrn_Group_Keymap);
   SLkm_define_key  ("\xE0I", (FVOID_STAR) group_pageup, Slrn_Group_Keymap);   
#else
   SLkm_define_key  ("\033[6~", (FVOID_STAR) group_pagedown, Slrn_Group_Keymap);
   SLkm_define_key  ("\033[5~", (FVOID_STAR) group_pageup, Slrn_Group_Keymap);
#endif
   SLkm_define_key  ("m", (FVOID_STAR) move_group_cmd, Slrn_Group_Keymap);
   SLkm_define_key  ("^U", (FVOID_STAR) group_pageup, Slrn_Group_Keymap);
   SLkm_define_key  ("\033V", (FVOID_STAR) group_pageup, Slrn_Group_Keymap);
   SLkm_define_key  ("a", (FVOID_STAR) add_group_cmd, Slrn_Group_Keymap);
   SLkm_define_key  ("u", (FVOID_STAR) unsubscribe, Slrn_Group_Keymap);
   SLkm_define_key  ("s", (FVOID_STAR) subscribe, Slrn_Group_Keymap);
   SLkm_define_key  ("\033u", (FVOID_STAR) uncatch_up, Slrn_Group_Keymap);
   SLkm_define_key  ("c", (FVOID_STAR) catch_up, Slrn_Group_Keymap);
   SLkm_define_key  ("K", (FVOID_STAR) toggle_scoring, Slrn_Group_Keymap);
   SLkm_define_key  ("L", (FVOID_STAR) toggle_list_all_groups, Slrn_Group_Keymap);
   SLkm_define_key  ("l", (FVOID_STAR) toggle_hide_groups, Slrn_Group_Keymap);
   SLkm_define_key  ("^Z", (FVOID_STAR) slrn_suspend_cmd, Slrn_Group_Keymap);
   SLkm_define_key  (" ", (FVOID_STAR) select_group_cmd, Slrn_Group_Keymap);
   SLkm_define_key  (".", (FVOID_STAR) slrn_repeat_last_key, Slrn_Group_Keymap);
   SLkm_define_key  ("P", (FVOID_STAR) slrn_post_cmd, Slrn_Group_Keymap);
   SLkm_define_key  ("?", (FVOID_STAR) slrn_group_help, Slrn_Group_Keymap);
   SLkm_define_key  ("\r", (FVOID_STAR) select_group_cmd, Slrn_Group_Keymap);
   SLkm_define_key  ("q", (FVOID_STAR) slrn_group_quit, Slrn_Group_Keymap);
   SLkm_define_key  ("^X^C", (FVOID_STAR) slrn_group_quit, Slrn_Group_Keymap);
   SLkm_define_key  ("^X^T", (FVOID_STAR) transpose_groups, Slrn_Group_Keymap);
   SLkm_define_key  ("^R", (FVOID_STAR) slrn_redraw, Slrn_Group_Keymap);
   SLkm_define_key  ("^L", (FVOID_STAR) slrn_redraw, Slrn_Group_Keymap);
   SLkm_define_key  ("^P", (FVOID_STAR) group_up, Slrn_Group_Keymap);
#ifdef __os2__
   SLkm_define_key  ("^@H", (FVOID_STAR) group_up, Slrn_Group_Keymap);
   SLkm_define_key  ("\xE0H", (FVOID_STAR) group_up, Slrn_Group_Keymap);
   SLkm_define_key  ("^@P", (FVOID_STAR) group_down, Slrn_Group_Keymap);
   SLkm_define_key  ("\xE0P", (FVOID_STAR) group_down, Slrn_Group_Keymap);
#else
   SLkm_define_key  ("\033[A", (FVOID_STAR) group_up, Slrn_Group_Keymap);
   SLkm_define_key  ("\033OA", (FVOID_STAR) group_up, Slrn_Group_Keymap);
   SLkm_define_key  ("\033[B", (FVOID_STAR) group_down, Slrn_Group_Keymap);
   SLkm_define_key  ("\033OB", (FVOID_STAR) group_down, Slrn_Group_Keymap);
#endif
   SLkm_define_key  ("N", (FVOID_STAR) group_down, Slrn_Group_Keymap);
   SLkm_define_key  ("^N", (FVOID_STAR) group_down, Slrn_Group_Keymap);
   SLkm_define_key  ("/", (FVOID_STAR) group_search_cmd, Slrn_Group_Keymap);
   SLkm_define_key  ("G", (FVOID_STAR) refresh_groups_cmd, Slrn_Group_Keymap);
   SLkm_define_key  ("X", (FVOID_STAR) save_newsrc_cmd, Slrn_Group_Keymap);
   
   /* mouse (left/right/middle) */
   SLkm_define_key  ("\033[M\040", (FVOID_STAR) group_mouse_left, Slrn_Group_Keymap);
   SLkm_define_key  ("\033[M\041", (FVOID_STAR) group_mouse_middle, Slrn_Group_Keymap);
   SLkm_define_key  ("\033[M\042", (FVOID_STAR) group_mouse_right, Slrn_Group_Keymap);
   
   if (SLang_Error) slrn_exit_error (err);
}

/*}}}*/

/*}}}*/

int slrn_select_group_mode (void) /*{{{*/
{
   Slrn_Current_Mode = &Group_Mode_Cap;
   
   init_group_win_struct ();
   
   Last_Cursor_Row = 0;
   group_quick_help ();
   Slrn_Full_Screen_Update = 1;
   
   enter_group_mode_hook ();

   return 0;
}

/*}}}*/

/*{{{ Read/Write Newsrc, Group Descriptions */

static void add_group_description (unsigned char *s, unsigned char *smax, /*{{{*/
				   unsigned char *dsc)
{
   Slrn_Group_Type *g;
   unsigned long hash;
   
   hash = slrn_compute_hash (s, smax);
   g = Group_Hash_Table[hash % GROUP_HASH_TABLE_SIZE];
   while (g != NULL)
     {
	if ((g->hash == hash) && (!strncmp (g->name, (char *) s, (unsigned int) (smax - s))))
	  {
	     /* Sometimes these get repeated --- not by slrn but on the server! */
	     slrn_free (g->descript);
	     
	     g->descript = slrn_strmalloc ((char *) dsc, 0);
	     /* Ok to fail. */
	     return;
	  }
	g = g->hash_next;
     }
}

/*}}}*/
void slrn_get_group_descriptions (void) /*{{{*/
{
   FILE *fp;
   char line[2 * SLRN_MAX_PATH_LEN];
   char file[SLRN_MAX_PATH_LEN];
   int num;
   
#ifdef VMS
   sprintf (file, "%s-dsc", Slrn_Newsrc_File);
#else
# ifdef __os2__
   sprintf (file, "ds-%s", Slrn_Newsrc_File);
# else
   sprintf (file, "%s.dsc", Slrn_Newsrc_File);
# endif
#endif
   
   
   if (NULL == (fp = slrn_open_home_file (file, "w", line, 0)))
     {
	slrn_exit_error ("\
Unable to create newsgroup description file:\n%s\n", line);
     }
   
   fprintf (stdout, "\nCreating description file %s.\n", line);
   fprintf (stdout, "Getting newsgroup descriptions from server.\n\
Note: This step may take some time if you have a slow connection!!!\n");
   
   fflush (stdout);
   
   if (OK_GROUPS != Slrn_Server_Obj->sv_list_newsgroups ())
     {
	slrn_error ("Server failed on list newsgroups command.");
	slrn_fclose (fp);
	return;
     }
   
   num = 0;
   
   while (NULL != Slrn_Server_Obj->sv_read_line (line, sizeof (line) - 1))
     {
	unsigned char *b, *bmax, *dsc, ch;
	
	num = num % 25;
	if (num == 0)
	  {
	     putc ('.', stdout);
	     fflush (stdout);
	  }
	
	/* Check the syntax on this line. They are often corrupt */
	b = (unsigned char *) slrn_skip_whitespace (line);
	
	bmax = b;
	while ((ch = *bmax) > ' ') bmax++;
	if ((ch == 0) || (ch == '\n')) continue;
	*bmax = 0;
	
	/* News group marked off, now get the description. */
	dsc = bmax + 1;
	while (((ch = *dsc) <= ' ') && (ch != 0)) dsc++;
	if ((ch == 0) || (ch == '?') || (ch == '\n')) continue;
	
	/* add_group_description (b, bmax, dsc); */
	
	fputs ((char *) b, fp);
	putc(':', fp);
	fputs ((char *) dsc, fp);
	putc('\n', fp);
	
	num++;
     }
   slrn_fclose (fp);
   putc ('\n', stdout);
}

/*}}}*/
int slrn_read_group_descriptions (void) /*{{{*/
{
   FILE *fp;
   char line[2 * SLRN_MAX_PATH_LEN];
   char file[SLRN_MAX_PATH_LEN];
   
#ifdef VMS
   sprintf (file, "%s-dsc", Slrn_Newsrc_File);
#else
# ifdef __os2__
   sprintf (file, "ds-%s", Slrn_Newsrc_File);
# else
   sprintf (file, "%s.dsc", Slrn_Newsrc_File);
# endif
#endif
   
   
   if (NULL == (fp = slrn_open_home_file (file, "r", line, 0)))
     {
	if (Slrn_Lib_Dir != NULL)
	  {
#ifdef VMS
	     sprintf (file, "%snewsgroups.dsc", Slrn_Lib_Dir);
	     if (NULL == (fp = slrn_open_home_file (file, "r", line, 0)))
	       {
		  sprintf (file, "%snewsgroups-dsc", Slrn_Lib_Dir);
		  fp = slrn_open_home_file (file, "r", line, 0);
	       }
#else
	     sprintf (file, "%s/newsgroups.dsc", Slrn_Lib_Dir);
	     fp = slrn_open_home_file (file, "r", line, 0);
#endif
	  }
	if (fp == NULL) return -1;
     }
   
   while (NULL != fgets (line, sizeof (line) - 1, fp))
     {
	unsigned char *bmax, *dsc, ch;
	
	bmax = (unsigned char *) line;
	while (((ch = *bmax) != ':') && ch) bmax++;
	if (ch <= ' ') continue;
	*bmax = 0;
	
	dsc = bmax + 1;
	add_group_description ((unsigned char *) line, bmax, dsc);
     }
   slrn_fclose (fp);
   return 0;
}

/*}}}*/

void slrn_check_new_groups (int create_flag) /*{{{*/
{
   FILE *fp;
   time_t tloc;
   struct tm *tm_struct;
   char line[SLRN_MAX_PATH_LEN];
   char file[SLRN_MAX_PATH_LEN];
   int num;
   char *p;
   int parse_error = 0;
   
#ifdef VMS
   sprintf (file, "%s-time", Slrn_Newsrc_File);
#else
#ifdef __os2__
   sprintf (file, "tm-%s", Slrn_Newsrc_File);
#else
   sprintf (file, "%s.time", Slrn_Newsrc_File);
#endif
#endif
   
   
   if ((create_flag == 0)
       && (NULL != (fp = slrn_open_home_file (file, "r", line, 0))))
     {
	char ch;
	int i;
	*line = 0;
	fgets (line, sizeof (line), fp);
	slrn_fclose (fp);
	
	time (&tloc);
	parse_error = 1;
	
	/* parse this line to make sure it is ok.  If it is bad, issue a warning
	 * and go on.
	 */
	if (strncmp ("NEWGROUPS ", line, 10)) goto parse_error_label;
	p = line + 10;
	
	p = slrn_skip_whitespace (p);
	
	/* parse yymmdd */
	for (i = 0; i < 6; i++)
	  {
	     ch = p[i];
	     if ((ch < '0') || (ch > '9')) goto parse_error_label;
	  }
	if (p[6] != ' ') goto parse_error_label;
	
	ch = p[2];
	if (ch > '1') goto parse_error_label;
	if ((ch == '1') && (p[3] > '2')) goto parse_error_label;
	ch = p[4];
	if (ch > '3') goto parse_error_label;
	if ((ch == '3') && (p[5] > '1')) goto parse_error_label;
	
	/* Now the hour: hhmmss */
	p = slrn_skip_whitespace (p + 6);

	for (i = 0; i < 6; i++)
	  {
	     ch = p[i];
	     if ((ch < '0') || (ch > '9')) goto parse_error_label;
	  }
	ch = p[0];
	if (ch > '2') goto parse_error_label;
	if ((ch == '2') && (p[1] > '3')) goto parse_error_label;
	if ((p[2] > '5') || (p[4] > '5')) goto parse_error_label;
	
	p = slrn_skip_whitespace (p + 6);
	
	if ((p[0] == 'G') && (p[1] == 'M') && (p[2] == 'T'))
	  p += 3;
	*p = 0;
	
	parse_error = 0;
	
	switch (Slrn_Server_Obj->sv_put_server_cmd (line, line, sizeof (line)))
	  {
	   case OK_NEWGROUPS:
	     break;
	     
	   case ERR_FAULT:	     
	     return;
	     
	   case ERR_COMMAND:
	     slrn_message ("Server does not implement NEWGROUPS command.");
	     return;

	   default:
	     slrn_message ("Server failed to return proper response to NEWGROUPS:\n%s\n",
			   line);
	     goto parse_error_label;
	  }
	
	num = 0;
	while (NULL != Slrn_Server_Obj->sv_read_line (line, sizeof (line)))
	  {
	     /* line contains new newsgroup name */
	     add_unsubscribed_group (line);
	     num++;
	  }
	
	if (num)
	  {
	     slrn_message ("%d new newsgroup(s) found.", num);
	  }
     }
   else time (&tloc);
   
   parse_error_label:
   if (parse_error)
     {
	slrn_message ("\
%s appears corrupt.\n\
I expected to see see: NEWGROUPS yymmdd hhmmss GMT\n\
I will patch the file up for you.\n", file);
     }
   
   
   if (NULL == (fp = slrn_open_home_file (file, "w", line, 1)))
     {
	slrn_exit_error ("Unable to open %s to record date.", line);
     }
   
#if defined(VMS) || defined(__BEOS__)
   /* gmtime is broken on BEOS */
   tm_struct = localtime (&tloc);
   fprintf (fp, "NEWGROUPS %02d%02d%02d %02d%02d%02d",
            tm_struct->tm_year, 1 + tm_struct->tm_mon,
            tm_struct->tm_mday, tm_struct->tm_hour,
            tm_struct->tm_min, tm_struct->tm_sec);
#else
   tm_struct = gmtime (&tloc);
   fprintf (fp, "NEWGROUPS %02d%02d%02d %02d%02d%02d GMT",
	    tm_struct->tm_year, 1 + tm_struct->tm_mon,
	    tm_struct->tm_mday, tm_struct->tm_hour,
	    tm_struct->tm_min, tm_struct->tm_sec);
#endif
   slrn_fclose (fp);
}

/*}}}*/

/* Rearrange Slrn_Group_Current_Group such that it follows last_group and 
 * return Slrn_Group_Current_Group.  If last_group is NULL, Slrn_Group_Current_Group
 * should be put at top of list.
 */
static Slrn_Group_Type *place_group_in_newsrc_order (Slrn_Group_Type *last_group) /*{{{*/
{
   Slrn_Group_Type *next_group, *prev_group;
   
   next_group = Slrn_Group_Current_Group->next;
   prev_group = Slrn_Group_Current_Group->prev;
   if (next_group != NULL) next_group->prev = prev_group;
   if (prev_group != NULL) prev_group->next = next_group;
   
   Slrn_Group_Current_Group->prev = last_group;
   if (last_group != NULL)
     {
	Slrn_Group_Current_Group->next = last_group->next;
	
	if (Slrn_Group_Current_Group->next != NULL)
	  Slrn_Group_Current_Group->next->prev = Slrn_Group_Current_Group;
	
	last_group->next = Slrn_Group_Current_Group;
     }
   else if (Slrn_Group_Current_Group != Groups)
     {
	Slrn_Group_Current_Group->next = Groups;
	if (Groups != NULL) Groups->prev = Slrn_Group_Current_Group;
	Groups = Slrn_Group_Current_Group;
     }
   else				       
     {
	/* correct next_group->prev since it was not set correctly above */
	if (next_group != NULL)
	  next_group->prev = Slrn_Group_Current_Group;
     }

   return Slrn_Group_Current_Group;
}

/*}}}*/

static int parse_active_line (char *name, unsigned int *lenp, /*{{{*/
			      int *minp, int *maxp)
{
   char *p;
   
   p = name;
   while (*p > ' ') p++;
   *lenp = (unsigned int) (p - name);
   
   while (*p == ' ') p++;
   *maxp = atoi (p);
   while (*p > ' ') p++;  while (*p == ' ') p++;
   *minp = atoi(p);
   if (*maxp < *minp) *minp = *maxp + 1;
   return 0;
}

/*}}}*/

int slrn_read_newsrc (int create_flag) /*{{{*/
{
   char file[SLRN_MAX_PATH_LEN];
   FILE *fp = NULL;
   char line[NNTP_BUFFER_SIZE];
   register char *p, ch;
   
   if (create_flag)
     {
	if (NULL == (fp = slrn_open_home_file (Slrn_Newsrc_File, "w", file, 1)))
	  {
	     slrn_exit_error ("Unable to create %s.", file);
	  }
	
	fputs ("\n--The next step may take a while if the NNTP connection is slow.--\n\n", stdout);
	fprintf (stdout, "Creating %s.", file);
	fflush (stdout);
     }
   
   if (create_flag || Slrn_List_Active_File)
     {
	int count = 0;

	if (OK_GROUPS != Slrn_Server_Obj->sv_list_active ())
	  slrn_exit_error ("Server failed LIST ACTIVE.");

	while (NULL != Slrn_Server_Obj->sv_read_line (line, sizeof(line)))
	  {
	     unsigned int len;
	     int min, max;
	     
	     parse_active_line (line, &len, &min, &max);
	     
	     if (NULL == create_group_entry (line, len, min, max, 0, 0))
	       continue;
	     
	     
	     if (create_flag)
	       {
		  count++;
		  count = count % 50;
		  if (count == 0)
		    {
		       putc ('.', stdout);
		       fflush (stdout);
		    }
		  
		  if ((*line == 'n')
		      && (!strncmp (line,    "news.newusers.questions", 23)
			  || !strncmp (line, "news.groups.questions", 21)
			  || !strncmp (line, "news.answers", 12)
			  || !strncmp (line, "news.announce.newusers", 22)
			  || !strncmp (line, "news.software.readers", 21)))
		    {
		       add_group (line, len, 0, 1);
		    }
		  else if ((*line == 'a')
			   && (!strncmp (line, "alt.test", 8)))
		    {
		       add_group (line, len, 0, 1);
		    }
		  else add_group (line, len, GROUP_UNSUBSCRIBED, 1);
	       }
	  }
     }
   
   if (create_flag == 0)
     {
	Slrn_Group_Type *last_group = NULL;
	
	if (Unsubscribed_Groups != NULL)
	  {
	     unsigned int subscribe_flag;
	     Unsubscribed_Slrn_Group_Type *ug = Unsubscribed_Groups, *ugnext;
	     
	     if (Slrn_Unsubscribe_New_Groups)
	       subscribe_flag = GROUP_UNSUBSCRIBED | GROUP_NEW_GROUP_FLAG;
	     else subscribe_flag = GROUP_NEW_GROUP_FLAG;
	     
	     while (ug != NULL)
	       {
		  ugnext = ug->next;
		  
		  if ((-1 != add_group (ug->name, strlen (ug->name), subscribe_flag, 0))
		      && Slrn_List_Active_File)
		    last_group = place_group_in_newsrc_order (last_group);
		  
		  SLFREE (ug);
		  ug = ugnext;
	       }
	     Unsubscribed_Groups = NULL;
	  }
	
	if ((NULL == (fp = slrn_open_home_file (Slrn_Newsrc_File, "r", file, 0)))
	    && (NULL == (fp = slrn_open_home_file (".newsrc", "r", file, 0))))
	  {
	     sprintf (line, "Unable to open %s.", file);
	     slrn_exit_error (line);
	  }
	
	while (fgets (line, sizeof(line) - 1, fp) != NULL)
	  {
	     p = line;
	     while (((ch = *p) != '!') && (ch != ':') && (ch != 0))
	       {
		  p++;
	       }
	     if ((ch == 0) || (p == line)) continue;
	     if (-1 == add_group (line, (unsigned int) (p - line),
				  ((ch == '!') ? GROUP_UNSUBSCRIBED : 0),
				  0))
	       continue;
	     
	     /* perform a re-arrangement to match arrangement in the
	      * newsrc file
	      */
	     if (Slrn_List_Active_File && (ch !=  '!'))
	       {
		  last_group = place_group_in_newsrc_order (last_group);
	       }
	  }
     }
   
   slrn_fclose (fp);
   
   Slrn_Group_Current_Group = Groups;

   init_group_win_struct ();
   
   toggle_hide_groups ();
   
   /* Unhide the new groups.  Do it here so that if there are no unread 
    * articles, it will be visible but also enables user to toggle them
    * so that they will become invisble again.
    */
   Slrn_Group_Current_Group = Groups;
   while ((Slrn_Group_Current_Group != NULL)
	  && (Slrn_Group_Current_Group->flags & GROUP_NEW_GROUP_FLAG))
     {
	Slrn_Group_Current_Group->flags &= ~GROUP_HIDDEN;
	Slrn_Group_Current_Group = Slrn_Group_Current_Group->next;
     }

   Slrn_Group_Current_Group = Groups;
   while ((Slrn_Group_Current_Group != NULL)
	  && (Slrn_Group_Current_Group->flags & GROUP_HIDDEN))
     Slrn_Group_Current_Group = Slrn_Group_Current_Group->next;
   
   find_line_num ();
   
   if (create_flag)
     {
	if (Slrn_Group_Window.num_lines == 0)
	  {
	     Slrn_Group_Current_Group = Groups;
	     while (Slrn_Group_Current_Group != NULL)
	       {
		  Slrn_Group_Current_Group->flags &= ~GROUP_HIDDEN;
		  Slrn_Group_Current_Group = Slrn_Group_Current_Group->next;
	       }
	     Slrn_Group_Current_Group = Groups;
	     find_line_num ();
	  }
	Slrn_Groups_Dirty = 1;
     }
   
   group_bob ();
   return 0;
}

/*}}}*/

int slrn_write_newsrc (void) /*{{{*/
{
   Slrn_Group_Type *g;
   Slrn_Range_Type *r;
   char file[SLRN_MAX_PATH_LEN], backup_file[SLRN_MAX_PATH_LEN];
   static FILE *fp;
   int pass;
   int max;
   struct stat filestat;
   int stat_worked;
   int have_backup;

   slrn_init_hangup_signals (0);
   
   if (Slrn_Groups_Dirty == 0) 
     {
	slrn_init_hangup_signals (1);
	return 0;
     }

   /* In case of hangup and we were writing the file, make sure it is closed.
    * This will not hurt since we are going to do it again anyway.
    */
   if (fp != NULL) slrn_fclose (fp); fp = NULL;
   
   slrn_message_now ("Writing %s ...", Slrn_Newsrc_File);
   
   slrn_make_home_filename (Slrn_Newsrc_File, file);
   
   /* Try to preserve .newsrc permissions and owner/group.  This also 
    * confirms existence of file.
    */
   stat_worked = (-1 != stat (file, &filestat));
  
#ifdef VMS
   sprintf (backup_file, "%s-bak", file);
#else
   sprintf (backup_file, "%s~", file);
#endif
   
   /* Create a temp backup file.  Delete it later if user 
    * does not want backups.
    */
   (void) slrn_delete_file (backup_file);

   have_backup = 1;
   if (-1 == rename (file, backup_file))
     {
	have_backup = 0;
     }
   
   
   if (NULL == (fp = fopen (file, "w")))
     {
	slrn_error ("Unable to open file %s for writing.", file);
	if (have_backup) (void) rename (backup_file, file);
	slrn_init_hangup_signals (1);
	return -1;
     }
   
#ifdef __unix__
# ifndef __os2__
   /* Try to preserve .newsrc permissions and owner/group */
#  ifndef S_IRUSR
#   define S_IRUSR 0400
#   define S_IWUSR 0200
#   define S_IXUSR 0100
#  endif
   if (stat_worked)
     {
 	if (-1 == chmod (file, filestat.st_mode & (S_IRUSR | S_IWUSR | S_IXUSR)))
	  (void) chmod (file, S_IWUSR | S_IRUSR);

 	(void) chown (file, filestat.st_uid, filestat.st_gid);
     }
# endif
#endif
   
   
   /* We are going to do this in 2 passes.  The first pass writes out just
    * the subscribed groups.  The second pass takes care of the rest.
    */
   for (pass = 0; pass < 2; pass++)
     {
	g = Groups;
	while (g != NULL)
	  {
	     if (pass == 0)
	       {
		  if (g->flags & GROUP_UNSUBSCRIBED)
		    {
		       g = g->next;
		       continue;
		    }
		  if ((EOF == fputs (g->name, fp))
		      || (EOF == putc (':', fp)))
		    goto write_error;
	       }
	     else if (pass == 1)
	       {
		  if ((g->flags & GROUP_UNSUBSCRIBED) == 0)
		    {
		       g = g->next;
		       continue;
		    }
		  
		  if (Slrn_Write_Newsrc_Flags)
		    {
		       if ((Slrn_Write_Newsrc_Flags == 1)
			   || ((Slrn_Write_Newsrc_Flags == 2)
			       && (g->range.next == NULL)))
			 {
			    g = g->next;
			    continue;
			 }
		    }
		  if ((EOF == fputs (g->name, fp))
		      || (EOF == putc ('!', fp)))
		    goto write_error;
	       }
	     
	     r = g->range.next;
	     max = g->range.max;
	     if (r != NULL)
	       {
		  if (EOF == putc (' ', fp))
		    goto write_error;
		  
		  while (1)
		    {
		       /* Make this check because the unsubscribed group
			* range may not have been initialized from the server.
			*/
		       if ((max != -1) && (g->range.min != -1)
			   && ((g->flags & GROUP_UNSUBSCRIBED) == 0))
			 {
			    if (r->min > max) break;
			    if (r->max > max) r->max = max;
			 }
		       
		       if (r->min != r->max)
			 {
			    if (fprintf (fp, "%d-%d", r->min, r->max) < 0)
			      goto write_error;
			 }
		       else if (fprintf (fp, "%d", r->min) < 0)
			 goto write_error;
		       
		       r = r->next;
		       if (r == NULL) break;
		       if (EOF == putc (',', fp))
			 goto write_error;
		    }
	       }
	     
	     if (EOF == putc ('\n', fp))
	       goto write_error;
	     g = g->next;
	  }
     }
   
   if (-1 == slrn_fclose (fp))
     goto write_error;
   fp = NULL;
   

   if (Slrn_No_Backups)
     {
	if (have_backup) slrn_delete_file (backup_file);
     }
   
   Slrn_Groups_Dirty = 0;
   if (Slrn_TT_Initialized & SLRN_TTY_INIT)
     slrn_message ("Writing %s ... done.", Slrn_Newsrc_File);

   slrn_init_hangup_signals (1);
   return 0;
   
   write_error:
   
   slrn_fclose (fp); fp = NULL;
   slrn_error ("Write to %s failed! Disk Full?", Slrn_Newsrc_File);
   
   if (stat_worked)
     {
	/* Put back orginal file */
	(void) slrn_delete_file (file);
	if (have_backup) (void) rename (backup_file, file);
     }
   
   slrn_init_hangup_signals (1);
   return -1;
}

/*}}}*/

/*}}}*/

static void group_quick_help (void) /*{{{*/
{
   char *hlp = "SPC:Select  p:Post  c:CatchUp  l:List  q:Quit  ^R:Redraw  (u)s:(Un)Subscribe";
   
   if (Slrn_Batch)
     return;

   if (Slrn_Group_Help_Line != NULL)
     hlp = Slrn_Group_Help_Line;
   
   if (0 == slrn_message (hlp))
     Slrn_Message_Present = 0;
}

/*}}}*/
static void group_update_screen (void) /*{{{*/
{
   Slrn_Group_Type *g;
   int height = (int) Slrn_Group_Window.nrows;
   int row;
   
   /* erase last cursor */
   if (Last_Cursor_Row && !Slrn_Full_Screen_Update)
     {
	SLsmg_gotorc (Last_Cursor_Row, 0);
	SLsmg_write_string ("  ");
     }

   g = (Slrn_Group_Type *) Slrn_Group_Window.top_window_line;
   (void) SLscroll_find_top (&Slrn_Group_Window);
   
   if (g != (Slrn_Group_Type *) Slrn_Group_Window.top_window_line)
     {
	Slrn_Full_Screen_Update = 1;
	g = (Slrn_Group_Type *) Slrn_Group_Window.top_window_line;
     }

   for (row = 0; row < height; row++)
     {	
	while ((g != NULL) && (g->flags & GROUP_HIDDEN))
	  g = g->next;
	
	if (g != NULL)
	  {
	     if (Slrn_Full_Screen_Update || (g->flags & GROUP_TOUCHED))
	       {
		  SLsmg_gotorc (row + 1, 0);
		  SLsmg_printf ("  %c%5d  ",
				((g->flags & GROUP_UNSUBSCRIBED) ? 'U'
				 : ((g->flags & GROUP_NEW_GROUP_FLAG) ? 'N' : ' ')),
				g->unread);
		  slrn_set_color (GROUP_COLOR);
		  SLsmg_printf ("%s", g->name);
		  slrn_set_color (0);
		  SLsmg_erase_eol ();
		  if (Slrn_Group_Display_Descriptions)
		    {
		       if (g->descript != NULL)
			 {
			    int dsc_row;
			    
			    dsc_row = SLsmg_get_column ();
			    
			    if (dsc_row < Slrn_Group_Description_Column)
			      dsc_row = Slrn_Group_Description_Column;
			    else dsc_row++;
			    SLsmg_gotorc (row + 1, dsc_row);
			    SLsmg_set_color (GROUP_DESCR_COLOR);
			    SLsmg_write_string (g->descript);
			    SLsmg_set_color (0);
			 }
		    }
		  else if (g->range.max != -1)
		    {
		       SLsmg_gotorc (row + 1, 63);
		       SLsmg_printf ("%7d-%-7d", g->range.min, g->range.max);
		    }
		  g->flags &= ~GROUP_TOUCHED;
	       }
	     g = g->next;
	  }
	else if (Slrn_Full_Screen_Update)
	  {
	     SLsmg_gotorc (row + 1, 0);
	     SLsmg_erase_eol ();
	  }
     }

   SLsmg_gotorc (SLtt_Screen_Rows - 2, 0);
   slrn_set_color (STATUS_COLOR);
   if (Slrn_Groups_Dirty)
     SLsmg_write_string ("-*-News Groups: ");
   else SLsmg_write_string ("---News Groups: ");
   
   if (Slrn_Server_Obj->sv_name != NULL)
     SLsmg_write_string (Slrn_Server_Obj->sv_name);

   slrn_print_percent (SLtt_Screen_Rows - 2, 60, &Slrn_Group_Window);
   
   if (Slrn_Use_Mouse) slrn_update_group_menu ();
   else slrn_update_top_status_line ();
   
   if (Slrn_Message_Present == 0) group_quick_help ();
   
   Last_Cursor_Row = 1 + Slrn_Group_Window.window_row;   
   SLsmg_gotorc (Last_Cursor_Row, 0);
   
   slrn_set_color (CURSOR_COLOR);
   SLsmg_write_string ("->");
   slrn_set_color (0);
   Slrn_Full_Screen_Update = 0;
}

/*}}}*/
