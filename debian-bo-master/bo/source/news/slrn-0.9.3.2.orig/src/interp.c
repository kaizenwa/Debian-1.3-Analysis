/* -*- mode: C; mode: fold -*- */

#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>

/* rest of file inside this #if statement */
#if SLRN_HAS_SLANG

/*{{{ Include files */

#if HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include <string.h>

#include <slang.h>
#include "jdmacros.h"

#include "slrn.h"
#include "group.h"
#include "art.h"
#include "misc.h"
#include "startup.h"
#include "server.h"
#include "menu.h"
#include "interp.h"
#include "util.h"

/*}}}*/

/*{{{ Public Global Variables */

int Slrn_Use_Slang = 0;
char *Slrn_Macro_Dir;

/*}}}*/

/*{{{ Screen update and message functions */

static void error (char *msg) /*{{{*/
{
   slrn_error ("%s", msg);
}

/*}}}*/
static void message (char *msg) /*{{{*/
{
   slrn_message ("%s", msg);
}

/*}}}*/
static void update (void) /*{{{*/
{
   slrn_update_screen (1);
}

/*}}}*/
static int interp_select_box (int *np) /*{{{*/
{
   int ret = 0;
   int n, i;
   char **selections;
   Slrn_Select_Box_Type box;
   
   n = *np;
   if (n <= 0) return ret;
   
   n++;				       /* for title */
   
   if (NULL == (selections = (char **) SLMALLOC (sizeof (char *) * (n + 1))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return ret;
     }
   
   selections [n] = NULL;
   i = n;
   while (i != 0)
     {
	i--;
	if (SLpop_string (selections + i))
	  {
	     i++;
	     goto return_error;
	  }
     }
   
   box.title = selections[0];;
   box.lines = selections + 1;	       /* +1 to skip title */
   
   if (Slrn_Batch)
     {
	slrn_error ("select box function now available in batch mode.");
	ret = -1;
     }
   else ret = slrn_select_box (&box);

   i = 0;
   
   /* drop */
   
   return_error:
   while (i < n)
     {
	SLFREE (selections[i]);
	i++;
     }
   SLFREE (selections);
   return ret;
}

/*}}}*/
static int get_yesno_cancel (char *prompt)
{
   char *p = prompt;
   
   /* The slrn_get_response function is sprintf.  For that reason,
    * % cannot be used unless it is doubled up.
    */
   while (NULL != (p = slrn_strchr (p, '%')))
     {
	p++;
	if (*p != '%') return -1;
	p++;
     }
   
   return slrn_get_yesno_cancel (prompt);
}

   
static void tt_send (void)
{
   char *s;
   
   if (SLpop_string (&s))
     return;
   
   if (Slrn_Batch == 0)
     {
	SLtt_write_string (s);
	SLtt_flush_output ();
     }
   SLFREE (s);
}

/*}}}*/

/*{{{ File functions */

static void make_home_filename (char *name) /*{{{*/
{
   char file [SLRN_MAX_PATH_LEN];
   slrn_make_home_filename (name, file);
   SLang_push_string (file);
}

/*}}}*/

static int evalfile (char *file)
{
   slrn_message_now ("loading %s", file);
   if (0 == SLang_load_file (file))
     return -1;

   return 0;
}


int slrn_eval_slang_file (char *name) /*{{{*/
{
   char file [SLRN_MAX_PATH_LEN];
   
   if (Slrn_Macro_Dir != NULL)
     {
	int n = 0;
	int comma = ',';
	char *dirp;
	char dir[SLRN_MAX_PATH_LEN];
	
	while (1)
	  {
	     dirp = SLang_extract_list_element (Slrn_Macro_Dir, &n, &comma);
	     
	     if (0 == strlen (dirp))
	       break;
	     
	     slrn_make_home_dirname (dirp, dir);
	     if ((-1 != slrn_dircat (dir, name, file))
		 && (1 == slrn_file_exists (file)))
	       {
		  return evalfile (file);
	       }
	     n++;
	  }
     }
   
   slrn_make_home_filename (name, file);
   return evalfile (file);
}

/*}}}*/


/*}}}*/

/*{{{ Set/Get Variable Functions */

static void set_string_variable (void) /*{{{*/
{
   char *s1 = NULL, *s2 = NULL;
   
   if ((0 == SLpop_string (&s2))
       && (0 == SLpop_string (&s1)))
     {
	if (-1 == slrn_set_string_variable (s1, s2))
	  slrn_error ("%s is not a valid variable name.", s1);
     }
   
   slrn_free (s1);
   slrn_free (s2);
}

/*}}}*/

static void set_integer_variable (void) /*{{{*/
{
   char *s1;
   int val;
   
   if (SLang_pop_integer (&val))
     return;
   
   if (SLpop_string (&s1))
     return;

   if (-1 == slrn_set_integer_variable (s1, val))
     slrn_error ("%s is not a valid variable name.", s1);
   
   SLFREE (s1);
}

/*}}}*/

static void get_variable_value (void) /*{{{*/
{
   char *name;
   char **s;
   int *ip;
   int type;
   
   if (SLpop_string (&name))
     return;
   
   if (-1 == slrn_get_variable_value (name, &type, &s, &ip))
     {
	slrn_error ("%s is not a valid variable name.", name);
	SLFREE (name);
	return;
     }
   SLFREE (name);
   
   if (type == STRING_TYPE)
     {
	char *str;
	if ((s == NULL) || (*s == NULL)) str = "";
	else str = *s;
	SLang_push_string (str);
     }
   else if (type == INT_TYPE)
     {
	int i;
	if (ip == NULL) i = 0; else i = *ip;
	SLang_push_integer (i);
     }
}

/*}}}*/

/*}}}*/

static char *get_server_name (void) /*{{{*/
{
   if ((NULL == Slrn_Server_Obj) 
       || (NULL == Slrn_Server_Obj->sv_name))
     return "";
   
   return Slrn_Server_Obj->sv_name;
}

/*}}}*/
static void quit (int *code) /*{{{*/
{
   slrn_quit (*code);
}

/*}}}*/
   
/*{{{ Keyboard related functions */

static void definekey (char *fun, char *key, char *map) /*{{{*/
{
   SLKeyMap_List_Type *kmap;
   
   if (NULL == (kmap = SLang_find_keymap (map)))
     {
	error ("definekey: no such keymap.");
	return;
     }
   
   if (0 != SLang_define_key (key, fun, kmap))
     {
     }
}

/*}}}*/

static void undefinekey (char *key, char *map) /*{{{*/
{
   SLKeyMap_List_Type *kmap;
   
   if (NULL == (kmap = SLang_find_keymap (map)))
     {
	error ("undefinekey: no such keymap.");
	return;
     }
   
   SLang_undefine_key (key, kmap);
}

/*}}}*/

static void generic_read_mini (int no_echo) /*{{{*/
{
   char str[256];
   int ret;
   char *prompt = NULL, *dfl = NULL, *init = NULL;
   
   if (SLpop_string (&init)
       || SLpop_string (&dfl)
       || SLpop_string (&prompt))
     {
	slrn_free (init);
	slrn_free (dfl);
	slrn_free (prompt);
	return;
     }
   
   strncpy (str, init, sizeof (str));
   str[sizeof(str) - 1] = 0;
   
   if (no_echo) 
     ret = slrn_read_input_no_echo (prompt, dfl, str, 0);
   else 
     ret = slrn_read_input (prompt, dfl, str, 0);
   
   SLFREE (init);
   SLFREE (dfl);
   SLFREE (prompt);

   if (-1 == ret)
     {
	error ("Quit!");
	return;
     }
   SLang_push_string (str);
}

/*}}}*/
static void read_mini (void) /*{{{*/
{
   generic_read_mini (0);
}

/*}}}*/
static void read_mini_no_echo (void) /*{{{*/
{
   generic_read_mini (1);
}

/*}}}*/

static void set_prefix_arg (int *arg) /*{{{*/
{
   slrn_set_prefix_argument (*arg);
}

/*}}}*/

static int check_tty (void)
{
   if (Slrn_TT_Initialized & SLRN_TTY_INIT)
     return 0;
   
   error ("Terminal not initialized.");
   return -1;
}

static int input_pending (int *tsecs)
{
   if (check_tty ())
     return 0;
   
   return SLang_input_pending (*tsecs);
}

static int getkey (void)
{
   if (check_tty ())
     return 0;
   
   return SLang_getkey ();
}

static void ungetkey (int *c)
{
   if (check_tty ())
     return;

   SLang_ungetkey (*c);
}

static void set_input_string (void)
{
   char *s;
   if (SLpop_string (&s)) s = NULL;
   slrn_set_input_string (s);
}

/*}}}*/

/*{{{ Article Mode Functions */

static int check_article_mode (void) /*{{{*/
{
   if ((Slrn_Current_Mode == NULL)
       || (Slrn_Current_Mode->mode != SLRN_ARTICLE_MODE))
     {
	error ("Not in article mode.");
	return -1;
     }
   return 0;
}

/*}}}*/

static void set_article_window_size (int *nrows)
{
   slrn_set_article_window_size (*nrows);
}

/*{{{ Article Mode Article Functions */

static void pipe_article_cmd (char *cmd) /*{{{*/
{
   if (-1 == check_article_mode ())
     return;
   
   (void) slrn_pipe_article_to_cmd (cmd);
}

/*}}}*/

static int save_current_article (char *file) /*{{{*/
{
   if (-1 == check_article_mode ())
     return -1;
   
   return slrn_save_current_article (file);
}

/*}}}*/

static int generic_search_article (char *str, int is_regexp) /*{{{*/
{
   Slrn_Article_Line_Type *l;
   char *ptr;
   
   if (-1 == check_article_mode ())
     return 0;
   
   l = slrn_search_article (str, &ptr, is_regexp, 1);
   if (l == NULL)
     return 0;
   
   SLang_push_string (l->buf);
   return 1;
}

/*}}}*/

static int search_article (char *s) /*{{{*/
{
   return generic_search_article (s, 0);
}

/*}}}*/

static int re_search_article (char *s) /*{{{*/
{
   return generic_search_article (s, 1);
}

/*}}}*/

static char *extract_article_header (char *h) /*{{{*/
{
   unsigned int len;
   char buf[128];
   
   if (-1 == check_article_mode ())
     return NULL;

   len = strlen (h);
   if ((len + 3) <= sizeof (buf))
     {
	sprintf (buf, "%s: ", h);
	h = slrn_extract_header (buf, len + 2);
     }
   else h = NULL;
   
   if (h == NULL) h = "";

   return h;
}

/*}}}*/

static char *article_as_string (void) /*{{{*/
{
   unsigned int len;
   Slrn_Article_Line_Type *l;
   char *s, *s1;
   
   l = Slrn_Article_Lines;

   len = 0;
   while (l != NULL)
     {
	char *buf;
	Slrn_Article_Line_Type *next = l->next;
	
	buf = l->buf;
	if (l->flags & WRAPPED_LINE) buf++;   /* skip space */
	
	len += strlen (buf);
	
	if ((next == NULL) || (0 == (next->flags & WRAPPED_LINE)))
	  len++;
	l = next;
     }
   
   if (NULL == (s = (char *) SLMALLOC (len + 1)))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return NULL;
     }
   
   l = Slrn_Article_Lines;
   s1 = s;
   
   while (l != NULL)
     {
	char *buf;
	Slrn_Article_Line_Type *next = l->next;
	
	buf = l->buf;
	if (l->flags & WRAPPED_LINE) buf++;   /* skip space */
	
	while (*buf != 0) *s1++ = *buf++;
	       
	if ((next == NULL) || (0 == (next->flags & WRAPPED_LINE)))
	  *s1++ = '\n';
	l = next;
     }
   *s1 = 0;
   return s;
}

/*}}}*/

/*}}}*/

/*{{{ Article Mode Header Functions */

/*{{{ Header Flag Variables */

static int Interp_Header_Read = HEADER_READ;
static int Interp_Header_Tagged = HEADER_TAGGED;
static int Interp_Header_High_Score = HEADER_HIGH_SCORE;
static int Interp_Header_Low_Score = HEADER_LOW_SCORE;

/*}}}*/

static void uncollapse_threads (void) /*{{{*/
{
   if (0 == check_article_mode ())
     slrn_uncollapse_threads (1);
}

/*}}}*/

static void collapse_threads (void) /*{{{*/
{
   if (0 == check_article_mode ())
     slrn_collapse_threads (1);
}

/*}}}*/

static void collapse_thread (void)
{
   if (0 == check_article_mode ())
     slrn_collapse_this_thread (Slrn_Current_Header, 1);
}

static void uncollapse_thread (void)
{
   if (0 == check_article_mode ())
     slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
}

static int thread_size (void)
{
   if (check_article_mode ()) return -1;
   return (int) slrn_thread_size (Slrn_Current_Header);
}
   
static int is_thread_collapsed (void)
{
   if (check_article_mode ()) return -1;
   return slrn_is_thread_collapsed (Slrn_Current_Header);
}

static int header_down (int *num) /*{{{*/
{
   if ((-1 == check_article_mode ())
       || (*num <= 0))
     return 0;
   
   return slrn_header_down_n (*num, 0);
}

/*}}}*/

static int header_up (int *num) /*{{{*/
{
   if ((-1 == check_article_mode ())
       || (*num <= 0))
     return 0;
   
   return slrn_header_up_n (*num, 0);
}

/*}}}*/

static int header_next_unread (void)
{
   if (-1 == check_article_mode ())
     return -1;
   
   return slrn_next_unread_header ();
}

static int get_header_flags (void) /*{{{*/
{
   if ((-1 == check_article_mode ())
       || (Slrn_Current_Header == NULL))
     return 0;
   return (int) (Slrn_Current_Header->flags & HEADER_HARMLESS_FLAGS_MASK);
}

/*}}}*/

static void set_header_flags (int *flagsp) /*{{{*/
{
   unsigned int flags;
   if ((-1 == check_article_mode ())
       || (Slrn_Current_Header == NULL))
     return;
   
   Slrn_Current_Header->flags &= ~HEADER_HARMLESS_FLAGS_MASK;
   flags = ((unsigned int) *flagsp) & HEADER_HARMLESS_FLAGS_MASK;
   Slrn_Current_Header->flags |= flags;
}

/*}}}*/

static int get_header_tag_number (void)
{
   if ((Slrn_Current_Header == NULL) 
       || (0 == (Slrn_Current_Header->flags & HEADER_NTAGGED)))
     return 0;
   
   return (int) Slrn_Current_Header->tag_number;
}

static void set_header_score (int *score)
{
   (void) slrn_set_header_score (Slrn_Current_Header, *score, 0);
}

static int get_header_score (void)
{
   if ((-1 == check_article_mode ())
       || (Slrn_Current_Header == NULL))
     return 0;

#if SLRN_HAS_SORT_BY_SCORE
   return Slrn_Current_Header->score;
#else
   if (Slrn_Current_Header->flags & HEADER_LOW_SCORE)
     return -1;
   return 0;
#endif
}

/*{{{ Header Searching */

static int re_header_search (char *pat, unsigned int offset, int dir) /*{{{*/
{
   SLRegexp_Type *re;
   Slrn_Header_Type *h = Slrn_Current_Header;
   
   if ((-1 == check_article_mode ())
       || (h == NULL)
       || (NULL == (re = slrn_compile_regexp_pattern (pat))))
     return 0;
   
   while (h != NULL)
     {
	if (NULL != slrn_regexp_match (re, *(char **) ((char *)h + offset)))
	  {
	     slrn_goto_header (h, 0);
	     return 1;
	  }
	
	if (dir > 0)
	  h = h->next;
	else
	  h = h->prev;
     }
   return 0;   
}

/*}}}*/

static int re_subject_search_forward (char *pat)
{
   Slrn_Header_Type h;
   return re_header_search (pat, (char *) &h.subject - (char *)&h, 1);
}


static int re_subject_search_backward (char *pat)
{
   Slrn_Header_Type h;
   return re_header_search (pat, (char *) &h.subject - (char *)&h, -1);
}

static int re_author_search_forward (char *pat)
{
   Slrn_Header_Type h;
   return re_header_search (pat, (char *) &h.from - (char *)&h, 1);
}

static int re_author_search_backward (char *pat)
{
   Slrn_Header_Type h;
   return re_header_search (pat, (char *) &h.from - (char *)&h, -1);
}
/*}}}*/

/*}}}*/

/*}}}*/

/*{{{ Group Functions */

static int Interp_Group_Unsubscribed = GROUP_UNSUBSCRIBED;
static int Interp_Group_New_Group_Flag = GROUP_NEW_GROUP_FLAG;

static int check_group_mode (void)
{
   if ((Slrn_Current_Mode == NULL)
       || (Slrn_Current_Mode->mode != SLRN_GROUP_MODE))
     {
	error ("Not in group mode.");
	return -1;
     }
   return 0;
}

static int is_group_mode (void)
{
   return ((Slrn_Current_Mode != NULL)
	   && (Slrn_Current_Mode->mode == SLRN_GROUP_MODE));
}

static int group_down_n (int *np)
{
   int n = *np;
   
   if (n < 0) return slrn_group_up_n (-n);
   return slrn_group_down_n (n);
}

static int group_up_n (int *np)
{
   int n = *np;
   
   if (n < 0) return slrn_group_down_n (-n);
   return slrn_group_up_n (n);
}

static char *current_group_name (void)
{
   if (Slrn_Group_Current_Group == NULL)
     return "";
   
   return Slrn_Group_Current_Group->name;
}

static int get_group_unread_count (void)
{
   if (Slrn_Group_Current_Group == NULL) return 0;
   return Slrn_Group_Current_Group->unread;
}

static int select_group (void)
{
   if (-1 == check_group_mode ())
     return -1;
   
   return slrn_group_select_group ();
}

static int get_group_flags (void)
{	
   if (Slrn_Group_Current_Group == NULL)
     return 0;
   
   return Slrn_Group_Current_Group->flags & GROUP_HARMLESS_FLAGS_MASK;
}

static void set_group_flags (int *flagsp)
{
   unsigned int flags;
   
   if (Slrn_Group_Current_Group == NULL)
     return;
   
   Slrn_Group_Current_Group->flags &= ~GROUP_HARMLESS_FLAGS_MASK;
   flags = ((unsigned int) *flagsp) & GROUP_HARMLESS_FLAGS_MASK;
   Slrn_Group_Current_Group->flags |= flags;
}

/*}}}*/

static SLang_Name_Type Slrn_Intrinsics [] = /*{{{*/
{   
   MAKE_INTRINSIC(".set_header_score", set_header_score, VOID_TYPE, 1),
   MAKE_INTRINSIC(".get_header_score", get_header_score, INT_TYPE, 0),
   MAKE_INTRINSIC(".article_as_string", article_as_string, STRING_TYPE, 0),
   MAKE_INTRINSIC(".call", slrn_call_command, VOID_TYPE, 1),
   MAKE_INTRINSIC(".collapse_thread", collapse_thread, VOID_TYPE, 0),
   MAKE_INTRINSIC(".collapse_threads", collapse_threads, VOID_TYPE, 0),
   MAKE_INTRINSIC(".current_newsgroup", current_group_name, STRING_TYPE, 0),
   MAKE_INTRINSIC(".definekey", definekey, VOID_TYPE, 3),
   MAKE_INTRINSIC(".error", error, VOID_TYPE, 1),
   MAKE_INTRINSIC(".extract_article_header", extract_article_header, STRING_TYPE, 1),
   MAKE_INTRINSIC(".get_group_flags", get_group_flags, INT_TYPE, 0),
   MAKE_INTRINSIC(".get_header_flags", get_header_flags, INT_TYPE, 0),
   MAKE_INTRINSIC(".get_header_tag_number", get_header_tag_number, INT_TYPE, 0),
   MAKE_INTRINSIC(".get_select_box_response", interp_select_box, INT_TYPE, 1),
   MAKE_INTRINSIC(".get_variable_value", get_variable_value, VOID_TYPE, 0),
   MAKE_INTRINSIC(".get_yes_no_cancel", get_yesno_cancel, INT_TYPE, 1),
   MAKE_INTRINSIC(".getkey", getkey, INT_TYPE, 0),
   MAKE_INTRINSIC(".goto_num_tagged_header", slrn_goto_num_tagged_header, INT_TYPE, 1),
   MAKE_INTRINSIC(".group_down_n", group_down_n, INT_TYPE, 1),
   MAKE_INTRINSIC(".group_search", slrn_group_search, INT_TYPE, 1),
   MAKE_INTRINSIC(".group_unread", get_group_unread_count, INT_TYPE, 0),
   MAKE_INTRINSIC(".group_up_n", group_up_n, INT_TYPE, 1),
   MAKE_INTRINSIC(".header_down", header_down, INT_TYPE, 1),
   MAKE_INTRINSIC(".header_next_unread", header_next_unread, INT_TYPE, 0),
   MAKE_INTRINSIC(".header_up", header_up, INT_TYPE, 1),
   MAKE_INTRINSIC(".input_pending", input_pending, INT_TYPE, 1),
   MAKE_INTRINSIC(".is_article_visible", slrn_is_article_visible, INT_TYPE, 0),
   MAKE_INTRINSIC(".is_group_mode", is_group_mode, INT_TYPE, 0),
   MAKE_INTRINSIC(".is_thread_collapsed", is_thread_collapsed, INT_TYPE, 0),
   MAKE_INTRINSIC(".make_home_filename", make_home_filename, VOID_TYPE, 1),
   MAKE_INTRINSIC(".message", message, VOID_TYPE, 1),
   MAKE_INTRINSIC(".next_tagged_header", slrn_next_tagged_header, INT_TYPE, 0),
   MAKE_INTRINSIC(".pipe_article", pipe_article_cmd, VOID_TYPE, 1),
   MAKE_INTRINSIC(".prev_tagged_header", slrn_prev_tagged_header, INT_TYPE, 0),
   MAKE_INTRINSIC(".quit", quit, VOID_TYPE, 1),
   MAKE_INTRINSIC(".re_bsearch_author", re_author_search_backward, INT_TYPE, 1),
   MAKE_INTRINSIC(".re_bsearch_subject", re_subject_search_backward, INT_TYPE, 1),
   MAKE_INTRINSIC(".re_fsearch_author", re_author_search_forward, INT_TYPE, 1),
   MAKE_INTRINSIC(".re_fsearch_subject", re_subject_search_forward, INT_TYPE, 1),
   MAKE_INTRINSIC(".re_search_article", re_search_article, INT_TYPE, 1),
   MAKE_INTRINSIC(".read_mini", read_mini, VOID_TYPE, 0),
   MAKE_INTRINSIC(".read_mini_no_echo", read_mini_no_echo, VOID_TYPE, 0),
   MAKE_INTRINSIC(".save_current_article", save_current_article, INT_TYPE, 1),
   MAKE_INTRINSIC(".search_article", search_article, INT_TYPE, 1),
   MAKE_INTRINSIC(".select_group", select_group, INT_TYPE, 0),
   MAKE_INTRINSIC(".server_name", get_server_name, STRING_TYPE, 0),
   MAKE_INTRINSIC(".set_article_window_size", set_article_window_size, VOID_TYPE, 1),
   MAKE_INTRINSIC(".set_group_flags", set_group_flags, VOID_TYPE, 1),
   MAKE_INTRINSIC(".set_header_flags", set_header_flags, VOID_TYPE, 1),
   MAKE_INTRINSIC(".set_input_string", set_input_string, VOID_TYPE, 0),
   MAKE_INTRINSIC(".set_integer_variable", set_integer_variable, VOID_TYPE, 0),
   MAKE_INTRINSIC(".set_prefix_argument", set_prefix_arg, VOID_TYPE, 1),
   MAKE_INTRINSIC(".set_string_variable", set_string_variable, VOID_TYPE, 0),
   MAKE_INTRINSIC(".thread_size", thread_size, INT_TYPE, 0),
   MAKE_INTRINSIC(".tt_send", tt_send, VOID_TYPE, 0),
   MAKE_INTRINSIC(".uncollapse_thread", uncollapse_thread, VOID_TYPE, 0),
   MAKE_INTRINSIC(".uncollapse_threads", uncollapse_threads, VOID_TYPE, 0),
   MAKE_INTRINSIC(".undefinekey", undefinekey, VOID_TYPE, 2),
   MAKE_INTRINSIC(".ungetkey", ungetkey, VOID_TYPE, 1),
   MAKE_INTRINSIC(".update", update, VOID_TYPE, 0),
   MAKE_VARIABLE(".GROUPS_DIRTY", &Slrn_Groups_Dirty, INT_TYPE, 1),
   MAKE_VARIABLE(".GROUP_NEW_GROUP_FLAG", &Interp_Group_New_Group_Flag, INT_TYPE, 1),
   MAKE_VARIABLE(".GROUP_UNSUBSCRIBED", &Interp_Group_Unsubscribed, INT_TYPE, 1),
   MAKE_VARIABLE(".HEADER_HIGH_SCORE", &Interp_Header_High_Score, INT_TYPE, 1),
   MAKE_VARIABLE(".HEADER_LOW_SCORE", &Interp_Header_Low_Score, INT_TYPE, 1),
   MAKE_VARIABLE(".HEADER_READ", &Interp_Header_Read, INT_TYPE, 1),
   MAKE_VARIABLE(".HEADER_TAGGED", &Interp_Header_Tagged, INT_TYPE, 1),
   MAKE_VARIABLE(".SCREEN_HEIGHT", &SLtt_Screen_Rows, INT_TYPE, 1),
   MAKE_VARIABLE(".SCREEN_WIDTH", &SLtt_Screen_Cols, INT_TYPE, 1),
   SLANG_END_TABLE
};

/*}}}*/

static int interp_system (char *s) /*{{{*/
{
   return slrn_posix_system (s, 1);
}

/*}}}*/

int slrn_init_slang (void) /*{{{*/
{
   Slrn_Use_Slang = 0;
   if (!init_SLang()		       /* basic interpreter functions */
       || !init_SLmath() 	       /* sin, cos, etc... */
#ifdef __unix__
       || !init_SLunix()	       /* unix system calls */
#endif
       || !init_SLfiles()	       /* file i/o */
       
       /* Now add intrinsics for this application */
       || !SLang_add_table(Slrn_Intrinsics, "slrn"))
     return -1;
   
   SLadd_name ("system", (long) interp_system, SLANG_INTRINSIC, SLANG_MAKE_ARGS(INT_TYPE, 1));
   
   SLang_Error_Routine = error;

   Slrn_Use_Slang = 1;
   SLang_User_Clear_Error = slrn_clear_message;
   return 0;
}

/*}}}*/

#endif
