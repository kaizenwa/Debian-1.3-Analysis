/* -*- mode: C; mode: fold; -*- */
/*  Copyright (c) 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"
#include "slrnfeat.h"

/*{{{ system include files */
#include <stdio.h>
#include <string.h>
#include <time.h>


#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <ctype.h>
#include <slang.h>
#include "jdmacros.h"


#ifndef isalpha
# define isalpha(x) \
  ((((x) <= 'Z') && ((x) >= 'A')) \
   || (((x) <= 'z') && ((x) >= 'a')))
#endif

#ifndef isspace
# define isspace(x) (((x) == ' ') || ((x) == '\t'))
#endif

/*}}}*/
/*{{{ slrn include files */
#include "slrn.h"
#include "group.h"
#include "server.h"
#include "art.h"
#include "misc.h"
#include "post.h"
/* #include "clientlib.h" */
#include "startup.h"
#include "hash.h" 
#include "score.h"
#include "menu.h"
#include "util.h"
#include "xover.h"
#include "chmap.h"

#if SLRN_HAS_UUDEVIEW
# include <uudeview.h>
#endif
#include "uudecode.h"

#if SLRN_HAS_MIME 
# include "mime.h"
#endif

#if SLRN_HAS_GROUPLENS
# include "grplens.h"
#endif

/*}}}*/

/*{{{ extern Global variables  */

SLKeyMap_List_Type *Slrn_Article_Keymap;
char *Slrn_X_Browser;
char *Slrn_NonX_Browser;
char *Slrn_Quote_String;
char *Slrn_Save_Directory;
char *Slrn_Header_Help_Line;
char *Slrn_Art_Help_Line;
char *Slrn_Followup_Custom_Headers;
char *Slrn_Reply_Custom_Headers;

#if SLRN_HAS_TILDE_FEATURE
int Slrn_Use_Tildes = 1;
#endif

int Slrn_Startup_With_Article = 0;
int Slrn_Show_Author = 2;		       /* If non-zero, show author in header */
int Slrn_Show_Author_Realname = 1;

int Slrn_Query_Next_Article = 1;
int Slrn_Query_Next_Group = 1;
int Slrn_Auto_CC_To_Poster = 0;
int Slrn_Score_After_XOver;
int Slrn_Use_Tmpdir = 0;
int Slrn_Threads_Visible = 0;
int Slrn_Use_Header_Numbers = 1;
#if SLRN_HAS_SORT_BY_SCORE
int Slrn_Display_Score;
#endif
int Slrn_High_Score_Min = 1;
int Slrn_Low_Score_Max = 0;
int Slrn_Kill_Score_Max = -9999;

int Slrn_Reads_Per_Update = 50;
int Slrn_Sig_Is_End_Of_Article = 0;
#if SLRN_HAS_SPOILERS
int Slrn_Spoiler_Char = 42;
int Slrn_Spoiler_Display_Mode = 1;
#endif
int Slrn_New_Subject_Breaks_Threads = 0;

char *Slrn_Current_Group_Name;
Slrn_Header_Type *Slrn_First_Header;
Slrn_Header_Type *Slrn_Current_Header;

/* range of articles on server for current group */
int Slrn_Server_Min, Slrn_Server_Max;

/* Sorting mode:
 *   0 No sorting
 *   1 sort by threads
 *   2 sort by subject
 *   3 sort by subject and threads
 *   4 sort by score
 *   5 sort by score and threads
 *   6 sort by score then by subject
 *   7 thread, then sort by score then subject
 *   8 sort by date
 */
#define SORT_BY_THREADS  1
#define SORT_BY_SUBJECT  2
#if SLRN_HAS_SORT_BY_SCORE
# define SORT_BY_SCORE   4
#endif
#define SORT_BY_DATE	8

int Slrn_Sorting_Mode = (SORT_BY_THREADS|SORT_BY_SUBJECT);

#define ALL_THREAD_FLAGS (FAKE_PARENT | FAKE_CHILDREN | FAKE_HEADER_HIGH_SCORE)

Slrn_Article_Line_Type *Slrn_Article_Lines;

/*}}}*/
/*{{{ static global variables */
static SLscroll_Window_Type Slrn_Article_Window;
static SLscroll_Window_Type Slrn_Header_Window;

static int Header_Window_Nrows;
static unsigned int Number_Killed;
static unsigned int Number_High_Scored;
static unsigned int Number_Low_Scored;
static int User_Aborted_Group_Read;
#if SLRN_HAS_SPOILERS
static Slrn_Header_Type *Spoilers_Visible;
#endif

#if SLRN_HAS_GROUPLENS
static int Num_GroupLens_Rated = -1;
#endif
static Slrn_Header_Type *Mark_Header;  /* header with mark set */

static Slrn_Group_Type *Current_Group; /* group being processed */

static int Total_Num_Headers;	       /* headers retrieved from server.  This
					* number is used only by update meters */
static Slrn_Header_Type *Headers;
static int Last_Cursor_Row;	       /* row where --> cursor last was */
static Slrn_Header_Type *Header_Showing;    /* header whose article is selected */
static Slrn_Header_Type *Last_Read_Header;
static int Article_Visible;	       /* non-zero if article window is visible */
static char Output_Filename[256];
static Slrn_Mode_Type *Last_Current_Mode;
static int Headers_Threaded;

/* If +1, threads are all collapsed.  If zero, none are.  If -1, some may
 * be and some may not.  In other words, if -1, this variable should not
 * be trusted.
 */
static int Threads_Collapsed = 0;


#define HEADER_TABLE_SIZE 1250
static Slrn_Header_Type *Header_Table[HEADER_TABLE_SIZE];
static int Headers_Hidden_Mode = 1;
static int Quotes_Hidden = 0;
static char *Super_Cite_Regexp = "^[^A-Za-z0-9]*\"\\([-_a-zA-Z/]+\\)\" == .+";
static int Do_Rot13;
static int Perform_Scoring;
static int Largest_Header_Number;
static int Article_Window_Nrows;
static int HScroll;
static SLKeymap_Function_Type *Art_Functions_Ptr;
static Slrn_Article_Line_Type *Article_Current_Line;


static Slrn_Header_Type *At_End_Of_Article;
/* If this variable is NULL, then we are not at the end of an article.  If it
 * points at the current article, the we are at the end of that article.
 * If it points anywhere else, ignore it.
 */

/*}}}*/
/*{{{ static function declarations */

static void slrn_art_hangup (int);
static void sort_threads (void);
static void hide_quotes (void);
static void wrap_article (void);
static void art_update_screen (void);
static void art_next_unread (void);
static void (*Slrn_Group_Hangup_Hook) (int);
static void thread_headers (void);
static void sort_by_threads (void);
static void sort_by_sorting_mode (void);
static int select_article (int);
static Slrn_Article_Line_Type *unwrap_line (Slrn_Article_Line_Type *);
static void quick_help (void);
static void for_this_tree (Slrn_Header_Type *, void (*)(Slrn_Header_Type *));
static void find_non_hidden_header (void);

static void skip_to_next_group (void);

#if SLRN_HAS_SPOILERS
static void show_spoilers (void);
#endif
/*}}}*/

/*{{{ utility functions */

static int is_blank_line (unsigned char *b) /*{{{*/
{
   b = (unsigned char *) slrn_skip_whitespace ((char *) b);
   return (*b == 0);
}

/*}}}*/

static char *map_char_to_string (int ch) /*{{{*/
{
   static char charbuf[8];
   switch (ch)
     {
      case ' ': 	return "SPACE";
      case '\t': 	return "TAB";
      case '\r': 	return "RETURN";
      case 27: 		return "ESCAPE";
      case 127: 	return "DELETE";
      case 8: 		return "BACKSPACE";
     }

   if (ch < 32)
     {
	sprintf (charbuf, "Ctrl-%c", ch + '@');
	return charbuf;
     }
   
   sprintf (charbuf, "'%c'", ch);
   return charbuf;
}

/*}}}*/

/*}}}*/

/*{{{ SIGWINCH and window resizing functions */
static void art_winch (void) /*{{{*/
{
   static int rows;
   
   if ((rows != SLtt_Screen_Rows) 
       || (Article_Window_Nrows <= 0)
       || (Article_Window_Nrows >= SLtt_Screen_Rows - 4))
     {
	rows = SLtt_Screen_Rows;
	
	Article_Window_Nrows = (3 * rows) / 4;
	if (rows <= 28)
	  Article_Window_Nrows = rows - 8;
     }

   if (Article_Visible)
     Header_Window_Nrows = rows - Article_Window_Nrows - 4;
   else
     Header_Window_Nrows = rows - 3;

   if (Header_Window_Nrows < 1) Header_Window_Nrows = 1;
   if (Article_Window_Nrows < 1) Article_Window_Nrows = 1;

   Slrn_Article_Window.nrows = Article_Window_Nrows;
   Slrn_Header_Window.nrows = Header_Window_Nrows;

   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void set_article_visibility (int visible)
{
   Article_Visible = visible;
   art_winch ();
}

static void art_winch_sig (int old_r, int old_c) /*{{{*/
{
   (void) old_c;
   if (old_r != SLtt_Screen_Rows)
     Article_Window_Nrows = 0;
   
   art_winch ();
}

/*}}}*/

static void shrink_window (void) /*{{{*/
{
   if (Article_Visible == 0) return;
   Article_Window_Nrows++;
   art_winch ();
}

/*}}}*/

static void enlarge_window (void) /*{{{*/
{
   if (Article_Visible == 0) return;
   Article_Window_Nrows--;
   art_winch ();
}

/*}}}*/

void slrn_set_article_window_size (int nrows)
{
   Article_Window_Nrows = nrows;
   art_winch ();
}

/*}}}*/
/*{{{ header hash functions */
static void delete_hash_table (void) /*{{{*/
{
   SLMEMSET ((char *) Header_Table, 0, sizeof (Header_Table));
}

/*}}}*/

static void make_hash_table (void) /*{{{*/
{
   Slrn_Header_Type *h;
   delete_hash_table ();
   h = Slrn_First_Header;
   while (h != NULL)
     {
	h->hash_next = Header_Table[h->hash % HEADER_TABLE_SIZE];
	Header_Table[h->hash % HEADER_TABLE_SIZE] = h;
	h = h->real_next;
     }
}

/*}}}*/

/*}}}*/

/*{{{ article line specific functions */

static void find_article_line_num (void) /*{{{*/
{
   Slrn_Article_Line_Type *l;
   
   /* Make sure Article_Current_Line is not hidden */
   l = Article_Current_Line;
   while ((l != NULL) && (l->flags & HIDDEN_LINE))
     l = l->prev;
   if (l == NULL)
     l = Article_Current_Line;
   while ((l != NULL) && (l->flags & HIDDEN_LINE))
     l = l->next;
   
   Article_Current_Line = l;

   Slrn_Article_Window.current_line = (SLscroll_Type *) Article_Current_Line;
   
   /* Force current line to be at top of window */
   Slrn_Article_Window.top_window_line = Slrn_Article_Window.current_line;
   Slrn_Full_Screen_Update = 1;
   
   SLscroll_find_line_num (&Slrn_Article_Window);
}

/*}}}*/

static void init_article_window_struct (void) /*{{{*/
{
   Slrn_Article_Window.hidden_mask = HIDDEN_LINE;
   Slrn_Article_Window.current_line = (SLscroll_Type *) Article_Current_Line;
   Slrn_Article_Window.cannot_scroll = SLtt_Term_Cannot_Scroll;
   Slrn_Article_Window.lines = (SLscroll_Type *) Slrn_Article_Lines;
   Slrn_Article_Window.border = 0;
   art_winch ();		       /* set nrows element */
   find_article_line_num ();
}

/*}}}*/

static void free_article (void) /*{{{*/
{
   Slrn_Article_Line_Type *l, *next;
   
   l = Slrn_Article_Lines;
   while (l != NULL)
     {
	SLFREE (l->buf);
	next = l->next;
	SLFREE (l);
	l = next;
     }
   
   SLMEMSET((char *) &Slrn_Article_Window, 0, sizeof(SLscroll_Window_Type));
   
   Article_Current_Line = Slrn_Article_Lines = NULL;
   Header_Showing = NULL;
   set_article_visibility (0);
}

/*}}}*/

/* Does NOT update line numbers */
static void hide_art_headers (void) /*{{{*/
{
   Slrn_Article_Line_Type *l = Slrn_Article_Lines;
   char ch, ch1, ch2;
   
   while ((l != NULL) && ((ch = *l->buf) != 0))
     {
	int hide_header;
	
	ch |= 0x20;
	ch1 = l->buf[1];
	if (ch1) ch2 = l->buf[2]; else ch2 = 0;
	ch1 |= 0x20;
	ch2 |= 0x20;
	l->flags = HEADER_LINE;
	
	hide_header = 0;
	switch (ch)
	  {
	   case 'r':		       /* references, etc... */
	     if (slrn_case_strncmp ((unsigned char *)l->buf, 
				     (unsigned char *) "Reply-To:", 9))
	       hide_header = 1;
	     break;
	   case 'n':		       /* nntp */
	     if (ch1 == 'e') break;
	     /* drop */
	   case 'i':		       /* In-Reply-To */
	   case 'x':		       /* user defined */
	   case 'a':		       /* approved */
	   case 'p':		       /* path */
	   case 'l':		       /* lines */
	   case 'm':		       /* msgid, mime */
	   case 'c':		       /* content */
	   case 't':		       /* To */
	     hide_header = 1;
	     break;
	     
	   case 'd':		       /* Distribution */
	     if (ch1 == 'a') break;    /* Date ok */
	     hide_header = 1;
	     break;
	     
	   case 's':		       /* sender etc... */
	     if ((ch1 == 'u') && (ch2 == 'b')) break;
	     hide_header = 1;
	  }
	
	do
	  {
	     l->flags |= HEADER_LINE;
	     if (Headers_Hidden_Mode && hide_header)
	       {
		  l->flags |= HIDDEN_LINE;
	       }
	     else l->flags &= ~HIDDEN_LINE;
	     
	     l = l->next;
	  }
	while ((l != NULL) && ((*l->buf == ' ') || (*l->buf == '\t')));
     }

   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void skip_quoted_text (void) /*{{{*/
{
   Slrn_Article_Line_Type *l = Article_Current_Line;
   
   /* look for a quoted line */
   while (l != NULL)
     {
	if ((l->flags & HIDDEN_LINE) == 0)
	  {
	     Article_Current_Line = l;
	     if (l->flags & QUOTE_LINE) break;
	  }
	l = l->next;
     }
   
   /* Now we are either at the end of the buffer or on a quote line. Skip
    * past other quote lines.
    */
   
   if (l == NULL) 
     return;
   
   l = l->next;
   
   while (l != NULL)
     {
	if (l->flags & HIDDEN_LINE)
	  {
	     l = l->next;
	     continue;
	  }
	Article_Current_Line = l;
	if ((l->flags & QUOTE_LINE) == 0)
	  {
	     /* Check to see if it is blank */
	     if (is_blank_line ((unsigned char *) l->buf) == 0) break;
	  }
	l = l->next;
     }
   
   find_article_line_num ();
}

/*}}}*/

static void skip_digest_forward (void) /*{{{*/
{
   Slrn_Article_Line_Type *l;
   int num_passes;
   
   /* We are looking for:
    * <blank line>  (actually, most digests do not have this-- even the FAQ that suggests it!!)
    * ------------------------------
    * <blank line>
    * Subject: something
    *
    * In fact, most digests do not conform to this.  So, I will look for:
    * <blank line>
    * Subject: something
    *
    * Actually, most faqs, etc... do not support this.  So, look for any line
    * beginning with a number on second pass.  Sigh.
    */
   num_passes = 0;
   while (num_passes < 2)
     {
	l = Article_Current_Line;
	if (l != NULL) l = l->next;
	
	while (l != NULL)
	  {
	     char ch;
	     char *buf;
	     
	     if ((l->flags & HIDDEN_LINE) || (l->flags & HEADER_LINE))
	       {
		  l = l->next;
		  continue;
	       }
	     
	     buf = l->buf;
	     if (num_passes == 0)
	       {
		  if ((strncmp ("Subject:", buf, 8))
		      || (((ch = buf[8]) != ' ') && (ch != '\t')))
		    {
		       l = l->next;
		       continue;
		    }
	       }
	     else
	       {
		  ch = *buf;
		  if ((ch > '9') || (ch < '0'))
		    {
		       l = l->next;
		       continue;
		    }
	       }
	     
	     Article_Current_Line = l;
	     find_article_line_num ();
	     return;
	  }
	num_passes++;
     }
   slrn_error ("No next digest.");
}

/*}}}*/

static int try_supercite (void) /*{{{*/
{
   Slrn_Article_Line_Type *l = Slrn_Article_Lines, *last, *lsave;
   static unsigned char compiled_pattern_buf[256];
   static SLRegexp_Type re;
   unsigned char *b;
   int count;
   char name[32];
   unsigned int len;
   int ret;
   
   re.pat = (unsigned char *) Super_Cite_Regexp;
   re.buf = compiled_pattern_buf;
   re.case_sensitive = 1;
   re.buf_len = sizeof (compiled_pattern_buf);
   
   /* skip header --- I should look for Xnewsreader: gnus */
   while ((l != NULL) && (*l->buf != 0)) l = l->next;
   
   if ((*compiled_pattern_buf == 0) && SLang_regexp_compile (&re))
     return -1;
   
   /* look at the first 15 lines on first attempt.
    * After that, scan the whole buffer looking for more citations */
   count = 15;
   lsave = l;
   ret = -1;
   while (1)
     {
	while (count && (l != NULL))
	  {
	     if ((l->flags & QUOTE_LINE) == 0)
	       {
		  if (NULL != slrn_regexp_match (&re, l->buf))
		    {
		       l->flags |= QUOTE_LINE;
		       break;
		    }
	       }
	     l = l->next;
	     count--;
	  }
	
	if ((l == NULL) || (count == 0)) return ret;
	
	/* Now find out what is used for citing. */
	b = (unsigned char *) l->buf + re.beg_matches[1];
	len = re.end_matches[1];
	if (len > sizeof (name) - 2) return ret;
	
	ret = 0;
	strncpy (name, (char *) b, len); name[len] = 0;
	/* strcat (name, ">");
	 len++; */
	
	while (l != NULL)
	  {
	     unsigned char ch;
	     
	     b = (unsigned char *) l->buf;
	     last = l;
	     l = l->next;
	     if (last->flags & QUOTE_LINE) continue;
	     
	     b = (unsigned char *) slrn_skip_whitespace ((char *) b);

	     if (!strncmp ((char *) b, name, len)
		 && (((ch = b[len] | 0x20) < 'a')
		     || (ch > 'z')))
	       {
		  last->flags |= QUOTE_LINE;
		  
		  while (l != NULL)
		    {
		       b = (unsigned char *) slrn_skip_whitespace (l->buf);
		       if (strncmp ((char *) b, name, len)
			   || (((ch = b[len] | 0x20) >= 'a')
			       && (ch <= 'z')))
			 break;
		       l->flags |= QUOTE_LINE;
		       l = l->next;
		    }
	       }
	  }
	count = -1;
	l = lsave;
     }
}

/*}}}*/

#if SLRN_HAS_SPOILERS
static void mark_spoilers (void) /*{{{*/
{
   Slrn_Article_Line_Type *l = Slrn_Article_Lines;
   int spoiler = 0;
   
   /* skip header */
   while ((l != NULL) && (l->flags & HEADER_LINE))
     l = l->next;
   
   while (l != NULL)
     {
	if ((l->buf[0] == 12) && (l->buf[1] == 0))
	  {
	     spoiler = 1;
	  }
	else if (spoiler)
	  {
	     l->flags |= SPOILER_LINE;
	  }
	l = l->next;
     }   
}

/*}}}*/
#endif

static void mark_quotes (void) /*{{{*/
{
   Slrn_Article_Line_Type *l, *last;
   unsigned char *b;
   SLRegexp_Type **r;
   
   if (0 == try_supercite ())
     {
	/* return; */
     }
   
   if (Slrn_Ignore_Quote_Regexp[0] == NULL) return;
   
   /* skip header */
   l = Slrn_Article_Lines;
   while ((l != NULL) && (*l->buf != 0)) l = l->next;
   
   while (l != NULL)
     {
	unsigned int min_len;
	b = (unsigned char *) l->buf;
	min_len = strlen ((char *) b);
	last = l;
	
	l = l->next;		       /* if first time through, this skips
					* blank line at at last header.
					*/
	
	r = Slrn_Ignore_Quote_Regexp;
	
	while (*r != NULL)
	  {
	     SLRegexp_Type *re;
	     
	     re = *r;
	     if ((re->min_length <= min_len) &&
		 (NULL != SLang_regexp_match (b, min_len, re)))
	       {
		  last->flags |= QUOTE_LINE;
		  while (l != NULL)
		    {
		       b = (unsigned char *) l->buf;
		       min_len = strlen ((char *) b);
		       if ((re->min_length <= min_len)
			   && (NULL == SLang_regexp_match (b, min_len, re)))
			 {
			    
			    /* Here it might be a good idea to add:
			     * l = l->prev;
			     */
			    break;
			 }
		       l->flags |= QUOTE_LINE;
		       l = l->next;
		    }
		  break;
	       }
	     r++;
	  }
     }
}

/*}}}*/

static void mark_signature (void) /*{{{*/
{
   Slrn_Article_Line_Type *l = Slrn_Article_Lines;
   int nmax = 10;
   
   if (l == NULL) return;
   /* go to end of article */
   while (l->next != NULL) l = l->next;
   
   /* skip back untill "-- " seen.  Assume that it is not more than
    * 10 lines long.
    */
   
   while ((l != NULL) && nmax--
	  && ((*l->buf != '-')
	      || strcmp (l->buf, "-- ")))
     l = l->prev;
   
   if (nmax == -1) return;
   
   while (l != NULL)
     {
        l->flags |= SIGNATURE_LINE;
        l->flags &= ~(
		      QUOTE_LINE  /* if in a signature, not a quote */
#if SLRN_HAS_SPOILERS
		      | SPOILER_LINE     /* not a spoiler */
#endif
		      );
	l = l->next;
     }
}

/*}}}*/

   
char *slrn_extract_header (char *hdr, unsigned int len) /*{{{*/
{
   Slrn_Article_Line_Type *l = Slrn_Article_Lines;
   
   while ((l != NULL)
	  && (*l->buf != 0))
     {
	if (0 == slrn_case_strncmp ((unsigned char *) hdr,
				    (unsigned char *) l->buf, len))
	  {
	     if ((l->next != NULL) && (l->next->flags & WRAPPED_LINE))
	       unwrap_line (l->next);
	     
	     return l->buf + len;
	  }
	l = l->next;
     }
   return NULL;
}

/*}}}*/

/*{{{ wrap article functions  */
/* The input line is assumed to be the first wrapped portion of a line.  For
 * example, if a series of lines denoted as A/B is wrapped: A0/A1/A2/B0/B1,
 * then to unwrap A, A1 is passed and B0 is returned.
 */
static Slrn_Article_Line_Type *unwrap_line (Slrn_Article_Line_Type *l) /*{{{*/
{
   char *b;
   Slrn_Article_Line_Type *next, *ll;
	     
   ll = l->prev;
   b = ll->buf;
   do 
     {
	b += strlen (b);
	strcpy (b, l->buf + 1);   /* skip the space at beginning of
				   * the wrapped line. */
	next = l->next;
	SLFREE (l->buf);
	SLFREE (l);
	if (l == Article_Current_Line) Article_Current_Line = ll;
	l = next;
     }
   while ((l != NULL) && (l->flags & WRAPPED_LINE));
	     
   ll->next = l;
   if (l != NULL) l->prev = ll;
   return l;
}

/*}}}*/

int Slrn_Wrap_Mode = 3;
static void wrap_article (void) /*{{{*/
{
   unsigned int len;
   unsigned char *buf, ch;
   Slrn_Article_Line_Type *l = Slrn_Article_Lines;
   int was_wrapped = 0;
   unsigned int wrap_mode = Slrn_Wrap_Mode;

   if (Slrn_Prefix_Arg_Ptr != NULL) wrap_mode = 0x7F;
   Slrn_Prefix_Arg_Ptr = NULL;
   
   if (Header_Showing == NULL) return;
   /* First scan through to see if it needs unwrapped. */
   while (l != NULL)
     {
	if (l->flags & WRAPPED_LINE)
	  {
	     l = unwrap_line (l);
	     was_wrapped = 1;
	  }
	else l = l->next;
     }
   
   l = Slrn_Article_Lines;

   if (was_wrapped == 0) while (l != NULL)
     {
	if (l->flags & HEADER_LINE) 
	  {
	     if ((wrap_mode & 1) == 0)
	       {
		  l = l->next;
		  continue;
	       }
	  }
	else if (l->flags & QUOTE_LINE)
	  {
	     if ((wrap_mode & 2) == 0)
	       {
		  l = l->next;
		  continue;
	       }
	  }

	len = 0;
	buf = (unsigned char *) l->buf;
	ch = *buf;
	while (ch != 0)
	  {
	     if ((ch == '\t') && (SLsmg_Tab_Width > 0))
	       {
		  len += SLsmg_Tab_Width;
		  len -= len % SLsmg_Tab_Width;
	       }
	     else if (((ch >= ' ') && (ch < 127))
		      || (ch >= (unsigned char) SLsmg_Display_Eight_Bit))
	       len++;
	     else
	       {
		  len += 2;
		  if (ch & 0x80) len++;
	       }

	     if ((len > (unsigned int) SLtt_Screen_Cols)
		 && (ch != ' ') && (ch != '\t'))
	       {
		  Slrn_Article_Line_Type *new_l;

		  /* Start wrapped lines with a space */
		  buf--;
		  ch = *buf;
		  *buf = ' ';
		  
		  new_l = (Slrn_Article_Line_Type *) slrn_malloc (sizeof (Slrn_Article_Line_Type), 1, 1);
		  if (new_l == NULL)
		    return;
		  
		  if (NULL == (new_l->buf = slrn_strmalloc ((char *)buf, 1)))
		    {
		       SLFREE (new_l);
		       return;
		    }

		  *buf++ = ch;
		  *buf = 0;
		  
		  new_l->next = l->next;
		  new_l->prev = l;
		  l->next = new_l;
		  if (new_l->next != NULL) new_l->next->prev = new_l;

		  new_l->flags = l->flags | WRAPPED_LINE;
		  
		  l = new_l;
		  buf = (unsigned char *) new_l->buf;
		  len = 0;
	       }
	     else buf++;
	     
	     ch = *buf;
	  }
	l = l->next;
     }
   Slrn_Full_Screen_Update = 1;
   find_article_line_num ();
}

/*}}}*/

/*}}}*/

Slrn_Article_Line_Type *slrn_search_article (char *string, /*{{{*/
					     char **ptrp,
					     int is_regexp,
					     int set_current_flag)
{
   SLsearch_Type st;
   Slrn_Article_Line_Type *l;
   char *ptr;
   int ret;
   SLRegexp_Type *re = NULL;
   
   if (-1 == (ret = select_article (1)))
     return NULL;
   
   if (Article_Current_Line == NULL)
     return NULL;
   
   if (is_regexp)
     {
	re = slrn_compile_regexp_pattern (string);
	if (re == NULL)
	  return NULL;
     }
   else SLsearch_init (string, 1, 0, &st);

   l = Article_Current_Line;
   
   if (ret == 1)
     l = l->next;
   
   while (l != NULL)
     {
	if ((l->flags & HIDDEN_LINE) == 0)
	  {
	     if (is_regexp)
	       ptr = (char *) slrn_regexp_match (re, l->buf);
	     else
	       ptr = (char *) SLsearch ((unsigned char *) l->buf,
					(unsigned char *) l->buf + strlen (l->buf),
					&st);
	     
	     if (ptr != NULL)
	       {
		  if (ptrp != NULL) *ptrp = ptr;
		  if (set_current_flag)
		    {
		       Article_Current_Line = l;
		       find_article_line_num ();
		    }
		  break;
	       }
	  }
	l = l->next;
     }
      
   return l;
}

/*}}}*/

#define URL_SIG_STR "://"
#define BROWSER_BUFLEN 4096
static char *find_url (char *buf) /*{{{*/
{
   Slrn_Article_Line_Type *l;
   char *ptr, *tmp, ch, *s;
   unsigned int len;
   
   if (NULL == (l = slrn_search_article (URL_SIG_STR, &ptr, 0, 1)))
     return NULL;
   
   s = l->buf;
   
   if (ptr > s) ptr--;
   
   while ((ptr >= s) 
	  && isalpha (*ptr))
     ptr--;
   
   ptr++;
   tmp = ptr;
     
   while ((ch = *ptr) != 0)
     {
	if (isalpha(ch))
	  {
	     ptr++;
	     continue;
	  }
	
	if (ch == '.')
	  {
	     ptr++;
	     ch = *ptr;
	     if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == 0))
	       {
		  ptr--;
		  break;
	       }
	     continue;
	  }
	
	if ((ch == ' ')
	    || (ch == '\t')
	    || (ch == '\n')
	    || (ch == '\"')
	    || (ch == '}')
	    || (ch == '{')
	    || (ch == ')')
	    || (ch == ',')
	    || (ch == ';')
	    || (ch == '>'))
	  break;
	ptr++;
     }
   
   len = (unsigned int) (ptr - tmp);
   if (len >= BROWSER_BUFLEN) len = BROWSER_BUFLEN - 1;
   strncpy (buf, tmp, len);
   buf[len] = 0;
   return buf;
}

/*}}}*/

static void browse_url (void) /*{{{*/
{
   char command [BROWSER_BUFLEN];
   char url[BROWSER_BUFLEN];
   char *browser, *has_percent;
   int reinit;
   
   if ((NULL == getenv ("DISPLAY")) || (NULL == Slrn_X_Browser))
     browser = Slrn_NonX_Browser;
   else browser = Slrn_X_Browser;
        
   if (browser == NULL)
     {
	slrn_error ("No Web Browser has been defined.");
	return;
     }
   
   /* Perform a simple-minded syntax check. */
   has_percent = slrn_strchr (browser, '%');
   if (has_percent != NULL)
     {
	if ((has_percent[1] != 's') 
	    || ((has_percent != browser) && (*(has_percent - 1) == '\\')))
	  has_percent = NULL;
     }
   
   if (NULL == find_url (url))
     {
	slrn_error ("No URLs found.");
	return;
     }
   
   if (slrn_read_input ("Browse (^G aborts)", NULL, url, 0) <= 0)
     {
	slrn_error ("Aborted.");
	return;
     }
  
   if (has_percent != NULL)
     sprintf (command, browser, url);
   else
     sprintf (command, "%s %s", browser, url);

   reinit = (browser != Slrn_X_Browser);
   if (reinit == 0)
     {
	/* Non_X and X browsers may be same. */
	if ((Slrn_NonX_Browser != NULL) && (0 == strcmp (Slrn_NonX_Browser, browser)))
	  reinit = 1;
     }
   
   slrn_posix_system (command, reinit);
   /* if (reinit) slrn_redraw (); */
}

/*}}}*/

static void article_search (void) /*{{{*/
{
   static char search_str[256];
   Slrn_Article_Line_Type *l;
   
   if (slrn_read_input ("Search", search_str, NULL, 0) <= 0) return;
   
   l = slrn_search_article (search_str, NULL, 0, 1);
   
   if (l == NULL) slrn_error ("Not found.");
}

/*}}}*/

/*}}}*/
/*{{{ current article movement functions */


static unsigned int art_lineup_n (unsigned int n) /*{{{*/
{
   if (select_article (1) <= 0) return 0;
   
   n = SLscroll_prev_n (&Slrn_Article_Window, n);
   
   Article_Current_Line = (Slrn_Article_Line_Type *) Slrn_Article_Window.current_line;
   
   /* Force current line to be at top of window */
   Slrn_Article_Window.top_window_line = Slrn_Article_Window.current_line;
   Slrn_Full_Screen_Update = 1;
   return n;
}

/*}}}*/

static void art_pageup (void) /*{{{*/
{
   if (select_article (1) <= 0)
     return;
   /* Since we always require the current line to be at the top of the
    * window, SLscroll_pageup cannot be used.  Instead, do it this way:
    */
   art_lineup_n (Slrn_Article_Window.nrows - 1);
}

/*}}}*/

static void art_pagedn (void) /*{{{*/
{
   unsigned char ch, ch1;
   char *msg = NULL;
   
   if (Slrn_Current_Header == NULL) return;

#if SLRN_HAS_SPOILERS
   if (Spoilers_Visible == Slrn_Current_Header)
     {
	show_spoilers ();
	return;
     }
#endif
   
   
   if ((Article_Visible == 0) || (At_End_Of_Article != Slrn_Current_Header))
     {
	int av = Article_Visible;
	
	At_End_Of_Article = NULL;

	if ((select_article (1) <= 0)
	    || (av == 0))
	  return;

	SLscroll_pagedown (&Slrn_Article_Window);
	Article_Current_Line = (Slrn_Article_Line_Type *) Slrn_Article_Window.current_line;
	/* Force current line to be at top of window */
	Slrn_Article_Window.top_window_line = Slrn_Article_Window.current_line;
	Slrn_Full_Screen_Update = 1;
	return;
     }

   At_End_Of_Article = NULL;

   if (Slrn_Batch) return;
   
   if (Slrn_Current_Header->next == NULL)
     {
	if (Slrn_Query_Next_Group)
	  msg = "At end of article, press %s for next group.";
     }
   else if (Slrn_Query_Next_Article)
     msg = "At end of article, press %s for next unread article.";
   
   if ((ch1 = SLang_Last_Key_Char) == 27) ch1 = ' ';
   ch = ch1;
	
   if (msg != NULL)
     {
	slrn_message_now (msg, map_char_to_string (ch));
	ch = SLang_getkey ();
     }
   
   if (ch == ch1)
     {
	At_End_Of_Article = NULL;
	if (Slrn_Current_Header->next != NULL) art_next_unread ();
	else skip_to_next_group ();
     }
   else SLang_ungetkey (ch);
}
   

/*}}}*/

static void art_lineup (void) /*{{{*/
{
   art_lineup_n (1);
}

/*}}}*/

static void art_bob (void) /*{{{*/
{
   while (0xFFFF == art_lineup_n (0xFFFF));
}

/*}}}*/


static unsigned int art_linedn_n (unsigned int n) /*{{{*/
{
   if (select_article (1) <= 0)
     return 0;

   n = SLscroll_next_n (&Slrn_Article_Window, n);
   Article_Current_Line = (Slrn_Article_Line_Type *) Slrn_Article_Window.current_line;
   
   /* Force current line to be at top of window */
   Slrn_Article_Window.top_window_line = Slrn_Article_Window.current_line;
   Slrn_Full_Screen_Update = 1;
   return n;
}

/*}}}*/


static void art_linedn (void) /*{{{*/
{
   art_linedn_n (1);
}

/*}}}*/

static void art_eob (void) /*{{{*/
{
   while (art_linedn_n (0xFFFF) > 0)
     ;
   (void) art_lineup_n (Slrn_Article_Window.nrows / 2);
}

/*}}}*/


/*}}}*/

/*{{{ Tag functions */

typedef struct /*{{{*/
{
   Slrn_Header_Type **headers;
   unsigned int max_len;
   unsigned int len;
}

/*}}}*/
Num_Tag_Type;

static Num_Tag_Type Num_Tag_List;

static void free_tag_list (void) /*{{{*/
{
   if (Num_Tag_List.headers != NULL)
     {
	SLFREE (Num_Tag_List.headers);
	Num_Tag_List.headers = NULL;
	Num_Tag_List.len = Num_Tag_List.max_len = 0;
     }
}

/*}}}*/

int slrn_goto_num_tagged_header (int *nump) /*{{{*/
{
   unsigned int num;
   
   num = (unsigned int) *nump;
   num--;
   
   if (num >= Num_Tag_List.len)
     return 0;
   
   if (Num_Tag_List.headers == NULL)
     return 0;
   
   if (-1 == slrn_goto_header (Num_Tag_List.headers[num], 0))
     return 0;
   
   Slrn_Full_Screen_Update = 1;
   return 1;
}

/*}}}*/

static void num_tag_header (void) /*{{{*/
{
   unsigned int len;
   
   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   if (Num_Tag_List.headers == NULL)
     {
	Slrn_Header_Type **headers;
	unsigned int max_len = 20;
	
	headers = (Slrn_Header_Type **) slrn_malloc (max_len * sizeof (Slrn_Header_Type),
						     0, 1);
	if (headers == NULL)
	  return;

	Num_Tag_List.max_len = max_len;
	Num_Tag_List.headers = headers;
	Num_Tag_List.len = 0;
     }
   
   if (Num_Tag_List.max_len == Num_Tag_List.len)
     {
	Slrn_Header_Type **headers = Num_Tag_List.headers;
	unsigned int max_len = Num_Tag_List.max_len + 20;
	
	headers = (Slrn_Header_Type **) slrn_realloc ((char *)headers,
						      max_len * sizeof (Slrn_Header_Type),
						      1);
	if (headers == NULL)
	  return;
	
	Num_Tag_List.max_len = max_len;
	Num_Tag_List.headers = headers;
     }
   
   Slrn_Full_Screen_Update = 1;
   if ((Slrn_Current_Header->flags & HEADER_NTAGGED) == 0)
     {
	Num_Tag_List.headers[Num_Tag_List.len] = Slrn_Current_Header;
	Num_Tag_List.len += 1;
	Slrn_Current_Header->tag_number = Num_Tag_List.len;
	Slrn_Current_Header->flags |= HEADER_NTAGGED;
	(void) slrn_header_down_n (1, 0);
	return;
     }
   
   /* It is already tagged.  So, what do we do.  The most sensible thing to
    * do is to simply give this header the last number and renumber the others
    * that follow it.  If it is the last one, untag it.
    */
   if (Slrn_Current_Header->tag_number == Num_Tag_List.len)
     {
	Num_Tag_List.len -= 1;
	Slrn_Current_Header->tag_number = 0;
	Slrn_Current_Header->flags &= ~HEADER_NTAGGED;
	return;
     }
   
   for (len = Slrn_Current_Header->tag_number + 1; len <= Num_Tag_List.len; len++)
     {
	Slrn_Header_Type *h = Num_Tag_List.headers[len - 1];
	Num_Tag_List.headers[len - 2] = h;
	h->tag_number -= 1;
     }
   Num_Tag_List.headers[len - 2] = Slrn_Current_Header;
   Slrn_Current_Header->tag_number = len - 1;
}

/*}}}*/

static void num_untag_headers (void) /*{{{*/
{
   unsigned int len;
   for (len = 1; len <= Num_Tag_List.len; len++)
     {
	Slrn_Header_Type *h = Num_Tag_List.headers[len - 1];
	h->flags &= ~HEADER_NTAGGED;
	h->tag_number = 0;
     }
   Num_Tag_List.len = 0;
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void toggle_one_header_tag (Slrn_Header_Type *h) /*{{{*/
{
   if (h == NULL) return;
   if (h->flags & HEADER_TAGGED)
     {
	h->flags &= ~HEADER_TAGGED;
     }
   else h->flags |= HEADER_TAGGED;
}

/*}}}*/

static void toggle_header_tag (void) /*{{{*/
{
   if (Slrn_Prefix_Arg_Ptr != NULL)
     {
	Slrn_Header_Type *h;
	
	Slrn_Prefix_Arg_Ptr = NULL;
	h = Headers;
	while (h != NULL)
	  {
	     h->flags &= ~HEADER_TAGGED;
	     h = h->next;
	  }
	Slrn_Full_Screen_Update = 1;
	return;
     }

   if ((Slrn_Current_Header->parent != NULL)/* in middle of thread */
       || (Slrn_Current_Header->child == NULL)/* At top with no child */
       /* or at top with child showing */
       || (0 == (Slrn_Current_Header->child->flags & HEADER_HIDDEN)))
     {
	toggle_one_header_tag (Slrn_Current_Header);
     }
   else
     {
	for_this_tree (Slrn_Current_Header, toggle_one_header_tag);
     }
   (void) slrn_header_down_n (1, 0);
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

int slrn_prev_tagged_header (void) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_Current_Header;
   
   if (h == NULL) return 0;
   
   while (h->prev != NULL)
     {
	h = h->prev;
	if (h->flags & HEADER_TAGGED)
	  {
	     slrn_goto_header (h, 0);
	     return 1;
	  }
     }
   return 0;
}

/*}}}*/

int slrn_next_tagged_header (void) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_Current_Header;
   
   if (h == NULL) return 0;
   
   while (h->next != NULL)
     {
	h = h->next;
	if (h->flags & HEADER_TAGGED)
	  {
	     slrn_goto_header (h, 0);
	     return 1;
	  }
     }
   return 0;
}

/*}}}*/

/*}}}*/
/*{{{ Header specific functions */

static void find_header_line_num (void) /*{{{*/
{
   Slrn_Full_Screen_Update = 1;
   find_non_hidden_header ();
   Slrn_Header_Window.lines = (SLscroll_Type *) Headers;
   Slrn_Header_Window.current_line = (SLscroll_Type *) Slrn_Current_Header;
   SLscroll_find_line_num (&Slrn_Header_Window);
}

/*}}}*/

static void init_header_window_struct (void) /*{{{*/
{
   Slrn_Header_Window.nrows = 0;
   Slrn_Header_Window.hidden_mask = HEADER_HIDDEN;
   Slrn_Header_Window.current_line = (SLscroll_Type *) Slrn_Current_Header;
   
   Slrn_Header_Window.cannot_scroll = SLtt_Term_Cannot_Scroll;
   Slrn_Header_Window.border = 1;
   
   if (Slrn_Scroll_By_Page)
     {
	/* Slrn_Header_Window.border = 0; */
	Slrn_Header_Window.cannot_scroll = 2;
     }

   Slrn_Header_Window.lines = (SLscroll_Type *) Headers;
   art_winch ();		       /* get row information correct */
   
   find_header_line_num ();
}

/*}}}*/

static Slrn_Header_Type *find_header_from_serverid (int id) /*{{{*/
{
   Slrn_Header_Type *h;
   
   h = Slrn_First_Header;
   while (h != NULL)
     {
	if (h->number > id) return NULL;
	if (h->number == id) break;
	h = h->real_next;
     }
   return h;
}

/*}}}*/

static void kill_cross_references (Slrn_Header_Type *h) /*{{{*/
{
   char *b;
   char group[256], *g;
   long num;
   
   if ((h->xref == NULL) || (*h->xref == 0))
     {
	if ((Header_Showing != h)
	    || (NULL == (b = slrn_extract_header ("Xref:", 5))))
	  {
	     return;
	  }
     }
   else b = h->xref;
   
   
   /* The format appears to be:
    * Xref: machine group:num group:num...
    */
   
   /* skip machine name */
   while (*b > ' ') b++;
   
   while (*b != 0)
     {
	while (*b == ' ') b++;
	if (*b == 0) break;
	
	/* now we are looking at the groupname */
	g = group;
	while (*b && (*b != ':')) *g++ = *b++;
	if (*b++ == 0) break;
	*g = 0;
	num = atoi (b);
	while ((*b <= '9') && (*b >= '0')) b++;
	if ((num != h->number)
	    || strcmp (group, Slrn_Current_Group_Name))
	  slrn_mark_article_as_read (group, num);
     }
}

/*}}}*/

static void for_all_headers (void (*func)(Slrn_Header_Type *), int all) /*{{{*/
{
   Slrn_Header_Type *h, *end;
   
   Slrn_Full_Screen_Update = 1;

   if (func == NULL) return;
   
   if (all) end = NULL; else end = Slrn_Current_Header;
   
   h = Headers;
   
   while (h != end)
     {
	(*func)(h);
	h = h->next;
     }
}

/*}}}*/

int slrn_goto_header (Slrn_Header_Type *header, int read_flag) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_First_Header;
   
   while ((h != NULL) && (h != header))
     h = h->real_next;

   if (h == NULL) return -1;
   
   Slrn_Current_Header = h;
   if (h->flags & HEADER_HIDDEN) slrn_uncollapse_this_thread (h, 0);
   find_header_line_num ();
   
   if (read_flag) select_article (1);
   return 0;
}

/*}}}*/

/*{{{ parse_from  */

static char *parse_from (char *from) /*{{{*/
{
   static char buf[256], *b;
   char ch, ch1;
   
   /* Here I am assume the following:
    * 1.  parenthesis and double quotes are comments.
    * 2.  If a name is in <> it is given precedence
    */
   
   if (from == NULL) return NULL;
   from = slrn_skip_whitespace (from);
   
   b = buf;
   
   /* strip comments */
   while ((ch = *from++) != 0)
     {
	if (ch == '"')
	  {
	     ch1 = ch;
	     while (((ch = *from++) != 0) && (ch != ch1)) 
	       ;
	     if (ch == 0) return NULL;
	  }
	else if (ch == '(')
	  {
	     int nump = 1;
	     ch1 = ')';
	     
	     while ((ch = *from++) != 0)
	       {
		  if (ch == '(') nump++;
		  else if (ch == ch1)
		    {
		       nump--;
		       if (nump == 0) break;
		    }
	       }
	     if (ch == 0) return NULL;
	  }
	else if (ch == ')') return NULL;
	else if (ch != ' ') *b++ = ch;
     }
   *b = 0;
   
   from = b = buf;
   
   while ((ch = *from++) != 0)
     {
	if (ch == '<')
	  {
	     while (((ch = *from++) != 0) && (ch != '>'))
	       {
		  *b++ = ch;
	       }
	     if (ch == 0) return NULL;
	     *b = 0;
	     break;
	  }
     }
   return buf;
}

/*}}}*/


/*}}}*/

/*{{{ fixup_realname */
static void fixup_realname (Slrn_Header_Type *h) /*{{{*/
{
   char *p, *pmin, *pmax;
   if ((h->realname == NULL) || (h->realname_len == 0)) return;
   pmin = h->realname;
   p = pmin + (h->realname_len - 1);
   while ((p >= pmin) && ((*p == ' ') || (*p == '\t'))) p--;
   p++;
   h->realname_len = (unsigned int) (p - pmin);
   
   /* Check if realname is enclosed in "" and if so strip them off.
    * Leave "" alone though.
    */
   if ((p > pmin + 2) &&
       (*pmin == '"') && (*(p - 1) == '"'))
     {
	h->realname++;
	h->realname_len -= 2;
     }
   
   /* Look for a character in the range [A-Za-z].  If none, found use from.
    */
   p = h->realname;
   pmax = p + h->realname_len;
   while (p < pmax)
     {
	char ch = *p;
	
	if (isalpha (ch)) return;
	p++;
     }
   
   h->realname = parse_from (h->from);
   
   if (h->realname == NULL)
     h->realname = h->from;
   
   h->realname_len = strlen (h->realname);
}

/*}}}*/

/*}}}*/

/*{{{ get_header_real_name */

static void get_header_real_name (Slrn_Header_Type *h) /*{{{*/
{
   register char *f, *f0, *f1;
   register char ch;
   char *from = h->from;
   int n;
   
   f = from;
   while (((ch = *f) != 0) && (ch != '<') && (ch != '(')) f++;
   if (ch == '(')
     {
	/* Look for a '<'.  There is some probability that this is of the
	 * form:  John Doe (800) 555-1212 <doe@nowhere.com>
	 */
	f0 = f;
	f1 = f + strlen (f);
	while ((f1 > f) && (*f1 != ')') && (*f1 != '<')) f1--;
	if (*f1 == '<')
	  {
	     ch = '<';
	     f = f1;
	  }
     }
   
   if (ch != '(')
     {
	if (ch == '<')
	  {
	     f1 = slrn_skip_whitespace (from);
	     if (*f1 == '<')
	       {
		  f = f1 + 1;
		  from = f;
		  while (((ch = *f) != 0) && (ch != '>')) f++;
	       }
	  }
	h->realname = from;
	h->realname_len = (unsigned int) (f - from);
	fixup_realname (h);
	return;
     }
   
   f1 = f;
   f0 = f + 1;
   f0 = slrn_skip_whitespace (f0);
   if (*f0 == ')')
     {
	h->realname = from;
	h->realname_len = (unsigned int) (f1 - from);
	fixup_realname (h);
	return;
     }
   f = f0;
   n = 0;
   while ((ch = *f) != 0)
     {
	if (ch == '(') n++;
	else if (ch == ')')
	  {
	     if (n == 0) break;
	     n--;
	  }
	f++;
     }
   h->realname = f0;
   h->realname_len = (unsigned int) (f - f0);
   fixup_realname (h);
}

/*}}}*/

/*}}}*/

static void extract_real_names (void) /*{{{*/
{
   Slrn_Header_Type *h = Headers;
   while (h != NULL)
     {
	get_header_real_name (h);
	h = h->next;
     }
}

/*}}}*/

static Slrn_Header_Type *find_header_from_msgid (char *r0, char *r1) /*{{{*/
{
   unsigned long hash;
   Slrn_Header_Type *h;
   unsigned int len;
   len = (unsigned int) (r1 - r0);
   hash = slrn_compute_hash ((unsigned char *) r0, (unsigned char *) r1);
   
   h = Header_Table[hash % HEADER_TABLE_SIZE];
   while (h != NULL)
     {
	if (!slrn_case_strncmp ((unsigned char *) h->msgid,
				(unsigned char *) r0,
				len))
	  break;
	
	h = h->hash_next;
     }
   return h;
}

/*}}}*/

Slrn_Header_Type *slrn_find_header_with_msgid (char *msgid) /*{{{*/
{
   return find_header_from_msgid (msgid, msgid + strlen (msgid));
}

/*}}}*/

static void goto_article (void) /*{{{*/
{
   Slrn_Header_Type *h;
   int want_n;
   
   if (-1 == slrn_read_integer ("Goto article: ", NULL, &want_n))
     return;
   
   h = Headers;
   while (h != NULL)
     {
	if (h->number == want_n)
	  {
	     Slrn_Current_Header = h;
	     if (h->flags & HEADER_HIDDEN) slrn_uncollapse_this_thread (h, 0);
	     find_header_line_num ();
	     return;
	  }
	
	h = h->next;
     }
   
   slrn_error ("Article not found.");
}

/*}}}*/

int slrn_is_article_visible (void)
{
   int mask = 0;
   
   if ((Slrn_Current_Header != NULL)
       && Article_Visible)
     {
	if (Slrn_Current_Header == Header_Showing)
	  mask = 3;
	else
	  mask = 1;
     }
   
   return mask;
}

/*}}}*/

static int unfold_art_header_lines (void)
{
   Slrn_Article_Line_Type *l;
   char ch;
   
   l = Slrn_Article_Lines;
   if (l == NULL) return 0;
   l = l->next;
   
   while ((l != NULL) && (0 != (ch = l->buf[0])))
     {
	if ((ch == ' ') || (ch == '\t'))
	  {
	     unsigned int len0, len1;
	     Slrn_Article_Line_Type *prev;
	     char *new_buf;
	     
	     l->buf[0] = ' ';
	     
	     prev = l->prev;
	     
	     len0 = strlen (prev->buf);
	     len1 = len0 + strlen (l->buf) + 1;
	     
	     new_buf = slrn_realloc (prev->buf, len1, 1);
	     if (new_buf == NULL)
	       return -1;

	     prev->buf = new_buf;
	     
	     strcpy (new_buf + len0, l->buf);
	     prev->next = l->next;
	     if (l->next != NULL) l->next->prev = prev;
	     SLFREE (l->buf);
	     SLFREE (l);
	     l = prev;
	  }
	
	l = l->next;
     }
   return 0;
}

	
static int read_article (Slrn_Header_Type *h, int kill_refs, int do_mime) /*{{{*/
{
   Slrn_Article_Line_Type *l;
   unsigned int total_lines;
   char buf[NNTP_BUFFER_SIZE], *b, *b1, ch;
   unsigned int len;
   int n = h->number;
   Slrn_Header_Type *last_header_showing;
   int num_lines_update;
   int status;

   last_header_showing = Header_Showing;
   
   if (Header_Showing == h)
     {
#if SLRN_HAS_MIME
	if (Slrn_Use_Mime == 0)
	  return 0;
	if ((do_mime && Slrn_Mime_Was_Parsed)
	    || ((do_mime == 0) && (Slrn_Mime_Was_Modified == 0)))
#endif
	  return 0; /* The cached copy is good */
     }
   
   if (h->tag_number) slrn_message_now ("#%2d/%-2d: Retrieving... %s", 
				    h->tag_number, Num_Tag_List.len, 
				    h->subject);
   else slrn_message_now ("[%d]Reading...", h->number);

   status = Slrn_Server_Obj->sv_select_article (n, h->msgid);
   if (status != OK_ARTICLE)
     {
	if (status == -1)
	  {
	     slrn_error ("Server failed to return article.");
	     return -1;
	  }

	slrn_error ("Article %d unavailable.", n);
	
	if (kill_refs && ((h->flags & HEADER_READ) == 0))
	  {
	     kill_cross_references (h);
	     h->flags |= HEADER_READ;
	  }
	return -1;
     }
   
   
   At_End_Of_Article = NULL;
   
   Do_Rot13 = 0;
   HScroll = 0;
   Slrn_Full_Screen_Update = 1;
   
   free_article ();
   
   if ((num_lines_update = Slrn_Reads_Per_Update) < 5)
     {
	if (h->lines < 200)
	  num_lines_update = 20;
	else 
	  num_lines_update = 50;
     }
   
   total_lines = 0;
   while (Slrn_Server_Obj->sv_read_line(buf, sizeof(buf)) != NULL)
     {
	if (SLang_Error == USER_BREAK)
	  {
	     slrn_error ("Article transfer aborted.");
	     if (Slrn_Article_Lines == NULL)
	       return -1;
	     break;
	  }
	
	total_lines++;
	if ((1 == (total_lines % num_lines_update))
	    /* Just so the ratio does not confuse the reader because of the
	     * header lines...
	     */
	    && (total_lines < (unsigned int) h->lines))
	  {
	     if (h->tag_number)
	       slrn_message_now ("#%2d/%-2d: Read %4d/%-4d lines (%s)",
				 h->tag_number, Num_Tag_List.len,
				 total_lines, h->lines, h->subject);
	     else
	       slrn_message_now ("[%d]Read %d/%d lines so far...", 
				 h->number, total_lines, h->lines);
	  }
	
	len = strlen (buf);
	
	l = (Slrn_Article_Line_Type *) slrn_malloc (sizeof(Slrn_Article_Line_Type),
						    1, 1);
	if ((l == NULL)
	    || (NULL == (l->buf = slrn_malloc (len + 1, 0, 1))))
	  {
	     slrn_free ((char *) l);
	     free_article ();
	     return -1;
	  }
	
	/* here I am going to remove _^H combinations.  Later, it will be a
	 * good idea to perform the implied underlining
	 */
	b1 = l->buf;
	b = buf;
	
	if ((*b == '.') && (*(b + 1) == '.')) b++;
	
	while (0 != (ch = *b++))
	  {
	     if ((ch == '_') && (*b == '\b'))
	       {
		  b++;
	       }
	     else *b1++ = ch;
	  }
	*b1 = 0;
	
	l->next = l->prev = NULL;
	l->flags = 0;
	
	if (Slrn_Article_Lines == NULL)
	  {
	     Slrn_Article_Lines = l;
	  }
	else
	  {
	     /* l->next = Article_Current_Line->next; */
	     l->prev = Article_Current_Line;
	     Article_Current_Line->next = l;
	     
	     /* if (l->next != NULL) l->next->prev = l; */
	  }
	Article_Current_Line = l;
     }
   
   Article_Current_Line = Slrn_Article_Lines;

   if (-1 == unfold_art_header_lines ())
     {
	free_article ();
	return -1;
     }
   
   if (kill_refs && ((h->flags & HEADER_READ) == 0))
     {
	kill_cross_references (h);
	h->flags |= HEADER_READ;
     }
   
   /* Do this now so that header lines are marked.  Mime processing 
    * assumes this.
    */
   if (h == Slrn_Current_Header) hide_art_headers ();

   /* This must be called before any of the other functions are called because
    * they may depend upon the line number and window information.
    */
   init_article_window_struct ();


#if SLRN_HAS_MIME
   if (Slrn_Use_Mime && (h == Slrn_Current_Header))
     {
	/* Note: the mime routines assume that the article flags are valid.
	 * That is, a header line is given by line->flags & HEADER_LINE
	 */
	slrn_mime_article_init ();
	if (do_mime) slrn_mime_process_article ();
     }
#endif

   slrn_chmap_fix_body ();
   
   if (h == Slrn_Current_Header)
     {
	mark_quotes ();
#if SLRN_HAS_SPOILERS
	if (Slrn_Spoiler_Char) mark_spoilers ();
#endif
	mark_signature ();
	hide_quotes ();
     }
      
   if (last_header_showing != h)
     {
	Last_Read_Header = last_header_showing;
     }
   Header_Showing = h;
   
   if (h == Slrn_Current_Header)
     {
	if (Slrn_Wrap_Mode & 0x4) wrap_article ();
	if (Last_Read_Header == NULL)
	  Last_Read_Header = h;
     }
   
   /* slrn_set_suspension (0); */
   return 0;
}

/*}}}*/

/*{{{ reply, reply_cmd, forward, followup */

static int insert_followup_format (char *f, FILE *fp) /*{{{*/
{
   char ch, *s, *smax;
   
   while ((ch = *f++) != 0)
     {
	if (ch != '%')
	  {
	     putc (ch, fp);
	     continue;
	  }
	s = smax = NULL;
	ch = *f++;
	if (ch == 0) break;
	
	switch (ch)
	  {
	   case 's':
	     s = Slrn_Current_Header->subject;
	     break;
	   case 'm':
	     s = Slrn_Current_Header->msgid;
	     break;
	   case 'r':
	     s = Slrn_Current_Header->realname;
	     smax = s + Slrn_Current_Header->realname_len;
	     break;
	   case 'f':
	     s = parse_from (Slrn_Current_Header->from);
	     break;
	   case 'n':
	     s = slrn_extract_header ("Newsgroups: ", 12);
	     break;
	   case 'd':
	     s = Slrn_Current_Header->date;
	     break;
	   case '%':
	   default:
	     putc (ch, fp);
	  }
	
	if (s == NULL) continue;
	if (smax == NULL) fputs (s, fp);
	else fwrite (s, 1, (unsigned int) (smax - s), fp);
     }
   return 0;
}

/*}}}*/

static void reply (void) /*{{{*/
{
   char *msgid, *from, *subject, *f;
   Slrn_Article_Line_Type *l;
   FILE *fp;
   char file[256];
   unsigned int n;

   if (-1 == slrn_check_batch ())
     return;
   
   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   
   if (read_article (Slrn_Current_Header, 1, 1) < 0) return;

   /* Check for FQDN.  If it appear bogus, warn user */
   
   if (NULL == (f = slrn_extract_header ("Reply-To: ", 10)))
     {
	f = slrn_extract_header ("From: ", 6);
     }
   
   from = parse_from (f);
   
   if ((from == NULL) 
       || (NULL == (f = slrn_strchr (from, '@')))
       || (f == from)
       || (0 == slrn_is_fqdn (f + 1)))
     {
	if (0 == slrn_get_yesno (1, "%s appears invalid.  Continue anyway",
				 ((from == NULL) ? "Email address" : from)))
	  return;
     }
   
   
   if (Slrn_Use_Tmpdir)
     {
	fp = slrn_open_tmpfile (file, "w");
     }
   else fp = slrn_open_home_file (SLRN_LETTER_FILENAME, "w", file, 0);
   
   if (NULL == fp)
     {
	slrn_error ("Unable to open %s for writing.", file);
	return;
     }
   
   /* parse header */
   msgid = slrn_extract_header ("Message-ID: ", 12);
   subject = slrn_extract_header ("Subject: ", 9);

   if (subject == NULL) subject = "";
   subject = slrn_skip_whitespace (subject);
   if (0 == slrn_case_strncmp ((unsigned char *)"Re:", (unsigned char *)subject, 3))
     subject = slrn_skip_whitespace (subject + 3);

   n = 0;
   fprintf (fp, "To: %s\nSubject: Re: %s\nIn-Reply-To: %s\n",
	    (from == NULL ? "" : from),
	    subject,
	    (msgid == NULL ? "" : msgid));
   n += 3;
   
#ifdef __os2__
   fprintf (fp, "From: %s@%s (%s)\n", 
	    Slrn_User_Info.username, Slrn_User_Info.host,
	    Slrn_User_Info.realname);
   n += 1;
#endif
   
   if (0 != *Slrn_User_Info.replyto)
     {
	fprintf (fp, "Reply-To: %s\n", Slrn_User_Info.replyto);
	n += 1;
     }
   
   n += slrn_add_custom_headers (fp, Slrn_Reply_Custom_Headers, insert_followup_format);
   
   fputs ("\n", fp);
   
   fprintf (fp, "In article %s, you wrote:\n", (msgid == NULL ? "" : msgid));
   
   n += 2;

   l = Slrn_Article_Lines;
   if (Slrn_Prefix_Arg_Ptr == NULL)
     {
	while ((l != NULL) && (*l->buf != 0)) l = l->next;
	if (l != NULL) l = l->next;
     }
   
   while (l != NULL)
     {
	fprintf (fp, "%s%s\n", 
		 ((Slrn_Quote_String == NULL) ? ">" : Slrn_Quote_String), 
		 l->buf);
	l = l->next;
     }
   slrn_add_signature (fp);
   slrn_fclose (fp);
   
   slrn_mail_file (file, 1, n, from, subject);
   if (Slrn_Use_Tmpdir) (void) slrn_delete_file (file);
}

/*}}}*/

static void reply_cmd (void) /*{{{*/
{
   if (-1 == slrn_check_batch ())
     return;

   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   
   if (read_article (Slrn_Current_Header, 1, 1) < 0) return;
   
   if (Slrn_User_Wants_Confirmation
       && (slrn_get_yesno (1, "Are you sure you want to reply") <= 0))
     return;
   reply ();
}

/*}}}*/

static void forward_article (void) /*{{{*/
{
   char *subject;
   Slrn_Article_Line_Type *l;
   FILE *fp;
   char file[256];
   char to[256];
   int edit;
   
   if (-1 == slrn_check_batch ())
     return;

   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   if (read_article (Slrn_Current_Header, 1, 1) < 0) return;
   
   *to = 0;
   if (slrn_read_input ("Forward to (^G aborts)", NULL, to, 1) <= 0)
     {
	slrn_error ("Aborted.  An email address is required.");
	return;
     }
   
   if (-1 == (edit = slrn_get_yesno_cancel ("Edit the message before sending")))
     return;
   

   if (Slrn_Use_Tmpdir)
     {
	fp = slrn_open_tmpfile (file, "w");
     }
   else fp = slrn_open_home_file (SLRN_LETTER_FILENAME, "w", file, 0);
   
   if (fp == NULL)
     {
	slrn_error ("Unable to open %s for writing.", file);
	return;
     }
   
   
   subject = slrn_extract_header ("Subject: ", 9);
   
   fprintf (fp, "To: %s\nSubject: Fwd: %s\n",
	    to, subject == NULL ? "" : subject);
   
#ifdef __os2__
   fprintf (fp, "From: %s@%s (%s)\n", 
	    Slrn_User_Info.username, Slrn_User_Info.host,
	    Slrn_User_Info.realname);
#endif
   
   if (0 != *Slrn_User_Info.replyto)
     fprintf (fp, "Reply-To: %s (%s)\n", 
	      Slrn_User_Info.replyto, Slrn_User_Info.realname);
   putc ('\n', fp);
   
   l = Slrn_Article_Lines;
   while (l != NULL)
     {
	fprintf (fp, "%s\n", l->buf);
	l = l->next;
     }
   slrn_fclose (fp);
      
   (void) slrn_mail_file (file, edit, 3, to, subject);

   if (Slrn_Use_Tmpdir) slrn_delete_file (file);
}

/*}}}*/



/* If prefix arg is 1, insert all headers.  If it is 2, insert all headers
 * but do not quote text nor attach signature.  2 is good for re-posting.
 */
static void followup (void) /*{{{*/
{
   char *msgid, *from, *newsgroups, *subject, *xref, *quote_str;
   Slrn_Article_Line_Type *l;
   FILE *fp;
   char file[256];
   unsigned int n;
   int prefix_arg;
   int perform_cc;

   if (-1 == slrn_check_batch ())
     return;

   if (Slrn_Post_Obj->po_can_post == 0)
     {
	slrn_error ("Posting not allowed by server.");
	return;
     }
   
   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   if (read_article (Slrn_Current_Header, 1, 1) < 0) return;

   if (Slrn_User_Wants_Confirmation
       && (slrn_get_yesno (1, "Are you sure you want to followup") <= 0))
     return;
   
   
   if (NULL != (newsgroups = slrn_extract_header ("Followup-To: ", 13)))
     {
	newsgroups = slrn_skip_whitespace (newsgroups);
	if ((!slrn_case_strncmp ((unsigned char *) newsgroups,
				(unsigned char *) "poster", 6))
	  /* The GNU newsgroups appear to have email addresses in the
	   * Followup-To header.  Yuk.
	   */
	    || (NULL != slrn_strchr (newsgroups, '@')))
	  {
	     if (slrn_get_yesno (1, "Do you want to reply to POSTER as poster prefers"))
	       {
		  reply ();
		  return;
	       }
	     newsgroups = NULL;
	  }
     }
   
   if (Slrn_Post_Obj->po_can_post == 0)
     {
	slrn_error ("Posting not allowed.");
	return;
     }

   if (Slrn_Prefix_Arg_Ptr != NULL)
     {
	prefix_arg = *Slrn_Prefix_Arg_Ptr;
	Slrn_Prefix_Arg_Ptr = NULL;
     }
   else prefix_arg = -1;
   
   if ((newsgroups == NULL)
       /* Hmm..  I have also seen an empty Followup-To: header on a GNU
	* newsgroup.
	*/
       || (*newsgroups == 0))
     {
	if (NULL == (newsgroups = slrn_extract_header ("Newsgroups: ", 12)))
	  newsgroups = "";
     }
   
   if (NULL == (from = slrn_extract_header ("Reply-To: ", 10)))
     {
	from = slrn_extract_header ("From: ", 6);
     }
   from = parse_from (from);
   
   if ((from == NULL) || (prefix_arg == 2))
     perform_cc = 0;
   else if (-1 == (perform_cc = Slrn_Auto_CC_To_Poster))
     perform_cc = slrn_get_yesno (1, "Cc message to poster");
   
   if (perform_cc)
     {
	char *ff;
	
	if ((NULL == (ff = slrn_strchr (from, '@')))
	    || (ff == from)
	    || (0 == slrn_is_fqdn (ff + 1))
	    || (strlen (ff + 1) < 5))
	  {
	     if (0 == slrn_get_yesno (1, "%s appears invalid.  CC anyway", from))
	       perform_cc = 0;
	  }
     }
   
   msgid = slrn_extract_header ("Message-ID: ", 12);
   
   if (NULL != (subject = slrn_extract_header ("Subject: ", 9)))
     {
	subject = slrn_skip_whitespace (subject);
	if (((*subject | 0x20) == 'r')
	    && ((*(subject + 1) | 0x20) == 'e')
	    && (*(subject + 2) == ':'))
	  {
	     subject = slrn_skip_whitespace (subject + 3);
	  }
     }
   else subject = "";
   
   if (Slrn_Use_Tmpdir)
     {
	fp = slrn_open_tmpfile (file, "w");
     }
   else fp = slrn_open_home_file (SLRN_FOLLOWUP_FILENAME, "w", file, 0);
   
   if (fp == NULL)
     {
	slrn_error ("Unable to open %s for writing.", file);
	return;
     }
   
   fprintf (fp, "Newsgroups: %s\nSubject: Re: %s\n",
	    newsgroups, subject);  n = 3;
   
   xref = slrn_extract_header("References: ", 12);
   if (msgid != NULL)
     {
	if (xref == NULL)
	  fprintf (fp, "References: %s\n", msgid);
	else 
	  fprintf (fp, "References: %s %s\n", xref, msgid);
	n++;
     }

   if (Slrn_User_Info.org != NULL)
     {
	fprintf (fp, "Organization: %s\n", Slrn_User_Info.org);
	n++;
     }
   
   if (perform_cc)
     {
	fprintf (fp, "Cc: %s\n", from);
	n++;
     }
   
   if (0 != *Slrn_User_Info.replyto)
     {
	fprintf (fp, "Reply-To: %s\n", Slrn_User_Info.replyto);
	n++;
     }
   
   fprintf (fp, "Followup-To: \n");
   n++;
   
   n += slrn_add_custom_headers (fp, Slrn_Followup_Custom_Headers, insert_followup_format);
   
   fputs ("\n", fp);
   
   if (prefix_arg != 2)
     {
	insert_followup_format (Slrn_User_Info.followup_string, fp);
	fputs ("\n", fp);
     }
   n += 1;			       /* by having + 1, the cursor will be
					* placed on the first line of message.
					*/
   
   
   /* skip header */
   l = Slrn_Article_Lines;
   if (prefix_arg == -1)
     {
	while ((l != NULL) && (*l->buf != 0)) l = l->next;
	if (l != NULL) l = l->next;
     }
   
   if (prefix_arg == 2) quote_str = ""; 
   else if (NULL == (quote_str = Slrn_Quote_String))
     quote_str = ">";

   while (l != NULL)
     {
	if ((l->next != NULL) && (l->next->flags & WRAPPED_LINE))
	  unwrap_line (l->next);
	
	fprintf (fp, "%s%s\n", quote_str, l->buf);
	
	l = l->next;
     }

   if (prefix_arg != 2) slrn_add_signature (fp);
   slrn_fclose (fp);
   
   if (slrn_edit_file (Slrn_Editor_Post, file, n) >= 0)
     {
	if (0 == slrn_post_file (file, from))
	  {
	     /* Success. */
	     if (Slrn_Last_Message_Id != NULL)
	       {
		  /* Later I want to actually get the article from the server
		   * and display it.  Of course I can only do it if I know
		   * the message-id.  If Slrn_Last_Message_Id is non-null, 
		   * then slrn generated the message-id and we can do it.
		   */
	       }
	  }
     }
   
   if (Slrn_Use_Tmpdir) (void) slrn_delete_file (file);
}

/*}}}*/

/*}}}*/

/*{{{ header movement functions */

unsigned int slrn_header_down_n (unsigned int n, int err) /*{{{*/
{
   unsigned int m;
   
   m = SLscroll_next_n (&Slrn_Header_Window, n);
   Slrn_Current_Header = (Slrn_Header_Type *) Slrn_Header_Window.current_line;
   
   if (err && (m != n))
     slrn_error ("End of buffer.");
   
   return m;
}
/*}}}*/

static void header_down (void) /*{{{*/
{
   slrn_header_down_n (1, 1);
}

/*}}}*/

unsigned int slrn_header_up_n (unsigned int n, int err) /*{{{*/
{
   unsigned int m;
   
   m = SLscroll_prev_n (&Slrn_Header_Window, n);
   Slrn_Current_Header = (Slrn_Header_Type *) Slrn_Header_Window.current_line;
   
   if (err && (m != n))
     slrn_error ("Top of buffer.");
   
   return m;
}

/*}}}*/

static void header_up (void) /*{{{*/
{
   slrn_header_up_n (1, 1);
}

/*}}}*/

static void header_pageup (void) /*{{{*/
{
   Slrn_Full_Screen_Update = 1;
   if (-1 == SLscroll_pageup (&Slrn_Header_Window))
     slrn_error ("Top of buffer.");
   Slrn_Current_Header = (Slrn_Header_Type *) Slrn_Header_Window.current_line;
}

/*}}}*/

static void header_pagedn (void) /*{{{*/
{
   Slrn_Full_Screen_Update = 1;
   if (-1 == SLscroll_pagedown (&Slrn_Header_Window))
     slrn_error ("End of buffer.");
   Slrn_Current_Header = (Slrn_Header_Type *) Slrn_Header_Window.current_line;
}

/*}}}*/

static void header_bob (void) /*{{{*/
{
   Slrn_Current_Header = Headers;
   find_header_line_num ();
}

/*}}}*/

static void header_eob (void) /*{{{*/
{
   while (0xFFFF == slrn_header_down_n (0xFFFF, 0));
}

/*}}}*/

static int prev_unread (void) /*{{{*/
{
   Slrn_Header_Type *h;
   
   h = Slrn_Current_Header -> prev;
   
   while (h != NULL)
     {
	if (0 == (h->flags & HEADER_READ)) break;
	h = h->prev;
     }
   
   if (h == NULL)
     {
	slrn_message ("No previous unread articles.");
	return 0;
     }
      
   Slrn_Current_Header = h;

   if (h->flags & HEADER_HIDDEN)
     slrn_uncollapse_this_thread (h, 0);

   find_header_line_num ();
   return 1;
}

/*}}}*/

static void goto_last_read (void) /*{{{*/
{
   if (Last_Read_Header == NULL) return;
   slrn_goto_header (Last_Read_Header, 1);
}

/*}}}*/

static void art_prev_unread (void) /*{{{*/
{
   if (prev_unread () && Article_Visible) art_pagedn  ();
}

/*}}}*/

int slrn_next_unread_header (void) /*{{{*/
{
   Slrn_Header_Type *h;
   
   h = Slrn_Current_Header->next;
   
   while (h != NULL)
     {
	if (0 == (h->flags & HEADER_READ)) break;
	h = h->next;
     }
   
   if (h == NULL)
     {
	slrn_message ("No following unread articles.");
	return 0;
     }
   
   Slrn_Current_Header = h;
   if (h->flags & HEADER_HIDDEN)
     slrn_uncollapse_this_thread (h, 0);
     
   find_header_line_num ();

   return 1;
}

/*}}}*/

static void art_next_unread (void) /*{{{*/
{
   char ch;
   unsigned char ch1;
   
   if (slrn_next_unread_header ())
     {
	if (Article_Visible) art_pagedn  ();
	return;
     }
   
   if (Slrn_Query_Next_Group == 0)
     {
	skip_to_next_group ();
	return;
     }
   
   if (Slrn_Batch) return;
   
   ch1 = SLang_Last_Key_Char;
   if (ch1 == 27) ch1 = 'n';
   slrn_message_now ("No following unread articles.  Press %s for next group.",
		     map_char_to_string (ch1));
   
   ch = SLang_getkey ();
   
   if ((unsigned char)ch == ch1)
     {
	skip_to_next_group ();
     }
   else SLang_ungetkey (ch);
}

/*}}}*/

static void next_high_score (void) /*{{{*/
{
   Slrn_Header_Type *l;
   
   l = Slrn_Current_Header->next;
   
   while (l != NULL)
     {
	if (l->flags & HEADER_HIGH_SCORE)
	  {
	     break;
	  }
	l = l->next;
     }
   
   if (l == NULL)
     {
	slrn_error ("No more high scoring articles.");
	return;
     }
   
   if (l->flags & HEADER_HIDDEN) slrn_uncollapse_this_thread (l, 0);
   
   Slrn_Current_Header = l;
   find_header_line_num ();
   
   if (Article_Visible)
     {
	art_pagedn ();
     }
}

/*}}}*/

static Slrn_Header_Type *Same_Subject_Start_Header;
static void next_header_same_subject (void) /*{{{*/
{
   SLsearch_Type st;
   Slrn_Header_Type *l;
   static char same_subject[256];
   
   if ((Same_Subject_Start_Header == NULL)
       || (Slrn_Prefix_Arg_Ptr != NULL))
     {
	Slrn_Prefix_Arg_Ptr = NULL;
	if (slrn_read_input ("Subject", same_subject, NULL, 0) <= 0) return;
	Same_Subject_Start_Header = Slrn_Current_Header;
     }
   SLsearch_init (same_subject, 1, 0, &st);
   
   l = Slrn_Current_Header->next;
   
   while (l != NULL)
     {
	if (
#if 0
	    /* Do we want to do this?? */
	    ((l->flags & HEADER_READ) == 0) &&
#endif
	    (l->subject != NULL)
	    && (NULL != SLsearch ((unsigned char *) l->subject,
				  (unsigned char *) l->subject + strlen (l->subject),
				  &st)))
	  break;
	
	l = l->next;
     }
   
   if (l == NULL)
     {
	slrn_error ("No more articles on that subject.");
	l = Same_Subject_Start_Header;
	Same_Subject_Start_Header = NULL;
     }
   
   if (l->flags & HEADER_HIDDEN) slrn_uncollapse_this_thread (l, 0);
   Slrn_Current_Header = l;
   find_header_line_num ();
   if ((Same_Subject_Start_Header != NULL)
       && (Article_Visible))
     {
	art_pagedn ();
     }
}

/*}}}*/

static void goto_header_number (void) /*{{{*/
{
   int diff, i, ich;

   if (Slrn_Batch) return;

   i = 0;
   ich = SLang_Last_Key_Char;
   do
     {
	i = i * 10 + (ich - '0');
	if (10 * i > Largest_Header_Number)
	  {
	     ich = '\r';
	     break;
	  }
	slrn_message_now ("Goto Header: %d", i);
     }
   while ((ich = SLang_getkey ()), (ich <= '9') && (ich >= '0'));
   
   if (SLKeyBoard_Quit) return;
   
   if (ich != '\r')
     SLang_ungetkey (ich);
   
   diff = i - Last_Cursor_Row;
   if (diff > 0) slrn_header_down_n (diff, 0); else slrn_header_up_n (-diff, 0);
#if SLRN_HAS_SLANG
   SLang_run_hooks ("header_number_hook", NULL, NULL);
#endif
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

/*}}}*/

/*{{{ article save/decode functions */

static int write_article_lines (FILE *fp)
{
   Slrn_Article_Line_Type *l;
   
   l = Slrn_Article_Lines;

   while (l != NULL)
     {
	char *buf;
	Slrn_Article_Line_Type *next = l->next;
	
	buf = l->buf;
	if (l->flags & WRAPPED_LINE) buf++;   /* skip space */
	
	if (EOF == fputs (buf, fp))
	  return -1;
	
	if ((next == NULL) || (0 == (next->flags & WRAPPED_LINE)))
	  {
	     if (EOF == putc ('\n', fp))
	       return -1;
	  }
	l = next;
     }
	
   return 0;
}

int slrn_save_current_article (char *file) /*{{{*/
{
   FILE *fp = NULL;
   
   if (Slrn_Current_Header == NULL) 
     return -1;
   
   if (read_article (Slrn_Current_Header, 1, 0) < 0) 
     return -1;
   
   fp = fopen (file, "w");
   
   if (fp == NULL)
     {
	slrn_error ("Unable to open %s.", file);
	return -1;
     }

   if (-1 == write_article_lines (fp))
     {
	slrn_error ("Error writing to %s.", file);
	fclose (fp);
	return -1;
     }
   
   fclose (fp);
   return 0;
}


/*}}}*/

static int save_article_as_unix_mail (Slrn_Header_Type *h, FILE *fp) /*{{{*/
{
   char *from;
   time_t now;
   Slrn_Article_Line_Type *l;
   
   if (read_article (h, 1, 0) < 0) return -1;
   
   from = slrn_extract_header ("From: ", 6);
   if (from != NULL) from = parse_from (from);
   if ((from == NULL) || (*from == 0)) from = "nobody@nowhere";
   
   time (&now);
   fprintf (fp, "From %s %s", from, ctime(&now));
   
   l = Slrn_Article_Lines;
   while (l != NULL)
     {
	if ((*l->buf == 'F')
	    && !strncmp ("From", l->buf, 4)
	    && ((unsigned char)(l->buf[4]) <= ' '))
	  {
	     putc ('>', fp);
	  }
	
	fputs (l->buf, fp);
	putc ('\n', fp);
	l = l->next;
     }
   fputs ("\n\n", fp);
   return 0;
}

/*}}}*/

static char *save_article_to_file (char *defdir, char *input_string) /*{{{*/
{
   char file[256];
   char name[256];
   int save_tagged = 0;
   int save_thread = 0;
   int save_simple;
   FILE *fp;
   
   if (-1 == slrn_check_batch ())
     return NULL;
   
   if (Num_Tag_List.len)
     {
	save_tagged = slrn_get_yesno_cancel ("Save tagged articles");
	if (save_tagged < 0) return NULL;
     }
   else if ((Slrn_Current_Header->child != NULL)
	    && (Slrn_Current_Header->child->flags & HEADER_HIDDEN))
     {
	save_thread = slrn_get_yesno_cancel ("Save this thread");
	if (save_thread == -1) return NULL;
     }
   
   save_simple = !(save_tagged || save_thread);
   
   if (*Output_Filename == 0)
     {
#ifdef VMS 
	char *p;
#endif
	unsigned int defdir_len;
	if (defdir == NULL) defdir = "News";
	
	defdir_len = strlen (defdir);
#ifdef VMS
	sprintf (name, "%s/", defdir);
	p = name + strlen (name);
	strcpy (p, Slrn_Current_Group_Name);
	while (*p != 0)
	  {
	     if (*p == '.') *p = '_';
	     p++;
	  }
	strcpy (p, ".txt");
#else
	sprintf (name, "%s/%s", defdir, Slrn_Current_Group_Name);
#endif
	
#ifndef VMS
#ifndef __os2__
	/* Lowercase first letter and see if it exists. */
	name[defdir_len + 1] = LOWER_CASE(name[defdir_len + 1]);
#endif
#endif
	slrn_make_home_filename (name, file);
#ifndef VMS
#ifndef __os2__
	if (1 != slrn_file_exists (file))
	  {
	     /* Lowercase version does not exist so user uppercase form. */
	     name[defdir_len + 1] = UPPER_CASE(name[defdir_len + 1]);
	     slrn_make_home_filename (name, file);
	  }
#endif
#endif
     }
   else strcpy (file, Output_Filename);
   
   if (slrn_read_input (input_string, NULL, file, 1) <= 0)
     {
	slrn_error ("Aborted.");
	return NULL;
     }
   
   if (NULL == (fp = fopen (file, "a")))
     {
	slrn_error ("Unable to open %s", file);
	return NULL;
     }

   strcpy (Output_Filename, file);
   if (save_simple) save_article_as_unix_mail (Slrn_Current_Header, fp);
   else if (save_tagged)
     {
	unsigned int i;
	unsigned int num_saved = 0;

	for (i = 0; i < Num_Tag_List.len; i++)
	  {
	     if (-1 == save_article_as_unix_mail (Num_Tag_List.headers[i], fp))
	       {
		  slrn_smg_refresh ();
		  SLang_Error = 0;
		  (void) SLang_input_pending (5);   /* half second delay */
	       }
	     else num_saved++;
	  }
	if (num_saved == 0) return NULL;
     }
   else
     {
	Slrn_Header_Type *h = Slrn_Current_Header;
	unsigned int num_saved = 0;
	do
	  {
	     if (-1 == save_article_as_unix_mail (h, fp))
	       {
		  slrn_smg_refresh ();
		  SLang_Error = 0;
		  (void) SLang_input_pending (5);   /* half second delay */
	       }
	     else num_saved++;

	     h = h->next;
	  }
	while ((h != NULL) && (h->parent != NULL));
	if (num_saved == 0) return NULL;
     }
   slrn_fclose (fp);
   
   if (SLang_Error) return NULL;
   
   return Output_Filename;
}

/*}}}*/

static void save_article (void) /*{{{*/
{
   (void) save_article_to_file (Slrn_Save_Directory, "Save to file (^G aborts)");
}

/*}}}*/

#if SLRN_HAS_DECODE
#if SLRN_HAS_UUDEVIEW
static int the_uudeview_busy_callback (void *param, uuprogress *progress)
{
   char stuff[26];
   unsigned int count, count_max;
   int pcts;
   char *ptr;
   
   if (progress->action != UUACT_DECODING) 
     return 0;
   
   pcts = (int)((100 * progress->partno + progress->percent - 100) / progress->numparts);
   
   count_max = sizeof (stuff) - 1;

   for (count = 0; count < count_max; count++)
     stuff[count] = (count < pcts/4) ? '#' : '.';

   stuff [count_max] = 0;

   slrn_message_now ("decoding %10s (%3d/%3d) %s",
		     progress->curfile,
		     progress->partno, progress->numparts,
		     stuff);
   return 0;
}

static int do_slrn_uudeview (char *uu_dir, char *file)
{
   uulist *item;
   char where [SLRN_MAX_PATH_LEN];
   int i, ret;

   slrn_make_home_dirname (uu_dir, where);
   /* this is expecting a '/' at the end...so we put one there */
   strcat (where, "/");
   
   ret = UUInitialize ();
   ret = UUSetBusyCallback (NULL, the_uudeview_busy_callback, 100);
   ret = UUSetOption (UUOPT_DESPERATE, 1, NULL);
   ret = UUSetOption (UUOPT_SAVEPATH, 0, where);
   if (UURET_OK != (ret = UULoadFile (file, NULL, 0)))
     {
	/* Not all systems have strerror... */
	if (ret == UURET_IOERR)
	  slrn_error ("could not load %s: errno = %d", 
		      file, UUGetOption (UUOPT_ERRNO, NULL, NULL, 0));
	else
	  slrn_error ("could not load %s: %s",  UUstrerror (ret));
     }
   
   i = 0;
   while (NULL != (item = UUGetFileListItem (i)))
     {
	i++;
	
	if (UURET_OK != (ret = UUDecodeFile (item, NULL)))
	  {
	     char *f;
	     char *err;
	     
	     if (NULL == (f = item->filename)) f = "oops";
	     
	     if (ret == UURET_IOERR) err = "I/O error.";
	     else err = UUstrerror (ret);
	     
	     slrn_error ("error decoding %s: %s", f, err);
	  }
     }

   UUCleanUp ();
}
#endif

static void decode_article (void) /*{{{*/
{
   char *uu_dir;
   char *file;
   
   if (NULL == (uu_dir = Slrn_Decode_Directory))
     {
	uu_dir = Slrn_Save_Directory;
     }
   else *Output_Filename = 0;	       /* force it to use this directory */

   file = save_article_to_file(uu_dir, "Filename (^G aborts)");

   if (file == NULL) return;
   
   if (1 == slrn_get_yesno (1, "decode %s", file))
     {
# if SLRN_HAS_UUDEVIEW
	(void) do_slrn_uudeview (uu_dir, file);
# else
	(void) slrn_uudecode_file (file, NULL, 0, NULL);
# endif

	if (SLang_Error == 0)
	  {
	     if (1 == slrn_get_yesno (1, "Delete %s", file))
	       {
		  if (-1 == slrn_delete_file (file))
		    slrn_error ("Unable to delete %s", file);
	       }
	  }
     }
   /* Since we have a decode directory, do not bother saving this */
   if (NULL != Slrn_Decode_Directory)
     *Output_Filename = 0;
}

/*}}}*/
#endif  /* SLRN_HAS_DECODE */

/*}}}*/
/*{{{ pipe_article functions */

int slrn_pipe_article_to_cmd (char *cmd) /*{{{*/
{
#if SLRN_HAS_PIPING
   FILE *fp;

   if (-1 == select_article (1))
     return -1;

   if (NULL == (fp = slrn_popen (cmd, "w")))
     {
	slrn_error ("Unable to open pipe to %s", cmd);
	return -1;
     }
   
   if (-1 == write_article_lines (fp))
     {
	slrn_pclose (fp);
	return -1;
     }
   
   slrn_pclose (fp);
   return 0;
#else
   slrn_error ("Piping not implemented on this system.");
   return -1;
#endif
}

/*}}}*/

static void pipe_article (void) /*{{{*/
{
#if SLRN_HAS_PIPING
   static char cmd[256];
   
   *cmd = 0;
   if (slrn_read_input ("Pipe to command", cmd, NULL, 1) <= 0)
     {
	slrn_error ("Aborted.  Command name is required.");
	return;
     }
   
   if (-1 == slrn_pipe_article_to_cmd (cmd))
     slrn_message ("Error piping to %s.", cmd);
#else
   slrn_error ("Piping not implemented on this system.");
#endif
}

/*}}}*/

/*}}}*/

/*{{{ sorting header functions */

static int subject_cmp (register unsigned char *sa, register unsigned char *sb) /*{{{*/
{
   register unsigned char ch;
   
   /* skip past re: */
   while (*sa == ' ') sa++;
   while (*sb == ' ') sb++;

   ch = *sa;
   if (((ch | 0x20) == 'r') && ((*(sa + 1) | 0x20) == 'e')
       && (*(sa + 2) == ':'))
     {
	sa += 3;
     }
   
   ch = *sb;
   if (((ch | 0x20) == 'r') && ((*(sb + 1) | 0x20) == 'e')
       && (*(sb + 2) == ':'))
     {
	sb += 3;
     }
   
   while (1)
     {
	register unsigned char cha, chb;

	while (*sa == ' ') sa++;
	while (*sb == ' ') sb++;

	cha = UPPER_CASE(*sa);
	chb = UPPER_CASE(*sb);
	
	if (cha != chb)
	  return (int) cha - (int) chb;
	
	if (cha == 0)
	  return 0;
	
	sa++;
	sb++;	
     }

   /* return slrn_case_strcmp ((unsigned char *) sa, (unsigned char *) sb); */
}

/*}}}*/

static int header_subj_cmp (Slrn_Header_Type **ap, Slrn_Header_Type **bp) /*{{{*/
{
   int cmp;
   Slrn_Header_Type *a = *ap, *b = *bp;
   int ahigh, bhigh;
   
   ahigh = (0 != (a->flags & (HEADER_HIGH_SCORE | FAKE_HEADER_HIGH_SCORE)));
   bhigh = (0 != (b->flags & (HEADER_HIGH_SCORE | FAKE_HEADER_HIGH_SCORE)));
   
   if (ahigh == bhigh)
     {
	cmp = subject_cmp ((unsigned char *) (a->subject),
			   (unsigned char *) (b->subject));
	if (!cmp) return a->number - b->number;
     }
   else cmp = bhigh - ahigh;
   
   return cmp;
}

/*}}}*/

static int header_date_cmp (Slrn_Header_Type **ap, Slrn_Header_Type **bp) /*{{{*/
{
   Slrn_Header_Type *a = *ap, *b = *bp;
   long ahigh, bhigh;
   
   ahigh = (0 != (a->flags & (HEADER_HIGH_SCORE | FAKE_HEADER_HIGH_SCORE)));
   bhigh = (0 != (b->flags & (HEADER_HIGH_SCORE | FAKE_HEADER_HIGH_SCORE)));
   
   if (ahigh == bhigh)
     {
	ahigh = slrn_date_to_order_parm (a->date);
	bhigh = slrn_date_to_order_parm (b->date);
	
	if (Slrn_Sorting_Mode & SORT_BY_SUBJECT)
	  {
	     long tmp = ahigh;
	     ahigh = bhigh;
	     bhigh = tmp;
	  }
	
	if (ahigh == bhigh)
	  return a->number - b->number;
     }

   return (int) (bhigh - ahigh);
}

/*}}}*/

typedef int (*Header_Cmp_Func_Type)(Slrn_Header_Type **, Slrn_Header_Type **);

static void sort_by_function (Header_Cmp_Func_Type cmp_func) /*{{{*/
{
   Slrn_Header_Type **header_list, *h;
   unsigned int i, nheaders;
   void (*qsort_fun) (char *, unsigned int,
		      unsigned int, int (*)(Slrn_Header_Type **, Slrn_Header_Type **));
   
   /* This is a silly hack to make up for braindead compilers and the lack of
    * uniformity in prototypes for qsort.
    */
   qsort_fun = (void (*)(char *, unsigned int,
			 unsigned int, int (*)(Slrn_Header_Type **, Slrn_Header_Type **)))
     qsort;
   
   /* Count the number we need to sort. */
   nheaders = 0;
   h = Slrn_First_Header;
   while (h != NULL)
     {
	if (h->parent == NULL) nheaders++;
	h = h->real_next;
     }
   if (nheaders < 2) return;
   
   if (NULL == (header_list = (Slrn_Header_Type **) SLCALLOC (sizeof (Slrn_Header_Type *), nheaders + 1)))
     {
	slrn_error ("sort_headers(): memory allocation failure.");
	return;
     }
   
   h = Slrn_First_Header;
   nheaders = 0;
   while (h != NULL)
     {
	if (h->parent == NULL)
	  header_list[nheaders++] = h;
	h = h->real_next;
     }
   header_list[nheaders] = NULL;
   
   (*qsort_fun) ((char *) header_list, nheaders, sizeof (Slrn_Header_Type *), cmp_func);
   
   /* What to do now depends upon the current threading state. */
   
   if (Headers_Threaded == 0)
     {
	header_list[0]->next = header_list[1];
	header_list[0]->prev = NULL;
	
	for (i = 1; i < nheaders; i++)
	  {
	     h = header_list[i];
	     h->next = header_list[i + 1];
	     h->prev = header_list[i - 1];
	  }
     }
   else
     {
	slrn_collapse_threads (0);
	/* The headers are threaded so we simply have sorted parents.  Arrange
	 * those.
	 */
	h = NULL;
	for (i = 0; i <= nheaders; i++)
	  {
	     Slrn_Header_Type *h1 = header_list[i];
	     if (h != NULL)
	       {
		  h->sister = h1;
		  while (h->child != NULL)
		    {
		       h = h->child;
		       while (h->sister != NULL) h = h->sister;
		    }
		  h->next = h1;
	       }
	     if (h1 != NULL) h1->prev = h;
	     h = h1;
	  }
     }
   
   Headers = header_list[0];
   find_header_line_num ();
   
   Slrn_Full_Screen_Update = 1;
   SLFREE (header_list);
   
   if (Slrn_Threads_Visible)
     {
	slrn_uncollapse_threads (1);
     }
}

/*}}}*/

static void sort_by_subject (void) /*{{{*/
{
   sort_by_function (header_subj_cmp);
}

/*}}}*/

static void sort_by_date (void) /*{{{*/
{
   sort_by_function (header_date_cmp);
}

/*}}}*/

#if SLRN_HAS_SORT_BY_SCORE
static int header_score_cmp (Slrn_Header_Type **a, Slrn_Header_Type **b) /*{{{*/
{
   /* sort by *descending* score */
   int cmp = (*b)->thread_score - (*a)->thread_score;
   if (cmp == 0) 
     {
	if (Slrn_Sorting_Mode & SORT_BY_SUBJECT)
	  return header_subj_cmp (a, b);
	else 
	  return (*a)->number - (*b)->number;
     }
   return cmp;
}

/*}}}*/

static void sort_by_score (void) /*{{{*/
{
   sort_by_function (header_score_cmp);
}

/*}}}*/
#endif


static void sort_by_server_number (void) /*{{{*/
{
   Slrn_Header_Type *h;
   
   /* This is easy since the real_next, prev are already ordered. */
   h = Slrn_First_Header;
   while (h != NULL)
     {
	Slrn_Header_Type *next = h->real_next;
	h->next = h->real_next;
	h->prev = h->real_prev;
	h->num_children = 0;
	h->flags &= ~(HEADER_HIDDEN | ALL_THREAD_FLAGS);
	h->sister = h->parent = h->child = NULL;
	h = next;
     }
   
   /* Now find out where to put the Headers pointer */
   while (Headers->prev != NULL) Headers = Headers->prev;
   
   Headers_Threaded = 0;
   
   find_header_line_num ();
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void sort_by_threads (void) /*{{{*/
{
   thread_headers ();
   sort_threads ();
   
   if (Slrn_Threads_Visible)
     {
	slrn_uncollapse_threads (1);
     }
   Slrn_Full_Screen_Update = 1;
   Headers_Threaded = 1;
}

/*}}}*/

static void toggle_sort (void) /*{{{*/
{
   int rsp;
   
   rsp = slrn_sbox_sorting_method ();
   if (rsp != -1)
     {
	Slrn_Sorting_Mode = rsp;
	sort_by_sorting_mode ();
     }
}

/*}}}*/

static void sort_by_sorting_mode (void) /*{{{*/
{
   if ((Slrn_Sorting_Mode & SORT_BY_THREADS) == 0)
     sort_by_server_number ();
   else
     sort_by_threads ();
   
   if (Slrn_Sorting_Mode & SORT_BY_DATE)
     sort_by_date ();
   else if (Slrn_Sorting_Mode & SORT_BY_SUBJECT)
     sort_by_subject ();

#if SLRN_HAS_SORT_BY_SCORE
   if (Slrn_Sorting_Mode & SORT_BY_SCORE)
     sort_by_score ();
#endif
}

/*}}}*/

/*}}}*/
/*{{{ Thread related functions */

static void find_non_hidden_header (void) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_Current_Header;
   
   while ((h != NULL) && (h->flags & HEADER_HIDDEN))
     h = h->prev;

   if (h == NULL)
     {
	h = Slrn_Current_Header;
	while ((h != NULL) && (h->flags & HEADER_HIDDEN))
	  h = h->next;
     }
   
   Slrn_Current_Header = h;
}

/*}}}*/

/* This function cannot depend upon routines which call SLscroll functions if
 * sync_now is non-zero.
 */
void slrn_collapse_threads (int sync_now) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_First_Header;
   
   if ((h == NULL) 
       || (Threads_Collapsed == 1))
     return;   
   
   while (h != NULL)
     {
	if (h->parent != NULL) h->flags |= HEADER_HIDDEN;
	else
	  {
	     h->flags &= ~HEADER_HIDDEN;
	  }
	h = h->real_next;
     }
   
   find_non_hidden_header ();
	
   if (sync_now) find_header_line_num ();

   Slrn_Full_Screen_Update = 1;
   Threads_Collapsed = 1;
}

/*}}}*/

void slrn_uncollapse_threads (int sync_now) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_First_Header;
   
   if ((h == NULL) 
       || (0 == Threads_Collapsed))
     return;
   
   while (h != NULL)
     {
	h->flags &= ~HEADER_HIDDEN;
	h = h->real_next;
     }
   Slrn_Full_Screen_Update = 1;
   Threads_Collapsed = 0;
   if (sync_now) find_header_line_num ();
}

/*}}}*/

static void uncollapse_header (Slrn_Header_Type *h) /*{{{*/
{
   h->flags &= ~HEADER_HIDDEN;
}

/*}}}*/

static void collapse_header (Slrn_Header_Type *h) /*{{{*/
{
   h->flags |= HEADER_HIDDEN;
}

/*}}}*/

static void for_this_tree (Slrn_Header_Type *h, void (*f)(Slrn_Header_Type *)) /*{{{*/
{
   Slrn_Header_Type *child = h->child;
   while (child != NULL)
     {
	for_this_tree (child, f);
	child = child->sister;
     }
   (*f) (h);
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void for_this_family (Slrn_Header_Type *h, void (*f)(Slrn_Header_Type *)) /*{{{*/
{
   while (h != NULL)
     {
	for_this_tree (h, f);
	h = h->sister;
     }
}

/*}}}*/

void slrn_uncollapse_this_thread (Slrn_Header_Type *h, int sync_linenum) /*{{{*/
{
   Slrn_Header_Type *child;
   
   /* if (Threads_Collapsed == 0) return; */
   
   while (h->parent != NULL) h = h->parent;
   if ((child = h->child) == NULL) return;
   if (0 == (child->flags & HEADER_HIDDEN)) return;
   
   for_this_family (child, uncollapse_header);

   if (sync_linenum)
     find_header_line_num ();

   Threads_Collapsed = -1;	       /* uncertain */
}

/*}}}*/

void slrn_collapse_this_thread (Slrn_Header_Type *h, int sync_linenum) /*{{{*/
{
   Slrn_Header_Type *child;
   
   /* if (Threads_Collapsed == 1) return; */
   
   while (h->parent != NULL) h = h->parent;
   
   if ((child = h->child) == NULL) return;
   if (child->flags & HEADER_HIDDEN) return;
   
   for_this_family (child, collapse_header);
   
   if (sync_linenum)
     find_header_line_num ();

   Threads_Collapsed = -1;	       /* uncertain */
}

/*}}}*/

static void toggle_collapse_threads (void) /*{{{*/
{
   if (Slrn_Prefix_Arg_Ptr != NULL)
     {
	if (Threads_Collapsed == 1)
	  {
	     slrn_uncollapse_threads (0);
	  }
	else slrn_collapse_threads (0);
	Slrn_Prefix_Arg_Ptr = NULL;
     }
   else
     {
	if (0 == slrn_is_thread_collapsed (Slrn_Current_Header))
	  slrn_collapse_this_thread (Slrn_Current_Header, 0);
	else
	  slrn_uncollapse_this_thread (Slrn_Current_Header, 0);
	
	find_non_hidden_header ();
     }
   find_header_line_num ();
}

/*}}}*/

static Slrn_Header_Type *sort_thread_node (Slrn_Header_Type *h, char *tree) /*{{{*/
{
   Slrn_Header_Type *last = NULL;
   static unsigned int level;
   unsigned char vline_char;
   
   if (h == NULL) return NULL;

#ifdef __os2__
   vline_char = SLSMG_VLINE_CHAR;
#else
   vline_char = (SLtt_Has_Alt_Charset ? SLSMG_VLINE_CHAR : ' ');
#endif

   while (1)
     {
	last = h;
	
	if (h->child != NULL)
	  {
	     Slrn_Header_Type *child = h->child;
	     unsigned int tree_level;
	     unsigned int save_level = level;
	     
	     h->next = child;
	     child->prev = h;
	     
	     
	     if (level == 0)
	       {
		  if (h->flags & FAKE_CHILDREN)
		    {
		       if ((child->flags & FAKE_PARENT) == 0)
			 {
			    level = 1;
			 }
		    }
	       }
	     else if (h->flags & FAKE_PARENT)
	       {
		  if (h->sister != NULL) tree[0] = vline_char;
		  else tree[0] = ' ';
		  tree[1] = ' ';
		  level = 1;
	       }
	     
	     tree_level = 2 * level - 2;
	     
	     if (level && (tree_level < sizeof (h->tree) - 2))
	       {
		  if (h->sister != NULL)
		    {
		       if (((h->sister->flags & FAKE_PARENT) == 0)
			   || (h->flags & FAKE_PARENT))
			 {
			    tree[tree_level] = vline_char;
			 }
		       else tree[tree_level] = ' ';
		    }
		  else
		    {
		       if ((h->parent == NULL) && (h->flags & FAKE_CHILDREN))
			 {
			    tree[tree_level] = vline_char;
			 }
		       else tree[tree_level] = ' ';
		    }
		  tree[tree_level + 1] = ' ';
		  tree[tree_level + 2] = 0;
	       }
	     
	     level++;
	     last = sort_thread_node (h->child, tree);
	     level--;
	     
	     if (level &&
		 ((tree_level < sizeof (h->tree) - 2)))
	       tree[tree_level] = 0;
	     
	     level = save_level;
	  }
	
	if (h->flags & FAKE_PARENT) *tree = 0;
	
	if (*tree)
	  {
	     strncpy ((char *) h->tree, tree, sizeof (h->tree) - 1);
	     h->tree[sizeof (h->tree) - 1] = 0;
	  }
	h = h->sister;
	last->next = h;
	if (h == NULL) break;
	h->prev = last;
     }
   return last;
}

/*}}}*/

static unsigned int compute_num_children (Slrn_Header_Type *h) /*{{{*/
{
   unsigned int n = 0, dn;
   
   h = h->child;
   while (h != NULL)
     {
	n++;
	if (h->child == NULL) dn = 0;
	else
	  {
	     dn = compute_num_children (h);
	     n += dn;
	  }
	h->num_children = dn;
	h = h->sister;
     }
   return n;
}

/*}}}*/

unsigned int slrn_thread_size (Slrn_Header_Type *h)
{
   if (h == NULL) return 0;
   return 1 + compute_num_children (h);
}

int slrn_is_thread_collapsed (Slrn_Header_Type *h)
{
   if (h == NULL) return 1;
   while (h->parent != NULL) h = h->parent;
   if (h->child == NULL) return 0;
   return (h->child->flags & HEADER_HIDDEN);
}


static void sort_threads (void) /*{{{*/
{
   Slrn_Header_Type *h;
   char tree[MAX_TREE_SIZE];
   
   h = Slrn_First_Header;
   Headers = NULL;
   
   if (h == NULL) return;
   while (h != NULL)
     {
	if ((h->parent == NULL) && (Headers == NULL))
	  Headers = h;
	
	h->prev = h->next = NULL;
	h->flags &= ~HEADER_HIDDEN;
	h = h->real_next;
     }
   Threads_Collapsed = 0;
   
   if (Headers == NULL)
     slrn_exit_error ("Internal Error.");
   
   *tree = 0;
   sort_thread_node (Headers, tree);
   while (Headers->prev != NULL) Headers = Headers->prev;
   
   slrn_collapse_threads (0);
   
   h = Headers;
   while (h != NULL)
     {
	if (h->child == NULL) h->num_children = 0;
	else
	  {
	     Slrn_Header_Type *next;
	     h->num_children = compute_num_children (h);
	     next = h->next;
	     while ((next != NULL) && (next->parent != NULL))
	       {
#if SLRN_HAS_SORT_BY_SCORE
		  if (next->flags & HEADER_HIGH_SCORE)
		    h->flags |= FAKE_HEADER_HIGH_SCORE;
		  if (next->score > h->thread_score)
		    h->thread_score = next->score;
#else
		  if (next->flags & HEADER_HIGH_SCORE)
		    {
		       h->flags |= FAKE_HEADER_HIGH_SCORE;
		       break;
		    }
#endif
		  next = next->next;
	       }
	  }
	h = h->sister;
     }
   find_header_line_num ();
}

/*}}}*/

static void link_same_subjects (void) /*{{{*/
{
   Slrn_Header_Type **header_list, *h;
   unsigned int i, nparents;
   void (*qsort_fun) (char *, unsigned int, unsigned int, int (*)(Slrn_Header_Type **, Slrn_Header_Type **));
   
   /* This is a silly hack to make up for braindead compilers and the lack of
    * uniformity in prototypes for qsort.
    */
   qsort_fun = (void (*)(char *,
			 unsigned int, unsigned int,
			 int (*)(Slrn_Header_Type **, Slrn_Header_Type **)))
     qsort;
   
   h = Slrn_First_Header;
   nparents = 0;
   while (h != NULL)
     {
	if (h->parent == NULL)
	  nparents++;
	h = h->real_next;
     }
   if (nparents < 2) return;
   
   
   if (NULL == (header_list = (Slrn_Header_Type **) SLCALLOC (sizeof (Slrn_Header_Type *), nparents)))
     {
	slrn_error ("link_same_subjects: memory allocation failure.");
	return;
     }
   
   h = Slrn_First_Header;
   i = 0;
   while (i < nparents)
     {
	if (h->parent == NULL) header_list[i++] = h;
	h = h->real_next;
     }
   
   (*qsort_fun) ((char *) header_list,
		 nparents, sizeof (Slrn_Header_Type *), header_subj_cmp);
   
   h = header_list[0];
   for (i = 1; i < nparents; i++)
     {
	Slrn_Header_Type *h1 = header_list[i];
	if (0 == subject_cmp ((unsigned char *) h->subject,
			      (unsigned char *) h1->subject))
	  {
	     if (h->child == NULL)
	       {
		  h->child = h1;
	       }
	     else
	       {
		  Slrn_Header_Type *child = h->child;
		  while (child->sister != NULL) child = child->sister;
		  child->sister = h1;
	       }
	     
	     h1->parent = h;
	     h->flags |= FAKE_CHILDREN;
	     h1->flags |= FAKE_PARENT;
	     if (h1->flags & FAKE_CHILDREN)
	       {
		  /* Well, we have to link them up to the new parent.  That
		   * is, h1 will become their sister.  So, extract the
		   * adopted children of h1 and make them the sister,
		   */
		  Slrn_Header_Type *child = h1->child, *last_child;
		  last_child = child;
		  
		  /* child CANNOT be NULL here!! (the parent claims to have
						  children) */
		  child = child->sister;
		  while (child != NULL)
		    {
		       if (child->flags & FAKE_PARENT)
			 break;
		       last_child = child;
		       child = child->sister;
		    }
		  
		  if (last_child->flags & FAKE_PARENT)
		    {
		       child = last_child;
		       h1->child = NULL;
		    }
		  else last_child->sister = NULL;
		  
		  last_child = child;
		  while (child != NULL)
		    {
		       child->parent = h;
		       /* No need to set fake parent flags since fake children
			* are all group together.  That is, once you loop
			* through the sisters and find one, you have found them
			* all.
			*/
		       child = child->sister;
		    }
		  
		  /* Now h1 will become the sister. */
		  child = h1;
		  while (child->sister != NULL) child = child->sister;
		  child->sister = last_child;
		  h1->flags &= ~FAKE_CHILDREN;
	       }
	  }
	else h = h1;
     }
   SLFREE (header_list);
}

/*}}}*/

typedef struct /*{{{*/
{
   unsigned long ref_hash;
   Slrn_Header_Type *h;
}

/*}}}*/
Relative_Type;

static void link_lost_relatives (void) /*{{{*/
{
   unsigned int n, i, j;
   Slrn_Header_Type *h;
   Relative_Type *relatives;
   
   /* count the number of possible relatives */
   n = 0;
   h = Slrn_First_Header;
   while (h != NULL)
     {
	if ((h->parent == NULL)
	    && (h->refs != NULL)
	    && (*h->refs != 0)) n++;
	
	h = h->real_next;
     }
   if (n < 2) return;
   
   relatives = (Relative_Type *) slrn_malloc (sizeof (Relative_Type) * n, 0, 0);
   if (relatives == NULL)
     return;
   
   n = 0;
   h = Slrn_First_Header;
   while (h != NULL)
     {
	if ((h->parent == NULL)
	    && (h->refs != NULL)
	    && (*h->refs != 0))
	  {
	     unsigned char *r, *ref_begin;
	     
	     r = (unsigned char *) h->refs;
	     while (*r && (*r != '<')) r++;
	     if (*r == '<')
	       {
		  ref_begin = r;
		  while (*r && (*r != '>')) r++;
		  if (*r == '>') r++;
		  relatives[n].ref_hash = slrn_compute_hash (ref_begin, r);
		  relatives[n].h = h;
		  n++;
	       }
	  }
	h = h->real_next;
     }
   
   for (i = 0; i < n; i++)
     {
	unsigned long ref_hash;
	Relative_Type *ri = relatives + i;
	Slrn_Header_Type *rih;
	
	ref_hash = ri->ref_hash;
	rih = ri->h;
	
	for (j = i + 1; j < n; j++)
	  {
	     if (relatives[j].ref_hash == ref_hash)
	       {
		  Slrn_Header_Type *rjh = relatives[j].h;
		  
		  if (Slrn_New_Subject_Breaks_Threads
		      && (rih->subject != NULL)
		      && (rjh->subject != NULL)
		      && (0 != subject_cmp ((unsigned char *)rih->subject, (unsigned char *)rjh->subject)))
		    continue;
		  
		  if (rih->parent != NULL)
		    {
		       rih->sister = rjh;
		       rjh->parent = rih->parent;
		    }
		  else if (rih->child == NULL)
		    {
		       rih->child = rjh;
		       rjh->parent = rih;
		       rih->flags |= FAKE_CHILDREN;
		    }
		  else
		    {
		       Slrn_Header_Type *child = rih->child;
		       /* This is an important step.  All adopted children
			* get linked to the LAST child.  This ordering
			* assumption is used elsewhere.
			*/
		       while (child->sister != NULL) child = child->sister;
		       child->sister = rjh;
		       rjh->parent = rih;
		       rih->flags |= FAKE_CHILDREN;
		    }
		  rjh->flags |= FAKE_PARENT;
		  break;
	       }
	  }
     }
   SLFREE (relatives);
}

/*}}}*/

static void thread_headers (void) /*{{{*/
{
   Slrn_Header_Type *h, *ref;
   char *r0, *r1, *rmin;
   
   make_hash_table ();
   
   h = Slrn_First_Header;
   while (h != NULL)
     {
	h->next = h->prev = h->child = h->parent = h->sister = NULL;
	h->flags &= ~ALL_THREAD_FLAGS;
	*h->tree = 0;
	h = h->real_next;
     }
   
   /* SLMEMSET ((char *) Lost_Ancestors, 0, sizeof (Lost_Ancestors)); */
   
   h = Slrn_First_Header;
   while (h != NULL)
     {
	if (*h->refs == 0)
	  {
	     h = h->real_next;
	     continue;
	  }
	
	rmin = h->refs;
	r1 = rmin + strlen (rmin);
	
	while (1)
	  {
	     while ((r1 > rmin) && (*r1 != '>')) r1--;
	     r0 = r1 - 1;
	     while ((r0 >= rmin) && (*r0 != '<')) r0--;
	     if ((r0 < rmin) || (r1 == rmin)) break;
	     
	     ref = find_header_from_msgid (r0, r1 + 1);
	     
	     if (ref != NULL)
	       {
		  Slrn_Header_Type *child, *rparent;
		  
		  if (Slrn_New_Subject_Breaks_Threads
		      && (h->subject != NULL)
		      && (ref->subject != NULL)
		      && (0 != subject_cmp ((unsigned char *)h->subject, (unsigned char *)ref->subject)))
		    break;
		  
		  rparent = ref;
		  while (rparent->parent != NULL) rparent = rparent->parent;
		  if (rparent == h)
		    {
		       /* self referencing!!! */
		       slrn_error ("Article %d is part of reference loop!", h->number);
		    }
		  else
		    {
		       h->parent = ref;
		       child = ref->child;
		       if (child == NULL) ref->child = h;
		       else
			 {
			    while (child->sister != NULL) child = child->sister;
			    child->sister = h;
			 }
		       break;
		    }
	       }
	     r1 = r0;
	  }
	h = h->real_next;
     }
   
   /* No perform a re-arrangement such that those with the no parents but
    * share the same reference are placed side-by-side as sisters.
    */
   
   link_lost_relatives ();
   
   /* Now perform sort on subject to catch those that have fallen through the
    * cracks, i.e., no references */
   link_same_subjects ();
   
   /* Now link others up as sisters */
   h = Slrn_First_Header;
   while ((h != NULL) && (h->parent != NULL))
     {
	h = h->real_next;
     }
   
   while (h != NULL)
     {
	Slrn_Header_Type *next;
	next = h->real_next;
	while ((next != NULL) && (next->parent != NULL))
	  next = next->real_next;
	h->sister = next;
	h = next;
     }
}

/*}}}*/

/*}}}*/

/*{{{ select_article */

/* returns 0 if article selected, -1 if something went wrong or 1 if article
 * already selected.
 */
static int select_article (int do_mime) /*{{{*/
{
   int ret = 1;
   
   Slrn_Full_Screen_Update = 1;
   
   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   
   if (Slrn_Current_Header != Header_Showing)
     {
	if (read_article (Slrn_Current_Header, 1, 1) < 0) return -1;
#if SLRN_HAS_MIME
	if (do_mime && Slrn_Use_Mime && Slrn_Mime_Needs_Metamail)
	  {
	     if (slrn_mime_call_metamail ())
	       return -1;
	  }
#endif
	ret = 0;
     }
   
   set_article_visibility (1);
   return ret;
}

/*}}}*/

/*}}}*/

/*{{{ mark_spot and exchange_mark */

static void mark_spot (void) /*{{{*/
{
   Mark_Header = Slrn_Current_Header;
   slrn_message ("Mark set.");
}

/*}}}*/


static void exchange_mark (void) /*{{{*/
{
   if (Mark_Header == NULL)
     {
	slrn_error ("Mark not set.");
	return;
     }
   
   if (-1 == slrn_goto_header (Mark_Header, 0)) return;
   mark_spot ();
}

/*}}}*/

/*}}}*/
/*{{{ subject/author header searching commands */

static void header_generic_search (int dir, int type) /*{{{*/
{
   static char search_str[256];
   SLsearch_Type st;
   Slrn_Header_Type *l;
   char prompt[80];
   
   sprintf (prompt, "%s Search %s",
	    type == 's' ? "Subject" : "Author",
	    dir > 0 ? "Forward" : "Backward");
   
   if (slrn_read_input (prompt, search_str, NULL, 0) <= 0)
     return;
   
   SLsearch_init (search_str, 1, 0, &st);
   
   if (dir > 0) l = Slrn_Current_Header->next;
   else l = Slrn_Current_Header->prev;
   
   while (l != NULL)
     {
	if (type == 's')
	  {
	     if ((l->subject != NULL)
		 && (NULL != SLsearch ((unsigned char *) l->subject,
				       (unsigned char *) l->subject + strlen (l->subject),
				       &st)))
	       break;
	  }
	else if ((l->subject != NULL)
		 && (NULL != SLsearch ((unsigned char *) l->from,
				       (unsigned char *) l->from + strlen (l->from),
				       &st)))
	  break;
	
	if (dir > 0) l = l->next; else l = l->prev;
     }
   
   if (l == NULL)
     {
	slrn_error ("Not found.");
	return;
     }
   
   if (l->flags & HEADER_HIDDEN) slrn_uncollapse_this_thread (l, 0);
   Slrn_Current_Header = l;
   find_header_line_num ();
}

/*}}}*/

static void subject_search_forward (void) /*{{{*/
{
   header_generic_search (1, 's');
}

/*}}}*/

static void subject_search_backward (void) /*{{{*/
{
   header_generic_search (-1, 's');
}

/*}}}*/

static void author_search_forward (void) /*{{{*/
{
   header_generic_search (1, 'a');
}

/*}}}*/

static void author_search_backward (void) /*{{{*/
{
   header_generic_search (-1, 'a');
}

/*}}}*/

/*}}}*/

/*{{{ score header support */

/*{{{ kill list functions */


typedef struct Kill_List_Type /*{{{*/
{
#define MAX_DKILLS 50
   int nums[MAX_DKILLS];
   unsigned int num_used;
   struct Kill_List_Type *next;
}

/*}}}*/

Kill_List_Type;

static Kill_List_Type *Kill_List;
static Kill_List_Type *Missing_Article_List;

static Kill_List_Type *add_to_specified_kill_list (int num, Kill_List_Type *root) /*{{{*/
{
   if (num < 0) return root;
   
   if ((root == NULL) || (root->num_used == MAX_DKILLS))
     {
	Kill_List_Type *k;
	k = (Kill_List_Type *) SLMALLOC (sizeof (Kill_List_Type));
	if (k == NULL) return root;
	k->num_used = 0;
	k->next = root;
	root = k;
     }
   root->nums[root->num_used++] = num;
   return root;
}

/*}}}*/

static void add_to_kill_list (int num) /*{{{*/
{
   Kill_List = add_to_specified_kill_list (num, Kill_List);
   Number_Killed++;
}

/*}}}*/

static void add_to_missing_article_list (int num) /*{{{*/
{
   Missing_Article_List = add_to_specified_kill_list (num, Missing_Article_List);
}

/*}}}*/

static void free_specific_kill_list_and_update (Kill_List_Type *k) /*{{{*/
{
   while (k != NULL)
     {
	Kill_List_Type *next = k->next;
	unsigned int i, imax = k->num_used;
	int *nums = k->nums;
	for (i = 0; i < imax; i++)
	  {
	     slrn_mark_article_as_read (NULL, nums[i]);
	  }
	SLFREE (k);
	k = next;
     }
}

/*}}}*/

static void free_kill_lists_and_update (void) /*{{{*/
{
   free_specific_kill_list_and_update (Kill_List);
   Kill_List = NULL;
   Number_Killed = 0;
   free_specific_kill_list_and_update (Missing_Article_List);
   Missing_Article_List = NULL;
}

/*}}}*/


/*}}}*/

Slrn_Header_Type *slrn_set_header_score (Slrn_Header_Type *h, 
					 int score, int apply_kill)
{
   if (h == NULL) return NULL;
   
   if (score >= Slrn_High_Score_Min)
     {
	h->flags &= ~(HEADER_LOW_SCORE);
	h->flags |= HEADER_HIGH_SCORE;
	Number_High_Scored++;
     }
   else if (score < Slrn_Low_Score_Max)
     {
	if ((score <= Slrn_Kill_Score_Max) && apply_kill)
	  {
	     int number = h->number;
	     SLFREE (h->subject);
	     SLFREE (h);
	     add_to_kill_list (number);
	     return NULL;
	  }
	
	h->flags &= ~(HEADER_HIGH_SCORE);
	h->flags |= (HEADER_READ | HEADER_LOW_SCORE);
	Number_Low_Scored++;
	/* The next line should be made configurable */
	kill_cross_references (h);
     }
#if SLRN_HAS_SORT_BY_SCORE
   h->thread_score = h->score = score;
#endif
   return h;
}

/*{{{ apply_score */
static Slrn_Header_Type *apply_score (Slrn_Header_Type *h) /*{{{*/
{
   int score;
   
   if (h == NULL) return h;
   
   if (Slrn_Apply_Score && Perform_Scoring)
     score = slrn_score_header (h, Slrn_Current_Group_Name);
   else score = 0;
   
   return slrn_set_header_score (h, score, 1);
}

/*}}}*/

/*}}}*/

/*{{{ score_headers */
static void score_headers (void) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_First_Header;
   int percent, last_percent, delta_percent;
   int num;
   
   if ((h == NULL) || (Slrn_Apply_Score == 0)) return;
   
   /* slrn_set_suspension (1); */
   
   percent = num = 0;
   delta_percent = (30 * 100) / Total_Num_Headers + 1;
   last_percent = -delta_percent;
   
   while (h != NULL)
     {
	Slrn_Header_Type *prev, *next;
	prev = h->real_prev;
	next = h->real_next;
	percent = (100 * num) / Total_Num_Headers;
	if (percent >= last_percent + delta_percent)
	  {
	     slrn_message_now ("Scoring articles: %2d%%, Killed: %u, High: %u, Low: %u",
			       percent, Number_Killed, Number_High_Scored, Number_Low_Scored);
	     last_percent = percent;
	  }
	
	num++;
	h = apply_score (h);
	
	if (h == NULL)
	  {
	     num--;
	     if (prev == NULL)
	       Slrn_First_Header = next;
	     else
	       prev->next = prev->real_next = next;
	     
	     if (next != NULL)
	       next->prev = next->real_prev = prev;
	  }
	h = next;
     }
   Slrn_Current_Header = Headers = Slrn_First_Header;
   /* slrn_set_suspension (0); */
}

/*}}}*/

/*}}}*/

static void create_score (void) /*{{{*/
{
   if (Slrn_Batch) return;

   (void) slrn_edit_score (Slrn_Current_Header, Slrn_Current_Group_Name);
}

/*}}}*/


/*}}}*/

/*{{{ get headers from server and process_xover */

static Slrn_Header_Type *process_xover (Slrn_XOver_Type *xov)
{
   Slrn_Header_Type *h;
   
   h = (Slrn_Header_Type *) slrn_safe_malloc (sizeof (Slrn_Header_Type));
   
   slrn_map_xover_to_header (xov, h);
   
   if ((Slrn_Score_After_XOver == 0) && Perform_Scoring)
     {
	if (NULL == (h = apply_score (h)))
	  return h;
     }
   
#if SLRN_HAS_MIME
   if (Slrn_Use_Mime)
     {
	slrn_rfc1522_decode_string (h->subject);
	slrn_rfc1522_decode_string (h->from);
     }
#endif
#if SLRN_HAS_GROUPLENS
   if (Slrn_Use_Group_Lens)
     {
	h->gl_rating = h->gl_pred = -1;
     }
#endif
   return h;
}


/*}}}*/

/*{{{ get_headers from server */
static int get_headers (int min, int max, int *totalp) /*{{{*/
{
   Slrn_Header_Type *h;
   /* int percent, last_percent, dpercent, */
   int expected_num;
   int scoring;
   int total = *totalp;
   int reads_per_update;
   int num_processed;
   Slrn_XOver_Type xov;
   
   if (total == 0)
     return 0;
   
   if (SLang_Error == USER_BREAK)
     return -1;
   
   scoring = Perform_Scoring && !Slrn_Score_After_XOver;

   if ((reads_per_update = Slrn_Reads_Per_Update) < 5)
     reads_per_update = 50;

   if (scoring) reads_per_update = reads_per_update / 3 + 1;
      
   /* slrn_set_suspension (1); */
   
   if (OK_XOVER != slrn_open_xover (min, max))
     return -1;
   
   num_processed = 0;
   expected_num = min;
   while (slrn_read_xover(&xov) > 0)
     {
	int this_num;
	int num = Total_Num_Headers + Number_Killed + num_processed;
	
	if (SLang_Error == USER_BREAK)
	  return -1;

	this_num = xov.id;
	
	if (expected_num != this_num)
	  {
	     int bad_num;
	     
	     total -= (this_num - expected_num);
	     
	     for (bad_num = expected_num; bad_num < this_num; bad_num++)
	       add_to_missing_article_list (bad_num);
	  }
	
	expected_num = this_num + 1;
	
	h = process_xover (&xov);
	
	num++;

	if ((1 == (num % reads_per_update))
	    && (SLang_Error == 0))
	  {
	     if (scoring)
	       {
		  slrn_message_now ("Headers Received and Scored: %3d/%-3d, Killed: %u, High: %u, Low: %u",
				    num, total, 
				    Number_Killed, Number_High_Scored, Number_Low_Scored);
	       }
	     else
	       slrn_message_now ("Headers Received: %2d/%d", num, total);
	  }
	
	if (h == NULL) continue;
	
	if (Slrn_First_Header == NULL)
	  Slrn_First_Header = Headers = h;
	else
	  {
	     h->real_next = Slrn_Current_Header->real_next;
	     h->real_prev = Slrn_Current_Header;
	     Slrn_Current_Header->real_next = h;
	     
	     if (h->real_next != NULL)
	       {
		  h->real_next->real_prev = h;
	       }
	  }
	
	Slrn_Current_Header = h;
	num_processed++;
     }
   
   slrn_close_xover ();
   
   if (expected_num != max + 1)
     {
	int bad_num;
	     
	total -= (max - expected_num) + 1;
	     
	for (bad_num = expected_num; bad_num <= max; bad_num++)
	  add_to_missing_article_list (bad_num);
     }
   
   /* slrn_set_suspension (0); */
   *totalp = total;
   
   Total_Num_Headers += num_processed;

   return (int) num_processed;
}

/*}}}*/


/*}}}*/


/*}}}*/
/*{{{ get parent/children headers, etc... */

/* Nothing is synced by this routine.  It is up to the calling routine. */
static void insert_header (Slrn_Header_Type *ref) /*{{{*/
{
   int n, id;
   Slrn_Header_Type *h;
   Slrn_Range_Type *r;
   
   ref->hash_next = Header_Table[ref->hash % HEADER_TABLE_SIZE];
   Header_Table[ref->hash % HEADER_TABLE_SIZE] = ref;
   
   n = ref->number;
   h = Slrn_First_Header;
   while (h != NULL)
     {
	if (h->number >= n)
	  {
	     ref->real_next = h;
	     ref->real_prev = h->real_prev;
	     if (h->real_prev != NULL) h->real_prev->real_next = ref;
	     h->real_prev = ref;
	     
	     if (h == Slrn_First_Header) Slrn_First_Header = ref;
	     if (h == Headers) Headers = ref;
	     
	     break;
	  }
	h = h->real_next;
     }
   
   if (h == NULL)
     {
	h = Slrn_First_Header;
	while (h->real_next != NULL) h = h->real_next;
	
	ref->real_next = NULL;
	ref->real_prev = h;
	h->real_next = ref;
     }
   
   if ((id = ref->number) <= 0) return;
   
   /* Set the flags for this guy. */
   r = Current_Group->range.next;
   while (r != NULL)
     {
	if (r->min > id) break;
	if (r->max >= id)
	  {
	     ref->flags = HEADER_READ;
	     return;
	  }
	r = r->next;
     }
   ref->flags &= ~HEADER_READ;
}

/*}}}*/

/* line number is not synced. */
static int get_header_by_message_id (char *msgid, int no_error_no_thread) /*{{{*/
{
   Slrn_Header_Type *ref;
   Slrn_XOver_Type xov;
   
   if ((msgid == NULL) || (*msgid == 0)) return -1;
   
   ref = find_header_from_msgid (msgid, msgid + strlen (msgid));
   if (ref != NULL)
     {
	Slrn_Current_Header = ref;
	if (no_error_no_thread == 0)
	  find_header_line_num ();
	Slrn_Full_Screen_Update = 1;
	return 0;
     }
   
   slrn_message_now ("Finding parent from server...");
   
   /* Try reading it from the server */
   if (-1 == slrn_xover_for_msgid (msgid, &xov))
     {
	if (no_error_no_thread == 0)
	  {
	     slrn_error ("Article %s not available.", msgid);
	     return -1;
	  }
	return 1;
     }
   
   ref = process_xover (&xov);

   if (ref == NULL) return -1;
   
   get_header_real_name (ref);
   
   insert_header (ref);
   
   Slrn_Current_Header = ref;
   if (no_error_no_thread == 0)
     {
	sort_by_sorting_mode ();
     }
   return 0;
}

/*}}}*/

/* returns -1 if not implemented or the number of children returned from
 * the server.  It does not sync line number. 
 */  
static int find_children_headers (Slrn_Header_Type *parent) /*{{{*/
{
   char buf[NNTP_BUFFER_SIZE];
   int id_array[1000];
   int num_ids, i, id;
   char *fmt = "Finding children from server...[%c]";
   char *meter_chars = "|/-\\";
   static unsigned int last_meter_char;

   if (OK_HEAD != Slrn_Server_Obj->sv_xpat_cmd ("References",
						Slrn_Server_Min, Slrn_Server_Max,
						parent->msgid))
     {
	slrn_error ("Your server does not provide support for this feature.");
	return -1;
     }
   
   if (meter_chars[last_meter_char] == 0)
     last_meter_char = 0;
   
   slrn_message_now (fmt, meter_chars[last_meter_char]);
   last_meter_char++;
   
   num_ids = 0;
   while (Slrn_Server_Obj->sv_read_line (buf, sizeof (buf) - 1) != NULL)
     {
	if (meter_chars[last_meter_char] == 0)
	  last_meter_char = 0;
   
	slrn_message_now (fmt, meter_chars[last_meter_char]);
	last_meter_char++;
	
	id = atoi (buf);
	if (id <= 0) continue;
	
	if (NULL != find_header_from_serverid (id)) continue;
	id_array[num_ids] = id;
	num_ids++;
     }
   
   for (i = 0; i < num_ids; i++)
     {
	Slrn_XOver_Type xov;
	
	id = id_array[i];

	if (OK_XOVER != slrn_open_xover (id, id))
	  break;
	
	/* This will loop once. */
	while (slrn_read_xover (&xov) > 0)
	  {
	     Slrn_Header_Type *h = process_xover (&xov);
	     if (h == NULL) continue;
	     
	     get_header_real_name (h);
	     insert_header (h);
	  }
	slrn_close_xover ();
     }
   return num_ids;
}

/*}}}*/

/* Line number not synced. */
static void get_children_headers_1 (Slrn_Header_Type *h) /*{{{*/
{
   while (h != NULL)
     {
	(void) find_children_headers (h);
	if (h->child != NULL)
	  {
	     get_children_headers_1 (h->child);
	  }
	h = h->sister;
     }
}

/*}}}*/

static void get_children_headers (void) /*{{{*/
{
   Slrn_Header_Type *h;
   
   /* slrn_set_suspension (1); */
   
   if (find_children_headers (Slrn_Current_Header) < 0)
     {
	/* slrn_set_suspension (0); */
	return;
     }
   
   sort_by_sorting_mode ();
   
   h = Slrn_Current_Header->child;
   if (h != NULL) 
     {
	/* Now walk the tree getting children headers.  For efficiency,
	 * only children currently threaded will be searched.  Hopefully the
	 * above attempt got everything.  If other newsreaders did not chop off
	 * headers, this would be unnecessary!
	 */
	get_children_headers_1 (h);
	sort_by_sorting_mode ();
     }
   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   
   /* slrn_set_suspension (0); */
}

/*}}}*/

static void get_parent_header (void) /*{{{*/
{
   char *r1, *r0, *rmin;
   unsigned int len;
   char buf[512];
   int no_error_no_thread;
   Slrn_Header_Type *last_header;
   
   if (Slrn_Current_Header == NULL) return;
   
   if (Slrn_Prefix_Arg_Ptr == NULL) no_error_no_thread = 0;
   else no_error_no_thread = 1;
   
   last_header = NULL;
   r1 = rmin = NULL;
   do
     {
	rmin = Slrn_Current_Header->refs;
	if (rmin == NULL) break;
	
	if (last_header != Slrn_Current_Header)
	  {
	     last_header = Slrn_Current_Header;
	     r1 = rmin + strlen (rmin);
	  }
	
	while ((r1 > rmin) && (*r1 != '>')) r1--;
	r0 = r1 - 1;
	while ((r0 >= rmin) && (*r0 != '<')) r0--;
	
	if ((r0 < rmin) || (r1 == rmin))
	  {
	     if (no_error_no_thread) break;
	     slrn_error ("Article has no parent reference.");
	     return;
	  }
	
	len = (unsigned int) ((r1 + 1) - r0);
	strncpy (buf, r0, len);
	buf[len] = 0;
	r1 = r0;
     }
   while ((get_header_by_message_id (buf, no_error_no_thread) >= 0)
	  && no_error_no_thread);
   
   if (no_error_no_thread)
     {
	sort_by_sorting_mode ();
	if (SLKeyBoard_Quit == 0) get_children_headers ();
     }
   
   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
   slrn_chmap_fix_headers ();  /* We didn't decode parent headers */
}

/*}}}*/

static void locate_header_by_msgid (void) /*{{{*/
{
   char msgid[256];
   *msgid = 0;
   if (slrn_read_input ("Enter Message-Id", NULL, msgid, 1) <= 0) return;
   get_header_by_message_id (msgid, 0);
   slrn_uncollapse_this_thread (Slrn_Current_Header, 1);
}

/*}}}*/


/*}}}*/

/*{{{ article window display modes */

static void hide_article (void) /*{{{*/
{
   Slrn_Full_Screen_Update = 1;
   if (Article_Visible == 0)
     {
	select_article (1);
	return;
     }
   
   set_article_visibility (0);
   HScroll = 0;
}

/*}}}*/


static void art_left (void) /*{{{*/
{
   if (HScroll == 0) return;
   HScroll -= (SLtt_Screen_Cols * 2) / 3;
   if (HScroll < 0) HScroll = 0;
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void art_right (void) /*{{{*/
{
   HScroll += (SLtt_Screen_Cols * 2) / 3;
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

/*{{{ rot13 and spoilers */
static void toggle_rot13 (void) /*{{{*/
{
   Do_Rot13 = !Do_Rot13;
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

 
#if SLRN_HAS_SPOILERS
static void show_spoilers (void) /*{{{*/
{
   Slrn_Article_Line_Type *l1 = NULL;
   Slrn_Article_Line_Type *l = Slrn_Article_Lines;
   
   do
     {
	/* first find the first spoiler-ed line */
	while ((l != NULL) && 
	       (0 == (l->flags & SPOILER_LINE)))
	  {
	     l = l->next;
	  }
	if (l1 == NULL) l1 = l;
	/* now un-spoiler until we hit an un-spoiler-ed line */
	while ((l != NULL) && (l->flags & SPOILER_LINE))
	  {
	     l->flags &= ~SPOILER_LINE;
	     l = l->next;
	  }
     } /* Prefix arg means un-spoiler the whole article */
   while ((l != NULL)
	  && ((Slrn_Prefix_Arg_Ptr != NULL) 
	      || (Slrn_Spoiler_Display_Mode & 2)));

   Slrn_Prefix_Arg_Ptr = NULL;
   
   if ((Slrn_Spoiler_Display_Mode & 1) && (l1 != NULL))
     {
	Article_Current_Line = l1;
	find_article_line_num ();
     }
   
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

#endif


/*}}}*/

/*{{{ hide/toggle quotes */

/* Does NOT update article line number */
static void hide_quotes (void) /*{{{*/
{
   Slrn_Article_Line_Type *l = Slrn_Article_Lines, *last = NULL;
   while (l != NULL)
     {
	if (l->flags & QUOTE_LINE)
	  {
	     if (Quotes_Hidden && (last != NULL)) l->flags |= HIDDEN_LINE;
	     else l->flags &= ~HIDDEN_LINE;
	     last = l;
	  }
	else last = NULL;
	l = l->next;
     }
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

/* This function needs generalized on an article by article basis */
static void toggle_quotes (void) /*{{{*/
{
   Quotes_Hidden = !Quotes_Hidden;
   hide_quotes ();
   find_article_line_num ();
}

/*}}}*/

/*}}}*/


static void toggle_headers (void) /*{{{*/
{
   Headers_Hidden_Mode = !Headers_Hidden_Mode;
   hide_art_headers ();
   find_article_line_num ();
   if (Headers_Hidden_Mode == 0) art_bob ();
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

/*}}}*/
/*{{{ header window display modes */

static void toggle_show_author (void) /*{{{*/
{
   Slrn_Show_Author++;
   Slrn_Show_Author = (Slrn_Show_Author % 3);
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

/*}}}*/

/*{{{ leave/suspend article mode and support functions */

static void update_ranges (void) /*{{{*/
{
   int min, max;
   int queued = 0;
   Slrn_Range_Type *r, *r_save;
   Slrn_Header_Type *h = Slrn_First_Header;
   int save_min, save_max, save_dirty;
   
   if (User_Aborted_Group_Read) return;
   
   /* skip articles for which the numeric id was not available */
   while ((h != NULL) && (h->number < 0)) h = h->real_next;
   if (h == NULL) return;
   
   /* we are creating new ranges so steal old. */
   r = Current_Group->range.next;
   Current_Group->range.next = NULL;
   
   /* Save the range context because we will do a comparison to see whether
    * or not the group was modified.
    */
   r_save = r;
   save_min = Current_Group->range.min;
   save_max = Current_Group->range.max;
   save_dirty = Slrn_Groups_Dirty;
   
   /* fill in range for articles prior to current group of headers.  This
    * is done in two parts.  First, mark as read everything up to the
    * first article on the server plus what we have read after that.  Then,
    * fill in the gap for articles on the server up to this group.
    */
   
   max = Slrn_Server_Min - 1;
   min = h->number;	       /* starting number of headers */
   
   /* Part one.  Skip past ranges that are below the server minimum number.
    * They will be fused together.
    */
   while ((r != NULL) && (r->min <= max))
     {
	if (r->max >= max)
	  {
	     max = r->max;
	     if (max >= min) max = min - 1;
	     r = r->next;
	     break;
	  }
	r = r->next;
     }
   slrn_add_group_ranges (Current_Group, 1, max);
   
   /* second part */
   while ((r != NULL) && (r->min < min))
     {
	max = r->max;
	if (max >= min) max = min - 1;
	slrn_add_group_ranges (Current_Group, r->min, max);
	r = r->next;
     }
   
   /* Now handle ranges in the current group.
    */
   
   queued = 0;
   max = Slrn_Server_Max;
   while (h != NULL)
     {
	max = h->number;
	if (h->flags & HEADER_READ)
	  {
	     queued = 1;
	  }
	else
	  {
	     if (queued || (min != max))
	       {
		  slrn_add_group_ranges (Current_Group, min, max - 1);
		  queued = 0;
	       }
	     
	     min = max + 1;	       /* mark next number as start of
					* a read range.  We mark it now
					* because h->next->number might not
					* be max + 1.
					*/
	  }
	h = h->real_next;
     }
   
   
   if (queued == 0) min = max + 1;
   slrn_add_group_ranges (Current_Group, min, Slrn_Server_Max);
   
   Slrn_Groups_Dirty = 1;
   
   if ((save_dirty == 0) 
       && (Current_Group->range.min == save_min) 
       && (Current_Group->range.max == save_max)
       && (Current_Group->range.next != r_save))
     {
	/* Compare the newly constructed ranges to the old.  If they differ
	 * then things were changed.
	 */
	Slrn_Range_Type *new_r;
	
	new_r = Current_Group->range.next;
	r = r_save;
	
	while ((new_r != NULL) && (r != NULL)
	       && (new_r->min == r->min)
	       && (new_r->max == r->max))
	  {
	     new_r = new_r->next;
	     r = r->next;
	  }
	
	if ((new_r == NULL) && (r == NULL))
	  Slrn_Groups_Dirty = save_dirty;
     }
   
   r = r_save;
   while (r != NULL)
     {
	r_save = r->next;
	SLFREE (r);
	r = r_save;
     }
}

/*}}}*/


/*{{{ art_quit */
static void art_quit (void) /*{{{*/
{
   Slrn_Header_Type *h = Headers, *next;

#if SLRN_HAS_GROUPLENS
   if (Slrn_Use_Group_Lens) slrn_put_grouplens_scores ();
#endif
   
   free_article ();
   
   free_kill_lists_and_update ();
   free_tag_list ();
   
   slrn_close_score ();
   
   if (h != NULL)
     {
	update_ranges ();
     }
   
   while (h != NULL)
     {
	next = h->next;
	SLFREE (h->subject);
	SLFREE (h);
	h = next;
     }
   
   Slrn_First_Header = Headers = Slrn_Current_Header = NULL;
   SLMEMSET ((char *) &Slrn_Header_Window, 0, sizeof (SLscroll_Window_Type));
   Total_Num_Headers = 0;
   
   Current_Group = NULL;
   Last_Read_Header = NULL;
   *Output_Filename = 0;

   Same_Subject_Start_Header = NULL;
   Slrn_Current_Group_Name = NULL;
   
   slrn_switch_to_mode (Last_Current_Mode);
}

/*}}}*/

/*}}}*/

static void skip_to_next_group (void) /*{{{*/
{
   art_quit ();
   slrn_select_next_group ();
}

/*}}}*/

static void skip_to_prev_group (void) /*{{{*/
{
   art_quit ();
   slrn_select_prev_group ();
}

/*}}}*/

static void fast_quit (void) /*{{{*/
{
   art_quit ();
   slrn_group_quit ();
}

/*}}}*/

static void art_suspend_cmd (void) /*{{{*/
{
   int rows = SLtt_Screen_Rows;
   slrn_suspend_cmd ();
   if (rows != SLtt_Screen_Rows)
     art_winch_sig (rows, -1);
}

/*}}}*/

/*}}}*/
/*{{{ art_xpunge */
static void art_xpunge (void) /*{{{*/
{
   Slrn_Header_Type *save, *next, *h;
   
   free_article ();
   free_kill_lists_and_update ();
   
   save = Headers;
   if (Headers != NULL)
     {
	update_ranges ();
     }
   
   while (Headers != NULL)
     {
	if (0 == (Headers->flags & HEADER_READ))
	  break;
	Headers = Headers->next;
     }
   
   if (Headers == NULL)
     {
	Headers = save;
	art_quit ();
	return;
     }
   
   if ((Num_Tag_List.len != 0)
       && (Num_Tag_List.headers != NULL))
     {
	unsigned int i, j;
	Slrn_Header_Type *th;
	
	j = 0;
	for (i = 0; i < Num_Tag_List.len; i++)
	  {
	     th = Num_Tag_List.headers[i];
	     if (th->flags & HEADER_READ)
	       {
		  th->tag_number = 0;
		  th->flags &= ~HEADER_NTAGGED;
		  continue;
	       }
	     
	     Num_Tag_List.headers [j] = th;
	     j++;
	     th->tag_number = j;
	  }
	Num_Tag_List.len = j;
     }
   
   next = Slrn_Current_Header;
   while (next != NULL)
     {
	if (0 == (next->flags & HEADER_READ))
	  break;
	next = next->next;
     }
   
   if (next == NULL)
     {
	next = Slrn_Current_Header;
	while (next != NULL)
	  {
	     if (0 == (next->flags & HEADER_READ))
	       break;
	     next = next->prev;
	  }
     }
   
   Slrn_Current_Header = next;	       /* cannot be NULL */
   
   h = Slrn_First_Header;
   /* h cannot be NULL here*/
   while (1)
     {
	next = h->real_next;
	if (0 == (h->flags & HEADER_READ))
	  break;
	SLFREE (h->subject);
	SLFREE (h);
	h = next;
     }
   Slrn_First_Header = h;
   h->real_prev = NULL;
   
   while (h != NULL)
     {	
	Slrn_Header_Type *next_next;

	next = h->real_next;
	while (next != NULL)
	  {
	     next_next = next->real_next;
	     if (0 == (next->flags & HEADER_READ))
	       break;
	     SLFREE (next->subject);
	     SLFREE (next);
	     next = next_next;
	  }
	h->real_next = next;
	if (next != NULL)
	  next->real_prev = h;
	h = next;
     }
   
   Last_Read_Header = NULL;
   h = Headers = Slrn_First_Header;
   Headers->prev = NULL;
   
   while (h != NULL)
     {
	h->next = next = h->real_next;
	if (next != NULL)
	  {
	     next->prev = h;
	  }
	h = next;
     }
   
   sort_by_sorting_mode ();
   
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/


/*}}}*/
/*{{{ cancel_article */

static void cancel_article (void) /*{{{*/
{
   char *from, *msgid, *newsgroups, *dist;
   char me[256];
   
   if (-1 == slrn_check_batch ())
     return;
   
   if (-1 == select_article (0)) return;

   slrn_update_screen (1);
   
   if (slrn_get_yesno (0, "Are you sure that you want to cancel this article") <= 0)
     return;
   
   slrn_message_now ("Cancelling...");
   
   /* TO cancel, we post a cancel message with a 'control' header.  First, check to
    * see if this is really the owner of the message.
    */
   
   from = slrn_extract_header ("From: ", 6);
   if (from != NULL) from = parse_from (from);
   if (from == NULL) from = "";
   sprintf (me, "%s@%s", Slrn_User_Info.username, Slrn_User_Info.host);
   
   if (slrn_case_strcmp ((unsigned char *) from, (unsigned char *) me))
     {
	slrn_error ("Failed: Your name: '%s' is not '%s'", me, from);
	return;
     }
   
   if (NULL == (newsgroups = slrn_extract_header ("Newsgroups: ", 12)))
     newsgroups = "";
   
   if (NULL == (msgid = slrn_extract_header ("Message-ID: ", 12)))
     {
	slrn_error ("No message id.");
	return;
     }
   
   
   dist = slrn_extract_header("Distribution: ", 14);
   
   if (Slrn_Post_Obj->po_start () < 0) return;
   
   Slrn_Post_Obj->po_printf ("From: %s\nNewsgroups: %s\nSubject: cancel %s\nControl: cancel %s\n",
		from, newsgroups, msgid, msgid);
   
   if (dist != NULL)
     {
	Slrn_Post_Obj->po_printf ("Distribution: %s\n", dist);
     }
   
   Slrn_Post_Obj->po_printf("\nignore\nArticle canceled by slrn %s\n", Slrn_Version);
   
   if (0 == Slrn_Post_Obj->po_end ())
     {
	slrn_message ("Done.");
     }
}

/*}}}*/


/*}}}*/

/*{{{ header/thread (un)deletion/(un)catchup */
static void delete_header (Slrn_Header_Type *h) /*{{{*/
{
   if (h->flags & HEADER_TAGGED) return;
   if (0 == (h->flags & HEADER_READ))
     {
	kill_cross_references (h);
	h->flags |= HEADER_READ;
     }
}

/*}}}*/

static void undelete_header (Slrn_Header_Type *h) /*{{{*/
{
   if (h->flags & HEADER_TAGGED) return;
   h->flags &= ~HEADER_READ;
}

/*}}}*/

static void catch_up_all (void) /*{{{*/
{
   for_all_headers (delete_header, 1);
}

/*}}}*/

static void un_catch_up_all (void) /*{{{*/
{
   for_all_headers (undelete_header, 1);
}

/*}}}*/

static void catch_up_to_here (void) /*{{{*/
{
   for_all_headers (delete_header, 0);
}

/*}}}*/

static void un_catch_up_to_here (void) /*{{{*/
{
   for_all_headers (undelete_header, 0);
}

/*}}}*/


static void undelete_header_cmd (void) /*{{{*/
{
   if ((Slrn_Current_Header->parent != NULL)/* in middle of thread */
       || (Slrn_Current_Header->child == NULL)/* At top with no child */
       /* or at top with child showing */
       || (0 == (Slrn_Current_Header->child->flags & HEADER_HIDDEN)))
     {
	undelete_header (Slrn_Current_Header);
     }
   else
     {
	for_this_tree (Slrn_Current_Header, undelete_header);
     }
   slrn_header_down_n (1, 0);
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void delete_header_cmd (void) /*{{{*/
{
   if ((Slrn_Current_Header->parent != NULL)/* in middle of thread */
       || (Slrn_Current_Header->child == NULL)/* At top with no child */
       /* or at top with child showing */
       || (0 == (Slrn_Current_Header->child->flags & HEADER_HIDDEN)))
     {
	delete_header (Slrn_Current_Header);
     }
   else
     {
	for_this_tree (Slrn_Current_Header, delete_header);
     }
   slrn_next_unread_header ();
   Slrn_Full_Screen_Update = 1;
}

/*}}}*/

static void thread_delete_cmd (void) /*{{{*/
{
   for_this_tree (Slrn_Current_Header, delete_header);
   delete_header_cmd ();
}

/*}}}*/


/*}}}*/

/*{{{ group_lens functions */
#if SLRN_HAS_GROUPLENS
static void grouplens_rate_article (void) /*{{{*/
{
   int ch;
   
   if ((Slrn_Current_Header == NULL)
       || (Num_GroupLens_Rated == -1))
     return;
   
   slrn_message_now ("Rate article (1-5):");
   
   ch = SLang_getkey ();
   if ((ch < '1') || (ch > '5'))
     {
	slrn_error ("Rating must be in range 1 to 5.");
	return;
     }
   
   slrn_group_lens_rate_article (Slrn_Current_Header, ch - '0',
				 (Article_Visible && (Header_Showing == Slrn_Current_Header)));
}

/*}}}*/

#endif
/*}}}*/
/*{{{ mouse commands */

/* actions for different regions:
 *	- top status line (help)
 *	- header status line
 *	- above header status line
 *	- below header status line
 *	- bottom status line
 */
static void art_mouse (void (*top_status)(void), /*{{{*/
		       void (*header_status)(void),
		       void (*bot_status)(void),
		       void (*normal_region)(void)
		       )
{
   int r, c;
   slrn_get_mouse_rc (&r, &c);
   
   /* take top status line into account */
   if (r == 1)
     {
	if (Slrn_Use_Mouse)
	  (void) slrn_execute_menu (c);
	else if (NULL != top_status) (*top_status) ();
 	return;
     }
   
   if (r >= SLtt_Screen_Rows)
     return;
   
   /* On header status line */
   if (r - 2 == Header_Window_Nrows)
     {
	if (NULL != header_status) (*header_status) ();
 	return;
     }
   
   /* bottom status line */
   if (r == SLtt_Screen_Rows - 1)
     {
	if (NULL != bot_status) (*bot_status) ();
	return;
     }
   
   if (r - 2 > Header_Window_Nrows)
     {
	if (NULL != normal_region) (*normal_region) ();
 	return;
     }
   
   r -= (1 + Last_Cursor_Row);
   if (r < 0)
     {
	r = -r;
	if (r != (int) slrn_header_up_n (r, 0)) return;
     }
   else if (r != (int) slrn_header_down_n (r, 0)) return;
   
   select_article (1);
   /* if (NULL != normal_region) (*normal_region) (); */
   
}

/*}}}*/


static void art_mouse_left (void) /*{{{*/
{
   art_mouse (slrn_article_help, header_pagedn,
	      art_next_unread, art_pagedn);
}

/*}}}*/

static void art_mouse_middle (void) /*{{{*/
{
   art_mouse (toggle_show_author, hide_article,
	      toggle_quotes, hide_article);
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

/*}}}*/


static void art_mouse_right (void) /*{{{*/
{
   art_mouse (slrn_article_help, header_pageup,
	      art_prev_unread, art_pageup);
}

/*}}}*/

/*}}}*/

/*{{{ slrn_init_article_mode */

#define A_KEY(s, f)  {s, (int (*)(void)) f}

static SLKeymap_Function_Type Art_Functions [] = /*{{{*/
{
#if SLRN_HAS_GROUPLENS
   A_KEY("grouplens_rate_article", grouplens_rate_article),
#endif
   A_KEY("digit_arg", slrn_digit_arg),
   A_KEY("browse_url", browse_url),
   A_KEY("browse_url", browse_url),
   A_KEY("art_xpunge", art_xpunge),
   A_KEY("wrap_article", wrap_article),
   A_KEY("goto_last_read", goto_last_read),
#if SLRN_HAS_DECODE
   A_KEY("decode", decode_article),
#endif
#if 0
#if SLRN_HAS_SPOILERS
     A_KEY("show_spoilers", show_spoilers),
#endif
#endif
     A_KEY("create_score", create_score),
     A_KEY("toggle_collapse_threads", toggle_collapse_threads),
     A_KEY("toggle_header_tag", toggle_header_tag),
     A_KEY("tag_header", num_tag_header),
     A_KEY("untag_headers", num_untag_headers),
     A_KEY("repeat_last_key", slrn_repeat_last_key),
     A_KEY("art_bob", art_bob),
     A_KEY("art_eob", art_eob),
     A_KEY("goto_beginning", art_bob),
     A_KEY("goto_end", art_eob),
     A_KEY("forward_digest", skip_digest_forward),
     A_KEY("locate_article", locate_header_by_msgid),
     A_KEY("delete_thread", thread_delete_cmd),
     A_KEY("post", slrn_post_cmd),
     A_KEY("get_children_headers", get_children_headers),
     A_KEY("get_parent_header", get_parent_header),
     A_KEY("skip_to_next_group", skip_to_next_group),
     A_KEY("skip_to_prev_group", skip_to_prev_group),
     A_KEY("fast_quit", fast_quit),
     A_KEY("catchup_all", catch_up_all),
     A_KEY("uncatchup_all", un_catch_up_all),
     A_KEY("catchup", catch_up_to_here),
     A_KEY("uncatchup", un_catch_up_to_here),
     A_KEY("pipe_article", pipe_article),
     A_KEY("toggle_rot13", toggle_rot13),
     A_KEY("toggle_show_author", toggle_show_author),
     A_KEY("toggle_sort", toggle_sort),
     A_KEY("skip_quotes", skip_quoted_text),
     A_KEY("header_bob", header_bob),
     A_KEY("header_eob", header_eob),
     A_KEY("goto_article", goto_article),
     A_KEY("shrink_window", shrink_window),
     A_KEY("enlarge_window", enlarge_window),
     A_KEY("scroll_dn", art_pagedn),
     A_KEY("scroll_up", art_pageup),
     A_KEY("article_pagedn", art_pagedn),
     A_KEY("article_pageup", art_pageup),
     A_KEY("article_linedn", art_linedn),
     A_KEY("article_lineup", art_lineup),
     A_KEY("article_search", article_search),
     A_KEY("author_search_backward", author_search_backward),
     A_KEY("author_search_forward", author_search_forward),
     A_KEY("cancel", cancel_article),
     A_KEY("delete", delete_header_cmd),
     A_KEY("down", header_down),
     A_KEY("exchange_mark", exchange_mark),
     A_KEY("followup", followup),
     A_KEY("forward", forward_article),
     A_KEY("help", slrn_article_help),
     A_KEY("hide_article", hide_article),
     A_KEY("left", art_left),
     A_KEY("mark_spot", mark_spot),
     A_KEY("next", art_next_unread),
     A_KEY("prev", art_prev_unread),
     A_KEY("quit", art_quit),
     A_KEY("redraw", slrn_redraw),
     A_KEY("reply", reply_cmd),
     A_KEY("right", art_right),
     A_KEY("save", save_article),
     A_KEY("subject_search_backward", subject_search_backward),
     A_KEY("subject_search_forward", subject_search_forward),
     A_KEY("suspend", art_suspend_cmd),
     A_KEY("toggle_headers", toggle_headers),
     A_KEY("toggle_quotes", toggle_quotes),
     A_KEY("undelete", undelete_header_cmd),
     A_KEY("up", header_up),
     A_KEY("pageup", header_pageup),
     A_KEY("pagedn", header_pagedn),
     A_KEY("next_same_subject", next_header_same_subject),
     A_KEY("next_high_score", next_high_score),
     A_KEY("next_high_score", next_high_score),
     A_KEY("locate_header_by_msgid", locate_header_by_msgid),
     A_KEY(NULL, NULL)
};

/*}}}*/


static Slrn_Mode_Type Art_Mode_Cap = /*{{{*/
{
   NULL,			       /* keymap */
   art_update_screen,
   art_winch_sig,		       /* sigwinch_fun */
   slrn_art_hangup,
   NULL,			       /* enter_mode_hook */
   SLRN_ARTICLE_MODE,
};

/*}}}*/

void slrn_init_article_mode (void) /*{{{*/
{
   char  *err = "Unable to create Article keymap!";
   char numbuf[2];
   char ch;
   
   if (NULL == (Slrn_Article_Keymap = SLang_create_keymap ("article", NULL)))
     slrn_exit_error (err);
   
   Art_Mode_Cap.keymap = Slrn_Article_Keymap;
   
   Slrn_Article_Keymap->functions = Art_Functions;
   Art_Functions_Ptr = Art_Functions;
   
   numbuf[1] = 0;
   
   for (ch = '0'; ch <= '9'; ch++)
     {
	numbuf[0] = ch;
	SLkm_define_key (numbuf, (FVOID_STAR) goto_header_number, Slrn_Article_Keymap);
     }
#if SLRN_HAS_GROUPLENS
   numbuf[0] = '0';
   /* Steal '0' for use as a prefix for rating. */
   SLkm_define_key  (numbuf, (FVOID_STAR) grouplens_rate_article, Slrn_Article_Keymap);
#endif
   
   SLkm_define_key  ("\033l", (FVOID_STAR) locate_header_by_msgid, Slrn_Article_Keymap);
   SLkm_define_key ("\0331", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0332", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0333", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0334", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0335", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0336", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0337", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0338", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0339", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key ("\0330", (FVOID_STAR) slrn_digit_arg, Slrn_Article_Keymap);
   SLkm_define_key  ("*", (FVOID_STAR) toggle_header_tag, Slrn_Article_Keymap);
   SLkm_define_key  ("#", (FVOID_STAR) num_tag_header, Slrn_Article_Keymap);
   SLkm_define_key  ("\033#", (FVOID_STAR) num_untag_headers, Slrn_Article_Keymap);
#if SLRN_HAS_DECODE
   SLkm_define_key  (":", (FVOID_STAR) decode_article, Slrn_Article_Keymap);
#endif
   SLkm_define_key  (";", (FVOID_STAR) mark_spot, Slrn_Article_Keymap);
   SLkm_define_key  (",", (FVOID_STAR) exchange_mark, Slrn_Article_Keymap);
   SLkm_define_key  ("g", (FVOID_STAR) skip_digest_forward, Slrn_Article_Keymap);
   SLkm_define_key  ("s", (FVOID_STAR) subject_search_forward, Slrn_Article_Keymap);
   SLkm_define_key  ("S", (FVOID_STAR) subject_search_backward, Slrn_Article_Keymap);
   SLkm_define_key  ("a", (FVOID_STAR) author_search_forward, Slrn_Article_Keymap);
   SLkm_define_key  ("A", (FVOID_STAR) author_search_backward, Slrn_Article_Keymap);
   SLkm_define_key  ("/", (FVOID_STAR) article_search, Slrn_Article_Keymap);
   SLkm_define_key  ("\033^C", (FVOID_STAR) cancel_article, Slrn_Article_Keymap);
   SLkm_define_key  ("o", (FVOID_STAR) save_article, Slrn_Article_Keymap);
   SLkm_define_key  ("|", (FVOID_STAR) pipe_article, Slrn_Article_Keymap);
   SLkm_define_key  ("?", (FVOID_STAR) slrn_article_help, Slrn_Article_Keymap);
   SLkm_define_key  ("H", (FVOID_STAR) hide_article, Slrn_Article_Keymap);
   SLkm_define_key  ("L", (FVOID_STAR) goto_last_read, Slrn_Article_Keymap);
   SLkm_define_key  ("N", (FVOID_STAR) skip_to_next_group, Slrn_Article_Keymap);
   SLkm_define_key  ("n", (FVOID_STAR) art_next_unread, Slrn_Article_Keymap);
   SLkm_define_key  ("j", (FVOID_STAR) goto_article, Slrn_Article_Keymap);
   SLkm_define_key  ("p", (FVOID_STAR) art_prev_unread, Slrn_Article_Keymap);
   SLkm_define_key  ("P", (FVOID_STAR) slrn_post_cmd, Slrn_Article_Keymap);
   SLkm_define_key  ("d", (FVOID_STAR) delete_header_cmd, Slrn_Article_Keymap);
   SLkm_define_key  ("u", (FVOID_STAR) undelete_header_cmd, Slrn_Article_Keymap);
   SLkm_define_key  ("U", (FVOID_STAR) browse_url, Slrn_Article_Keymap);
   SLkm_define_key  ("x", (FVOID_STAR) art_xpunge, Slrn_Article_Keymap);
   SLkm_define_key  ("t", (FVOID_STAR) toggle_headers, Slrn_Article_Keymap);
   SLkm_define_key  ("T", (FVOID_STAR) toggle_quotes, Slrn_Article_Keymap);
   SLkm_define_key  ("\033a", (FVOID_STAR) toggle_show_author, Slrn_Article_Keymap);
   SLkm_define_key  ("\033d", (FVOID_STAR) thread_delete_cmd, Slrn_Article_Keymap);
   SLkm_define_key  ("\033p", (FVOID_STAR) get_parent_header, Slrn_Article_Keymap);
   SLkm_define_key  ("\033^P", (FVOID_STAR) get_children_headers, Slrn_Article_Keymap);
   SLkm_define_key  ("\033t", (FVOID_STAR) toggle_collapse_threads, Slrn_Article_Keymap);
   SLkm_define_key  ("\t", (FVOID_STAR) skip_quoted_text, Slrn_Article_Keymap);
   SLkm_define_key  (" ", (FVOID_STAR) art_pagedn, Slrn_Article_Keymap);
   SLkm_define_key  (".", (FVOID_STAR) slrn_repeat_last_key, Slrn_Article_Keymap);
   SLkm_define_key  ("\r", (FVOID_STAR) art_pagedn, Slrn_Article_Keymap);
#ifdef __os2__
   SLkm_define_key  ("^@S", (FVOID_STAR) art_pageup, Slrn_Article_Keymap);
   SLkm_define_key  ("\xE0S", (FVOID_STAR) art_pageup, Slrn_Article_Keymap);
#else
   SLkm_define_key  ("^?", (FVOID_STAR) art_pageup, Slrn_Article_Keymap);
#endif
   SLkm_define_key  ("b", (FVOID_STAR) art_pageup, Slrn_Article_Keymap);
   SLkm_define_key  ("q", (FVOID_STAR) art_quit, Slrn_Article_Keymap);
   SLkm_define_key  ("F", (FVOID_STAR) forward_article, Slrn_Article_Keymap);
   SLkm_define_key  ("f", (FVOID_STAR) followup, Slrn_Article_Keymap);
   SLkm_define_key  ("K", (FVOID_STAR) create_score, Slrn_Article_Keymap);
   SLkm_define_key  ("W", (FVOID_STAR) wrap_article, Slrn_Article_Keymap);
   SLkm_define_key  ("r", (FVOID_STAR) reply_cmd, Slrn_Article_Keymap);
   SLkm_define_key  ("=", (FVOID_STAR) next_header_same_subject, Slrn_Article_Keymap);
   SLkm_define_key  ("!", (FVOID_STAR) next_high_score, Slrn_Article_Keymap);
   SLkm_define_key  ("<", (FVOID_STAR) art_bob, Slrn_Article_Keymap);
   SLkm_define_key  (">", (FVOID_STAR) art_eob, Slrn_Article_Keymap);
   SLkm_define_key  ("^R", (FVOID_STAR) slrn_redraw, Slrn_Article_Keymap);
   SLkm_define_key  ("^L", (FVOID_STAR) slrn_redraw, Slrn_Article_Keymap);
   SLkm_define_key  ("^Z", (FVOID_STAR) art_suspend_cmd, Slrn_Article_Keymap);
   SLkm_define_key  ("^P", (FVOID_STAR) header_up, Slrn_Article_Keymap);
   SLkm_define_key  ("^M", (FVOID_STAR) art_linedn, Slrn_Article_Keymap);
#ifdef __os2__
   SLkm_define_key  ("\033^@H", (FVOID_STAR) art_lineup, Slrn_Article_Keymap);
   SLkm_define_key  ("\033\xE0H", (FVOID_STAR) art_lineup, Slrn_Article_Keymap);
   SLkm_define_key  ("\033^@P", (FVOID_STAR) art_linedn, Slrn_Article_Keymap);
   SLkm_define_key  ("\033\xE0P", (FVOID_STAR) art_linedn, Slrn_Article_Keymap);
   SLkm_define_key  ("^@H", (FVOID_STAR) header_up, Slrn_Article_Keymap);
   SLkm_define_key  ("\xE0H", (FVOID_STAR) header_up, Slrn_Article_Keymap);
   SLkm_define_key  ("^@P", (FVOID_STAR) header_down, Slrn_Article_Keymap);
   SLkm_define_key  ("\xE0P", (FVOID_STAR) header_down, Slrn_Article_Keymap);
   SLkm_define_key  ("^@M", (FVOID_STAR) art_right, Slrn_Article_Keymap);
   SLkm_define_key  ("\xE0M", (FVOID_STAR) art_right, Slrn_Article_Keymap);
   SLkm_define_key  ("^@K", (FVOID_STAR) art_left, Slrn_Article_Keymap);
   SLkm_define_key  ("\xE0K", (FVOID_STAR) art_left, Slrn_Article_Keymap);
#else
   SLkm_define_key  ("\033\033[A", (FVOID_STAR) art_lineup, Slrn_Article_Keymap);
   SLkm_define_key  ("\033\033OA", (FVOID_STAR) art_lineup, Slrn_Article_Keymap);
   SLkm_define_key  ("\033\033[B", (FVOID_STAR) art_linedn, Slrn_Article_Keymap);
   SLkm_define_key  ("\033\033OB", (FVOID_STAR) art_linedn, Slrn_Article_Keymap);
   SLkm_define_key  ("\033[A", (FVOID_STAR) header_up, Slrn_Article_Keymap);
   SLkm_define_key  ("\033OA", (FVOID_STAR) header_up, Slrn_Article_Keymap);
   SLkm_define_key  ("\033[B", (FVOID_STAR) header_down, Slrn_Article_Keymap);
   SLkm_define_key  ("\033OB", (FVOID_STAR) header_down, Slrn_Article_Keymap);
   SLkm_define_key  ("\033[C", (FVOID_STAR) art_right, Slrn_Article_Keymap);
   SLkm_define_key  ("\033OC", (FVOID_STAR) art_right, Slrn_Article_Keymap);
   SLkm_define_key  ("\033[D", (FVOID_STAR) art_left, Slrn_Article_Keymap);
   SLkm_define_key  ("\033OD", (FVOID_STAR) art_left, Slrn_Article_Keymap);
#endif
   SLkm_define_key  ("\033S", (FVOID_STAR) toggle_sort, Slrn_Article_Keymap);
   SLkm_define_key  ("^U", (FVOID_STAR) header_pageup, Slrn_Article_Keymap);
   SLkm_define_key  ("\033V", (FVOID_STAR) header_pageup, Slrn_Article_Keymap);
#ifdef __os2__
   SLkm_define_key  ("^@I", (FVOID_STAR) header_pageup, Slrn_Article_Keymap);
   SLkm_define_key  ("\xE0I", (FVOID_STAR) header_pageup, Slrn_Article_Keymap);
   SLkm_define_key  ("^@Q", (FVOID_STAR) header_pagedn, Slrn_Article_Keymap);
   SLkm_define_key  ("\xE0Q", (FVOID_STAR) header_pagedn, Slrn_Article_Keymap);
#else
   SLkm_define_key  ("\033[5~", (FVOID_STAR) header_pageup, Slrn_Article_Keymap);
   SLkm_define_key  ("\033[6~", (FVOID_STAR) header_pagedn, Slrn_Article_Keymap);
#endif
   SLkm_define_key  ("^D", (FVOID_STAR) header_pagedn, Slrn_Article_Keymap);
   SLkm_define_key  ("^V", (FVOID_STAR) header_pagedn, Slrn_Article_Keymap);
   SLkm_define_key  ("\033>", (FVOID_STAR) header_eob, Slrn_Article_Keymap);
   SLkm_define_key  ("\033<", (FVOID_STAR) header_bob, Slrn_Article_Keymap);
   SLkm_define_key  ("c", (FVOID_STAR) catch_up_all, Slrn_Article_Keymap);
   SLkm_define_key  ("\033c", (FVOID_STAR) catch_up_all, Slrn_Article_Keymap);
   SLkm_define_key  ("\033u", (FVOID_STAR) un_catch_up_all, Slrn_Article_Keymap);
   SLkm_define_key  ("\033C", (FVOID_STAR) catch_up_to_here, Slrn_Article_Keymap);
   SLkm_define_key  ("C", (FVOID_STAR) catch_up_to_here, Slrn_Article_Keymap);
   SLkm_define_key  ("\033U", (FVOID_STAR) un_catch_up_to_here, Slrn_Article_Keymap);
   SLkm_define_key  ("\033R", (FVOID_STAR) toggle_rot13, Slrn_Article_Keymap);
#if 0
#if SLRN_HAS_SPOILERS
   SLkm_define_key  ("\033?", (FVOID_STAR) show_spoilers, Slrn_Article_Keymap);
#endif
#endif
   SLkm_define_key  ("^N", (FVOID_STAR) header_down, Slrn_Article_Keymap);
   SLkm_define_key  ("^", (FVOID_STAR) enlarge_window, Slrn_Article_Keymap);
   SLkm_define_key  ("^^", (FVOID_STAR) shrink_window, Slrn_Article_Keymap);
   
   /* mouse (left/middle/right) */
   SLkm_define_key  ("\033[M\040", (FVOID_STAR) art_mouse_left, Slrn_Article_Keymap);
   SLkm_define_key  ("\033[M\041", (FVOID_STAR) art_mouse_middle, Slrn_Article_Keymap);
   SLkm_define_key  ("\033[M\042", (FVOID_STAR) art_mouse_right, Slrn_Article_Keymap);
   
   if (SLang_Error) slrn_exit_error (err);
}

/*}}}*/


/*}}}*/
/*{{{ slrn_article_mode and support functions */


static void slrn_art_hangup (int sig) /*{{{*/
{
   if (Slrn_Current_Header != NULL)
     undelete_header_cmd ();		       /* in case we are reading one */
   art_quit ();
   if (Slrn_Group_Hangup_Hook != NULL) (*Slrn_Group_Hangup_Hook) (sig);
   slrn_quit (sig);
}

/*}}}*/

static void mark_ranges_read (Slrn_Range_Type *r) /*{{{*/
{
   Slrn_Header_Type *h = Slrn_First_Header;
   int min, max;
   
   while ((r != NULL) && (h != NULL))
     {
	min = r->min;
	max = r->max;
	while (h != NULL)
	  {
	     if (h->number < min)
	       {
		  h = h->real_next;
		  continue;
	       }
	     
	     if (h->number > max)
	       {
		  break;
	       }
	     
	     h->flags |= HEADER_READ;
	     h = h->real_next;
	  }
	r = r->next;
     }
}

/*}}}*/

/* If all > 0, get last 'all' headers from server independent of whether
 *             they have been read or not.
 * If all < 0, and this is not the first time this group has been accessed,
 *             either during this session or during previous sessions, get
 *             that last 'all' UNREAD articles.
 * Otherwise,  fetch ALL UNREAD headers from the server.
 */
int slrn_select_article_mode (Slrn_Group_Type *g, int all, int score) /*{{{*/
{
   int min, max;
   int smin, smax;
   Slrn_Range_Type *r;
   int status;
   
   User_Aborted_Group_Read = 0;
   Headers = Slrn_First_Header = NULL;
   Threads_Collapsed = 0;
   Same_Subject_Start_Header = NULL;
   Number_Killed = Number_High_Scored = Number_Low_Scored = 0;
   
   Current_Group = g;
   r = &g->range;
   Slrn_Current_Group_Name = g->name;
   Slrn_Score_After_XOver = Slrn_Server_Obj->sv_has_xover;

#if SLRN_HAS_SLANG
   SLang_run_hooks ("pre_article_mode_hook", NULL, NULL);
#endif
   
   if (score && (1 == slrn_open_score (Slrn_Current_Group_Name)))
     Perform_Scoring = 1;
   else Perform_Scoring = 0;
   
   Slrn_Server_Min = r->min;
   Slrn_Server_Max = r->max;
   r = r->next;
   /* Now r points to ranges already read.  */
   
   status = 0;
   
   if (all > 0)
     {
	min = Slrn_Server_Max - all + 1;
	if (min < Slrn_Server_Min) min = Slrn_Server_Min;
	status = get_headers (min, Slrn_Server_Max, &all);
	if (status != -1)
	  mark_ranges_read (r);
     }
   else
     {
	if ((all < 0) && (r != NULL))
	  {
	     int unread;

	     /* This condition will occur when the user wants to read unread
	      * articles that occur in a gap, i.e., RRRUUUUURRRUUUUUUU and
	      * we need to dig back far enough below the last group of read
	      * ones until we have retrieved abs(all) articles.
	      * 
	      * The problem with this is that some articles may not be 
	      * available on the server which means that the number to
	      * go back will be under estimated.
	      */
	     all = -all;
	     
	     while (r->next != NULL) r = r->next;
	     /* Go back through previously read articles counting unread.
	      * If number unread becomes greater than the number that we
	      * intend to read, then we know where to start querying 
	      * the server.
	      */
	     unread = 0;
	     max = Slrn_Server_Max;
	     while (r->prev != NULL)
	       {
		  unread += max - r->max;
		  if (unread >= all) break;
		  max = r->min - 1;
		  r = r->prev;
	       }
	     
	     if (unread >= all)
	       {
		  /* This may be problematic if some articles are missing on 
		   * the server.  If that is the case, smin will be to high
		   * and we will fall short of the goal.
		   */
		  smin = r->max + (unread - all) + 1;
	       }
	     else smin = Slrn_Server_Min;
	     smax = Slrn_Server_Max;
	     r = r->next;
	  }
	else
	  {
	     /* all == 0, or no previously read articles. */
	     smin = Slrn_Server_Min;
	     smax = Slrn_Server_Max;
	     if (r != NULL)
	       {
		  Slrn_Range_Type *r1;
		  
		  /* Estimate how many are available to read */
		  all = smax - r->max;
#if 0				       /* is this correct?? */
		  all++;
#endif
		  
		  /* Now subtract the ones that we have already read. */
		  r1 = r->next;
		  while (r1 != NULL)
		    {
		       all -= (r1->max - r1->min) + 1;
		       r1 = r1->next;
		    }
		  /* This condition should never arise */
		  if (all == 0) all = smax - smin + 1;
	       }
	     else all = smax - smin + 1;
	  }
	
	while (r != NULL)
	  {
	     if (r->min > smin)
	       {
		  min = smin;
		  max = r->min - 1;

		  status = get_headers (min, max, &all);

		  if (status == -1)
		    break;
		  
		  if (status == 0)
		    {
		       Slrn_Groups_Dirty = 1;
		       r->min = min;
		    }
		  
		  smin = r->max + 1;
	       }
	     else
	       {
		  smin = r->max + 1;
	       }
	     r = r->next;
	  }
	
	if (smin <= smax)
	  {
	     status = get_headers (smin, smax, &all);
	  }
     }
   
   if ((status == -1) || SLKeyBoard_Quit)
     {
	if (SLang_Error == USER_BREAK)
	  slrn_error_now ("Group transfer aborted.");
	else
	  slrn_error_now ("Server read failed.");
	
	/* This means that we cannot update ranges for this group because 
	 * the user aborted and update_ranges assumes that all articles 
	 * upto server max are present.
	 */
	User_Aborted_Group_Read = 1;
	Last_Current_Mode = Slrn_Current_Mode;
	art_quit ();
     }
   else if (Slrn_Score_After_XOver 
       && Perform_Scoring)
     score_headers ();
   
   if (Headers == NULL)
     {
	slrn_close_score ();
	if (Number_Killed)
	  slrn_error ("No unread articles found. (%d killed)", Number_Killed);
	else
	  slrn_error ("No unread articles found.");

	free_kill_lists_and_update ();
	Slrn_Current_Group_Name = NULL;
	if (SLang_Error == USER_BREAK) return -1;
	else return -2;
     }
   
   make_hash_table ();
   
   /* This must go here to fix up the next/prev pointers */
   sort_by_server_number ();

   extract_real_names ();

   slrn_chmap_fix_headers ();

   Last_Current_Mode = Slrn_Current_Mode;
   if (Slrn_Current_Mode != NULL)
     Slrn_Group_Hangup_Hook = Slrn_Current_Mode->hangup_fun;
   else Slrn_Group_Hangup_Hook = NULL;

   Slrn_Current_Mode = &Art_Mode_Cap;
   
   Slrn_Current_Header = Headers;
   Last_Cursor_Row = 0;
   Mark_Header = NULL;
   init_header_window_struct ();
   
   Article_Current_Line = Slrn_Article_Lines = NULL;
   At_End_Of_Article = NULL;
   Header_Showing = NULL;
   SLMEMSET ((char *) &Slrn_Article_Window, 0, sizeof (SLscroll_Window_Type));
   
   set_article_visibility (0);

#if SLRN_HAS_SLANG
   (void) SLang_run_hooks ("article_mode_hook", NULL, NULL);
#endif   
   sort_by_sorting_mode ();
   header_bob ();
   
#if SLRN_HAS_GROUPLENS
   /* slrn_set_suspension (1); */
   Num_GroupLens_Rated = slrn_get_grouplens_scores ();
   /* slrn_set_suspension (0); */
#endif
   
   quick_help ();
   
   if (Slrn_Startup_With_Article) art_pagedn ();
   
   if (SLang_Error == 0)
     {
	if (Perform_Scoring 
	    /* && (Number_Killed || Number_High_Scored) */
	    )
	  {
#if SLRN_HAS_GROUPLENS
	     if (Num_GroupLens_Rated != -1)
	       {
		  slrn_message ("Num Killed: %u, Num High: %u, Num Low: %u, Num GroupLens Rated: %d",
				Number_Killed, Number_High_Scored, Number_Low_Scored,
				Num_GroupLens_Rated);
	       }
	     else
#endif
	       slrn_message ("Num Killed: %u, Num High: %u, Num Low: %u",
			     Number_Killed, Number_High_Scored, Number_Low_Scored);
	  }
	
	else slrn_clear_message ();
     }
   
   Number_Low_Scored = Number_Killed = Number_High_Scored = 0;
   return 0;
}

/*}}}*/

/*}}}*/

/*{{{ screen update functions */

static void quick_help (void) /*{{{*/
{
   char *msg;
   
   if (Slrn_Batch) return;

   if (Article_Visible == 0)
     {
	msg = "SPC:Select  Ctrl-D:PgDn  Ctrl-U:PgUp  d:Mark-as-Read  n:Next  p:Prev  q:Quit";
	if (Slrn_Header_Help_Line != NULL) msg = Slrn_Header_Help_Line;
     }
   else
     {
	msg = "SPC:Pgdn  B:PgUp  u:Un-Mark-as-Read  f:Followup  n:Next  p:Prev  q:Quit";
	if (Slrn_Art_Help_Line != NULL) msg = Slrn_Art_Help_Line;
     }

   if (0 == slrn_message (msg))
     Slrn_Message_Present = 0;
}

/*}}}*/

static void write_rot13 (unsigned char *buf) /*{{{*/
{
   static int init_rot13;
   static char rot13buf[256];
   unsigned char ch;
   int i;
   
   if (init_rot13 == 0)
     {
	init_rot13 = 1;
	for (i = 0; i < 256; i++)
	  {
	     rot13buf[i] = i;
	  }
	
	for (i = 'A'; i <= 'M'; i++)
	  {
	     rot13buf[i] = i + 13;
	     /* Now take care of lower case ones */
	     rot13buf[i + 32] =  i + 32 + 13;
	  }
	
	for (i = 'N'; i <= 'Z'; i++)
	  {
	     rot13buf[i] = i - 13;
	     /* Now take care of lower case ones */
	     rot13buf[i + 32] =  i + 32 - 13;
	  }
     }
   
   while ((ch = *buf++) != 0)
     {
	ch = rot13buf[ch];
	SLsmg_write_nchars ((char *) &ch, 1);
     }
}

/*}}}*/

/*{{{ utility routines */

#if SLRN_HAS_SPOILERS
/* write out the line, replacing all printable chars with '*' */
static void write_spoiler (unsigned char *buf) /*{{{*/
{
   unsigned char ch;
   
   Spoilers_Visible = Slrn_Current_Header;
   
   if (Slrn_Spoiler_Char == ' ')
     return;
   
   while ((ch = *buf++) != 0)
     {
	if (!isspace(ch)) ch = Slrn_Spoiler_Char;
	SLsmg_write_nchars ((char *) &ch, 1);
     }
}
/*}}}*/
#endif

static void draw_tree (Slrn_Header_Type *h) /*{{{*/
{
   unsigned char *b;
   
#ifndef __os2__
   if (SLtt_Has_Alt_Charset == 0)
     {
	SLsmg_write_string ((char *) h->tree);
	SLsmg_write_string ("  ");
	return;
     }
   SLsmg_set_char_set (1);
#endif
   
   slrn_set_color (TREE_COLOR);
   
   if (*h->tree) SLsmg_write_string ((char *) h->tree);
   
   if (h->flags & FAKE_CHILDREN)
     {
	static unsigned char buf[2] = {SLSMG_UTEE_CHAR,SLSMG_HLINE_CHAR};
	b = buf;
	SLsmg_forward (-1);
	SLsmg_write_char ((char)SLSMG_ULCORN_CHAR);
     }
   else if ((h->sister == NULL) ||
	    ((h->sister->flags & FAKE_PARENT) && ((h->flags & FAKE_PARENT) == 0)))
     {
	static unsigned char buf[2] = {SLSMG_LLCORN_CHAR,SLSMG_HLINE_CHAR};
	b = buf;
     }
   else
     {
	static unsigned char buf[2] = {SLSMG_LTEE_CHAR,SLSMG_HLINE_CHAR
	};
	b = buf;
     }
   SLsmg_write_nchars ((char *) b, 2);
#ifndef __os2__
   SLsmg_set_char_set (0);
#endif
}

/*}}}*/

/*{{{ check_subject */

/*
 * This checks if subjects should be printed (correctly I hope)
 * hacked: articles in a tree shouldn't display their subject if the
 *          subject is already displayed (i.e. at top)
 * To add: take more Re:'s into account (currently, one is allowed)
 */
int Slrn_Show_Thread_Subject = 0;
static int check_subject (Slrn_Header_Type *h) /*{{{*/
{
   char *psubj, *subj;
   
   subj = h->subject;
   psubj = h->prev->subject;	       /* used to be: h->parent->subject */
   if ((subj == NULL) || (psubj == NULL)) return 1;

   return subject_cmp ((unsigned char *)subj, (unsigned char *)psubj);
}

/*}}}*/

/*}}}*/

#if SLRN_HAS_END_OF_THREAD
static int display_end_of_thread (Slrn_Header_Type *h) /*{{{*/
{
   Slrn_Header_Type *parent, *next_parent;
   
   if ((h == NULL)
       || (h->parent == NULL)
       || (h->child != NULL)
       || (h->next == NULL))
     return -1;
   
   parent = h->parent;
   while (parent->parent != NULL) parent = parent->parent;
	     
   next_parent = h->next;
   while (next_parent->parent != NULL) 
     next_parent = next_parent->parent;
   
   if (next_parent != parent)
     {
	slrn_message ("End of Thread.");
	return 0;
     }
   
   if ((h->sister == NULL) 
       || (h->parent != h->next->parent)
       || (FAKE_PARENT & (h->next->flags ^ h->flags)))
     {
	/* The last test involving ^ is necessary because the two can be
	 * sisters except that one can have a fake parent.  If this is the
	 * case, we are at the end of a subthread.
	 */
	slrn_message ("End of Sub-Thread");
	return 0;
     }
   
   return -1;
}
/*}}}*/
#endif

#if SLRN_HAS_GROUPLENS
static int Update_Column_Offset = 0;
#endif

static void write_header_flags (Slrn_Header_Type *h, int row) /*{{{*/
{
   unsigned int flags = h->flags;

#if 1
   if (Slrn_Use_Header_Numbers)
     {
	slrn_set_color (HEADER_NUMBER_COLOR);
	SLsmg_printf ("%2d", row);
	if (row > Largest_Header_Number) Largest_Header_Number = row;
     }
   else SLsmg_write_string ("  ");
#endif

   if (flags & HEADER_NTAGGED)
     {
	slrn_set_color (HIGH_SCORE_COLOR);
	SLsmg_printf ("%2d",
		      h->tag_number);
     }
   else
     {
	if ((flags & HEADER_HIGH_SCORE)
	    || ((flags & FAKE_HEADER_HIGH_SCORE)
		&& (h->child != NULL)
		&& (h->child->flags & HEADER_HIDDEN)))
	  {
	     slrn_set_color (HIGH_SCORE_COLOR);
	     SLsmg_printf ("!%c",
			   ((flags & HEADER_TAGGED) ? '*': ' '));
	  }
	else
	  {
	     slrn_set_color (0);
	     SLsmg_printf (" %c",
			   ((flags & HEADER_TAGGED) ? '*': ' '));
	  }
     }
   slrn_set_color (0);
   if (Slrn_Sorting_Mode == 0)
     {
	SLsmg_printf ("%c %6d ",
		      ((flags & HEADER_READ) ? 'D': '-'),
		      h->number);
     }
#if SLRN_HAS_SORT_BY_SCORE
   else if (Slrn_Display_Score)
     {
	SLsmg_printf ("%c %5d ",
		      ((flags & HEADER_READ) ? 'D': '-'),
		      ((h->child != NULL) && (h->child->flags & HEADER_HIDDEN))
		      ? h->thread_score : h->score);
     }
#endif
   else SLsmg_printf ("%c%5d:",
		      ((flags & HEADER_READ) ? 'D': '-'),
		      h->lines);
   
#if SLRN_HAS_GROUPLENS
# define UPDATE_COLUMN_OFFSET Update_Column_Offset
# define SLRN_GROUPLENS_DISPLAY_WIDTH 5
   if (Num_GroupLens_Rated != -1)
     {
	char buf [SLRN_GROUPLENS_DISPLAY_WIDTH], *b, *bmax;
	int pred = h->gl_pred;
	
	b = buf;
	bmax = b + SLRN_GROUPLENS_DISPLAY_WIDTH;
	
	if (pred < 0)
	  {
	     while (b < bmax) *b++ = ' ';
	     buf [SLRN_GROUPLENS_DISPLAY_WIDTH / 2] = '?';
	  }
	else
	  {
	     while ((pred > 0) && (b < bmax))
	       {
		  pred--;
		  *b++ = '*';
	       }
	     
	     while (b < bmax) *b++ = ' ';
	  }
	
	slrn_set_color (GROUPLENS_DISPLAY_COLOR);
	SLsmg_write_nchars (buf, SLRN_GROUPLENS_DISPLAY_WIDTH);
	slrn_set_color (0);
	
	Update_Column_Offset = SLRN_GROUPLENS_DISPLAY_WIDTH;
     }
   else Update_Column_Offset = 0;
#else
# define UPDATE_COLUMN_OFFSET 0
#endif
}

/*}}}*/

/*}}}*/

static void write_header_subject (Slrn_Header_Type *h, int row, unsigned int width)
{
   (void) width;
   
   if ((h->next != NULL)
       && (h->next->flags & HEADER_HIDDEN))
     {
	slrn_set_color (THREAD_NUM_COLOR);
	SLsmg_printf (" %2d ",
		      1 + h->num_children);
     }
   else
     {
	SLsmg_write_string ("    ");
	if ((h->parent != NULL) || (h->flags & FAKE_CHILDREN))
	  draw_tree (h);
     }
   
   slrn_set_color (SUBJECT_COLOR);
   if ((Slrn_Show_Thread_Subject)
       /* || (0 == h->num_children) */
       || (h->parent == NULL)
       || (row == 1)
       || check_subject (h))
     SLsmg_write_string (h->subject);
   else
     SLsmg_write_nchars (">", 1);	/* subthread */
}

static void write_header_author (Slrn_Header_Type *h, int row, unsigned int width)
{
   unsigned int w;
   char *author;
	
   (void) row;
   
   slrn_set_color (AUTHOR_COLOR);
   
   if (Slrn_Show_Author_Realname)
     {
	author = h->realname;
	w = h->realname_len;
     }
   else
     {
	author = h->from;
	if (author == NULL) author = "";
	w = strlen (author);
     }
   
   if (w > width) 
     {
	w = width;
	if (w == 0) w = SLtt_Screen_Cols;
     }
	
   SLsmg_write_nchars (author, w);
   while (w < width)
     {
	SLsmg_write_nchars (" ", 1);
	w++;
     }
}

static void display_header_line (Slrn_Header_Type *h, int row)
{   
   write_header_flags (h, row);
   SLsmg_erase_eol ();
   
   if (Slrn_Show_Author > 1)
     {
	SLsmg_write_char ('[');
	write_header_author (h, row, 12);
	SLsmg_set_color (0);
	SLsmg_write_char (']');
	
	/* SLsmg_gotorc (row, 24 + UPDATE_COLUMN_OFFSET); */
	write_header_subject (h, row, 0);
     }
   else
     {
	write_header_subject (h, row, 0);
	if (Slrn_Show_Author)
	  {
	     SLsmg_gotorc (row, 50 + UPDATE_COLUMN_OFFSET);
	     SLsmg_write_char (' ');
	     write_header_author (h, row, SLtt_Screen_Cols);
	  }
     }
   SLsmg_erase_eol ();
}

static void display_article_line (Slrn_Article_Line_Type *l)
{
   char *lbuf = l->buf;
   
   if (l->flags & HEADER_LINE)
     {
	if ((unsigned char)*lbuf > (unsigned char)' ')
	  {
	     lbuf = slrn_strchr (l->buf, ':');
	     if (lbuf != NULL)
	       {
		  lbuf++;
		  slrn_set_color (SLRN_HEADER_KEYWORD_COLOR);
		  SLsmg_write_nchars (l->buf, lbuf - l->buf);
	       }
	  }
	slrn_set_color (HEADER_COLOR);
	SLsmg_write_string (lbuf);
     }
   else
     {
	if (l->flags & QUOTE_LINE)
	  slrn_set_color (QUOTE_COLOR);
	else if (l->flags & SIGNATURE_LINE)
	  slrn_set_color (SIGNATURE_COLOR);
	else slrn_set_color (ARTICLE_COLOR);
#if SLRN_HAS_SPOILERS
	if (l->flags & SPOILER_LINE)
	  write_spoiler ((unsigned char *) lbuf);
	else
#endif
	  if (Do_Rot13) write_rot13 ((unsigned char *) lbuf);
	else SLsmg_write_string (lbuf);
     }
}

static void art_update_screen (void) /*{{{*/
{
   Slrn_Header_Type *h;
   Slrn_Article_Line_Type *l;
   int height;
   int row;
   int c0;
   
   At_End_Of_Article = NULL;
#if SLRN_HAS_SPOILERS
   Spoilers_Visible = NULL;
#endif
   if (Slrn_Full_Screen_Update) Largest_Header_Number = 0;

   /* erase last cursor */
   if (Last_Cursor_Row >= 0)
     {
	SLsmg_gotorc (Last_Cursor_Row, 0);
	if (Slrn_Use_Header_Numbers)
	  {
	     slrn_set_color (HEADER_NUMBER_COLOR);
	     SLsmg_printf ("%2d", Last_Cursor_Row);
	  }
	else SLsmg_write_string ("  ");
     }
   
   height = Slrn_Header_Window.nrows;

   h = (Slrn_Header_Type *) Slrn_Header_Window.top_window_line;
   SLscroll_find_top (&Slrn_Header_Window);
   if (h != (Slrn_Header_Type *) Slrn_Header_Window.top_window_line)
     {
	Slrn_Full_Screen_Update = 1;
	h = (Slrn_Header_Type *) Slrn_Header_Window.top_window_line;
     }
   Last_Cursor_Row = 1 + (int) Slrn_Header_Window.window_row;
   
   SLsmg_gotorc (height + 1, 0);
   slrn_set_color (STATUS_COLOR);
   SLsmg_printf ("News Group: %s", Slrn_Current_Group_Name);
   slrn_print_percent (height + 1, 60, 
		       &Slrn_Header_Window);
   
   slrn_set_color (0);
   
   
   if (Slrn_Full_Screen_Update)
     {
	for (row = 1; row <= height; row++)
	  {
	     SLsmg_gotorc (row, 0);
	     SLsmg_erase_eol ();

	     while ((h != NULL) && (h->flags & HEADER_HIDDEN))
	       h = h->next;
	     
	     if (h == NULL)
	       continue;
	     
	     display_header_line (h, row);
	  
	     h = h->next;
	     slrn_set_color (0);
	  }
     }
   
   l = (Slrn_Article_Line_Type *) Slrn_Article_Window.top_window_line;

   if (Article_Visible)
     {
	SLscroll_find_top (&Slrn_Article_Window);
     }
   
   if (Article_Visible 
       && (Slrn_Full_Screen_Update 
	   || (l != Article_Current_Line)
	   || (l != (Slrn_Article_Line_Type *) Slrn_Article_Window.top_window_line)))
     {
	
  	l = Article_Current_Line;
	
	row = height + 2;
	height = SLtt_Screen_Rows - 2;
	
	
	SLsmg_gotorc (height, 0);
	slrn_set_color (STATUS_COLOR);
	if (HScroll) SLsmg_write_char ('<'); else SLsmg_write_char (' ');
	SLsmg_printf (" %d : %s", Header_Showing->number, Header_Showing->subject);
	slrn_print_percent (height, 60, &Slrn_Article_Window);
	slrn_set_color (0);
	
	c0 = HScroll;
	SLsmg_set_screen_start (NULL, &c0);
		
	while (row < height)
	  {
	     SLsmg_gotorc (row, 0);
	     
	     if (l != NULL) 
	       {
		  if (l->flags & HIDDEN_LINE)
		    {
		       l = l->next;
		       continue;
		    }
		  
		  display_article_line (l);
		  l = l->next;
	       }
#if SLRN_HAS_TILDE_FEATURE
	     else if (Slrn_Use_Tildes)
	       {
		  slrn_set_color (SLRN_TILDE_COLOR);
		  SLsmg_write_char ('~');
	       }
#endif

	     slrn_set_color (0);
	     SLsmg_erase_eol ();
	     
	     row++;
	  }
	
	if (((l == NULL) 
	     || ((l->flags & SIGNATURE_LINE) && Slrn_Sig_Is_End_Of_Article))
	    && (Slrn_Current_Header == Header_Showing))
	  At_End_Of_Article = Slrn_Current_Header;
	
	SLsmg_set_screen_start (NULL, NULL);
     }

   if (Slrn_Use_Mouse) slrn_update_article_menu ();
   else
     slrn_update_top_status_line ();

   if (Slrn_Message_Present == 0) 
     {
#if SLRN_HAS_SPOILERS
	if (Spoilers_Visible != NULL)
	  slrn_message ("Spoilers visible!");
	else
#endif
#if SLRN_HAS_END_OF_THREAD
	  if (Article_Visible
	      && (-1 != display_end_of_thread (Slrn_Current_Header)))
	    /* do nothing */ ;
	else
#endif
	  quick_help ();
     }
   
   SLsmg_gotorc (Last_Cursor_Row, 0);
   write_header_flags (Slrn_Current_Header, Last_Cursor_Row);
   SLsmg_gotorc (Last_Cursor_Row, 0);
   slrn_set_color (CURSOR_COLOR);
   SLsmg_write_string ("->");
   slrn_set_color (0);
   
   /*   if (Slrn_Show_Author > 1) SLsmg_gotorc (Last_Cursor_Row, 7); */
   Slrn_Full_Screen_Update = 0;
}

/*}}}*/


/*}}}*/

