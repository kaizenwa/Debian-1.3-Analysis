/*  Copyright (c) 1995 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <string.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#include "slrn.h"
#include "misc.h"
#include "util.h"

static char *Global_Help [] = 
{
   " For more information about slrn, check out the newsgroups:",
   "   news.software.readers",
   "   alt.lang.s-lang",
   " Email bug reports, suggestions, comments to davis@space.mit.edu",
   "",
   NULL
};

static char *Art_Help[] =
{
   "Searching:",
     "  /           Search forward in the article.",
     "  a           Author search forward.",
     "  A           Author search backward.",
     "  s           Subject search forward.",
     "  S           Subject search backward.",
     "  =           Skip to next article of specified subject",
     "  !           Skip to next article with high score",
     "  ESC-l       Locate header by message-id.",
     "Commands that affect the article:",
     "  h           Hide window (zoom header window)",
     "  SPACE       Select article or scroll to next page",
     "  DEL, b      Scroll article back one page",
     "  ESC-UP      Scroll article up one line",
     "  ESC-DOWN    Scroll article down one line",
     "  RIGHT       Pan article to the right",
     "  LEFT        Pan article to the left",
     "  W           Wrap or Unwrap article",
     "  t           Toggle on/off the display of unimportant headers.",
     "  T           Toggle on/off the display of quoted lines.",
     "  TAB         Skip beyond quoted text.",
     "  ESC-r       Toggle ROT 13 decryption on and off.",
     "  g           Skip to next digest article.",
     "  ESC Ctrl-C  Cancel article (you must be the author).",
#if SLRN_HAS_GROUPLENS
     "  0           Assign article a GroupLens rating.",
#endif
     "Posting, Replying, Piping, and Saving:",
     "  P           Post a new article (NOT followup)",
     "  f           Followup to current article.",
     "    Note: ESC 1 f ==> all headers will be inserted.",
     "          ESC 2 f ==> the message will be inserted without quoting.",
     "  F           Forward the article to someone",
     "  r           Reply to the poster",
     "  o           Append the article, thread, or tagged articles to a file",
     "  :           Decode article, numerically tagged articles, or thread.",
     "  #           Numerically tag the article for saving/decoding.",
     "  ESC #       Remove numerical tags.",
     "  |           Pipe the article to and external command.",
     "Commands that pertain to the header window:",
     "  d           Mark article as read and move to next unread.",
     "  ESC-d       Mark thread as read and move to next unread.",
     "  u           Mark the article as unread",
     "  p           Move to a previously unread article",
     "  n           Move to the next unread article or next group if at end.",
     "  L           Goto the article that was last read.",
     "  N           Skip to next news group.",
     "  UP          Move to the previous article",
     "  DOWN        Move to the next article",
     "  ;           Set a mark at current position",
     "  ,           Return to the previous mark setting mark first",
     "  q           Quit back to group mode",
     "  Ctrl-^      Decrease the size of the header window by one line",
     "  ^           Increase the size of the header window by one line",
     "  ESC t       Toggle collapse/uncollapse of thread",
     "    Note: A prefix argument may be used to operate on all threads.",
     "  ESC >       Move to the end of the header list",
     "  ESC <       Move to the beginning of the header list",
     "  Ctrl-U      Perform Page-Up on the header window.",
     "  Ctrl-D      Perform Page-Dn on the header window.",
     "  ESC-a       Toggle header display format",
     "  ESC-s       Select sort mode",
     "  ESC-p       Find parent header reading from server if necessary",
     "  ESC 1 ESC p Reconstruct thread.",
     "  ESC Ctrl-P  Find all children of current header (queries server)",
     "  c           Catch-up (Mark ALL articles as read).",
     "  ESC-u       Mark ALL articles as UN-read",
     "  C           Mark articles TO the current position as read",
     "  ESC-U       Mark articles TO current position as UN-read",
     "  K           Create a score entry based on current header.",
     "  ESC 1 K     Edit score file",
     "  *           Mark header so it is un-affected by catchup commands.",
     "  ESC 1 *     Un-mark all headers marked with '*'.",
     "  x           Remove all read articles from list.",
     "Miscellaneous:",
     "  . (Period)  Repeat last key sequence",
     "  U           Search for URL and follow it",
     NULL
};

static char *Group_Help [] =
{
     " Note: The keys are case sensitive!  That is, 's' and 'S' are not the same.",
     "       Some commands take a prefix argument.  This means press ESC, then",
     "       press a digit followed by desired command key sequence.",
     "",
     "Subscribing/Unsubscribing Hints:",
     "  1. Press CAPITAL 'L' and enter the name of the groups to display, e.g.,",
     "      *.music.*  to see all 'music' groups.",
     "  2. Move to the group you want to subscribe to and then press 's'.",
     "      Repeat this step for each group.",
     "  3. Press the Capital-L key again to display only subscribed groups.",
     "Commands that affect the current group:",
     "  P                    Post an article to the current newsgroup",
     "  SPACE, RETURN        Select the current newsgroup.",
     "  ESC 1 SPACE          Select group with article number query.",
     "  ESC 2 SPACE          Select group but do not apply score.",
     "  ESC 3 SPACE          Select group with query but do not apply score.",
     "  s                    Subscribe to the current newsgroup",
     "  ESC 1 s              Subscribe to pattern",
     "  u                    Un-subscribe from the current newsgroup",
     "  ESC 1 u              Un-subscribe to pattern",
     "  c                    Catchup-- mark all articles as read",
     "  ESC u                Un-Catchup-- mark all articles as un-read",
     "Movement Commands:",
     "  ESC <                Move to top of the list",
     "  ESC >                Move to bottom of the list",
     "  Ctrl-V, Ctrl-D       Scroll to next page",
     "  ESC V, Ctrl-U        Scroll to previous page",
     "  DOWN                 Move to next group",
     "  UP                   Move to previous group",
     "Miscellaneous Commands:",
     "  a                    Add a new newsgroup (See 'L' help too!)",
     "  Ctrl-L, Ctrl-R       Redraw the screen.",
     "  Ctrl-Z               Suspend newsreader.",
     "  l                    Toggle listing of groups that have no unread articles.",
     "  L                    Toggle listing of unsubscribed groups.",
     "  ESC 1 L              Hide unsubscribed groups",
     "  /                    Group keyword search.  Use 'l' to search hidden groups.",
     "  q                    Quit newsreader",
     "  G                    Get new news",
     "  K                    Toggle scoring mode",
     "  X                    Force a save of the newsrc file.",
     "  ESC-a                Toggle display of group description on/off.",
     "  Ctrl-X Ctrl-T        Transpose position of groups.",
     "  m                    Move newsgroup to different location.",
     "  . (Period)           Repeat last key sequence",
     NULL
};

#define MAX_HELP_LINES 256
static char *User_Article_Help[MAX_HELP_LINES];
static char *User_Group_Help[MAX_HELP_LINES];

static void do_help (char **help)
{
   int i;
   char **p, *sect = NULL;
   char quit;
   char **this_help;
   
   this_help = p = Global_Help;
   
   slrn_enable_mouse (0);
   
   while (1)
     {
	i = 0;
	if (*p == NULL) break;
	
	slrn_push_suspension (0);

	SLsmg_cls ();
	
	if ((sect != NULL) && (**p == ' '))
	  {
	     SLsmg_set_color (1);
	     SLsmg_gotorc (i, 0);
	     SLsmg_write_string (sect);
	     SLsmg_set_color (0);
	     SLsmg_write_string (" (continued)");
	     i += 2;
	  }
	
	while (i < SLtt_Screen_Rows - 4)
	  {
	     char pp;
	     
	     if (*p == NULL)
	       {
		  if (this_help == help) break;
		  this_help = p = help;
		  sect = NULL;
	       }
	     
	     pp = **p;
	     if ((pp != ' ') && pp)
	       {
		  sect = *p;
		  if ((i + 6) > SLtt_Screen_Rows) break;
		  i++;
		  SLsmg_set_color (1);
	       }
	     
	     SLsmg_gotorc (i, 0);
	     SLsmg_write_string (*p); i++;
	     if (pp && (pp != ' '))
	       {
		  SLsmg_set_color (0);
		  i++;
	       }
	     p++;
	  }
	
	SLsmg_gotorc (i + 1, 0);
	SLsmg_set_color (1);
	
	if ((*p == NULL)
	    && (this_help == Global_Help))
	  {
	     this_help = p = help;
	     sect = NULL;
	  }
	    
	if (*p == NULL)
	  {
	     SLsmg_write_string ("\
Press '?' to start over, or any other key to return to news reader."
				 );
	  }
	else SLsmg_write_string ("\
Press 'q' to quit help, '?' to start over, or any other key to continue."
				 );
	
	slrn_smg_refresh ();
	
	slrn_pop_suspension ();

	SLang_flush_input ();
	quit = SLang_getkey ();
	if (quit == '?')
	  {
	     this_help = p = Global_Help;
	     sect = NULL;
	  }
	else if ((*p == NULL) || ((quit | 0x20)== 'q'))
	    break;
     }
   Slrn_Full_Screen_Update = 1;
   slrn_set_color (0);
   /* slrn_redraw (); */
   SLang_flush_input ();
   slrn_enable_mouse (1);
}

int slrn_parse_helpfile (char *helpfile)
{
   FILE *fp;
   char buf[256];
   char ch;
   char **current_help = NULL;
   int num_lines = 0;
   unsigned char *b;

   if (Slrn_Batch)
     return 0;
   
   if (NULL == (fp = fopen (helpfile, "r"))) return -1;
   while (fgets (buf, sizeof (buf) - 1, fp) != NULL)
     {
	ch = *buf;
	
	/* Skip over common comments */
	if ((ch == '#') || (ch == '%') || (ch == ';') || (ch == '!'))
	  continue;
	
	b = (unsigned char *) slrn_skip_whitespace (buf);
	if (*b == 0) continue;
	
	if (ch == '[')
	  {
	     /* end current help */
	     if (current_help != NULL)
	       {
		  slrn_free (current_help[num_lines]);
		  current_help[num_lines] = NULL;
	       }
	     
	     num_lines = 0;
	     ch = *(buf + 1) | 0x20;
	     if (ch == 'a')
	       {
		  current_help = User_Article_Help;
	       }
	     else if (ch == 'g') current_help = User_Group_Help;
	     else current_help = NULL;
	     
	     continue;
	  }
	
	if (current_help == NULL) continue;
	
	if (MAX_HELP_LINES == num_lines + 1)
	  {
	     current_help[num_lines] = NULL;
	     current_help = NULL;
	     continue;
	  }
	
	slrn_free (current_help [num_lines]);
	
	if (NULL != (current_help [num_lines] = (char *) slrn_strmalloc (buf, 0)))
	  num_lines++;
     }
   if (current_help != NULL)
     {
	slrn_free (current_help[num_lines]);
	current_help[num_lines] = NULL;
     }
   slrn_fclose (fp);
   return 0;
}


void slrn_article_help (void)
{
   char **h;
   if (Slrn_Batch) return;
   if (User_Article_Help[0] != NULL) h = User_Article_Help; else h = Art_Help;
   do_help (h);
}

void slrn_group_help (void)
{
   char **h;
   if (Slrn_Batch) return;
   if (User_Group_Help[0] != NULL) h = User_Group_Help; else h = Group_Help;
   do_help (h);
}
