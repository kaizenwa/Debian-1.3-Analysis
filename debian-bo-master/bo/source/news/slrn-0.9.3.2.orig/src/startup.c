/* -*- mode: C; mode: fold; -*- */
/*  Copyright (c) 1995, 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
/* Read startup .slrnrc file */

#include "config.h"
#include "slrnfeat.h"

/*{{{ Include Files */

#include <stdio.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <string.h>
#include <slang.h>

#include "jdmacros.h"

#include "slrn.h"
#include "group.h"
#include "misc.h"
#include "art.h"
#include "post.h"
#include "startup.h"
#include "score.h"
#include "util.h"
#include "uudecode.h"
#if SLRN_HAS_MIME
# include "mime.h"
#endif
#if SLRN_HAS_GROUPLENS
# include "grplens.h"
#endif
#if SLRN_HAS_SLANG
# include "interp.h"
#endif
#include "server.h"
#include "chmap.h"

#ifdef VMS
# include "vms.h"
#endif
/*}}}*/

/*{{{ Forward Function Declarations */

static int unsetkey_fun (int, SLcmd_Cmd_Table_Type *);
static int setkey_fun (int, SLcmd_Cmd_Table_Type *);
static int server_fun (int, SLcmd_Cmd_Table_Type *);
static int color_fun (int, SLcmd_Cmd_Table_Type *);
static int mono_fun (int, SLcmd_Cmd_Table_Type *);
static int user_data_fun (int, SLcmd_Cmd_Table_Type *);
static int ignore_quote_fun (int, SLcmd_Cmd_Table_Type *);
static int autobaud_fun (int, SLcmd_Cmd_Table_Type *);
static int set_variable_fun (int, SLcmd_Cmd_Table_Type *);
static int nnrp_fun (int, SLcmd_Cmd_Table_Type *);
static int grouplens_fun (int, SLcmd_Cmd_Table_Type *);
static int interpret_fun (int, SLcmd_Cmd_Table_Type *);

/*}}}*/
/*{{{ Static Global Variables */

static int This_Line_Num;	       /* current line number in startup file */
static char *This_File;
static char *This_Line;		       /* line being parsed */

static SLcmd_Cmd_Table_Type Slrn_Cmd_Table;
static SLcmd_Cmd_Type Slrn_Startup_File_Cmds[] = /*{{{*/
{
     {unsetkey_fun, "unsetkey", "SS"},
     {setkey_fun, "setkey", "SSS"},
     {server_fun, "server", "SS"},
     {color_fun, "color", "SSS"},
     {mono_fun, "mono", "SSsss"},
     {set_variable_fun, "set", "SG"},
     {user_data_fun, "username", "S"},
     {user_data_fun, "hostname", "S"},
     {nnrp_fun, "nnrpaccess", "SSS" },
#define SLRN_MAX_QUOTE_REGEXP 5
     {ignore_quote_fun, "ignore_quotes", "Sssss"},
     {autobaud_fun, "autobaud", ""},
     {grouplens_fun, "grouplens_add", "S"},
     {interpret_fun, "interpret", "S"},
     {user_data_fun, "scorefile", "S"},
   
   /* The following are considered obsolete */
     {user_data_fun, "replyto", "S"},
     {user_data_fun, "organization", "S"},
     {user_data_fun, "signature", "S"},
     {user_data_fun, "realname", "S"},
     {user_data_fun, "followup", "S"},
     {user_data_fun, "cc_followup_string", "S"},
     {user_data_fun, "quote_string", "S"},
#if SLRN_HAS_DECODE
     {user_data_fun, "decode_directory", "S"},
#endif
     {user_data_fun, "editor_command", "S"},
     {NULL, "", ""}
};

/*}}}*/

/*}}}*/
/*{{{ Public Global Variables */

SLRegexp_Type *Slrn_Ignore_Quote_Regexp [SLRN_MAX_QUOTE_REGEXP + 1];
int Slrn_Autobaud = 0;
char *Slrn_Score_File;
int Slrn_Scroll_By_Page;

/*}}}*/

/*{{{ Utility Functions */

static void exit_malloc_error (void) /*{{{*/
{
   if (This_File == NULL)
     slrn_exit_error ("Memory Allocation Failure");
   
   slrn_exit_error ("%s: Line %d\n%sMemory Allocation Failure",
		    This_File, This_Line_Num, This_Line);
}

/*}}}*/

static char *safe_malloc (unsigned int n) /*{{{*/
{
   char *s;
   s = (char *) SLMALLOC (n);
   if (s == NULL) exit_malloc_error ();
   return s;
}

/*}}}*/


static void exit_unknown_object (void) /*{{{*/
{
   slrn_exit_error ("%s: Error encountered processing line %d\n%s",
		    This_File, This_Line_Num, This_Line);
}

/*}}}*/

static void issue_obsolete_message (void) /*{{{*/
{
   slrn_message ("%s: Command is obsolete on line %d:\n%s",
		 This_File, This_Line_Num, This_Line);
   slrn_message ("The new usage is:\nset %s\n", This_Line);
}

/*}}}*/

   
/*}}}*/
/*{{{ Set/Unset Key Functions */

static int setkey_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   char *map = table->string_args[1];
   char *fun = table->string_args[2];
   char *key = table->string_args[3];
   SLKeyMap_List_Type *kmap = NULL;
   
   (void) argc;

   if (!strcmp (map, "group")) kmap = Slrn_Group_Keymap;
   else if (!strcmp (map, "article")) kmap = Slrn_Article_Keymap;
   else if (!strcmp (map, "readline")) kmap = Slrn_RLine_Keymap;
   else slrn_exit_error ("%s: line %d:\n%sNo such keymap: %s", This_File, This_Line_Num, This_Line, map);
   
   if (SLang_define_key (key, fun, kmap) != 0)
     {
	slrn_exit_error ("%s: line %d:\n%serror defining key.", This_File, This_Line_Num, This_Line);
     }
   return 0;
}

/*}}}*/

static int unsetkey_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   char *map = table->string_args[1];
   char *key = table->string_args[2];
   SLKeyMap_List_Type *kmap = NULL;
   
   (void) argc;
   
   if (!strcmp (map, "group")) kmap = Slrn_Group_Keymap;
   else if (!strcmp (map, "article")) kmap = Slrn_Article_Keymap;
   else slrn_exit_error ("%s: line %d:\n%sNo such keymap: %s",
			 This_File, This_Line_Num, This_Line, map);
   
   SLang_undefine_key (key, kmap);
   return 0;
}

/*}}}*/

/*}}}*/

static int autobaud_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   (void) argc; (void) table;
   Slrn_Autobaud = 1;
   return 0;
}

/*}}}*/

static SLRegexp_Type *compile_quote_regexp (char *str) /*{{{*/
{
   unsigned char *compiled_pattern_buf;
   SLRegexp_Type *r;
   
   compiled_pattern_buf = (unsigned char *) safe_malloc (512);
   r = (SLRegexp_Type *) safe_malloc (sizeof (SLRegexp_Type));
   
   r->pat = (unsigned char *) str;
   r->buf = compiled_pattern_buf;
   r->case_sensitive = 1;
   r->buf_len = 512;
   
   if (SLang_regexp_compile (r))
     {
	slrn_exit_error ("%s: line %d:\n%sInvalid regular expression.",
			 This_File, This_Line_Num, This_Line);
     }
   
   return r;
}

/*}}}*/
static int ignore_quote_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   unsigned int i;
   SLRegexp_Type *r;
   
   if (argc > SLRN_MAX_QUOTE_REGEXP + 1)
     {
	slrn_exit_error ("%s: line %d:\n%sToo many expressions specified.",
			 This_File, This_Line_Num, This_Line);
     }
   
   for (i = 0; i < SLRN_MAX_QUOTE_REGEXP; i++)
     {
	r = Slrn_Ignore_Quote_Regexp[i];
	if (r != NULL)
	  {
	     slrn_free ((char *) r->buf);
	     SLFREE (r);
	     Slrn_Ignore_Quote_Regexp [i] = NULL;
	  }
     }
	
   for (i = 1; i < (unsigned int) argc; i++)
     {
	Slrn_Ignore_Quote_Regexp[i - 1] 
	  = compile_quote_regexp (table->string_args[i]);
     }
   return 0;
}

/*}}}*/

static int grouplens_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   (void) argc;
#if SLRN_HAS_GROUPLENS
   (void) slrn_grouplens_add_group (table->string_args[1]);
#else 
   (void) table;
#endif
   return 0;
}

/*}}}*/

/*{{{ Setting/Getting Variable Functions */

typedef struct /*{{{*/
{
   char *what;
   int *valuep;
}

/*}}}*/
Set_Int_Type;

static Set_Int_Type Int_Things_To_Set [] = /*{{{*/
{
     {"new_subject_breaks_threads",	&Slrn_New_Subject_Breaks_Threads},
     {"scroll_by_page", &Slrn_Scroll_By_Page},
     {"use_color", &SLtt_Use_Ansi_Colors},
     {"ignore_signature", &Slrn_Sig_Is_End_Of_Article},
     {"reject_long_lines", &Slrn_Reject_Long_Lines},
#if SLRN_HAS_NNTP_SUPPORT
     {"query_reconnect", &Slrn_Query_Reconnect},
#else
     {"query_reconnect", NULL},
#endif
#if SLRN_HAS_SORT_BY_SCORE
     {"display_score", &Slrn_Display_Score},
#else
     {"display_score", NULL},
#endif
     {"use_xgtitle", &Slrn_Use_Xgtitle},
     {"show_article", &Slrn_Startup_With_Article},
     {"author_display", &Slrn_Show_Author},
     {"display_author_realname", &Slrn_Show_Author_Realname},
     {"show_descriptions", &Slrn_Group_Display_Descriptions},
     {"no_backups", &Slrn_No_Backups},
     {"beep", &SLtt_Ignore_Beep},
     {"unsubscribe_new_groups", &Slrn_Unsubscribe_New_Groups},
     {"show_thread_subject", &Slrn_Show_Thread_Subject},
     {"mouse", &Slrn_Use_Mouse},
     {"query_next_group", &Slrn_Query_Next_Group},
     {"query_next_article", &Slrn_Query_Next_Article},
     {"confirm_actions", &Slrn_User_Wants_Confirmation},
     {"cc_followup", &Slrn_Auto_CC_To_Poster},
     {"use_tmpdir", &Slrn_Use_Tmpdir},
     {"sorting_method", &Slrn_Sorting_Mode},
     {"uncollapse_threads", &Slrn_Threads_Visible},
     {"read_active", &Slrn_List_Active_File},
     {"use_metamail", &Slrn_Use_Meta_Mail},
     {"group_dsc_start_column", &Slrn_Group_Description_Column},
     {"lines_per_update", &Slrn_Reads_Per_Update},
     {"min_high_score", &Slrn_High_Score_Min},
     {"max_low_score", &Slrn_Low_Score_Max},
     {"kill_score", &Slrn_Kill_Score_Max},
#ifndef __os2__
     {"use_blink", &SLtt_Blink_Mode},
#endif
     {"wrap_flags", &Slrn_Wrap_Mode},
     {"write_newsrc_flags", &Slrn_Write_Newsrc_Flags},
     {"query_read_group_cutoff", &Slrn_Query_Group_Cutoff},
     {"prompt_next_group", &Slrn_Prompt_Next_Group},
     {"use_header_numbers", &Slrn_Use_Header_Numbers},
#if SLRN_HAS_SPOILERS
     {"spoiler_char", &Slrn_Spoiler_Char},
     {"spoiler_display_mode", &Slrn_Spoiler_Display_Mode},
#else
     {"spoiler_start_new_page", NULL},
     {"spoiler_char", NULL},
#endif
#if SLRN_HAS_MIME
     {"use_mime", &Slrn_Use_Mime},
#else
     {"use_mime", NULL},
#endif
#if SLRN_HAS_GROUPLENS
     {"use_grouplens", &Slrn_Use_Group_Lens},
     {"grouplens_port", &Slrn_GroupLens_Port},
#else
     {"use_grouplens", NULL},
#endif
#if 0
#if SLRN_HAS_INEWS_SUPPORT
     {"use_inews", &Slrn_Use_Inews},
#else
     {"use_inews", NULL},
#endif
#endif
#if SLRN_HAS_PULL_SUPPORT
     {"use_slrnpull", &Slrn_Use_Pull_Post},
#else
     {"use_slrnpull", NULL},
#endif
#if SLRN_HAS_TILDE_FEATURE
     {"use_tilde", &Slrn_Use_Tildes},
#else
     {"use_tilde", NULL},
#endif
     {NULL, NULL}
};

/*}}}*/

typedef struct /*{{{*/
{
   char *what;
   char **svaluep;
}

/*}}}*/
Set_String_Type;

static Set_String_Type String_Things_To_Set [] = /*{{{*/
{
     {"art_help_line", &Slrn_Art_Help_Line},
     {"header_help_line", &Slrn_Header_Help_Line},
     {"group_help_line", &Slrn_Group_Help_Line},
     {"quote_string", &Slrn_Quote_String},
     {"realname", &Slrn_User_Info.realname},
     {"replyto", &Slrn_User_Info.replyto},
     {"organization", &Slrn_User_Info.org},
     {"followup", &Slrn_User_Info.followup_string},
     {"cc_followup_string", &Slrn_Courtesy_CC_Message},
#if SLRN_HAS_DECODE
     {"decode_directory", &Slrn_Decode_Directory},
#endif
     {"editor_command", &Slrn_Editor},
     {"post_editor_command", &Slrn_Editor_Post},
     {"score_editor_command", &Slrn_Editor_Score},
     {"mail_editor_command", &Slrn_Editor_Mail},
     {"non_Xbrowser", &Slrn_NonX_Browser},
     {"Xbrowser", &Slrn_X_Browser},
     {"save_posts", &Slrn_Save_Posts_File},
     {"save_directory", &Slrn_Save_Directory},
     {"signature", &Slrn_User_Info.signature},
     {"custom_headers", &Slrn_Post_Custom_Headers},
     {"followup_custom_headers", &Slrn_Followup_Custom_Headers},
     {"reply_custom_headers", &Slrn_Reply_Custom_Headers},
#if SLRN_HAS_GROUPLENS
     {"grouplens_pseudoname", &Slrn_GroupLens_Pseudoname},
     {"grouplens_host", &Slrn_GroupLens_Host},
#else
     {"grouplens_pseudoname", NULL},
     {"grouplens_host", NULL},
#endif
     {"decode_directory", 
#if SLRN_HAS_DECODE
	  &Slrn_Decode_Directory
#else
	  NULL
#endif
     },
   
     {"inews_program",
#if SLRN_HAS_INEWS_SUPPORT && SLRN_HAS_USER_INEWS
	  &Slrn_Inews_Pgm
#else
	  NULL
#endif
     },
     
#if SLRN_HAS_MIME
     {"mime_charset", &Slrn_Mime_Display_Charset},
     {"metamail_command", &Slrn_MetaMail_Cmd},
#else
     {"mime_charset", NULL},
     {"metamail_command", NULL},
#endif
#if SLRN_HAS_CHARACTER_MAP
     {"charset", &Slrn_Charset},
#else
     {"charset", NULL},
#endif
#ifndef VMS
     {"sendmail_command", &Slrn_SendMail_Command},
#endif
     {"spool_inn_root", 
#if SLRN_HAS_SPOOL_SUPPORT
     &Slrn_Inn_Root
#else
     NULL
#endif
     },
     {"spool_root",
#if SLRN_HAS_SPOOL_SUPPORT
     &Slrn_Spool_Root
#else
     NULL
#endif
     },
     {"spool_nov_root",
#if SLRN_HAS_SPOOL_SUPPORT
     &Slrn_Nov_Root
#else
     NULL
#endif
     },
     {"spool_nov_file",
#if SLRN_HAS_SPOOL_SUPPORT
     &Slrn_Nov_File
#else
     NULL
#endif
     },
     {"spool_active_file",
#if SLRN_HAS_SPOOL_SUPPORT
     &Slrn_Active_File
#else
     NULL
#endif
     },
     {"spool_activetimes_file",
#if SLRN_HAS_SPOOL_SUPPORT
     &Slrn_ActiveTimes_File
#else
     NULL
#endif
     },
     {"spool_newsgroups_file",
#if SLRN_HAS_SPOOL_SUPPORT
     &Slrn_Newsgroups_File
#else
     NULL
#endif
     },
     {"macro_directory",
#if SLRN_HAS_SLANG
	&Slrn_Macro_Dir
#else
	  NULL
#endif
     },

     {NULL, NULL}
};

/*}}}*/

int slrn_set_string_variable (char *name, char *value) /*{{{*/
{
   Set_String_Type *sp = String_Things_To_Set;
	
   while (sp->what != NULL)
     {
	if (!strcmp (sp->what, name))
	  {
	     char *ss;
		  
	     if (sp->svaluep == NULL) return 0;
	     
	     ss = *sp->svaluep;
		  
	     slrn_free (ss);
	     if (NULL == (ss = SLmake_string (value)))
	       exit_malloc_error ();
		  
	     *sp->svaluep = ss;
	     return 0;
	  }
	sp++;
     }
   return -1;
}

/*}}}*/

int slrn_set_integer_variable (char *name, int value) /*{{{*/
{
   Set_Int_Type *ip = Int_Things_To_Set;
   
   while (ip->what != NULL)
     {
	if (!strcmp (ip->what, name))
	  {
	     if (ip->valuep == NULL) return 0;
	     *ip->valuep = value;
	     return 0;
	  }
	ip++;
     }
   return -1;
}

/*}}}*/

int slrn_get_variable_value (char *name, int *type, char ***sval, int **ival) /*{{{*/
{
   Set_String_Type *sp;
   Set_Int_Type *ip;
   
   sp = String_Things_To_Set;
   while (sp->what != NULL)
     {
	if (!strcmp (sp->what, name))
	  {
	     *sval = sp->svaluep;
	     *type = STRING_TYPE;
	     return 0;
	  }
	sp++;
     }
   
   ip = Int_Things_To_Set;
   while (ip->what != NULL)
     {
	if (!strcmp (ip->what, name))
	  {
	     *ival = ip->valuep;
	     *type = INT_TYPE;
	     return 0;
	  }
	ip++;
     }
   
   return -1;
}

/*}}}*/

static int set_variable_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   int ret;
   char *what = table->string_args[1];
   int ivalue = table->int_args[2];
   char *svalue = table->string_args[2];
   int type = table->arg_type[2];
   
   (void) argc;
   
   if (type == STRING_TYPE)
     ret = slrn_set_string_variable (what, svalue);
   else if (type == INT_TYPE) 
     ret = slrn_set_integer_variable (what, ivalue);
   else ret = -1;
   
   if (ret != 0) exit_unknown_object ();
   return 0;
}

/*}}}*/


/*}}}*/

/*{{{ Setting Color/Mono Attributes */

typedef struct /*{{{*/
{
   char *name;
   int value;
   char *fg, *bg;
   SLtt_Char_Type mono;
}

/*}}}*/
Color_Handle_Type;

/* default colors -- suitable for a color xterm */
   
static Color_Handle_Type Color_Handles[] = /*{{{*/
{
     {"normal",		0,		"black",	"white", 0},
     {"status",		STATUS_COLOR,	"yellow",	"blue", SLTT_REV_MASK},
     {"menu",		MENU_COLOR,	"yellow",	"blue", SLTT_REV_MASK},
     {"menu_press",	MENU_PRESS_COLOR,"blue",	"yellow", 0},
     {"headers",	HEADER_COLOR,	"brightcyan",	"white", 0},
     {"group",		GROUP_COLOR,	"blue",		"white", 0},
     {"subject",	SUBJECT_COLOR,	"black",	"white", 0},
     {"author",		AUTHOR_COLOR,	"magenta",	"white", 0},
     {"error",		ERROR_COLOR,	"red",		"white", SLTT_BLINK_MASK},
     {"cursor",		CURSOR_COLOR,	"brightgreen",	"white", SLTT_REV_MASK},
     {"article",	ARTICLE_COLOR,	"blue",		"white", 0},
     {"tree",		TREE_COLOR,	"red",		"white", 0},
     {"quotes",		QUOTE_COLOR,	"red",		"white", 0},
     {"signature",	SIGNATURE_COLOR,"red",		"white", 0},
     {"thread_number",	THREAD_NUM_COLOR,"blue",	"white", SLTT_BOLD_MASK},
     {"header_number",	HEADER_NUMBER_COLOR,"green",	"white", 0},
     {"high_score",	HIGH_SCORE_COLOR,"red",		"white", SLTT_BOLD_MASK},
     {"description",	GROUP_DESCR_COLOR,"magenta",	"white", 0},
     {"grouplens_display",GROUPLENS_DISPLAY_COLOR,"blue","white", 0},
     {"tilde",		SLRN_TILDE_COLOR,"green",	"white", SLTT_BOLD_MASK},
     {"header_name",SLRN_HEADER_KEYWORD_COLOR,"green", "white", SLTT_BOLD_MASK},
     {NULL, -1, NULL, NULL, 0}
};

/*}}}*/

static int color_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   char *what = table->string_args[1];
   char *fg = table->string_args[2];
   char *bg = table->string_args[3];
   Color_Handle_Type *ct = Color_Handles;
   
   (void) argc;   
   
   while (ct->name != NULL)
     {
	if (!strcmp (ct->name, what))
	  {
	     SLtt_set_color (ct->value, what, fg, bg);
	     return 0;
	  }
	ct++;
     }
   exit_unknown_object ();
   return 0;
}

/*}}}*/

#if SLANG_VERSION < 9932
# ifdef __os2__
void SLtt_set_mono (int a, char *b, SLtt_Char_Type c) /*{{{*/
{
   (void) a;
   (void) b;
   (void) c;
}

/*}}}*/
# endif
#endif

static int mono_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   char *what = table->string_args[1];
   char *attr;
   int i;
   
   Color_Handle_Type *ct = Color_Handles;
   
   while (ct->name != NULL)
     {
	if (!strcmp (ct->name, what))
	  {
	     SLtt_Char_Type mono_attr = 0;
	     for (i = 2; i < argc; i++)
	       {
		  attr = table->string_args[i];
		  if (!strcmp (attr, "bold")) mono_attr |= SLTT_BOLD_MASK;
		  else if (!strcmp (attr, "blink")) mono_attr |= SLTT_BLINK_MASK;
		  else if (!strcmp (attr, "underline")) mono_attr |= SLTT_ULINE_MASK;
		  else if (!strcmp (attr, "reverse")) mono_attr |= SLTT_REV_MASK;
		  else if (!strcmp (attr, "none")) mono_attr = 0;
		  else exit_unknown_object ();
	       }
	     SLtt_set_mono (ct->value, NULL, mono_attr);
	     return 0;
	  }
	ct++;
     }
   exit_unknown_object ();
   return 0;
}

/*}}}*/

/*}}}*/

/*{{{ User Info Data (Hostname, etc) */

/*----------------------------------------------------------------------*\
* static int user_data_fun ();
 *
 * convenient mechanism to set Slrn_User_Info fields without adding
 * extra environment variables
 *
 * recognized fields
 *
 *   replyto		- alternative name for replies
 *   organization	- use double quotes if there are spaces!
 *   signature		- an alternate to ~/.signature (for news posting)
 *   hostname		- full name of the current host
\*----------------------------------------------------------------------*/

typedef struct /*{{{*/
{
   char *name;
   char **addr;
   unsigned int size;
}

/*}}}*/
User_Info_Variable_Type;

static User_Info_Variable_Type User_Info_Variables[] = /*{{{*/
{
     {"username", &Slrn_User_Info.username, 0},
     {"hostname", (char **)Slrn_User_Info.host, MAX_HOST_NAME_LEN},
     {"scorefile", &Slrn_Score_File, 0},
   
#define USER_INFO_OBSOLETE_STARTS_HERE 3
   /* The following are obsolete and will not be supported in the future. */
     {"realname", &Slrn_User_Info.realname, 0},
     {"replyto", &Slrn_User_Info.replyto, 0},
     {"organization", &Slrn_User_Info.org, 0},
     {"followup", &Slrn_User_Info.followup_string, 0},
     {"signature", &Slrn_User_Info.signature, 0},
     {"cc_followup_string", &Slrn_Courtesy_CC_Message, 0},
#if SLRN_HAS_DECODE
     {"decode_directory", &Slrn_Decode_Directory, 0},
#endif
     {"editor_command", &Slrn_Editor, 0},
     {"quote_string", NULL, 0},
     {NULL, NULL, 0}
};

/*}}}*/

static int user_data_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   char *what = table->string_args[0];
   char *field = table->string_args[1];
   User_Info_Variable_Type *u = User_Info_Variables;
   char **ptr, *contents;
   unsigned int n;
   
   (void) argc;
   
   while (u->name != NULL)
     {
	if (!strcmp (u->name, what))
	  {
	     n = strlen (field);
	     
	     if (u->size)
	       {
		  contents = (char *) u->addr;
		  strncpy (contents, field, u->size);
		  contents [u->size - 1] = 0;
	       }
	     else if (NULL != (ptr = u->addr))
	       {
		  contents = safe_malloc (n + 1);
		  strcpy (contents, field);
		  
		  slrn_free (*ptr);
		  *ptr = contents;
	       }

	     if (u >= User_Info_Variables + USER_INFO_OBSOLETE_STARTS_HERE)
	       issue_obsolete_message ();
	     
	     return 0;
	  }
	u++;
     }
   exit_unknown_object ();
   return -1;
}

/*}}}*/

/*}}}*/

/*{{{ Server Newsrc Mapping Functions */

typedef struct Server_List_Type /*{{{*/
{
   struct Server_List_Type *next;
   char *file;
   char *host;
   char *username;
   char *password;
}

/*}}}*/
Server_List_Type;

static Server_List_Type *Server_List;

static Server_List_Type *find_server (char *host) /*{{{*/
{
   Server_List_Type *s = Server_List;
   
   while (s != NULL)
     {
	if (0 == slrn_case_strcmp ((unsigned char *)host, 
				   (unsigned char *) s->host))
	  break;
	s = s->next;
     }
   return s;
}

/*}}}*/

static int add_to_server_list (char *host, char *file, char *username, char *password) /*{{{*/
{
   Server_List_Type *s;
   
   if (NULL == (s = find_server (host)))
     {
	s = (Server_List_Type *) safe_malloc (sizeof (Server_List_Type));
	memset ((char *) s, 0, sizeof (Server_List_Type));
	s->next = Server_List;
	Server_List = s;
	s->host = slrn_safe_strmalloc (host);
     }
   
   if (file != NULL)
     {
	slrn_free (s->file);
	s->file = slrn_safe_strmalloc (file);
     }

   if (username != NULL)
     {
	slrn_free (s->username);
	s->username = slrn_safe_strmalloc (username);
     }

   if (password != NULL)
     {
	slrn_free (s->password);
	s->password = slrn_safe_strmalloc (password);
     }
   
   return 0;
}

/*}}}*/

static int server_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
   char *the_file = table->string_args[2], *the_host = table->string_args[1];
      
   (void) argc;
   
   return add_to_server_list (the_host, the_file, NULL, NULL);
}

/*}}}*/

char *slrn_map_file_to_host (char *host) /*{{{*/
{
   Server_List_Type *s;
   
   if (NULL == (s = find_server (host)))
     return NULL;
   
   return s->file;
}

/*}}}*/

/*}}}*/

/*{{{ Server Authorization Information */

/*----------------------------------------------------------------------*\
* static int nnrp_fun ();
 *
 * convenient mechanism to set nnrp Slrn_User_Info fields without adding
 * extra environment variables
 *
 * recognized fields
 *
 *   nnrpaccess         - used to log in to a server using authinfo
 *                        it has the following format.
 *                         "host  username  password"
\*----------------------------------------------------------------------*/

static int nnrp_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
#if SLRN_HAS_NNTP_SUPPORT
   char *server = table->string_args[1];
   char *name = table->string_args[2];
   char *pass = table->string_args[3];

   (void) argc;
   return add_to_server_list (server, NULL, name, pass);
#else
   (void) argc;
   (void) table;
   return 0;
#endif   
}
/*}}}*/

int slrn_get_authorization (char *host, char **name, char **pass) /*{{{*/
{
   Server_List_Type *s;
   char buf[256];
   
   *name = NULL;
   *pass = NULL;
   
   s = find_server (host);

   if (s == NULL)
     return 0;
   
   if ((s->username == NULL) || (s->password == NULL))
     return 0;
   
   if (*s->username == 0)
     {
	*buf = 0;
	if (-1 == slrn_read_input ("Username", NULL, buf, 1))
	  return -1;
	SLFREE (s->username);
	s->username = slrn_safe_strmalloc (buf);
     }

   if (*s->password == 0)
     {
	*buf = 0;
	if (-1 == slrn_read_input_no_echo ("Password", NULL, buf, 1))
	  return -1;
	SLFREE (s->password);
	s->password = slrn_safe_strmalloc (buf);
     }

   *name = s->username;
   *pass = s->password;

   return 0;
}

/*}}}*/

/*}}}*/

static void slrn_init_modes (void) /*{{{*/
{
   slrn_init_group_mode ();
   slrn_init_article_mode ();
}

/*}}}*/

void slrn_startup_initialize (void) /*{{{*/
{
   Color_Handle_Type *h;
   char *server;
   
   slrn_init_modes ();
   SLang_init_case_tables ();
   
   Slrn_Ignore_Quote_Regexp[0] = compile_quote_regexp ("^ ?[:>]");
   
   h = Color_Handles;
   while (h->name != NULL)
     {
	SLtt_set_color (h->value, NULL, h->fg, h->bg);
	SLtt_set_mono (h->value, NULL, h->mono);
	h++;
     }

#ifndef __os2__
   /* We are not using the blink characters (unless in mono) */
   SLtt_Blink_Mode = 0;
#endif
   
   switch (Slrn_Server_Id)
     {
      case SLRN_SERVER_ID_NNTP:
	server = "NNTP";
	break;
	
      case SLRN_SERVER_ID_SPOOL:
	server = "SPOOL";
	break;
	
      default:
	server = NULL;
     }
   
   if ((server != NULL) 
       && (0 == SLdefine_for_ifdef (server)))
     {
	slrn_exit_error ("Unable to add preprocessor name %s.", server);
     }
}


/*}}}*/

void slrn_read_startup_file (char *name) /*{{{*/
{
   FILE *fp;
   char file [SLRN_MAX_PATH_LEN];
   char line [256];
   SLPreprocess_Type pt;

   if (-1 == slrn_init_readline ())
     {
	slrn_exit_error ("Unable to initialize S-Lang readline library.");
     }
   

   if (-1 == SLprep_open_prep (&pt))
     {
	slrn_exit_error ("Error initializing S-Lang preprocessor.");
     }
   
   
   fp = slrn_open_home_file (name, "r", file, 0);
   if (fp == NULL) return;
   
   slrn_message ("Reading startup file %s.", file);

   This_File = file;
   This_Line = line;
   
   Slrn_Cmd_Table.table = Slrn_Startup_File_Cmds;
   
   This_Line_Num = 0;
   while (NULL != fgets (line, sizeof(line) - 1, fp))
     {
	This_Line_Num++;
	if (SLprep_line_ok (line, &pt))
	  (void) SLcmd_execute_string (line, &Slrn_Cmd_Table);
	
	if (SLang_Error) exit_unknown_object ();
     }
   slrn_fclose (fp);
   
   SLprep_close_prep (&pt);
}

/*}}}*/

static int interpret_fun (int argc, SLcmd_Cmd_Table_Type *table) /*{{{*/
{
#if SLRN_HAS_SLANG
   char *file = table->string_args [1];
   
   (void) argc;
   if (Slrn_Use_Slang == 0) return 0;
   return slrn_eval_slang_file (file);
#else
   (void) argc;
   return 0;
#endif
}

/*}}}*/

