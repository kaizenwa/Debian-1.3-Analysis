/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"
/*{{{ Include Files */

#include <stdio.h>
#include <string.h>

#include <slang.h>

#include "jdmacros.h"

#include "buffer.h"
#include "replace.h"
#include "search.h"
#include "screen.h"
#include "ins.h"
#include "ledit.h"
#include "misc.h"
#include "paste.h"
#include "cmds.h"

/*}}}*/

int Replace_Preserve_Case = 0;

/* returns the length of the region inserted. */
int replace_chars (int *np, char *neew) /*{{{*/
{
   int len, n;
   int preserve_case;
   char *old = NULL;
   int i;
   
   CHECK_READ_ONLY
     
   if ((n = *np) < 0) return 0;
   len = strlen (neew);
   
   preserve_case = (Replace_Preserve_Case && (len == n));
   push_mark (); 
   if (preserve_case) push_mark ();
   n = forwchars (np);
   if (preserve_case)
     {
	if (n == len)
	  {
	     if (NULL == (old = make_buffer_substring(&n))) return 0;
	  }
	else 
	  {
	     preserve_case = 0;
	     pop_mark (&Number_Zero);
	  }
     }
   
   delete_region ();
   
   if (preserve_case)
     {
	unsigned char ch;
	
	for (i = 0; i < len; i++)
	  {
	     ch = (unsigned char) old[i];
	     if (ch == UPPER_CASE(ch))
	       {
		  if (ch == LOWER_CASE(ch))
		    old[i] = neew[i];
		  else
		    old[i] = UPPER_CASE(neew[i]);
	       }
	     else old[i] = LOWER_CASE(neew[i]);
	  }
	neew = old;
     }
   
   ins_chars((unsigned char *) neew, len);
   
   if (preserve_case) SLFREE (old);
   return len;
}

/*}}}*/

int replace_next(char *old, char *neew) /*{{{*/
{
   int n;
   
   if (search(old, 1, 0) == 0) return(0);
   n = strlen (old);
   (void) replace_chars (&n, neew);
   return(1);
}

/*}}}*/

/* This code implements a kill ring of sorts. It could be done in S-Lang but
 * S-Lang cannot handle strings with embedded NULL characters.  I do not
 * want to lose compatability with C or I would allow S-Lang strings to handle
 * the NULL chars.
 */

#ifndef Sixteen_Bit_System

# define MAX_KILL_ARRAY_SIZE 16
int Kill_Array_Size = MAX_KILL_ARRAY_SIZE;
  
typedef struct /*{{{*/
{
   unsigned char *buf;
   int len;
}

/*}}}*/
Char_Array_Type;

Char_Array_Type Kill_Array [MAX_KILL_ARRAY_SIZE];

void copy_region_to_kill_array (int *np) /*{{{*/
{
   int n = *np;
   unsigned char *buf;
   
   if (n < 0) n = 0;
   n = n % MAX_KILL_ARRAY_SIZE;
   
   if ((buf = Kill_Array[n].buf) != NULL) SLFREE (buf);   
   Kill_Array[n].buf = (unsigned char *) make_buffer_substring (&Kill_Array[n].len);
}

/*}}}*/

void insert_from_kill_array (int *np) /*{{{*/
{
   int n = *np;
   unsigned char *buf;

   CHECK_READ_ONLY_VOID
   if (n < 0) n = 0;
   n = n % MAX_KILL_ARRAY_SIZE;
   
   if ((buf = Kill_Array[n].buf) == NULL) return;
   ins_chars (buf, Kill_Array[n].len);
}

/*}}}*/

void append_region_to_kill_array (int *np) /*{{{*/
{
   int n = *np, len, oldlen;
   unsigned char *buf, *newbuf;
   
   if (n < 0) n = 0;
   n = n % MAX_KILL_ARRAY_SIZE;

   buf = (unsigned char *) make_buffer_substring (&len);
   if (buf == NULL) return;
   
   oldlen = Kill_Array[n].len;
   newbuf = (unsigned char *) SLREALLOC (Kill_Array[n].buf, oldlen + len + 1);
   
   if (newbuf != NULL)
     {
	SLMEMCPY ((char *) newbuf + oldlen, (char *) buf, len);
	Kill_Array[n].buf = newbuf;
	Kill_Array[n].len = oldlen + len;
     }
   
   SLFREE (buf);
}

/*}}}*/

#endif  /* NOT 16bit system */

static int bol_bsearch_char (int *ch) /*{{{*/
{
   char buf[2];
   buf[0] = (unsigned char) (*ch); buf[1] = 0;
   return bol_bsearch (buf);
}

/*}}}*/

static int bol_fsearch_char (int *ch) /*{{{*/
{
   char buf[2];
   buf[0] = (unsigned char) (*ch); buf[1] = 0;
   return bol_fsearch (buf);
}

/*}}}*/

static int fsearch_char (int *ch) /*{{{*/
{
   char buf[2];
   buf[0] = (unsigned char) (*ch); buf[1] = 0;
   return search_forward (buf);
}

/*}}}*/

static int bsearch_char (int *ch) /*{{{*/
{
   char buf[2];
   buf[0] = (unsigned char) (*ch); buf[1] = 0;
   return search_backward (buf);
}

/*}}}*/

static int ffind_char (int *ch) /*{{{*/
{
   char buf[2];
   buf[0] = (unsigned char) (*ch); buf[1] = 0;
   return forward_search_line (buf);
}

/*}}}*/

static int bfind_char (int *ch) /*{{{*/
{
   char buf[2];
   buf[0] = (unsigned char) (*ch); buf[1] = 0;
   return backward_search_line (buf);
}

/*}}}*/

static void widen_this_buffer (void) /*{{{*/
{
   jed_widen_whole_buffer (CBuf);
}

/*}}}*/

static int is_visible_mark (void) /*{{{*/
{
   if ((CBuf->marks == NULL) || (0 == (CBuf->marks->flags & VISIBLE_MARK)))
     return 0;
   return 1;
}

/*}}}*/

SLang_Name_Type Jed_Other_Intrinsics [] = /*{{{*/
{
   MAKE_INTRINSIC(".fsearch", search_forward, INT_TYPE, 1),
   /* Prototype: Integer fsearch (String str);
    * This function may be used to search forward in buffer looking for the
    * string @str@.  If not found, this functions returns zero.  However,
    * if found, the length of the string is returned and the current point
    * is moved to the to the start of the match.  It respects the setting
    * of the variable @CASE_SEARCH@.  If the string that one is searching
    * for is known to be at the beginning of a line, the function
    * @bol_fsearch@ should be used instead. 
    *
    * Note: This function cannot find a match that crosses lines. 
    * Related Functions: @ffind@, @fsearch_char@, @bsearch@, @bol_fsearch@, @re_fsearch@, @looking_at@ 
    * Related Variables: @CASE_SEARCH@
    */
   
   MAKE_INTRINSIC(".bsearch", search_backward, INT_TYPE, 1),
   /* Prototype: Integer bsearch (String str);
    * The @bsearch@ function searches backward from the current position
    * for the string @str@.  If @str@ is found, this function will return
    * the length of @str@ and move the current position to the beginning of
    * the matched text.  If a match is not found, zero will be returned and
    * the position will not change.  It respects the value of the variable
    * @CASE_SEARCH@.
    * 
    * Related Functions: @fsearch@, @bol_bsearch@, @re_bsearch@ 
    */
   MAKE_INTRINSIC(".bfind", backward_search_line, INT_TYPE, 1),
   /* Prototype: Integer bfind (String str);
    * @bfind@ searches backward from the current position to the beginning
    * of the line for the string @str@.  If a match is found, the length of
    * @str@ is returned and the current point is moved to the start of the
    * match. If no match is found, zero is returned. 
    * Note: This function respects the setting of the @CASE_SEARCH@ variable.
    * Related Functions: @bsearch@, @ffind@, @bol_bsearch@, @re_bsearch@
    * Related Variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".ffind", forward_search_line, INT_TYPE, 1),
   /* Prototype: Integer ffind (String s);
    * @ffind@ searches forward from the current position to the end of the
    * line for the string @str@.  If a match is found, the length of @str@
    * is returned and the current point is moved to the start of the match.
    * If no match is found, zero is returned.
    * Note: This function respects the setting of the @CASE_SEARCH@ variable.
    * To perform a search that includes multiple lines, use the @fsearch@
    * function.
    * Related Functions: @fsearch@, @bfind@, @re_fsearch@, @bol_fsearch@
    */
   MAKE_INTRINSIC(".bol_fsearch", bol_fsearch, INT_TYPE, 1),
   /* Prototype: Integer bol_fsearch (str);
    * @bol_fsearch@ searches forward from the current point until the end
    * of the buffer for occurrences of the string @str@ at the beginning of
    * a line.  If a match is found, the length of @str@ is returned and the
    * current point is moved to the start of the match.  If no match is
    * found, zero is returned.
    * Note: @bol_fsearch@ is much faster than using @re_fsearch@ to perform
    * a search that matches the beginning of a line.
    *
    * Related Functions: @bol_bsearch@, @fsearch@, @ffind@, @re_fsearch@
    * Related Variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".bol_bsearch", bol_bsearch, INT_TYPE, 1),
   /* Prototype: Integer bol_bsearch (str);
    * @bol_bsearch@ searches backward from the current point until the
    * beginning of the buffer for the occurrences of the string @str@ at
    * the beginning of a line.  If a match is found, the length of @str@ is
    * returned and the current point is moved to the start of the match. If
    * no match is found, zero is returned.
    *
    * Note: @bol_bsearch@ is much faster than using @re_bsearch@ to perform
    * a search that matches the beginning of a line.
    *
    * Related Functions: @bol_fsearch@, @bsearch@, @bfind@, @re_bsearch@
    * Related Variables: @CASE_SEARCH@
    */
   
   MAKE_INTRINSIC(".bol_fsearch_char", bol_fsearch_char, INT_TYPE, 1),
   /* Prototype: Integer bol_fsearch_char (Integer ch);
    * This function searches forward for a character @ch@ at the beginning 
    * of a line.  If it is found, @1@ is returned; otherwise @0@ is returned.
    * Related functions: @bol_fsearch@, @bol_bsearch_char@, @fsearch_char@
    * Related variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".bol_bsearch_char", bol_bsearch_char, INT_TYPE, 1),
   /* Prototype: Integer bol_fsearch_char (Integer ch);
    * This function searches backward for a character @ch@ at the beginning 
    * of a line.  If it is found, @1@ is returned; otherwise @0@ is returned.
    * Related functions: @bol_bsearch@, @bol_fsearch_char@, @bsearch_char@
    * Related variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".fsearch_char", fsearch_char, INT_TYPE, 1),
   /* Prototype: Integer fsearch_char (Integer ch);
    * This function searches forward for a character @ch@.  If it is
    * found, @1@ is returned; otherwise @0@ is returned. 
    * Related functions: @fsearch@, @ffind_char@, @bsearch_char@ 
    * Related variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".bsearch_char", bsearch_char, INT_TYPE, 1),
   /* Prototype: Integer fsearch_char (Integer ch);
    * This function searches backward for a character @ch@.  If it is
    * found, @1@ is returned; otherwise @0@ is returned. 
    * Related functions: @fsearch_char@, @ffind_char@, @fsearch@
    * Related variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".bfind_char", bfind_char, INT_TYPE, 1),
   /* Prototype: Integer fsearch_char (Integer ch);
    * This function searches backward on the current line for a character
    * @ch@.  If it is found, @1@ is returned; otherwise @0@ is returned. 
    * Related functions: @fsearch_char@, @ffind_char@, @fsearch@
    * Related variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".ffind_char", ffind_char, INT_TYPE, 1),
   /* Prototype: Integer fsearch_char (Integer ch);
    * This function searches forwardward on the current line for a character
    * @ch@.  If it is found, @1@ is returned; otherwise @0@ is returned. 
    * Related functions: @fsearch_char@, @bfind_char@, @fsearch@
    * Related variables: @CASE_SEARCH@
    */
   MAKE_INTRINSIC(".replace", replace_cmd, VOID_TYPE, 2),
   /* Prototype: Void replace(String old, String new);
    * This function may be used to replace all occurances of the string
    * @old@ with the string, @new@, from current editing point to the end
    * of the buffer. The editing point is returned to the initial location.
    * That is, this function does not move the editing point.
    * Related Functions: @replace_chars@, @fsearch@, @re_fsearch@, @bsearch@, @ffind@, @del@
    * Related Variables: @REPLACE_PRESERVE_CASE@
    */
   MAKE_INTRINSIC(".replace_chars", replace_chars, INT_TYPE, 2),
   /* Prototype: Void replace_chars (Integer n, String new);
    * This function may be used to replace the next @n@ characters at the
    * editing position by the string @new@.  After the replacement, the editing
    * point will be moved to the end of the inserted string.  The length of
    * the replacement string @new@ is returned.
    * Related Functions: @fsearch@, @re_fsearch@, @bsearch@, @ffind@, @del@
    * Related Variables: @REPLACE_PRESERVE_CASE@
    */
   MAKE_INTRINSIC(".regexp_nth_match", regexp_nth_match, VOID_TYPE, 1),
   /* Prototype: String regexp_nth_match (Integer n);
    * This function returns the nth sub-expression matched by the last regular
    * expression search.  If the parameter @n@ is zero, the entire match is 
    * returned.
    * Note: The value returned by this function is meaningful only if the
    * editing point has not been moved since the match.
    * Related Functions: @re_fsearch@, @re_bsearch@
    */
   MAKE_INTRINSIC(".replace_match", replace_match, INT_TYPE, 2),
   /* Prototype: Integer replace_match(String s, Integer how);
    * This function replaces text previously matched with @re_fsearch@ or
    * @re_bsearch@ at the current editing point with string @s@.  If @how@ is
    * zero, @s@ is a specially formatted string of the form described below.
    * If @how@ is non-zero, @s@ is regarded as a simple string and is used
    * literally.  If the replacement fails, this function returns zero
    * otherwise, it returns non-zero.
    */
   MAKE_INTRINSIC(".re_fsearch", re_search_forward, INT_TYPE, 1),
   /* Prototype: Integer re_fsearch(String pattern);
    * Search forward for regular expression @pattern@.  This function returns
    * the 1 + length of the string  matched.  If no match is found, it returns 
    * 0.
    * 
    * Related Functions: @fsearch@, @bol_fsearch@, @re_bsearch@ */
   MAKE_INTRINSIC(".re_bsearch", re_search_backward, INT_TYPE, 1),
   /* Prototype: Integer re_bsearch(String pattern);
    * Search backward for regular expression @pattern@.  This function returns
    * the 1 + length of the string  matched.  If no match is found, it returns 
    * 0.
    * 
    * Related Functions: @bsearch@, @bol_bsearch@, @re_fsearch@ */
   MAKE_INTRINSIC(".is_visible_mark", is_visible_mark, INT_TYPE, 0),
   /* Prototype: is_visible_mark ();
    * This function may be used to test whether or not the mark is a visible
    * mark.  A visible mar is one which causes the region defined by it to
    * be highlighted.
    * It returns @1@ is the mark is visible, or @0@ if the mark 
    * is not visible or does not exist.
    * 
    * Related Functions: @markp@, @push_mark@
    */
   MAKE_VARIABLE(".REPLACE_PRESERVE_CASE_INTERNAL", &Replace_Preserve_Case, INT_TYPE, 0),
   /* Prototype: Integer REPLACE_PRESERVE_CASE_INTERNAL = 0;
    * This variable affects the @replace@ function.  If the length of the
    * old string is the same as the new string and this variable is
    * non-zero, then then the case of the old string will be preserved by
    * the replace function.  For example, if @REPLACE_PRESERVE_CASE_INTERNAL@ 
    * is non-zero, and @"abCdE"@ is replaced by @"tuvwx"@, then @"tuVwX"@ will
    * actually be used.
    * Note: This variable's value automatically gets reset to zero after every
    * keystroke.
    */
   MAKE_VARIABLE(".CASE_SEARCH", &Case_Sensitive, INT_TYPE, 0),
    /* Prototype: Integer CASE_SEARCH = 0;
     * if 1, searches are case sensitive.  If 0, they are not 
     */
#if JED_HAS_SAVE_NARROW
   MAKE_INTRINSIC(".push_narrow", jed_push_narrow, VOID_TYPE, 0),
    /* Prototype: Void push_narrow ();
     * This function saves the current narrow context.  This is useful when
     * one wants to restore this context after widening the buffer.
     * Related Functions: @pop_narrow@, @narrow@, @widen@, @widen_buffer@
     */
   MAKE_INTRINSIC(".pop_narrow", jed_pop_narrow, VOID_TYPE, 0),
    /* Prototype: Void pop_narrow ();
     * The purpose of this function is to restore the last narrow
     * context that was saved via @push_narrow@.
     * Related Functions: @push_narrow@, @widen@, @widen_buffer@
     */
#endif
#if JED_HAS_BUFFER_LOCAL_VARS
   MAKE_INTRINSIC(".set_blocal_var", jed_set_blocal_var, VOID_TYPE, 0),
    /* Prototype: Void set_blocal_var (val, String v);
     * This function sets the value of the buffer local variable with name @v@
     * to value @val@.  The buffer local variable specified by @v@ must have 
     * been previously created by the @create_blocal_var@ function.  @val@ must
     * have the type that was declared when @create_blocal_var@ was called.
     * Related Functions: @get_blocal_var@, @create_blocal_var@
     */
   MAKE_INTRINSIC(".get_blocal_var", jed_get_blocal_var, VOID_TYPE, 1),
    /* Prototype: get_blocal_var (String name);
     * This function returns the value of the buffer local variable specified
     * by @name@.
     * Related Functions: @set_blocal_var@, @create_blocal_var@
     */
   MAKE_INTRINSIC(".create_blocal_var", jed_make_blocal_var, VOID_TYPE, 0),
    /* Prototype: Void create_blocal_var (String name, Integer type);
     * This function is used to create a buffer local variable named @name@
     * whose type is specified by @type@.  A buffer local variable is a 
     * variable whose value is local to the current buffer.  Currently only
     * integer and string types are supported.  An integer is specifed if the
     * parameter @type@ is @'i'@, and a string is specified if @type@ is @'s'@.
     * 
     * Related Functions: @get_blocal_var@, @set_blocal_var@
     */
#endif
   MAKE_INTRINSIC(".count_narrows", jed_count_narrows, INT_TYPE, 0),
     /* Prototype Integer count_narrows ();
      * This function returns the narrow depth of the current buffer.
      * Related Functions: @narrow@, @widen@, @widen_buffer@, @push_narrow@
      */
   MAKE_INTRINSIC(".widen_buffer", widen_this_buffer, VOID_TYPE, 0),
     /* Prototype: Void widen_buffer ();
      * This function widens the whole buffer.  If one intends to restore the
      * narrow context after calling this function, the narrow context should be
      * saved via @push_narrow@.
      * Related Functions: @narrow@, @widen@, @push_narrow@, @pop_narrow@
      */
   MAKE_VARIABLE(".USE_TABS", &Jed_Use_Tabs, INT_TYPE, 0),
   /* Prototype: Integer USE_TABS = 1;
    * If non-zero, the editor will try to use tab characters whenever possible
    * in creating whitespace.
    * Related Variables: @TAB@, @TAB_DEFAULT@
    */
     SLANG_END_TABLE
};

/*}}}*/

    
