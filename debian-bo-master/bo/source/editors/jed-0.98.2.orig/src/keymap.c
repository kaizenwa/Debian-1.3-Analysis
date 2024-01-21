/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"
/*{{{ Include Files */

#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"

#include <string.h>
#include <setjmp.h>

#include "buffer.h"
#include "cmds.h"
#include "paste.h"
#include "screen.h"
#include "window.h"
#include "text.h"
#include "ledit.h"
#include "keymap.h"
#include "sysdep.h"
#include "misc.h"
#include "display.h"
#include "file.h"
#include "undo.h"
#include "ins.h"
#include "hooks.h"
#include "replace.h"

/*}}}*/

void (*X_Define_Keys_Hook)(SLKeyMap_List_Type *);

typedef struct /*{{{*/
{
   jmp_buf b;
}

/*}}}*/
jmp_buf_struct;

jmp_buf_struct Jump_Buffer, *Jump_Buffer_Ptr;

FVOID_STAR Last_Key_Function;
int *Repeat_Factor;                    /* repeats a key sequence if non null */

char *Read_This_Character = NULL;      /* alternate keyboard buffer */

char Jed_Key_Buffer[13];
char Key_Buffer[13];
char *Key_Bufferp = Key_Buffer;
static int Last_Key_Buffer_Len;

int Jed_Max_Hits = 300;


int beep (void) /*{{{*/
{
   tt_beep ();
   flush_output ();
   return 1;
}

/*}}}*/

SLKeymap_Function_Type Jed_Functions[] = /*{{{*/
{
     {"backward_delete_char", backward_delete_char_cmd},
     {"backward_delete_char_untabify", backward_delete_char_untabify},
     {"beep", beep},
     {"begin_macro", begin_keyboard_macro},
     {"beg_of_buffer", bob},
     {"beg_of_line", bol},
     {"center_line", center_line},
     {"copy_region", copy_to_pastebuffer},
#if (defined(msdos) || defined(__GO32__)) && !defined(__WIN32__)
     {"coreleft", show_memory},
#endif
     {"delete_char_cmd", delete_char_cmd},
     {"delete_window", delete_window},
     {"digit_arg", digit_arg},
/*      {"double_line", double_line}, */
     {"end_macro", end_keyboard_macro},
     {"enlarge_window", enlarge_window},
     {"end_of_buffer", eob},
     {"eol_cmd", eol_cmd},
     {"evaluate_cmd", evaluate_cmd},
     {"execute_macro", execute_keyboard_macro},
     {"exchange",exchange_point_mark},
     {"exit_jed", exit_jed},
     {"exit_mini", exit_minibuffer},
     {"find_file", find_file},
     {"format_paragraph", text_format_paragraph},
     {"goto_match", goto_match},
     {"indent_line", indent_line},
     {"insert_file", insert_file_cmd},
#ifdef HAS_MOUSE
     {"mouse_cmd", jed_mouse_cmd},
#endif
     {"kbd_quit", kbd_quit},
     {"kill_buffer", kill_buffer},
     {"kill_line", kill_line},
     {"kill_region", kill_region},
     {"macro_query", macro_query},
     {"mark_spot", mark_spot},
     {"mini_complete", mini_complete},
     {"narrow", narrow_to_lines},
     {"narrow_paragraph", narrow_paragraph},
     {"narrow_to_region", narrow_to_region},
     {"newline", newline},
     {"newline", newline_cmd},
     {"newline_and_indent", newline_and_indent},
     {"next_char_cmd", next_char_cmd},
     {"next_line_cmd", next_line_cmd},
     {"one_window", one_window},
     {"other_window", other_window},
     {"page_down", pagedown_cmd},
     {"page_up", pageup_cmd},
     {"pop_spot", pop_spot},
     {"previous_char_cmd", previous_char_cmd},
     {"previous_line_cmd", previous_line_cmd},
     {"quoted_insert", quoted_insert},
     {"redraw", redraw},
      /* {"replace", replace}, */
     {"save_buffers", save_some_buffers},
     {"scroll_left", scroll_left},
     {"scroll_right", scroll_right},
       /* {"search_backward", search_backward_cmd},
	  {"search_forward", search_forward_cmd}, */
     {"self_insert_cmd", ins_char_cmd},
     {"set_mark_cmd", set_mark_cmd},
     {"split_window", split_window},
     {"switch_to_buffer", get_buffer},
     {"sys_spawn_cmd", sys_spawn_cmd},
     {"text_smart_quote", text_smart_quote},
/*       {"transpose_lines", transpose_lines}, */
     {"trim_whitespace", trim_whitespace},
     {"undo", undo},
     {"widen", widen},
     {"widen_region", widen_region},
     {"write_buffer", write_buffer},
     {"yank", yank},
     {(char *) NULL, NULL}
};

/*}}}*/

SLKeyMap_List_Type *Global_Map;
SLKeyMap_List_Type *Mini_Map;

int kbd_quit(void) /*{{{*/
{
   int sle = SLang_Error;
   SLKeyBoard_Quit = 1;
   
   /* --- never serious, testing only
   if (Repeat_Factor != NULL) 
     {
	msg_error("Bang!");
	longjmp(Jump_Buffer_Ptr->b, 1);
     } */
   
   msg_error("Quit!");
   if (Ignore_User_Abort) SLang_Error = sle;
#ifdef UNDO_HAS_REDO
   set_current_undo ();
#endif
   return(1);
}

/*}}}*/

void init_keymaps(void) /*{{{*/
{
   int ch;
   char simple[2];
   simple[1] = 0;
   
   if (NULL == (Global_Map = SLang_create_keymap ("global", NULL)))
     exit_error ("Malloc error creating keymap.", 0);
   
   Global_Map->functions = Jed_Functions;
   
   /* This breaks under some DEC ALPHA compilers (scary!) */
#ifndef __DECC
   for (ch = ' '; ch < 256; ch++)
     {
	simple[0] = (char) ch;
	SLkm_define_key (simple, (FVOID_STAR) ins_char_cmd, Global_Map);
     }
#else
   ch = ' ';
   while (1)
     {
	simple[0] = (char) ch;
	SLkm_define_key (simple, (FVOID_STAR) ins_char_cmd, Global_Map);
	ch = ch + 1;
	if (ch == 256) break;
     }
#endif
   
   SLkm_define_key ("^[1", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[2", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[3", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[4", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[5", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[6", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[7", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[8", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[9", (FVOID_STAR) digit_arg, Global_Map);
   SLkm_define_key ("^[0", (FVOID_STAR) digit_arg, Global_Map);
   
#ifndef pc_system
   /* give vtxxx arrow keys */
   SLkm_define_key ("^[[D", (FVOID_STAR) previous_char_cmd, Global_Map);
   SLkm_define_key ("^[OD", (FVOID_STAR) previous_char_cmd, Global_Map);
   SLkm_define_key ("^[[A", (FVOID_STAR) previous_line_cmd, Global_Map);
   SLkm_define_key ("^[OA", (FVOID_STAR) previous_line_cmd, Global_Map);
   SLkm_define_key ("^[[B", (FVOID_STAR) next_line_cmd, Global_Map);
   SLkm_define_key ("^[OB", (FVOID_STAR) next_line_cmd, Global_Map);
   SLkm_define_key ("^[[C", (FVOID_STAR) next_char_cmd, Global_Map);
   SLkm_define_key ("^[OC", (FVOID_STAR) next_char_cmd, Global_Map);
   SLkm_define_key ("^[[6~", (FVOID_STAR) pagedown_cmd, Global_Map);
   SLkm_define_key ("^[[5~", (FVOID_STAR) pageup_cmd, Global_Map);
   SLkm_define_key ("^K^[[C", (FVOID_STAR) scroll_left, Global_Map);
   SLkm_define_key ("^K^[[D", (FVOID_STAR) scroll_right, Global_Map);
   SLkm_define_key ("^K^[[A", (FVOID_STAR) bob, Global_Map);
   SLkm_define_key ("^K^[[B", (FVOID_STAR)eob, Global_Map);
   
#ifdef sun
   SLkm_define_key ("\033[216z", (FVOID_STAR) pageup_cmd, Global_Map);
   SLkm_define_key ("\033[222z", (FVOID_STAR) pagedown_cmd, Global_Map);
   SLkm_define_key ("\033[214z", (FVOID_STAR) bol, Global_Map);
   SLkm_define_key ("\033[220z", (FVOID_STAR) eol_cmd, Global_Map);
#endif
   
#else
#include "doskeys.c"
#endif
   
   SLkm_define_key ("'", (FVOID_STAR) text_smart_quote, Global_Map);
   SLkm_define_key ("\"", (FVOID_STAR) text_smart_quote, Global_Map);
   SLkm_define_key ("^_", (FVOID_STAR) undo, Global_Map);
   SLkm_define_key ("^?", (FVOID_STAR) backward_delete_char_untabify, Global_Map);
#ifndef pc_system
   SLkm_define_key ("^H", (FVOID_STAR) backward_delete_char_untabify, Global_Map);
#endif
   SLkm_define_key ("^B", (FVOID_STAR) bol, Global_Map);
   SLkm_define_key ("^D", (FVOID_STAR) pagedown_cmd, Global_Map);
   SLkm_define_key ("^E", (FVOID_STAR) eol_cmd, Global_Map);
/*
 SLkm_define_key ("^FB", (FVOID_STAR) search_backward_cmd, Global_Map);
 SLkm_define_key ("^FF", (FVOID_STAR) search_forward_cmd, Global_Map);
*/
   SLkm_define_key ("^G", (FVOID_STAR) kbd_quit, Global_Map);
   SLkm_define_key ("^I", (FVOID_STAR) indent_line, Global_Map);
   SLkm_define_key ("^K(", (FVOID_STAR) begin_keyboard_macro, Global_Map);
   SLkm_define_key ("^K)", (FVOID_STAR) end_keyboard_macro, Global_Map);
   SLkm_define_key ("^KD", (FVOID_STAR) evaluate_cmd, Global_Map);
   SLkm_define_key ("^KE", (FVOID_STAR) exit_jed, Global_Map);
   SLkm_define_key ("^KG", (FVOID_STAR) find_file, Global_Map);
   SLkm_define_key ("^KK", (FVOID_STAR) copy_to_pastebuffer, Global_Map);
   SLkm_define_key ("^KM", (FVOID_STAR) mark_spot, Global_Map);
   SLkm_define_key ("^KX", (FVOID_STAR) execute_keyboard_macro, Global_Map);
   SLkm_define_key ("^K^B", (FVOID_STAR) set_mark_cmd, Global_Map);
   SLkm_define_key ("^K^I", (FVOID_STAR) insert_file_cmd, Global_Map);
/*     SLang_define_key1("^K^L", (VOID_STAR) double_line, SLKEY_F_INTRINSIC, Global_Map); */
   SLkm_define_key ("^K^M", (FVOID_STAR) pop_spot, Global_Map);
   SLkm_define_key ("^K^P", (FVOID_STAR) yank, Global_Map);
/*     SLang_define_key1("^K^R", (VOID_STAR) replace, SLKEY_F_INTRINSIC, Global_Map); */
   SLkm_define_key ("^K^V", (FVOID_STAR) kill_region, Global_Map);
   SLkm_define_key ("^K^W", (FVOID_STAR) write_buffer, Global_Map);
   SLkm_define_key ("^L", (FVOID_STAR) kill_line, Global_Map);
   SLkm_define_key ("^M", (FVOID_STAR) newline_and_indent, Global_Map);
   SLkm_define_key ("^R", (FVOID_STAR) redraw, Global_Map);
   SLkm_define_key ("^U", (FVOID_STAR) pageup_cmd, Global_Map);
   SLkm_define_key ("^V", (FVOID_STAR) delete_char_cmd, Global_Map);
   SLkm_define_key ("^W0", (FVOID_STAR) delete_window, Global_Map);
   SLkm_define_key ("^W1", (FVOID_STAR) one_window, Global_Map);
   SLkm_define_key ("^W2", (FVOID_STAR) split_window, Global_Map);
   SLkm_define_key ("^WO", (FVOID_STAR) other_window, Global_Map);
   SLkm_define_key ("^XB", (FVOID_STAR) get_buffer, Global_Map);
   SLkm_define_key ("^XK", (FVOID_STAR) kill_buffer, Global_Map);
   SLkm_define_key ("^XN", (FVOID_STAR) narrow_to_region, Global_Map);
   SLkm_define_key ("^XQ", (FVOID_STAR) macro_query, Global_Map);
   SLkm_define_key ("^XS", (FVOID_STAR) save_some_buffers, Global_Map);
   SLkm_define_key ("^XW", (FVOID_STAR) widen_region, Global_Map);
   SLkm_define_key ("^X^", (FVOID_STAR) enlarge_window, Global_Map);
/*     SLang_define_key1("^X^T", (VOID_STAR) transpose_lines, SLKEY_F_INTRINSIC, Global_Map); */
   SLkm_define_key ("^X^[", (FVOID_STAR) evaluate_cmd, Global_Map);
   SLkm_define_key ("^X^X", (FVOID_STAR) exchange_point_mark, Global_Map);
   SLkm_define_key ("^Z", (FVOID_STAR) sys_spawn_cmd, Global_Map);
   SLkm_define_key ("^[<", (FVOID_STAR) bob, Global_Map);
   SLkm_define_key ("^[>", (FVOID_STAR) eob, Global_Map);
   SLkm_define_key ("^[N", (FVOID_STAR) narrow_paragraph, Global_Map);
   SLkm_define_key ("^[Q", (FVOID_STAR) text_format_paragraph, Global_Map);
   SLkm_define_key ("^[S", (FVOID_STAR) center_line, Global_Map);
   SLkm_define_key ("^[X", (FVOID_STAR) evaluate_cmd, Global_Map);
   SLkm_define_key ("^[\\", (FVOID_STAR) trim_whitespace, Global_Map);
   SLkm_define_key ("^\\", (FVOID_STAR) goto_match, Global_Map);
   SLkm_define_key ("`", (FVOID_STAR) quoted_insert, Global_Map);
   
   if (X_Define_Keys_Hook != NULL)  (*X_Define_Keys_Hook)(Global_Map);
}

/*}}}*/

char Last_Kbd_Command_String[32];
char Current_Kbd_Command_String[32];

void set_current_kbd_command (char *s) /*{{{*/
{
   strncpy (Current_Kbd_Command_String, s, 31);
   Current_Kbd_Command_String[31] = 0;
}

/*}}}*/

static int key_interpret(SLang_Key_Type *key) /*{{{*/
{
   char *rtc_save;
   char *str;
   int ret;
   static int macro_depth;
   
   strcpy (Last_Kbd_Command_String, Current_Kbd_Command_String);
   
   if (key->type == SLKEY_F_INTRINSIC)
     {
	set_current_kbd_command (Jed_Key_Buffer);
	Current_Kbd_Command_String[31] = 0;
	ret = (key->f.f)();
     }
   else
     {
	str = key->f.s;
	set_current_kbd_command (str);
	if (*str == ' ')
	  {
	     /* string to insert */
	     insert_string(str + 1);
	  }
	else if ((*str == '@') && (0 != *(str + 1)))
	  {
	     int repeat;
	     
	     if (Repeat_Factor != NULL)
	       {
		  repeat = *Repeat_Factor;
		  Repeat_Factor = NULL;
	       }
	     else repeat = 1;
	     
	     if (macro_depth > 10)
	       {
		  macro_depth = 0;
		  msg_error ("Possible runaway macro aborted.");
		  return 1;
	       }
	     
	     macro_depth++;
	     rtc_save = Read_This_Character;
	     
	     while (repeat-- > 0)
	       {
		  Read_This_Character = str + 1;
		  do
		    {
		       do_key();
		    }
		  while ((Read_This_Character != NULL)
			 && (SLKeyBoard_Quit  == 0)
			 && (SLang_Error == 0));
		  
		  if (SLKeyBoard_Quit || SLang_Error)
		    break;
	       }
	     
	     Read_This_Character = rtc_save;
	     macro_depth--;
	  }
	else if ((*str == '.')
		 || !SLang_execute_function(str))
	  SLang_load_string(str);
	
	ret = 1;
     }
   
   Last_Key_Function = key->f.f;
   return(ret);
}

/*}}}*/

static void update_jed_keybuffer (void) /*{{{*/
{
   Last_Key_Buffer_Len = (int) (Key_Bufferp - Key_Buffer);
   if (Last_Key_Buffer_Len == 1)
     {
	*Jed_Key_Buffer = *Key_Buffer;
	*(Jed_Key_Buffer + 1) = 0;
     }
   else
     {
        *Key_Bufferp = 0;
	strcpy (Jed_Key_Buffer, Key_Buffer);
     }
   
   Key_Bufferp = Key_Buffer;
}

/*}}}*/


#define XKEY(key)  key_interpret((key))

int do_key (void) /*{{{*/
{
   SLang_Key_Type *key;
   int repeat;
   
   if (SLang_Key_TimeOut_Flag == 0)
     {
	Key_Bufferp = Key_Buffer;
	*Key_Bufferp = 0;
     }
   
   key = SLang_do_key (CBuf->keymap, jed_getkey);
   update_jed_keybuffer ();
   
   if ((key != NULL) && (key->f.f != NULL))
     {
	if (Repeat_Factor == NULL) return XKEY(key);
	repeat = *Repeat_Factor;
	Suspend_Screen_Update = 1;
	
	/* some routines may use the repeat factor as a prefix argument */
	while (repeat-- > 0)
	  {
	     if (SLKeyBoard_Quit || SLang_Error ||
		 (Repeat_Factor == NULL)) break;
	 /* (!Executing_Keyboard_Macro && (Repeat_Factor == NULL))) break; */
	     XKEY(key);
	  }
	Repeat_Factor = NULL;
	
	return(1);
     }
   else if (!Executing_Keyboard_Macro && !SLKeyBoard_Quit)
     {
	beep ();
	flush_input ();
     }
   if (SLKeyBoard_Quit) kbd_quit();
   return(0);
}

/*}}}*/

void do_jed(void) /*{{{*/
{
   char *name;
   Buffer *tthis = CBuf;
   
   /* Mark Undo boundary now because tthis might not be valid later */
   mark_undo_boundary(tthis);
   
   Repeat_Factor = NULL;
   Replace_Preserve_Case = 0;
   if (do_key()) JWindow->trashed = 1;
   
    /* internal editing commands may have selected a different buffer
       so put it back. */
   if (CBuf != JWindow->buffer)
     {
	if (buffer_exists(JWindow->buffer)) name = JWindow->buffer->name; else name = "*scratch*";
	switch_to_buffer_cmd(name);
     }
}

/*}}}*/

void jed(void) /*{{{*/
{
   /* This routine is called from main.  So before actually strting, check
      one more hook to just to make sure  all things are go. */
   char *startup = ".()jed_startup_hook";
   if (2 == SLang_is_defined(startup + 3))
     {
	SLang_run_hooks(startup + 3, NULL, NULL);
	/* remove its definition from memory */
	SLang_load_string (startup);
     }
   
   
   Jump_Buffer_Ptr = &Jump_Buffer;
   
   if (setjmp(Jump_Buffer.b) != 0)
     {
	SLang_restart(1);   /* just in case */
     }
   
   if (CBuf != JWindow->buffer)
     {
	switch_to_buffer(JWindow->buffer);
	window_buffer(CBuf);
     }
   JWindow->trashed = 1;
   update((Line *) NULL, 0, 0);
   Read_This_Character = NULL;
   while(1)
     {
	Suspend_Screen_Update = 0;
	do_jed();
	if (!SLKeyBoard_Quit && (CBuf->flags & BUFFER_TRASHED)
	    && (!Cursor_Motion)) CBuf->hits += 1;
	if (CBuf->hits > Jed_Max_Hits)
	  {
	     auto_save_buffer(CBuf);
	     check_buffers();   /* check files on disk to see if they are recent */
	  }
	
	if ((Last_Key_Function != (FVOID_STAR) ins_char_cmd)
	    || (JWindow->trashed) || (JWindow != JWindow->next)
	    || Suspend_Screen_Update ||
	    (Mini_Ghost) || (*Message_Buffer) || (*Error_Buffer))
	  {
	     update((Line *) NULL, 0, 0);
	  }
     }
}

/*}}}*/

int digit_arg(void) /*{{{*/
{
   static int repeat;
   char buf[20];
   int key;
   int i;
   
   i = 0;
   buf[i++] = (char) SLang_Last_Key_Char;
   
   /* After do_key (what called this), Key_Bufferp is reset.  However, I want 
    * to keep it for echoing subsequent characters.  I restore its previous 
    * value so that echoing will continue. 
    */
   
   Key_Bufferp = Key_Buffer + Last_Key_Buffer_Len;
   
   SLang_Key_TimeOut_Flag = 1;
   while(1)
     {
	buf[i] = 0;
	key = jed_getkey();
	if ((key < '0') || (key > '9')) break;
	buf[i++] = (char) key;
     }
   repeat = atoi(buf);
   Repeat_Factor = &repeat;
   if (Executing_Keyboard_Macro) Macro_Buffer_Ptr--;
   else
     {
	ungetkey (&key);
	if (Defining_Keyboard_Macro) Macro_Buffer_Ptr--;
     }
   
   if (Key_Bufferp != Key_Buffer) Key_Bufferp--;
   
   /* Key_Timeout is still active and is only reset after this call. */
   do_key();
   return(1);
}

/*}}}*/

int which_key (char *f) /*{{{*/
{
   int num = 0, i;
   SLang_Key_Type *key, *key_root;
   FVOID_STAR fp;
   unsigned char type;
   unsigned char buf[5];
   
   if (NULL == (fp = (FVOID_STAR) SLang_find_key_function(f, CBuf->keymap)))
     type = SLKEY_F_INTERPRET;
   else type = SLKEY_F_INTRINSIC;
   
   i = 256;
   key_root = CBuf->keymap->keymap;
   while (i--)
     {
	key = key_root->next;
	if ((key == NULL) && (type == key_root->type) &&
	    (((type == SLKEY_F_INTERPRET) && (!strcmp((char *) f, key_root->f.s)))
	     || ((type == SLKEY_F_INTRINSIC) && (fp == key_root->f.f))))
	  {
	     *buf = 2;
	     *(buf + 1) = 256 - 1 - i;
	     SLang_push_string(SLang_make_keystring(buf));
	     num++;
	  }
	
	while (key != NULL)
	  {
	     if ((key->type == type) &&
		 (((type == SLKEY_F_INTERPRET) && (!strcmp((char *) f, key->f.s)))
		  || ((type == SLKEY_F_INTRINSIC) && (fp == key->f.f))))
	       {
		  SLang_push_string(SLang_make_keystring(key->str));
		  num++;
	       }
	     key = key->next;
	  }
	key_root++;
     }
   return(num);
}

/*}}}*/

static char *find_function_string (FVOID_STAR f) /*{{{*/
{
   SLKeymap_Function_Type *fp;
   
   fp = Jed_Functions;
   
   if (f == (FVOID_STAR) ins_char_cmd) return "self_insert_cmd";
   
   while ((fp != NULL) && (fp->name != NULL))
     {
	if ((FVOID_STAR) fp->f == f) return fp->name;
	fp++;
     }
   return NULL;
}

/*}}}*/

static void dump_this_binding (SLang_Key_Type *key) /*{{{*/
{
   unsigned char ch, *s;
   char *str,  ctrl[2];
   char *fun;
   int n, len;
   
   s = key->str;
   
   ctrl[0] = '^';
   len = *s++ - 1;;
   
   while (len-- > 0)
     {
	n = 1;
        ch = *s++;
	if (ch == 127)
	  {
	     str = "DEL"; n = 3;
	  }
	else if (ch > ' ') str = (char *) &ch;
	else if (ch == 27)
	  {
	     str = "ESC";
	     n = 3;
	  }
	else if (ch == ' ')
	  {
	     str = "SPACE";
	     n = 5;
	  }
	else if (ch == '\t')
	  {
	     str = "TAB"; n = 3;
	  }
	else
	  {
	     str = ctrl;
	     *(str + 1) = ch + '@';
	     n = 2;
	  }
	quick_insert((unsigned char *) str, n);
	ins(' ');
     }
   ins_char_n_times('\t', 3);
   
   if (key->type == SLKEY_F_INTRINSIC)
     {
   	fun = find_function_string (key->f.f);
     }
   else fun = key->f.s;
   
   insert_string (fun);
   newline();
}

/*}}}*/

void  dump_bindings(char *map) /*{{{*/
{
   int i;
   SLang_Key_Type *next, *key_root;
   SLKeyMap_List_Type *kml;
   
   if (NULL == (kml = SLang_find_keymap(map)))
     {
	msg_error("Keymap undefined.");
	return;
     }
   key_root = kml->keymap;
   
   for (i = 0; i < 256; i++)
     {
	next = key_root->next;
	if (next != NULL)
	  {
	     while (next != NULL)
	       {
		  dump_this_binding (next);
		  next = next->next;
	       }
	  }
	else if (key_root->f.f != NULL) dump_this_binding (key_root);
	key_root++;
     }
}

/*}}}*/

char *find_key(int *ret) /*{{{*/
{
   char *fstr = NULL;
   SLang_Key_Type *key;
   
   *ret = 0;
   
   SLang_Key_TimeOut_Flag = 0;
   Key_Bufferp = Key_Buffer;
   key = SLang_do_key (CBuf->keymap, jed_getkey);
   update_jed_keybuffer ();
   
   if (key == NULL) return(NULL);
   if (key->type == SLKEY_F_INTRINSIC)
     {
	*ret = 1;
	fstr = find_function_string (key->f.f);
     }
   else fstr = key->f.s;
   
   return fstr;
}

/*}}}*/

void use_keymap(char *name) /*{{{*/
{
   SLKeyMap_List_Type *map;
   if (NULL == (map = SLang_find_keymap(name)))
     {
	msg_error("Unknown keymap.");
     }
   else (CBuf->keymap = map);
}

/*}}}*/

char *what_keymap() /*{{{*/
{
   return CBuf->keymap->name;
}

/*}}}*/

void set_abort_char(int *c) /*{{{*/
{
   char str[2];
   int i;
#ifdef pc_system
   char ch, *s, *scan = "@#$%^&*()_+?IQWERTYUIOP[]!!ASDFGHJKL!!!!\\ZXCVBNM";
   
   ch = 64 + (char) *c;
   s = scan; while (*s && (*s != ch)) s++;
   if (*s == 0) return;
   Abort_Char = (int) (s - scan) + 0x03;
#else
   Abort_Char = *c;
#endif
#ifndef MSWINDOWS
   reset_tty();
   init_tty();
#endif
   str[0] = *c; str[1] = 0;
   
   for (i = 0; i < SLANG_MAX_KEYMAPS; i++)
     {
	if (SLKeyMap_List[i].keymap == NULL) continue;
	SLang_undefine_key(str, &SLKeyMap_List[i]);
	SLkm_define_key (str, (FVOID_STAR) kbd_quit, &SLKeyMap_List[i]);
     }
}

/*}}}*/

static SLKeymap_Function_Type *Flist_Context;
static int Flist_Context_Len;

#define MAX_USER_FLIST 100
static char *Slang_Functions[MAX_USER_FLIST];
static char **Slang_Flist_Context;

int next_function_list(char *buf) /*{{{*/
{
   SLKeymap_Function_Type *tthis = Jed_Functions;
   register char *name;
   char **max;
   
   /* Convert '-' to '_' */
   
   name = buf;
   while (*name != 0)
     {
	if (*name == '-') *name = '_';
	name++;
     }
   
   while (1)
     {
	tthis = Flist_Context;
	name = tthis->name;
	if (name == NULL) break;
	Flist_Context++;
	if (!Flist_Context_Len || !strncmp(buf, name, Flist_Context_Len))
	  {
	     strcpy(buf, name);
	     return(1);
	  }
     }
   
   max = Slang_Functions + MAX_USER_FLIST;
   while (Slang_Flist_Context < max)
     {
	name = *Slang_Flist_Context;
	if (name == NULL) return(0);
	name++;  /* skip past hash mark */
	Slang_Flist_Context++;
	if (!Flist_Context_Len || !strncmp(buf, name, Flist_Context_Len))
	  {
	     strcpy(buf, name);
	     return(1);
	  }
     }
   return(0);
}

/*}}}*/

int open_function_list(char *buf) /*{{{*/
{
   Flist_Context = Jed_Functions;
   Slang_Flist_Context = Slang_Functions;
   Flist_Context_Len = strlen(buf);
   return next_function_list(buf);
}

/*}}}*/

void add_to_completion(char *name) /*{{{*/
{
   char **p, *n;
   static char **last = Slang_Functions;
   
   n = SLang_find_name(name);
   if (n == NULL)
     {
	msg_error("Undefined Symbol.");
	return;
     }
   
   p = last;  /* Slang_Functions; */
   
   while (p < Slang_Functions + MAX_USER_FLIST)
     {
	if (*p == NULL)
	  {
	     *p = n;
	     last = ++p;
	     return;
	  }
	p++;
     }
   msg_error("Completion Quota Exceeded.");
}

/*}}}*/

int is_internal(char *f) /*{{{*/
{
   if (NULL == SLang_find_key_function(f, CBuf->keymap)) return(0);
   return 1;
}

/*}}}*/

void create_keymap (char *name) /*{{{*/
{
   if (NULL == SLang_create_keymap (name, Global_Map))
     {
	msg_error ("Unable to create keymap.");
     }
}

/*}}}*/
