/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1996 John E. Davis  (davis@space.mit.edu)
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
#include "screen.h"
#include "window.h"
#include "display.h"
#include "sysdep.h"
#include "file.h"
#include "keymap.h"
#include "ledit.h"
#include "misc.h"
#include "ins.h"
#include "paste.h"

/*}}}*/

/*{{{ Global Variables */

char Error_Buffer[256];
char Message_Buffer[256];
int Exit_From_MiniBuffer;

#define MACRO_SIZE JED_MAX_PATH_LEN
char Macro_Buffer[MACRO_SIZE];
char *Macro_Buffer_Ptr = Macro_Buffer;
char *Macro_Ptr_Max = Macro_Buffer;
int Defining_Keyboard_Macro = 0;
int Executing_Keyboard_Macro = 0;

MiniInfo_Type Mini_Info;

/*}}}*/

typedef struct /*{{{*/
{
   jmp_buf b;
}

/*}}}*/
jmp_buf_struct;

extern jmp_buf_struct Jump_Buffer, *Jump_Buffer_Ptr;

static struct /*{{{*/
{
   int def;
   int exe;
}

/*}}}*/
Macro_State;

static void macro_store_key(char ch) /*{{{*/
{
   /* I need to put something here to increase the size of the Macro_Buffer
      in case of overflow */
   
   if (Macro_Buffer_Ptr - Macro_Buffer >= MACRO_SIZE) msg_error("Macro Size exceeded.");
   else *Macro_Buffer_Ptr++ = ch;
}

/*}}}*/

static void restore_macro_state(void) /*{{{*/
{
   Defining_Keyboard_Macro = Macro_State.def;
   Executing_Keyboard_Macro = Macro_State.exe;
}

/*}}}*/

static void set_macro_state(void) /*{{{*/
{
   Macro_State.def = Defining_Keyboard_Macro;
   Macro_State.exe = Executing_Keyboard_Macro;
}

/*}}}*/

/* if not in the mini buffer and if during keyboard macro, allow user to enter
   different text each time macro is executed */

int macro_query() /*{{{*/
{
   char *s;
   int n;
   
   /* macro state is already set */
   Defining_Keyboard_Macro = 0;
   Executing_Keyboard_Macro = 0;
   
   if (!IS_MINIBUFFER)
     {
	if (NULL == (s = read_from_minibuffer("Enter String:", (char *) NULL, NULL, &n))) return(0);
	ins_chars((unsigned char *) s, n);
	SLFREE(s);
     }
   
   /* exit from mini restores the state */
   return(1);
}

/*}}}*/

int jed_getkey() /*{{{*/
{
   int ch, diff, ch1;
   static int mini_flag = 0;
   int *srf;
   
   if (Read_This_Character != NULL)
     {
	ch = *Read_This_Character++;
	if (ch == 0) Read_This_Character = NULL;
	else
	  {
	     if ((ch1 = *Read_This_Character) == 0)
	       {
		  Read_This_Character = NULL;
		  return (ch);
	       }
	     
	     
	     if (ch == '^')
	       {
		  if (ch1 == '?') ch = 127;
		  else ch = ch1 - '@';
		  Read_This_Character++;
	       }
	     else if (ch == '\\')
	       {
		  ch = ch1;
		  Read_This_Character++;
	       }
	     if (*Read_This_Character == 0) Read_This_Character = NULL;
	     return(ch);
	  }
	
     }
   
   if (Executing_Keyboard_Macro)
     {
	if (Macro_Buffer_Ptr < Macro_Ptr_Max) return (*Macro_Buffer_Ptr++);
	Executing_Keyboard_Macro = 0;
	update((Line *) NULL, 0, 0);
     }
   
   diff = (int) (Key_Bufferp - Key_Buffer);
   if (!SLang_Error && SLang_Key_TimeOut_Flag &&
       ((mini_flag && (diff > 0))
	|| !input_pending(&Number_Ten)))
     {
	message(Key_Buffer);
	strcat(Message_Buffer, "-");
	
	/* This ensures that the update is performed if we are echoing from 
	 * digit argument */
	srf = Repeat_Factor; Repeat_Factor = NULL;
	update((Line *)NULL, 0, 0);
	Repeat_Factor = srf;
	clear_message ();
	mini_flag = 1;
     }
   else (mini_flag = 0);
   
   ch = my_getkey();
   if (diff < 13)
     {
	*Key_Bufferp++ = (char) ch;
	*Key_Bufferp = 0;
     }
   else
     {
	/* msg_error("KeyBuffer overflow!"); */
	Key_Bufferp = Key_Buffer;
     }
   
   
   if (Defining_Keyboard_Macro)
     {
	/* starting a new keysequence to save this point */
	if (diff == 0)
	  {
	     Macro_Ptr_Max = Macro_Buffer_Ptr;
	  }
	
	macro_store_key(ch);
     }
   return(ch);
}

/*}}}*/

void msg_error(char *msg) /*{{{*/
{
   flush_input();
   JWindow->trashed = 1;
   Defining_Keyboard_Macro = Executing_Keyboard_Macro = 0;
   Read_This_Character = NULL;
   Repeat_Factor = NULL;
   set_macro_state();
   if (Batch) fprintf(stderr, "%s\n", msg);
   
   if (!SLang_Error) SLang_Error = INTRINSIC_ERROR;
   if (Error_Buffer[0] == 0) safe_strcpy(Error_Buffer, msg, sizeof (Error_Buffer));
}

/*}}}*/

/* later I will do more with this-- for now, keep last one */
void message (char *msg) /*{{{*/
{
   if (Executing_Keyboard_Macro) return;
   if (msg == NULL) msg = "";
   
   if (Batch || (JScreen[0].old == NULL)) fprintf(stderr, "%s\n", msg);
   
   if (*msg == 0) msg = " ";
   strncpy(Message_Buffer, msg, 255);
   Message_Buffer[255] = 0;
}

/*}}}*/

/* read from minibuffer using prompt providing a default, and stuffing
   the minibuffer with init if necessary */
/* I should make recursive mini buffers */
char *read_from_minibuffer (char *prompt, char *deflt, char *what, int *n) /*{{{*/
{
   char buf[JED_MAX_PATH_LEN];
   jmp_buf_struct *mini_jmp_save, mini_jmp_buf;
   unsigned char *ps;
   unsigned char *p, ch;
   
   static Window_Type *current_window;
   /* may get changed if user leaves minibuffer and returns from other
    * window which means that the new window becomes target 
    * of minibuffer action 
    */
   Window_Type *w;
   char *ret;
   int len, save_point;
   
   if (!IS_MINIBUFFER) current_window = JWindow;
   if (select_minibuffer()) return(NULL);   /* we should be on a new line of mini buffer */
   
   ps = Mini_Info.prompt;
   p = (unsigned char *) prompt;
   len = 0;
   
   if (p != NULL)
     {
	while (0 != (ch = *p++))
	  {
	     *ps++ = ch;
	     len++;
	  }
	*ps++ =  ' ';
	len++;
     }
   
   if ((deflt != NULL) && (*deflt != 0))
     {
	sprintf(buf,"(default: %s) ", deflt);
	p = (unsigned char *) buf;
	
	while (0 != (ch = *p++))
	  {
	     *ps++ = ch;
	     len++;
	  }
     }
   *ps = 0;
   
   /* Use calculate column to find effective length */
   
   Mini_Info.prompt_len = len;
   p = CLine->data; save_point = Point;
   CLine->data = Mini_Info.prompt;  Point = len;
   Mini_Info.effective_prompt_len = 0;
   Mini_Info.effective_prompt_len = calculate_column() - 1;
   CLine->data = p;  Point = save_point;
   
   touch_window();
   if (what != NULL) insert_string(what);
   update((Line *) NULL, 0, 0);
   
   mini_jmp_save = Jump_Buffer_Ptr;
   Jump_Buffer_Ptr = &mini_jmp_buf;
   
   if (setjmp(mini_jmp_buf.b) != 0)
     {
	SLang_restart(1);   /* just in case */
	update((Line *) NULL, 0, 0);
     }
   
   while(!Exit_From_MiniBuffer)
     {
	do_jed();
	/* if (setjmp(Jump_Buffer) != 0)
	 * {
	 * Exit_From_MiniBuffer = 1;
	 * }  */
	
	if (SLKeyBoard_Quit && IS_MINIBUFFER) break;
	update((Line *) NULL, 0, 0);
     }
   
   if (Exit_From_MiniBuffer && !Executing_Keyboard_Macro
       && (Repeat_Factor == NULL) && !JWindow->trashed && !input_pending(&Number_Zero))
     {
	tt_goto_rc (*tt_Screen_Rows - 1, 0);
	flush_output ();
     }
   
   Jump_Buffer_Ptr = mini_jmp_save;
   
   exit_minibuffer ();		       /* be graceful */
   
   Exit_From_MiniBuffer = 0;
   MiniBuffer_Active = 0;
   restore_macro_state();
   
   if (!SLKeyBoard_Quit)
     {
	if (CLine->len == 0)
	  {
	     if (deflt != NULL) insert_string(deflt);
	  }
	bob();
	push_mark();
	eob();
	ret = make_buffer_substring(n);
     }
   else ret = NULL;
   
   /* we should be in minibuffer so delete marks and spots */
   while (CBuf->spots) pop_spot();
   while (CBuf->marks) pop_mark(&Number_Zero);
   erase_buffer();
   
   /* Remove MiniWindow from the ring */
   w = JWindow;
   while (w->next != JWindow) w = w->next;
   other_window();
   w->next = JWindow;
   
   /* Note that by this time, current_window might be history. */
   while((JWindow != current_window) && (JWindow != w)) other_window();
   JWindow->trashed = 1;
   
   /* delete_buffer(MiniBuffer); */
   MiniBuffer = NULL;
   return(ret);
}

/*}}}*/

static int Macro_Illegal_Now = 0;
static int check_macro_legality(void) /*{{{*/
{
   if (Macro_Illegal_Now)
     {
	msg_error("Illegal context for Macro definition.");
	return (0);
     }
   return (1);
}

/*}}}*/

int begin_keyboard_macro() /*{{{*/
{
   if (!check_macro_legality()) return (0);
   
   Macro_Buffer_Ptr = Macro_Buffer;
   message("Defining Macro.");
   Defining_Keyboard_Macro = 1;
   set_macro_state();
   return(1);
}

/*}}}*/

int end_keyboard_macro() /*{{{*/
{
   
   if (Defining_Keyboard_Macro) message("Macro Defined.");
   else
     {
	if (!Executing_Keyboard_Macro) msg_error("Not defining Macro!");
	Executing_Keyboard_Macro = 0;
	return(1);
     }
   
   /* Macro_Ptr_Max = Macro_Buffer_Ptr; */
   Defining_Keyboard_Macro = 0;
   set_macro_state();
   return(1);
}

/*}}}*/

int execute_keyboard_macro() /*{{{*/
{
   int repeat = 0, *repeat_ptr;
   
   if (!check_macro_legality()) return (0);
   
   if (Defining_Keyboard_Macro)
     {
	msg_error("Can't execute a macro while defining one.");
	return(0);
     }
   
   Executing_Keyboard_Macro = 1;
   set_macro_state();
   Macro_Buffer_Ptr = Macro_Buffer;
   
   /* save the repeat context */
   repeat_ptr = Repeat_Factor;
   if (repeat_ptr != NULL) repeat = *repeat_ptr;
   
   JWindow->trashed = 1;
   Macro_Illegal_Now = 1;
   while ((Macro_Buffer_Ptr < Macro_Ptr_Max)
	  && Executing_Keyboard_Macro) /* since we might enter minibuffer
					and stop macro before returning
					here */
     {
	Repeat_Factor = NULL;
	/* ch = *Macro_Buffer_Ptr++; */
	do_key();
	if (SLKeyBoard_Quit || (*Error_Buffer)) break;
     }
   Macro_Illegal_Now = 0;
   
   /* restore context */
   Repeat_Factor = repeat_ptr;
   if (repeat_ptr != NULL) *repeat_ptr = repeat;
   Executing_Keyboard_Macro = 0;
   set_macro_state();
   return(1);
}

/*}}}*/

void get_last_macro () /*{{{*/
{
   register char *m, *s, ch;
   char buf[2 * MACRO_SIZE + 1];
   
   if (Defining_Keyboard_Macro)
     {
	msg_error("Complete Macro first!");
	return;
     }
   
   m = Macro_Buffer;
   if (m == Macro_Ptr_Max)
     {
	msg_error("Macro not defined.");
	return;
     }
   
   s = buf;
   while (m < Macro_Ptr_Max)
     {
	ch = *m++;
	if ((ch < ' ') || (ch == 127))
	  {
	     *s++ = '^';
	     if (ch == 127) ch = '?'; else ch = '@' + ch;
	  }
	else if ((ch == '^') || (ch == '\\'))
	  {
	     *s++ = '\\';
	  }
	
	*s++ = ch;
     }
   *s = 0;
   SLang_push_string(buf);
}

/*}}}*/

char *safe_strcpy (char *a, char *b, unsigned int n) /*{{{*/
{
   if (n != 0)
     {
	n--;
	strncpy (a, b, n);
	a[n] = 0;
     }
   return a;
}

/*}}}*/

char *safe_strcat (char *a, char *b, unsigned int n) /*{{{*/
{
   unsigned int len = strlen (a);
   unsigned int max;
   
   n--;
   
   if (n > len)
     {
	max = n - len;
	strncpy (a + len, b, max);
	a[n] = 0;
     }
   return a;
}

/*}}}*/

void clear_message (void) /*{{{*/
{
   message (NULL);
}

/*}}}*/
