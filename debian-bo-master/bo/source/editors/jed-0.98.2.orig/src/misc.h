/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#ifndef _JED_MISC_H_
#define _JED_MISC_H_
#include "window.h"

extern void exit_error(char *, int);
extern void msg_error(char *);
extern void message(char *);
extern void clear_message (void);
extern void read_string(char *, int *);
extern void do_error(void);
extern void clear_error(void);
extern int jed_getkey(void);

extern int begin_keyboard_macro(void);
extern int macro_query(void);
extern int end_keyboard_macro(void);
extern int execute_keyboard_macro(void);
extern void get_last_macro(void);
extern int Defining_Keyboard_Macro;
extern int Executing_Keyboard_Macro;

extern char Error_Buffer[256];
extern char Message_Buffer[256];

/* information needed by minibuffer and display routines */
typedef struct MiniInfo_Type
  {
     Window_Type *action_window;   /* buffer for minibuffer action */
     unsigned char prompt[132];
     int prompt_len;
     int effective_prompt_len;	       /* length when tabs/etc are expanded */
  }
MiniInfo_Type;

extern MiniInfo_Type Mini_Info;
extern int get_macro(void);
extern int Exit_From_MiniBuffer;
extern char *Macro_Buffer_Ptr;
extern char *read_from_minibuffer(char *, char *, char *, int *);
extern char *safe_strcat (char *, char *, unsigned int);
extern char *safe_strcpy (char *, char *, unsigned int);

#define IS_MINIBUFFER (JWindow->top == *tt_Screen_Rows)

#endif

