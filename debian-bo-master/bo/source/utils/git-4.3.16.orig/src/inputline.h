/* inputline.h -- Prototypes for the functions in inputline.c.  */

/* Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifndef _GIT_INPUTLINE_H
#define _GIT_INPUTLINE_H


#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#include "stdc.h"
#include "window.h"


#define IL_RECORD       0
#define IL_PREVIOUS     1
#define IL_NEXT         2


#define IL_DONT_STORE   0
#define IL_DONT_KILL    0
#define IL_STORE        1
#define IL_KILL         2


/* Some #defines used in external input line interfaces.  */
#define IL_FREEZED	 0
#define IL_EDIT		 1
#define IL_MOVE		 2
#define IL_BEEP		 4
#define IL_ERROR	 8
#define IL_SAVE		16
#define IL_HOME		32

#define IL_ISEARCH_ACTION_FAILED       -1
#define IL_ISEARCH_ACTION_NONE          0
#define IL_ISEARCH_ACTION_DECREASE      1
#define IL_ISEARCH_ACTION_RETRY         2
#define IL_ISEARCH_ACTION_INCREASE      3

#define IL_UPDATE        1


typedef struct
{
    window_t *win;
    int echo;                   /* echo flag.  */
    int error;                  /* use error-like output.  */
    int last_operation;         /* last basic edit operation performed.  */
    size_t point;               /* point position.  */
    size_t mark;                /* mark position.  */
    size_t columns;             /* number of columns.  */
    size_t length;              /* total length of the input line.  */
    size_t static_length;       /* static text length.  */
    size_t dynamic_length;      /* dynamic text length.  */
    size_t size;                /* current buffer size.  */
    char *buffer;               /* buffer.  */
    char *kill_ring;            /* the kill ring :-).  */
    char *history_file;
} input_line_t;


extern input_line_t *il;

extern void il_free PROTO ((input_line_t *));
extern void il_init PROTO ((size_t, size_t));
extern void il_end PROTO (());
extern input_line_t *il_save PROTO (());
extern void il_restore PROTO ((input_line_t *));
extern int il_echo PROTO ((int));
extern int il_is_empty PROTO (());
extern void il_set_mark PROTO (());
extern void il_kill_region PROTO (());
extern void il_kill_ring_save PROTO (());
extern void il_yank PROTO (());
extern void il_exchange_point_and_mark PROTO (());
extern void il_backward_char PROTO (());
extern void il_forward_char PROTO (());
extern void il_backward_word PROTO (());
extern void il_forward_word PROTO (());
extern void il_beginning_of_line PROTO (());
extern void il_end_of_line PROTO (());
extern void il_insert_char PROTO ((char));
extern void il_delete_char PROTO (());
extern void il_backward_delete_char PROTO (());
extern void il_kill_word PROTO (());
extern void il_backward_kill_word PROTO (());
extern void il_reset_line PROTO (());
extern void il_kill_line PROTO ((int));
extern void il_kill_to_beginning_of_line PROTO (());
extern void il_kill_to_end_of_line PROTO (());
extern void il_just_one_space PROTO (());
extern void il_delete_horizontal_space PROTO (());
extern void il_downcase_word PROTO (());
extern void il_upcase_word PROTO (());
extern void il_capitalize_word PROTO (());
extern void il_set_static_text PROTO ((char *));
extern void il_insert_text PROTO ((char *));
extern void il_update_point PROTO (());
extern void il_update PROTO (());
extern void il_full_update PROTO (());
extern int il_get_contents PROTO ((char **));
extern void il_message PROTO ((char *));
extern void il_set_error_flag PROTO ((int));
extern void il_history PROTO ((int));


#endif  /* _GIT_INPUTLINE_H */
