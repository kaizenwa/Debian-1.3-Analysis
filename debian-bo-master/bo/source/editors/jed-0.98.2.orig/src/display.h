/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */

extern void (*tt_goto_rc)(int, int);
extern void (*tt_begin_insert)(void);
extern void (*tt_end_insert)(void);
extern void (*tt_del_eol)(void);
extern void (*tt_delete_nlines)(int);
extern void (*tt_delete_char)(void);
extern void (*tt_erase_line)(void);
extern void (*tt_cls)(void);
extern void (*tt_beep)(void);
extern void (*tt_reverse_index)(int);
extern void (*tt_smart_puts)(unsigned short *, unsigned short *, int, int);
extern void (*tt_wide_width)(void);
extern void (*tt_narrow_width)(void);
extern void (*tt_write_string)(char *);
extern void (*tt_putchar)(char);
extern void (*tt_init_video)(void);
extern void (*tt_reset_video)(void);
extern void (*tt_normal_video)(void);
extern void (*tt_set_scroll_region)(int, int);
extern void (*tt_reset_scroll_region)(void);
extern void (*tt_get_terminfo)(void);
extern void (*tt_enable_cursor_keys)(void);
extern void (*tt_set_term_vtxxx)(int *);
extern void (*tt_set_color)(int, char *, char *, char *);
extern void (*tt_set_color_esc)(int, char *);

extern int *tt_Ignore_Beep;
extern int *tt_Term_Cannot_Insert;
extern int *tt_Use_Ansi_Colors;
extern int *tt_Term_Cannot_Scroll;
extern int *tt_Screen_Rows;
extern int *tt_Screen_Cols;
extern int *tt_Baud_Rate;

#ifndef pc_system
extern int *tt_Blink_Mode;
#endif

#ifdef pc_system
  extern int Attribute_Byte;
# ifndef __GO32__
   extern int Cheap_Video;
   extern void video_off(void);
   extern void video_on(void);
# endif
#endif

/* Miscellaneous Stuff */
extern int Scroll_By_Copying;

extern void flush_output (void);
