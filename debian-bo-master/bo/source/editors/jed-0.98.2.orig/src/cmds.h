/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
extern int jed_buffer_visible (char *b);
extern int newline(void);
extern int newline_cmd(void);
extern int previous_char_cmd(void);
extern int delete_char_cmd(void);
extern int backward_delete_char_cmd(void);
extern int backward_delete_char_untabify(void);
extern int previous_line_cmd(void);
extern int next_line_cmd(void);
extern int next_char_cmd(void);
extern int kill_line(void);
extern void insert_line(Line *line);
extern int double_line(void);
extern int kill_eol_cmd(void);
extern int kill_line_cmd(void);
extern int trim_whitespace(void);
extern int indent_line(void);
extern int transpose_lines(void);
extern int newline_and_indent(void);
extern int eol_cmd(void);
extern int sys_spawn_cmd(void);
extern int ins_char_cmd(void);
extern int exit_jed(void);
extern int quit_jed(void);
extern int save_some_buffers(void);
extern int pagedown_cmd(void);
extern int pageup_cmd(void);
extern int quoted_insert(void);
extern int scroll_left(void);
extern int scroll_right(void);
extern void indent_to(int);
extern int goto_column1(int *);
extern void goto_column(int *);
extern void goto_top_of_window (void);
extern int goto_bottom_of_window (void);
extern void insert_whitespace(int *);
extern unsigned char *get_current_indent(int *);
extern unsigned int skip_whitespace(void);
extern int looking_at(char *);
extern void skip_chars(char *);
extern void skip_chars1(char *, int);
extern void bskip_chars(char *);
extern void bskip_chars1(char *, int);

extern int Blink_Flag;
extern int Indented_Text_Mode;
extern int Kill_Line_Feature;
extern int Jed_Secure_Mode;
extern int Goal_Column;
extern int Jed_Tab_Default;
extern int Jed_Wrap_Column;
extern int Jed_Suspension_Not_Allowed;
extern int Jed_Use_Tabs;
#if 0
extern int new_find_matching (int *, int *);
#endif


