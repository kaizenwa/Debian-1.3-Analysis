/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */

extern Buffer *The_MiniBuffer;

extern int load_file(char *);
extern int find_file_in_window(char *);
extern int find_file(void);
extern int get_buffer(void);
extern int evaluate_cmd(void);
extern int kill_buffer(void);
extern int write_buffer(void);
extern int search_forward_cmd(void);
extern int global_setkey_cmd(void);
extern int search_backward_cmd(void);
extern int insert_file_cmd(void);
extern void init_minibuffer(void);
extern int exit_minibuffer(void);
extern int select_minibuffer(void);
extern void flush_message(char *);
extern int mini_complete(void);
extern void buffer_substring(void);
extern char *make_line_string(char *);
extern int set_buffer(char *);
extern void insert_string(char *);
extern void unset_key(char *);
extern void set_key(char *, char *);
extern void unset_key_in_keymap(char *, char *);
extern int keymap_p(char *);
extern void set_key_in_keymap(char *, char *, char *);
extern char *pop_to_buffer(char *);
extern int num_windows(void);
extern int init_jed_intrinsics(void);
#if defined (__unix__) || defined (__os2__)
extern int shell_command(char *);
extern int pipe_region(char *);
#endif

extern void load_buffer(void);
extern void get_key_function(void);
extern int what_char(void);
extern void update_cmd(int *);
extern void call_cmd(char *);
extern void copy_region_cmd(char *);

#ifndef pc_system
# if defined(__QNX__)
   extern int show_memory(void);
# endif
extern void screen_w80(void);
extern void screen_w132(void);
#else
extern int show_memory(void);
#endif
extern int markp(void);
extern int dup_mark(void);
extern void mini_read(char *, char *, char *);
extern void send_string_to_term(char *);
extern void get_buffer_info(void);
extern void set_buffer_info(char *, char *, char *, int *);
extern void make_buffer_list(void);
extern char *make_buffer_substring(int *);
extern int window_size_intrinsic(int *);
extern int what_mode(void);
extern int expand_wildcards(char *);
extern int bufferp(char *);
extern int kill_buffer_cmd(char *);
extern int get_doc_string(char *, char *);
extern int insert_buffer_name(char *);
extern int find_file_cmd(char *);
extern void read_object_with_completion(char *, char *, char *, int *);
extern int replace_cmd(char *, char *);
extern void set_mode_cmd(char *, int *);
extern void switch_to_buffer_cmd(char *);
extern char *what_buffer(void);
extern int write_buffer_cmd(char *);
extern void set_tab(int *);
extern int get_yes_no(char *);
extern char *command_line_argv(int *);
extern void count_chars(void);
extern int MiniBuffer_Active;
extern int Main_Argc;
extern char **Main_Argv;
extern char Jed_Library[];
extern FILE *jed_open_slang_file(char *, char *);
extern char *Completion_Buffer;
extern void set_expansion_hook (char *);
