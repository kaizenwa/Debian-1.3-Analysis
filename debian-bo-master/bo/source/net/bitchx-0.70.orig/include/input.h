/*
 * input.h: header for input.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: input.h,v 1.8 1995/08/31 03:51:34 scottr Exp $
 */

#ifndef __input_h_
#define __input_h_
	char	input_pause _((char *));
	void	set_input _((char *));
	void	set_input_prompt _((Window *, char *, int));
	char	*get_input_prompt _((void));
	char	*get_input _((void));
	void	update_input _((int));
	void	init_input _((void));
	void	input_move_cursor _((int));
	void	change_input_prompt _((int));
	void	cursor_to_input _((void));
	void	setup_ov_mode _((int));

/* keybinding functions */
extern	void 	backward_character 	_((char, char *));
extern	void 	backward_history 	_((char, char *));
extern	void 	clear_screen 		_((char, char *));
extern	void	command_completion 	_((char, char *));
extern	void 	forward_character	_((char, char *));
extern	void 	forward_history 	_((char, char *));
extern	void	highlight_off 		_((char, char *));
extern	void	input_add_character 	_((char, char *));
extern	void	input_backspace 	_((char, char *));
extern	void	input_backward_word 	_((char, char *));
extern	void	input_beginning_of_line _((char, char *));
extern	void	new_input_beginning_of_line _((char, char *));
extern	void	input_clear_line 	_((char, char *));
extern	void	input_clear_to_bol 	_((char, char *));
extern	void	input_clear_to_eol 	_((char, char *));
extern	void	input_delete_character 	_((char, char *));
extern	void	input_delete_next_word 	   _((char, char *));
extern	void	input_delete_previous_word _((char, char *));
extern 	void	input_delete_to_previous_space _((char, char *));
extern	void	input_end_of_line 	   _((char, char *));
extern	void	input_forward_word 	   _((char, char *));
extern  void	input_transpose_characters _((char, char *));
extern	void	input_yank_cut_buffer 	   _((char, char *));
extern	void	insert_bold 		_((char, char *));
extern	void	insert_reverse 		_((char, char *));
extern	void	insert_underline 	_((char, char *));
extern	void 	meta1_char 		_((char, char *));
extern	void 	meta2_char 		_((char, char *));
extern	void 	meta3_char 		_((char, char *));
extern	void 	meta4_char 		_((char, char *));
extern	void 	meta5_char 		_((char, char *));
extern	void 	meta6_char 		_((char, char *));
extern	void 	meta7_char 		_((char, char *));
extern	void 	meta8_char 		_((char, char *));
extern	void 	meta9_char 		_((char, char *));
/*
extern	void 	parse_text 		_((char, char *));
extern	void 	quote_char 		_((char, char *));
*/
extern	void	refresh_inputline 	_((char, char *));
extern	void 	send_line 		_((char, char *));
extern	void 	toggle_insert_mode 	_((char, char *));
extern	void	input_msgreply		_((char, char *));
extern	void	input_autoreply		_((char, char *));

extern	void	input_msgreplyback	_((char, char *));
extern	void	input_autoreplyback	_((char, char *));

extern	void	my_scrollback		_((char, char *));
extern	void	my_scrollforward	_((char, char *));
extern	void	my_scrollend		_((char, char *));
/*
extern	void 	type_text 		_((char, char *));
*/
extern	void	wholeft			_((char, char *));
extern	void	toggle_ov			_((char, char *));
extern	void	toggle_cloak		_((char, char *));
extern	void	cdcc_plist		_((char, char *));
extern	void	dcc_plist		_((char, char *));
extern	void	channel_chops		_((char, char *));
extern	void	channel_nonops		_((char, char *));
extern	void	change_to_split		_((char, char *));
extern	void	do_chelp		_((char, char *));
extern	void	join_last_invite	_((char, char *));
extern	void	window_swap1		_((char, char *));
extern	void	window_swap2		_((char, char *));
extern	void	window_swap3		_((char, char *));
extern	void	window_swap4		_((char, char *));
extern	void	window_swap5		_((char, char *));
extern	void	window_swap6		_((char, char *));
extern	void	window_swap7		_((char, char *));
extern	void	window_swap8		_((char, char *));
extern	void	window_swap9		_((char, char *));
extern	void	window_swap10		_((char, char *));
extern	void	w_help			_((char, char *));

extern	void	window_balance		_((char, char *));
extern	void	window_grow_one		_((char, char *));
extern	void	window_hide		_((char, char *));
extern	void	window_kill		_((char, char *));
extern	void	window_list		_((char, char *));
extern	void	window_move		_((char, char *));
extern	void	window_shrink_one	_((char, char *));
extern	void	nick_completion		_((char, char *));

/* used by update_input */
#define NO_UPDATE 0
#define UPDATE_ALL 1
#define UPDATE_FROM_CURSOR 2
#define UPDATE_JUST_CURSOR 3

#endif /* __input_h_ */
