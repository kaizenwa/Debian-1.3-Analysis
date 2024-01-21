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
	void	set_input_prompt _((char *));
	char	*get_input_prompt _((void));
	char	*get_input _((void));
	void	update_input _((int));
	void	init_input _((void));
	void	input_move_cursor _((int));
	void	change_input_prompt _((int));
	void	cursor_to_input _((void));
	void	input_add_character _((unsigned char, char *));
	void	input_backward_word _((unsigned char, char *));
	void	input_forward_word _((unsigned char, char *));
	void	input_delete_previous_word _((unsigned char, char *));
	void	input_delete_next_word _((unsigned char, char *));
	void	input_clear_to_bol _((unsigned char, char *));
	void	input_clear_line _((unsigned char, char *));
	void	input_end_of_line _((unsigned char, char *));
	void	input_clear_to_eol _((unsigned char, char *));
	void	input_beginning_of_line _((unsigned char, char *));
	void	refresh_inputline _((unsigned char, char *));
	void	input_delete_character _((unsigned char, char *));
	void	input_backspace _((unsigned char, char *));
	void	input_transpose_characters _((unsigned char, char *));
	void	input_yank_cut_buffer _((unsigned char, char *));

/* used by update_input */
#define NO_UPDATE 0
#define UPDATE_ALL 1
#define UPDATE_FROM_CURSOR 2
#define UPDATE_JUST_CURSOR 3

#endif /* __input_h_ */
