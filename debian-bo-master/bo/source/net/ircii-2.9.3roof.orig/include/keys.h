/*
 * keys.h: header for keys.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * This file is automatically created from keys.h.proto.
 *
 * @(#)$Id: keys.h,v 1.3 1995/11/04 13:51:10 mrg Exp $
 */

#ifndef __keys_h_
#define __keys_h_

#define BACKSPACE 0
#define BACKWARD_CHARACTER 1
#define BACKWARD_HISTORY 2
#define BACKWARD_WORD 3
#define BEGINNING_OF_LINE 4
#define CLEAR_SCREEN 5
#define COMMAND_COMPLETION 6
#define DELETE_CHARACTER 7
#define DELETE_NEXT_WORD 8
#define DELETE_PREVIOUS_WORD 9
#define END_OF_LINE 10
#define ENTER_DIGRAPH 11
#define	ENTER_MENU 12
#define ERASE_LINE 13
#define ERASE_TO_BEG_OF_LINE 14
#define ERASE_TO_END_OF_LINE 15
#define FORWARD_CHARACTER 16
#define FORWARD_HISTORY 17
#define FORWARD_WORD 18
#define META1_CHARACTER 19
#define META2_CHARACTER 20
#define META3_CHARACTER 21
#define META4_CHARACTER 22
#define NEXT_WINDOW 23
#define NOTHING 24
#define PARSE_COMMAND 25
#define PREVIOUS_WINDOW 26
#define QUIT_IRC 27
#define QUOTE_CHARACTER 28
#define REFRESH_INPUTLINE 29
#define REFRESH_SCREEN 30
#define	SCROLL_BACKWARD 31
#define	SCROLL_END 32
#define SCROLL_FORWARD 33
#define SCROLL_START 34
#define SELF_INSERT 35
#define SEND_LINE 36
#define STOP_IRC 37
#define SWAP_LAST_WINDOW 38
#define SWAP_NEXT_WINDOW 39
#define SWAP_PREVIOUS_WINDOW 40
#define SWITCH_CHANNELS 41
#define TOGGLE_INSERT_MODE 42
#define TOGGLE_STOP_SCREEN 43
#define TRANSPOSE_CHARACTERS 44
#define TYPE_TEXT 45
#define UNSTOP_ALL_WINDOWS 46
#define YANK_FROM_CUTBUFFER 47
#define NUMBER_OF_FUNCTIONS 48

/* KeyMap: the structure of the irc keymaps */
typedef struct
{
	int	index;
	char	changed;
	int	global;
	char	*stuff;
}	KeyMap;

/* KeyMapNames: the structure of the keymap to realname array */
typedef struct
{
	char	*name;
	void	(*func) _((unsigned char, char *));
}	KeyMapNames;

extern	KeyMap	keys[],
		meta1_keys[],
		meta2_keys[],
		meta3_keys[],
		meta4_keys[];
extern	KeyMapNames key_names[];

	void	(* get_send_line _((void))) _((unsigned char, char*));
	void	save_bindings _((FILE *, int));
	void	change_send_line _((void (*)(unsigned char, char *)));
	void	bindcmd _((char *, char *, char *));
	void	rbindcmd _((char *, char *, char *));
	void	parsekeycmd _((char *, char *, char *));
	void	type _((char *, char *, char *));

#endif /* __keys_h_ */
