/*
 * keys.h: header for keys.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: keys.h.proto,v 1.6 1994/07/02 02:38:10 mrg Exp $
 */

#ifndef _KEYS_H_
#define _KEYS_H_

/*#include "irc_std.h"*/

#define MAX_META 9
#define NUM_KEYMAPS MAX_META + 1

enum KEY_TYPES {
	AUTOREPLY,
	AUTOREPLY_BACK,
	BACKSPACE,
	BACKWARD_CHARACTER,
	BACKWARD_HISTORY,
	BACKWARD_WORD,
	BEGINNING_OF_LINE,
	BOLD,
	CDCC_PLIST,
	CHANNEL_CHOPS,
	CHANNEL_NONOPS,
	CHANGE_TO_SPLIT,
	CHELP,
	CLEAR_SCREEN,
	COMMAND_COMPLETION,
	DCC_PLIST,
	DELETE_CHARACTER,
	DELETE_NEXT_WORD,
	DELETE_PREVIOUS_WORD,
	DELETE_TO_PREVIOUS_SPACE,
	END_OF_LINE,
#if 0
	ENTER_DIGRAPH,
#endif
	ERASE_LINE,
	ERASE_TO_BEG_OF_LINE,
	ERASE_TO_END_OF_LINE,
	FORWARD_CHARACTER,
	FORWARD_HISTORY,
	FORWARD_WORD,
	HIGHLIGHT_OFF,
	JOIN_LAST_INVITE,
/* These MUST be in order and must be together... */
	META1_CHARACTER,
	META2_CHARACTER,
	META3_CHARACTER,
	META4_CHARACTER,
	META5_CHARACTER,
	META6_CHARACTER,
	META7_CHARACTER,
	META8_CHARACTER,
	META9_CHARACTER,
/* If they arent, everything will break.  Got it? */
	NEW_BEGINNING_OF_LINE,
	NEW_SCROLL_BACKWARD,
	NEW_SCROLL_END,
	NEW_SCROLL_FORWARD,
	NEXT_WINDOW,
	NICK_COMPLETION,
	NOTHING,
	PARSE_COMMAND,
	PREVIOUS_WINDOW,
	QUIT_IRC,
	QUOTE_CHARACTER,
	REFRESH_INPUTLINE,
	REFRESH_SCREEN,
	REVERSE,
	SCROLL_BACKWARD,
	SCROLL_END,
	SCROLL_FORWARD,
	SCROLL_START,
	SELF_INSERT,
	SEND_LINE,
	SHOVE_TO_HISTORY,
	STOP_IRC,
	SWAP_LAST_WINDOW,
	SWAP_NEXT_WINDOW,
	SWAP_PREVIOUS_WINDOW,
	SWITCH_CHANNELS,
	TAB_MSG,
	TAB_MSG_BACK,
	TOGGLE_CLOAK,
	TOGGLE_INSERT_MODE,
	TOGGLE_OV,
	TOGGLE_STOP_SCREEN,
	TRANSPOSE_CHARACTERS,
	TYPE_TEXT,
	UNDERLINE,
	UNSTOP_ALL_WINDOWS,
	WHOLEFT,
	
	WINDOW_BALANCE,
	WINDOW_GROW_ONE,
	WINDOW_HELP,
	WINDOW_HIDE,
	WINDOW_KILL,
	WINDOW_LISTK,
	WINDOW_MOVE,
	WINDOW_SHRINK_ONE,
	
	WINDOW_SWAP_1,
	WINDOW_SWAP_2,
	WINDOW_SWAP_3,
	WINDOW_SWAP_4,
	WINDOW_SWAP_5,
	WINDOW_SWAP_6,
	WINDOW_SWAP_7,
	WINDOW_SWAP_8,
	WINDOW_SWAP_9,
	WINDOW_SWAP_10,
	YANK_FROM_CUTBUFFER,
	NUMBER_OF_FUNCTIONS
};

/* I hate typdefs... */
typedef void (*KeyBinding) _((char, char *));

/* KeyMap: the structure of the irc keymaps */
typedef struct
{
	enum KEY_TYPES	key_index;
		char	changed;
		int	global;
		char	*stuff;
}	KeyMap;


/* KeyMapNames: the structure of the keymap to realname array */
typedef struct
{
	char	*name;
	KeyBinding func;
}	KeyMapNames;

extern	KeyMap	*keys[NUM_KEYMAPS][256+1];
extern	KeyMapNames key_names[];

extern	KeyBinding 	get_send_line 		_((void));
extern	void		save_bindings 		_((FILE *, int));
extern	void		input_add_character	_((char, char *));
extern	void		bindcmd 		_((char *, char *, char *));
extern  void    	rbindcmd 		_((char *, char *, char *));
extern  void    	parsekeycmd 		_((char *, char *, char *));
extern  void    	type 			_((char *, char *, char *));
extern	void		init_keys_1 		_((void));

#endif /* _KEYS_H_ */
