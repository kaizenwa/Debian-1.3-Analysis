/*
 * keys.c: Decides what happens when you press a key
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#include "config.h"
#include "edit.h"
#include "history.h"
#include "hold.h"
#include "ircaux.h"
#include "input.h"
#include "keys.h"
#include "names.h"
#include "output.h"
#include "screen.h"
#include "ircterm.h"
#include "vars.h"
#include "window.h"

extern void get_line_return _((char , char *));

/* !!!DONT!!! change this number on a whim!  You could break stuff.  It
 * is defined here so that if/when we want to add more key maps, it will
 * be real easy to change all the stuff that is dependant on how many
 * key maps there are.  However, there is no support for more then 10
 * keymaps in the code!  You have been warned!
 */

void new_key _((int, int, int, int, int, char *));
static void init_keys_2 _((void));

/* This is it.. this is the keymap -- replaces all those thousands of
 * lines that used to be at the end of this file.... good riddens, too.
 */
KeyMap *keys[NUM_KEYMAPS][256+1];

/*
 * init_keys_1 - initializes all the key bindings to zero and calls
 *    init_keys_2 which does all the default bindings
 */
void init_keys_1 _((void))
{
	bzero(keys, sizeof(KeyMap)*NUM_KEYMAPS*256);
	init_keys_2();
}
 
/*
 * lookup_function: looks up an irc function by name, and returns the
 * number of functions that match the name, and sets where index points
 * to to be the index of the (first) function found.
 */
int lookup_function (char *name, int *lf_index)
{
	int	len,
		cnt,
		i;

	if (name)
	{
		upper(name);
		len = strlen(name);
		cnt = 0;
		*lf_index = -1;
		for (i = 0; i < NUMBER_OF_FUNCTIONS; i++)
		{
			if (strncmp(name, key_names[i].name, len) == 0)
			{
				cnt++;
				if (*lf_index == -1)
					*lf_index = i;
			}
		}
		if (*lf_index == -1)
			return (0);
		if (my_stricmp(name, key_names[*lf_index].name) == 0)
			return (1);
		else
			return (cnt);
	}
	return (0);
}

/*
 * display_key: converts the character c to a displayable form and returns
 * it.  Very simple indeed 
 */
unsigned char *display_key (unsigned char c)
{
	static	unsigned char key[3];

	key[2] = (char) 0;
	if (c < 32)
	{
		key[0] = '^';
		key[1] = c + 64;
	}
	else if (c == '\177')
	{
		key[0] = '^';
		key[1] = '?';
	}
	else
	{
		key[0] = c;
		key[1] = (char) 0;
	}
	return (key);
}

/*
 * show_binding: given the ascii value of a key and a meta key status (1 for
 * meta1 keys, 2 for meta2 keys, anything else for normal keys), this will
 * display the key binding for the key in a nice way
 */
void show_binding (unsigned char c, int meta)
{
	char	meta_str[8];

	*meta_str = 0;
	if (meta < 1 || meta > MAX_META)
		meta = 0;
	else
		sprintf(meta_str, "META%d-", meta);

	if (keys[meta][c])
		say("%s%s is bound to %s %s", meta_str, display_key(c),
			key_names[keys[meta][c]->key_index].name, (keys[meta][c]->stuff &&
			(*(keys[meta][c]->stuff))) ? keys[meta][c]->stuff : empty_string);
	else
		say("%s%s is bound to NOTHING", meta_str, display_key(c));
}

/*
 * parse_key: converts a key string. Accepts any key, or ^c where c is any
 * key (representing control characters), or META1- or META2- for meta1 or
 * meta2 keys respectively.  The string itself is converted to true ascii
 * value, thus "^A" is converted to 1, etc.  Meta key info is removed and
 * returned as the function value, 0 for no meta key, 1 for meta1, and 2 for
 * meta2.  Thus, "META1-a" is converted to "a" and a 1 is returned.
 * Furthermore, if ^X is bound to META2_CHARACTER, and "^Xa" is passed to
 * parse_key(), it is converted to "a" and 2 is returned.  Do ya understand
 * this? 
 */
int parse_key (char *key_str)
{
	char	*ptr1,
		*ptr2;
	unsigned char	c;
	int	meta = 0;

	ptr2 = ptr1 = key_str;
	while (*ptr1)
	{
		if (*ptr1 == '^')
		{
			ptr1++;
			switch (*ptr1)
			{
				case 0:
					*(ptr2++) = '^';
					break;
				case '?':
					*(ptr2++) = '\177';
					ptr1++;
					break;
				default:
					c = *(ptr1++);
					if (islower(c))
						c = toupper(c);
					if (c < 64)
					{
						say("Illegal key sequence: ^%c", c);
						return (-1);
					}
					*(ptr2++) = c - 64;
			}
		}
		else
			*(ptr2++) = *(ptr1++);
	}
	*ptr2 = (char) 0;
	if (strlen(key_str) > 1)
	{
		/* There used to be ten cases here that checked for
		 * each of the METAX- strings.  Now we want to be able
		 * to have double digit METAXX strings, so we look after
		 * the META and extract the number there before the hyphen.
		 * then we remove the METAXX- string.
		 */
		if (my_strnicmp(key_str, "META", 4) == 0)
		{
			char *ptr = key_str+4,
			     *str = key_str+4;

			while (*ptr && *ptr != '-')
				ptr++;
			if (*ptr) 
				ptr++;
			meta = atoi(str);
			strcpy(key_str, ptr);
		}
		/* Here too, used to be ten cases.  Im being a little more
		 * risky by doing it this way, becuase it makes the assumption
		 * that METAX_CHARACTERs are all defined as consecutive
		 * numbers.  A note has gone in keys.h.proto to not break
		 * up the METAX_CHARACTER defines.
		 */
		else
		{
			int foo = META1_CHARACTER - 1; /* just to make sure */
			if (keys[0][(u_char)*key_str])
				foo = keys[0][(u_char) *key_str]->key_index;
			if ((foo >= META1_CHARACTER) && (foo - META1_CHARACTER < MAX_META))
				meta = foo - META1_CHARACTER + 1;
			strcpy(key_str, key_str + 1);
		}
	}
	return (meta);
}

/*
 * bind_it: does the actually binding of the function to the key with the
 * given meta modifier
 */
static void bind_it (char *function, char *string, char key, int meta)
{
	int	cnt,
		bi_index,
		i;
	int 	changed;

	if (meta < 1 || meta > MAX_META)
		meta = 0;

	if (*string == (char) 0)
		string = NULL;

	switch (cnt = lookup_function(function, &bi_index))
	{
		case 0:
			say("No such function: %s", function);
			break;
		case 1:
		{
			changed = 1;
			new_key (meta, key, bi_index, changed, loading_global, string);
			show_binding(key, meta);
			break;
		}
		default:
			say("Ambiguous function name: %s", function);
			for (i = 0; i < cnt; i++, bi_index++)
				put_it("%s", key_names[bi_index].name);
			break;
	}
}

/* parsekeycmd: does the PARSEKEY command.  */
void parsekeycmd (char *command, char *args, char *subargs)
{
	int	i;
	char	*arg;

	if ((arg = next_arg(args, &args)) != NULL)
	{
		switch (lookup_function(arg, &i))
		{
		case 0:
			say("No such function %s", arg);
			return;
		case 1:
			key_names[i].func(0, args);
			break;
		default:
			say("Ambigious function %s", arg);
			break;
		}
	}
}

/*
 * bindcmd: the bind command, takes a key sequence followed by a function
 * name followed by option arguments (used depending on the function) and
 * binds a key.  If no function is specified, the current binding for that
 * key is shown 
 */
void bindcmd (char *command, char *args, char *subargs)
{
	char	*key,
		*function;
	int	meta = 0;

	if ((key = next_arg(args, &args)) != NULL)
	{
		int in_space = 0;
		in_space = stristr(key, "space") ? 1 : 0;
		if (((meta = parse_key(key)) == -1))
			return;
		if (strlen(key) > 1 && !in_space)
		{
			say("Key sequences may not contain more than two keys");
			return;
		}
		if ((function = next_arg(args, &args)) != NULL)
			bind_it(function, args, in_space ? ' ' : *key, meta);
		else
			show_binding(in_space?' ':(unsigned char )*key, meta);
	}
	else
	{
		int i, j, k = charset_size();
		for (i = 0; i <= MAX_META; i++)
			for (j = 0; j <= k; j++)
				if (keys[i][j] && (keys[i][j]->key_index != NOTHING) && (keys[i][j]->key_index != SELF_INSERT))
					show_binding(j, i);
	}
}

/*
 * rbindcmd: does the rbind command.  you give it a string that something
 * is bound to and it tells you all the things that are bound to that
 * functions
 */
void rbindcmd (char *command, char *args, char *subargs)
{
	int	f;
	char	*arg;

	if ((arg = next_arg(args, &args)) != NULL)
	{
		int	i, j;
		int	charsize = charset_size();

		switch (lookup_function(arg, &f))
		{
		case 0:
			say("No such function %s", arg);
			return;

		case 1:
			break;

		default:
			say("Ambigious function %s", arg);
			return;
		}

		for (i = 0; i <= MAX_META; i++)
			for (j = 0; j <= charsize; j++)
				if (keys[i][j] && keys[i][j]->key_index == f)
					show_binding(j, i);
	}
}

/*
 * type: The TYPE command.  This parses the given string and treats each
 * character as tho it were typed in by the user.  Thus key bindings are used
 * for each character parsed.  Special case characters are control character
 * sequences, specified by a ^ follow by a legal control key.  Thus doing
 * "/TYPE ^B" will be as tho ^B were hit at the keyboard, probably moving the
 * cursor backward one character.
 */
void type (char *command, char *args, char *subargs)
{
	int	c;
	char	key;

	while (*args)
	{
		if (*args == '^')
		{
			switch (*(++args))
			{
			case '?':
				key = '\177';
				args++;
				break;
			default:
				c = *(args++);
				if (islower(c))
					c = toupper(c);
				if (c < 64)
				{
					say("Illegal key sequence: ^%c", c);
					return;
				}
				key = c - 64;
				break;
			}
		}
		else if (*args == '\\')
		{
			key = *++args;
			args++;
		}
		else
			key = *(args++);
		edit_char(key);
	}
}

KeyMapNames key_names[] =
{
	{ "AUTOREPLY",			input_autoreply },
	{ "AUTOREPLY_BACK",		input_autoreplyback },
	{ "BACKSPACE",			input_backspace },
	{ "BACKWARD_CHARACTER",		backward_character },
	{ "BACKWARD_HISTORY",		backward_history },
	{ "BACKWARD_WORD",		input_backward_word },
	{ "BEGINNING_OF_LINE",		input_beginning_of_line },
	{ "BOLD",			insert_bold },
	{ "CDCC_PLIST",			cdcc_plist },
	{ "CHANNEL_CHOPS",		channel_chops },
	{ "CHANNEL_NONOPS",		channel_nonops },
	{ "CHANGE_TO_SPLIT",		change_to_split }, 
	{ "CHELP",			do_chelp },
	{ "CLEAR_SCREEN",		clear_screen },
	{ "COMMAND_COMPLETION",		command_completion },
	{ "DCC_PLIST",			dcc_plist },
	{ "DELETE_CHARACTER",		input_delete_character },
	{ "DELETE_NEXT_WORD",		input_delete_next_word },
	{ "DELETE_PREVIOUS_WORD",	input_delete_previous_word },
	{ "DELETE_TO_PREVIOUS_WORD",	input_delete_to_previous_space },
	{ "END_OF_LINE",		input_end_of_line },
	{ "ERASE_LINE",			input_clear_line },
	{ "ERASE_TO_BEG_OF_LINE",	input_clear_to_bol },
	{ "ERASE_TO_END_OF_LINE",	input_clear_to_eol },
	{ "FORWARD_CHARACTER",		forward_character },
	{ "FORWARD_HISTORY",		forward_history },
	{ "FORWARD_WORD",		input_forward_word },
	{ "HIGHLIGHT_OFF",		highlight_off },
	{ "JOIN_LAST_INVITE",		join_last_invite },
	{ "META1_CHARACTER",		meta1_char },
	{ "META2_CHARACTER",		meta2_char },
	{ "META3_CHARACTER",		meta3_char },
	{ "META4_CHARACTER",		meta4_char },
	{ "META5_CHARACTER",		meta5_char },
	{ "META6_CHARACTER",		meta6_char },
	{ "META7_CHARACTER",		meta7_char },
	{ "META8_CHARACTER",		meta8_char },
	{ "META9_CHARACTER",		meta9_char },
	{ "NEW_BEGINNING_OF_LINE",	new_input_beginning_of_line },
	{ "NEW_SCROLL_BACKWARD",	my_scrollback },
	{ "NEW_SCROLL_END",		my_scrollend },
	{ "NEW_SCROLL_FORWARD",		my_scrollforward},
	{ "NEXT_WINDOW",		next_window },
	{ "NICK_COMPLETION",		nick_completion },
	{ "NOTHING",			NULL },
	{ "PARSE_COMMAND",		parse_text },
	{ "PREVIOUS_WINDOW",		previous_window },
	{ "QUIT_IRC",			irc_quit },
	{ "QUOTE_CHARACTER",		quote_char },
	{ "REFRESH_INPUTLINE",		refresh_inputline },
	{ "REFRESH_SCREEN",		(KeyBinding) refresh_screen },
	{ "REVERSE",			insert_reverse },
	{ "SCROLL_BACKWARD",		scrollback_backwards },
	{ "SCROLL_END",			scrollback_end },
	{ "SCROLL_FORWARD",		scrollback_forwards },
	{ "SCROLL_START",		scrollback_start },
	{ "SELF_INSERT",		input_add_character },
	{ "SEND_LINE",			get_line_return },
	{ "SHOVE_TO_HISTORY",		shove_to_history },
	{ "STOP_IRC",			term_pause },
	{ "SWAP_LAST_WINDOW",		swap_last_window },
	{ "SWAP_NEXT_WINDOW",		swap_next_window },
	{ "SWAP_PREVIOUS_WINDOW",	swap_previous_window },
	{ "SWITCH_CHANNELS",		switch_channels },
	{ "TAB_MSG",			input_msgreply },
	{ "TAB_MSG_BACK",		input_msgreplyback },
	{ "TOGGLE_CLOAK",		toggle_cloak },
	{ "TOGGLE_INSERT_MODE",		toggle_insert_mode },
	{ "TOGGLE_OV",			toggle_ov },
	{ "TOGGLE_STOP_SCREEN",		toggle_stop_screen },
	{ "TRANSPOSE_CHARACTERS",	input_transpose_characters },
	{ "TYPE_TEXT",			type_text },
	{ "UNDERLINE",			insert_underline },
	{ "UNSTOP_ALL_WINDOWS",		unstop_all_windows },
	{ "WHOLEFT",			wholeft },

	{ "WINDOW_BALANCE",		window_balance },
	{ "WINDOW_GROW_ONE",		window_grow_one },
	{ "WINDOW_HELP",		w_help },
	{ "WINDOW_HIDE",		window_hide },
	{ "WINDOW_KILL",		window_kill },
	{ "WINDOW_LIST",		window_list },
	{ "WINDOW_MOVE",		window_move },
	{ "WINDOW_SHRINK_ONE",		window_shrink_one },

	{ "WINDOW_SWAP_1",		window_swap1 },
	{ "WINDOW_SWAP_2",		window_swap2 },
	{ "WINDOW_SWAP_3",		window_swap3 },
	{ "WINDOW_SWAP_4",		window_swap4 },
	{ "WINDOW_SWAP_5",		window_swap5 },
	{ "WINDOW_SWAP_6",		window_swap6 },
	{ "WINDOW_SWAP_7",		window_swap7 },
	{ "WINDOW_SWAP_8",		window_swap8 },
	{ "WINDOW_SWAP_9",		window_swap9 },
	{ "WINDOW_SWAP_10",		window_swap10 },
	{ "YANK_FROM_CUTBUFFER",	input_yank_cut_buffer }
};

/* real simple way to make a new keybinding */
void new_key (int map, int chr, int type, int change, int global, char *stuff)
{
	if (keys[map][chr])
	{
		if (keys[map][chr]->stuff)
			new_free(&(keys[map][chr]->stuff));
		new_free((char **)&(keys[map][chr]));
		keys[map][chr] = NULL;
	}

	if (type != NOTHING)
	{
		keys[map][chr] = (KeyMap *)new_malloc(sizeof(KeyMap));
		keys[map][chr]->key_index = type;
		keys[map][chr]->changed = change;
		keys[map][chr]->global = global;
		if (stuff)
			keys[map][chr]->stuff = m_strdup(stuff);
		else
			keys[map][chr]->stuff = NULL;
	}
}

/* special interface to new_key for the default key bindings */
static void snew_key (int map, int chr, int type)
{
	new_key (map, chr, type, 0, 0, NULL);
}

/* This is where you put all the default key bindings.  This is a lot
 * simpler, just defining those you need, instead of all of them, isnt
 * it?  And it takes up so much less memory, too...
 */
static void init_keys_2 _((void))
{
	int i;

	/* all the "default" bindings are self_insert unless we bind
	 * them differently */
	for (i = 0; i <= 254; i++)
		snew_key(0, i, SELF_INSERT);

	/* "default" characters that arent self_insert */
	snew_key(0,  1, BEGINNING_OF_LINE);		/* ^A */
	snew_key(0,  2, BOLD);				/* ^B */
	snew_key(0,  4, DELETE_CHARACTER);		/* ^D */
	snew_key(0,  5, CHANGE_TO_SPLIT);		/* ^E */
	snew_key(0,  6, WHOLEFT);			/* ^F */
	snew_key(0,  8, BACKSPACE);			/* ^H (delete) */
#if 0
	snew_key(0,  9, TOGGLE_INSERT_MODE);		/* ^I (tab) */
#endif
	snew_key(0,  9, TAB_MSG);			/* ^I (tab) */
	snew_key(0, 10, SEND_LINE);			/* ^J (enter) */
	snew_key(0, 11, JOIN_LAST_INVITE);		/* ^K */
	snew_key(0, 12, REFRESH_SCREEN);		/* ^L (linefeed) */
	snew_key(0, 13, SEND_LINE);			/* ^M (return) */
	snew_key(0, 14, QUOTE_CHARACTER);		/* ^N */
	snew_key(0, 15, HIGHLIGHT_OFF);			/* ^O */
	snew_key(0, 16, BACKWARD_HISTORY);		/* ^P */

	snew_key(0, 17, QUOTE_CHARACTER);		/* ^Q */
	snew_key(0, 18, NICK_COMPLETION);		/* ^R */
	snew_key(0, 19, TOGGLE_STOP_SCREEN);		/* ^S */
	snew_key(0, 20, TRANSPOSE_CHARACTERS);		/* ^T */
	snew_key(0, 21, ERASE_LINE);			/* ^U */
	snew_key(0, 22, REVERSE);			/* ^V */
	snew_key(0, 23, META2_CHARACTER);               /* ^W */
	snew_key(0, 24, SWITCH_CHANNELS);		/* ^X */

	snew_key(0, 25, YANK_FROM_CUTBUFFER);		/* ^Y */

	snew_key(0, 26, STOP_IRC);			/* ^Z */
	snew_key(0, 27, META1_CHARACTER);		/* ^[ (escape) */
#if 0
	snew_key(0, 29, SHOVE_TO_HISTORY);		/* ^] */
#endif
	snew_key(0, 29, AUTOREPLY);
/*	snew_key(1, 27, AUTOREPLY_BACK);*/
	snew_key(0, 31, UNDERLINE);			/* ^_ */

	snew_key(0, 127, BACKSPACE);			/* ^? (delete) */

	/* european keyboards (and probably others) use the eigth bit
	   for extended characters.  Having these keys bound by default
	   causes them lots of grief, so unless you really want to use
	   these, they are commented out.
	 */
#ifdef EMACS_KEYBINDS
	snew_key(0, 188, SCROLL_START);	
	snew_key(0, 190, SCROLL_END);
	snew_key(0, 226, BACKWARD_WORD);
	snew_key(0, 228, DELETE_NEXT_WORD);
	snew_key(0, 229, SCROLL_END);
	snew_key(0, 230, FORWARD_WORD);
	snew_key(0, 232, DELETE_PREVIOUS_WORD);
	snew_key(0, 255, DELETE_PREVIOUS_WORD);
#endif

	/* meta 1 characters */
	snew_key(1,  27, COMMAND_COMPLETION);
	snew_key(1,  46, CLEAR_SCREEN);
	snew_key(1,  60, SCROLL_START);
	snew_key(1,  62, SCROLL_END);
	snew_key(1,  79, META2_CHARACTER);
	snew_key(1,  91, META2_CHARACTER);
	snew_key(1,  98, BACKWARD_WORD);
	snew_key(1, 100, DELETE_NEXT_WORD);
	snew_key(1, 101, SCROLL_END);
	snew_key(1, 102, FORWARD_WORD);
	snew_key(1, 104, DELETE_PREVIOUS_WORD);
	snew_key(1, 110, SCROLL_FORWARD);
	snew_key(1, 112, SCROLL_BACKWARD);
	snew_key(1, 127, DELETE_PREVIOUS_WORD);

	snew_key(1, '1', WINDOW_SWAP_1);
	snew_key(1, '2', WINDOW_SWAP_2);
	snew_key(1, '3', WINDOW_SWAP_3);
	snew_key(1, '4', WINDOW_SWAP_4);
	snew_key(1, '5', WINDOW_SWAP_5);
	snew_key(1, '6', WINDOW_SWAP_6);
	snew_key(1, '7', WINDOW_SWAP_7);
	snew_key(1, '8', WINDOW_SWAP_8);
	snew_key(1, '9', WINDOW_SWAP_9);
	snew_key(1, '0', WINDOW_SWAP_10);

	/* meta 2 characters */
	snew_key(2,  26, STOP_IRC);
	snew_key(2,  65, BACKWARD_HISTORY);
	snew_key(2,  66, FORWARD_HISTORY);
	snew_key(2,  67, FORWARD_CHARACTER);
	snew_key(2,  68, BACKWARD_CHARACTER);
	snew_key(2, 110, SWAP_NEXT_WINDOW);
	snew_key(2, 112, PREVIOUS_WINDOW);

	snew_key(2, '1', NEW_BEGINNING_OF_LINE);
	snew_key(2, '2', TOGGLE_OV);
	snew_key(2, '3', TOGGLE_CLOAK);
	snew_key(2, '4', NEW_SCROLL_END);
	snew_key(2, '5', NEW_SCROLL_BACKWARD);
	snew_key(2, '6', NEW_SCROLL_FORWARD);

	snew_key(2, '?', WINDOW_HELP);
	snew_key(2, '+', WINDOW_GROW_ONE);
	snew_key(2, '-', WINDOW_SHRINK_ONE);
	snew_key(2, 'm', WINDOW_MOVE);
	snew_key(2, 'l', WINDOW_LISTK);
	snew_key(2, 'k', WINDOW_KILL);
	snew_key(2, 'b', WINDOW_BALANCE);
	snew_key(2, 'h', WINDOW_HIDE);
		
	/* meta 3 characters */
	/* <none> */
	snew_key(2, '[', META3_CHARACTER);
/*	snew_key(2, '1', META3_CHARACTER);*/

	snew_key(3, 'A', CHELP);
	snew_key(3, 'B', CHANNEL_CHOPS);
	snew_key(3, 'C', CHANNEL_NONOPS);
	snew_key(3, 'D', CDCC_PLIST);
	snew_key(3, 'E', DCC_PLIST);
	
	/* meta 4 characters -- vi key mappings */
	snew_key(4,   8, BACKWARD_CHARACTER);
	snew_key(4,  32, FORWARD_CHARACTER);
	snew_key(4,  65, META4_CHARACTER);
	snew_key(4,  72, BACKWARD_CHARACTER);
	snew_key(4,  73, META4_CHARACTER);
	snew_key(4,  74, FORWARD_HISTORY);
	snew_key(4,  75, BACKWARD_HISTORY);
	snew_key(4,  76, FORWARD_CHARACTER);
	snew_key(4,  88, DELETE_CHARACTER);
	snew_key(4,  97, META4_CHARACTER);
	snew_key(4, 104, BACKWARD_CHARACTER);
	snew_key(4, 105, META4_CHARACTER);
	snew_key(4, 106, FORWARD_HISTORY);
	snew_key(4, 107, BACKWARD_HISTORY);
	snew_key(4, 108, FORWARD_CHARACTER);
	snew_key(4, 120, DELETE_CHARACTER);
}


/*
 * write_binding: This will write to the given FILE pointer the information
 * about the specified key binding.  The format it writes it out is such that
 * it can be parsed back in later using LOAD or with the -l switch 
 */
static void write_binding (unsigned char c, unsigned char meta, FILE *fp, int do_all)
{
	char	meta_str[8];

	if (c == 32)
		return;

	*meta_str = 0;
	if (meta < 1 || meta > MAX_META)
		meta = 0;
	else
		sprintf(meta_str, "META%d-", meta);

	if (keys[meta][c] && keys[meta][c]->changed)
	{
		fprintf(fp, "BIND %s%s %s", meta_str, display_key(c), key_names[keys[meta][c]->key_index].name);
		if (keys[meta][c]->stuff && (*(keys[meta][c]->stuff)))
			fprintf(fp, " %s", keys[meta][c]->stuff);
		fprintf(fp, "\n");
	}
}

/*
 * save_bindings: this writes all the keys bindings for IRCII to the given
 * FILE pointer using the write_binding function 
 */
void save_bindings (FILE *fp, int do_all)
{
	int	i, j;
	int	charsize = charset_size();

	for (i = 0; i <= MAX_META; i++)
		for (j = 0; j < charsize; j++)
			write_binding(j, i, fp, do_all);
}

void clear_bindings(void)
{
	int	i, j;
	int	charsize = charset_size();

	for (i = 0; i <= MAX_META; i++)
		for (j = 0; j < charsize; j++)
		{
			if (keys[i][j] && keys[i][j]->stuff)
				new_free(&(keys[i][j]->stuff));
			if (keys[i][j])
				new_free((char **)&(keys[i][j]));
		}
}
