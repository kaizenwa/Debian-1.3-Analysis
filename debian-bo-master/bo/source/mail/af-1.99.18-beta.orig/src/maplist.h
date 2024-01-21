/* Maplist.h - List of default keymaps and key bindings for af.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

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


/****************************************************************************/
/* RCS info */

#ifndef lint
static char *MaplistId = "$Id: maplist.h,v 1.15 1997/03/05 21:23:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The list of keymaps by name */

static KEYMAP *keymaps = NULL;

/****************************************************************************/
/* The structures used to define the default key bindings */

typedef struct {
	char key;				/* The key within the map */
	FORM *(*func)();			/* The bound function */
} CMD_BINDING;

typedef struct {
	char key;				/* The key within the map */
	char *map;				/* The bound keymap */
} MAP_BINDING;

typedef struct {
	char *map_name;				/* Name of the keymap */
	CMD_BINDING *commands;			/* Commands bound in map */
	MAP_BINDING *keymaps;			/* Keymaps bound in map */
} KEYMAP_DEFAULTS;

/****************************************************************************/
/* The default bindings for the global keymap */

static CMD_BINDING global_cbindings[] = {
	{ CTRL('@'),	set_mark },
	{ CTRL('G'),	kbd_quit },
	{ CTRL('K'),	kill_line },
	{ CTRL('L'),	recenter },
	{ CTRL('N'),	next_line },
	{ CTRL('P'),	prev_line },
	{ CTRL('R'),	se_back },
	{ CTRL('S'),	se_fwd },
	{ CTRL('U'),	univ_arg },
	{ CTRL('V'),	up_scroll },
	{ CTRL('W'),	kill_region },
	{ CTRL('Y'),	yank },
#ifdef HAVE_JOBCONTROL
	{ CTRL('Z'),	suspend },
#else /* ! HAVE_JOBCONTROL */
	{ CTRL('Z'),	shell },
#endif /* ! HAVE_JOBCONTROL */
	{ CTRL('\\'),	se_fwd },
	{ '\0',		NULL }
};

static MAP_BINDING global_mbindings[] = {
	{ CTRL('C'),	CONTROL_C_MAP },
	{ CTRL('H'),	HELP_MAP },
	{ CTRL('X'),	CONTROL_X_MAP },
	{ CTRL('['),	PREFIX_MAP },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the help keymap */

static CMD_BINDING help_cbindings[] = {
	{ CTRL('A'),	alias_list },
	{ CTRL('C'),	copying },
	{ CTRL('D'),	cmd_list },
	{ CTRL('F'),	func_list },
	{ CTRL('G'),	kbd_quit },
	{ CTRL('H'),	help_help },
	{ CTRL('K'),	macro_list },
	{ CTRL('N'),	news },
	{ CTRL('V'),	var_list },
	{ CTRL('W'),	warranty },
	{ '?',		help_help },
	{ 'a',		apropos },
	{ 'b',		bind_list },
	{ 'c',		brf_describe },
	{ 'd',		func_describe },
	{ 'f',		func_describe },
	{ 'h',		help_help },
	{ 'i',		info },
	{ 'k',		key_describe },
	{ 'j',		major_describe },
	{ 'l',		view_lossage },
	{ 'm',		mode_describe },
	{ 'n',		news },
	{ 'v',		var_describe },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the prefix keymap */

static CMD_BINDING pfx_cbindings[] = {
	{ CTRL('G'),	kbd_quit },
	{ ' ',		set_mark },
	{ '!',		typecmd },
	{ '-',		neg_arg },
	{ '0',		digit_arg },
	{ '1',		digit_arg },
	{ '2',		digit_arg },
	{ '3',		digit_arg },
	{ '4',		digit_arg },
	{ '5',		digit_arg },
	{ '6',		digit_arg },
	{ '7',		digit_arg },
	{ '8',		digit_arg },
	{ '9',		digit_arg },
	{ '<',		buf_start },
	{ '>',		buf_end },
	{ 'g',		goto_line },
	{ 'v',		down_scroll },
	{ 'w',		copy_region },
	{ 'x',		exec_command },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the control-x keymap */

static CMD_BINDING cx_cbindings[] = {
	{ CTRL('B'),	buf_list },
	{ CTRL('G'),	kbd_quit },
	{ CTRL('X'),	exchange },
#ifdef HAVE_JOBCONTROL
	{ CTRL('Z'),	suspend },
#else /* ! HAVE_JOBCONTROL */
	{ CTRL('Z'),	shell },
#endif /* ! HAVE_JOBCONTROL */
	{ '!',		shellcmd },
	{ '(',		start_macro },
	{ ')',		end_macro },
	{ '=',		cursor_pos },
	{ '?',		key_describe },
	{ 'a',		set },
	{ 'e',		call_macro },
	{ 'q',		macro_query },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the mail keymap */

static CMD_BINDING mail_cbindings[] = {
	{ CTRL('M'),	open_msg },
	{ CTRL('O'),	open_msg },
	{ ' ',		clear_mb },
	{ '<',		buf_start },
	{ '>',		buf_end },
	{ 'k',		kill_line },
	{ 'n',		next_line },
	{ 'p',		prev_line },
	{ '\0',		NULL }
};

static MAP_BINDING mail_mbindings[] = {
	{ CTRL('C'),	MAIL_CONTROL_C_MAP },
	{ CTRL('T'),	MAIL_CONTROL_T_MAP },
	{ CTRL('X'),	MAIL_CONTROL_X_MAP },
	{ CTRL('['),	MAIL_PREFIX_MAP },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the mail prefix keymap */

static CMD_BINDING mail_pfx_cbindings[] = {
	{ CTRL('M'),	page_msg },
	{ CTRL('O'),	page_msg },
	{ CTRL('R'),	group_reply },
	{ CTRL('V'),	owin_scroll },
	{ '+',		save_msg },
	{ '.',		move_win },
	{ '=',		msg_info },
	{ 'b',		bounce },
	{ 'f',		forward },
	{ 'm',		mail },
	{ 'p',		print_msg },
	{ 'r',		reply },
	{ 's',		mail },
	{ 'y',		yank_pop },
	{ 'z',		save_and_exit },
	{ '|',		pipe_msg },
	{ '~',		not_modified },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the mail control-x keymap */

static CMD_BINDING mail_cx_cbindings[] = {
	{ CTRL('A'),	set_alias },
	{ CTRL('C'),	exit_af },
	{ CTRL('E'),	edit_msg },
	{ CTRL('F'),	find_file },
	{ CTRL('I'),	insert_file },
	{ CTRL('M'),	save_all },
	{ CTRL('Q'),	readonly },
	{ CTRL('R'),	find_readonly },
	{ CTRL('S'),	save },
	{ CTRL('V'),	find_alternate },
	{ CTRL('W'),	save_name },
	{ CTRL('\\'),	save },
	{ '+',		save_region },
	{ '0',		del_cwin },
	{ '1',		del_owin },
	{ '2',		split_win },
	{ '@',		pipe_msg },
	{ '^',		enlarge_win },
	{ 'b',		switch_buf },
	{ 'i',		insert_file },
	{ 'k',		kill_buf },
	{ 'n',		nrw_region },
	{ 'o',		other_win },
	{ 'p',		print_region },
	{ 'r',		resync },
	{ 's',		save_some },
	{ 'w',		widen },
	{ '|',		pipe_region },
	{ '\0',		NULL }
};

static MAP_BINDING mail_cx_mbindings[] = {
	{ '4',		MAIL_CONTROL_X_4_MAP },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the mail control-t keymap */

static CMD_BINDING mail_ct_cbindings[] = {
	{ CTRL('G'),	kbd_quit },
	{ CTRL('K'),	kill_tagset },
	{ CTRL('R'),	st_back },
	{ CTRL('S'),	st_fwd },
	{ '+',		save_tagset },
	{ '?',		msg_tags },
	{ 'n',		nrw_tagset },
	{ 'p',		print_tagset },
	{ 'r',		rm_tags },
	{ 's',		se_tag },
	{ 't',		tag_msg },
	{ 'u',		untag_msg },
	{ 'w',		copy_tagset },
	{ '|',		pipe_tagset },
	{ '\0',		NULL },
};

/****************************************************************************/
/* The default bindings for the mail control-x 4 keymap */

static CMD_BINDING mail_cx4_cbindings[] = {
	{ CTRL('F'),	owin_find },
	{ CTRL('G'),	kbd_quit },
	{ 'b',		owin_switch },
	{ 'f',		owin_find },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the mail pop3 keymap */

static CMD_BINDING pop3_cbindings[] = {
	{ CTRL('K'),	del_msg },
	{ CTRL('Y'),	undel_msg }
};

/****************************************************************************/
/* The default bindings for the typeout keymap */

static CMD_BINDING type_cbindings[] = {
	{ CTRL('?'),	down_scroll },
	{ CTRL('G'),	kbd_quit },
	{ ' ',		to_scroll },
	{ '<',		buf_start },
	{ '>',		buf_end },
	{ 'n',		next_line },
	{ 'p',		prev_line },
	{ 'q',		kbd_quit },
	{ '\0',		NULL }
};

static MAP_BINDING type_mbindings[] = {
	{ CTRL('C'),	TYPEOUT_CONTROL_C_MAP },
	{ CTRL('X'),	TYPEOUT_CONTROL_X_MAP },
	{ CTRL('['),	TYPEOUT_PREFIX_MAP },
	{ '\0',		NULL }
};
/****************************************************************************/
/* The default bindings for the minibuffer keymap */

static CMD_BINDING mbuf_cbindings[] = {
	{ CTRL('A'),	start_of_line },
	{ CTRL('B'),	back_char },
	{ CTRL('D'),	del_fwd },
	{ CTRL('E'),	end_of_line },
	{ CTRL('F'),	fwd_char },
	{ CTRL('I'),	self_insert },
	{ CTRL('M'),	newline },
	{ CTRL('Q'),	quote_char },
	{ CTRL('T'),	transpose },
	{ CTRL('^'),	quote_char },
	{ ' ',		self_insert },
	{ '!',		self_insert },
	{ '"',		self_insert },
	{ '#',		self_insert },
	{ '$',		self_insert },
	{ '%',		self_insert },
	{ '&',		self_insert },
	{ '\'',		self_insert },
	{ '(',		self_insert },
	{ ')',		self_insert },
	{ '*',		self_insert },
	{ '+',		self_insert },
	{ ',',		self_insert },
	{ '-',		self_insert },
	{ '.',		self_insert },
	{ '/',		self_insert },
	{ '0',		self_insert },
	{ '1',		self_insert },
	{ '2',		self_insert },
	{ '3',		self_insert },
	{ '4',		self_insert },
	{ '5',		self_insert },
	{ '6',		self_insert },
	{ '7',		self_insert },
	{ '8',		self_insert },
	{ '9',		self_insert },
	{ ':',		self_insert },
	{ ';',		self_insert },
	{ '<',		self_insert },
	{ '=',		self_insert },
	{ '>',		self_insert },
	{ '?',		self_insert },
	{ '@',		self_insert },
	{ '[',		self_insert },
	{ '\\',		self_insert },
	{ ']',		self_insert },
	{ '^',		self_insert },
	{ '_',		self_insert },
	{ '`',		self_insert },
	{ 'a',		self_insert },
	{ 'b',		self_insert },
	{ 'c',		self_insert },
	{ 'd',		self_insert },
	{ 'e',		self_insert },
	{ 'f',		self_insert },
	{ 'g',		self_insert },
	{ 'h',		self_insert },
	{ 'i',		self_insert },
	{ 'j',		self_insert },
	{ 'k',		self_insert },
	{ 'l',		self_insert },
	{ 'm',		self_insert },
	{ 'n',		self_insert },
	{ 'o',		self_insert },
	{ 'p',		self_insert },
	{ 'q',		self_insert },
	{ 'r',		self_insert },
	{ 's',		self_insert },
	{ 't',		self_insert },
	{ 'u',		self_insert },
	{ 'v',		self_insert },
	{ 'w',		self_insert },
	{ 'x',		self_insert },
	{ 'y',		self_insert },
	{ 'z',		self_insert },
	{ '{',		self_insert },
	{ '|',		self_insert },
	{ '}',		self_insert },
	{ '~',		self_insert },
	{ CTRL('?'),	del_back },
	{ '\0',		NULL }
};

static MAP_BINDING mbuf_mbindings[] = {
	{ CTRL('C'),	MBUF_CONTROL_C_MAP },
	{ CTRL('X'),	MBUF_CONTROL_X_MAP },
	{ CTRL('['),	MBUF_PREFIX_MAP },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the minibuffer prefix keymap */

static CMD_BINDING mbuf_pfx_cbindings[] = {
	{ 'b',		back_word },
	{ 'c',		cap_word },
	{ 'd',		fwd_kill_word },
	{ 'f',		fwd_word },
	{ 'l',		dcase_word },
	{ 'u',		ucase_word },
	{ CTRL('?'),	back_kill_word },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The default bindings for the minibuffer completion keymap */

static CMD_BINDING comp_cbindings[] = {
	{ CTRL('I'),	mb_complete },
	{ CTRL('M'),	mb_exit_complete },
	{ ' ',		mb_word_complete },
	{ '?',		mb_list_completions },
	{ '\0',		NULL }
};

/****************************************************************************/
/* The general structure for default bindings */

KEYMAP_DEFAULTS def_keymaps[] = {
	{ GLOBAL_MAP, global_cbindings, global_mbindings },
	{ HELP_MAP, help_cbindings, NULL },
	{ PREFIX_MAP, pfx_cbindings, NULL },
	{ CONTROL_X_MAP, cx_cbindings, NULL },
	{ CONTROL_C_MAP, NULL, NULL },
	{ MAIL_MAP, mail_cbindings, mail_mbindings },
	{ MAIL_PREFIX_MAP, mail_pfx_cbindings, NULL },
	{ MAIL_CONTROL_T_MAP, mail_ct_cbindings, NULL },
	{ MAIL_CONTROL_X_MAP, mail_cx_cbindings, mail_cx_mbindings },
	{ MAIL_CONTROL_X_4_MAP, mail_cx4_cbindings, NULL },
	{ MAIL_CONTROL_C_MAP, NULL, NULL },
	{ POP3_MAP, pop3_cbindings, NULL },
	{ TYPEOUT_MAP, type_cbindings, type_mbindings },
	{ TYPEOUT_PREFIX_MAP, NULL, NULL },
	{ TYPEOUT_CONTROL_X_MAP, NULL, NULL },
	{ TYPEOUT_CONTROL_C_MAP, NULL, NULL },
	{ MBUF_MAP, mbuf_cbindings, mbuf_mbindings },
	{ MBUF_PREFIX_MAP, mbuf_pfx_cbindings, NULL },
	{ MBUF_CONTROL_X_MAP, NULL, NULL },
	{ MBUF_CONTROL_C_MAP, NULL, NULL },
	{ COMPLETE_MAP, comp_cbindings, NULL },
	{ NULL, NULL, NULL }
};

/****************************************************************************/
/* The structure for a binding's mode indicators */

typedef struct {
	char *left, *right;		/* Left and right indicators */
} INDICATOR;

/* The possible values of the indicator */

#define MI_GLOBAL	0
#define MI_MAIL		1
#define MI_TYPEOUT	2
#define MI_MBUF		3

/* The characters they indicate */

static INDICATOR indicators[] = {
	{ "", "" },
	{ "[", "]" },
	{ "{", "}" },
	{ "(", ")" }
};

/****************************************************************************/
