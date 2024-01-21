/*
 * edit.h: header for edit.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: edit.h,v 1.16 1995/09/21 04:16:39 mrg Exp $
 */

#ifndef __edit_h_
#define __edit_h_

#include "irc_std.h"

extern	char	*sent_nick;
extern	char	*sent_body;
extern	char	*recv_nick;

	void	load _((char *, char *, char *));
	void	send_text _((char *, char *, char *, int, int));
	void	eval_inputlist _((char *, char *));
	int	parse_command _((char *, int, char *));
	void	parse_line _((char *, char *, char *, int, int));
	void	edit_char _((unsigned char));
	void	execute_timer _((void));
	void	ison_now _((char *, char *));
	void	query _((char *, char *, char *));
	void	quote_char _((char, char *));
	void	type_text _((char, char *));
	void	parse_text _((char, char *));
	void	irc_clear_screen _((char, char *));
	int	check_wait_command _((char *));
	void	ExecuteTimers _((void));
	void	my_clear _((char *, char *, char *));
	void	reconnect_cmd _((char *, char *, char *));			
extern	void	e_hostname _((char *, char *, char *));
	int	check_mode_lock _((char *, char *, int));
	void	away _((char *, char *, char *));
	void	e_quit _((char *, char *, char *));
	void	destroy_call_stack _((void));
	void	panic_dump_call_stack _((void));
	void	dump_call_stack _((void));
	void	unwind_stack _((void));
	void	wind_stack _((char *));
		
		
#define AWAY_ONE 0
#define AWAY_ALL 1

#define STACK_POP 0
#define STACK_PUSH 1
#define STACK_SWAP 2

#ifdef __STDC__
#define BUILT_IN_COMMAND(x) \
	void x (char *command, char *args, char *subargs)
#else
#define BUILT_IN_COMMAND(x) \
	void x (command, args, subargs) char *command, *args, *subargs;
#endif

extern IrcCommandDll *dll_commands;

#endif /* __edit_h_ */
