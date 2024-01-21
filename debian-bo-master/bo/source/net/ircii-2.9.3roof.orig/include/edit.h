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

extern	char	*sent_nick;
extern	char	*sent_body;
extern	char	*recv_nick;

	void	load _((char *, char *, char *));
	void	send_text _((char *, char *, char *));
	void	eval_inputlist _((char *, char *));
	void	parse_command _((char *, int, char *));
	void	parse_line _((char *, char *, char *, int, int));
	void	edit_char _((unsigned char));
	void	execute_timer _((void));
	void	ison_now _((WhoisStuff *, char *, char *));
	void	query _((char *, char *, char *));
	void	forward_character _((unsigned char, char *));
	void	backward_character _((unsigned char, char *));
	void	forward_history _((unsigned char, char *));
	void	backward_history _((unsigned char, char *));
	void	toggle_insert_mode _((unsigned char, char *));
	void	send_line _((unsigned char, char *));
	void	meta1_char _((unsigned char, char *));
	void	meta2_char _((unsigned char, char *));
	void	meta3_char _((unsigned char, char *));
	void	meta4_char _((unsigned char, char *));
	void	quote_char _((unsigned char, char *));
	void	type_text _((unsigned char, char *));
	void	parse_text _((unsigned char, char *));
	void	irc_clear_screen _((unsigned char, char *));
	void	command_completion _((unsigned char, char *));
	int	check_wait_command _((char *));
	
#define AWAY_ONE 0
#define AWAY_ALL 1

#define STACK_POP 0
#define STACK_PUSH 1
#define STACK_SWAP 2

/* a structure for the timer list */
typedef struct	timerlist_stru
{
	int	ref;
	int	in_on_who;
	time_t	time;
	char	*command;
	struct	timerlist_stru *next;
}	TimerList;

extern TimerList *PendingTimers;

#endif /* __edit_h_ */
