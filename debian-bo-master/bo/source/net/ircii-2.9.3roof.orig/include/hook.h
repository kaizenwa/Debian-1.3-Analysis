/*
 * hook.h.proto: header for hook.c
 * 
 * Generated from hook.h.proto automatically by the Makefile
 *
 * @(#)$Id: hook.h,v 1.4 1995/11/04 13:51:06 mrg Exp $
 */

#ifndef __hook_h_
# define __hook_h_

/* Hook: The structure of the entries of the hook functions lists */
typedef struct	hook_stru
{
	struct	hook_stru *next;	/* pointer to next element in list */
	char	*nick;			/* The Nickname */
	int	not;			/* If true, this entry should be
					 * ignored when matched, otherwise it
					 * is a normal entry */
	int	noisy;			/* flag indicating how much output
					 * should be given */
	int	server;			/* the server in which this hook
					 * applies. (-1 if none). If bit 0x1000
					 * is set, then no other hooks are
					 * tried in the given server if all the
					 * server specific ones fail
					 */
	int	sernum;			/* The serial number for this hook. This
					 * is used for hooks which will be
					 * concurrent with others of the same
					 * pattern. The default is 0, which
					 * means, of course, no special
					 * behaviour. If any 1 hook suppresses
					 * the * default output, output will be
					 * suppressed.
					 */
	char	*stuff;			/* The this that gets done */
	int	global;			/* set if loaded from `global' */
}	Hook;

/* HookFunc: A little structure to keep track of the various hook functions */
typedef struct
{
	char	*name;			/* name of the function */
	Hook	*list;			/* pointer to head of the list for this
					 * function */
	int	params;			/* number of parameters expected */
	int	mark;
	unsigned flags;
}	HookFunc;

/*
 * NumericList: a special list type to dynamically handle numeric hook
 * requests 
 */
typedef struct numericlist_stru
{
	struct	numericlist_stru *next;
	char	*name;
	Hook	*list;
}	NumericList;

#define	ACTION_LIST 0
#define	CHANNEL_NICK_LIST 1
#define CHANNEL_SIGNOFF_LIST 2
#define CONNECT_LIST 3
#define CTCP_LIST 4
#define CTCP_REPLY_LIST 5
#define	DCC_CHAT_LIST 6
#define DCC_CONNECT_LIST 7
#define DCC_ERROR_LIST 8
#define DCC_LOST_LIST 9
#define	DCC_RAW_LIST 10
#define DCC_REQUEST_LIST 11
#define DISCONNECT_LIST 12
#define ENCRYPTED_NOTICE_LIST 13
#define ENCRYPTED_PRIVMSG_LIST 14
#define EXEC_LIST 15
#define EXEC_ERRORS_LIST 16
#define EXEC_EXIT_LIST 17
#define EXEC_PROMPT_LIST 18
#define EXIT_LIST 19
#define FLOOD_LIST 20
#define HELP_LIST 21
#define	HOOK_LIST 22
#define IDLE_LIST 23
#define INPUT_LIST 24
#define INVITE_LIST 25
#define JOIN_LIST 26
#define LEAVE_LIST 27
#define LIST_LIST 28
#define MAIL_LIST 29
#define MODE_LIST 30
#define MSG_LIST 31
#define MSG_GROUP_LIST 32
#define NAMES_LIST 33
#define NICKNAME_LIST 34
#define NOTE_LIST 35
#define NOTICE_LIST 36
#define NOTIFY_SIGNOFF_LIST 37
#define NOTIFY_SIGNON_LIST 38
#define PUBLIC_LIST 39
#define PUBLIC_MSG_LIST 40
#define PUBLIC_NOTICE_LIST 41
#define PUBLIC_OTHER_LIST 42
#define	RAW_IRC_LIST 43
#define	SEND_ACTION_LIST 44
#define	SEND_DCC_CHAT_LIST 45
#define SEND_MSG_LIST 46
#define SEND_NOTICE_LIST 47
#define SEND_PUBLIC_LIST 48
#define	SEND_TALK_LIST 49
#define	SERVER_NOTICE_LIST 50
#define SIGNOFF_LIST 51
#define	TALK_LIST 52
#define TIMER_LIST 53
#define TOPIC_LIST 54
#define WALL_LIST 55
#define WALLOP_LIST 56
#define WHO_LIST 57
#define WIDELIST_LIST 58
#define WINDOW_LIST 59
#define WINDOW_KILL_LIST 60
#define KICK_LIST 61

#ifdef ON_KICK
# define NUMBER_OF_LISTS KICK_LIST + 1
#else
# define NUMBER_OF_LISTS WINDOW_KILL_LIST + 1
#endif 

#ifdef HAVE_STDARG_H
	int	do_hook _((int, char *, ...));
#else
	int	do_hook _(());
#endif /* HAVE_STDARG_H */
	void	on _((char *, char *, char *));
	void	save_hooks _((FILE *, int));
	void	remove_hook _((int, char *, int, int, int));
	void	show_hook _((Hook *, char *));

extern	char	*hook_info;
extern	NumericList *numeric_list;
extern	HookFunc hook_functions[];

extern	int	in_on_who;

#endif /* __hook_h_ */
