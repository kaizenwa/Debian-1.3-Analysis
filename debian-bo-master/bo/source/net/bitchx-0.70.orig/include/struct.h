/*
 * struct.h: header file for structures needed for prototypes
 *
 * Written by Scott Reynolds, based on code by Michael Sandrof
 * Heavily modified by Colten Edwards for BitchX
 *
 * Copyright(c) 1997
 *
 */

#ifndef __struct_h_
#define	__struct_h_


typedef struct 
{
	int is_open;
	void (*func) _((int));
} SocketList;

typedef char *(bf) _((char *));
typedef struct 
{
	char    *name;
	bf      *func;
}       BuiltInFunctions;

typedef struct _BuiltInDllFunctions
{
	struct _BuiltInDllFunctions *next;
	char    *name;
	bf      *func;
}       BuiltInDllFunctions;
                
typedef struct _NumericFunction
{
	struct _NumericFunction *next;
	char	*name;
	int	number;
	int	(*func) _((char *, char *, char **));
}       NumericFunction;
                
/* IrcCommand: structure for each command in the command table */
typedef	struct
{
	char	*name;					/* what the user types */
	char	*server_func;				/* what gets sent to the server
							 * (if anything) */
	void	(*func) _((char *, char *, char *));	/* function that is the command */
	unsigned	flags;
}	IrcCommand;


typedef	struct _IrcCommandDll
{
	struct  _IrcCommandDll *next;			/* pointer to next record. */
	char	*name;					/* what the user types */
	char	*server_func;				/* what gets sent to the server
							 * (if anything) */
	void	(*func) _((struct _IrcCommandDll *, char *, char *, char *));	/* function that is the command */
	unsigned	flags;
	char	*result;
}	IrcCommandDll;

typedef struct _last_msg_stru
{
	struct _last_msg_stru *next;
	char    *from;
	char	*uh;
	char	*to;
	char    *last_msg;
	char	*time;
	int     count;
} LastMsg;
                                
typedef struct  userlist_stru
{
	struct	userlist_stru	*next;	/* pointer to next user entry */
	char	*nick;			/* user's name in nick!user@host */
	char	*host;
	int	level;			/* users level */
	int	aop;			/* Should I aop? */
	int	prot;			/* users prot level */
	char	*channels;		/* channel for list to take effect */
	char	*password;		/* users password */
	time_t	time;			/* time when put on list */
}	UserList;

/* ShitList: structure proto for the shitlist */
typedef struct  shitlist_stru
{
	struct	shitlist_stru	*next;	/* pointer to next shit entry */
	char	*filter;		/* filter in nick!user@host */
	int	level;			/* level of shitted */
	char	*channels;		/* channel for list to take effect */
	char	*reason;		/* Reason */
	time_t	time;			/* time shit was put on */
}	ShitList;

/* WordKickList: structure for your wordkick list */
typedef struct  wordkicklist_stru
{
	struct	wordkicklist_stru *next;	/* pointer to next user entry */
	char	*string;			/* string */
	char	*channel;			/* channel */
}	WordKickList;

/* LameList: structure for the users on your LameNick Kick*/
typedef struct  lamelist_stru
{
	struct	lamelist_stru	*next;	/* pointer to next lame entry */
	char	*nick;			/* Lame Nick */
}	LameList;

/* NotifyList: structure for the users on your Notify list */
typedef struct  mynotifylist_stru
{
	struct	mynotifylist_stru	*next;	/* pointer to next notify entry */
	char    *nick;				/* Notify Nick */
}	MyNotifyList;

/* invitetoList: structure for the invitetolist list */
typedef struct  invitetolist_stru
{
	struct	invitetolistlist_stru	*next;	/* pointer to next entry */
	char    *channel;			/* channel */
	int     times;				/* times I have been invited */
	time_t  time;				/* time of last invite */
}	InviteToList;

typedef struct server_split
{
	struct server_split *next;
	char *name;	/* name of this server. */
	char *link;	/* linked to what server */
	int status;	/* split or not */
	int count;	/* number of times we have not found this one */
	int hopcount; 	/* number of hops away */
	char *time; 	/* time of split */
} irc_server;

/*
 * ctcp_entry: the format for each ctcp function.   note that the function
 * described takes 4 parameters, a pointer to the ctcp entry, who the message
 * was from, who the message was to (nickname, channel, etc), and the rest of
 * the ctcp message.  it can return null, or it can return a malloced string
 * that will be inserted into the oringal message at the point of the ctcp.
 * if null is returned, nothing is added to the original message

 */
struct _CtcpEntry;

typedef char *(*CTCP_Handler) _((struct _CtcpEntry *, char *, char *, char *));

typedef	struct _CtcpEntry
{
	char		*name;  /* name of ctcp datatag */
	int		id;	/* index of this ctcp command */
	int		flag;	/* Action modifiers */
	char		*desc;  /* description returned by ctcp clientinfo */
	CTCP_Handler 	func;	/* function that does the dirty deed */
	CTCP_Handler 	repl;	/* Function that is called for reply */
}	CtcpEntry;

struct _CtcpEntryDll;

typedef char *(*CTCP_DllHandler) _((struct _CtcpEntryDll *, char *, char *, char *));

typedef	struct _CtcpEntryDll
{
	struct		_CtcpEntryDll *next;
	char		*name;  /* name of ctcp datatag */
	int		id;	/* index of this ctcp command */
	int		flag;	/* Action modifiers */
	char		*desc;  /* description returned by ctcp clientinfo */
	CTCP_DllHandler	func;	/* function that does the dirty deed */
	CTCP_DllHandler	repl;	/* Function that is called for reply */
}	CtcpEntryDll;




struct transfer_struct {
	unsigned short packet_id;
	unsigned char byteorder;
	u_32int_t byteoffset;
}; 


typedef struct _File_Stat {
	struct _File_Stat *next;
	char *filename;
	long filesize;
} FileStat;

typedef struct _File_List {
	struct _File_List *next;
	char * description;
	char * notes;
	FileStat *filename;
	char * nick;
	int packnumber;
	int numberfiles;
	double filesize;
	double minspeed;
	int gets;
	time_t timequeue;
} FileList;


typedef	struct	DCC_struct
{
	struct DCC_struct *next;
	char		*user;
	char		*userhost;
	unsigned	flags;
	int		read;
	int		write;
	int		file;

	u_32int_t	filesize;

	int		dccnum;
	int		eof;
	char		*description;
	char		*othername;
	struct in_addr	remote;
	u_short		remport;
	u_32int_t	bytes_read;
	u_32int_t	bytes_sent;

	int				window_sent;
	int				window_max;

	int		in_dcc_chat;
	int		echo;
	struct timeval	lasttime;
	struct timeval	starttime;
	char		*buffer;
	char		*cksum;
	char		*encrypt;
	u_32int_t	packets_total;
	u_32int_t	packets_transfer;
	struct transfer_struct transfer_orders;
}	DCC_list;

/* Hold: your general doubly-linked list type structure */

typedef struct HoldStru
{
	char	*str;
	struct	HoldStru	*next;
	struct	HoldStru	*prev;
	int	logged;
}	Hold;

typedef struct	lastlog_stru
{
	int	level;
	char	*msg;
	struct	lastlog_stru	*next;
	struct	lastlog_stru	*prev;
}	Lastlog;

struct	MenuOptionTag
{
	char	*Name;
	char	*Arguments;
	void	(*Func) _((char *));
};

typedef	struct	MenuOptionTag	MenuOption;

struct	MenuTag
{
	struct	MenuTag	*next;
	char	*Name;
	int	TotalOptions;
	MenuOption	**Options;
};

typedef struct MenuTag Menu;

struct	ScreenStru;	/* ooh! */

struct	WindowMenuTag
{
	Menu	*menu;
	int	lines;
	int	items_per_line;
	int	cursor;
};

typedef	struct	WindowMenuTag	WindowMenu;



/* NickList: structure for the list of nicknames of people on a channel */
typedef struct nick_stru
{
	struct	nick_stru	*next;	/* pointer to next nickname entry */
	char	*nick;			/* nickname of person on channel */
	char	*host;

	char	*server;
	UserList *userlist;
	ShitList *shitlist;
	UserList *botlist;

	int	chanop;			/* True if the given nick has chanop */
	int	away;
	int	voice;
	int	ircop;
	
	time_t	idle_time;

	int	floodcount;
	time_t	floodtime;

	int	nickcount;
	time_t  nicktime;

	int	kickcount;
	time_t	kicktime;

	int	joincount;
	time_t	jointime;

	int	dopcount;
	time_t	doptime;


	time_t	created;

	int	stat_kicks;		/* Total kicks done by user */
	int	stat_dops;		/* Total deops done by user */
	int	stat_ops;		/* Total ops done by user */
	int	stat_bans;		/* Total bans done by user */
	int	stat_unbans;		/* Total unbans done by user */
	int	stat_nicks;		/* Total nicks done by user */
	int	stat_pub;		/* Total publics sent by user */
	int	stat_topics;		/* Total topics set by user */

	int	sent_reop;
	time_t	sent_reop_time;
	
	int	sent_deop;
	time_t	sent_deop_time;
	int	need_userhost;		/* on join we send a userhost for this nick */	
}	NickList;

typedef	struct	DisplayStru
{
	char	*line;
	int	linetype;
	struct	DisplayStru	*next;
}	Display;

typedef	struct	WindowStru
{

#define MAX_FUNCTIONS		36
	unsigned int	refnum;		/* the unique reference number,
					 * assigned by IRCII */
	char	*name;			/* window's logical name */
	int	server;			/* server index */
	int	prev_server;		/* previous server index */
	int	top;			/* The top line of the window, screen
					 * coordinates */
	int	bottom;			/* The botton line of the window, screen
					 * coordinates */
	int	cursor;			/* The cursor position in the window, window
					 * relative coordinates */
	int	line_cnt;		/* counter of number of lines displayed in
					 * window */
	int	scroll;			/* true, window scrolls... false window wraps */
	int	display_size;		/* number of lines of window - menu lines */
	int	old_size;		/* if new_size != display_size, resize_display */
	int	visible;		/* true, window ise, window is drawn... false window is hidden */
	int	update;			/* window needs updating flag */
	unsigned miscflags;		/* Miscellaneous flags. */

	char	*prompt;		/* A prompt string, usually set by EXEC'd process */
	Display	*top_of_display,	/* Pointer to first line of display structure */
		*display_ip;		/* Pointer to insertiong point of display
					 * structure */

	char	*current_channel;	/* Window's current channel */
	char	*waiting_channel;
	char	*bind_channel;
	char	*query_nick;		/* User being QUERY'ied in this window */
	NickList	*nicks;		/* List of nicks that will go to window */
	int	window_level;		/* The LEVEL of the window, determines what
					 * messages go to it */

	/* hold stuff */
	int	hold_mode;		/* true, hold mode is on for window...
					 * false it isn't */
	int	hold_on_next_rite;	/* true, the next call to rite() will
					 * activate a hold */
	int	held;			/* true, the window is currently being
					 * held */
	int	beep_always;		/* should this window beep when hidden */
	int	last_held;		/* Previous value of hold flag.  Used
					 * for various updating needs */
	Hold	*hold_head,		/* Pointer to first entry in hold
					 * list */
		*hold_tail;		/* Pointer to last entry in hold list */
	int	held_lines;		/* number of lines being held */
	int	scrolled_lines;		/* number of lines scrolled back */
	int	new_scrolled_lines;	/* number of lines since scroll back
					 * keys where pressed */
	WindowMenu	menu;		/* The menu (if any) */

	/* lastlog stuff */
	Lastlog	*lastlog_head;		/* pointer to top of lastlog list */
	Lastlog	*lastlog_tail;		/* pointer to bottom of lastlog list */
	unsigned long lastlog_level;	/* The LASTLOG_LEVEL, determines what
					 * messages go to lastlog */
	int	lastlog_size;		/* Max number of messages for the window
					 * lastlog */

	unsigned long notify_level;	/* the notify level.. */

	char	*logfile;		/* window's logfile name */
	/* window log stuff */
	int	log;			/* true, file logging for window is on */
	FILE	*log_fp;		/* file pointer for the log file */


	struct	ScreenStru	*screen;
	int	server_group;		/* server group number */


	int	window_display;		/* should we display to this window */

	char	*status_line[3];	/* The status lines string */
	int	double_status;		/* number of status lines */
	int	status_split;		/* split status to top and bottom */
		

	char	*status_format[4];	/* holds formated status info */
	char	*format_status[4];	/* holds raw format for window */	

	char    *(*status_func[4][MAX_FUNCTIONS]) _((struct WindowStru *));
	
	int	func_cnt[4];


	char	*status_mode;
	char	*status_topic;
	char	*status_umode;
	char	*status_hold_lines;
	char	*status_channel;
	char	*status_notify;
	char	*status_oper_kills;
	char	*status_lag;
	char	*status_mail;
	char	*status_query;
	char	*status_server;
	char	*status_clock;
	char	*status_users;

/* These are the various formats from a window */ 
	char    *mode_format;
	char    *umode_format;
	char    *topic_format;
	char    *query_format;
	char    *clock_format;
	char    *hold_lines_format;
	char    *channel_format;
	char    *mail_format;
	char    *server_format;
	char    *notify_format;
	char    *status_oper_kills_format;
	char    *status_users_format;
	char    *status_lag_format;
		
	struct	WindowStru	*next;	/* pointer to next entry in window list (null
					 * is end) */
	struct	WindowStru	*prev;	/* pointer to previous entry in window list
					 * (null is end) */
}	Window;

/*
 * WindowStack: The structure for the POP, PUSH, and STACK functions. A
 * simple linked list with window refnums as the data 
 */
typedef	struct	window_stack_stru
{
	unsigned int	refnum;
	struct	window_stack_stru	*next;
}	WindowStack;

typedef	struct
{
	int	top;
	int	bottom;
	int	position;
}	ShrinkInfo;

typedef struct PromptStru
{
	char	*prompt;
	char	*data;
	int	type;
	void	(*func) _((char *, char *));
	struct	PromptStru	*next;
}	WaitPrompt;


typedef	struct	ScreenStru
{
	int	screennum;
	Window	*current_window;
	unsigned int	last_window_refnum;	/* reference number of the
						 * window that was last
						 * the current_window */
	Window	*window_list;			/* List of all visible
						 * windows */
	Window	*window_list_end;		/* end of visible window
						 * list */
	Window	*cursor_window;			/* Last window to have
						 * something written to it */
	int	visible_windows;		/* total number of windows */
	WindowStack	*window_stack;		/* the windows here */

	int	meta_hit[10];
	int	quote_hit;			/* true if a key bound to
						 * QUOTE_CHARACTER has been
						 * hit. */
	int	digraph_hit;			/* A digraph key has been hit */
	int	inside_menu;			/* what it says. */

	unsigned char	digraph_first;

	struct	ScreenStru *prev;		/* These are the Screen list */
	struct	ScreenStru *next;		/* pointers */

	FILE	*fpin;				/* These are the file pointers */
	int	fdin;				/* and descriptions for the */
	FILE	*fpout;				/* screen's input/output */
	int	fdout;
#ifdef WINNT
	HANDLE hStdin,hStdout;
#endif

	char	input_buffer[INPUT_BUFFER_SIZE+1];	/* the input buffer */
	int	buffer_pos;			/* and the positions for the */
	int	buffer_min_pos;			/* screen */

	char	saved_input_buffer[INPUT_BUFFER_SIZE+1];
	int	saved_buffer_pos;
	int	saved_min_buffer_pos;

	WaitPrompt	*promptlist;

	char	*redirect_name;
	char	*redirect_token;
	int	redirect_server;

	char	*tty_name;
	int	co;
	int	li;

	int	alive;
}	Screen;

/* BanList: structure for the list of bans on a channel */
typedef struct ban_stru
{
	struct	ban_stru	*next;  /* pointer to next ban entry */
	char	*ban;			/* the ban */
	char	*setby;			/* person who set the ban */
	int	sent_unban;		/* flag if sent unban or not */
	time_t	sent_unban_time;	/* sent unban's time */
	time_t	time;			/* time ban was set */
	int	count;
}	BanList;

typedef struct chan_flags_stru {

	unsigned int got_modes : 1;
	unsigned int got_who : 1;
	unsigned int got_bans : 1;

} chan_flags;

/* ChannelList: structure for the list of channels you are current on */
typedef	struct	channel_stru
{
	struct	channel_stru	*next;	/* pointer to next channel entry */
	char	*channel;		/* channel name */
	int	server;			/* server index for this channel */
	u_long	mode;			/* Current mode settings for channel */
	u_long	i_mode;			/* channel mode for cached string */
	char	*s_mode;		/* cached string version of modes */
	char	*topic;
	int	topic_lock;
		
	char 	*modelock_key;
	long	modelock_val;
	
	int	limit;			/* max users for the channel */
	char	*key;			/* key for this channel */
	char	chop;			/* true if you are chop */
	char	voice;			/* true if you are voice */
	char	bound;			/* true if channel is bound */
	char	connected;		/* true if this channel is actually connected */

	Window	*window;		/* the window that the channel is "on" */

	NickList	*nicks;		/* pointer to list of nicks on channel */
	chan_flags	flags;


	time_t	max_idle;		/* max idle time for this channel */
	int	tog_limit;
	int	check_idle;		/* toggle idle check */
	int	do_scan;		/* flag for checking auto stuff */
	struct timeval	channel_create;		/* time for channel creation */
	struct timeval	join_time;		/* time of last join */


	int	stats_ops;		/* total ops I have seen in channel */
	int	stats_dops;		/* total dops I have seen in channel */
	int	stats_bans;		/* total bans I have seen in channel */
	int	stats_unbans;		/* total unbans I have seen in channel */

	int	stats_sops;		/* total server ops I have seen in channel */
	int	stats_sdops;		/* total server dops I have seen in channel */
	int	stats_sbans;		/* total server bans I have seen in channel */
	int	stats_sunbans;		/* total server unbans I have seen in channel */

	int	stats_topics;		/* total topics I have seen in channel */
	int	stats_kicks;		/* total kicks I have seen in channel */
	int	stats_pubs;		/* total pubs I have seen in channel */
	int	stats_parts;		/* total parts I have seen in channel */
	int	stats_signoffs;		/* total signoffs I have seen in channel */
	int	stats_joins;		/* total joins I have seen in channel */

	int	set_aop;		/* channel specific /set */
	int	set_ainv;		/* channel specific /set */
	int	set_auto_rejoin;	/* channel specific /set */
	int	set_deop_on_deopflood;	/* channel specific /set */
	int	set_deop_on_kickflood;	/* channel specific /set */
	int	set_deopflood;		/* channel specific /set */
	int	set_deopflood_time;	/* channel specific /set */
	int	set_hacking;		/* channel specific /set */
	int	set_kick_on_deopflood;	/* channel specific /set */
	int	set_kick_on_joinflood;
	int	set_kick_on_kickflood;	/* channel specific /set */
	int	set_kick_on_nickflood;	/* channel specific /set */
	int	set_kick_on_pubflood;	/* channel specific /set */
	int	set_kickflood;		/* channel specific /set */
	int	set_kickflood_time;	/* channel specific /set */
	int	set_nickflood;		/* channel specific /set */
	int	set_nickflood_time;	/* channel specific /set */
	int	set_joinflood;		/* channel specific /set */
	int	set_joinflood_time;	/* channel specific /set */
	int	set_pubflood;		/* channel specific /set */
	int	set_pubflood_ignore;	/* channel ignore time val */
	int	set_pubflood_time;	/* channel specific /set */
	int	set_userlist;		/* channel specific /set */
	int	set_shitlist;		/* channel specific /set */
	int	set_lamelist;		/* channel specific /set */
	int	set_kick_if_banned;     /* channel specific /set */
	int	bitch_mode;		/* channel specific /set */
	
	int	totalnicks;		/* total number of users in channel */
	int	maxnicks;		/* max number of users I have seen */
	time_t	maxnickstime;		/* time of max users */

	int	totalbans;		/* total numbers of bans on channel */


	BanList	*bans;			/* pointer to list of bans on channel */
	int	maxbans;		/* max number of bans I have seen */
	time_t	maxbanstime;		/* time of max bans */
	struct {
		char 	*op;
		int	type;
	} cmode[4];

	char	*mode_buf;
	int	mode_len;	

}	ChannelList;

typedef	struct	list_stru
{
	struct	list_stru	*next;
	char	*name;
}	List;

typedef	struct	_ajoin_list
{
	struct	_ajoin_list *next;
	char	*name;
	char	*key;
	int	server;
	int	window;
	int	ajoin_list;
}	AJoinList;

/* a structure for the timer list */
typedef struct	timerlist_stru
{
	char	ref[REFNUM_MAX + 1];
	unsigned long refno;
	int	in_on_who;
	time_t	time;
	int	(*callback) _((void *));
	char	*command;
	char	*subargs;
	struct	timerlist_stru *next;
}	TimerList;

extern TimerList *PendingTimers;
typedef struct nicktab_stru
{
	struct nicktab_stru *next;
	char *nick;
} NickTab;

typedef struct clonelist_stru
{
	struct clonelist_stru *next;
	char *number;
	char *server;
	int  port;
	int  socket_num;
	int  warn;
} CloneList;

typedef struct IgnoreStru
{
	struct IgnoreStru *next;
	char *nick;
	int type;
	int dont;
	int high;
	int num;
	char *pre_msg_high;
	char *pre_nick_high;
	char *post_high;
} Ignore;

/* IrcVariable: structure for each variable in the variable table */
typedef struct
{
	char	*name;			/* what the user types */
	int	type;			/* variable types, see below */
	int	integer;		/* int value of variable */
	char	*string;		/* string value of variable */
	void	(*func)(Window *, char *, int);		/* function to do every time variable is set */
	char	int_flags;		/* internal flags to the variable */
	unsigned short	flags;		/* flags for this variable */
}	IrcVariable;

#ifdef WANT_DLL
/* IrcVariableDll: structure for each variable in the dll variable table */
typedef struct _ircvariable
{
	struct _ircvariable *next;
	char	*name;			/* what the user types */
	int	type;			/* variable types, see below */
	int	integer;		/* int value of variable */
	char	*string;		/* string value of variable */
	void	(*func)(Window *, char *, int);	/* function to do every time variable is set */
	char	int_flags;		/* internal flags to the variable */
	unsigned short	flags;		/* flags for this variable */
}	IrcVariableDll;
#endif

#endif /* __struct_h_ */
