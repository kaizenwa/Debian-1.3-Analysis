/*
 * struct.h: header file for structures needed for prototypes
 *
 * Written by Scott Reynolds, based on code by Michael Sandrof
 *
 * Copyright(c) 1995
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: struct.h,v 1.7.2.1 1996/05/27 16:26:23 mrg Exp $
 */

#ifndef __struct_h_
#define	__struct_h_

/*
 * ctcp_entry: the format for each ctcp function.   note that the function
 * described takes 4 parameters, a pointer to the ctcp entry, who the message
 * was from, who the message was to (nickname, channel, etc), and the rest of
 * the ctcp message.  it can return null, or it can return a malloced string
 * that will be inserted into the oringal message at the point of the ctcp.
 * if null is returned, nothing is added to the original message
 */
typedef	struct _ctcp_entry
{
	char	*name;		/* name of ctcp datatag */
	char	*desc;		/* description returned by ctcp clientinfo */
	int	flag;
	char	*(*func) _((struct _ctcp_entry *, char *, char *, char *));	/* function that does the dirty deed */
}	CtcpEntry;

typedef	struct	DCC_struct
{
	unsigned	flags;
	int	read;
	int	write;
	int	file;
	off_t	filesize;
	char	*description;
	char	*user;
	char	*othername;
	struct DCC_struct	*next;
	struct	in_addr	remote;
	u_short	remport;
	off_t	bytes_read;
	off_t	bytes_sent;
	time_t	lasttime;
	time_t	starttime;
	char	*buffer;
	char	talkchars[3];
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
	int	chanop;			/* True if the given nick has chanop */
}	NickList;

typedef	struct	DisplayStru
{
	char	*line;
	int	linetype;
	struct	DisplayStru	*next;
}	Display;

typedef	struct	WindowStru
{
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
	int	old_size;		/* if new_size != display_size,
					 * resize_display is called */
	int	visible;		/* true, window is drawn... false window is
					 * hidden */
	int	update;			/* window needs updating flag */
	unsigned miscflags;		/* Miscellaneous flags. */

	char	*prompt;		/* A prompt string, usually set by EXEC'd process */
	char	*status_line[2];	/* The status lines string */

	int	double_status;		/* Display the 2nd status line ?*/

	Display	*top_of_display,	/* Pointer to first line of display structure */
		*display_ip;		/* Pointer to insertiong point of display
					 * structure */

	char	*current_channel;	/* Window's current channel */
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
	int	lastlog_level;		/* The LASTLOG_LEVEL, determines what
					 * messages go to lastlog */
	int	lastlog_size;		/* Max number of messages for the window
					 * lastlog */

	int	notify_level;		/* the notify level.. */

	char	*logfile;		/* window's logfile name */
	/* window log stuff */
	int	log;			/* true, file logging for window is on */
	FILE	*log_fp;		/* file pointer for the log file */

	struct	ScreenStru	*screen;
	int	server_group;		/* server group number */

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

	int	meta1_hit;			/* if meta1 is hit in this
						 * screen or not */
	int	meta2_hit;			/* above, for meta2 */
	int	meta3_hit;			/* above, for meta3 */
	int	meta4_hit;			/* above, for meta4 */
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

/* ChannelList: structure for the list of channels you are current on */
typedef	struct	channel_stru
{
	struct	channel_stru	*next;	/* pointer to next channel entry */
	char	*channel;		/* channel name */
	int	server;			/* server index for this channel */
	u_long	mode;			/* Current mode settings for channel */
	u_long	i_mode;			/* channel mode for cached string */
	char	*s_mode;		/* cached string version of modes */
	int	limit;			/* max users for the channel */
	int	i_limit;		/* max users in cached string */
	char	*key;			/* key for this channel */
	char	chop;			/* true if you are chop */
	char	bound;			/* true if channel is bound */
	char	connected;		/* true if this channel is actually connected */
	Window	*window;		/* the window that the channel is "on" */
	NickList	*nicks;		/* pointer to list of nicks on channel */
}	ChannelList;

typedef	struct	list_stru
{
	struct	list_stru	*next;
	char	*name;
}	List;
#endif /* __struct_h_ */
