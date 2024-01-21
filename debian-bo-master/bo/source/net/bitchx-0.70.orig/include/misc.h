/* 
 * Copyright Colten Edwards 1997.
 * various miscellaneous routines needed for irc functions
 */
 
#ifndef _misc_h
#define _misc_h

#define KICKLIST		0x01
#define LEAVELIST		0x02
#define JOINLIST		0x03
#define CHANNELSIGNOFFLIST	0x04
#define PUBLICLIST		0x05
#define PUBLICOTHERLIST		0x06
#define PUBLICNOTICELIST	0x07
#define NOTICELIST		0x08
#define TOPICLIST		0x09
#define MODEOPLIST		0x0a
#define MODEDEOPLIST		0x0b
#define MODEBANLIST		0x0c
#define MODEUNBANLIST		0x0d
#define NICKLIST		0x0e

enum color_attributes {	
	BLACK = 0, BLUE, GREEN, CYAN, RED, MAGENTA, YELLOW, WHITE, 
	BLACKB, BLUEB, GREENB, CYANB, REDB, MAGENTAB, YELLOWB, WHITEB,NO_COLOR, 
	BACK_BLACK, BACK_RED, BACK_GREEN, BACK_YELLOW,
	BACK_BLUE, BACK_MAGENTA, BACK_CYAN, BACK_WHITE, 
	BACK_BBLACK, BACK_BRED, BACK_BGREEN, BACK_BYELLOW,
	BACK_BBLUE, BACK_BMAGENTA, BACK_BCYAN, BACK_BWHITE, 
	REVERSE_COLOR, BOLD_COLOR, BLINK_COLOR, UNDERLINE_COLOR
};

#define DONT_CARE 3	
#define NEED_OP 1
#define NO_OP 0
	
extern  CloneList *clones;
extern char *color_str[];
extern	int	split_watch;
void	clear_link _((irc_server **));
extern  irc_server *tmplink, *server_last;

#define MAX_LAST_MSG 10
extern LastMsg last_msg[MAX_LAST_MSG];
extern LastMsg last_notice[MAX_LAST_MSG];
extern LastMsg last_servermsg[MAX_LAST_MSG];



	void	update_stats	_((int, char *, char *, ChannelList *, int));
	int	check_split	_((char *, char *, char *));
	void	userage		_((char *, char *));
	void	stats_k_grep_end _((void));
	char	*stripansicodes _((char *));
	char	*stripansi	_((char *));
	char 	*gettabkey _((int, int, char *));
	void	addtabkey _((char *, int));
	void	clear_array _((NickTab **));
	char	*random_str _((int, int));
	int	check_serverlag _((void *));
	void	do_clones _((fd_set *, fd_set *));
	void	auto_away _((unsigned long));
ChannelList *	prepare_command _((int *, char *, int));
	int	rename_file _((char *, char **));
	void	putlog _((int, ...));

	void	add_mode_buffer _(( char *, int));
	void	flush_mode _((ChannelList *));
	void	flush_mode_all _((ChannelList *));
	void	add_mode _((ChannelList *, char *, int, char *, char *, int));
	int	delay_flush_all _((void *));
	char	*clear_server_flags _((char *));
	char	*ban_it _((char *, char *, char *));	

	int	logmsg _((int, unsigned long, char *, char *, char *, char *, int));
	void	log_toggle _((int));

	char	*do_nslookup _((char *));
	void	do_newuser _((char *, char *, char *));
	char	*cluster _((char *));
	int	caps_fucknut _((char *));

	void    do_reconnect _((char *));
	void    userhost_nsl _((WhoisStuff *, char *, char *));
	void	reset _((char *, char *, char *));
	void    newnick _((char *, char *, char *));
	void    cycle _((char *, char *, char *));
	void    newuser _((char *, char *, char *));
	void    bomb _((char *, char *, char *));
	void    do_flood _((char *, char *, char *));
	void    nslookup _((char *, char *, char *));
	void    finger _((char *, char *, char *));
	void	start_finger _((WhoisStuff *, char *, char *));

	void	users _((char *, char *, char *));

	int	are_you_opped _((char *));
	void	error_not_opped _((char *));
	void	not_on_a_channel _((Window *));

	char	*get_reason _((char *));
	char	*get_signoffreason _((char *));
	int	isme _((char *));

	char 	*convert_output_format _((char *, char *, ...));
	void	add_last_type _((LastMsg *, char *, char *, char *, char *));
	int	check_last_type _((LastMsg *, char *, char *));
	int	matchmcommand _((char *, int));
	char	*convert_time _((time_t));
	void	set_socket_read _((fd_set *, fd_set *));
	void	scan_sockets _((fd_set *, fd_set *));
#endif
