/*
 * server.h: header for server.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: server.h,v 1.31.2.1 1996/06/26 13:14:04 mrg Exp $
 */

#ifndef __server_h_
#define __server_h_
  
/* for ChannelList */
#include "names.h"

/*
 * type definition to distinguish different
 * server versions
 */
#define Server2_5	0
#define Server2_6	1
#define Server2_7	2
#define Server2_8	3
#define Server2_9	4
#define Server2_10	5
#define Server2_11	6

/* Server: a structure for the server_list */
typedef	struct
{
	char	*name;			/* the name of the server */
	char	*itsname;		/* the server's idea of its name */
	char	*password;		/* password for that server */
	int	port;			/* port number on that server */
	char	*nickname;		/* nickname for this server */
	char	*away;			/* away message for this server */
	int	operator;		/* true if operator */
	int	version;		/* the version of the server -
					 * defined above */
	char	*version_string;	/* what is says */
	int	whois;			/* true if server sends numeric 318 */
	int	flags;			/* Various flags */
	int	connected;		/* true if connection is assured */
	int	write;			/* write descriptor */
	int	read;			/* read descriptior */
	pid_t	pid;			/* process id of server */
	int	eof;			/* eof flag for server */
	int	motd;			/* motd flag (used in notice.c) */
	int	sent;			/* set if something has been sent,
					 * used for redirect */
	char	*buffer;		/* buffer of what dgets() doesn't get */
	WhoisQueue	*WQ_head;	/* WHOIS Queue head */
	WhoisQueue	*WQ_tail;	/* WHOIS Queue tail */
	WhoisStuff	whois_stuff;	/* Whois Queue current collection buffer */
	int	copy_from;		/* server to copy the channels from
					 * when (re)connecting */
	int	close_serv;		/* Server to close when we're LOGGED_IN */
	time_t	ctcp_last_reply_time;	/* used to limit flooding */
	struct in_addr local_addr;	/* ip address of this connection */
	ChannelList	*chan_list;	/* list of channels for this server */
	void	(*parse_server) _((char *));	/* pointer to parser for this server */
}	Server;

typedef struct ser_group_list
{
	struct ser_group_list	*next;
	char	*name;
	int	number;
}	SGroup;

typedef	unsigned	short	ServerType;

	int	find_server_group _((char *, int));
	char *	find_server_group_name _((int));
	void	add_to_server_list _((char *, int, char *, char *, int));
	void	build_server_list _((char *));
	int	connect_to_server _((char *, int, int));
	void	get_connected _((int));
	int	read_server_file _((void));
	void	display_server_list _((void));
	void	do_server _((fd_set *, fd_set *));
#ifdef HAVE_STDARG_H
	void	send_to_server _((char *, ...));
#else
	void	send_to_server _(());
#endif /* HAVE_STDARG_H */
	int	get_server_whois _((int));

	WhoisStuff	*get_server_whois_stuff _((int));
	WhoisQueue	*get_server_qhead _((int));
	WhoisQueue	*get_server_qtail _((int));

extern	int	save_chan_from;	/* to keep the channel list if all servers
				 * are lost */

extern	int	attempting_to_connect;
extern	int	number_of_servers;
extern	int	connected_to_server;
extern	int	never_connected;
extern	int	using_server_process;
extern	int	primary_server;
extern	int	from_server;
extern	char	*connect_next_nick;
extern	char	*connect_next_password;
extern	int	parsing_server_index;
extern	SGroup	*server_group_list;

	void	server _((char *, char *, char *));
	char	*get_server_nickname _((int));
	char	*get_server_name _((int));
	char	*get_server_itsname _((int));
	void	set_server_flag _((int, int, int));
	int	find_in_server_list _((char *, int));
	char	*create_server_list _((void));
	void	remove_from_server_list _((int));
	void	set_server_motd _((int, int));
	int	get_server_motd _((int));
	int	get_server_operator _((int));
	int	get_server_2_6_2 _((int));
	int	get_server_version _((int));
	void	close_server _((int, char *));
	void	MarkAllAway _((char *, char *));
	int	is_server_connected _((int));
	void	flush_server _((void));
	int	get_server_flag _((int, int));
	void	set_server_operator _((int, int));
	void	server_is_connected _((int, int));
	int	parse_server_index _((char *));
	void	parse_server_info _((char *, char **, char **, char **));
	void	set_server_bits _((fd_set *, fd_set *));
	void	set_server_itsname _((int, char *));
	void	set_server_version _((int, int));
	int	is_server_open _((int));
	int	get_server_port _((int));
	char	*set_server_password _((int, char *));
	void	set_server_nickname _((int, char *));
	void	set_server_2_6_2 _((int, int));
	void	set_server_qhead _((int, WhoisQueue *));
	void	set_server_qtail _((int, WhoisQueue *));
	void	set_server_whois _((int, int));
	void	close_all_server _((void));
	void	disconnectcmd _((char *, char *, char *));

	/* server_list: the list of servers that the user can connect to,etc */
	extern	Server	*server_list;

#define	USER_MODE_I	0x0001
#define	USER_MODE_W	0x0002
#define	USER_MODE_S	0x0004
#define	USER_MODE_R	0x0008
#define	SERVER_2_6_2	0x0010
#define CLOSE_PENDING	0x0020
#define LOGGED_IN	0x0040

#endif /* __server_h_ */
