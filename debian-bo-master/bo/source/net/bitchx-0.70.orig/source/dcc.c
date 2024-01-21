/*
 * dcc.c: Things dealing client to client connections. 
 *
 * Written By Troy Rollo <troy@cbme.unsw.oz.au> 
 *
 * Copyright(c) 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 * Heavily modified Colten Edwards 1996-97
 */

#include "irc.h"


#if defined(ISC30) && defined(_POSIX_SOURCE)
# undef _POSIX_SOURCE
#include <sys/stat.h>
# define _POSIX_SOURCE
#else
# include <sys/stat.h>
#endif /* ICS30 || _POSIX_SOURCE */

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdio.h>

#include <dirent.h>

#include "ctcp.h"
#include "crypt.h"
#include "cdcc.h"
#include "dcc.h"
#include "hook.h"
#include "ircaux.h"
#include "lastlog.h"
#include "newio.h"
#include "output.h"
#include "parse.h"
#include "server.h"
#include "status.h"
#include "vars.h"
#include "whois.h"
#include "window.h"
#include "ircterm.h"
#include "hook.h"
#include "misc.h"
#include "tcl_bx.h"
#include "screen.h"

#include <float.h>


static	off_t		filesize = 0;

DCC_list *	ClientList = NULL;

static	char		DCC_current_transfer_buffer[BIG_BUFFER_SIZE/4];
extern	int		dgets_errno;
extern	int		in_ctcp_flag;

#define	_1KB	(1024.0)
#define	_1MEG	(1024.0*1024.0)
#define	_1GIG	(1024.0*1024.0*1024.0)
#define	_1TER	(1024.0*1024.0*1024.0*1024.0)

#define	_GMKs(x)	( (x > _1TER) ? "Tb" : ((x > _1GIG) ? "Gb" : \
			((x > _1MEG) ? "Mb" : ((x > _1KB)? "Kb" : "bytes"))))

#define	_GMKv(x)	( (x > _1TER) ? (double)(x/_1TER) : ((x > _1GIG) ? \
			(double)(x/_1GIG) : ((x > _1MEG) ? \
			(double)(x/_1MEG) : ((x > _1KB) ? \
			(double)(x/_1KB): (double)x))) )


static	void    	add_to_dcc_buffer _((DCC_list *, char *));
static	void 		dcc_add_deadclient _((DCC_list *));
static	void 		dcc_close _((char *));
static	void		dcc_getfile _((char *));
static	int		dcc_open _((DCC_list *));
static	void 		dcc_really_erase _((void));
static	void		dcc_rename _((char *));
static	void		dcc_send_raw _((char *));
static	void 		output_reject_ctcp _((char *, char *));
static	void		process_incoming_chat _((DCC_list *));
static	void		process_incoming_listen _((DCC_list *));
static	void		process_incoming_raw _((DCC_list *));
static	void		process_outgoing_file _((DCC_list *, int));
static	void		process_incoming_file _((DCC_list *));
static	void		DCC_close_filesend _((DCC_list *, char *));
static	void		update_transfer_buffer _((char *format, ...));
#ifdef NON_BLOCKING_CONNECTS
static	void	dcc_got_connected _((DCC_list *));
#endif

static  void    dcc_update_stats _((DCC_list *));
static	void	dcc_set_paths _((char *));
static  void	dcc_set_quiet _((char *));
static  void	dcc_tog_auto _((char *));
static	void	dcc_show_active _((char *));
static	void	dcc_help1 _((char *));

static unsigned char byteordertest _((void));
static void dcc_reject_notify _((char *, char *, char *));
static	void 	output_reject_ctcp (char *notused, char *nicklist);
static	int	get_to_from _((char *));

static char *strip_path _((char *));
static void dcc_set_quiet _(( char *)); 
static void dcc_tog_rename _(( char *)); 
static void dcc_show_active _(( char *)); 
static void dcc_help1 _((char *));
static char *check_paths _((char *));
static void dcc_set_paths _((char *));
static void dcc_overwrite_toggle _((char *));
static void dcc_tog_auto _((char *));
static void dcc_update_stats _((DCC_list *));

#ifdef MIRC_BROKEN_DCC_RESUME
static	void		dcc_getfile_resume _((char *));
static 	void 		dcc_getfile_resume_demanded _((char *user, char *filename, char *port, char *offset));
static	void		dcc_getfile_resume_start _((char *nick, char *filename, char *port, char *offset));
#endif



typedef void (*dcc_function) _((char *));
struct
{
	char	*	name;
	dcc_function 	function;
}	dcc_commands[] =
{
	{ "ACTIVE",	dcc_show_active },
	{ "AUTO",	dcc_tog_auto },
	{ "AUTO_RENAME",dcc_tog_rename },
	{ "BOT",	dcc_chatbot },
	{ "CHAT",	dcc_chat },
	{ "LIST",	dcc_list },
	{ "GLIST",	dcc_glist },
	{ "SEND",	dcc_filesend },
	{ "RESEND",	dcc_resend }, 
	{ "GET",	dcc_getfile },
	{ "REGET",	dcc_regetfile },
	{ "CLOSE",	dcc_close },
	{ "RENAME",	dcc_rename },
#ifdef MIRC_BROKEN_DCC_RESUME
	{ "RESUME",	dcc_getfile_resume },
#endif
	{ "RAW",	dcc_send_raw },
	{ "QUIET",	dcc_set_quiet },
	{ "STATS",	dcc_stats },
	{ "PATHS",	dcc_set_paths },
	{ "OVERWRITE",	dcc_overwrite_toggle },
	{ "?",		dcc_help1 },
	{ "HELP",	dcc_help1 },
	{ NULL,		(dcc_function) NULL }
};

#define BAR_LENGTH 50
static  int	dcc_count = 1;
	int	dcc_active_count = 0;
static	int	doing_multi = 0;
static	int	dcc_quiet = 0;
static	int	dcc_paths = 0;
static	int	dcc_overwrite_var = 0;
	double	dcc_bytes_in = 0;
	double	dcc_bytes_out = 0;
	double  dcc_max_rate_in = 0.0;
static 	double  dcc_min_rate_in = DBL_MAX;
	double  dcc_max_rate_out = 0.0;
static 	double  dcc_min_rate_out = DBL_MAX;
static 	int     dcc_count_stat = 0;

	char	*last_chat_req = NULL;


	char	*dcc_types[] =
{
	"<none>",
	"CHAT",
	"SEND",
	"GET",
	"RAW_LISTEN",
	"RAW",
	"RESEND",
	"REGET",
	"BOT",
	NULL
};

struct	deadlist
{
	DCC_list *it;
	struct deadlist *next;
}	*deadlist = NULL;



static int dccBlockSize(void)
{
int BlockSize;
	BlockSize = get_int_var(DCC_BLOCK_SIZE_VAR);
	if (BlockSize > MAX_DCC_BLOCK_SIZE)
	{
		BlockSize = MAX_DCC_BLOCK_SIZE;
		set_int_var(DCC_BLOCK_SIZE_VAR, MAX_DCC_BLOCK_SIZE);
	}
	else if (BlockSize < 16)
	{
		BlockSize = 16;
		set_int_var(DCC_BLOCK_SIZE_VAR, 16);
	}
	return (BlockSize);
}
/*
 * dcc_searchlist searches through the dcc_list and finds the client
 * with the the flag described in type set.
 */
#ifdef __STDC__
DCC_list *dcc_searchlist(char *name, char *user, int type, int flag, char *othername, char *userhost, int active)
#else
DCC_list * dcc_searchlist(name, user, type, flag, othername, userhost, active)
	char	*name,
		*user;
	int	type,
		flag;
	char	*othername;
	char	*userhost;
	int	active;
#endif
{
	register DCC_list **Client = NULL, *NewClient = NULL;
	
	int dcc_num = 0;
	if (user && *user == '#' && my_atol(user+1)) 
	{
		/* we got a number so find that instead */
		char *p;
		p = user;
		p++;
		dcc_num = my_atol(p);
		if (dcc_num > 0)
		{
			for(Client = (&ClientList); *Client; Client = (&(**Client).next))
				if (dcc_num && ((**Client).dccnum == dcc_num))
					return *Client;
		}
	} 
	else
	{
	for (Client = (&ClientList); *Client ; Client = (&(**Client).next))
	{
		/* 
		 * The following things have to be true:
		 * -> The types have to be the same
		 * -> One of the following things is true:
		 *      -> `name' is NULL or `name' is the same as the 
		 *	   entry's description
		 *          *OR*
		 *	-> `othername' is the same as the entry's othername
		 * -> `user' is the same as the entry's user-perr
		 * -> One of the following is true:
		 *	-> `active' is 1 and the entry is active
		 *	-> `active' is 0 and the entry is not active
		 *	-> `active' is -1 (dont care)
		 */
		if (
		     ( ((**Client).flags & DCC_TYPES) == type ) &&
		     ( (!name || ((**Client).description && !my_stricmp(name, (**Client).description))) ||
		       (othername && (**Client).othername && !my_stricmp(othername, (**Client).othername))   ) &&
		     ( !my_stricmp(user, (**Client).user) ) &&
		     (  (active == -1) ||
		       ((active == 0) && !((**Client).flags & DCC_ACTIVE)) ||
		       ((active == 1) && ((**Client).flags & DCC_ACTIVE))
		     )
		   )
			return *Client;
	}
	}
	if (!flag)
		return NULL;
	*Client = NewClient = (DCC_list *) new_malloc(sizeof(DCC_list));
	NewClient->flags = type;
	NewClient->read = NewClient->write = NewClient->file = -1;
	NewClient->filesize = filesize;
/*
	NewClient->next = NULL;
	NewClient->user = NewClient->description = NewClient->othername = NULL;
	NewClient->bytes_read = NewClient->bytes_sent = 0L;
	NewClient->starttime.tv_sec = NewClient->starttime.tv_usec = 0;
	NewClient->buffer = 0;
	NewClient->window_max = 0;
	NewClient->window_sent = 0;
	NewClient->in_dcc_chat = 0;
	NewClient->cksum = NULL;
	NewClient->packets_transfer = 0;
*/
	malloc_strcpy(&NewClient->description, name);
	malloc_strcpy(&NewClient->user, user);
	malloc_strcpy(&NewClient->othername, othername);
	malloc_strcpy(&NewClient->userhost, userhost);
	NewClient->packets_total = filesize ? (filesize / dccBlockSize() + 1) : 0;
	get_time(&NewClient->lasttime);
	if (dcc_count == 0) dcc_count = 1;
	NewClient->dccnum = dcc_count++;
#ifdef WANT_STRUCT
	update_structs(1, NewClient);
#endif
	return NewClient;
}

/*
 * Added by Chaos: Is used in edit.c for checking redirect.
 */
#ifdef __STDC__
extern int	dcc_active (char *user)
#else
extern int	dcc_active (user)
	char	*user;
#endif
{
	return (dcc_searchlist("chat", user, DCC_CHAT, 0, NULL, NULL, 1)) ? 1 : 0;
}

#ifdef __STDC__
extern int	dcc_activebot (char *user)
#else
extern int	dcc_activebot (user)
	char	*user;
#endif
{
	return (dcc_searchlist("chat", user, DCC_BOTMODE, 0, NULL, NULL, 1)) ? 1 : 0;
}


#ifdef __STDC__
static	void dcc_add_deadclient(DCC_list *client)
#else
static	void dcc_add_deadclient(client)
	DCC_list *client;
#endif
{
	struct deadlist *new;

	new = (struct deadlist *) new_malloc(sizeof(struct deadlist));
	new->next = deadlist;
	new->it = client;
	deadlist = new;
}

/*
 * dcc_erase searches for the given entry in the dcc_list and
 * removes it
 */
#ifdef __STDC__
int dcc_erase(DCC_list *Element)
#else
int dcc_erase(Element)
	DCC_list	*Element;
#endif
{
	DCC_list	**Client;
	int		erase_one = 0;
	dcc_count = 1;
		
	for (Client = &ClientList; *Client; Client = &(**Client).next)
	{
		if (*Client == Element)
		{
			*Client = Element->next;
#ifdef WANT_STRUCT
			update_structs(0, Element);
#endif
			if (Element->write > -1)
			{
				FD_CLR(Element->write, &writables);
				close(Element->write);
			}
			if (Element->read > -1)
			{
				FD_CLR(Element->read, &readables);
				close(Element->read);
			}
			if (Element->file > -1)
				close(Element->file);

			new_free(&Element->othername);
			new_free(&Element->description);
			new_free(&Element->userhost);
			new_free(&Element->user);
			new_free(&Element->buffer);
			new_free(&Element->encrypt);
			new_free(&Element->cksum);
			new_free((char **)&Element);
			erase_one++;
			break;
		}
	}
	for (Client = &ClientList; *Client; Client = &(**Client).next)
		(*Client)->dccnum = dcc_count++;
	if (erase_one)
	{
		*DCC_current_transfer_buffer = 0;
		status_update(1);
	}
	return erase_one;
}

static void dcc_really_erase _((void))
{
	struct deadlist *dies;
	for (;(dies = deadlist) != NULL;)
	{
		deadlist = deadlist->next;
		dcc_erase(dies->it);
	}
}

/*
 * Set the descriptor set to show all fds in Client connections to
 * be checked for data.
 */
#ifdef __STDC__
extern void	set_dcc_bits (fd_set *rd, fd_set *wd)
#else
extern void set_dcc_bits(rd, wd)
	fd_set	*rd, *wd;
#endif
{
	register DCC_list	*Client;

	for (Client = ClientList; Client != NULL; Client = Client->next)
	{

		if (Client->write != -1)
		{
#ifdef DCC_CNCT_PEND
			if (Client->flags & DCC_CNCT_PEND)
				FD_SET(Client->write, wd);
#endif
#ifdef NON_BLOCKING_CONNECTS
			if ((((Client->flags & DCC_TYPES) == DCC_FILEOFFER) ||
			    ((Client->flags & DCC_TYPES) == DCC_RESENDOFFER))
			    && !Client->eof)
				FD_SET(Client->write, wd);
#else
			;
#endif
		}
		if (Client->read != -1)
			FD_SET(Client->read, rd);
	}
}

#ifdef NON_BLOCKING_CONNECTS
static	void
dcc_got_connected(client)
	DCC_list *client;
{
struct sockaddr_in	remaddr = {0};
int	rl = sizeof(remaddr);

	if (getpeername(client->read, (struct sockaddr *) &remaddr, &rl) != -1)
	{

		if (client->flags & DCC_OFFER)
		{
			client->flags &= ~DCC_OFFER;

			if ((client->flags & DCC_TYPES) != DCC_RAW)
			{

				if (do_hook(DCC_CONNECT_LIST, "%s %s %s %d", client->user, 
					dcc_types[client->flags&DCC_TYPES],inet_ntoa(remaddr.sin_addr),
					ntohs(remaddr.sin_port))) 
				{
					if (!dcc_quiet)
						put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
						"%s %s %s %s %s %d", update_clock(GET_TIME), 
						dcc_types[client->flags&DCC_TYPES], client->user, 
						client->userhost?client->userhost:"u@h", inet_ntoa(remaddr.sin_addr), (int) ntohs(remaddr.sin_port)));
				}

			}
		}
		get_time(&client->starttime);
		client->flags &= ~DCC_CNCT_PEND;
		if (((client->flags & DCC_TYPES) == DCC_REGETFILE))
		{
		        /* send a packet to the sender with transfer resume instructions */
			put_it("%s", convert_output_format("$G %RDCC %YTelling uplink we want to start at%n: $0", "%d", client->transfer_orders.byteoffset));
	       	        send(client->read, (const char *)&client->transfer_orders, sizeof(struct transfer_struct), 0);
		}
		if (!get_int_var(DCC_FAST_VAR))
		{
			set_blocking(client->read);
			if (client->read != client->write)
				set_blocking(client->write);
		}
	}
	
}
#endif

/*
 * Check all DCCs for data, and if they have any, perform whatever
 * actions are required.
 */
#ifdef __STDC__
extern void	dcc_check (fd_set *Readables, fd_set *Writables)
#else
extern void dcc_check(Readables)
	fd_set	*Readables;
	fd_set	*Writables;
#endif
{
	register DCC_list	**Client;
	struct	timeval	timeout;
	int	previous_server;
	int	lastlog_level;
	register int	flags;
	int	fast_dcc = get_int_var(DCC_FAST_VAR);
		
	previous_server = from_server;
	from_server = (-1);
	timeout.tv_sec = timeout.tv_usec = 0;
	message_from(NULL, LOG_DCC);
	lastlog_level = set_lastlog_msg_level(LOG_DCC);

	for (Client = (&ClientList); *Client != NULL;)
	{
		flags = (*Client)->flags;
#ifdef NON_BLOCKING_CONNECTS
		if (flags & DCC_CNCT_PEND)
			dcc_got_connected(*Client);
#endif

#if defined(NON_BLOCKING_CONNECTS)
		if (fast_dcc && (*Client)->write != -1 && FD_ISSET((*Client)->write, Writables))
		{
			FD_CLR((*Client)->write, Writables);
			switch(flags & DCC_TYPES)
			{
				case DCC_RESENDOFFER:
				case DCC_FILEOFFER:
				process_outgoing_file(*Client, 0);
			}
		}
#endif
		if ((*Client)->read != -1 && FD_ISSET((*Client)->read, Readables))
		{
			FD_CLR((*Client)->read, Readables);
			switch(flags&DCC_TYPES)
			{
				case DCC_BOTMODE:
				case DCC_CHAT:
					process_incoming_chat(*Client);
					break;
				case DCC_RAW_LISTEN:
					process_incoming_listen(*Client);
					break;
				case DCC_RAW:
					process_incoming_raw(*Client);
					break;
				case DCC_FILEOFFER:
				case DCC_RESENDOFFER:
					process_outgoing_file(*Client, 1);
					break;
				case DCC_FILEREAD:
				case DCC_REGETFILE:
					process_incoming_file(*Client);
					break;
			}
		}
		if (flags & DCC_DELETE)
			dcc_add_deadclient(*Client);
		Client = (&(**Client).next);
	}

	message_from(NULL, LOG_CRAP);
	(void) set_lastlog_msg_level(lastlog_level);
	dcc_really_erase();
	from_server = previous_server;
}

/*
 * Process a DCC command from the user.
 */
#ifdef __STDC__
extern void	process_dcc(char *args)
#else
extern void process_dcc(args)
	char	*args;
#endif
{
	char	*command;
	int	i;
	int	lastlog_level;

	if (!(command = next_arg(args, &args)))
		return;
	for (i = 0; dcc_commands[i].name != NULL; i++)
	{
		if (!my_stricmp(dcc_commands[i].name, command))
		{
			message_from(NULL, LOG_DCC);
			lastlog_level = set_lastlog_msg_level(LOG_DCC);
			dcc_commands[i].function(args);
			message_from(NULL, LOG_CRAP);
			(void) set_lastlog_msg_level(lastlog_level);
			return;
		}
	}
	put_it("%s", convert_output_format("$G Unknown %RDCC%n command: $0", "%s", command));
}

static	int	dcc_open _((DCC_list *Client))
{
char    		*user,
			*Type;

struct	in_addr 	myip;

#ifndef NON_BLOCKING_CONNECTS
struct  host_ent	*temp;
struct	in_addr		temp1;	
struct	sockaddr_in	remaddr;
int			rl = sizeof(remaddr);
#endif

int			old_server;

#ifdef MIRC_BROKEN_DCC_RESUME
	char		buf[10];
#endif

	user = Client->user;
	old_server = from_server;
	if (from_server == -1)
		from_server = get_window_server(0);

	myip.s_addr = server_list[from_server].local_addr.s_addr;


	if (myip.s_addr == htonl(0x7f000001))
		myip.s_addr = MyHostAddr.s_addr;
		
	Type = dcc_types[Client->flags & DCC_TYPES];
	if (Client->flags & DCC_OFFER)
	{
		message_from(NULL, LOG_DCC);
#ifdef DCC_CNCT_PEND
		Client->flags |= DCC_CNCT_PEND;
#endif
	/*BLAH!*/
		Client->remport = ntohs(Client->remport);
		if ((Client->write = connect_by_number(inet_ntoa(Client->remote), &Client->remport, SERVICE_CLIENT, PROTOCOL_TCP, 1)) < 0)
		{
			put_it("%s", convert_output_format("$G %RDCC%n Unable to create connection: $0-", "%s", errno ? sys_errlist[errno] : "Unknown Host"));
			message_from(NULL, LOG_CURRENT);
			dcc_erase(Client);
			from_server = old_server;
			return 0;
		}
	/* BLAH! */
		Client->remport = htons(Client->remport);
		Client->read = Client->write;
		FD_SET(Client->read, &readables);
		Client->flags |= DCC_ACTIVE;
#ifndef NON_BLOCKING_CONNECTS
		Client->flags &= ~DCC_OFFER;

		(void) getpeername(Client->read, (struct sockaddr *) &remaddr, &rl);

		if (get_to_from(Type) != -1)
			dcc_active_count++;
		if ((Client->flags & DCC_TYPES) != DCC_RAW)
		{
                        if (do_hook(DCC_CONNECT_LIST,"%s %s %s %d",
       	                        user, Type, inet_ntoa(remaddr.sin_addr),
               	                ntohs(remaddr.sin_port)))
				if (!dcc_quiet)
					put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
						"%s %s %s %s %s %d", update_clock(GET_TIME), Type, user, Client->userhost?Client->userhost:"u@h", 
						inet_ntoa(remaddr.sin_addr), (int)ntohs(remaddr.sin_port)));
		}
#endif	
		message_from(NULL, LOG_CURRENT);
		get_time(&Client->starttime);
		Client->bytes_read = Client->bytes_sent = 0L;
		from_server = old_server;
		return 1;
	}
	else
	{
		unsigned short portnum = 0;

#ifdef DCC_CNCT_PEND
		Client->flags |= DCC_WAIT|DCC_CNCT_PEND;
#else
		Client->flags |= DCC_WAIT;
#endif
		message_from(NULL, LOG_DCC);
		if (Client->remport)
			portnum = Client->remport;
		if ((Client->read = connect_by_number(NULL, &portnum, SERVICE_SERVER, PROTOCOL_TCP, 1)) < 0)
		{
			put_it("%s", convert_output_format("$G %RDCC%n Unable to create connection: $0-", "%s", errno ? sys_errlist[errno] : "Unknown Host"));
			message_from(NULL, LOG_CURRENT);
			dcc_erase(Client);
			from_server = old_server;
			return 0;
		}
		if (get_to_from(Type) != -1)
			dcc_active_count++;
		if (Client->flags & DCC_TWOCLIENTS)
		{
			/* patch to NOT send pathname accross */
			char	*nopath;

#ifdef WINNT
			if (((Client->flags & DCC_FILEOFFER)== DCC_FILEOFFER) && (nopath = rindex(Client->description, '\\')))
#else
			if (((Client->flags & DCC_FILEOFFER)== DCC_FILEOFFER) && (nopath = rindex(Client->description, '/')))
#endif
				nopath++;
			else
				nopath = Client->description;

			if (Client->filesize)
				send_ctcp(CTCP_PRIVMSG, user, CTCP_DCC,
					 "%s %s %lu %u %d", Type, nopath,
					 (u_long) ntohl(myip.s_addr),
					 (u_short) portnum,
					 Client->filesize);
			else if ((Client->flags & DCC_TYPES) == DCC_BOTMODE)
				send_ctcp(CTCP_NOTICE, user, CTCP_BOTLINK,
					 "%s %s %lu %u %s", Type, nopath,
					 (u_long) ntohl(myip.s_addr),
					 (u_short) portnum, Client->encrypt?Client->encrypt:"");
			else
				send_ctcp(CTCP_PRIVMSG, user, CTCP_DCC,
					 "%s %s %lu %u", Type, nopath,
					 (u_long) ntohl(myip.s_addr),
					 (u_short) portnum);
			message_from(NULL, LOG_DCC);
			if (!doing_multi && !dcc_quiet)
				put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_DCC_CHAT_VAR), 
					"%s %s %s", update_clock(GET_TIME), Type, user));
			message_from(NULL, LOG_CURRENT);
		}
#ifdef MIRC_BROKEN_DCC_RESUME
		sprintf(buf, "%d", (int) portnum);
		malloc_strcpy(&Client->othername, buf);
#endif
		FD_SET(Client->read, &readables);
		Client->starttime.tv_sec = Client->starttime.tv_usec = 0;
		from_server = old_server;
		return 2;
	}
}
#ifdef __STDC__
void add_userhost_to_dcc(WhoisStuff *stuff, char *nick, char *args)
#else
void add_userhost_to_dcc(stuff, nick, args)
	WhoisStuff *stuff;
	char	*nick;
	char	*args;
#endif
{
DCC_list *Client;
	Client = dcc_searchlist("chat", nick, DCC_CHAT, 0, NULL, NULL, -1);
	if (!stuff || !nick || !stuff->nick || !stuff->user || !stuff->host || !strcmp(stuff->user, "<UNKNOWN>"))
		return;
	if (Client)	
		Client->userhost = m_sprintf("%s@%s", stuff->user, stuff->host);
	return;
}

#ifdef __STDC__
void	dcc_chat (char *args)
#else
void dcc_chat(args)
	char	*args;
#endif
{
	char	*user;
	char	*passwd = NULL;
	char	*port 	= NULL;
	char	*equal_user = NULL;
	DCC_list	*Client;
	
	if ((user = next_arg(args, &args)) == NULL)
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must supply a nickname for DCC chat", NULL, NULL));
		return;
	}
	while (args && *args)
	{
		char *argument = next_arg(args, &args);
		if (*argument == '-' || *(argument+1) == 'e')
			passwd = next_arg(args, &args);
		else if (*argument == '-' || *(argument+1) == 'p')
			port = next_arg(args, &args);
	}

	Client = dcc_searchlist("chat", user, DCC_CHAT, 1, NULL, NULL, -1);

	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		put_it("%s", convert_output_format("$G %RDCC%n A previous DCC chat to $0 exists", "%s", user));
		return;
	}
	if (port)
		Client->remport = atol(port);
	Client->flags |= DCC_TWOCLIENTS;
	if (passwd)
		Client->encrypt = m_strdup(passwd);

	equal_user = m_sprintf("=%s", user);
	addtabkey(equal_user, 0);
	new_free(&equal_user);

	dcc_open(Client);
	add_to_userhost_queue(user, add_userhost_to_dcc, "%d %s", Client->read, user);
}


#ifdef __STDC__
void	dcc_chatbot (char *args)
#else
void dcc_chatbot(args)
	char	*args;
#endif
{
	char	*user;
	char	*passwd = NULL;
	DCC_list	*Client;
	char	*port 	= NULL;
	
	if ((user = next_arg(args, &args)) == NULL)
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must supply a nickname for DCC bot", NULL, NULL));
		return;
	}
	while (args && *args)
	{
		char *argument = next_arg(args, &args);
		if (*argument == '-' )
		{
			if ((*(argument+1)) == 'e')
				passwd = next_arg(args, &args);
			else if ((*(argument+1)) == 'p')
				port = next_arg(args, &args);
		}
	}
		
	Client = dcc_searchlist("chat", user, DCC_BOTMODE, 1, NULL, NULL, -1);

	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		put_it("%s", convert_output_format("$G %RDCC%n A previous DCC Bot to $0 exists", "%s", user));
		return;
	}
	Client->flags |= DCC_TWOCLIENTS;
	if (passwd)
		Client->encrypt = m_strdup(passwd);
	if (port)
		Client->remport = atol(port);
	dcc_open(Client);
	add_to_userhost_queue(user, add_userhost_to_dcc, "%d %s", Client->read, user);
}

#ifdef __STDC__
extern char	*dcc_raw_listen (int port)
#else
extern char	* dcc_raw_listen(port)
	int	port;
#endif
{
	DCC_list	*Client;
	char	*PortName;
	struct	sockaddr_in locaddr;
	char	*RetName = NULL;
	int	size;
	int	lastlog_level;

	lastlog_level = set_lastlog_msg_level(LOG_DCC);
	if (port && port < 1025)
	{
		put_it("%s", convert_output_format("$G %RDCC%n Cannot bind to a privileged port", NULL, NULL));
		(void) set_lastlog_msg_level(lastlog_level);
		return NULL;
	}
	PortName = ltoa(port);
	Client = dcc_searchlist("raw_listen", PortName, DCC_RAW_LISTEN, 1, NULL, NULL, -1);
	if (Client->flags & DCC_ACTIVE)
	{
		put_it("%s", convert_output_format("$G %RDCC%n A previous Raw Listen on $0 exists", "%s", PortName));
		set_lastlog_msg_level(lastlog_level);
		return RetName;
	}
	bzero((char *) &locaddr, sizeof(locaddr));
	locaddr.sin_family = AF_INET;
	locaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	locaddr.sin_port = htons(port);
	if (0 > (Client->read = socket(AF_INET, SOCK_STREAM, 0)))
	{
		dcc_erase(Client);
		put_it("%s", convert_output_format("$G %RDCC%n socket() failed: $0-", "%s", sys_errlist[errno]));
		(void) set_lastlog_msg_level(lastlog_level);
		return RetName;
	}
	FD_SET(Client->read, &readables);
	set_socket_options(Client->read);
	if (bind(Client->read, (struct sockaddr *) &locaddr, sizeof(locaddr)) == -1)
	{
		dcc_erase(Client);
		put_it("%s", convert_output_format("$G %RDCC%n Count not bind port: $0-", "%s", sys_errlist[errno]));
		(void) set_lastlog_msg_level(lastlog_level);
		return RetName;
	}
	listen(Client->read, 4);
	size = sizeof(locaddr);
	get_time(&Client->starttime);
	getsockname(Client->read, (struct sockaddr *) &locaddr, &size);
	Client->write = ntohs(locaddr.sin_port);
	Client->flags |= DCC_ACTIVE;
	malloc_strcpy(&Client->user, ltoa(Client->write));
	malloc_strcpy(&RetName, Client->user);
	(void) set_lastlog_msg_level(lastlog_level);
	return RetName;
}

#ifdef __STDC__
extern char	*dcc_raw_connect(char *host, u_short port)
#else
extern char	* dcc_raw_connect(host, port)
	char	*host;
	u_short	port;
#endif
{
	DCC_list	*Client;
	char	*PortName;
	struct	in_addr	address;
	struct	hostent	*hp;
	int	lastlog_level;

	lastlog_level = set_lastlog_msg_level(LOG_DCC);
	if ((address.s_addr = inet_addr(host)) == -1)
	{
		hp = gethostbyname(host);
		if (!hp)
		{
			put_it("%s", convert_output_format("$G %RDCC%n Unknown host: $0-", "%s", host));
			set_lastlog_msg_level(lastlog_level);
			return m_strdup(empty_string);
		}
		bcopy(hp->h_addr, &address, sizeof(address));
	}
	Client = dcc_searchlist(host, ltoa(port), DCC_RAW, 1, NULL, NULL, -1);
	if (Client->flags & DCC_ACTIVE)
	{
		put_it("%s", convert_output_format("$G %RDCC%n A previous DCC raw to $0 on $1 exists", "%s %d", host, port));
		set_lastlog_msg_level(lastlog_level);
		return m_strdup(empty_string);
	}
	/* Sorry. The missing 'htons' call here broke $connect() */
	Client->remport = htons(port);
	bcopy((char *) &address, (char *) &Client->remote, sizeof(address));
	Client->flags = DCC_OFFER | DCC_RAW;
	if (!dcc_open(Client))
		return m_strdup(empty_string);
	PortName = ltoa(Client->read);
	Client->user = m_strdup(PortName);
	if (do_hook(DCC_RAW_LIST, "%s %s E %d", PortName, host, port))
        	if (do_hook(DCC_CONNECT_LIST,"%s RAW %s %d",PortName, host, port))
			put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
				"%s %s %s %s %s %d", update_clock(GET_TIME), "RAW", host, 
				Client->userhost?Client->userhost:"u@h", PortName, port));
	(void) set_lastlog_msg_level(lastlog_level);
	return m_strdup(ltoa(Client->read));
}

void real_dcc_filesend(char *filename, char *real_file, char *user, char *passwd, int type, int portnum)
{
	DCC_list *Client;
	struct	stat	stat_buf;

	stat_file(filename, &stat_buf);
#ifdef S_IFDIR
	if (stat_buf.st_mode & S_IFDIR)
	{
		put_it("%s", convert_output_format("$G %RDCC%n Cannot send a directory", NULL, NULL));
		return;
	}
#endif
#ifndef WINNT
	if (scanstr(filename, "/etc/"))
	{
		put_it("%s", convert_output_format("$G %RDCC%n Send request for /etc rejected", NULL, NULL));
		return;
	}
#endif
	filesize = stat_buf.st_size;
	Client = dcc_searchlist(filename, user, type, 1, real_file, NULL, -1);
	filesize = 0;
	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		put_it("%s", convert_output_format("$G %RDCC%n A previous DCC send:$0 to $1 exists", "%s %s", filename, user));
		return;
	}
	Client->flags |= DCC_TWOCLIENTS;
	Client->remport = portnum;
	dcc_open(Client);
}

void dcc_resend(char *args)
{
	char	*user = NULL;
	char	*filename = NULL,
		*fullname = NULL;
	char	*passwd = NULL;
	char	*FileBuf = NULL;
	int	portnum = 0;
	
	if (!(user = next_arg(args, &args)) ||
	    !(filename = next_arg(args, &args)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must supply a nickname and a filename for DCC resend", NULL, NULL));
		return;
	}
#ifdef WINNT
	if (*filename == '\\')
#else
	if (*filename == '/')
#endif
	{
		malloc_strcpy(&FileBuf, filename);
	}
	else if (*filename == '~')
	{
		if (!(fullname = expand_twiddle(filename)))
		{
			put_it("%s", convert_output_format("$G %RDCC%n Unable to expand: $0", "%s", filename));
			return;
		}
		malloc_strcpy(&FileBuf, fullname);
		new_free(&fullname);
	}
	else
	{
		char current_dir[BIG_BUFFER_SIZE+1];
		getcwd(current_dir, sizeof(current_dir) - strlen(filename) - 4);
		malloc_sprintf(&FileBuf, "%s/%s", current_dir, filename); 
	}

	if (access(FileBuf, R_OK))
	{
		put_it("%s", convert_output_format("$G %RDCC%n Cannot access: $0", "%s", FileBuf));
		new_free(&FileBuf);
		return;
	}

	while (args && *args)
	{
		char *argument = next_arg(args, &args);
		if (*argument == '-' || *(argument+1) == 'e')
			passwd = next_arg(args, &args);
		else if (*argument == '-' || *(argument+1) == 'p')
		{
			char *v = next_arg(args, &args);
			portnum = (v) ? my_atol(v) : 0;
		}
	}

	real_dcc_filesend(FileBuf,filename, user, passwd, DCC_RESENDOFFER, portnum);
	new_free(&FileBuf);
}

#ifdef __STDC__
void		dcc_filesend (char *args)
#else
void dcc_filesend(args)
	char	*args;
#endif
{
	char	*user = NULL;
	char	*filename = NULL,
		*fullname = NULL;
	char	FileBuf[BIG_BUFFER_SIZE + 1];
	char	*passwd = NULL;
	int	portnum = 0;
		
	if (!(user = next_arg(args, &args)) || !(filename = next_arg(args, &args)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must supply a nickname and a filename for DCC send", NULL, NULL));
		return;
	}
	

#ifdef WINNT
	if (*filename == '\\')
#else
	if (*filename == '/')
#endif
		strcpy(FileBuf, filename);

	else if (*filename == '~')
	{
		if (0 == (fullname = expand_twiddle(filename)))
		{
			put_it("%s", convert_output_format("$G %RDCC%n Unable to expand: $0", "%s", filename));
			return;
		}
		strcpy(FileBuf, fullname);
		new_free(&fullname);
	}
	else
	{
		getcwd(FileBuf, sizeof(FileBuf));
#ifdef WINNT
		strcat(FileBuf, "\\");
#else
		strcat(FileBuf, "/");
#endif
		strcat(FileBuf, filename);
	}

	while (args && *args)
	{
		char *argument = next_arg(args, &args);
		if (*argument == '-' && *(argument+1) == 'e')
			passwd = next_arg(args, &args);
		else if (*argument == '-' && *(argument+1) == 'p')
		{
			char *v = next_arg(args, &args);
			portnum = (v) ? my_atol(v) : 0;
		}
	}

	if (!strchr(FileBuf, '*') && access(FileBuf, R_OK))
	{
		put_it("%s", convert_output_format("$G %RDCC%n Cannot access: $0", "%s", FileBuf));
		return;
	} 
	else if (strchr(FileBuf, '*'))
	{
		char *path = NULL;
		char *thefile;
		DIR *dp;
		struct dirent *dir;
		struct stat stbuf;
		char *filebuf = NULL;
		char *expand = NULL;
		int count = 0;
		malloc_strcpy(&path, FileBuf);

#ifdef WINNT
		if ((thefile = strrchr(path, '\\')) != NULL)
#else
		if ((thefile = strrchr(path, '/')) != NULL)
#endif
			*thefile++ = '\0';
		else {
			malloc_strcpy(&path, "~");
			thefile = FileBuf;
		}
		expand = expand_twiddle(path);
		
		if ((dp = opendir(expand)) == NULL)
		{
			put_it("%s", convert_output_format("$G %RDCC%n Cannot access directory: $0", "%s", expand));
			new_free(&expand);
			new_free(&path);
			return;
		}
		
		while(1)
		{
			dir = readdir(dp);
			if (!dir) 
				break;
			if (dir->d_ino == 0)
				continue;
			if (!match(thefile, dir->d_name))
				continue;
#ifdef WINNT
			malloc_sprintf(&filebuf, "%s\\%s", expand, dir->d_name);
#else
			malloc_sprintf(&filebuf, "%s/%s", expand, dir->d_name);
#endif
			stat(filebuf, &stbuf);
			if (S_ISDIR(stbuf.st_mode))
				continue;
			count++;
			real_dcc_filesend(filebuf, dir->d_name, user, passwd, DCC_FILEOFFER, 0);
		}
		new_free(&filebuf);
		if (!dcc_quiet)
		{
			if (count)
				put_it("%s", convert_output_format("$G %RDCC%n Sent DCC SEND request to $0 for files $1", "%s %s", user, FileBuf));
			else
				put_it("%s", convert_output_format("$G %RDCC%n No Files found matching $0", "%s", FileBuf));
		}
		new_free(&path);
		new_free(&expand);
		return;
	}
	real_dcc_filesend(FileBuf, filename, user, passwd, DCC_FILEOFFER, portnum);
}

void	multiget(usern, filen)
char	*usern, *filen;
{
	DCC_list	*dccList;
	char		*newbuf = NULL;
	char 		*expand = NULL;	
	doing_multi = 1;
	if (strchr(filen, ' '))
		*strchr(filen, ' ') = '\0';
	for (dccList = ClientList; dccList ; dccList = dccList->next)
        {
        	if((match(dccList->description, filen) || match(filen, dccList->description))&& 
			!my_stricmp(usern, dccList->user) && 
			(dccList->flags & DCC_TYPES) == DCC_FILEREAD)
		{
			if (get_string_var(DCC_DLDIR_VAR)) {
				expand = expand_twiddle(get_string_var(DCC_DLDIR_VAR));
#ifdef WINNT
				malloc_sprintf(&newbuf, "%s %s\\%s", usern, expand, dccList->description);
#else
				malloc_sprintf(&newbuf, "%s %s/%s", usern, expand, dccList->description);
#endif
			} else
				malloc_sprintf(&newbuf, "%s %s", usern, dccList->description);
			if (!dcc_quiet)
				put_it("%s", convert_output_format("$G %RDCC%n Attempting DCC get: $0", "%s", dccList->description));
			dcc_getfile(newbuf);
			new_free(&expand);
			new_free(&newbuf);
		}
	}
	doing_multi = 0;
}
 

#ifdef __STDC__
static	void		dcc_getfile (char *args)
#else
static void dcc_getfile(args)
	char	*args;
#endif
{
	char	*user;
	char	*filename = NULL;
	char	*tmp = NULL;
	DCC_list	*Client;
	char	*fullname = NULL;
	char	*passwd = NULL;
	char	*argument;
		
	if (0 == (user = next_arg(args, &args)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must supply a nickname for DCC get", NULL, NULL));
		return;
	}
	if (args && *args)
	{
		if (*args != '-' && *(args+1) != 'e')
			filename = next_arg(args, &args);

		if (args && *args == '-' && *(args+1) == 'e')
		{
			argument = next_arg(args, &args);
			passwd = next_arg(args, &args);
		}
	}


	if (0 == (Client = dcc_searchlist(filename, user, DCC_FILEREAD, 0, NULL, NULL, 0)))
	{
		if (filename)
			put_it("%s", convert_output_format("$G %RDCC%n No file ($0) offered in SEND mode by $1", "%s %s", filename, user));
		else
			put_it("%s", convert_output_format("$G %RDCC%n No file offered in SEND mode by $0", "%s", user));
		return;
	}
	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		put_it("%s", "$G %RDCC%n A previous DCC get:$0 to $0 exists", "%s %s", filename, user);
		return;
	}
	Client->flags |= DCC_TWOCLIENTS;
	Client->bytes_sent = Client->bytes_read = 0L;

	if (passwd)
		Client->encrypt = m_strdup(passwd);

	if (!dcc_open(Client))
		return;
	if (get_string_var(DCC_DLDIR_VAR))
#ifdef WINNT
		malloc_sprintf(&tmp, "%s\\%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#else
		malloc_sprintf(&tmp, "%s/%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#endif
	else
		malloc_sprintf(&tmp, "%s", Client->description);
	if (0 == (fullname = expand_twiddle(tmp)))
		/* left over phone bug */
		malloc_strcpy(&fullname, tmp);


	if (-1 == (Client->file = open(fullname, O_WRONLY | O_TRUNC | O_CREAT, 0644)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n Unable to open $0: $1", "%s %s", Client->description, errno?sys_errlist[errno]:"Unknown"));
		FD_CLR(Client->read, &readables);
		close(Client->read);
		if (dcc_active_count) dcc_active_count--;
		dcc_erase(Client);
	}
	new_free(&fullname);
	new_free(&tmp);
}


void dcc_regetfile(args)
	char	*args;
{
	char	*user;
	char	*tmp = NULL;
	char	*filename;
	DCC_list	*Client;
	char	*fullname = NULL;
	struct stat buf;

	if (0 == (user = next_arg(args, &args)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must supply a nickname for DCC get", NULL, NULL));
		return;
	}
	filename = next_arg(args, &args);

	if (!(Client = dcc_searchlist(filename, user, DCC_REGETFILE, 0, NULL, NULL, 0)))
	{
		if (filename)
			put_it("%s", convert_output_format("$G %RDCC%n No file ($0) offered in RESEND mode by $1", "%s %s", filename, user));
		else
			put_it("%s", convert_output_format("$G %RDCC%n No file offered in RESEND mode by $0", "%s", user));
		return;
	}
	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		put_it("%s", "$G %RDCC%n A previous DCC reget:$0 to $0 exists", "%s %s", filename, user);
		return;
	}
	Client->flags |= DCC_TWOCLIENTS;
	Client->bytes_sent = Client->bytes_read = 0L;

	if (get_string_var(DCC_DLDIR_VAR))
#ifdef WINNT
		malloc_sprintf(&tmp, "%s\\%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#else
		malloc_sprintf(&tmp, "%s/%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#endif
	else
		malloc_sprintf(&tmp, "%s", Client->description);
	if (0 == (fullname = expand_twiddle(tmp)))
		malloc_strcpy(&fullname, tmp);

	if (!dcc_open(Client))
		return;
	if (-1 == (Client->file = open(fullname, O_WRONLY | O_CREAT, 0644)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n Unable to open $0: $1", "%s %s", Client->description, errno?sys_errlist[errno]:"Unknown"));
		close(Client->read);
		dcc_erase(Client);
		if (dcc_active_count) dcc_active_count--;
		return;
	}

	/* seek to the end of the file about to be resumed */
	lseek(Client->file, 0, SEEK_END);

	/* get the size of our file to be resumed */
	fstat(Client->file, &buf);

	Client->transfer_orders.packet_id = DCC_PACKETID;
	Client->transfer_orders.byteoffset = buf.st_size;
	Client->transfer_orders.byteorder = byteordertest();
#ifndef NON_BLOCKING_CONNECTS
	/* send a packet to the sender with transfer resume instructions */
	send(Client->read, (const char *)&(Client->transfer_orders), sizeof(struct transfer_struct), 0);
#endif
	new_free(&fullname);
	new_free(&tmp);
}

#ifdef __STDC__
extern void	register_dcc_offer (char *user, char *type, char *description, char *address, char *port, char *size, char *extra, char *userhost)
#else
extern void register_dcc_offer(user, type, description, address, port, size, extra, userhost)
	char	*user;
	char	*type;
	char	*description;
	char	*address;
	char	*port;
	char	*size;
	char	*extra;
	char	*userhost;
#endif
{
	DCC_list *	Client;
	int		CType;
	char *		c = NULL;
	u_long		TempLong;
	unsigned	TempInt;
	int		do_auto = 0;	/* used in dcc chat collisions */
	int		do_autog = 1;
	long		packets = 0;
		
#ifndef WINNT
	if ((c = rindex(description, '/')))
		description = c + 1;
	if ('.' == *description)
		*description = '_';
#else
	if ((c = rindex(description, '\\')))
		description = c + 1;
#endif

	message_from(NULL, LOG_DCC);
	if (size && *size)
	{
		filesize = my_atol(size);
		packets = filesize / dccBlockSize() + 1;
	}
	else
		packets = filesize = 0;

	if (!my_stricmp(type, "CHAT"))
		CType = DCC_CHAT;
	else if (!my_stricmp(type, "BOT"))
		CType = DCC_BOTMODE;
	else if (!my_stricmp(type, "SEND"))
		CType = DCC_FILEREAD;
	else if (!my_stricmp(type, "RESEND"))
		CType = DCC_REGETFILE;
#ifdef MIRC_BROKEN_DCC_RESUME
	else if (!my_stricmp(type, "RESUME"))
	{
		/* 
		 * Dont be deceieved by the arguments we're passing it.
		 * The arguments are "out of order" because MIRC doesnt
		 * send them in the traditional order.  Ugh.
		 */
		dcc_getfile_resume_demanded(user, description, address, port);
		return;
	}
	else if (!my_stricmp(type, "ACCEPT"))
	{
		/*
		 * See the comment above.
		 */
		dcc_getfile_resume_start (user, description, address, port);
		return;
	}
#endif
        else
        {
		put_it("%s", convert_output_format("$G %RDCC%n Unknown DCC $0 ($1) recieved from $2", "%s %s %s", type, description, user));
		message_from(NULL, LOG_CRAP);
                return;
        }

	Client = dcc_searchlist(description, user, CType, 1, NULL, NULL, -1);
	filesize = 0;

	if (extra && *extra)
		Client->cksum = m_strdup(extra);

	if (Client->flags & DCC_WAIT)
	{
		FD_CLR(Client->read, &readables);
		close(Client->read);
		dcc_erase(Client);
		if (DCC_CHAT == CType)
		{
			Client = dcc_searchlist(description, user, CType, 1, NULL, NULL, -1);
			do_auto = 1;
		}
		else
		{
		put_it("%s", convert_output_format("$G %RDCC%n $0 collision for $1:$2", "%s %s %s", type, user, description));
			send_ctcp(CTCP_NOTICE, user, CTCP_DCC, "DCC %s collision occured while connecting to %s (%s)", type, nickname, description);
			message_from(NULL, LOG_CRAP);
			return;
		}
	}
	if (Client->flags & DCC_ACTIVE)
	{
		put_it("%s", convert_output_format("$G %RDCC%n Recieved DCC $0 request from $1 while previous session active", "%s %s", type, user));
		message_from(NULL, LOG_CRAP);
		return;
	}
	Client->flags |= DCC_OFFER;

	TempLong = strtoul(address, NULL, 10);
	Client->remote.s_addr = htonl(TempLong);
	TempInt = (unsigned) strtoul(port, NULL, 10);
	Client->remport = htons(TempInt);

	if (TempInt < 1024)
	{
		put_it("%s", convert_output_format("$G %RDCC%n $0 ($1) request from $2 rejected", "%s %s %s", type, description, user));
		dcc_erase(Client);
		message_from(NULL, LOG_CRAP);
		return;
	}
	if (userhost)
		Client->userhost = m_strdup(userhost);
#ifdef HACKED_DCC_WARNING
	/* This right here compares the hostname from the userhost stamped
	 * on the incoming privmsg to the address stamped in the handshake.
	 * We do not automatically reject any discrepencies, but warn the
	 * user instead to be cautious.
	 */
	{
		char tmpbuf[128];
		char *fromhost;
		u_32int_t compare, compare2;
		struct hostent *hostent_fromhost;

		strncpy(tmpbuf, FromUserHost, 127);
		fromhost = index(tmpbuf, '@') + 1;
		alarm(1);	/* dont block too long... */
		hostent_fromhost = gethostbyname(fromhost);
		alarm(0);
		if (!hostent_fromhost)
		{
			yell("Incoming handshake has an address [%s] that could not be figured out!", fromhost);
			yell("Please use caution in deciding whether to accept it or not");
		}
		else
		{
			compare = *((u_32int_t *)hostent_fromhost->h_addr_list[0]);
			compare2 = inet_addr(fromhost);
			if ((compare != Client->remote.s_addr) &&
				(compare2 != Client->remote.s_addr))
			{
				say("WARNING: Fake dcc handshake detected! [%x]",Client->remote.s_addr);
				say("Unless you know where this dcc request is coming from");
				say("It is recommended you ignore it!");
			}
		}
	}
#endif
	if ((u_long) 0 == TempLong || 0 == Client->remport)
	{
		yell("DCC handshake from %s ignored becuase it had an null port or address", user);
		dcc_erase(Client);
		message_from(NULL, LOG_CRAP);
		return;
	}
	if (((Client->flags & DCC_TYPES) == DCC_FILEREAD) && ((Client->flags & DCC_TYPES) != DCC_REGETFILE))
	{
		struct stat statit;
		char *tmp = NULL, *p;
#ifdef WINNT
		malloc_sprintf(&tmp, "%s\\%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#else
		malloc_sprintf(&tmp, "%s/%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#endif
		p = expand_twiddle(tmp);
		if ( !dcc_overwrite_var && get_int_var(DCC_AUTOGET_VAR) && ((do_autog = stat(p, &statit)) == 0) )
		{
			if (!get_int_var(DCC_AUTORENAME_VAR))
			/* the file exists. warning is generated */
				put_it("%s", convert_output_format("$G %RDCC%n Warning: File $0 exists: use /DCC rename if you dont want to overwrite", "%s", p));
			else
			{
				rename_file(p, &Client->description);
				put_it("%s", convert_output_format("$G %RDCC%n Warning: File $0 renamed: $1", "%s %s", p, Client->description));
				description = Client->description;
				do_autog = 1;
			}
		}
		new_free(&tmp); new_free(&p);
	}
  	if (do_auto)
  	{
                if (do_hook(DCC_CONNECT_LIST,"%s CHAT",user))
			put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
				"%s %s %s %s %s %d", update_clock(GET_TIME), "CHAT", user, 
				" ", "already requested connecting...", 0));
  		dcc_chat(user);
  	}
        else if (do_hook(DCC_REQUEST_LIST,"%s %s %s %d",user, type, description, Client->filesize))
	{
/* Thanks, Tychy! (lherron@imageek.york.cuny.edu) */
		if (!dcc_quiet)
		{
	
			char buf[40];
			sprintf(buf, "%2.4g",_GMKv(Client->filesize));
			if (Client->filesize)
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_REQUEST_VAR), 
					"%s %s %s %s %s %s %d %s %s", 
					update_clock(GET_TIME),type,description,user, FromUserHost,
					inet_ntoa(Client->remote),ntohs(Client->remport), 
					_GMKs(Client->filesize), buf));
			else
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_REQUEST_VAR), 
					"%s %s %s %s %s %s %d", update_clock(GET_TIME),type,description,
					user, FromUserHost,inet_ntoa(Client->remote),ntohs(Client->remport)));
			
		}
		if (CType == DCC_CHAT)
		{
			if (get_int_var(BOT_MODE_VAR))
			{
				dcc_open(Client);
				message_from(NULL, LOG_CRAP);
				return;
			}
			else
			{
				bitchsay("Type /chat to answer or /nochat to close");
				malloc_sprintf(&last_chat_req, "%s", user);
			}
		}
	}

	if (CType == DCC_BOTMODE && get_string_var(BOT_PASSWD_VAR))
	{
		if (((Client->encrypt && !strcmp(get_string_var(BOT_PASSWD_VAR), Client->encrypt))) || !Client->encrypt)
			dcc_open(Client);
		message_from(NULL, LOG_CRAP);
		return;
	}
	if ((get_int_var(DCC_AUTOGET_VAR)) && 
	    (Client->filesize/1024) < get_int_var(MAX_AUTOGET_SIZE_VAR) 
	    && (!my_stricmp(type, "SEND") || !my_stricmp(type, "RESEND")))
	{
		char *thebuf = NULL;
		if (!Client->filesize)
		{
			put_it("%s", convert_output_format("$G %RDCC Caution Filesize is 0!! No Autoget", NULL, NULL));
			message_from(NULL, LOG_CRAP);
			return;
		}
		malloc_sprintf(&thebuf, "%s %s", user, description);
		if (((CType == DCC_REGETFILE) || do_autog) && !dcc_quiet)
		{
			char *prompt;
			prompt = m_strdup(convert_output_format(get_string_var(CDCC_PROMPT_VAR), NULL, NULL));
			put_it("%s", convert_output_format("$G $3- Auto-$0ting file %C$1%n from %K[%C$2%K]", "%s %s %s %s", (CType == DCC_REGETFILE) ? "reget":"get", description, user, prompt));
			new_free(&prompt);
		}
		if (do_autog)
		{
			if (CType == DCC_REGETFILE)
				dcc_regetfile(thebuf);
			else 
				dcc_getfile(thebuf);
		}
		new_free(&thebuf);
	}
	message_from(NULL, LOG_CRAP);
	return;
}

static	void		process_incoming_chat (DCC_list *Client)
{
	struct	sockaddr_in	remaddr;
	int	sra;
	char	tmp[MAX_DCC_BLOCK_SIZE + 1];
	char	*s = NULL, *bufptr = NULL;
	char	*buf = NULL;
	u_32int_t	bytesread;
	int	old_timeout;
	int	len = 0;
	int	type = Client->flags & DCC_TYPES;
	
	if (Client->flags & DCC_WAIT)
	{
		sra = sizeof(struct sockaddr_in);
		Client->write = accept(Client->read, (struct sockaddr *) &remaddr, &sra);
		FD_CLR(Client->read, &readables);
		close(Client->read);
		Client->read = -1;
		if ((Client->read = Client->write) > 0)
			FD_SET(Client->read, &readables);
		else
		{
			put_it("%s", convert_output_format("$G %RDCC error: accept() failed. punt!!", NULL, NULL));
			Client->flags |= DCC_DELETE;
			return;
		}
		Client->flags &= ~DCC_WAIT;
		Client->flags |= DCC_ACTIVE;
                if (do_hook(DCC_CONNECT_LIST, "%s CHAT %s %d", Client->user,
                         inet_ntoa(remaddr.sin_addr)), ntohs(remaddr.sin_port))
			put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
				"%s %s %s %s %s %d", update_clock(GET_TIME),
				type == DCC_BOTMODE ? "BOT":"CHAT", 
				Client->user, Client->userhost?Client->userhost:"u@h",
				inet_ntoa(remaddr.sin_addr),ntohs(remaddr.sin_port)));
		if ((type == DCC_BOTMODE) && Client->encrypt)
			new_free(&Client->encrypt);
		get_time(&Client->starttime);
		return;
	}
	old_timeout = dgets_timeout(1);
        s = Client->buffer;
        bufptr = tmp;
        if (s && *s)
        {
                len = strlen(s);
		if (len > MAX_DCC_BLOCK_SIZE - 1)
		{
			put_it("%s", convert_output_format("$G %RDCC buffer overrun. Data lost", NULL, NULL));
			new_free(&(Client->buffer));
		} 
		else
		{ 
	                strncpy(tmp, s, len);
			bufptr += len;
		}
        }
	bytesread = dgets(bufptr, dccBlockSize() - len, Client->read, NULL);
	(void) dgets_timeout(old_timeout);
	switch (bytesread)
	{
		case -1:
        	        add_to_dcc_buffer(Client, tmp);
                	return;
		case 0:
		{
			char *real_tmp = ((dgets_errno == -1) ? "Remote End Closed Connection" : strerror(dgets_errno));
	                if (do_hook(DCC_LOST_LIST, "%s CHAT %s", Client->user, real_tmp))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), 
					"%s %s %s %s", update_clock(GET_TIME), 
					type == DCC_BOTMODE? "BOT":"CHAT", 
					Client->user, real_tmp));
			FD_CLR(Client->read, &readables);
			close(Client->read);
			Client->read = Client->write = -1;
			Client->flags |= DCC_DELETE;
			return;
		}
		default:
		{
			char userhost[BIG_BUFFER_SIZE+1];
			char equal_nickname[NICKNAME_LEN+4];
		
			new_free(&Client->buffer);
			tmp[strlen(tmp) - 1]='\0';
			my_decrypt(tmp, strlen(tmp), Client->encrypt);
			Client->bytes_read += bytesread;
			message_from(Client->user, LOG_DCC);
			malloc_strcpy(&buf, tmp);

			if ((Client->flags & DCC_TYPES) == DCC_BOTMODE)
			{
				if (Client->read > -1)
					handle_dcc_bot(Client->read, buf);
			}
			else 
			{
				if (*buf == '.')
				{
					if (!check_tcl_dcc(buf+1, Client->user, Client->userhost?Client->userhost:"", Client->read, Client))
						dcc_printf(Client->write, "Unknown command\n");
				}
				else 
				{
					strcpy(userhost, "Unknown@");
					strcat(userhost, inet_ntoa(remaddr.sin_addr));
					FromUserHost = userhost;
					strcpy(equal_nickname, "=");
					strcat(equal_nickname, Client->user);
					if (!strncmp(tmp, "CTCP_MESSAGE ", strlen("CTCP_MESSAGE ")))
						do_ctcp(equal_nickname, get_server_nickname(-1), stripansicodes(tmp + strlen("CTCP_MESSAGE ")));
					else if (!strncmp(tmp, "CTCP_REPLY ", strlen("CTCP_REPLY ")))
						do_notice_ctcp(equal_nickname, get_server_nickname(-1), stripansicodes(tmp + strlen("CTCP_REPLY ")));
					else if (Client->in_dcc_chat)
						handle_tcl_chan(Client->read, Client->user, Client->userhost, tmp);
					else if (do_hook(DCC_CHAT_LIST, "%s %s", Client->user, tmp))
					{
						addtabkey(equal_nickname, 0);
						put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CHAT_VAR), 
							"%s %s %s %s", update_clock(GET_TIME), Client->user, Client->userhost?Client->userhost:"u@h", tmp));
					}
				}
			}
		}
	}
	message_from(NULL, LOG_CURRENT);
	new_free(&buf);
}

#ifdef __STDC__
static	void		process_incoming_listen (DCC_list *Client)
#else
static void process_incoming_listen(Client)
	DCC_list	*Client;
#endif
{
	struct	sockaddr_in	remaddr;
	int	sra;
	char	FdName[10];
	DCC_list	*NewClient;
	int	new_socket;
	struct	hostent	*hp;
#if defined(__linux__) || defined(__sgi)
	const char	*Name;
#else
	char	*Name;
#endif

	sra = sizeof(struct sockaddr_in);
	new_socket = accept(Client->read, (struct sockaddr *) &remaddr, &sra);
	if (new_socket < 0)
	{
		put_it("%s", convert_output_format("$G %RDCC error: accept() failed. punt!!", NULL, NULL));
		return;
	}
	if (0 != (hp = gethostbyaddr((char *)&remaddr.sin_addr,
	    sizeof(remaddr.sin_addr), remaddr.sin_family)))
		Name = hp->h_name;
	else
		Name = inet_ntoa(remaddr.sin_addr);
	sprintf(FdName, "%d", new_socket);
	NewClient = dcc_searchlist((char *)Name, FdName, DCC_RAW, 1, NULL, NULL, 0);
	get_time(&NewClient->starttime);
	NewClient->read = NewClient->write = new_socket;
	FD_SET(NewClient->read, &readables);
	NewClient->remote = remaddr.sin_addr;
	NewClient->remport = remaddr.sin_port;
	NewClient->flags |= DCC_ACTIVE;
	NewClient->bytes_read = NewClient->bytes_sent = 0L;
	if (do_hook(DCC_RAW_LIST, "%s %s N %d", NewClient->user,
						NewClient->description,
						Client->write))
	        if (do_hook(DCC_CONNECT_LIST,"%s RAW %s %d", NewClient->user,
        	                                             NewClient->description,
                	                                     Client->write))
			put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
				"%s %s %s %s %s %d", update_clock(GET_TIME), 
				"RAW", NewClient->user, 
				Client->userhost? Client->userhost:"u@h", 
				NewClient->description, Client->write));
}

#ifdef __STDC__
static	void		process_incoming_raw (DCC_list *Client)
#else
static void process_incoming_raw(Client)
	DCC_list	*Client;
#endif
{
	char	tmp[MAX_DCC_BLOCK_SIZE + 1];
	char 	*s, *bufptr;
	u_32int_t bytesread;
	int     old_timeout;
	int	len =  0;
	
        s = Client->buffer;
        bufptr = tmp;
        if (s && *s)
        {
		len = strlen(s);
		if (len > MAX_DCC_BLOCK_SIZE - 1)
		{
			put_it("%s", convert_output_format("$G %RDCC raw buffer overrun. Data lost", NULL, NULL));
			new_free(&Client->buffer);
		}
		else
		{ 
	                strncpy(tmp, s, len);
        	        bufptr += len;
		}
        }
        old_timeout = dgets_timeout(1);
	switch(bytesread = dgets(bufptr, dccBlockSize() - len, Client->read, NULL))
	{
	case -1:
	{
                add_to_dcc_buffer(Client, tmp);
                return;
	}
	case 0:
	{
		if (do_hook(DCC_RAW_LIST, "%s %s C", Client->user, Client->description))
	       		if (do_hook(DCC_LOST_LIST,"%s RAW %s", Client->user, Client->description))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), "RAW", Client->user, Client->description));
		FD_CLR(Client->read, &readables);
		close(Client->read);
		Client->read = Client->write = -1;
		Client->flags |= DCC_DELETE;
		(void) dgets_timeout(old_timeout);
		return;
	}
	default:
	{
		new_free(&Client->buffer);
		tmp[strlen(tmp) - 1] = '\0';
		Client->bytes_read += bytesread;
		if (do_hook(DCC_RAW_LIST, "%s %s D %s",	Client->user, Client->description, tmp))
			say("Raw data on %s from %s: %s", Client->user, Client->description, tmp);
		dgets_timeout(old_timeout);
		return;
	}
	}
}


/* Now that ive mucked this function up, i should go back and fix it. */
#ifdef __STDC__
static void	process_outgoing_file (DCC_list *Client, int readwaiting)
#else
static void process_outgoing_file(Client, readwaiting)
	register DCC_list	*Client;
	int		readwaiting;
#endif
{
	struct	sockaddr_in	remaddr;
	int			sra;
	char			tmp[MAX_DCC_BLOCK_SIZE+1];
	u_32int_t		bytesrecvd = 0;
	int			bytesread = 0;
	char			*Type;
	unsigned char 		packet[BIG_BUFFER_SIZE/8];
	struct  transfer_struct *received;
	
	Type = dcc_types[Client->flags & DCC_TYPES];
	if (Client->flags & DCC_WAIT)
	{
		sra = sizeof(struct sockaddr_in);
		Client->write = accept(Client->read, (struct sockaddr *) &remaddr, &sra);
		if ((Client->flags & DCC_RESENDOFFER) == DCC_RESENDOFFER)
		{
			recv(Client->write, packet, sizeof(struct transfer_struct), 0);
			received = (struct transfer_struct *)packet;
			if (byteordertest() != received->byteorder)
			{
				/* the packet sender orders bytes differently than us,
				 * reverse what they sent to get the right value 
				 */
				Client->transfer_orders.packet_id = 
					((received->packet_id & 0x00ff) << 8) | 
					((received->packet_id & 0xff00) >> 8);

				 Client->transfer_orders.byteoffset = 
					((received->byteoffset & 0xff000000) >> 24) |
					((received->byteoffset & 0x00ff0000) >> 8) |
					((received->byteoffset & 0x0000ff00) << 8) |
					((received->byteoffset & 0x000000ff) << 24);
			}
			else
				memcpy(&Client->transfer_orders,packet,sizeof(struct transfer_struct));

			if (Client->transfer_orders.packet_id != DCC_PACKETID)
				put_it("%s", convert_output_format("$G %RDCC%n reget packet is invalid!!", NULL, NULL));
			else
				put_it("%s", convert_output_format("$G %RDCC%n reget starting at $0", "%d", Client->transfer_orders.byteoffset));
		}

		FD_CLR(Client->read, &readables);
		close(Client->read);

		if ((Client->read = Client->write) >= 0)
		{
			FD_SET(Client->read, &readables);
/*CDE */		FD_SET(Client->write, &writables);
		}
		else
		{
			put_it("%s", convert_output_format("$G %RDCC error: accept() failed. punt!!", NULL, NULL));

			Client->flags |= DCC_DELETE;
			if (get_to_from(Type) != -1 && dcc_active_count)
				dcc_active_count--;
			return;
		}
#if defined(NON_BLOCKING_CONNECTS)
		if (get_int_var(DCC_FAST_VAR))
			set_non_blocking(Client->write);
#endif
		Client->flags &= ~DCC_WAIT;
		Client->flags |= DCC_ACTIVE;
		Client->eof = 0;
		get_time(&Client->starttime);
		if ((Client->file = open(Client->description, O_RDONLY)) == -1)
		{
			put_it("%s", convert_output_format("$G %RDCC%n Unable to open $0: $1", "%s %s", Client->description, errno ? sys_errlist[errno] : "Unknown Host"));
			FD_CLR(Client->read, &readables);
			FD_CLR(Client->write, &writables);
			if (get_to_from(Type) != -1 && dcc_active_count)
				dcc_active_count--;
			close(Client->read);
			Client->read = Client->write = (-1);
			Client->flags |= DCC_DELETE;
			return;
		}
		if ((Client->flags & DCC_RESENDOFFER) == DCC_RESENDOFFER)		
		{
			lseek(Client->file, Client->transfer_orders.byteoffset, SEEK_SET);
/*			Client->bytes_sent = Client->transfer_orders.byteoffset;*/
			errno = 0;
			if (do_hook(DCC_CONNECT_LIST,"%s RESEND %s %d",Client->user,
				inet_ntoa(remaddr.sin_addr), ntohs(remaddr.sin_port)))
				if (!dcc_quiet)
					put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
						"%s %s %s %s %s %d %d", update_clock(GET_TIME), "RESEND", 
						Client->user, Client->userhost?Client->userhost:"u@h", inet_ntoa(remaddr.sin_addr),
						ntohs(remaddr.sin_port), Client->transfer_orders.byteoffset));
		}
		else
		{
			if (do_hook(DCC_CONNECT_LIST,"%s SEND %s %d %s %d",Client->user,
				inet_ntoa(remaddr.sin_addr), ntohs(remaddr.sin_port), Client->description, Client->filesize))
				if (!dcc_quiet)
					put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_CONNECT_VAR), 
						"%s %s %s %s %s %d %d", update_clock(GET_TIME), "SEND", 
						Client->user, Client->userhost?Client->userhost:"u@h", inet_ntoa(remaddr.sin_addr),
						ntohs(remaddr.sin_port), Client->transfer_orders.byteoffset));
			if (Client->transfer_orders.byteoffset)
				lseek(Client->file, Client->transfer_orders.byteoffset, SEEK_SET);
		}
	}
	else if (readwaiting)
	{ 
		if (read(Client->read, (char *)&bytesrecvd, sizeof(u_32int_t)) < sizeof(u_32int_t))
		{
                        if (do_hook(DCC_LOST_LIST,"%s SEND %s CONNECTION LOST",
                                Client->user, Client->description))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), "SEND", Client->user, Client->description));
			FD_CLR(Client->read, &readables);
			FD_CLR(Client->write, &writables);
			close(Client->read);
			if (get_to_from(Type) != -1 && dcc_active_count)
				dcc_active_count--;
			Client->read = Client->write = (-1);
			Client->flags |= DCC_DELETE;
			close(Client->file);
			dcc_update_stats(Client);
			status_update(1);
			return;
		}
		else if (ntohl(bytesrecvd) != Client->bytes_sent)
			return;
	}

	if ((bytesread = read(Client->file, tmp, dccBlockSize())) != 0)
	{
		my_encrypt(tmp, bytesread, Client->encrypt);
		send(Client->write, tmp, bytesread, 0);
		Client->packets_transfer++;
		Client->bytes_sent += bytesread;
		update_transfer_buffer("");
		if (!(Client->packets_transfer % 20))
			status_update(1);
	} 
	else if (!readwaiting)
	{
		Client->eof = 1;
		return;
	}
	else
		DCC_close_filesend(Client, "SEND");
}

#ifdef __STDC__
static	void		process_incoming_file (DCC_list *Client)
#else
static void process_incoming_file(Client)
	register DCC_list	*Client;
#endif
{
	char		tmp[MAX_DCC_BLOCK_SIZE+1];
	u_32int_t	bytestemp;
	int		bytesread;
	char		*Type;
	
	Type = dcc_types[Client->flags & DCC_TYPES];
	if ((bytesread = read(Client->read, tmp, dccBlockSize())) <= 0)
	{
		if (Client->bytes_read + Client->transfer_orders.byteoffset < Client->filesize)
			put_it("%s", convert_output_format("$G %RDCC get to $0 lost: Remote peer closed connection", "%s", Client->user));
		DCC_close_filesend(Client, "GET");
	}
	else
	{
		my_decrypt(tmp, bytesread, Client->encrypt);
#ifdef WINNT
	        write((HANDLE)_get_osfhandle(Client->file), tmp, bytesread);
#else
		write(Client->file, tmp, bytesread);
#endif

		Client->bytes_read += bytesread;
		bytestemp = htonl(Client->bytes_read);
		write(Client->write, (char *)&bytestemp, sizeof(u_32int_t));
		Client->packets_transfer++;

/* TAKE THIS OUT IF IT CAUSES PROBLEMS */
		if ((Client->filesize) && (Client->bytes_read > Client->filesize))
		{
			put_it("%s", convert_output_format("$G %RDCC%n Warning: incoming file is larger than the handshake said", NULL, NULL));
			put_it("%s", convert_output_format("$G %RDCC%n Warning: GET: closing connection", NULL, NULL));
			FD_CLR(Client->read, &readables);
			FD_CLR(Client->write, &writables);
			close(Client->read);
			if (dcc_active_count)
				dcc_active_count--;
			close(Client->file);
			Client->read = Client->write = -1;
			Client->flags |= DCC_DELETE;
			dcc_update_stats(Client);
			status_update(1);
			return;
		}
		update_transfer_buffer("");
		if (!(Client->packets_transfer % 20))
			status_update(1);
	}
}

/* flag == 1 means show it.  flag == 0 used by redirect (and /ctcp) */

#ifdef __STDC__
extern void	dcc_message_transmit (char *user, char *text, int type, int flag, char *cmd)
#else
extern void dcc_message_transmit(user, text, type, flag, type)
	char	*user;
	char	*text;
	int	type,
		flag;
	char	*cmd;	
#endif
{
	DCC_list	*Client;
	char	tmp[MAX_DCC_BLOCK_SIZE+1];
	int	lastlog_level;
	char	thing = '\0';
	int	list = 0;
	int 	len = 0;
	char	*host = NULL;

	*tmp = 0;
	switch(type)
	{
		case DCC_CHAT:
			host = "chat";
			thing = '=';
			list = SEND_DCC_CHAT_LIST;
			break;
		case DCC_RAW:
			host = next_arg(text, &text);
			if (!host)
			{
				put_it("%s", convert_output_format("$G %RDCC%n No host specified for DCC RAW", NULL, NULL));
				return;
			}
			break;
	}
	if (!(Client = dcc_searchlist(host, user, type, 0, NULL, NULL, 1)) || !(Client->flags & DCC_ACTIVE))
	{
		put_it("%s", convert_output_format("$G %RDCC No active $0:$1 connection for $2", dcc_types[type], host?host:"(null)", user));
		return;
	}
	lastlog_level = set_lastlog_msg_level(LOG_DCC);
	message_from(Client->user, LOG_DCC);

	/*
	 * Check for CTCPs... whee.
	 */
	if (*text == CTCP_DELIM_CHAR)
	{
		if (!strcmp(cmd, "PRIVMSG"))
			strmcpy(tmp, "CTCP_MESSAGE ", dccBlockSize());
		else
			strmcpy(tmp, "CTCP_REPLY ", dccBlockSize());
	}

	strmcat(tmp, stripansicodes(text), dccBlockSize()-3);
	if (!my_strnicmp(tmp, ".chat", strlen(tmp)))
		Client->in_dcc_chat = 1;
	strmcat(tmp, "\n", dccBlockSize()-2); 

	len = strlen(tmp);
	my_encrypt(tmp, len, Client->encrypt);
	write(Client->write, tmp, len);
	Client->bytes_sent += len;

	if (flag)
	{
		if (type != DCC_RAW)
			if (do_hook(list, "%s %s", Client->user, text))
				put_it("%s", convert_output_format(get_string_var(FORMAT_SEND_DCC_CHAT_VAR), "%c %s %s", thing, Client->user, text));
	}
	set_lastlog_msg_level(lastlog_level);
	message_from(NULL, LOG_CURRENT);
	return;
}

#ifdef __STDC__
extern void	dcc_chat_transmit (char *user, char *text, char *type)
#else
extern void dcc_chat_transmit(user, text, type)
	char	*user;
	char	*text;
	char	*type;
#endif
{
	dcc_message_transmit(user, text, DCC_CHAT, 0, type);
}

#ifdef __STDC__
extern void	dcc_bot_transmit (char *user, char *text, char *type)
#else
extern void dcc_chat_transmit(user, text, type)
	char	*user;
	char	*text;
	char	*type;
#endif
{
	dcc_message_transmit(user, text, DCC_BOTMODE, 0, type);
}

#ifdef __STDC__
extern void dcc_chat_transmit_quiet (char *user, char *text, char *type)
#else
extern void dcc_chat_transmit_quiet (user, text, type)
	char *user;
	char *text;
	char * type;
#endif
{
	dcc_message_transmit(user, text, DCC_CHAT, 0, type);
}

#ifdef __STDC__
extern void dcc_chat_crash_transmit (char *user, char *text)
#else
extern void dcc_chat_crash_transmit (user, text)
	char *user;
	char *text;
#endif
{
char buffer[20000];
	DCC_list	*Client;
	char	*host = "chat";

	memset(buffer, ' ', 20000-10);
	buffer[20000-8] = '\0';	
	if (!(Client = dcc_searchlist(host, user, DCC_CHAT, 0, NULL, NULL, 1)) || !(Client->flags&DCC_ACTIVE))
	{
		put_it("%s", convert_output_format("$G %RDCC%n No active DCC $0:$1 connection for $2", "%s %s %s", dcc_types[DCC_CHAT], host?host:"(null)", user));
		return;
	}
	send(Client->write, buffer, strlen(buffer), 0);
	Client->bytes_sent += strlen(buffer);
	return;
}

#ifdef __STDC__
static	void		dcc_send_raw (char *args)
#else
static void dcc_send_raw(args)
	char	*args;
#endif
{
	char	*name;

	if (!(name = next_arg(args, &args)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n No name specified for DCC raw", NULL, NULL));
		return;
	}
	dcc_message_transmit(name, args, DCC_RAW, 1, NULL);
}

/*
 * dcc_time: Given a time value, it returns a string that is in the
 * format of "hours:minutes:seconds month day year" .  Used by 
 * dcc_list() to show the start time.
 */
#ifdef __STDC__
char *dcc_time (time_t time)
#else
char	* dcc_time(time)
	time_t	time;
#endif
{
	struct	tm	*btime;
	char	*buf = NULL;
	static	char	*months[] = 
	{
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	};

	btime = localtime(&time);
	if (time)
		malloc_sprintf(&buf, "%-2.2d:%-2.2d:%-2.2d %s %-2.2d %d", btime->tm_hour,
			btime->tm_min, btime->tm_sec, months[btime->tm_mon],
			btime->tm_mday, btime->tm_year + 1900);
	return buf;
}

#ifdef __STDC__
extern void dcc_list (char *args)
#else
extern void dcc_list(args)
	char	*args;
#endif
{
	DCC_list	*Client;
	static	char	*format =
			"%-5.5s%-3.3s %-9.9s %-8.8s %-20.20s %-8.8s %-8.8s %s";
	unsigned	flags;
	int count = 0;
	char *filename = NULL;
		
	for (Client = ClientList ; Client != NULL ; Client = Client->next)
	{
		char	completed[9];
		char	size[9];
		char	*stime = NULL;

		if (Client->filesize)
		{
			sprintf(completed, "%ld%%",(unsigned long) (Client->bytes_sent?Client->bytes_sent:Client->bytes_read) * 100 / Client->filesize);
			sprintf(size, "%ld", (unsigned long)Client->filesize);
		}
		else
		{
			sprintf(completed, "%ldK", (unsigned long) (Client->bytes_sent ? Client->bytes_sent : Client->bytes_read) / 1024);
			strcpy(size, empty_string);
		}
		stime = Client->starttime.tv_sec ? dcc_time(Client->starttime.tv_sec): "";
		flags = Client->flags;
		if (!dcc_paths)
#ifdef WINNT
			filename = rindex(Client->description, '\\');
#else
			filename = rindex(Client->description, '/');
#endif
		if (!filename)
			filename = Client->description;

		if (!count)
			put_it(format, "Type", " ", "Nick", "Status", "Start time", "Size", "Complete", "Arguments");
		put_it(format,  
				dcc_types[flags&DCC_TYPES],
				Client->encrypt ? "[E]" : "",
				Client->user,
				flags&DCC_DELETE ? "Closed" :
				flags&DCC_ACTIVE ? "Active" : 
				flags&DCC_WAIT ? "Waiting" :
				flags&DCC_OFFER ? "Offered" : "Unknown",
				stime, size, completed, filename);

		if (stime && *stime)
			new_free(&stime);
		count++;
	}
}

void
dcc_glist(args)
	char	*args;
{
DCC_list	*Client = ClientList;

#if 0
static	char	*format =
		"$[3]0%W³%n $[5]1$2%W³%n $[10]3%W³%n $[8]4$[16]5%W³%n $[6]6%W³%n $7";
#endif
static	char	*dformat =
		"#$[2]0 $[6]1%Y$2%n $[11]3 $[25]4 $[7]5 $6";
static	char	*d1format =
		"#$[2]0 $[6]1%Y$2%n $[11]3 $4 $[11]5 $[10]6 $[7]7 $8-";

	unsigned	flags;
	unsigned	count= 0;
	int 		size = 0;
	double		barsize = 0.0, perc = 0.0;
	int 		barlen = BAR_LENGTH;
	char		spec[BIG_BUFFER_SIZE+1];
	
	errno = 0;
	barlen = BAR_LENGTH;
	barsize = 0.0;
	bzero(spec, sizeof(spec)-1);
	
	if (ClientList && do_hook(DCC_HEADER_LIST, "%s %s %s %s %s %s %s", "Dnum","Type","Nick", "Status", "K/s", "File","Encrypt"))
	{
		put_it("%s", convert_output_format("%G#  %W³%n %GT%gype  %W³%n %GN%gick      %W³%n %GP%gercent %GC%gomplete        %W³%n %GK%g/s   %W³%n %GF%gile", NULL, NULL));
		put_it("%s", convert_output_format("%KÄÄ%nÄ%WÄ%nÄ%KÄÄÄÄÄ%nÄ%WÄ%nÄ%KÄÄÄÄÄÄÄÄÄ%nÄ%WÄ%nÄ%KÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%nÄ%WÄ%nÄ%KÄÄÄÄÄ%nÄ%WÄ%nÄ%KÄÄÄÄÄ%nÄ%WÄ%nÄ%KÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ", NULL, NULL));
	}
	for (Client = ClientList ; Client != NULL ; Client = Client->next)
	{
		flags = Client->flags & DCC_TYPES;
		if ( (flags == DCC_FILEOFFER || flags == DCC_FILEREAD || flags == DCC_RESENDOFFER || flags == DCC_REGETFILE))
			if ((Client->flags & DCC_ACTIVE))
				continue;
			
		if (do_hook(DCC_STAT_LIST, "%d %s %s %s %s %s %s", 
				Client->dccnum, dcc_types[flags &DCC_TYPES],
				Client->user,
				Client->flags & DCC_OFFER ? "Offer" :
				Client->flags & DCC_DELETE ? "Close" :
				Client->flags & DCC_ACTIVE ? "Active" :
				Client->flags & DCC_WAIT ? "Wait" :
#ifdef DCC_DCNT_PEND
				Client->flags & DCC_CNCT_PEND ?	"Connect" :
#endif
				"Unknown",
				"N/A", Client->description, Client->encrypt?"E":""))
		{				

			put_it("%s", convert_output_format(dformat, "%d %s %s %s %s %s %s", 
				Client->dccnum, 
				dcc_types[flags & DCC_TYPES],
				Client->encrypt ? "E" : "ÿ",
				Client->user,
				
				Client->flags & DCC_OFFER ?     "Offer" :
				Client->flags & DCC_DELETE ?    "Close" :
				Client->flags & DCC_ACTIVE ?    "Active" :
				Client->flags & DCC_WAIT ?      "Wait" :
#ifdef DCC_DCNT_PEND
				Client->flags & DCC_CNCT_PEND ?	"Connect" :
#endif
								"Unknown",
				"N/A", 
				check_paths(Client->description)));
		}
			
		count++;
	}
	for (Client = ClientList ; Client != NULL ; Client = Client->next)
	{
		char	kilobytes[100];
		time_t	xtime = time(NULL) - Client->starttime.tv_sec;
		double	bytes = Client->bytes_read + Client->bytes_sent + Client->transfer_orders.byteoffset, 
			sent;
		char stats[80];
		int seconds = 0, minutes = 0;
		int iperc = 0; 
		char *_dcc_offer[12] = {"%K±°°°°°°°°°%n",		/*  0 */
					"%K±°°°°°°°°°%n",		/* 10 */
					"%K±²°°°°°°°°%n",		/* 20 */
					"%K±²Û°°°°°°°%n",		/* 30 */
					"%K±²Û%1%K²%0%K°°°°°°%n",	/* 40 */
					"%K±²Û%1%K²±%0%K°°°°°%n",	/* 50 */
					"%K±²Û%1%K²±°%0%K°°°°%n",	/* 60 */
					"%K±²Û%1%K²±°ÿ%0%K°°°%n",	/* 70 */
					"%K±²Û%1%K²±°ÿ%R°%0%K°°%n",	/* 80 */
					"%K±²Û%1%K²±°ÿ%R°±%0%K°%n",	/* 90 */
					"%K±²Û%1%K²±°ÿ%R°±²%n",		/* 100 */
					""};
		char *bar_end;
		*stats = 0;
		*kilobytes = 0;
		perc = 0.0;		
		sent = bytes - Client->transfer_orders.byteoffset;
		flags = Client->flags & DCC_TYPES;

		if ((Client->flags & DCC_WAIT) || ((flags & DCC_FILEOFFER)==0) || ((flags & DCC_FILEREAD) == 0) || ((flags & DCC_RESENDOFFER) == 0) || ((flags & DCC_REGETFILE) == 0))
			continue;
			
		sent /= (double)1024.0;
		if (xtime <= 0)
			xtime = 1;
		sprintf(kilobytes, "%2.4g", sent/(double)xtime);
	
		if ((bytes >= 0) && (Client->flags & DCC_ACTIVE)) 
		{
			if (bytes && Client->filesize >= bytes)
			{
				perc = (100.0 * ((double)bytes)   / (double)(Client->filesize));
				if ( perc > 100.0) perc = 100.0;
				else if (perc < 0.0) perc = 0.0;
				seconds = (int) (( (Client->filesize - bytes) / (bytes / xtime)) + 0.5);
				minutes = seconds / 60;
				seconds = seconds - (minutes * 60);
				if (minutes > 999) {
					minutes = 999;
					seconds = 59;
				}
				if (seconds < 0) seconds = 0;
			} else
				seconds = minutes = perc = 0;
				
			iperc = ((int)perc) / 10;
			barsize = ((double) (Client->filesize)) / (double) barlen;

			size = (int) ((double) bytes / (double)barsize);
	
			if (Client->filesize == 0)
				size = barlen;
			sprintf(stats, "%4.1f", perc);
			sprintf(spec, "%s %s%s %02d:%02d", _dcc_offer[iperc], stats, "%%", minutes, seconds);
			sprintf(spec, "%s", convert_output_format(spec, NULL, NULL));
		}
		
		if (do_hook(DCC_STATF_LIST, "%d %s %s %s %s %s %s", 
			Client->dccnum, dcc_types[flags &DCC_TYPES],
			Client->user,
			Client->flags & DCC_OFFER ? "Offer" :
			Client->flags & DCC_DELETE ? "Close" :
			Client->flags & DCC_ACTIVE ? "Active" :
			Client->flags & DCC_WAIT ? "Wait" :
#ifdef DCC_DCNT_PEND	
			Client->flags & DCC_CNCT_PEND ?	"Connect" :
#endif
			"Unknown",
			kilobytes, check_paths(Client->description), Client->encrypt?"E":""))
		{
			put_it("%s", convert_output_format(get_int_var(DISPLAY_ANSI_VAR)?d1format:dformat, "%d %s %s %s %s %s %s", 
				Client->dccnum, 
				dcc_types[flags & DCC_TYPES],
				Client->encrypt ? "E":"ÿ",
				Client->user,
				Client->flags & DCC_OFFER ? "Offer" :
				Client->flags & DCC_DELETE ? "Close" :
				Client->flags & DCC_ACTIVE ? (get_int_var(DISPLAY_ANSI_VAR) ? spec : "Active"):
				Client->flags & DCC_WAIT ? "Wait" :
#ifdef DCC_DCNT_PEND
				Client->flags & DCC_CNCT_PEND ?	"Connect" :
#endif	
				"?????",
				kilobytes, 
				strip_path(Client->description)));
		}

		if (do_hook(DCC_STATF1_LIST, "%s %ld %ld %d %d", 
			stats,(unsigned long)bytes, (unsigned long)Client->filesize, 
			minutes, seconds) && !get_int_var(DISPLAY_ANSI_VAR))
		{
			sprintf(stats, "%4.1f%% (%ld of %ld bytes)", perc, (unsigned long)bytes, (unsigned long)Client->filesize);
			strcpy( spec, "\002[\026");
			sprintf(spec+3, "%*s", size+1, " ");
			bar_end = spec + (strlen(spec));
			sprintf(bar_end, "%*s", barlen-size+1, " ");
			strcat(spec, "\026\002]\002 ETA %02d:%02d");
			memcpy((spec+(((BAR_LENGTH+2) / 2) - (strlen(stats) / 2))), stats, strlen(stats));
			if (size < barlen)
			{
#if defined(HAVE_MEMMOVE)
				memmove(bar_end+1, bar_end, strlen(bar_end));
#else
				mem_move(bar_end+1, bar_end, strlen(bar_end));
#endif	
				*bar_end = '\002';
			}
			put_it(spec, minutes, seconds);	
		}
		count++;
	}
	if (ClientList && count)
		do_hook(DCC_POST_LIST, "%s %s %s %s %s %s %s", "DCCnum","Type","Nick", "Status", "K/s", "File","Encrypt");
	if (count == 0)
		put_it("%s", convert_output_format("$G %RDCC%n Nothing on DCC list.", NULL, NULL));
}

static char DCC_reject_type[12];
static char DCC_reject_description[40];

#ifdef __STDC__
static	void 	output_reject_ctcp (char *notused, char *nicklist)
#else
static void output_reject_ctcp (notused, nicklist)
char *notused, *nicklist;
#endif
{
	if (nicklist && *nicklist && *DCC_reject_description)
		send_ctcp(CTCP_NOTICE, nicklist, CTCP_DCC,
			"REJECT %s %s", DCC_reject_type, DCC_reject_description);
                                                                                        strcpy(DCC_reject_description, empty_string);
	strcpy(DCC_reject_type, empty_string);
}

/* added by Patch */
void
dcc_close_client_num(unsigned int closenum)
{
	DCC_list	*Client, *next;
	unsigned	flags;
	char		*Type;
	int		to_from_idx;

	for (Client = ClientList ; Client != NULL ; Client = next)
	{
		flags = Client->flags;
		Type = dcc_types[flags & DCC_TYPES];
		to_from_idx = get_to_from(Type);
		next = Client->next;

		if (Client->dccnum == closenum)
		{
			if (flags & DCC_DELETE)
				return;
			if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
			{
				if (flags & DCC_ACTIVE && to_from_idx != -1)
					dcc_update_stats(Client);
				if (to_from_idx != -1 && dcc_active_count) dcc_active_count--;
				if (Client->read > -1) FD_CLR(Client->read, &readables);
				close(Client->read);
				if (Client->file)
					close(Client->file);
			}

			if (do_hook(DCC_LOST_LIST, "%s %s %s", Client->user, Type, 
				Client->description? Client->description : "<any>"))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), Type, Client->user, Client->description));
			dcc_reject_notify(Client->description, Client->user, Type);
			dcc_erase(Client);
			update_transfer_buffer("");
			status_update(1);
			return;
		}
	}

	put_it("%s", convert_output_format("%RDCC%n CLOSE number $0 does not exist","%d", closenum));
}

/* added by Patch */
void
dcc_close_all(void)
{
	DCC_list	*Client, *next;
	unsigned	flags;
	char		*Type;
	int		to_from_idx;

	for (Client = ClientList ; Client != NULL ; Client = next)
	{
		flags = Client->flags;
		Type = dcc_types[flags & DCC_TYPES];
		to_from_idx = get_to_from(Type);
		next = Client->next;
		
		if (flags & DCC_DELETE)
			continue;
		if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
		{
			if (flags & DCC_ACTIVE && to_from_idx != -1)
				dcc_update_stats(Client);
			if (to_from_idx != -1 && dcc_active_count) dcc_active_count--;
			if (Client->read > -1) FD_CLR(Client->read, &readables);
			close(Client->read);
			if (Client->file)
				close(Client->file);
		}

		if (do_hook(DCC_LOST_LIST, "%s %s %s", Client->user, Type, 
			Client->description? Client->description : "<any>"))
			put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), Type, Client->user, Client->description));
		dcc_reject_notify(Client->description, Client->user, Type);
		dcc_erase(Client);
		update_transfer_buffer("");
		status_update(1);
	}
}

/* added by Patch */
void
dcc_close_type_all(char *typestr)
{
	DCC_list	*Client, *next;
	unsigned	flags;
	char		*Type;
	int		to_from_idx;

	to_from_idx = get_to_from(typestr);

	for (Client = ClientList ; Client != NULL ; Client = next)
	{
		flags = Client->flags;
		Type = dcc_types[flags & DCC_TYPES];
		next = Client->next;
		
		if (my_stricmp(Type,typestr) == 0)
		{
			if (flags & DCC_DELETE)
				return;
			if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
			{
				if (flags & DCC_ACTIVE && to_from_idx != -1)
					dcc_update_stats(Client);
				if (to_from_idx != -1 && dcc_active_count) dcc_active_count--;
				if (Client->read > -1) FD_CLR(Client->read, &readables);
				close(Client->read);
				if (Client->file)
					close(Client->file);
			}
			if (do_hook(DCC_LOST_LIST, "%s %s %s", Client->user, Type, 
				Client->description? Client->description : "<any>"))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), Type, Client->user, Client->description));
			dcc_reject_notify(Client->description, Client->user, Type);
			dcc_erase(Client);
			update_transfer_buffer("");
			status_update(1);
		}
	}
}

/* added by Patch */
void
dcc_close_nick_all(char *nickstr)
{
	DCC_list	*Client, *next;
	unsigned	flags;
	char		*Type;
	int		to_from_idx;

	for (Client = ClientList ; Client != NULL ; Client = next)
	{
		flags = Client->flags;
		Type = dcc_types[flags & DCC_TYPES];
		to_from_idx = get_to_from(Type);
		next = Client->next;
		
		if (my_stricmp(Client->user,nickstr) == 0)
		{
			if (flags & DCC_DELETE)
				return;
			if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
			{
				if (flags & DCC_ACTIVE && to_from_idx != -1)
					dcc_update_stats(Client);
				if (to_from_idx != -1 && dcc_active_count) dcc_active_count--;
				FD_CLR(Client->read, &readables);
				close(Client->read);
				if (Client->file)
					close(Client->file);
			}

			if (do_hook(DCC_LOST_LIST, "%s %s %s", Client->user, Type, 
				Client->description? Client->description : "<any>"))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), Type, Client->user, Client->description));
			dcc_reject_notify(Client->description, Client->user, Type);
			dcc_erase(Client);
			update_transfer_buffer("");
			status_update(1);
		}
	}
}

/* added by Patch */
void
dcc_close_type_nick_all(char *typestr, char *nickstr)
{
	DCC_list	*Client, *next;
	unsigned	flags;
	char		*Type;
	int		to_from_idx;

	to_from_idx = get_to_from(typestr);

	for (Client = ClientList ; Client != NULL ; Client = next)
	{
		flags = Client->flags;
		Type = dcc_types[flags & DCC_TYPES];
		next = Client->next;
		
		if (my_stricmp(Type,typestr) == 0 &&
			my_stricmp(Client->user,nickstr) == 0)
		{
			if (flags & DCC_DELETE)
				return;
			if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
			{
				if (flags & DCC_ACTIVE && to_from_idx != -1)
					dcc_update_stats(Client);
				if (to_from_idx != -1 && dcc_active_count) dcc_active_count--;
				FD_CLR(Client->read, &readables);
				close(Client->read);
				if (Client->file)
					close(Client->file);
			}
			if (do_hook(DCC_LOST_LIST, "%s %s %s", Client->user, Type, 
				Client->description? Client->description : "<any>"))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), Type, Client->user, Client->description));
			dcc_reject_notify(Client->description, Client->user, Type);
			dcc_erase(Client);
			update_transfer_buffer("");
			status_update(1);
		}
	}
}

/* added by Patch */
void
dcc_close_filename(char *filename, char *user, char *Type, int CType)
{
	DCC_list	*Client;
	unsigned	flags;
	int		to_from_idx;

	to_from_idx = get_to_from(Type);
	if ((Client = dcc_searchlist(filename, user, CType, 0, filename, NULL, -1)))
	{
		flags = Client->flags;
		if (flags & DCC_DELETE)
			return;
		if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
		{
			if (flags & DCC_ACTIVE && to_from_idx != -1)
				dcc_update_stats(Client);
			if (to_from_idx != -1 && dcc_active_count) dcc_active_count--;
		}

		if (do_hook(DCC_LOST_LIST, "%s %s %s", Client->user, Type, 
			Client->description? Client->description : "<any>"))
			put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), Type, Client->user, Client->description));
		dcc_reject_notify(Client->description, Client->user, Type);
		dcc_erase(Client);
		update_transfer_buffer("");
		status_update(1);
	}
	else
		put_it("%s", convert_output_format("$G %RDCC%n No DCC $0:$1 to $2 found","%s %s %s", Type, filename?filename:"(null)",user));
}

/* completely rewritten by Patch */
void
dcc_close(char *args)
{
	char		*Type;
	char		*user;
	char		*description;
	int		CType;
	unsigned int 	closenum;

	Type = next_arg(args, &args);
	user = next_arg(args, &args);
	description = next_arg(args, &args);

/*****************************************************************************
 DCC CLOSE #all or #number
*****************************************************************************/

	if (!Type)
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must specify a type|dcc_num for DCC close", NULL, NULL));
		return;
	}

	if ((Type[0] == '#' && !user && *(Type+1) && my_atol(Type+1)) || !my_stricmp(Type, "-all"))
	{
		if (!my_stricmp(Type,"-all"))
		{
			dcc_close_all();
			dcc_active_count = 0;
			status_update(1);
			return;
		}

		closenum = atol(Type+1);

		if (closenum == 0)
		{
			put_it("%s", convert_output_format("$G %RDCC%n close invalid number", NULL, NULL));
			return;
		}

		dcc_close_client_num(closenum);
		status_update(1);
		return;
	}

/*****************************************************************************
 DCC CLOSE SEND|GET|RESEND|REGET|nick #all
*****************************************************************************/

	if (!user)
	{
		put_it("%s", convert_output_format("$G %RDCC%n specify a type and a nick for DCC close", NULL, NULL));
		return;
	}

	for (CType = 0; dcc_types[CType] != NULL; CType++)
		if (!my_stricmp(Type, dcc_types[CType])) break;

	if (user[0] == '#' && !description && (my_atol(user+1) || my_stricmp(user+1, "all")==0))
	{
		if (my_stricmp(user+1,"all") == 0)
		{
			if (!dcc_types[CType])
				dcc_close_nick_all(Type);
			else
				dcc_close_type_all(Type);
			status_update(1);
			return;
		}
		put_it("%s", convert_output_format("$G %RDCC%n close invalid number", NULL, NULL));
		return;
	}

/*****************************************************************************
 DCC CLOSE SEND|GET|RESEND|REGET nick #all|filename
*****************************************************************************/

	if (description && *description == '-')
	{
		if (!my_stricmp(description,"-all"))
			dcc_close_type_nick_all(Type,user);
		else 
		put_it("%s", convert_output_format("$G %RDCC%n CLOSE invalid description", NULL, NULL));
		return;
	}

	if (dcc_types[CType])
		dcc_close_filename(description, user, Type, CType);
	else
		put_it("%s", convert_output_format("$G %RDCC%n Unknown type [$0]", "%s", dcc_types[CType]));
}

void dcc_reject_notify(char *description, char *user, char *Type)
{
	strcpy(DCC_reject_description, description ? description : "(null)");
	if (!my_stricmp(Type, "SEND"))
		strcpy(DCC_reject_type, "GET");
	else if (!my_stricmp(Type, "GET"))
		strcpy(DCC_reject_type, "SEND");
	else if (!my_stricmp(Type, "RESEND"))
		strcpy(DCC_reject_type, "REGET");
	else if (!my_stricmp(Type, "REGET"))
		strcpy(DCC_reject_type, "RESEND");
	else
		strcpy(DCC_reject_type, Type);
	if (*user == '=')
		user++;
	add_ison_to_whois (user, output_reject_ctcp);
}

#ifdef __STDC__
extern void dcc_reject (char *from, char *type, char *args)
#else
extern void dcc_reject (from, type, args)
char *from, *type, *args;
#endif
{
	DCC_list	*Client;
	unsigned	flags;
	char	*description;
	int	CType;

	for (CType = 0; dcc_types[CType] != NULL; CType++)
		if (!my_stricmp(type, dcc_types[CType]))
			break;

	if (!dcc_types[CType])
		return;

	description = next_arg(args, &args);

	if ((Client = dcc_searchlist(NULL, from, CType, 0, description, NULL, -1)))
	{
		flags = Client->flags;
		if (flags & DCC_DELETE)
			return;
		if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
		{
			FD_CLR(Client->read, &readables);
			close(Client->read);
			if (Client->file)
				close(Client->file);
		}
                if (do_hook(DCC_LOST_LIST,"%s %s %s REJECTED", from, type, description ? description : "<any>"))
			put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_ERROR_VAR), "%s %s %s %s", update_clock(GET_TIME), type, Client->user, Client->description));
		dcc_erase(Client);
		update_transfer_buffer("");
		status_update(1);
	}
}

#ifdef __STDC__
static	void		dcc_rename (char *args)
#else
static void dcc_rename(args)
	char	*args;
#endif
{
	DCC_list	*Client;
	char	*user;
	char	*description;
	char	*newdesc;
	char	*temp;
	
	if (!(user = next_arg(args, &args)) || !(temp = next_arg(args, &args)))
	{
		put_it("%s", convert_output_format("$G %RDCC%n You must specify a nick and a new filename", NULL, NULL));
		return;
	}
	if ((newdesc = next_arg(args, &args)) != NULL)
		description = temp;
	else
	{
		newdesc = temp;
		description = NULL;
	}

	if ((Client = dcc_searchlist(description, user, DCC_FILEREAD, 0, NULL, NULL, 0)))
	{
		/* Is this needed now? */
		if (!(Client->flags & DCC_OFFER))
		{
			put_it("%s", convert_output_format("$G %RDCC Too late to rename that file", NULL, NULL));
			return;
		}
		new_free(&(Client->description));
		malloc_strcpy(&(Client->description), newdesc);
		put_it("%s", convert_output_format("$G %RDCC File $0 from $1 rename to $2", "%s %s %s", description?description:"(null)", user, newdesc));
	}
	else
		put_it("%s", convert_output_format("$G %RDCC No file $0 from $1 found, or it's too late to rename", "%s %s", description?description:"(null)", user));
}

/*
 * close_all_dcc:  We call this when we create a new process so that
 * we don't leave any fd's lying around, that won't close when we
 * want them to..
 */
extern void close_all_dcc _((void))
{
	DCC_list *Client;

	while ((Client = ClientList))
		dcc_erase(Client);
}


#ifdef __STDC__
static void add_to_dcc_buffer(DCC_list *Client, char *buf)
#else
static        void add_to_dcc_buffer(Client, buf)
       DCC_list        *Client;
       char    *buf;
#endif
{
       if (buf && *buf)
       {
               if (Client->buffer)
                       malloc_strcat(&Client->buffer, buf);
               else
                       malloc_strcpy(&Client->buffer, buf);
       }
}

/* Looks for the dcc transfer that is "current" (last recieved data)
 * and returns information for it
 */
extern char *DCC_get_current_transfer _((void))
{
	return DCC_current_transfer_buffer;
}

#ifdef __STDC__
void	chat (char *command, char *args, char *subargs)
#else
void	chat (command, args, subargs)
char *command;
char *args;
char *subargs;
#endif
{
int no_chat = 0;
	if (!my_strnicmp(command, "NOC", 3))
		no_chat = 1;
	if (args && *args)
	{
		char *tmp = NULL;
		if (no_chat)
			malloc_sprintf(&tmp, "CLOSE CHAT %s", args);
		else
			malloc_sprintf(&tmp, "CHAT %s", args);
		process_dcc(tmp);
		new_free(&tmp);
	}
	else if (last_chat_req)
	{
		DCC_list *chat_req = NULL;
		if ((chat_req = dcc_searchlist("chat", last_chat_req, DCC_CHAT, 0, NULL, NULL, no_chat?1:0)))
		{
			if (no_chat)
			{
				dcc_close_filename(NULL, last_chat_req, "CHAT", DCC_CHAT);
				new_free(&last_chat_req);
			}
			else	
			{
				char nick[BIG_BUFFER_SIZE];
				dcc_open(chat_req);
				sprintf(nick, "=%s", chat_req->user);
				addtabkey(nick, 0);
			}
		}
		else
			bitchsay("Error occurred");
	} else 
		userage(command, "[nick]");
}


#ifdef __STDC__
static void DCC_close_filesend (DCC_list *Client, char *type)
#else
static void DCC_close_filesend (Client, type)
DCC_list *Client;
char *type;
#endif
{
	char	lame_ultrix[30];	/* should be plenty */
	char	lame_ultrix2[30];
	char	lame_ultrix3[30];
	char	buffer[200];
	char 	*tofrom = NULL;
	time_t xtime;
	double xfer;

	xtime = time_diff(Client->starttime, get_time(NULL));
	xfer = (double)(Client->bytes_sent ? Client->bytes_sent : Client->bytes_read);

	if (xfer <= 0)
		xfer = 1;
	if (xtime <= 0)
		xtime = 1;
	sprintf(lame_ultrix, "%2.4g", (xfer/ 1024.0 / xtime));
	
	/* Cant pass %g to put_it (lame ultrix/dgux), fix suggested by sheik. */

	sprintf(lame_ultrix2, "%2.4g%s", _GMKv(xfer), _GMKs(xfer));

	sprintf(lame_ultrix3, "%2.4g", (double)xtime);

	sprintf(buffer, "%%s %s %%s %%s TRANSFER COMPLETE", type);
	switch(get_to_from(dcc_types[Client->flags&DCC_TYPES]))
	{
		case 0:
		case 1:
			tofrom = "to";
			break;
		case 2:
		case 3:
			tofrom = "from";
			break;
		default:
			tofrom = "to";
			break;
	}
	if(do_hook(DCC_LOST_LIST,buffer,Client->user, check_paths(Client->description), lame_ultrix))
		put_it("%s", convert_output_format(get_string_var(FORMAT_DCC_LOST_VAR),
		"%s %s %s %s %s %s %s %s %s", update_clock(GET_TIME), type, 
		check_paths(Client->description), lame_ultrix2, tofrom, Client->user, 
		lame_ultrix3, lame_ultrix, "Kb"));

	dcc_update_stats(Client);
	if (get_to_from(dcc_types[Client->flags&DCC_TYPES]) != -1 && dcc_active_count)
		dcc_active_count--;
	if  (!ClientList && dcc_active_count) dcc_active_count = 0; /* A HACK */
	FD_CLR(Client->read, &readables);  
	close(Client->read);
	FD_CLR(Client->write, &writables);
	if (Client->read != Client->write)
		close (Client->write);
	close(Client->file);
	Client->file = Client->read = Client->write = -1;
	Client->flags |= DCC_DELETE;
	*DCC_current_transfer_buffer = 0;
	update_transfer_buffer("");
	status_update(1);
}

char transfer_buffer[BIG_BUFFER_SIZE];

#if defined(__STDC__) && defined(HAVE_STDARG_H)
static void update_transfer_buffer (char *format, ...)
#else
static void update_transfer_buffer (format, arg0, arg1)
char *format, *arg0, *arg1;
#endif
{
	register DCC_list	*Client = ClientList;
	unsigned	count= 0;
	double		perc = 0.0;
	char temp_str[60];
		
	errno = 0;
	*transfer_buffer = 0;
	for (Client = ClientList ; Client; Client = Client->next)
	{
		double	bytes;

		if (((Client->flags & DCC_TYPES) == DCC_RAW) || ((Client->flags & DCC_TYPES) == DCC_RAW_LISTEN) || ((Client->flags & DCC_TYPES) == DCC_CHAT) || ((Client->flags & DCC_TYPES) == DCC_BOTMODE))
			continue;
		if ((Client->flags & DCC_WAIT))

			continue;
		bytes = Client->bytes_read + Client->bytes_sent + Client->transfer_orders.byteoffset;
		if (bytes >= 0) 
		{
			if (Client->filesize >= bytes)
			{
				perc = (100.0 * ((double)bytes)   / (double)(Client->filesize));
				if ( perc > 100.0) perc = 100.0;
				else if (perc < 0.0) perc = 0.0;

			}				
			sprintf(temp_str,"%d%%,",(int) perc);
			strcat(transfer_buffer,temp_str);
		}
		if (count++ > 9)
			break;
	}
	if (count)
	{
		chop(transfer_buffer, 1);
		sprintf(DCC_current_transfer_buffer, "[%s]", transfer_buffer);
	}
	else
		*DCC_current_transfer_buffer = 0;
}

int get_to_from(char *Type) {
	if (my_stricmp(Type, "SEND") == 0)
		return 0;
	else if (my_stricmp(Type, "RESEND") == 0)
		return 1;
	else if (my_stricmp(Type, "GET") == 0)
		return 2;
	else if (my_stricmp(Type, "REGET") == 0)
		return 3;
	else 
		return -1;
}

static void dcc_help1 (char *args)
{
	put_it("%s", convert_output_format("$G %RDCC%n help -", NULL, NULL));
	put_it("%s", convert_output_format("   Active    Stats    List     GList   ?", NULL, NULL));
	put_it("%s", convert_output_format("   Resend/Reget    Send/Get    Chat    Raw   Bot   Close  Rename", NULL, NULL));	
	put_it("%s", convert_output_format("   Auto   Quiet   Paths   Quiet   Overwrite  Auto_Rename", NULL, NULL));
}

static void dcc_show_active( char * args) 
{
	put_it("%s", convert_output_format("$G %RDCC%n  DCC Active = \002$0\002, Limit = \002$1\002", 
		"%d %d", dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR)));
}

static void dcc_set_quiet( char *args ) 
{
	dcc_quiet ^=1;
	put_it("%s", convert_output_format("$G %RDCC%n  DCC Quiet = \002$0\002", "%s", on_off(dcc_quiet)));
}

/* returns the string without the path (if present) */
static char * strip_path(char *str)
{
	char *ptr;

#ifdef WINNT
	ptr = strrchr(str,'\\');
#else
	ptr = strrchr(str,'/');
#endif
	if (ptr == NULL)
		return str;
	else
		return ptr+1;
}

/* returns the string without the path (if present) */
static char *check_paths(char *str)
{
	if (dcc_paths == 0)
		return strip_path(str);
	else
		return str;
}

static void dcc_set_paths(char *args)
{
	dcc_paths ^= 1;
	
	put_it("%s", convert_output_format("$G %RDCC%n  DCC paths is now \002$0\002", "%s", on_off(dcc_paths)));
}	

static void dcc_tog_rename(char *args)
{
int	arename = get_int_var(DCC_AUTORENAME_VAR);
	arename ^= 1;
	set_int_var(DCC_AUTORENAME_VAR, arename);
	put_it("%s", convert_output_format("$G %RDCC%n  DCC auto rename is now \002$0\002", "%s", on_off(get_int_var(DCC_AUTORENAME_VAR))));
}	

static void dcc_overwrite_toggle(char *args)
{
	dcc_overwrite_var ^= 1;
	
	put_it("%s", convert_output_format("  DCC overwrite is now \002$0\002", "%s", on_off(dcc_overwrite_var)));
}	

void dcc_tog_auto(char *args)
{
	int dcc_auto = get_int_var(DCC_AUTOGET_VAR);
	dcc_auto ^= 1;
	set_int_var(DCC_AUTOGET_VAR, dcc_auto);	
	put_it("%s", convert_output_format("  DCC autoget is now \002$0\002", "%s", on_off(dcc_auto)));
}	

void dcc_stats (char *unused)
{
char max_rate_in[20];
char min_rate_in[20];
char max_rate_out[20];
char min_rate_out[20];

	sprintf(max_rate_in, "%6.2f", dcc_max_rate_in/1024.0);
	sprintf(min_rate_in, "%6.2f", ((dcc_min_rate_in != DBL_MAX )?dcc_min_rate_in/1024.0: 0.0));
	sprintf(max_rate_out, "%6.2f", dcc_max_rate_out/1024.0);
	sprintf(min_rate_out, "%6.2f", ((dcc_min_rate_out != DBL_MAX) ? dcc_min_rate_out/1024.0: 0.0));
	if (do_hook(DCC_TRANSFER_STAT_LIST, "%lu %s %s %lu %s %s %lu %u %u %s %s %s %s", 
		(unsigned long)dcc_bytes_in, max_rate_in, min_rate_in,
		(unsigned long)dcc_bytes_out, max_rate_out, min_rate_out,
		dcc_count_stat, dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR),
		on_off(get_int_var(DCC_AUTOGET_VAR)), on_off(dcc_paths), 
		on_off(dcc_quiet), on_off(dcc_overwrite_var)))
	{
		char in[40], out[40];
		sprintf(in, "%lu%s", (unsigned long) _GMKv(dcc_bytes_in),_GMKs(dcc_bytes_in));
		sprintf(out, "%lu%s", (unsigned long) _GMKv(dcc_bytes_out), _GMKs(dcc_bytes_out));

		put_it("%s",convert_output_format("       %GÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ%K[%Cdcc transfer stats%K]%GÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸", NULL));
		put_it("%s",convert_output_format("       %G³                                                                 ³", NULL));
		put_it("%s",convert_output_format("       %G³%gÖÄ%K[%Cx%cferd %Ci%cn%K]%gÄÖ-%K[%Cx%cferd %Co%cut%K]%gÄ·Ä%K[%Ct%cotal %Cf%ciles%K]%gÄÖÄ%K[%Ca%cctive%K]%gÄ·Ä[%Cl%cimit%K]%gÄ·%G³", NULL));
		put_it("%s",convert_output_format("       %G³%gº %W$[-10]0 %gº  %W$[-10]1 %gº    %W$[-10]2 %gº %W$[-8]3 %gº %W$[-7]4 %gº%G³", "%s %s %d %d %d", in, out,dcc_count_stat,dcc_active_count,get_int_var(DCC_SEND_LIMIT_VAR)));
		put_it("%s",convert_output_format("       %G³%gÓÄÄÄÄÄÄÄÄÄÄÄÄ½ÄÄÄÄÄÄÄÄÄÄÄÄÄÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½ÄÄÄÄÄÄÄÄÄÄÓÄÄÄÄÄÄÄÄÄ½%G³", NULL));
		put_it("%s",convert_output_format("       %G³                                                                 ³", NULL));
		put_it("%s",convert_output_format("       %gÖÄÄÄÄ%K[%Ci%cn %Cs%ctats%K]%gÄÄÄÖÄÄÄ%K[%Co%cut %Cs%ctats%K]%gÄÄÄ·ÄÄÄÄÄÄÄÄÄÄ%K[%Ct%coggles%K]%gÄÄÄÄÄÄÄÄÄÄ·", NULL));
		put_it("%s",convert_output_format("       %gº %Cm%nax: %W$[-6]0%n%Rkb/s %gº %Cm%nax: %W$[-6]1%n%Rkb/s %gº   %Ca%nutoget: %W$[-3]2%n   %Cp%naths: %W$[-3]3 %gº", "%s %s %s %s", max_rate_in, max_rate_out, on_off(get_int_var(DCC_AUTOGET_VAR)),on_off(dcc_paths)));
		put_it("%s",convert_output_format("       %gº %Cm%nin: %W$[-6]0%n%Rkb/s %gº %Cm%nin: %W$[-6]1%n%Rkb/s %gº %Co%nverwrite: %W$[-3]2%n   %Cq%nuiet: %W$[-3]3 %gº", "%s %s %s %s", min_rate_in, min_rate_out, on_off(dcc_overwrite_var), on_off(dcc_quiet)));
		put_it("%s",convert_output_format("       %gÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½", NULL));

	}
}

/*
 * only call this on dcc finish
 */
static void dcc_update_stats (DCC_list *Client)
{
time_t	xtime = time_diff(Client->starttime, get_time(NULL));
	
	dcc_count_stat++;
	dcc_bytes_in += Client->bytes_read;
	dcc_bytes_out += Client->bytes_sent;
	if (xtime <= 0)
		xtime = 1;
	if (Client->bytes_read)
	{
		if ((double)Client->bytes_read/(double)xtime > dcc_max_rate_in)
			dcc_max_rate_in = (double)Client->bytes_read/(double)xtime;
		if ((double)Client->bytes_read/ (double)xtime < dcc_min_rate_in)
			dcc_min_rate_in = (double)Client->bytes_read/(double)xtime;	
	}
	if (Client->bytes_sent)
	{
		if ((double)Client->bytes_sent/(double)xtime > dcc_max_rate_out)
			dcc_max_rate_out = (double)Client->bytes_sent/(double)xtime;
		if ((double)Client->bytes_sent/(double)xtime < dcc_min_rate_out)
			dcc_min_rate_out = (double)Client->bytes_sent/ (double)xtime;	
	}
}

unsigned char byteordertest(void)
{
	unsigned short test = DCC_PACKETID;

	if (*((unsigned char *)&test) == ((DCC_PACKETID & 0xff00) >> 8))
		return 0;

	if (*((unsigned char *)&test) == (DCC_PACKETID & 0x00ff))
		return 1;
	return 0;
}

/*
 * This stuff doesnt conform to the protocol.
 * Thanks mirc for disregarding the protocol.
 */
#ifdef MIRC_BROKEN_DCC_RESUME
extern  int doing_msg, doing_notice;

/*
 * Usage: /DCC RESUME <nick> [file] [-e passkey]
 */
#ifdef __STDC__
static	void	dcc_getfile_resume (char *args)
#else
static void 	dcc_getfile_resume (args)
	char	*args;
#endif
{
	char		*user;
	char		*filename = NULL;
	char		*fullname = NULL;
	char		*tmp = NULL;
	DCC_list	*Client;
	char		*passwd = NULL;
	struct stat	sb;
	char		buf[10];

	if (!(user = next_arg(args, &args)))
	{
		say("You must supply a nickname for DCC RESUME");
		return;
	}

	if (args && *args)
	{
		/* Leeme lone, Yoshi. :P */
		if (args[0] != '-' || args[1] != 'e')
			filename = next_arg(args, &args);

		if (args && args[0] == '-' && args[1] == 'e')
		{
			next_arg(args, &args);
			passwd = next_arg(args, &args);
		}
	}



	if (!(Client = dcc_searchlist(filename, user, DCC_FILEREAD, 0, NULL, NULL, 0)))
	{
		if (filename)
			say("No file (%s) offered in SEND mode by %s", filename, user);
		else
			say("No file offered in SEND mode by %s", user);
		return;
	}

	if (get_string_var(DCC_DLDIR_VAR))
#ifdef WINNT
		malloc_sprintf(&tmp, "%s\\%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#else
		malloc_sprintf(&tmp, "%s/%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#endif
	else
		malloc_sprintf(&tmp, "%s", Client->description);

	if (!(fullname = expand_twiddle(tmp)))
		malloc_strcpy(&fullname, tmp);
	/*
	 * This has to be done by hand, we cant use send_ctcp,
	 * because this violates the protocol, and send_ctcp checks
	 * for that.  Ugh.
	 */

	if (stat(fullname, &sb) == -1)
	{
		/* File doesnt exist.  Sheesh. */
		say("DCC RESUME: Cannot use DCC RESUME if the file doesnt exist. [%s|%s]", fullname, strerror(errno));
		return;
	}

	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		say("A previous DCC GET:%s to %s exists", filename?filename:"<any>", user);
		return;
	}

	if (passwd)
		Client->encrypt = m_strdup(passwd);
	Client->bytes_sent = 0L;
	Client->transfer_orders.byteoffset = sb.st_size;
/*	Client->bytes_read = sb.st_size;*/

	sprintf(buf, "%hd", ntohs(Client->remport));
	malloc_strcpy(&Client->othername, buf);

	malloc_strcpy(&Client->othername, ltoa((long)ntohs(Client->remport)));

	/* Just in case we have to fool the protocol enforcement. */
	doing_privmsg = doing_notice = in_ctcp_flag = 0;
	send_ctcp(CTCP_PRIVMSG, user, CTCP_DCC, "RESUME %s %d %d", 
		Client->description, ntohs(Client->remport), sb.st_size);

	new_free(&tmp);
	new_free(&fullname);
	/* Then we just sit back and wait for the reply. */
}

/*
 * When the peer demands DCC RESUME
 * We send out a DCC ACCEPT
 */
#ifdef __STDC__
static void dcc_getfile_resume_demanded (char *user, char *filename, char *port, char *offset)
#else
static void dcc_getfile_resume_demanded (user, filename, port, offset)
char *user, *filename, *port, *offset;
#endif
{
	DCC_list	*Client;

	if (!(Client = dcc_searchlist(filename, user, DCC_FILEOFFER, 0, port, NULL, 0)))
		return;		/* Its a fake. */

	if (!offset)
		return;		/* Its a fake */

	Client->transfer_orders.byteoffset = my_atol(offset);
	Client->bytes_read = 0L;

	doing_privmsg = doing_notice = in_ctcp_flag = 0;
	send_ctcp(CTCP_PRIVMSG, user, CTCP_DCC, "ACCEPT %s %s %s",
		filename, port, offset);

	/* Wait for them to open the connection */
}


/*
 * When we get the DCC ACCEPT
 * We start the connection
 */
#ifdef __STDC__
static	void	dcc_getfile_resume_start (char *nick, char *filename, char *port, char *offset)
#else
static void 	dcc_getfile_resume_start (nick, filename, port, offset)
	char 	*nick, *filename, *port, *offset;
#endif
{
	DCC_list	*Client;
	char		*fullname = NULL;
	char		*tmp = NULL;
	
	if (!(Client = dcc_searchlist(filename, nick, DCC_FILEREAD, 0, port, NULL, 0)))
		return;		/* Its fake. */

	Client->flags |= DCC_TWOCLIENTS;
	if (!dcc_open(Client))
		return;

	if (get_string_var(DCC_DLDIR_VAR))
#ifdef WINNT
		malloc_sprintf(&tmp, "%s\\%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#else
		malloc_sprintf(&tmp, "%s/%s", get_string_var(DCC_DLDIR_VAR), Client->description);
#endif
	else
		malloc_sprintf(&tmp, "%s", Client->description);
	if (0 == (fullname = expand_twiddle(tmp)))
		malloc_strcpy(&fullname, tmp);

	if (!(Client->file = open(fullname, O_WRONLY | O_APPEND, 0644)))
	{
		say("Unable to open %s: %s", Client->description, errno ? sys_errlist[errno] : "<No Error>");
		FD_CLR(Client->read, &readables);
		FD_CLR(Client->write, &writables);
		close(Client->read);
		Client->read = -1;
		Client->flags |= DCC_DELETE;
	}

	new_free(&fullname);
	new_free(&tmp);
}

#endif




void dcc_check_idle _((void))
{
register DCC_list *Client = NULL, *tmpClient;

time_t dcc_idle_time = get_int_var(_CDCC_CLOSE_IDLE_SENDS_TIME_VAR);
static char *last_notify = NULL;
int minidlecheck = get_int_var(_CDCC_MINSPEED_TIME_VAR);

	if (dcc_idle_time)
	{
		int this_idle_time = dcc_idle_time;
		int erase_it;
		for(Client = ClientList; Client;)
		{
			time_t client_idle = time(NULL) - Client->lasttime.tv_sec;
			if (client_idle <= 0 ) client_idle = 1;
			erase_it = 0;
			tmpClient = Client->next;
			switch (Client->flags & DCC_TYPES)
			{
				case DCC_FILEOFFER:
				case DCC_FILEREAD:
				case DCC_RESENDOFFER:
				case DCC_REGETFILE:
					this_idle_time = dcc_idle_time * 3;
					break;
				default:
					this_idle_time = dcc_idle_time;
					break;
			}
			if ((client_idle > this_idle_time) && !(Client->flags&DCC_ACTIVE))
			{
				put_it("%s", convert_output_format("$G %RDCC%n Auto-closing idle dcc $0 to $1", "%s %s", dcc_types[Client->flags&DCC_TYPES], Client->user));
				if (!last_notify || !match(Client->user,last_notify))
				{
					send_to_server("NOTICE %s :Dcc %s Auto Closed", Client->user, dcc_types[Client->flags&DCC_TYPES]);
					malloc_strcpy(&last_notify, Client->user);
				}
				if ((get_to_from(dcc_types[Client->flags & DCC_TYPES]) != -1))
					if (dcc_active_count)
						dcc_active_count--;
				erase_it = 1;
			}
			if (Client->flags&DCC_ACTIVE)
			{
				switch (Client->flags & DCC_TYPES)
				{
					case DCC_FILEOFFER:
					case DCC_RESENDOFFER:
						if (cdcc_minspeed && minidlecheck && ((client_idle % minidlecheck) == 0))
						{
							u_32int_t sent = Client->bytes_sent / 1024;
							double this_speed = 0.0;
							char lame_ultrix1[20];
							char lame_ultrix[20];
							this_speed = (double)((double) sent / (double)client_idle);

							if (this_speed < cdcc_minspeed)
							{
								sprintf(lame_ultrix, "%2.4g", (double)(sent / client_idle));
								sprintf(lame_ultrix1,"%2.4g", (double)cdcc_minspeed);
								put_it("%s", convert_output_format("$G %RDCC%n Auto-closing Slow dcc $0 to $1 require $2KB/s got $3KB/s", "%s %s %s %s", dcc_types[Client->flags&DCC_TYPES], Client->user, lame_ultrix1, lame_ultrix));
								if (!last_notify || !match(Client->user,last_notify))
								{
									send_to_server("NOTICE %s :CDCC Slow dcc %s Auto Closed. Require %sKB/s got %sKB/s", Client->user, dcc_types[Client->flags&DCC_TYPES], lame_ultrix1, lame_ultrix);
									malloc_strcpy(&last_notify, Client->user);
								}
								if (dcc_active_count)
									dcc_active_count--;
								erase_it = 1;
							}
						}
					default:
						break;
				}
			} 
			if (erase_it)
				dcc_erase(Client);
			Client = tmpClient;
		}
	}
	cdcc_timer_offer();
}

void dcx(char *command, char *args, char *subargs)
{
char *user = NULL;
int do_chat = 0;
int do_send = 0;
int do_get = 0;
int do_all = 0;
	context;
	do_chat = !my_stricmp(command, "dcx");
	do_send = !my_stricmp(command, "dcs");
	do_get  = !my_stricmp(command, "dcg");
	do_all	= !my_stricmp(command, "dca");

	if (!do_all && !(user = next_arg(args, &args)))
	{
		userage(command, "<nick>");
		return;
	}

	if  (do_all && !user)
	{
		dcc_close_all();
		dcc_active_count = 0;
	}
	else if (do_send)
	{
		dcc_close_type_nick_all("SEND",user);
		dcc_close_type_nick_all("RESEND", user);
	} 
	else if (do_get)
	{
		dcc_close_type_nick_all("GET",user);
		dcc_close_type_nick_all("REGET", user);
	} 
	else if (do_chat)
	{
		dcc_close_type_nick_all("CHAT",user);
	}
	status_update(1);
	return;
}
