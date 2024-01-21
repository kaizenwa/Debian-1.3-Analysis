/*
 * dcc.c: Things dealing client to client connections. 
 *
 * Written By Troy Rollo <troy@cbme.unsw.oz.au> 
 *
 * Copyright(c) 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: dcc.c,v 1.78.2.4 1996/07/20 19:14:30 mrg Exp $";
#endif

#include "irc.h"

#ifdef ESIX
# include <lan/net_types.h>
#endif /* ESIX */

#if defined(ISC30) && defined(_POSIX_SOURCE)
# undef _POSIX_SOURCE
#include <sys/stat.h>
# define _POSIX_SOURCE
#else
# include <sys/stat.h>
#endif /* ICS30 || _POSIX_SOURCE */

#include "talkd.h"
#include "server.h"
#include "ircaux.h"
#include "whois.h"
#include "lastlog.h"
#include "ctcp.h"
#include "dcc.h"
#include "hook.h"
#include "vars.h"
#include "window.h"
#include "output.h"
#include "newio.h"

static	void	dcc_chat _((char *));
static	void	dcc_chat_rename _((char *));
static	void	dcc_filesend _((char *));
static	void	dcc_getfile _((char *));
static	void	dcc_close _((char *));
static	void	dcc_talk _((char *));
static	void	dcc_tmsg _((char *));
static	void	dcc_rename _((char *));
static	void	dcc_summon _((char *));
static	void	dcc_send_raw _((char *));
static	void	process_incoming_chat _((DCC_list *));
static	void	process_outgoing_file _((DCC_list *));
static	void	process_incoming_file _((DCC_list *));
static	void	process_incoming_talk _((DCC_list *));
static	void	process_incoming_raw _((DCC_list *));
static	void	process_incoming_listen _((DCC_list *));

struct
{
	char	*name;	/* *MUST* be in ALL CAPITALS */
	int	uniq; /* minimum length to be a unique command */
	void	(*function) _((char *));
}	dcc_commands[] =
{
	{ "CHAT",	2, dcc_chat },
	{ "LIST",	1, dcc_list },
	{ "SEND",	2, dcc_filesend },
	{ "GET",	1, dcc_getfile },
	{ "CLOSE",	2, dcc_close },
	{ "TALK",	2, dcc_talk },
	{ "TMSG",	2, dcc_tmsg },
	{ "RENAME",	2, dcc_rename },
	{ "SUMMON",	2, dcc_summon },
	{ "RAW",	2, dcc_send_raw },
	{ NULL,		0, (void (*) _((char *))) NULL }
};

	char	*dcc_types[] =
{
	"<null>",
	"CHAT",
	"SEND",
	"GET",
	"TALK",
	"SUMMON",
	"RAW_LISTEN",
	"RAW",
	NULL
};

/* this is such a fucking kludge */

struct	deadlist
{
	DCC_list *it;
	struct deadlist *next;
}	*deadlist = NULL;

extern	int	in_ctcp_flag;
extern	char	MyHostName[];
extern	struct	in_addr	MyHostAddr;
extern int dgets_errno;
static	off_t	filesize = 0;

DCC_list	*ClientList = NULL;

static	void	add_to_dcc_buffer _((DCC_list *, char *));
static	void	dcc_really_erase _((void));
static	void	dcc_add_deadclient _((DCC_list *));
static	int	dcc_open _((DCC_list *));
static	char	*dcc_time _((time_t));

/*
 * dcc_searchlist searches through the dcc_list and finds the client
 * with the the flag described in type set.
 */
DCC_list *
dcc_searchlist(name, user, type, flag, othername)
	char	*name,
		*user;
	int	type,
		flag;
	char	*othername;
{
	DCC_list **Client, *NewClient;

	for (Client = (&ClientList); *Client ; Client = (&(**Client).next))
	{
		if ((((**Client).flags&DCC_TYPES) == type) &&
		    ((!name || (!my_stricmp(name, (**Client).description))) ||
		    (othername && (**Client).othername && (!my_stricmp(othername, (**Client).othername)))) &&
		    (my_stricmp(user, (**Client).user)==0))
			return *Client;
	}
	if (!flag)
		return NULL;
	*Client = NewClient = (DCC_list *) new_malloc(sizeof(DCC_list));
	NewClient->flags = type;
	NewClient->read = NewClient->write = NewClient->file = -1;
	NewClient->filesize = filesize;
	NewClient->next = (DCC_list *) 0;
	NewClient->user = NewClient->description = NewClient->othername = NULL;
	NewClient->bytes_read = NewClient->bytes_sent = 0L;
	NewClient->starttime = 0;
	NewClient->buffer = 0;
	malloc_strcpy(&NewClient->description, name);
	malloc_strcpy(&NewClient->user, user);
	malloc_strcpy(&NewClient->othername, othername);
	time(&NewClient->lasttime);
	return NewClient;
}

static	void
dcc_add_deadclient(client)
	DCC_list *client;
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
void
dcc_erase(Element)
	DCC_list	*Element;
{
	DCC_list	**Client;

	for (Client = &ClientList; *Client; Client = &(**Client).next)
		if (*Client == Element)
		{
			*Client = Element->next;
			new_close(Element->write);
			new_close(Element->read);
			if (Element->file != -1)
				new_close(Element->file);
			new_free(&Element->description);
			new_free(&Element->user);
			new_free(&Element->buffer);
			new_free(&Element);
			return;
		}
}

static	void
dcc_really_erase()
{
	struct deadlist *dies;

	while ((dies = deadlist) != NULL)
	{
		deadlist = deadlist->next;
		dcc_erase(dies->it);
	}
}

/*
 * Set the descriptor set to show all fds in Client connections to
 * be checked for data.
 */
void
set_dcc_bits(rd, wd)
	fd_set	*rd, *wd;
{
	DCC_list	*Client;

	for (Client = ClientList; Client != NULL; Client = Client->next)
	{
#ifdef DCC_CNCT_PEND
		if (Client->write != -1 && (Client->flags & DCC_CNCT_PEND))
			FD_SET(Client->write, wd);
#endif
		if (Client->read != -1)
			FD_SET(Client->read, rd);
	}
}

/*
 * Check all DCCs for data, and if they have any, perform whatever
 * actions are required.
 */
void
dcc_check(rd, wd)
	fd_set	*rd,
		*wd;
{
	DCC_list	**Client;
	struct	timeval	timeout;
	int	previous_server;
	int	lastlog_level;

	previous_server = from_server;
	from_server = (-1);
	timeout.tv_sec = timeout.tv_usec = 0;
	lastlog_level = set_lastlog_msg_level(LOG_DCC);
	for (Client = (&ClientList); *Client != NULL && !break_io_processing;)
	{
#ifdef NON_BLOCKING_CONNECTS
		/*
		 * run all connect-pending sockets.. suggested by deraadt@theos.com
		 */
		if ((*Client)->flags & DCC_CNCT_PEND)
		{
			struct sockaddr_in	remaddr;
			int	rl = sizeof(remaddr);

			if (getpeername((*Client)->read, (struct sockaddr *) &remaddr, &rl) != -1)
			{
				if ((*Client)->flags & DCC_OFFER)
				{
					(*Client)->flags &= ~DCC_OFFER;
					message_from((char *) 0, LOG_DCC);
					if (((*Client)->flags & DCC_TYPES) != DCC_RAW)
						say("DCC %s connection with %s[%s,%d] established",
							dcc_types[(*Client)->flags&DCC_TYPES], (*Client)->user,
							inet_ntoa(remaddr.sin_addr), ntohs(remaddr.sin_port));
					message_from((char *) 0, LOG_CURRENT);
				}
				(*Client)->starttime = time(NULL);
				(*Client)->flags &= ~DCC_CNCT_PEND;
				set_blocking((*Client)->read);
				if ((*Client)->read != (*Client)->write)
					set_blocking((*Client)->write);
			} /* else we're not connected yet */
		}
#endif
		if ((*Client)->read != -1 && FD_ISSET((*Client)->read, rd))
		{
			switch((*Client)->flags & DCC_TYPES)
			{
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
				process_outgoing_file(*Client);
				break;
			case DCC_FILEREAD:
				process_incoming_file(*Client);
				break;
			case DCC_TALK:
				process_incoming_talk(*Client);
				break;
			}
		}
done:
		if ((*Client)->flags & DCC_DELETE)
		{
			dcc_add_deadclient(*Client);
			Client = (&(**Client).next);
		}
		else
			Client = (&(**Client).next);
	}
	(void) set_lastlog_msg_level(lastlog_level);
	dcc_really_erase();
	from_server = previous_server;
}

/*
 * Process a DCC command from the user.
 */
void
process_dcc(args)
	char	*args;
{
	char	*command;
	int	i;
	int	lastlog_level;
	int	len;

	if (!(command = next_arg(args, &args)))
		return;
	len = strlen(command);
	upper(command);
	for (i = 0; dcc_commands[i].name != NULL; i++)
	{
		if (!strncmp(dcc_commands[i].name, command, len))
		{
			if (len < dcc_commands[i].uniq)
			{
				say("DCC command not unique: %s", command );
				return;
			}
			message_from((char *) 0, LOG_DCC);
			lastlog_level = set_lastlog_msg_level(LOG_DCC);
			dcc_commands[i].function(args);
			message_from((char *) 0, LOG_CURRENT);
			(void) set_lastlog_msg_level(lastlog_level);
			return;
		}
	}
	say("Unknown DCC command: %s", command);
}

static	int
dcc_open(Client)
	DCC_list	*Client;
{
	char    *user,
		*Type;
	struct	sockaddr_in	localaddr;
	struct	in_addr 	myip;
	int	sla;
	int	old_server;
#ifndef NON_BLOCKING_CONNECTS
	struct	sockaddr_in	remaddr;
	int	rl = sizeof(remaddr);
#endif

	user = Client->user;
	old_server = from_server;
	if (-1 == from_server)
		from_server = get_window_server(0);
	myip.s_addr = server_list[from_server].local_addr.s_addr;
	if (myip.s_addr == htonl(0x7f000001))
		myip.s_addr = MyHostAddr.s_addr;
	Type = dcc_types[Client->flags & DCC_TYPES];
	if (Client->flags & DCC_OFFER)
	{
#ifdef DCC_CNCT_PEND
		Client->flags |= DCC_CNCT_PEND;
#endif
		if ((Client->write = connect_by_number(Client->remport,
			      inet_ntoa(Client->remote), 1)) < 0)
		{
			message_from((char *) 0, LOG_DCC);
			say("Unable to create connection: %s",
				errno ? strerror(errno) : "Unknown Host");
			message_from((char *) 0, LOG_CURRENT);
			dcc_erase(Client);
			from_server = old_server;
			return 0;
		}
		Client->read = Client->write;
		Client->bytes_read = Client->bytes_sent = 0L;
		Client->flags |= DCC_ACTIVE;
#ifndef NON_BLOCKING_CONNECTS
		Client->flags &= ~DCC_OFFER;
		Client->starttime = time(NULL);
		if (getpeername(Client->read, (struct sockaddr *) &remaddr, &rl) == -1)
		{
			message_from((char *) 0, LOG_DCC);
			say("DCC error: getpeername failed: %s", strerror(errno));
			message_from((char *) 0, LOG_CURRENT);
			dcc_erase(Client);
			from_server = old_server;
			return 0;
		}
		message_from((char *) 0, LOG_DCC);
		if ((Client->flags & DCC_TYPES) != DCC_RAW)
			say("DCC %s connection with %s[%s,%d] established",
				Type, user, inet_ntoa(remaddr.sin_addr),
				ntohs(remaddr.sin_port));
#endif
		message_from((char *) 0, LOG_CURRENT);
		from_server = old_server;
		return 1;
	}
	else
	{
#ifdef DCC_CNCT_PEND
		Client->flags |= DCC_WAIT|DCC_CNCT_PEND;
#else
		Client->flags |= DCC_WAIT;
#endif
		if ((Client->read = connect_by_number(0, empty_string, 1)) < 0)
		{
			message_from((char *) 0, LOG_DCC);
			say("Unable to create connection: %s",
				errno ? strerror(errno) : "Unknown Host");
			message_from((char *) 0, LOG_CURRENT);
			dcc_erase(Client);
			from_server = old_server;
			return 0;
		}
		sla = sizeof(struct sockaddr_in);
		getsockname(Client->read, (struct sockaddr *) &localaddr, &sla);
		if (Client->flags & DCC_TWOCLIENTS)
		{
			/* patch to NOT send pathname accross */
			char	*nopath;

			if ((Client->flags & DCC_FILEOFFER) &&
			    (nopath = rindex(Client->description, '/')))
				nopath++;
			else
				nopath = Client->description;

			/*
			 * XXX
			 * should make the case below for the filesize into
			 * generic off_t2str() function, or something.  this
			 * cast is merely a STOP-GAP measure.
			 */
			if (Client->filesize)
				send_ctcp(ctcp_type[in_ctcp_flag], user, "DCC",
					 "%s %s %lu %u %d", Type, nopath,
					 (u_long) ntohl(myip.s_addr),
					 (u_short) ntohs(localaddr.sin_port),
					 (int)Client->filesize);
			else
				send_ctcp(ctcp_type[in_ctcp_flag], user, "DCC",
					 "%s %s %lu %u", Type, nopath,
					 (u_long) ntohl(myip.s_addr),
					 (u_short) ntohs(localaddr.sin_port));
			message_from((char *) 0, LOG_DCC);
			say("Sent DCC %s request to %s", Type, user);
			message_from((char *) 0, LOG_CURRENT);
		}
		/*
		 * Is this where dcc times are fucked up??  - phone
		 * Yes, it was..  and they are all hunky dory now..
		 */
		Client->starttime = 0;
		from_server = old_server;
		return 2;
	}	
}

static void
dcc_chat(args)
	char	*args;
{
	char	*user;
	DCC_list	*Client;

	if ((user = next_arg(args, &args)) == NULL)
	{
		say("You must supply a nickname for DCC CHAT");
		return;
	}
	Client = dcc_searchlist("chat", user, DCC_CHAT, 1, (char *) 0);
	if ((Client->flags&DCC_ACTIVE) || (Client->flags&DCC_WAIT))
	{
		say("A previous DCC CHAT to %s exists", user);
		return;
	}
	Client->flags |= DCC_TWOCLIENTS;
	dcc_open(Client);
}

char	*
#ifdef __STDC__
dcc_raw_listen(u_short port)
#else
dcc_raw_listen(port)
	u_short	port;
#endif
{
	DCC_list	*Client;
	char	PortName[10];
	struct	sockaddr_in locaddr;
	char	*RetName = NULL;
	int	size;
	int	lastlog_level;

	lastlog_level = set_lastlog_msg_level(LOG_DCC);
	if (port && port < 1025)
	{
		say("Cannot bind to a privileged port");
		(void) set_lastlog_msg_level(lastlog_level);
		return NULL;
	}
	sprintf(PortName, "%d", port);
	Client = dcc_searchlist("raw_listen", PortName, DCC_RAW_LISTEN, 1, (char *) 0);
	if (Client->flags & DCC_ACTIVE)
	{
		say("A previous DCC RAW_LISTEN on %s exists", PortName);
		(void) set_lastlog_msg_level(lastlog_level);
		return RetName;
	}
	bzero((char *) &locaddr, sizeof(locaddr));
	locaddr.sin_family = AF_INET;
	locaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	locaddr.sin_port = htons(port);
	if (0 > (Client->read = socket(AF_INET, SOCK_STREAM, 0)))
	{
		dcc_erase(Client);
		say("socket() failed: %s", strerror(errno));
		(void) set_lastlog_msg_level(lastlog_level);
		return RetName;
	}
	set_socket_options(Client->read);
	if (bind(Client->read, (struct sockaddr *) &locaddr, sizeof(locaddr))
				== -1)
	{
		dcc_erase(Client);
		say("Could not bind port: %s", strerror(errno));
		(void) set_lastlog_msg_level(lastlog_level);
		return RetName;
	}
	listen(Client->read, 4);
	size = sizeof(locaddr);
	Client->starttime = time((time_t *) 0);
	getsockname(Client->read, (struct sockaddr *) &locaddr, &size);
	Client->write = ntohs(locaddr.sin_port);
	Client->flags |= DCC_ACTIVE;
	sprintf(PortName, "%d", Client->write);
	malloc_strcpy(&Client->user, PortName);
	malloc_strcpy(&RetName, PortName);
	(void) set_lastlog_msg_level(lastlog_level);
	return RetName;
}

char	*
#ifdef __STDC__
dcc_raw_connect(char *host, u_short port)
#else
dcc_raw_connect(host, port)
	char	*host;
	u_short	port;
#endif
{
	DCC_list	*Client;
	char	PortName[10];
	struct	in_addr	address;
	struct	hostent	*hp;
	char	*RetName = (char *) 0;
	int	lastlog_level;

	lastlog_level = set_lastlog_msg_level(LOG_DCC);
	if ((address.s_addr = inet_addr(host)) == -1)
	{
		hp = gethostbyname(host);
		if (!hp)
		{
			say("Unknown host: %s", host);
			(void) set_lastlog_msg_level(lastlog_level);
			return RetName;
		}
		bcopy(hp->h_addr, &address, sizeof(address));
	}
	sprintf(PortName, "%d", port);
	Client = dcc_searchlist(host, PortName, DCC_RAW, 1, (char *) 0);
	if (Client->flags & DCC_ACTIVE)
	{
		say("A previous DCC RAW to %s on %s exists", host, PortName);
		(void) set_lastlog_msg_level(lastlog_level);
		return RetName;
	}
	Client->remport = port;
	bcopy((char *) &address, (char *) &Client->remote, sizeof(address));
	Client->flags = DCC_OFFER | DCC_RAW;
	if (!dcc_open(Client))
		return RetName;
	sprintf(PortName, "%d", Client->read);
	malloc_strcpy(&Client->user, PortName);
	if (do_hook(DCC_RAW_LIST, "%s %s E %d", PortName, host, port))
		put_it("DCC RAW connection to %s on %s via %d established",
				host, PortName, port);
	malloc_strcpy(&RetName, PortName);
	(void) set_lastlog_msg_level(lastlog_level);
	return RetName;
}

char    *talk_errors[] =
{
	"<No Error>",
	"User not logged in",
	"Connection failed",
	"Remote host does not recognise us",
	"Your party is refusing writes",
	"Unknown request",
	"Unknown protocol version",
	"Unable to decipher your address",
	"Unable to decipher return address" /* How the hell does it get
						back then? */
};

static	void
dcc_talk(args)
	char	*args;
{
	char	*user;
	char	*host;
	struct	hostent	*hp;
	int	status;

	DCC_list	*Client;

#ifdef DAEMON_UID
	if (getuid() == DAEMON_UID)
	{
	 	say("You are not permitted to use DCC TALK");
		return;
	}
#endif /* DAEMON_UID */
	if ((user = next_arg(args, &args)) == NULL)
	{
		say("You must supply a user[@host] for DCC TALK");
		return;
	}
	if ((host = index(user, '@')) != NULL)
		*(host++) = '\0';
	else
		host = MyHostName;
	Client = dcc_searchlist(host, user, DCC_TALK, 1, (char *) 0);
	if (Client->flags & DCC_ACTIVE || Client->flags & DCC_WAIT)
	{
		say("A previous DCC TALK to %s@%s exists", user, host);
		return;
	}
	if (host != MyHostName)
	{
		if ((hp = gethostbyname(host)) == NULL)
		{
			say("Unable to find address for %s", host);
			dcc_erase(Client);
			return;
		}
		bcopy(hp->h_addr, (char *) &(Client->remote),
			sizeof(struct in_addr));
	}
	else
		bcopy((char *) &MyHostAddr, (char *) &(Client->remote),
			sizeof(struct in_addr));
	if ((Client->file = connect_by_number(-1, empty_string, 0)) < 0)
	{
		say("Unable to create DCC TALK connection: %s",
				errno ? strerror(errno) : "Unknown Host");
		dcc_erase(Client);
		return;
	}
	say("Checking for invitation on caller's machine");
	if (!(status = send_talk_control(Client, DCC_TALK_CHECK)))
	{
		new_close(Client->file);
		dcc_erase(Client);
		say("DCC TALK: connection timed out");
		return;
	}
	if (--status || (Client->read = connect_by_number(Client->remport,
				inet_ntoa(Client->remote), 0)) < 0)
	{
		say("Inviting %s@%s", Client->user, Client->description);
		if ((Client->read = connect_by_number(0, empty_string, 0)) == -1 ||
		    !send_talk_control(Client, DCC_TALK_INVITE) ||
		    !(status=send_talk_control(Client, DCC_TALK_ANNOUNCE)))
		{
			send_talk_control(Client, DCC_TALK_DELETE_LOCAL);
			new_close(Client->read);
			new_close(Client->file);
			dcc_erase(Client);
			return;
		}
		if (--status)
		{
			new_close(Client->read);
			new_close(Client->file);
			dcc_erase(Client);
			say("DCC TALK: %s", talk_errors[status]);
			return;
		}
		say("Waiting for your party to respond");
		Client->flags |= DCC_WAIT;
	}
	else
	{
		say("Connected to %s@%s", Client->user, Client->description);
		Client->write = Client->read;
		send(Client->write,  "\008\025\027", 3, 0);
		recv(Client->read, Client->talkchars, 3, 0);
		Client->bytes_read = Client->bytes_sent = 3;
		Client->flags |= DCC_ACTIVE;
	}
}

static	void
dcc_summon(args)
	char	*args;
{
	char	*user;
	char	*host;
	struct	hostent	*hp;
	DCC_list *Client;

	if (0 == (user = next_arg(args, &args)))
	{
		say("You must supply a user[@host] for DCC SUMMON");
		return;
	}
	if (0 != (host = index(user, '@')))
		*host++ = '\0';
	else
		host = MyHostName;

	Client = dcc_searchlist(host, user, DCC_SUMMON, 1, (char *) 0);

	if (host != MyHostName)
	{
		if (0 == (hp = gethostbyname(host)))
		{
			say("Unable to find address for %s", host);
			dcc_erase(Client);
			return;
		}
		bcopy(hp->h_addr, (char *) &(Client->remote),
			sizeof(struct in_addr));
	}
	else
		bcopy((char *) &MyHostAddr, (char *) &(Client->remote),
			sizeof(struct in_addr));
	if ((Client->file = connect_by_number(-1, empty_string, 0)) < 0)
	{
		say("Unable to create DCC SUMMON connection: %s",
				errno ? strerror(errno) : "Unknown Host");
		return;
	}
	if (0 == send_talk_control(Client, DCC_TALK_SUMMON))
		say("DCC SUMMON: connection timed out");
	send_talk_control(Client, DCC_TALK_DELETE_SUMMON);
	new_close(Client->file);
	dcc_erase(Client);
}

int    
send_talk_control(Client, MessageType)
	DCC_list	*Client;
	int     MessageType;
{
	CTL_MSG	Message;
	CTL_RESPONSE	Response;
	static	long	 SeqNum = 0;
	struct	sockaddr_in SockAddr;
	struct	timeval timeout;
	fd_set	selset;
	int	i;
	int	dummy;

	Message.vers = TALK_VERSION;
	Message.id_num = htonl(SeqNum);
	SeqNum++; /* Not in the htonl because on some machines it's a macro */
	dummy = sizeof(SockAddr);
	getsockname(Client->file, (struct sockaddr *) &SockAddr, &dummy);
	Message.ctl_addr = (*(struct sockaddr *) &SockAddr);
	if (Client->read > 0)
	{
		getsockname(Client->read, (struct sockaddr *) &SockAddr,
			    &dummy);
		SockAddr.sin_addr=MyHostAddr;
	}
	Message.addr = (*(struct sockaddr *) &SockAddr);
	strncpy(Message.l_name, username, NAME_SIZE);
	Message.l_name[NAME_SIZE - 1] = '\0';
	strncpy(Message.r_name, Client->user, NAME_SIZE);
	Message.r_name[NAME_SIZE - 1] = '\0';
	Message.r_tty[0] = '\0';
	Message.pid = getpid();
	SockAddr.sin_addr = Client->remote;
	SockAddr.sin_port = htons(518);
	switch(MessageType)
	{
	case DCC_TALK_CHECK:
		Message.type = LOOK_UP;
		break;
	case DCC_TALK_INVITE:
		Message.type = LEAVE_INVITE;
		SockAddr.sin_addr = MyHostAddr;
		break;
	case DCC_TALK_ANNOUNCE:
		Message.type = ANNOUNCE;
		break;
	case DCC_TALK_DELETE_LOCAL:
		SockAddr.sin_addr = MyHostAddr;
	case DCC_TALK_DELETE_REMOTE:
		Message.type = DELETE;
		break;
	case DCC_TALK_SUMMON:
		strcpy(Message.l_name, "I:");
		strcat(Message.l_name, get_server_nickname(from_server));
		Message.type = ANNOUNCE;
		break;
	case DCC_TALK_DELETE_SUMMON:
		strcpy(Message.l_name, "I:");
		strcat(Message.l_name, get_server_nickname(from_server));
		Message.type = DELETE;
		break;
	}
	for (i = 0; i < 3; i++)
	{
		if (sendto(Client->file, (char *) &Message, sizeof(Message), 0,
			   (struct sockaddr *) &SockAddr,
			   sizeof(SockAddr))!=sizeof(Message))
		{
			perror("sendto");
			return 0;
		}
		timeout.tv_sec = 10;
		timeout.tv_usec = 0;
		FD_ZERO(&selset);
		FD_SET(Client->file, &selset);
		switch(select(Client->file+1, &selset, NULL, NULL, &timeout))
		{
		case -1:
			perror("select");
			return 0;
		case 1:
			do
			{
				recv(Client->file, (char *) &Response, sizeof(Response), 0);
				FD_ZERO(&selset);
				FD_SET(Client->file, &selset);
				timeout.tv_sec = 0;
				timeout.tv_usec = 0;
			}
			while (select(Client->file + 1, &selset, NULL, NULL,
					&timeout) > 0);
			if (Response.type != Message.type)
				continue;
			if (LOOK_UP == Response.type &&
			    SUCCESS == Response.answer)
			{
				SockAddr = (*(struct sockaddr_in *)
					&Response.addr);
				Client->remote = SockAddr.sin_addr;
				Client->remport = ntohs(SockAddr.sin_port);
			}
			return Response.answer + 1;
		}
	}
	return 0;
}

static	void
dcc_filesend(args)
	char	*args;
{
	char	*user;
	char	*filename,
		*fullname;
	DCC_list *Client;
	char	FileBuf[BIG_BUFFER_SIZE+1];
	struct	stat	stat_buf;

#ifdef  DAEMON_UID
	if (DAEMON_UID == getuid())
	{
		say("You are not permitted to use DCC to exchange files");
		return;
	}
#endif
	if (0 == (user = next_arg(args, &args)) ||
	    0 == (filename = next_arg(args, &args)))
	{
		say("You must supply a nickname and filename for DCC SEND");
		return;
	}
	if (*filename == '/')
	{
		strcpy(FileBuf, filename);
	}
	else if (*filename == '~')
	{
		if (0 == (fullname = expand_twiddle(filename)))
		{
			yell("Unable to expand %s", filename);
			return;
		}
		strcpy(FileBuf, fullname);
		new_free(&fullname);
	}
	else
	{
		getcwd(FileBuf, sizeof(FileBuf));
		strcat(FileBuf, "/");
		strcat(FileBuf, filename);
	}
	if (0 != access(FileBuf, R_OK))
	{
		yell("Cannot access %s", FileBuf);
		return;
	}
	stat_file(FileBuf, &stat_buf);
/* some unix didn't have this ???? */
#ifdef S_IFDIR
	if (stat_buf.st_mode & S_IFDIR)
	{
		yell("Cannot send a directory");
		return;
	}
#endif
	if (scanstr(FileBuf, "/etc/"))
	{
		yell("Send request rejected");
		return;
	}
	if ((int) strlen(FileBuf) >= 7 && 0 == strcmp(FileBuf + strlen(FileBuf) - 7, "/passwd"))
	{
		yell("Send request rejected");
		return;
	}
	filesize = stat_buf.st_size;
	Client = dcc_searchlist(FileBuf, user, DCC_FILEOFFER, 1, filename);
	if ((Client->file = open(Client->description, O_RDONLY)) == -1)
	{
		say("Unable to open %s: %s\n", Client->description,
			errno ? strerror(errno) : "Unknown Host");
		new_close(Client->read);
		Client->read = Client->write = (-1);
		Client->flags |= DCC_DELETE;
		return;
	}
	filesize = 0;
	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		say("A previous DCC SEND:%s to %s exists", FileBuf, user);
		return;
	}
	Client->flags |= DCC_TWOCLIENTS;
	dcc_open(Client);
}


static	void
dcc_getfile(args)
	char	*args;
{
	char	*user;
	char	*filename;
	DCC_list	*Client;
	char	*fullname = (char *) 0;

#ifdef  DAEMON_UID
	if (DAEMON_UID == getuid())
	{
		say("You are not permitted to use DCC to exchange files");
		return;
	}
#endif
	if (0 == (user = next_arg(args, &args)))
	{
		say("You must supply a nickname for DCC GET");
		return;
	}
	filename = next_arg(args, &args);
	if (0 == (Client = dcc_searchlist(filename, user, DCC_FILEREAD, 0, (char *) 0)))
	{
		if (filename)
			say("No file (%s) offered in SEND mode by %s",
					filename, user);
		else
			say("No file offered in SEND mode by %s", user);
		return;
	}
	if ((Client->flags & DCC_ACTIVE) || (Client->flags & DCC_WAIT))
	{
		say("A previous DCC GET:%s to %s exists", filename, user);
		return;
	}
	if (0 == (Client->flags & DCC_OFFER))
	{
		say("I'm a teapot!");
		dcc_erase(Client);
		return;
	}
	Client->flags |= DCC_TWOCLIENTS;
	Client->bytes_sent = Client->bytes_read = 0L;
	if (!dcc_open(Client))
		return;
	if (0 == (fullname = expand_twiddle(Client->description)))
		malloc_strcpy(&fullname, Client->description);
	if (-1 == (Client->file = open(fullname,
				O_WRONLY | O_TRUNC | O_CREAT, 0644)))
	{
		say("Unable to open %s: %s", Client->description,
				errno ? strerror(errno) : "<No Error>");
		new_close(Client->read);
		dcc_erase(Client);
	}
	new_free(&fullname);
}


void
register_dcc_offer(user, type, description, address, port, size)
	char	*user;
	char	*type;
	char	*description;
	char	*address;
	char	*port;
	char	*size;
{
	DCC_list	*Client;
	int	CType;
	char	*c;
	u_long	TempLong;
	unsigned	TempInt;
	int	do_auto = 0;	/* used in dcc chat collisions */
	char	*cmd = (char *) 0;

	if (0 != (c = rindex(description, '/')))
		description = c + 1;
	if ('.' == *description)
		*description = '_';
	if (size && *size)
		filesize = atoi(size);
	else
		filesize = 0;
	malloc_strcpy(&cmd, type);
	upper(cmd);
	if (!strcmp(cmd, "CHAT"))
		CType = DCC_CHAT;
#ifndef  DAEMON_UID
	else if (!strcmp(cmd, "SEND"))
#else
	else if (!strcmp(cmd, "SEND") && DAEMON_UID != getuid())
#endif /*DAEMON_UID*/
		CType = DCC_FILEREAD;
	else
	{
		say("Unknown DCC %s (%s) received from %s", type, description, user);
		return;
	}
	Client = dcc_searchlist(description, user, CType, 1, (char *) 0);
	filesize = 0;
	if (Client->flags & DCC_WAIT)
	{
		new_close(Client->read);
		dcc_erase(Client);
		if (DCC_CHAT == CType)
		{
			Client = dcc_searchlist(description, user, CType, 1, (char *) 0);
			do_auto = 1;
		}
		else
		{
			say("DCC %s collision for %s:%s", type, user,
				description);
			send_ctcp_reply(user, "DCC", "DCC %s collision occured while connecting to %s (%s)", type, nickname, description);
			return;
		}
	}
	if (Client->flags & DCC_ACTIVE)
	{
		say("Received DCC %s request from %s while previous session still active", type, user);
		return;
	}
	Client->flags |= DCC_OFFER;
	sscanf(address, "%lu", &TempLong);
	Client->remote.s_addr = htonl(TempLong);
	sscanf(port, "%u", &TempInt);
	Client->remport = TempInt;
	if (TempInt < 1024)
	{
		say("DCC %s (%s) request from %s rejected [port = %d]", type, description, user, TempInt);
		dcc_erase(Client);
		return;
	}
	if ((u_long) 0 == TempLong || 0 == Client->remport)
	{
		dcc_erase(Client);
		return;
	}
	if (do_auto)
	{
		say("DCC CHAT already requested by %s, connecting...", user);
		dcc_chat(user);
	}
	/*
	 * XXX
	 * should make the case below for the filesize into
	 * generic off_t2str() function, or something.  this
	 * cast is merely a STOP-GAP measure.
	 */
	else if (Client->filesize)
		say("DCC %s (%s %d) request received from %s", type, description, (int)Client->filesize, user);
	else
		say("DCC %s (%s) request received from %s", type, description, user);
	return;
}

static	void
process_incoming_chat(Client)
	DCC_list	*Client;
{
	struct	sockaddr_in	remaddr;
	int	sra;
	char	tmp[BIG_BUFFER_SIZE+1];
	char	*s, *bufptr;
	long	bytesread;
	int	old_timeout;

	if (Client->flags & DCC_WAIT)
	{
		sra = sizeof(struct sockaddr_in);
		Client->write = accept(Client->read, (struct sockaddr *)
			&remaddr, &sra);
#ifdef  ESIX
		mark_socket(Client->write);
#endif
		new_close(Client->read);
		Client->read = Client->write;
		Client->flags &= ~DCC_WAIT;
		Client->flags |= DCC_ACTIVE;
		say("DCC chat connection to %s[%s,%d] established", Client->user,
			inet_ntoa(remaddr.sin_addr), ntohs(remaddr.sin_port));
		Client->starttime = time(NULL);
		return;
	}
	old_timeout = dgets_timeout(1);
	s = Client->buffer;
	bufptr = tmp;
	if (s && *s)
	{
		int	len = strlen(s);

		strncpy(tmp, s, len);
		bufptr += len;
	}
	bytesread = dgets(bufptr, BIG_BUFFER_SIZE, Client->read, (char *)0);
	(void) dgets_timeout(old_timeout);
	switch (bytesread)
	{
	case -1:
		add_to_dcc_buffer(Client, tmp);
		return;
	case 0:
		say("DCC CHAT connection to %s lost: %s", Client->user, dgets_errno == -1 ? "Remote end closed connection" : strerror(dgets_errno));
		new_close(Client->read);
		Client->read = Client->write = -1;
		Client->flags |= DCC_DELETE;
		return;
	default:
		new_free(&Client->buffer);
		tmp[strlen(tmp) - 1]='\0';
		Client->bytes_read += bytesread;
		message_from(Client->user, LOG_DCC);
		if (do_hook(DCC_CHAT_LIST, "%s %s", Client->user, tmp))
		{
			if (away_set)
			{
				time_t	t;
				char	*msg = (char *) 0;

				t = time(0);
				msg = (char *) new_malloc(strlen(tmp) + 20);
				sprintf(msg, "%s <%.16s>", tmp, ctime(&t));
				strcpy(tmp, msg);
				new_free(&msg);
			}
			put_it("=%s= %s", Client->user, tmp);
		}
		message_from((char *) 0, LOG_CURRENT);
		return;
	}
}

static	void
process_incoming_listen(Client)
	DCC_list	*Client;
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
	new_socket = accept(Client->read, (struct sockaddr *) &remaddr,
			     &sra);
	if (0 != (hp = gethostbyaddr((char *)&remaddr.sin_addr,
	    sizeof(remaddr.sin_addr), remaddr.sin_family)))
		Name = hp->h_name;
	else
		Name = inet_ntoa(remaddr.sin_addr);
#ifdef  ESIX
	mark_socket(new_socket);
#endif
	sprintf(FdName, "%d", new_socket);
	NewClient = dcc_searchlist(Name, FdName, DCC_RAW, 1, (char *) 0);
	NewClient->starttime = time((time_t *) 0);
	NewClient->read = NewClient->write = new_socket;
	NewClient->remote = remaddr.sin_addr;
	NewClient->remport = remaddr.sin_port;
	NewClient->flags |= DCC_ACTIVE;
	NewClient->bytes_read = NewClient->bytes_sent = 0L;
	if (do_hook(DCC_RAW_LIST, "%s %s N %d", NewClient->user,
						NewClient->description,
						Client->write))
		say("DCC RAW connection to %s on %s via %d established",
					NewClient->description,
					NewClient->user,
					Client->write);
}

static	void
process_incoming_raw(Client)
	DCC_list	*Client;
{
	char	tmp[BIG_BUFFER_SIZE+1];
	char	*s, *bufptr;
	long	bytesread;
	int     old_timeout;

	s = Client->buffer;
	bufptr = tmp;
	if (s && *s)
	{
		int	len = strlen(s);

		strncpy(tmp, s, len);
		bufptr += len;
	}
	old_timeout = dgets_timeout(1);
	switch(bytesread = dgets(bufptr, BIG_BUFFER_SIZE, Client->read, (char *)0))
	{
	case -1:
		add_to_dcc_buffer(Client, tmp);
		return;
	case 0:
		if (do_hook(DCC_RAW_LIST, "%s %s C",
				Client->user, Client->description))
			say("DCC RAW connection to %s on %s lost",
				Client->user, Client->description);
		new_close(Client->read);
		Client->read = Client->write = -1;
		Client->flags |= DCC_DELETE;
		(void) dgets_timeout(old_timeout);
		return;
	default:
		new_free(&Client->buffer);
		tmp[strlen(tmp) - 1] = '\0';
		Client->bytes_read += bytesread;
		if (do_hook(DCC_RAW_LIST, "%s %s D %s",
				Client->user, Client->description, tmp))
			say("Raw data on %s from %s: %s",
				Client->user, Client->description, tmp);
		(void) dgets_timeout(old_timeout);
		return;
	}
}

static	void
process_incoming_talk(Client)
	DCC_list	*Client;
{
	struct	sockaddr_in	remaddr;
	int	sra;
	char	tmp[BIG_BUFFER_SIZE+1];
	char	*s, *bufptr;
	long	bytesread;
	int     old_timeout;

	if (Client->flags & DCC_WAIT)
	{
		sra = sizeof(struct sockaddr_in);
		Client->write = accept(Client->read, (struct sockaddr *)
			&remaddr, &sra);
#ifdef ESIX
		mark_socket(Client->write);
#endif
		new_close(Client->read);
		Client->read = Client->write;
		Client->flags &= ~DCC_WAIT;
		Client->flags |= DCC_ACTIVE;
		send_talk_control(Client, DCC_TALK_DELETE_LOCAL);
		new_close(Client->file);
		send(Client->write, "\010\025\027", 3, 0);
		recv(Client->read, Client->talkchars, 3, 0);
		Client->bytes_read = Client->bytes_sent = 3;
		say("TALK connection to %s[%s,%d] established", Client->user,
			inet_ntoa(remaddr.sin_addr), ntohs(remaddr.sin_port));
		return;
	}
	s = Client->buffer;
	bufptr = tmp;
	if (s && *s)
	{
		int	len = strlen(s);

		strncpy(tmp, s, len);
		bufptr += len;
	}
	old_timeout = dgets_timeout(1);
	switch(bytesread = dgets(bufptr, -BIG_BUFFER_SIZE, Client->read,
			Client->talkchars))
	{
	case -2: return;
	case -1:
		add_to_dcc_buffer(Client, tmp);
		return;
	case 0:
		say("TALK connection to %s lost", Client->user);
		new_close(Client->read);
		Client->read=Client->write = -1;
		Client->flags |= DCC_DELETE;
		(void) dgets_timeout(old_timeout);
		return;
	default:
		new_free(&Client->buffer);
		Client->bytes_read += bytesread;
		message_from(Client->user, LOG_DCC);
		if (do_hook(TALK_LIST, "%s %s", Client->user, tmp))
			put_it("+%s+ %s", Client->user, tmp);
		message_from((char *) 0, LOG_CURRENT);
		(void) dgets_timeout(old_timeout);
		return;
	}
}

static	void
process_outgoing_file(Client)
	DCC_list	*Client;
{
	struct	sockaddr_in	remaddr;
	int	sra;
	char	tmp[BIG_BUFFER_SIZE+1];
	u_32int_t	bytesrecvd;
	int	bytesread;
	int	BlockSize;

	if (Client->flags & DCC_WAIT)
	{
		sra = sizeof(struct sockaddr_in);
		Client->write = accept(Client->read,
				(struct sockaddr *) &remaddr, &sra);
#ifdef  ESIX
		mark_socket(Client->write);
#endif
		new_close(Client->read);
		Client->read = Client->write;
		Client->flags &= ~DCC_WAIT;
		Client->flags |= DCC_ACTIVE;
		Client->bytes_sent = 0L;
		Client->starttime = time(NULL);
		say("DCC SEND connection to %s[%s,%d] established", Client->user,
			inet_ntoa(remaddr.sin_addr), ntohs(remaddr.sin_port));
	}
	else 
	{ 
		if (recv(Client->read, (char *) &bytesrecvd, sizeof(u_32int_t), 0) < sizeof(u_32int_t))
		{
	       		say("DCC SEND:%s connection to %s lost: %s", Client->description, Client->user, strerror(errno));
			new_close(Client->read);
			Client->read = Client->write = (-1);
			Client->flags |= DCC_DELETE;
			new_close(Client->file);
			return;
		}
		else
			if (ntohl(bytesrecvd) != Client->bytes_sent)
				return;
	}
	BlockSize = get_int_var(DCC_BLOCK_SIZE_VAR);
	if (BlockSize > BIG_BUFFER_SIZE)
		BlockSize = BIG_BUFFER_SIZE;
	else if (BlockSize < 16)
		BlockSize = 16;
	if ((bytesread = read(Client->file, tmp, BIG_BUFFER_SIZE)) != 0)
	{
		send(Client->write, tmp, bytesread, 0);
		Client->bytes_sent += bytesread;
	}
	else
	{
		/*
		 * We do this here because lame Ultrix doesn't let us
		 * call put_it() with a float.  Perhaps put_it() should
		 * be fixed properly, and this kludge removed ..
		 * sometime....  -phone jan, 1993.
		 */

		char	lame_ultrix[10];	/* should be plenty */
		time_t	xtime = time(NULL) - Client->starttime;
		double	sent = (double)Client->bytes_sent;

		if (sent <= 0)
			sent = 1;
		sent /= (double)1024.0;
		if (xtime <= 0)
			xtime = 1;
		sprintf(lame_ultrix, "%2.4g", (sent / (double)xtime));
		say("DCC SEND:%s to %s completed %s kb/sec",
			Client->description, Client->user, lame_ultrix);
		new_close(Client->read);
		Client->read = Client->write = -1;
		Client->flags |= DCC_DELETE;
		new_close(Client->file);
		return;
	}
}

static	void
process_incoming_file(Client)
	DCC_list	*Client;
{
	char	tmp[BIG_BUFFER_SIZE+1];
	u_32int_t	bytestemp;
	int	bytesread;

	if ((bytesread = recv(Client->read, tmp, BIG_BUFFER_SIZE, 0)) <= 0)
	{
		/*
		 * We do this here because lame Ultrix doesn't let us
		 * call put_it() with a float.  Perhaps put_it() should
		 * be fixed properly, and this kludge removed ..
		 * sometime....  -phone jan, 1993.
		 */

		char    lame_ultrix[10];        /* should be plenty */
		time_t	xtime = time(NULL) - Client->starttime;
		double	sent = (double)Client->bytes_read;

		if (sent <= 0)
			sent = 1;
		sent /= (double)1024.0;
		if (xtime <= 0)
			xtime = 1;
		sprintf(lame_ultrix, "%2.4g", (sent / (double)xtime));
		say("DCC GET:%s from %s completed %s kb/sec",
			Client->description, Client->user, lame_ultrix);
		new_close(Client->read);
		new_close(Client->file);
		Client->read = Client->write = (-1);
		Client->flags |= DCC_DELETE;
		return;
	}
	write(Client->file, tmp, bytesread);
	Client->bytes_read += bytesread;
	bytestemp = htonl(Client->bytes_read);
	send(Client->write, (char *) &bytestemp, sizeof(u_32int_t), 0);
}

/* flag == 1 means show it.  flag == 0 used by redirect */

void
dcc_message_transmit(user, text, type, flag)
	char	*user;
	char	*text;
	int	type,
		flag;
{
	DCC_list	*Client;
	char	tmp[BIG_BUFFER_SIZE+1];
	int	lastlog_level;
	char	thing = '\0';
	int	list = 0;
	char	*host = (char *) 0;

	switch(type)
	{
	case DCC_TALK:
		if ((host = index(user, '@')) != NULL)
			*(host++) = '\0';
		thing = '+';
		list = SEND_TALK_LIST;
		break;
	case DCC_CHAT:
		host = "chat";
		thing = '=';
		list = SEND_DCC_CHAT_LIST;
		break;
	case DCC_RAW:
		host = next_arg(text, &text);
		if (!host)
		{
			say("No host specified for DCC RAW");
			return;
		}
		break;
	}
	if (!(Client = dcc_searchlist(host, user, type, 0, (char *) 0)) || !(Client->flags&DCC_ACTIVE))
	{
		say("No active DCC %s:%s connection for %s", dcc_types[type], host ? host : "<any>", user);
		return;
	}
#ifdef DCC_DCNT_PEND
	/*
	 * XXX - should make this buffer
	 * XXX - just for dcc chat ?  maybe raw dcc too.  hmm.
	 */
	if (Client->flags & DCC_CNCT_PEND)
	{
		say("DCC %s:%s connection to %s is still connecting...", dcc_types[type], host ? host : "<any>", user);
		return;
	}
#endif
	lastlog_level = set_lastlog_msg_level(LOG_DCC);
	message_from(Client->user, LOG_DCC);
	strmcpy(tmp, text, BIG_BUFFER_SIZE);
	strmcat(tmp, "\n", BIG_BUFFER_SIZE); 
	send(Client->write, tmp, strlen(tmp), 0);
	Client->bytes_sent += strlen(tmp);
	if (flag)
	{
		if (type != DCC_RAW)
			if (do_hook(list, "%s %s", Client->user, text))
				put_it("=> %c%s%c %s", thing, Client->user,
							thing, text);
	}
	set_lastlog_msg_level(lastlog_level);
	message_from((char *) 0, LOG_CURRENT);
	return;
}

void
dcc_chat_transmit(user,	text)
	char	*user;
	char	*text;
{
	dcc_message_transmit(user, text, DCC_CHAT, 1);
}

static	void
dcc_tmsg(args)
	char	*args;
{
	char	*user;

	if (!(user = next_arg(args, &args)))
	{
		say("You must specify a connection for a DCC TMSG");
		return;
	}
	dcc_message_transmit(user, args, DCC_TALK, 1);
}

static	void
dcc_send_raw(args)
	char	*args;
{
	char	*name;

	if (!(name = next_arg(args, &args)))
	{
		say("No name specified for DCC RAW");
		return;
	}
	dcc_message_transmit(name, args, DCC_RAW, 1);
}

/*
 * dcc_time: Given a time value, it returns a string that is in the
 * format of "hours:minutes:seconds month day year" .  Used by 
 * dcc_list() to show the start time.
 */
static	char	*
dcc_time(time)
	time_t	time;
{
	struct	tm	*btime;
	char	*buf;
	static	char	*months[] = 
	{
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	};

	btime = localtime(&time);
	buf = (char *) malloc(22);
	if (sprintf(buf, "%-2.2d:%-2.2d:%-2.2d %s %-2.2d %d", btime->tm_hour,
			btime->tm_min, btime->tm_sec, months[btime->tm_mon],
			btime->tm_mday, btime->tm_year + 1900))
		return buf;
	else
		return empty_string;
}

void
dcc_list(args)
	char	*args;
{
	DCC_list	*Client;
	static	char	*format =
			"%-7.7s %-9.9s %-10.10s %-20.20s %-8.8s %-8.8s %s";
	unsigned	flags;

	put_it(format, "Type", "Nick", "Status", "Start time", "Sent", "Read",
			"Arguments");
	for (Client = ClientList ; Client != NULL ; Client = Client->next)
	{
		char	sent[9],
			read[9];
		char	*time;

		sprintf(sent, "%ld", Client->bytes_sent);
		sprintf(read, "%ld", Client->bytes_read);
		time = (Client->starttime) ? dcc_time(Client->starttime) : "",
		flags = Client->flags;
		put_it(format, dcc_types[flags & DCC_TYPES],
				Client->user,
				flags & DCC_OFFER ? "Offered" :
				flags & DCC_DELETE ? "Closed" :
				flags & DCC_ACTIVE ? "Active" :
				flags & DCC_WAIT ? "Waiting" :
#ifdef DCC_DCNT_PEND
				flags & DCC_CNCT_PEND ?	"Connecting" :
#endif
				"Unknown",
				time, sent, read, Client->description);
		if (*time)
			new_free(&time);
	}
}

static	void
dcc_close(args)
	char	*args;
{
	DCC_list	*Client;
	unsigned	flags;
	char	*Type;
	char	*user;
	char	*description;
	int	CType;
	char	*cmd = NULL;

	if (!(Type = next_arg(args, &args)) || !(user=next_arg(args, &args)))
	{
		say("you must specify a type and nick for DCC CLOSE");
		return;
	}
	description = next_arg(args, &args);
	malloc_strcpy(&cmd, Type);
	upper(cmd);
	for (CType = 0; dcc_types[CType] != NULL; CType++)
		if (!strcmp(cmd, dcc_types[CType]))
			break;
	if (!dcc_types[CType])
	{
		say("Unknown DCC type: %s", Type);
		return;
	}
	if ((Client = dcc_searchlist(description, user, CType, 0, description)))
	{
		flags = Client->flags;
		if (flags & DCC_DELETE)
			return;
		if ((flags & DCC_WAIT) || (flags & DCC_ACTIVE))
		{
			new_close(Client->read);
			if (Client->file)
				new_close(Client->file);
		}
		say("DCC %s:%s to %s closed", Type,
			description ? description : "<any>", user);
		dcc_erase(Client);
	}
	else
		say("No DCC %s:%s to %s found", Type,
			description ? description : "<any>", user);
}

static void
dcc_chat_rename(args)
	char	*args;
{
	DCC_list	*Client;
	char	*user;
	char	*temp;
	
	if (!(user = next_arg(args, &args)) || !(temp = next_arg(args, &args)))
	{
		say("you must specify a current DCC CHAT connection, and a new name for it");
		return;
	}
	if (dcc_searchlist("chat", temp, DCC_CHAT, 0, (char *) 0))
	{
		say("You already have a DCC CHAT connection with %s, unable to rename.", temp);
		return;
	}
	if ((Client = dcc_searchlist("chat", user, DCC_CHAT, 0, (char *) 0)))
	{
		new_free(&(Client->user));
		malloc_strcpy(&(Client->user), temp);
		say("DCC CHAT connection with %s renamed to %s", user, temp);
	}
	else
		say("No DCC CHAT connection with %s", user);
}


static	void
dcc_rename(args)
	char	*args;
{
	DCC_list	*Client;
	char	*user;
	char	*description;
	char	*newdesc;
	char	*temp;
	
	if ((user = next_arg(args, &args)) && my_strnicmp(user, "-chat", strlen(user)) == 0)
	{
		dcc_chat_rename(args);
		return;
	}
	if (!user || !(temp = next_arg(args, &args)))
	{
		say("you must specify a nick and new filename for DCC RENAME");
		return;
	}
	if ((newdesc = next_arg(args, &args)) != NULL)
		description = temp;
	else
	{
		newdesc = temp;
		description = NULL;
	}
	if ((Client = dcc_searchlist(description, user, DCC_FILEREAD, 0, (char *) 0)))
	{
		if (!(Client->flags & DCC_OFFER))
		{
			say("Too late to rename that file");
			return;
		}
		new_free(&(Client->description));
		malloc_strcpy(&(Client->description), newdesc);
		say("File %s from %s renamed to %s",
			 description ? description : "<any>", user, newdesc);
	}
	else
		say("No file %s from %s found",
			description ? description : "<any>", user);
}

/*
 * close_all_dcc:  We call this when we create a new process so that
 * we don't leave any fd's lying around, that won't close when we
 * want them to..
 */

void
close_all_dcc()
{
	DCC_list *Client;

	while ((Client = ClientList))
		dcc_erase(Client);
}

static	void
add_to_dcc_buffer(Client, buf)
	DCC_list	*Client;
	char	*buf;
{
	if (buf && *buf)
	{
		if (Client->buffer)
			malloc_strcat(&Client->buffer, buf);
		else
			malloc_strcpy(&Client->buffer, buf);
	}
}
