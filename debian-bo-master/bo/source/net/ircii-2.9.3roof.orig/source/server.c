/*
 * server.c: Things dealing with server connections, etc. 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: server.c,v 1.82.2.3 1996/07/20 19:14:48 mrg Exp $";
#endif

#include "irc.h"

#ifdef ESIX
# include <lan/net_types.h>
#endif /* ESIX */

#ifdef HAVE_SYS_UN_H
# include <sys/un.h>

int	connect_to_unix _((int, char *));
#endif /* HAVE_SYS_UN_H */

#include "server.h"
#include "screen.h"
#include "ircaux.h"
#include "whois.h"
#include "lastlog.h"
#include "exec.h"
#include "window.h"
#include "output.h"
#include "names.h"
#include "parse.h"
#include "list.h"
#include "newio.h"

static	void	add_to_server_buffer _((int, char *));
static	void	login_to_server _((int));
static	int	connect_to_server_direct _((char *, int));
static	int	connect_to_server_process _((char *, int));

/*
 * Don't want to start ircio by default...
 */
	int	using_server_process = 0;

/* server_list: the list of servers that the user can connect to,etc */
	Server	*server_list = (Server *) 0;

/* number_of_servers: in the server list */
	int	number_of_servers = 0;

/* server_group_list:  list of server groups */
	SGroup	*server_group_list = (SGroup *) 0;

extern	WhoisQueue	*WQ_head;
extern	WhoisQueue	*WQ_tail;

	int	primary_server = -1;
	int	from_server = -1;
	int	attempting_to_connect= 0;
	int	never_connected = 1;		/* true until first connection
						 * is made */
	int	connected_to_server = 0;	/* true when connection is
						 * confirmed */
	char	*connect_next_nick;
	char	*connect_next_password;
	int	parsing_server_index = -1;

	int	save_chan_from = -1;

extern	int	dgets_errno;

#define DEFAULT_SERVER_VERSION Server2_8

/*
 * close_server: Given an index into the server list, this closes the
 * connection to the corresponding server.  It does no checking on the
 * validity of the index.  It also first sends a "QUIT" to the server being
 * closed 
 */
void
close_server(server_index, message)
	int	server_index;
	char	*message;
{
	int	i,
		min,
		max;

	if (server_index == -1)
	{
		min = 0;
		max = number_of_servers;
	}
	else
	{
		min = server_index;
		max = server_index + 1;
	}
	for (i = min; i < max; i++)
	{
		int	old_server = from_server;

		if (server_list[i].flags & CLOSE_PENDING)
			continue;
		if (waiting)
			irc_io_loop = 0;
		if (i == primary_server)
			clean_whois_queue();

		from_server = -1;
		clear_channel_list(i);
		from_server = old_server;

		server_list[i].operator = 0;
		server_list[i].connected = 0;
		server_list[i].buffer = (char *) 0;
		server_list[i].flags = 0;
		if (-1 != server_list[i].write)
		{
			if (message && *message)
			{
				sprintf(buffer, "QUIT :%s\n", message);
				send(server_list[i].write, buffer, strlen(buffer), 0);
			}
			new_close(server_list[i].write);
			if (server_list[i].write == server_list[i].read)
				server_list[i].read = -1;
			server_list[i].write = -1;
		}
		if (-1 != server_list[i].read)
		{
			new_close(server_list[i].read);
			server_list[i].read = -1;
		}
		if (-1 != server_list[i].pid)
		{
			kill(server_list[i].pid, SIGKILL);
			server_list[i].pid = (pid_t) -1;
		}
	}
}

/*
 * set_server_bits: Sets the proper bits in the fd_set structure according to
 * which servers in the server list have currently active read descriptors.  
 */

void
set_server_bits(rd, wd)
	fd_set *rd, *wd;
{
	int	i;

	for (i = 0; i < number_of_servers; i++)
	{
		if (server_list[i].read != -1)
			FD_SET(server_list[i].read, rd);
#ifdef NON_BLOCKING_CONNECTS
		if (!(server_list[i].flags & (LOGGED_IN|CLOSE_PENDING)) &&
		    server_list[i].write != -1)
			FD_SET(server_list[i].write, wd);
#endif
	}
}

/*
 * do_server: check the given fd_set against the currently open servers in
 * the server list.  If one have information available to be read, it is read
 * and and parsed appropriately.  If an EOF is detected from an open server,
 * one of two things occurs. 1) If the server was the primary server,
 * get_connected() is called to maintain the connection status of the user.
 * 2) If the server wasn't a primary server, connect_to_server() is called to
 * try to keep that connection alive. 
 */

void
do_server(rd, wd)
	fd_set	*rd, *wd;
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	int	des,
		j;
	static	int	times = 0;
	int	old_timeout;

	for (j = 0; j < number_of_servers && !break_io_processing; j++)
	{
#ifdef NON_BLOCKING_CONNECTS
		/*
		 *	deraadt@theos.com suggests that every fd awaiting connection
		 *	should be run at this point.
		 */
		if ((des = server_list[j].write) != -1 && /*FD_ISSET(des, wd) && */
		    !(server_list[j].flags & LOGGED_IN)) {
			struct sockaddr_in sa;
			int salen = sizeof(struct sockaddr_in);

			if (getpeername(server_list[j].write, (struct sockaddr *) &sa, &salen) != -1)
				login_to_server((from_server = j));
		}
#endif
		if ((des = server_list[j].read) != -1 && FD_ISSET(des, rd))
		{
			int	junk;
			char 	*bufptr;
			char	*s;
			int	i = j;

			from_server = i;
			old_timeout = dgets_timeout(1);
			s = server_list[from_server].buffer;
			bufptr = buffer;
			if (s && *s)
			{
				int	len = strlen(s);

				strncpy(buffer, s, len);
				bufptr += len;
			}
			junk = dgets(bufptr, BIG_BUFFER_SIZE, des, (char *) 0);
			(void) dgets_timeout(old_timeout);
			switch (junk)
			{
			case -1:
				add_to_server_buffer(from_server, buffer);
				continue;
			case 0:
			{
#ifdef NON_BLOCKING_CONNECTS
				int	old_serv = server_list[i].close_serv;
#endif

				server_list[i].copy_from = i;
				close_server(i, empty_string);
				say("Connection closed from %s: %s", server_list[i].name,
					dgets_errno == -1 ? "Remote end closed connection" : strerror(dgets_errno));
				server_list[i].connected = 0;
				server_list[i].read = server_list[i].write = -1;
#ifdef NON_BLOCKING_CONNECTS
				if (!(server_list[i].flags & LOGGED_IN))
				{
					if (old_serv == i)	/* a hack?  you bet */
						goto a_hack;
					if (old_serv != -1 && (server_list[old_serv].flags & CLOSE_PENDING))
					{
						say("Connection to server %s resumed...", server_list[old_serv].name);
						server_list[i].close_serv = -1;
						server_list[old_serv].flags &= ~CLOSE_PENDING;
						server_list[old_serv].flags |= LOGGED_IN;
						server_list[old_serv].connected = 1;
						get_connected(old_serv);
					}
					break;
				}
a_hack:
#endif
				if (i == primary_server)
				{
					if (server_list[i].eof)
					{
						say("Unable to connect to server %s",
							server_list[i].name);
						if (++i == number_of_servers)
  						{
 							int	serv = primary_server;

							clean_whois_queue();
							save_chan_from = primary_server;
							server_list[primary_server].copy_from = -1;
							window_check_servers();
							if (connected_to_server)
							{
								save_chan_from = -1;
								clear_channel_list(serv);
							}
							else		/* should hook DISCONNECT here */
								say("Use /SERVER to connect to a server");
							times = 0;
						}
						else
							get_connected(i);
					}
					else
					{
						if (times++ > 1)
						{
 							int	serv = primary_server;

							clean_whois_queue();
							save_chan_from = primary_server;
							server_list[primary_server].copy_from = -1;
							window_check_servers();
							if (connected_to_server)
							{
								save_chan_from = -1;
								clear_channel_list(serv);
							}
							else		/* should hook DISCONNECT here */
								say("Use /SERVER to connect to a server");
							times = 0;
  						}
						else
							get_connected(i);
					}
				}
				else if (server_list[i].eof)
				{
					say("Connection to server %s lost.", server_list[i].name);
					server_list[i].copy_from = -1;
					clean_whois_queue();
					window_check_servers();
				}
				else if (connect_to_server(server_list[i].name, server_list[i].port, -1))
				{
					say("Connection to server %s lost.", server_list[i].name);
					server_list[i].copy_from = -1;
					clean_whois_queue();
					window_check_servers();
				}
				server_list[i].eof = 1;
				break;
			}
			default:
				parsing_server_index = i;
				parse_server(buffer);
				new_free(&server_list[i].buffer);
				parsing_server_index = -1;
				break;
			}
			from_server = primary_server;
		}
	}
}

/*
 * find_in_server_list: given a server name, this tries to match it against
 * names in the server list, returning the index into the list if found, or
 * -1 if not found 
 */
extern	int
find_in_server_list(server, port)
	char	*server;
	int	port;
{
	int	i,
		len,
		len2;

	len = strlen(server);
	for (i = 0; i < number_of_servers; i++)
	{
		if (port && server_list[i].port &&
		    port != server_list[i].port && port != -1)
			continue;
		len2 = strlen(server_list[i].name);
		if (!my_strnicmp(server, server_list[i].name, MIN(len, len2)))
			return (i);
	}
	return (-1);
}

/*
 * parse_server_index:  given a string, this checks if it's a number, and if
 * so checks it validity as a server index.  Otherwise -1 is returned 
 */
int
parse_server_index(str)
	char	*str;
{
	int	i;

	if (is_number(str))
	{
		i = atoi(str);
		if ((i >= 0) && (i < number_of_servers))
			return (i);
	}
	return (-1);
}

/*
 * add_to_server_list: adds the given server to the server_list.  If the
 * server is already in the server list it is not re-added... however, if the
 * overwrite flag is true, the port and passwords are updated to the values
 * passes.  If the server is not on the list, it is added to the end. In
 * either case, the server is made the current server. 
 */
void
add_to_server_list(server, port, password, nick, overwrite)
	char	*server;
	int	port;
	char	*password;
	char	*nick;
	int	overwrite;
{
	if ((from_server = find_in_server_list(server, port)) == -1)
	{
		from_server = number_of_servers++;
		if (server_list)
			server_list = (Server *) new_realloc((char *) server_list, number_of_servers * sizeof(Server));
		else
			server_list = (Server *) new_malloc(number_of_servers * sizeof(Server));
		server_list[from_server].name = (char *) 0;
		server_list[from_server].itsname = (char *) 0;
		server_list[from_server].password = (char *) 0;
		server_list[from_server].away = (char *) 0;
		server_list[from_server].version_string = (char *) 0;
		server_list[from_server].operator = 0;
		server_list[from_server].read = -1;
		server_list[from_server].write = -1;
		server_list[from_server].pid = -1;
		server_list[from_server].version = Server2_8;	/* default */
		server_list[from_server].whois = 0;
		server_list[from_server].flags = SERVER_2_6_2;
		server_list[from_server].nickname = (char *) 0;
		server_list[from_server].connected = 0;
		server_list[from_server].eof = 0;
		server_list[from_server].motd = 1;
		server_list[from_server].chan_list = (ChannelList *) 0;
		malloc_strcpy(&(server_list[from_server].name), server);
		if (password && *password)
			malloc_strcpy(&(server_list[from_server].password),
				password);
		if (nick && *nick)
			malloc_strcpy(&(server_list[from_server].nickname),
				nick);
		server_list[from_server].port = port;
		server_list[from_server].WQ_head = (WhoisQueue *) 0;
		server_list[from_server].WQ_tail = (WhoisQueue *) 0;
		server_list[from_server].whois_stuff.nick = (char *) 0;
		server_list[from_server].whois_stuff.user = (char *) 0;
		server_list[from_server].whois_stuff.host = (char *) 0;
		server_list[from_server].whois_stuff.channel = (char *) 0;
		server_list[from_server].whois_stuff.channels = (char *) 0;
		server_list[from_server].whois_stuff.name = (char *) 0;
		server_list[from_server].whois_stuff.server = (char *) 0;
		server_list[from_server].whois_stuff.server_stuff = (char *) 0;
		server_list[from_server].whois_stuff.away = (char *) 0;
		server_list[from_server].whois_stuff.oper = 0;
		server_list[from_server].whois_stuff.chop = 0;
		server_list[from_server].whois_stuff.not_on = 0;
		server_list[from_server].buffer = (char *) 0;
		server_list[from_server].copy_from = -1;
		server_list[from_server].close_serv = -1;
		server_list[from_server].local_addr.s_addr = 0;
		server_list[from_server].parse_server = irc2_parse_server;
		server_list[from_server].ctcp_last_reply_time = 0;
	}
	else
	{
		if (overwrite)
		{
			server_list[from_server].port = port;
			if (password || !server_list[from_server].password)
			{
				if (password && *password)
					malloc_strcpy(&(server_list[from_server].password), password);
				else
					new_free(&(server_list[from_server].password));
			}
		}
		if ((int) strlen(server) > (int) strlen(server_list[from_server].name))
			malloc_strcpy(&(server_list[from_server].name), server);
	}
}

extern	void
remove_from_server_list(i)
	int	i;
{
	int	old_server = from_server,
		flag = 1,
		ind;
	Window	*tmp;

	from_server = i;
	clean_whois_queue();
	from_server = old_server;

	if (server_list[i].name)
		new_free(&server_list[i].name);
	if (server_list[i].itsname)
		new_free(&server_list[i].itsname);
	if (server_list[i].password)
		new_free(&server_list[i].password);
	if (server_list[i].away)
		new_free(&server_list[i].away);
	if (server_list[i].version_string)
		new_free(&server_list[i].version_string);
	if (server_list[i].nickname)
		new_free(&server_list[i].nickname);
	if (server_list[i].whois_stuff.nick)
		new_free(&server_list[i].whois_stuff.nick);
	if (server_list[i].whois_stuff.user)
		new_free(&server_list[i].whois_stuff.user);
	if (server_list[i].whois_stuff.host)
		new_free(&server_list[i].whois_stuff.host);
	if (server_list[i].whois_stuff.channel)
		new_free(&server_list[i].whois_stuff.channel);
	if (server_list[i].whois_stuff.channels)
		new_free(&server_list[i].whois_stuff.channels);
	if (server_list[i].whois_stuff.name)
		new_free(&server_list[i].whois_stuff.name);
	if (server_list[i].whois_stuff.server)
		new_free(&server_list[i].whois_stuff.server);
	if (server_list[i].whois_stuff.server_stuff)
		new_free(&server_list[i].whois_stuff.server_stuff);

	/* update all he structs with server in them */
	channel_server_delete(i);	/* fix `higher' servers */
	clear_channel_list(i);
	exec_server_delete(i);
	if (i < primary_server)
		--primary_server;
	if (i < from_server)
		--from_server;
	while ((tmp = traverse_all_windows(&flag)) != NULL)
		if (tmp->server > i)
			tmp->server--;

	for (ind = 0; ind < number_of_servers; ind++)
		if (server_list[ind].copy_from > i)
			server_list[ind].copy_from -= 1;
	if (save_chan_from > i)
		--save_chan_from;

	bcopy((char *) &server_list[i + 1], (char *) &server_list[i], (number_of_servers - i) * sizeof(Server));
	server_list = (Server *) new_realloc((char *) server_list, --number_of_servers * sizeof(Server));
}

/*
 * parse_server_info:  This parses a single string of the form
 * "server:portnum:password:nickname".  It the points port to the portnum
 * portion and password to the password portion.  This chews up the original
 * string, so * upon return, name will only point the the name.  If portnum
 * or password are missing or empty,  their respective returned value will
 * point to null. 
 */
void
parse_server_info(name, port, password, nick)
	char	*name,
		**port,
		**password,
		**nick;
{
	char *ptr;

	*port = *password = *nick = NULL;
	if ((ptr = (char *) index(name, ':')) != NULL)
	{
		*(ptr++) = (char) 0;
		if (strlen(ptr) == 0)
			*port = (char *) 0;
		else
		{
			*port = ptr;
			if ((ptr = (char *) index(ptr, ':')) != NULL)
			{
				*(ptr++) = (char) 0;
				if (strlen(ptr) == 0)
					*password = '\0';
				else
				{
					*password = ptr;
					if ((ptr = (char *) index(ptr, ':'))
							!= NULL)
					{
						*(ptr++) = '\0';
						if (!strlen(ptr))
							*nick = NULL;
						else
							*nick = ptr;
					}
				}
			}
		}
	}
}

/*
 * build_server_list: given a whitespace separated list of server names this
 * builds a list of those servers using add_to_server_list().  Since
 * add_to_server_list() is used to added each server specification, this can
 * be called many many times to add more servers to the server list.  Each
 * element in the server list case have one of the following forms: 
 *
 * servername 
 *
 * servername:port 
 *
 * servername:port:password 
 *
 * servername::password 
 *
 * Note also that this routine mucks around with the server string passed to it,
 * so make sure this is ok 
 */
void
build_server_list(servers)
	char	*servers;
{
	char	*host,
		*rest,
		*password = (char *) 0,
		*port = (char *) 0,
		*nick = (char *) 0;
	int	port_num;

	if (servers == (char *) 0)
		return;
	/* port_num = irc_port; */
	while (servers)
	{
		if ((rest = (char *) index(servers, '\n')) != NULL)
			*rest++ = '\0';
		while ((host = next_arg(servers, &servers)) != NULL)
		{
			parse_server_info(host, &port, &password, &nick);
			if (port && *port)
			{
				port_num = atoi(port);
				if (!port_num)
					port_num = irc_port;
			}
			else
				port_num = irc_port;
			add_to_server_list(host, port_num, password, nick, 0);
		}
		servers = rest;
	}
}

/*
 * connect_to_server_direct: handles the tcp connection to a server.  If
 * successful, the user is disconnected from any previously connected server,
 * the new server is added to the server list, and the user is registered on
 * the new server.  If connection to the server is not successful,  the
 * reason for failure is displayed and the previous server connection is
 * resumed uniterrupted. 
 *
 * This version of connect_to_server() connects directly to a server 
 */
static	int
connect_to_server_direct(server_name, port)
	char	*server_name;
	int	port;
{
	int	new_des;
	struct sockaddr_in localaddr;
	int	address_len;

	using_server_process = 0;
	oper_command = 0;
	errno = 0;
#ifdef HAVE_SYS_UN_H
	if (*server_name == '/')
		new_des = connect_to_unix(port, server_name);
	else
#endif /* HAVE_SYS_UN_H */
		new_des = connect_by_number(port, server_name, 1);
	if (new_des < 0)
	{
		say("Unable to connect to port %d of server %s: %s", port,
				server_name, errno ? strerror(errno) :
				"unknown host");
		if (from_server != -1 && server_list[from_server].read != -1)
			say("Connection to server %s resumed...",
					server_list[from_server].name);
		return (-1);
	}
#ifdef HAVE_SYS_UN_H
	if (*server_name != '/')
#endif /* HAVE_SYS_UN_H */
	{
		address_len = sizeof(struct sockaddr_in);
		getsockname(new_des, (struct sockaddr *) &localaddr,
				&address_len);
	}
	update_all_status();
	add_to_server_list(server_name, port, (char *) 0, (char *) 0, 1);
	if (port)
	{
		server_list[from_server].read = new_des;
		server_list[from_server].write = new_des;
	}
	else
		server_list[from_server].read = new_des;
	server_list[from_server].local_addr.s_addr = localaddr.sin_addr.s_addr;
	server_list[from_server].operator = 0;
	return (0);
}

/*
 * connect_to_server_process: handles the tcp connection to a server.  If
 * successful, the user is disconnected from any previously connected server,
 * the new server is added to the server list, and the user is registered on
 * the new server.  If connection to the server is not successful,  the
 * reason for failure is displayed and the previous server connection is
 * resumed uniterrupted. 
 *
 * This version of connect_to_server() uses the ircio process to talk to a
 * server 
 */
static	int
connect_to_server_process(server_name, port)
	char	*server_name;
	int	port;
{
	int	write_des[2],
		read_des[2],
		pid,
		c;
	char	*path,
		*name = (char *) 0,
		*s;
	int	old_timeout;

	path = IRCIO_PATH;
	if ((s = rindex(path, '/')) != NULL)
		malloc_strcpy(&name, s + 1);
	if (!name)
		name = path;
	if (*path == '\0')
		return (connect_to_server_direct(server_name, port));
	using_server_process = 1;
	oper_command = 0;
	write_des[0] = -1;
	write_des[1] = -1;
	if (pipe(write_des) || pipe(read_des))
	{
		if (write_des[0] != -1)
		{
			new_close(write_des[0]);
			new_close(write_des[1]);
		}
		say("Couldn't start new process: %s", strerror(errno));
		return (connect_to_server_direct(server_name, port));
	}
	switch (pid = fork())
	{
	case -1:
		say("Couldn't start new process: %s\n", strerror(errno));
		return (-1);
	case 0:
		(void) MY_SIGNAL(SIGINT, (sigfunc *)SIG_IGN, 0);
		dup2(read_des[1], 1);
		dup2(write_des[0], 0);
		new_close(read_des[0]);
		new_close(read_des[1]);
		new_close(write_des[0]);
		new_close(write_des[1]);
		sprintf(buffer, "%u", port);
		setuid(getuid());
		execl(path, name, server_name, buffer, (char *) 0);
		printf("-5 0\n"); /* -1 - -4 returned by connect_by_number() */
		fflush(stdout);
		_exit(1);
	default:
		new_close(read_des[1]);
		new_close(write_des[0]);
		break;
	}
	old_timeout = dgets_timeout(3);
	c = dgets(buffer, BIG_BUFFER_SIZE, read_des[0], (char *) 0);
	(void) dgets_timeout(old_timeout);
	if ((c == 0) || ((c = atoi(buffer)) != 0))
	{
		if (c == -5)
			return (connect_to_server_direct(server_name, port));
		else
		{
			char *ptr;

			if ((ptr = (char *) index(buffer, ' ')) != NULL)
			{
				ptr++;
				if (atoi(ptr) > 0)
		say("Unable to connect to port %d of server %s: %s",
			port, server_name, strerror(atoi(ptr)));
				else
		say("Unable to connect to port %d of server %s: Unknown host",
							port, server_name);
			}
			else
		say("Unable to connect to port %d of server %s: Unknown host",
							port, server_name);
			if ((from_server != -1) &&
					(server_list[from_server].read != -1))
				say("Connection to server %s resumed...",
						server_list[from_server].name);
			new_close(read_des[0]);
			new_close(write_des[1]);
			return (-1);
		}
	}
	update_all_status();
	add_to_server_list(server_name, port, (char *) 0, (char *) 0, 1);
	server_list[from_server].read = read_des[0];
	server_list[from_server].write = write_des[1];
	server_list[from_server].pid = pid;
	server_list[from_server].operator = 0;
	return (0);
}

/*
 * connect_to_server: Given a name and portnumber, this will attempt to
 * connect to that server using either a direct connection or process
 * connection, depending on the value of using_server_process.  If connection
 * is successful, the proper NICK, USER, and PASS commands are sent to the
 * server.  If the c_server parameter is not -1, then the server with that
 * index will be closed upon successful connection here. Also, if connection
 * is successful, the attempting_to_connect variable is incremented.  This is
 * checked in the notice.c routines to make sure that connection was truely
 * successful (and not closed immediately by the server). 
 */
int
connect_to_server(server_name, port, c_server)
	char	*server_name;
	int	port;
	int	c_server;
{
	int	server_index;
	struct sockaddr_in	sa;
	int salen = sizeof( struct sockaddr_in );

	message_from((char *) 0, LOG_CURRENT);
	server_index = find_in_server_list(server_name, port);
	attempting_to_connect++;
	/*
	 * check if the server doesn't exist, or that we're not already
	 * connected to it.
	 */
	if (server_index == -1 || server_list[server_index].read == -1)
	{
		if (port == -1)
		{
			if (server_index != -1)
				port = server_list[server_index].port;
			else
				port = irc_port;
		}
		say("Connecting to port %d of server %s", port, server_name);
		if (using_server_process)
			server_index = connect_to_server_process(server_name, port);
		else
			server_index = connect_to_server_direct(server_name, port);
		if (server_index)
		{
			attempting_to_connect--;
			return -1;
		}
		if ((c_server != -1) && (c_server != from_server))
		{
#ifdef NON_BLOCKING_CONNECTS
			int i;

			/*
			 * this sucks.. we can't keep our channels if the
			 * new server connect() fails.
			 */
#if defined(GKM)
			say("--- server %s will be closed when we connect", server_list[c_server].name);
			if (server_list[c_server].flags & CLOSE_PENDING)
				say("--- why are we flagging this for closing a second time?");
#endif
			server_list[from_server].close_serv = c_server;
			server_list[c_server].flags |= CLOSE_PENDING;
			server_list[c_server].connected = 0;
			i = from_server;
			from_server = -1;
			clear_channel_list(c_server);
			from_server = i;
#else
			close_server(c_server, "changing servers");
#endif /* NON_BLOCKING_CONNECTS */
		}
		else
		{
			server_list[from_server].close_serv = -1;
		}
		if (connect_next_nick && *connect_next_nick)
		{
			malloc_strcpy(&(server_list[from_server].nickname),
					connect_next_nick);
			new_free(&connect_next_nick);
		}
		if (connect_next_password && *connect_next_password)
		{
			malloc_strcpy(&(server_list[from_server].password),
					connect_next_password);
			new_free(&connect_next_password);
		}
		if (server_list[from_server].nickname == (char *) 0)
			malloc_strcpy(&(server_list[from_server].nickname),
					nickname);
		server_list[from_server].flags &= ~LOGGED_IN;
		/*
		 * this used to be an ifndef NON_BLOCKING_CONNECTS .. we want to do this
		 * whenever the connection is valid, it's possible for a connect to be
		 * "immediate".
		 */
		if (server_list[from_server].read != -1 &&
			getpeername(server_list[from_server].read, (struct sockaddr *) &sa, &salen) != -1)
			login_to_server(from_server);
	}
	else
	{
		if (port == -1)
		{
			if (server_index != -1)
				port = server_list[server_index].port;
			else
				port = irc_port;
		}
		say("Connected to port %d of server %s", port, server_name);
		from_server = server_index;
		if ((c_server != -1) && (c_server != from_server))
			close_server(c_server, "changing servers");
	}
	update_all_status();
	return 0;
}

static	void
login_to_server(server)
	int server;
{
#ifdef NON_BLOCKING_CONNECTS
	int	old_serv = server_list[server].close_serv;
#endif

	server_list[server].flags |= LOGGED_IN;
#ifdef NON_BLOCKING_CONNECTS
	set_blocking(server_list[server].read);
	if (server_list[server].read != server_list[server].write)
		set_blocking(server_list[server].write);
	if (old_serv != -1)
	{
#if defined(GKM)
		say("--- closing server %s - changing servers", server_list[server_list[server].close_serv].name);
		if (!(server_list[server_list[server].close_serv].flags & CLOSE_PENDING))
			say("--- uh oh. closing a server that wasn't CLOSE_PENDING");
#endif
		server_list[old_serv].flags &= ~CLOSE_PENDING;
		close_server(old_serv, "changing servers");
		server_list[server].close_serv = -1;
		/* should we pause here to let the net catch up with us? */
	}
#if defined(GKM)
	else
	{
		say("--- no server to close in login_to_server()");
	}
#endif
#endif /* NON_BLOCKING_CONNECTS */
	send_to_server("NICK %s", server_list[server].nickname);
	if (server_list[server].password)
		send_to_server("PASS %s", server_list[server].password);
	send_to_server("USER %s %s %s :%s", username,
		(send_umode && *send_umode) ? send_umode : hostname,
		server_list[server].name, realname);
}

/*
 * get_connected: This function connects the primary server for IRCII.  It
 * attempts to connect to the given server.  If this isn't possible, it
 * traverses the server list trying to keep the user connected at all cost.  
 */
void
get_connected(server)
	int	server;
{
	int	s,
		ret = -1;

	if (server_list)
	{
		int	already_connected = 0;

		if (server == number_of_servers)
			server = 0;
		else if (server < 0)
			server = number_of_servers - 1;
		s = server;
		if (connect_to_server(server_list[server].name, server_list[server].port, primary_server))
		{
			while (server_list[server].read == -1)
			{
				server++;
				if (server == number_of_servers)
					server = 0;
				if (server == s)
				{
					clean_whois_queue();
					say("Use /SERVER to connect to a server");
					break;
				}
				from_server = server;
				already_connected = is_server_connected(server);
				ret = connect_to_server(server_list[server].name, server_list[server].port, primary_server);
			}
			if (!ret)
				from_server = server;
			else
				from_server = -1;
		}
		if (from_server != -1)
		{
			if (!already_connected)
				server_list[from_server].copy_from = primary_server;
			if (curr_scr_win->server_group)
				set_window_server(-2, from_server, curr_scr_win->server_group);
			else
				set_window_server(-1, from_server, 1);
		}
		else
			save_chan_from = primary_server;
	}
	else
	{
		clean_whois_queue();
		say("Use /SERVER to connect to a server");
	}
}

#ifdef SERVERS_FILE
/*
 * read_server_file: reads hostname:portnum:password server information from
 * a file and adds this stuff to the server list.  See build_server_list()/ 
 */
int
read_server_file()
{
	FILE *fp;
	char format[11];
	char *file_path = (char *) 0;

	malloc_strcpy(&file_path, irc_lib);
	malloc_strcat(&file_path, SERVERS_FILE);
	sprintf(format, "%%%ds", BIG_BUFFER_SIZE);
	fp = fopen(file_path, "r");
	new_free(&file_path);
	if ((FILE *) 0 != fp)
	{
		while (fscanf(fp, format, buffer) != EOF)
			build_server_list(buffer);
		fclose(fp);
		return (0);
	}
	return (1);
}
#endif

/* display_server_list: just guess what this does */
void
display_server_list()
{
	int	i;

	if (server_list)
	{
		if (from_server != -1)
			say("Current server: %s %d",
					server_list[from_server].name,
					server_list[from_server].port);
		else
			say("Current server: <None>");
		if (primary_server != -1)
			say("Primary server: %s %d",
				server_list[primary_server].name,
				server_list[primary_server].port);
		else
			say("Primary server: <None>");
		say("Server list:");
		for (i = 0; i < number_of_servers; i++)
		{
			if (!server_list[i].nickname)
			{
				say("\t%d) %s %d%s", i,
					server_list[i].name,
					server_list[i].port,
					server_list[i].read == -1 ? " (not connected)" : "");
			}
			else
			{
				if (server_list[i].read == -1)
					say("\t%d) %s %d (was %s)", i,
						server_list[i].name,
						server_list[i].port,
						server_list[i].nickname);
				else
					say("\t%d) %s %d (%s)", i,
						server_list[i].name,
						server_list[i].port,
						server_list[i].nickname);
			}
#ifdef GKM
			say("\t\tflags: %s%s%s%s%s%s",
				server_list[i].flags & SERVER_2_6_2 ? "SERVER_2_6_2 " : "",
				server_list[i].flags & USER_MODE_I ? "USER_MODE_I " : "",
				server_list[i].flags & USER_MODE_W ? "USER_MODE_W " : "",
				server_list[i].flags & USER_MODE_S ? "USER_MODE_S " : "",
				server_list[i].flags & CLOSE_PENDING ? "CLOSE_PENDING " : "",
				server_list[i].flags & LOGGED_IN ? "LOGGED_IN " : "" );
			say("\t\tcopy_from=%d, close_serv=%d, connected=%d", server_list[i].copy_from, server_list[i].close_serv, server_list[i].connected );
#endif
		}
	}
	else
		say("The server list is empty");
}

void
MarkAllAway(command, message)
	char	*command;
	char	*message;
{
	int	old_server;

	old_server = from_server;
	for (from_server=0; from_server<number_of_servers; from_server++)
	{
		if (server_list[from_server].connected)
			send_to_server("%s :%s", command, message);
	}
	from_server = old_server;
}


/*
 * set_server_password: this sets the password for the server with the given
 * index.  If password is null, the password for the given server is returned 
 */
char	*
set_server_password(server_index, password)
	int	server_index;
	char	*password;
{

	if (server_list)
	{
		if (password)
			malloc_strcpy(&(server_list[server_index].password), password);
		return (server_list[server_index].password);
	}
	else
		return ((char *) 0);
}

/*
 * server: the /SERVER command. Read the SERVER help page about 
 */
/*ARGSUSED*/
void
server(command, args, subargs)
	char	*command,
		*args,
		*subargs;
{
	char	*server,
		*port,
		*password = (char *) 0,
		*nick = (char *) 0;
	int	port_num,
		i,
		already_connected = 0;

	if ((server = next_arg(args, &args)) != NULL)
	{
		while (*server == '-')
		{
			int	len;

			/*
			 * old usage of `/server -' handled here.
			 */
			if (*++server == '\0')
			{
				get_connected(primary_server - 1);
				return;
			}
			upper(server);
			len = strlen(server);
			/*
			 * just don't return if you want to perform some action in one of
			 * the flag handling sections.
			 */
			if (!strncmp(server, "DELETE", len))
			{
				if ((server = next_arg(args, &args)) != NULL)
				{
					if ((i = parse_server_index(server)) == -1)
					{
						if (-1 == (i = find_in_server_list(server, 0)))
						{
							say("No such server in list");
							return;
						}
					}
					if (server_list[i].connected)
					{
						say("Can not delete server that is already open");
						return;
					}
					remove_from_server_list(i);
					return;
				}
				say("Need server number for -DELETE");
				return;
			}
			else
			{
				say("SERVER: %s is an unknown flag", server);
				return;
			}
	/* while there are no other cases, this code is not needed -mrg */
#if 0
			if ((server = next_arg(args, &args)) == NULL)
			{
				say("SERVER: need a server name");
				return;
			}
#endif
		/* NOTREACHED */
		}

		if (index(server, ':') != NULL)
		{
			parse_server_info(server, &port, &password, &nick);
			if (!strlen(server))
			{
				say("Server name required");
				return;
			}
			port_num = atoi(port);
			if (!port_num)
				port_num = -1;
		}
		else
		{
			if ((port = next_arg(args, &args)) != NULL)
			{
				port_num = atoi(port);
				if (!port_num)
					port_num = irc_port;
				if ((password = next_arg(args, &args)) != NULL)
					nick = next_arg(args, &args);
			}
			else
				port_num = -1;
		}
 
		if (nick && *nick)
			malloc_strcpy(&connect_next_nick, nick);
		if (password && *password)
			malloc_strcpy(&connect_next_password, password);

		if (*server == '+')
		{
			if (*(++server))
			{
				char	servinfo[INPUT_BUFFER_SIZE+1];

				/* Reconstitute whole server info so
				  window_get_connected can parse it -Sol */
				sprintf(servinfo, "%s:%d:%s:%s",
					server, port_num,
					password ? password : empty_string,
					nick ? nick : empty_string);
				window_get_connected(curr_scr_win, servinfo, -1, 0, (char *) 0);
			}
			else
				get_connected(primary_server + 1);
			return;
		}
#ifdef SERVER_DOT
		/*
		 * work in progress.. window->prev_server needs to be set for
		 * all windows that used to be associated with a server as it
		 * switches [successfully] to a new server.
		 * this'll be fun since that can happen in server.c and
		 * window.c and non-blocking-connects will throw yet another
		 * wrench into things since we only want it to happen on
		 * a successful connect. - gkm
		 */
		else if (*server == '.')
		{
			if (*(++server))
			{
				say("syntax error - nothing may be specified after the '.'");
				return;
			}
			if (current_screen && curr_scr_win && curr_scr_win->prev_server != -1)
			{
				window_restore_server(curr_scr_win->prev_server);
				window_get_connected(curr_scr_win, NULL, curr_scr_win->server, 0, (char *) 0);
			}
			else
				say("No server previously in use in this window");
			return;
		}
#endif /* SERVER_DOT */
		if ((i = parse_server_index(server)) != -1)
		{
			server = server_list[i].name;
			if (server_list[i].port != -1)
				port_num = server_list[i].port;
		}
		if ((i = find_in_server_list(server, port_num)) != -1)
			already_connected = is_server_connected(i);
		if (connected_to_server)
			save_chan_from = primary_server;
		if (connect_to_server(server, port_num, primary_server) != -1)
		{
			if (primary_server > -1 && from_server != primary_server &&
			    !server_list[from_server].away && server_list[primary_server].away)
				malloc_strcpy(&server_list[from_server].away, server_list[primary_server].away);
			if (connected_to_server)
			{
				if (!already_connected)
				{
					server_list[from_server].copy_from = primary_server;
					save_chan_from = -1;
				}
			}
			else
				server_list[from_server].copy_from = save_chan_from;
			if (curr_scr_win->server_group)
				set_window_server(-2, from_server, curr_scr_win->server_group);
			else
				set_window_server(-1, from_server, 1);
		}
	}
	else
		display_server_list();
}

/*
 * flush_server: eats all output from server, until there is at least a
 * second delay between bits of servers crap... useful to abort a /links. 
 */
void
flush_server()
{
	fd_set rd;
	struct timeval timeout;
	int	flushing = 1;
	int	des;
	int	old_timeout;

	if ((des = server_list[from_server].read) == -1)
		return;
	timeout.tv_usec = 0;
	timeout.tv_sec = 1;
	old_timeout = dgets_timeout(1);
	while (flushing)
	{
		FD_ZERO(&rd);
		FD_SET(des, &rd);
		switch (new_select(&rd, (fd_set *) 0, &timeout))
		{
		case -1:
		case 0:
			flushing = 0;
			break;
		default:
			if (FD_ISSET(des, &rd))
			{
				if (0 == dgets(buffer, BIG_BUFFER_SIZE, des,
						(char *) 0))
					flushing = 0;
				
			}
			break;
		}
	}
	/* make sure we've read a full line from server */
	FD_ZERO(&rd);
	FD_SET(des, &rd);
	if (new_select(&rd, (fd_set *) 0, &timeout) > 0)
		dgets(buffer, BIG_BUFFER_SIZE, des, (char *) 0);
	(void) dgets_timeout(old_timeout);
}

/*
 * set_server_whois: sets the whois value for the given server index.  If the
 * whois value is 0, it assumes the server doesn't send End of WHOIS commands
 * and the whois.c routines use the old fashion way of getting whois info. If
 * the whois value is non-zero, then the server sends End of WHOIS and things
 * can be done more effienciently 
 */
void
set_server_whois(server_index, value)
	int	server_index,
		value;
{
	server_list[server_index].whois = value;
}

/* get_server_whois: Returns the whois value for the given server index */
int
get_server_whois(server_index)
	int	server_index;
{
	if (server_index == -1)
		server_index = primary_server;
	return (server_list[server_index].whois);
}


void
set_server_2_6_2(server_index, value)
	int	server_index,
		value;
{
	set_server_flag(server_index, SERVER_2_6_2, value);
}

int
get_server_2_6_2(server_index)
	int	server_index;
{
	if (server_index == -1)
		server_index = primary_server;
	return (get_server_flag(server_index, SERVER_2_6_2));
}

void
set_server_flag(server_index, flag, value)
	int	server_index;
	int	flag;
	int	value;
{
	if (server_index == -1)
		server_index = primary_server;
	if (value)
		server_list[server_index].flags |= flag;
	else
		server_list[server_index].flags &= ~flag;
}

int
get_server_flag(server_index, value)
	int	server_index;
	int	value;
{
	if (server_index == -1)
		server_index = primary_server;
	return server_list[server_index].flags & value;
}

/*
 * set_server_version: Sets the server version for the given server type.  A
 * zero version means pre 2.6, a one version means 2.6 aso. (look server.h
 * for typedef)
 */
void
set_server_version(server_index, version)
	int	server_index;
	int	version;
{
	if (server_index == -1)
		server_index = primary_server;
	server_list[server_index].version = version;
}

/*
 * get_server_version: returns the server version value for the given server
 * index 
 */
int
get_server_version(server_index)
	int	server_index;
{
	if (server_index == -1)
		server_index = primary_server;
	if (server_index == -1)
		return DEFAULT_SERVER_VERSION;
	else
		return (server_list[server_index].version);
}

/* get_server_name: returns the name for the given server index */
char	*
get_server_name(server_index)
	int	server_index;
{
	if (server_index == -1)
		server_index = primary_server;
	return (server_list[server_index].name);
}

/* set_server_itsname: returns the server's idea of its name */
char	*
get_server_itsname(server_index)
	int	server_index;
{
	if (server_index == -1)
		server_index = primary_server;
	if (server_list[server_index].itsname)
		return server_list[server_index].itsname;
	else
		return server_list[server_index].name;
}

void
set_server_itsname(server_index, name)
	int	server_index;
	char	*name;
{
	if (server_index == -1)
		server_index = primary_server;
	malloc_strcpy(&server_list[server_index].itsname, name);
}

/*
 * is_server_open: Returns true if the given server index represents a server
 * with a live connection, returns false otherwise 
 */
int
is_server_open(server_index)
	int	server_index;
{
	if (server_index < 0) return (0);
		return (server_list[server_index].read != -1);
}

/*
 * is_server_connected: returns true if the given server is connected.  This
 * means that both the tcp connection is open and the user is properly
 * registered 
 */
int
is_server_connected(server_index)
	int	server_index;
{
	return (server_list[server_index].connected);
}

/* get_server_port: Returns the connection port for the given server index */
int
get_server_port(server_index)
	int	server_index;
{
	if (server_index == -1)
		server_index = primary_server;
	return (server_list[server_index].port);
}

/*
 * get_server_nickname: returns the current nickname for the given server
 * index 
 */
char	*
get_server_nickname(server_index)
	int	server_index;
{
	if ((server_index != -1) && server_list[server_index].nickname)
		return (server_list[server_index].nickname);
	else
		return (nickname);
}



/* get_server_qhead - get the head of the whois queue */
WhoisQueue *
get_server_qhead(server_index)
	int	server_index;
{
	if (server_index != -1)
		return server_list[server_index].WQ_head;
	else
		return WQ_head;
}

/* get_server_whois_stuff */
WhoisStuff *
get_server_whois_stuff(server_index)
	int	server_index;
{
	if (server_index == -1)
		server_index = primary_server;
	return &server_list[server_index].whois_stuff;
}

/* get_server_qtail - get the tail of the whois queue */
WhoisQueue *
get_server_qtail(server_index)
	int	server_index;
{
	if (server_index !=-1)
		return server_list[server_index].WQ_tail;
	else
		return WQ_tail;
}



/* set_server_qhead - set the head of the whois queue */
void
set_server_qhead(server_index, value)
	int	server_index;
	WhoisQueue *value;
{
	if (server_index != -1)
		server_list[server_index].WQ_head = value;
	else
		WQ_head = value;
}

/* set_server_qtail - set the tail of the whois queue */
void
set_server_qtail(server_index, value)
	int	server_index;
	WhoisQueue *value;
{
	if (server_index !=-1)
		server_list[server_index].WQ_tail = value;
	else
		WQ_tail = value;
}



/*
 * get_server_operator: returns true if the user has op privs on the server,
 * false otherwise 
 */
int
get_server_operator(server_index)
	int	server_index;
{
	return (server_list[server_index].operator);
}

/*
 * set_server_operator: If flag is non-zero, marks the user as having op
 * privs on the given server.  
 */
void
set_server_operator(server_index, flag)
	int	server_index;
	int	flag;
{
	server_list[server_index].operator = flag;
}

/*
 * set_server_nickname: sets the nickname for the given server to nickname.
 * This nickname is then used for all future connections to that server
 * (unless changed with NICK while connected to the server 
 */
void
set_server_nickname(server_index, nick)
	int	server_index;
	char	*nick;
{
	if (server_index != -1)
	{
		malloc_strcpy(&(server_list[server_index].nickname), nick);
		if (server_index == primary_server)
			strmcpy(nickname,nick,NICKNAME_LEN);
	}
	update_all_status();
}

void
set_server_motd(server_index, flag)
	int	server_index;
	int	flag;
{
	if (server_index != -1)
		server_list[server_index].motd = flag;
}

int
get_server_motd(server_index)
	int	server_index;
{
	if (server_index != -1)
		return(server_list[server_index].motd);
	return (0);
}

void
server_is_connected(server_index, value)
	int	server_index,
		value;
{
	server_list[server_index].connected = value;
	if (value)
		server_list[server_index].eof = 0;
}

/* send_to_server: sends the given info the the server */
void
#ifdef HAVE_STDARG_H
send_to_server(char *format, ...)
#else
send_to_server(format, arg1, arg2, arg3, arg4, arg5,
		arg6, arg7, arg8, arg9, arg10)
	char	*format;
	char	*arg1,
		*arg2,
		*arg3,
		*arg4,
		*arg5,
		*arg6,
		*arg7,
		*arg8,
		*arg9,
		*arg10;
#endif
{
	static	int	in_send_to_server = 0;
	char	buffer[BIG_BUFFER_SIZE + 1];	/* make this buffer *much*
						 * bigger than needed */
	char	*buf = buffer;
	int	len,
		des;
	int	server = from_server;
#ifdef HAVE_STDARG_H
	va_list vlist;

	va_start(vlist, format);
#endif

	if (in_send_to_server)
		return;
	in_send_to_server = 1;
	if (server == -1)
		server = primary_server;
	if (server != -1 && ((des = server_list[server].write) != -1) &&
	    (server_list[server].flags & LOGGED_IN) )
	{
		server_list[server].sent = 1;
#ifdef HAVE_STDARG_H
		vsprintf(buf, format, vlist);
		va_end(vlist);
#else
			     
		sprintf(buf, format, arg1, arg2, arg3, arg4, arg5,
		    arg6, arg7, arg8, arg9, arg10);
#endif
		len = strlen(buffer);
		if (len > (IRCD_BUFFER_SIZE - 2))
			buffer[IRCD_BUFFER_SIZE - 2] = (char) 0;
		strmcat(buffer, "\n", IRCD_BUFFER_SIZE);
		send(des, buffer, strlen(buffer), 0);
	}
	else
	    say("You are not connected to a server, use /SERVER to connect.");
	in_send_to_server = 0;
}

#ifdef HAVE_SYS_UN_H
/*
 * Connect to a UNIX domain socket. Only works for servers.
 * submitted by Avalon for use with server 2.7.2 and beyond.
 */
int
connect_to_unix(port, path)
	int	port;
	char	*path;
{
	struct	sockaddr_un un;
	int	    sock;

	sock = socket(AF_UNIX, SOCK_STREAM, 0);

	un.sun_family = AF_UNIX;
	sprintf(un.sun_path, "%-.100s/%-.6d", path, port);

	if (connect(sock, (struct sockaddr *)&un, strlen(path)+2) == -1)
	{
		new_close(sock);
		return -1;
	}
	return sock;
}
#endif /* HAVE_SYS_UN_H */

/*
 * close_all_server: Used whn creating new screens to close all the open
 * server connections in the child process...
 */
extern	void
close_all_server()
{
	int	i;

	for (i = 0; i < number_of_servers; i++)
	{
		if (server_list[i].read != -1)
			new_close(server_list[i].read);
		if (server_list[i].write != -1)
			new_close(server_list[i].write);
	}
}

extern	char	*
create_server_list()
{
	int	i;
	char	*value = (char *) 0;

	*buffer = '\0';
	for (i = 0; i < number_of_servers; i++)
		if (server_list[i].read != -1)
		{
			strcat(buffer, server_list[i].itsname);
			strcat(buffer, " ");
		}
	malloc_strcpy(&value, buffer);

	return value;
}

static	void
add_to_server_buffer(server, buf)
	int	server;
	char	*buf;
{
	if (buf && *buf)
	{
		if (server_list[server].buffer)
			malloc_strcat(&server_list[server].buffer, buf);
		else
			malloc_strcpy(&server_list[server].buffer, buf);
	}
}

void
disconnectcmd(command, args, subargs)
	char	*command,
		*args,
		*subargs;
{
	char	*server;
	char	*message;
	int	i;

	if ((server = next_arg(args, &args)) != NULL)
	{
		i = parse_server_index(server);
		if (-1 == i)
		{
			say("No such server!");
			return;
		}
	}
	else
		i = get_window_server(0);
	/*
	 * XXX - this is a major kludge.  i should never equal -1 at
	 * this point.  we only do this because something has gotten
	 * *really* confused at this point.  .mrg.
	 */
	if (i == -1)
	{
		for (i = 0; i < number_of_servers; i++)
		{
			clear_channel_list(i);
			server_list[i].eof = -1;
			new_close(server_list[i].read);
			new_close(server_list[i].write);
		}
		save_chan_from = -1;
		goto done;
	}
	if (!args || !*args)
		message = "Disconnecting";
	else
		message = args;
	if (-1 == server_list[i].write)
	{
		say("That server isn't connected!");
		return;
	}
	server = server_list[i].itsname ? server_list[i].itsname :
		server_list[i].name ? server_list[i].name : "unknown?";
	say("Disconnecting from server %s", server);
	save_chan_from = i;
	close_server(i, message);
	server_list[i].eof = 1;
done:
	clean_whois_queue();
	window_check_servers();
	if (!connected_to_server)
	{
		say("You are not connected to a server. Use /SERVER to connect.");
	}
	else
	{
		save_chan_from = -1;
		clear_channel_list(i);
	}
}

int
find_server_group(group, add)
	char	*group;
	int	add;
{
	static	int	next = 1;
	SGroup *g = (SGroup *) find_in_list((List **) &server_group_list, group, 0);

	if (g)
		goto end;

	if (!add)
		return 0;

	g = (SGroup *) new_malloc(sizeof(SGroup));
	g->name = (char *) 0;
	malloc_strcpy(&g->name, group);
	g->number = next++;
	add_to_list((List **) &server_group_list, (List *) g);
end:
	return g->number;
}

char	*
find_server_group_name(number)
	int	number;
{
	SGroup *g = server_group_list;

	for (; g; g = g->next)
		if (g->number == number)
			return g->name;
	return empty_string;
}
