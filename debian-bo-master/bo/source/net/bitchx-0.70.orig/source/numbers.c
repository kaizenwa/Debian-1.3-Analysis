/*
 * numbers.c:handles all those strange numeric response dished out by that
 * wacky, nutty program we call ircd 
 *
 *
 * written by michael sandrof
 *
 * copyright(c) 1990 
 *
 * see the copyright file, or do a help ircii copyright 
 */


#include "irc.h"

#include "input.h"
#include "edit.h"
#include "ircaux.h"
#include "vars.h"
#include "lastlog.h"
#include "list.h"
#include "hook.h"
#include "server.h"
#include "whois.h"
#include "numbers.h"
#include "window.h"
#include "screen.h"
#include "output.h"
#include "names.h"
#include "whois.h"
#include "funny.h"
#include "parse.h"
#include "misc.h"
#include "status.h"
#include "timer.h"
#include "tcl_bx.h"

static	void	reset_nickname		_((void));
static	void	nickname_in_use		_((char *, char **));
static	void	password_sendline	_((char *, char *));
static	void	get_password		_((void));
static	void	nickname_sendline	_((char *, char *));
static	void	channel_topic		_((char *, char **, int));
static	void	not_valid_channel	_((char *, char **));
static	void	cannot_join_channel	_((char *, char **));
static	void	version1		_((char *, char **));
static	void	invite			_((char *, char **));
static 	void	fudge_nickname		_((void));

static	int	already_doing_reset_nickname = 0;
static	int	number_of_bans = 0;

#ifdef WANT_DLL
/* this is kinda like a hook */
NumericFunction *numeric_dll = NULL;
#endif

extern void	parse_364		_((char *, char *, char *));
extern void	parse_365		_((char *, char *, char *));
extern int	stats_k_grep		_((char **));
extern void	who_handlekill		_((char *, char *, char *));
extern void	handle_tracekill	_((char *, char *, char *));
extern int	link_look;
extern int	no_hook_notify;
extern int	in_e_nick;
extern int	who_on_join;
extern int	doing_who;
extern int	in_who_kill;
extern int	in_trace_kill;

char	*thing_ansi = NULL;


/*
 * numeric_banner: This returns in a static string of either "xxx" where
 * xxx is the current numeric, or "***" if SHOW_NUMBERS is OFF 
 */
char	*numeric_banner(void)
{
	static	char	thing[4];
	if (!get_int_var(SHOW_NUMERICS_VAR))
		return (thing_ansi?thing_ansi:empty_string);
	sprintf(thing, "%3.3u", -current_numeric);
	return (thing);
}

int check_sync(int comm, char *channel, char *nick, char *whom, char *bantime, ChannelList *chan)
{
ChannelList *tmp = NULL;
BanList *new;

	context;	

	if (!chan)
		if (!(tmp = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
			return -1; /* channel lookup problem */
	if (!in_join_list(channel, from_server))
		return -1;

	if (tmp == NULL)
		tmp = chan;	

	switch (comm)
	{
		case 367: /* bans on channel */
		{
			if (tmp)
			{
				new = (BanList *)new_malloc(sizeof(BanList));
				malloc_strcpy(&new->ban, nick);
				if (bantime)
					new->time = strtoul(bantime, NULL, 10);
				else
					new->time = time(NULL);
				if (whom)
					new->setby = m_strdup(whom);
				add_to_list((List **)&tmp->bans,(List *)new); 
			}
		}
		default:
			break;
	}	
	return 0;
#if 0
	if (!chan)
		if (!(tmp = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
			return -1; /* channel lookup problem */
	if (tmp == NULL)
		tmp = chan;	
	if (!tmp->flags.got_modes && !tmp->flags.got_who && !tmp->flags.got_bans)
		return -1;	/* already synced this channel */
	
	switch (comm)
	{
		case 315: /* End of who */
		{			
			if (tmp->flags.got_who)
				tmp->flags.got_who = 0;
			break;
		}
		case 368: /* End of Bans */
		{
			if (tmp->flags.got_bans)
				tmp->flags.got_bans = 0;
			break;
		}
		case 324: /* End of channel mode */
		{
			if (tmp->flags.got_modes)
				tmp->flags.got_modes = 0;
			break;
		}
		case 367: /* bans on channel */
		{
			new = (BanList *)new_malloc(sizeof(BanList));
			malloc_strcpy(&new->ban, nick);
			if (bantime)
				new->time = strtoul(bantime, NULL, 10);
			else
				new->time = time(NULL);
			if (whom)
				new->setby = m_strdup(whom);
			add_to_list((List **)&tmp->bans,(List *)new); 
		}
		default:
			break;
	}
	
	
	if (!tmp->flags.got_modes && !tmp->flags.got_who && !tmp->flags.got_bans)
	{

		struct timeval tv;
		get_time(&tv);
		message_to(tmp->window?tmp->window->refnum:0); /*get_target_by_server(tmp->server));*/
		if (do_hook(CHANNEL_SYNCH_LIST, "%s %1.3f", channel, time_diff(tmp->join_time,tv)))
			bitchsay("Join to %s was synced in %1.3f secs!!", channel, time_diff(tmp->join_time,tv));
		delay_check_auto(channel);
		update_all_status(curr_scr_win, NULL, 0);
		message_to(0);
		return 1; /*  just finished all syncs */
	}
	else
		return 0; /* not done just yet */
#endif
}

static int check_server_sync(char *from, char **ArgList)
{
static char *desync = NULL;

	context;
	if (!desync || (desync && !match(desync, from)))
	{
		if (!match(from, get_server_name(from_server)))
		{
			malloc_strcpy(&desync, from);
			if (do_hook(DESYNC_MESSAGE_LIST, "%s %s", from, ArgList[0]))
				put_it("%s", convert_output_format(get_string_var(FORMAT_DESYNC_VAR), "%s %s %s", update_clock(GET_TIME), ArgList[0], from));
			return 1;
		}	 
	} 
	return 0;
}

/*
 * display_msg: handles the displaying of messages from the variety of
 * possible formats that the irc server spits out.  you'd think someone would
 * simplify this 
 */
void display_msg(char *from, char **ArgList)
{
	char	*ptr;
	char	*rest;

	context;
	rest = PasteArgs(ArgList, 0);
	if (from && (my_strnicmp(get_server_itsname(from_server), from,
			strlen(get_server_itsname(from_server))) == 0))
		from = NULL;
	if ((ptr = (char *) index(rest, ':')) != NULL)
	{
		*(ptr++) = (char) 0;
		if (strlen(rest))
		{
			if (from)
				put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG1_FROM_VAR), "%s %s %s %s", update_clock(GET_TIME), rest, ptr, from));
			else
				put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG2_VAR), "%s %s %s", update_clock(GET_TIME), rest, ptr));
		}
		else
		{
			if (from)
				put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG2_FROM_VAR), "%s %s %s", update_clock(GET_TIME), ptr, from));
			else
				put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG2_VAR), "%s %s", update_clock(GET_TIME), ptr));
		}
	}
	else
	{
		if (from)
			put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG1_FROM_VAR), "%s %s %s", update_clock(GET_TIME), rest, from));
		else
			put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG2_VAR), "%s %s", update_clock(GET_TIME), rest));
	}
}

/*
 * password_sendline: called by send_line() in get_password() to handle
 * hitting of the return key, etc 
 */
static	void
password_sendline(data, line)
	char	*data;
	char	*line;
{
	int	new_server;

	new_server = atoi(line);
	set_server_password(new_server, line);
	connect_to_server_by_refnum(new_server, -1);
}

/*
 * get_password: when a host responds that the user needs to supply a
 * password, it gets handled here!  the user is prompted for a password and
 * then reconnection is attempted with that password.  but, the reality of
 * the situation is that no one really uses user passwords.  ah well 
 */
static	void
get_password()
{
	char	server_num[8];

	say("password required for connection to server %s",
		get_server_name(from_server));
	close_server(from_server, empty_string);
        if (!dumb)
	{
		strcpy(server_num, ltoa(from_server));
		add_wait_prompt("Server Password:", password_sendline,
			server_num, WAIT_PROMPT_LINE);
	}
}

/*ARGSUSED*/
static	void
nickname_sendline(data, nick)
	char	*data;
	char	*nick;
{
	int	new_server, server;

	new_server = atoi(data);
	if ((nick = check_nickname(nick)) != NULL)
	{
		server = from_server;
		from_server = new_server;
		send_to_server("NICK %s", nick);
		if (new_server == primary_server)
			strmcpy(nickname, nick, NICKNAME_LEN);
		set_server_nickname(new_server, nick);
		from_server = server;
		already_doing_reset_nickname = 0;
		update_all_status(curr_scr_win, NULL, 0);
	}
	else
	{
		say("illegal nickname, try again");
		if (!dumb)
			add_wait_prompt("Nickname: ", nickname_sendline, data,
					WAIT_PROMPT_LINE);
	}
}

/*
 * reset_nickname: when the server reports that the selected nickname is not
 * a good one, it gets reset here. 
 */
static	void
reset_nickname()
{
	char	server_num[10];

	if (already_doing_reset_nickname)
		return;
	say("You have specified an illegal nickname");
	if (!dumb)
	{
		already_doing_reset_nickname = 1;
		say("Please enter your nickname");
		strcpy(server_num, ltoa(from_server));
		add_wait_prompt("Nickname: ", nickname_sendline, server_num,
			WAIT_PROMPT_LINE);
	}
	update_all_status(curr_scr_win, NULL, 0);
}

/*ARGSUSED*/
static	void channel_topic(char *from, char **ArgList, int what)
{
	char	*topic, *channel;
	ChannelList *chan;
	
	context;
	if (ArgList[1] && is_channel(ArgList[0]))
	{
		channel = ArgList[0];
		topic = ArgList[1];
		message_from(channel, LOG_CRAP);
		if (what == 333 && ArgList[2])
			put_it("%s", convert_output_format(get_string_var(FORMAT_TOPIC_SETBY_VAR), "%s %s %s %d", update_clock(GET_TIME), channel, topic, strtoul(ArgList[2], NULL, 10)));
		else
		{
			if ((chan = lookup_channel(channel, from_server, 0)))
				malloc_strcpy(&chan->topic, topic);
			put_it("%s", convert_output_format(get_string_var(FORMAT_TOPIC_VAR), "%s %s %s", update_clock(GET_TIME), channel, topic));

		}		
	}
	else
	{
		PasteArgs(ArgList, 0);
		message_from(NULL, LOG_CURRENT);
		put_it("%s", convert_output_format(get_string_var(FORMAT_TOPIC_VAR), "%s %s", update_clock(GET_TIME), ArgList[0]));
	}
}

static	void
nickname_in_use(from, ArgList)
	char	*from,
		**ArgList;
{
	context;
	PasteArgs(ArgList, 0);
	if (is_server_connected(from_server))
		if (do_hook(current_numeric, "%s", *ArgList))
			display_msg(from, ArgList);
	else if (never_connected || from_server != primary_server ||
	    !attempting_to_connect)
	{
		if (!in_e_nick)
		{
			fudge_nickname();
			if (do_hook(current_numeric, "%s", *ArgList))
				display_msg(from, ArgList);
		}
	}
	else
	{
		send_to_server("USER %s %s %s :%s", username,
			(send_umode && *send_umode) ? send_umode : hostname,
			server_list[from_server].name, realname);
		send_to_server("NICK %s", get_server_nickname(from_server));
		if (send_umode && *send_umode == '+')
			send_to_server("MODE %s %s", get_server_nickname(from_server), send_umode);
	}
}

static	void
not_valid_channel(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*channel;
	char	*s;

	context;
	if (!(channel = ArgList[0]) || !ArgList[1])
		return;
	PasteArgs(ArgList, 1);
	s = get_server_name(from_server);
	if (0 == my_strnicmp(s, from, strlen(s)))
	{
		remove_channel(channel, from_server);
		put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG2_VAR), "%s %s %s", update_clock(GET_TIME), channel, ArgList[1]));
	}
}

/* from ircd .../include/numeric.h */
/*
#define ERR_CHANNELISFULL    471
#define ERR_INVITEONLYCHAN   473
#define ERR_BANNEDFROMCHAN   474
#define ERR_BADCHANNELKEY    475
#define ERR_BADCHANMASK      476
*/
static	void
cannot_join_channel(from, ArgList)
	char	*from,	
		**ArgList;
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	char	*f = NULL;
	char	*chan;		
	context;
#if 0
	if (ArgList[0])
		remove_channel(ArgList[0], from_server);
#endif
	if (ArgList[0])
		chan = ArgList[0];
	else
		chan = get_chan_from_join_list(from_server);

	remove_from_join_list(chan, from_server);
	if (!is_on_channel(chan, from_server, get_server_nickname(from_server)))
		remove_channel(chan, from_server);
	else
		return;


	PasteArgs(ArgList, 0);
	strcpy(buffer, ArgList[0]);
	switch(-current_numeric)
	{
	case 471:
		strcat(buffer, " (Channel is full)");
		f = get_string_var(FORMAT_471_VAR);
		break;
	case 473:
		strcat(buffer, " (You must be invited)");
		f = get_string_var(FORMAT_473_VAR);
		break;
	case 474:
		strcat(buffer, " (You are banned)");
		f = get_string_var(FORMAT_474_VAR);
		break;
	case 475:
		strcat(buffer, " (Bad channel key)");
		f = get_string_var(FORMAT_475_VAR);
		break;
	case 476:
		strcat(buffer, " (Bad channel mask)");
		f = get_string_var(FORMAT_476_VAR);
		break;
	default:
		return;
	}
if (f)
	put_it("%s", convert_output_format(f, "%s %s", update_clock(GET_TIME), ArgList[0]));
else
	put_it("%s %s", numeric_banner(), buffer);
}


/*ARGSUSED*/
static	void
version1(from, ArgList)
	char	*from,
		**ArgList;
{
	context;
	if (ArgList[2])
	{
		PasteArgs(ArgList, 2);
		put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_VAR), "Server %s %s %s", ArgList[0], ArgList[1], ArgList[2]));
	}
	else
	{
		PasteArgs(ArgList, 1);
		put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_VAR), "Server %s %s", ArgList[0], ArgList[1]));
	}
}


/*ARGSUSED*/
static	void
invite(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*who,
		*channel;

	context;
	if ((who = ArgList[0]) && (channel = ArgList[1]))
	{
		message_from(channel, LOG_CRAP);
		if (do_hook(current_numeric, "%s %s %s", from, who, channel))
			put_it("%s", convert_output_format(get_string_var(FORMAT_INVITE_USER_VAR), "%s %s %s", update_clock(GET_TIME), who, channel));
	}
}

extern AJoinList *ajoin_list;

/*
 * numbered_command: does (hopefully) the right thing with the numbered
 * responses from the server.  I wasn't real careful to be sure I got them
 * all, but the default case should handle any I missed (sorry) 
 */
void
numbered_command(from, comm, ArgList)
	char	*from;
	int	comm;
	char	**ArgList;
{
	char	*user;
	char	none_of_these = 0;
	int	flag,
		lastlog_level;
	int	i;
	
	AJoinList *tmp = NULL;
#ifdef WANT_DLL
	NumericFunction *dll = NULL;
#endif
	int	code = 0;
		
	context;
	if (!ArgList[1] || !from || !*from)
		return;
	
	user = (*ArgList[0]) ? ArgList[0] : NULL;

	lastlog_level = set_lastlog_msg_level(LOG_CRAP);
	message_from(NULL, LOG_CRAP);
	ArgList++;
	current_numeric = -comm;	/* must be negative of numeric! */

#ifdef WANT_DLL
	if (numeric_dll)
	{
		for (dll = numeric_dll; dll; dll = dll->next)
		{
			if (dll->number == comm)
				if ((code = (dll->func) (from, user, ArgList)))
					return;
		}
	}
#endif
	switch (comm)
	{
	case 001:	/* #define RPL_WELCOME          001 */
	{		
		char buffer[BIG_BUFFER_SIZE+1]; 
		bitchsay("For more information about \002BitchX\002 type \002/about\002");
		set_server2_8(from_server, 1);
		set_server_nickname(from_server, user);
		load_scripts();
		PasteArgs(ArgList, 0);
		if (do_hook(current_numeric, "%s %s", from, *ArgList)) 
			display_msg(from, ArgList);
		if (get_int_var(AUTO_REJOIN_VAR))
		{
			for(tmp = ajoin_list, i = 0; tmp; tmp = tmp->next, i+=3)
			{
				sprintf(buffer, "JOIN %s %s", tmp->name, tmp->key?tmp->key:"");
				add_timer("", i, NULL, buffer, NULL);
			}
		}
		break;
	}		
/* should do something with this some day, 2.8 had channel/user mode switches */
	case 004:	/* #define RPL_MYINFO           004 */
		got_initial_version_28(ArgList);
		load_scripts();
		PasteArgs(ArgList, 0);
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		break;

	case 251:
	case 252:
	case 253:
	case 254:
	case 255:
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		break;

	case 301:		/* #define RPL_AWAY             301 */
		user_is_away(from, ArgList);
		break;

	case 302:		/* #define RPL_USERHOST         302 */
		userhost_returned(from, ArgList);
		break;

	case 303:		/* #define RPL_ISON             303 */
		ison_returned(from, ArgList);
		break;

	case 311:		/* #define RPL_WHOISUSER        311 */
		whois_name(from, ArgList);
		break;

	case 312:		/* #define RPL_WHOISSERVER      312 */
		whois_server(from, ArgList);
		break;

	case 313:		/* #define RPL_WHOISOPERATOR    313 */
		whois_oper(from, ArgList);
		break;

	case 314:		/* #define RPL_WHOWASUSER       314 */
		whowas_name(from, ArgList);
		break;

	case 316:		/* #define RPL_WHOISCHANOP      316 */
/*		whois_chop(from, ArgList);*/
		break;

	case 317:		/* #define RPL_WHOISIDLE        317 */
		whois_lastcom(from, ArgList);
		break;

	case 318:		/* #define RPL_ENDOFWHOIS       318 */
		end_of_whois(from, ArgList);
		break;

	case 319:		/* #define RPL_WHOISCHANNELS    319 */
		whois_channels(from, ArgList);
		break;

	case 321:		/* #define RPL_LISTSTART        321 */
		ArgList[0] = "Channel\0Users\0Topic";
		ArgList[1] = ArgList[0] + 8;
		ArgList[2] = ArgList[1] + 6;
		ArgList[3] = NULL;
		funny_list(from, ArgList);
		break;

	case 322:		/* #define RPL_LIST             322 */
		if (is_server_connected(from_server) && strcmp(from, server_list[from_server].itsname)) 
		{
			say("322 crash attempt from hacked server %s", from);
			break;
		}
		funny_list(from, ArgList);
		break;

	case 324:		/* #define RPL_CHANNELMODEIS    324 */
		funny_mode(from, ArgList);
		break;

	case 341:		/* #define RPL_INVITING         341 */
		invite(from, ArgList);
		break;

	case 352:		/* #define RPL_WHOREPLY         352 */
		if (in_who_kill)
		{
			who_handlekill(ArgList[4], ArgList[1], ArgList[2]);
			break;
		}
		whoreply(NULL, ArgList);
		break;

	case 353:		/* #define RPL_NAMREPLY         353 */
		funny_namreply(from, ArgList);
		break;

	case 366:		/* #define RPL_ENDOFNAMES       366 */
	{
		int	flag = 1;
		char	*tmp = NULL,
			*chan;
		PasteArgs(ArgList, 0);
		if (get_int_var(SHOW_END_OF_MSGS_VAR))
			flag = do_hook(current_numeric, "%s %s", from, ArgList[0]);
		malloc_strcpy(&tmp, ArgList[0]);
		chan = strtok(tmp," ");
                               		
		if (!in_join_list(chan, from_server))
		{
			if (get_int_var(SHOW_END_OF_MSGS_VAR) && flag)
				display_msg(from, ArgList);
		} else
			got_info(chan,from_server, GOTNAMES);
		new_free(&tmp);
	}
	break;

	case 381: 		/* #define RPL_YOUREOPER        381 */
		PasteArgs(ArgList, 0);
		if (!is_server_connected(from_server))
		{
			say("Odd Server stuff from %s: %s",from, ArgList[0]);
			return; 
		}
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
		{
			if (get_string_var(FORMAT_381_VAR))
				put_it("%s", 
					convert_output_format(
					get_string_var(FORMAT_381_VAR), 
					"%s %s %s", 
					update_clock(GET_TIME),from, *ArgList));
			else
				display_msg(from, ArgList);
			set_int_var(SHOW_SERVER_CRAP_VAR, 1);
		}
		break;

	case 401:		/* #define ERR_NOSUCHNICK       401 */
		no_such_nickname(from, ArgList);
		break;

	case 421:		/* #define ERR_UNKNOWNCOMMAND   421 */
		if (check_screen_redirect(ArgList[0]))
			break;
		if (check_wait_command(ArgList[0]))
			break;
		if (!strcmp(ArgList[0], "WAITFORNOTIFY"))
		{
			no_hook_notify = 0;
			break;
		}
		PasteArgs(ArgList, 0);
		
		flag = do_hook(current_numeric, "%s %s", from, *ArgList);
		if (flag)
			display_msg(from, ArgList);
		break;

	case 432:		/* #define ERR_ERRONEUSNICKNAME 432 */
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		fudge_nickname();
		break;
	case 437:
	case 433:		/* #define ERR_NICKNAMEINUSE    433 */ 
		if (is_server_connected(from_server) && server_list[from_server].itsname && strcmp(from, server_list[from_server].itsname))
		{
			say("433 nicknameinuse attempt from hacked server %s", from);
			break;
		}
		nickname_in_use(from, ArgList);
		fudge_nickname();
		break;

	case 464:		/* #define ERR_PASSWDMISMATCH   464 */
		if (is_server_connected(from_server)  && server_list[from_server].itsname && strcmp(from, server_list[from_server].itsname)) 
		{
			say("464 crash attempt from hacked server %s", from);
			break;
		}
		PasteArgs(ArgList, 0);
		if (!is_server_connected(from_server))
		{
			say("Odd Server stuff from %s: %s",from, ArgList[0]);
			return; 
		}
		flag = do_hook(current_numeric, "%s %s", from, ArgList[0]);
		if (oper_command)
		{
			if (flag)
				display_msg(from, ArgList);
		}
		else
			get_password();
		break;

	case 465:		/* #define ERR_YOUREBANNEDCREEP 465 */
	{

		if (is_server_connected(from_server)  && server_list[from_server].itsname && strcmp(from, server_list[from_server].itsname)) 
		{
			say("465 crash attempt from hacked server %s", from);
			break;
		}
		PasteArgs(ArgList, 0);
		if (!is_server_connected(from_server))
			return;
		if (do_hook(current_numeric, "%s %s", from, ArgList[0]))
			display_msg(from, ArgList);
				
		close_server(from_server, empty_string);
		window_check_servers();
		if (from_server == primary_server)
			get_connected(from_server+1);
		break;
	}

	case 484:
	{
		if (do_hook(current_numeric, "%s %s", from, ArgList[0]))
			display_msg(from, ArgList);
		set_server_flag(from_server, USER_MODE_R, 1);
		break;
	}
	case 367:
		number_of_bans++;
		if (check_sync(comm, ArgList[0], ArgList[1], ArgList[2], ArgList[3], NULL) == 0)
			break;
		if (ArgList[2])
		{
			time_t tme = (time_t) strtoul(ArgList[3], NULL, 10);
			if (do_hook(current_numeric, "%s %s %s %s %s", 
				from, ArgList[0], ArgList[1], ArgList[2], ArgList[3]))
				put_it("%s", convert_output_format(get_string_var(FORMAT_BANS_VAR), "%d %s %s %s %d", number_of_bans, ArgList[0], ArgList[1], ArgList[2], tme));
		}
		else
			if (do_hook(current_numeric, "%s %s %s", from, ArgList[0], ArgList[1]))
				put_it("%s", convert_output_format(get_string_var(FORMAT_BANS_VAR), "%d %s %s %s %d", number_of_bans, ArgList[0], ArgList[1], "unknown", time(NULL)));
		break;
	case 368:		/* #define RPL_ENDOFBANLIST     368 */
	{
		if (got_info(ArgList[0], from_server, GOTBANS) == 0)
			break;

/*
		if (check_sync(comm, ArgList[0], NULL, NULL, NULL, NULL) == 0)
			break;
*/
		if (get_int_var(SHOW_END_OF_MSGS_VAR))
		{
			if (do_hook(current_numeric, "%s %d %s", from, number_of_bans, *ArgList))
			{
				put_it("%s Total Number of Bans on %s [%d]",
					numeric_banner(), ArgList[0],
					number_of_bans);
			}
		}
		number_of_bans = 0;
		break;
	}
	case 364:
		if (is_server_connected(from_server)  && server_list[from_server].itsname && strcmp(from, server_list[from_server].itsname))
		{
			say("364 links attempt from hacked server %s", from);
			break;
		}
		if (comm == 364)
		{
			if (get_int_var(LLOOK_VAR) && link_look) 
			{
				parse_364(ArgList[0], ArgList[1], ArgList[2] ? ArgList[2] : "0");
				break;
			} 
			
			if ((do_hook(current_numeric, "%s %s %s", ArgList[0], ArgList[1], ArgList[2])))
			{
				if (get_string_var(FORMAT_LINKS_VAR))
				{
					put_it("%s", convert_output_format(get_string_var(FORMAT_LINKS_VAR), "%s %s %s", ArgList[0], ArgList[1], ArgList[2] ? ArgList[2] : "0"));
					break;
				}
			}
		}
	case 365:		/* #define RPL_ENDOFLINKS       365 */
		if (comm == 365 && get_int_var(LLOOK_VAR) && link_look) {
			parse_365(ArgList[0], ArgList[1],NULL);
			link_look--;
			break;
		}
	case 216: /* STATS k */
		if (comm == 216 && stats_k_grep(ArgList))
			break;
	case 404:
	case 442:
	case 482:
		if (comm == 404 || comm == 482 || comm == 442)
		{
			if (check_server_sync(from, ArgList))
			{
				send_to_server("WHO -server %s %s", from, ArgList[0]);
				break;
			}
		}
	case 204:
	case 205:
	case 206:
	case 203:
	case 207:
	case 209:
	case 262:
	case 402:
		if (in_trace_kill)
		{
			if (comm == 204 || comm == 206 || comm == 203 || comm == 207)
				break;
			if (comm == 205)
			{
				handle_tracekill(ArgList[2], NULL, NULL);
				break;
			}
			if (comm == 209 || comm == 262 || comm == 402)
			{
				handle_tracekill(NULL, NULL, NULL);
				break;
			}
		}
	
	/*
	 * The following accumulates the remaining arguments
	 * in ArgSpace for hook detection. We can't use
	 * PasteArgs here because we still need the arguments
	 * separated for use elsewhere.
	 */
	default:
		{
			char	*ArgSpace = NULL;
			int	i,
				len,
				do_message_from = 0;

			
			for (i = len = 0; ArgList[i]; len += strlen(ArgList[i++]))
				;
			len += (i - 1);
			ArgSpace = new_malloc(len + 1);
			ArgSpace[0] = '\0';
			/* this is cheating */

			if (ArgList[0] && is_channel(ArgList[0]))
				do_message_from = 1;
			for (i = 0; ArgList[i]; i++)
			{
				if (i)
					strcat(ArgSpace, " ");
				strcat(ArgSpace, ArgList[i]);
			}
			if (do_message_from)
				message_from(ArgList[0], LOG_CRAP);
			if (!do_hook(current_numeric, "%s %s", from, ArgSpace))
			{
				new_free(&ArgSpace);
				if (do_message_from)
					message_from(NULL, lastlog_level);
				return;
			}
			if (do_message_from)
				message_from(NULL, lastlog_level);
			new_free(&ArgSpace);
			none_of_these = 1;
		}
	}
	/* the following do not hurt the ircII if intercepted by a hook */
	if (none_of_these)
	{
		switch (comm)
		{
		case 221: 		/* #define RPL_UMODEIS          221 */
			put_it("%s Your user mode is [%s]", numeric_banner(), ArgList?ArgList[0]:" ");
			break;

		case 242:		/* #define RPL_STATSUPTIME      242 */
			PasteArgs(ArgList, 0);
			if (from && !my_strnicmp(get_server_itsname(from_server),
			    from, strlen(get_server_itsname(from_server))))
				from = NULL;
			if (from)
				put_it("%s %s from (%s)", numeric_banner(),
					*ArgList, from);
			else
				put_it("%s %s", numeric_banner(), *ArgList);			break;

		case 272:/* ENDOFSILENCE */
			PasteArgs(ArgList, 0);
			put_it("%s", convert_output_format(get_string_var(FORMAT_SILENCE_VAR), update_clock(GET_TIME), ArgList[0]));
			break;

		case 329:		/* #define CREATION_TIME	329 */
		{
			time_t tme;
			char *this_sucks;

			if (!ArgList[1] || !*ArgList[1])
				break;
			sscanf(ArgList[1], "%lu", &tme);
			this_sucks = ctime(&tme);
			this_sucks[strlen(this_sucks)-1] = '\0';		

			message_from(ArgList[0], LOG_CRAP);
			put_it("%s Channel %s was created at %s",numeric_banner(),
				ArgList[0], this_sucks);
			message_from(NULL, LOG_CURRENT);
			break;
		}
		case 332:		/* #define RPL_TOPIC            332 */
			channel_topic(from, ArgList, 332);
			break;
		case 333:
			channel_topic(from, ArgList, 333);
			break;
			
		case 351:		/* #define RPL_VERSION          351 */
			version1(from, ArgList);
			break;

		case 364:		/* #define RPL_LINKS            364 */
		{

			if (ArgList[2])
			{
				PasteArgs(ArgList, 2);
				put_it("%s %-20s %-20s %s", numeric_banner(),
					ArgList[0], ArgList[1], ArgList[2]);
			}
			else
			{
				PasteArgs(ArgList, 1);
				put_it("%s %-20s %s", numeric_banner(),
					ArgList[0], ArgList[1]);
			}
		}
		break;

		case 377:
		case 372:		/* #define RPL_MOTD             372 */
			if (!get_int_var(SUPPRESS_SERVER_MOTD_VAR) ||
			    !get_server_motd(from_server))
			{
				PasteArgs(ArgList, 0);
				put_it("%s %s", numeric_banner(), ArgList[0]);
			}
			break;

		case 375:		/* #define RPL_MOTDSTART        375 */
			if (!get_int_var(SUPPRESS_SERVER_MOTD_VAR) ||
			    !get_server_motd(from_server))
			{
				PasteArgs(ArgList, 0);
				put_it("%s %s", numeric_banner(), ArgList[0]);
			}
			break;

		case 376:		/* #define RPL_ENDOFMOTD        376 */
			if (get_int_var(SHOW_END_OF_MSGS_VAR) &&
			    (!get_int_var(SUPPRESS_SERVER_MOTD_VAR) ||
			    !get_server_motd(from_server)))
			{
				PasteArgs(ArgList, 0);
				put_it("%s %s", numeric_banner(), ArgList[0]);
			}
			set_server_motd(from_server, 0);
			break;

		case 384:		/* #define RPL_MYPORTIS         384 */
			PasteArgs(ArgList, 0);
			put_it("%s %s %s", numeric_banner(), ArgList[0], user);
			break;

		case 385:		/* #define RPL_NOTOPERANYMORE   385 */
			set_server_operator(from_server, 0);
			display_msg(from, ArgList);
			update_all_status(curr_scr_win, NULL, 0);
			break;

		case 403:		/* #define ERR_NOSUCHCHANNEL    403 */
			not_valid_channel(from, ArgList);
			break;

		case 432:		/* #define ERR_ERRONEUSNICKNAME 432 */
			display_msg(from, ArgList);
			reset_nickname();
			break;

		case 451:		/* #define ERR_NOTREGISTERED    451 */
	/*
	 * Sometimes the server doesn't catch the USER line, so
	 * here we send a simplified version again  -lynx 
	 */
			send_to_server("USER %s %s %s :%s", username,
				(send_umode && *send_umode) ? send_umode : hostname,
				server_list[from_server].name, realname);
			send_to_server("NICK %s",
				get_server_nickname(from_server));
			break;

		case 462:		/* #define ERR_ALREADYREGISTRED 462 */
			send_to_server("NICK %s",
				get_server_nickname(from_server));
			break;

		case 471:		/* #define ERR_CHANNELISFULL    471 */
		case 473:		/* #define ERR_INVITEONLYCHAN   473 */
		case 474:		/* #define ERR_BANNEDFROMCHAN   474 */
		case 475: 		/* #define ERR_BADCHANNELKEY    475 */
		case 476:		/* #define ERR_BADCHANMASK      476 */
			cannot_join_channel(from, ArgList);
			break;

#define RPL_CLOSEEND         363
#define RPL_SERVLISTEND      235
		case 315:		/* #define RPL_ENDOFWHO         315 */
			{
				if (in_join_list(ArgList[0], from_server))
				{
					if (who_on_join) 
						who_on_join--;
					got_info(ArgList[0], from_server, GOTWHO);
					break;
				} else if (who_on_join)
				{
					who_on_join--;
					break;
				}
				if (doing_who)
					doing_who--;
				if (in_who_kill)
				{
					who_handlekill(NULL, NULL, NULL);
					break;
				}
				if (get_int_var(SHOW_END_OF_MSGS_VAR))
					if (do_hook(current_numeric, "%s %s", from, ArgList[0]))
						put_it("%s %s %s",numeric_banner(), from, ArgList[0]);
			}
		case 316:
			break;

		case 323:               /* #define RPL_LISTEND          323 */
			funny_print_widelist();

		case 219:		/* #define RPL_ENDOFSTATS       219 */
			if (comm == 219)
				stats_k_grep_end();
		case 232:		/* #define RPL_ENDOFSERVICES    232 */
		case 365:		/* #define RPL_ENDOFLINKS       365 */
			if (comm == 365 && get_int_var(LLOOK_VAR) && link_look) {
				parse_365(ArgList[0], ArgList[1],NULL);
				link_look--;
				break;
			}
		case 369:		/* #define RPL_ENDOFWHOWAS      369 */
		case 374:		/* #define RPL_ENDOFINFO        374 */
		case 394:		/* #define RPL_ENDOFUSERS       394 */
			if (get_int_var(SHOW_END_OF_MSGS_VAR))
			{
				PasteArgs(ArgList, 0);
				if (do_hook(current_numeric, "%s %s", from, *ArgList))
					display_msg(from, ArgList);
			}
			break;
		default:
			display_msg(from, ArgList);
		}
	}
	set_lastlog_msg_level(lastlog_level);
}

/* Dont even think of calling this function with anything that isnt
 * really a nickname -- it may cause segfaults!
 */
/* This will generate up to 18 nicknames plus the 9-length(nickname)
 * that are unique but still have some semblance of the original.
 * This is intended to allow the user to get signed back on to
 * irc after a nick collision without their having to manually
 * type a new nick every time..
 * 
 * The func will try to make an intelligent guess as to when it is
 * out of guesses, and if it ever gets to that point, it will do the
 * manually-ask-you-for-a-new-nickname thing.
 */
static void fudge_nickname _((void))
{
	static char oldnick[NICKNAME_LEN+1] = {0};
	static int fudge_index = 0;
	char blah[4];
	int i;
	extern int from_server;
	static char nickname[NICKNAME_LEN + 1] = {0};

	context;
	if (in_e_nick)
		return;		/* if we're changing nickname, dont bother */

	strcpy(nickname, get_server_nickname(from_server));

	/* First time through... */
	if (!*oldnick)
	{
		strncpy(oldnick, nickname, NICKNAME_LEN);
		fudge_index = strlen(nickname);
	}
	else
	{
		/* Same nick as the last one we convoluted? */
		if (!my_stricmp(oldnick, nickname))
		{
			fudge_index++;
			if (fudge_index == NICKNAME_LEN-1)
			{
				/* give up... */
				reset_nickname();
				fudge_index = 0;
				return;
			}
		}
		/* Different nick then last time.. start over. */
		else
		{
			strcpy(oldnick, nickname);
			fudge_index = strlen(nickname);
		}
	}

	/* 
	 * Process of fudging a nickname:
	 * If the nickname length is less then 9, add an underscore.
	 */
	if (strlen(nickname) < 9)
		strcat(nickname, "_");

	/* 
	 * The nickname is 9 characters long. roll the nickname
	 */
	else
	{
		char tmp = nickname[8];
	
		for (i = 8; i > 0; i--)
			nickname[i] = nickname[i-1];
		nickname[0] = tmp;
	}

	strcpy(blah, ltoa(from_server));
	strcpy(oldnick, nickname);
	nickname_sendline(blah, nickname);
}
