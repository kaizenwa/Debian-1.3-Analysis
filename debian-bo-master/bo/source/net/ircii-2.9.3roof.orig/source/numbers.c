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

#ifndef lint
static	char	rcsid[] = "@(#)$Id: numbers.c,v 1.26.2.4 1996/07/20 19:14:45 mrg Exp $";
#endif

#include "irc.h"

#include "input.h"
#include "edit.h"
#include "ircaux.h"
#include "vars.h"
#include "lastlog.h"
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

static	void	reset_nickname _((void));
static	void	nickname_in_use _((char *, char **));
static	void	password_sendline _((char *, char *));
static	void	get_password _((void));
static	void	nickname_sendline _((char *, char *));
static	void	channel_topic _((char *, char **));
static	void	not_valid_channel _((char *, char **));
static	void	cannot_join_channel _((char *, char **));
static	void	version _((char *, char **));
static	void	invite _((char *, char **));

static	int	already_doing_reset_nickname = 0;

/*
 * numeric_banner: This returns in a static string of either "xxx" where
 * xxx is the current numeric, or "***" if SHOW_NUMBERS is OFF 
 */
char	*
numeric_banner()
{
	static	char	thing[4];

	if (get_int_var(SHOW_NUMERICS_VAR))
		sprintf(thing, "%3.3u", -current_numeric);
	else
		strcpy(thing, "***");
	return (thing);
}


/*
 * display_msg: handles the displaying of messages from the variety of
 * possible formats that the irc server spits out.  you'd think someone would
 * simplify this 
 */
void
display_msg(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*ptr;
	char	*rest;

	rest = PasteArgs(ArgList, 0);
	if (from && (my_strnicmp(get_server_itsname(parsing_server_index), from,
			strlen(get_server_itsname(parsing_server_index))) == 0))
		from = (char *) 0;
	if ((ptr = (char *) index(rest, ':')) != NULL)
	{
		*(ptr++) = (char) 0;
		if (strlen(rest))
		{
			if (from)
				put_it("%s %s: %s (from %s)", numeric_banner(),
					rest, ptr, from);
			else
				put_it("%s %s: %s", numeric_banner(), rest,
					ptr);
		}
		else
		{
			if (from)
				put_it("%s %s (from %s)", numeric_banner(),
					ptr, from);
			else
				put_it("%s %s", numeric_banner(), ptr);
		}
	}
	else
	{
		if (from)
			put_it("%s %s (from %s)", numeric_banner(), rest, from);
		else
			put_it("%s %s", numeric_banner(), rest);
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
	connect_to_server(get_server_name(new_server),
		get_server_port(new_server), -1);
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
		get_server_name(parsing_server_index));
	close_server(parsing_server_index, empty_string);
        if (!dumb)
	{
		sprintf(server_num, "%d", parsing_server_index);
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
		server = parsing_server_index;
		from_server = new_server;
		send_to_server("NICK %s", nick);
		if (new_server == primary_server)
			strmcpy(nickname, nick, NICKNAME_LEN);
		set_server_nickname(new_server, nick);
		from_server = server;
		already_doing_reset_nickname = 0;
		update_all_status();
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
		sprintf(server_num, "%d", parsing_server_index);
		add_wait_prompt("Nickname: ", nickname_sendline, server_num,
			WAIT_PROMPT_LINE);
	}
	update_all_status();
}

/*ARGSUSED*/
static	void
channel_topic(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*topic, *channel;

	if (ArgList[1] && is_channel(ArgList[0]))
	{
		topic = ArgList[1];
		channel = ArgList[0];
		message_from(channel, LOG_CRAP);
		put_it("%s Topic for %s: %s", numeric_banner(), channel,
			topic);
		message_from((char *) 0, LOG_CURRENT);
	}
	else
	{
		message_from((char *) 0, LOG_CURRENT);	/* XXX should remove this */
		PasteArgs(ArgList, 0);
		put_it("%s Topic: %s", numeric_banner(), ArgList[0]);
	}
}

static	void
nickname_in_use(from, ArgList)
	char	*from,
		**ArgList;
{
	PasteArgs(ArgList, 0);
	if (is_server_connected(parsing_server_index))
		if (do_hook(current_numeric, "%s", *ArgList))
			display_msg(from, ArgList);
	else if (never_connected || parsing_server_index != primary_server ||
	    !attempting_to_connect)
	{
		if (do_hook(current_numeric, "%s", *ArgList))
			display_msg(from, ArgList);
		reset_nickname();
	}
	else
	{
		send_to_server("USER %s %s . :%s", username,
			(send_umode && *send_umode) ? send_umode : ".",
			realname);
		send_to_server("NICK %s", get_server_nickname(parsing_server_index));
	}
}

static	void
not_valid_channel(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*channel;
	char	*s;

	if (!(channel = ArgList[0]) || !ArgList[1])
		return;
	PasteArgs(ArgList, 1);
	s = get_server_name(parsing_server_index);
	if (0 == my_strnicmp(s, from, strlen(s)))
	{
		remove_channel(channel, parsing_server_index);
		put_it("%s %s %s", numeric_banner(), channel, ArgList[1]);
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
	char	*chan;

	if (ArgList[0])
		chan = ArgList[0];
	else
		chan = get_chan_from_join_list(parsing_server_index);

	remove_from_join_list(chan, parsing_server_index);
	if (!is_on_channel(chan, parsing_server_index,
			get_server_nickname(parsing_server_index)))
		remove_channel(chan, parsing_server_index);
	else
		return;

	PasteArgs(ArgList, 0);
	strcpy(buffer, ArgList[0]);
	switch(-current_numeric)
	{
	case 471:
		strcat(buffer, " (Channel is full)");
		break;
	case 473:
		strcat(buffer, " (Invite only channel)");
		break;
	case 474:
		strcat(buffer, " (Banned from channel)");
		break;
	case 475:
		strcat(buffer, " (Bad channel key)");
		break;
	case 476:
		strcat(buffer, " (Bad channel mask)");
		break;
	}
	put_it("%s %s", numeric_banner(), buffer);
}


/*ARGSUSED*/
static	void
version(from, ArgList)
	char	*from,
		**ArgList;
{
	if (ArgList[2])
	{
		PasteArgs(ArgList, 2);
		put_it("%s Server %s: %s %s", numeric_banner(), ArgList[1],
			ArgList[0], ArgList[2]);
	}
	else
	{
		PasteArgs(ArgList, 1);
		put_it("%s Server %s: %s", numeric_banner(), ArgList[1],
			ArgList[0]);
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

	if ((who = ArgList[0]) && (channel = ArgList[1]))
	{
		message_from(channel, LOG_CRAP);
		if (do_hook(current_numeric, "%s %s %s", from, who, channel))
			put_it("%s Inviting %s to channel %s",
					numeric_banner(), who, channel);
		message_from((char *) 0, LOG_CURRENT);
	}
}


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
	char	blah[BIG_BUFFER_SIZE];
	int	flag,
		lastlog_level;
#if 0
	int	user_cnt,
		inv_cnt,
		server_cnt;
#endif

	if (!from || !*from)
		return;
	if (!*ArgList[0])
		user = (char *) 0;
	else
		user = ArgList[0];
	if (!ArgList[1])
		return;
	lastlog_level = set_lastlog_msg_level(LOG_CRAP);
	message_from((char *) 0, LOG_CRAP);
	ArgList++;
	current_numeric = -comm;	/* must be negative of numeric! */
	switch (comm)
	{
	case 001:	/* #define RPL_WELCOME          001 */
		PasteArgs(ArgList, 0);
		if (do_hook(current_numeric, "%s %s", from, *ArgList)) 
			display_msg(from, ArgList);
		break;
	case 002:	/* #define RPL_YOURHOST         002 */
		PasteArgs(ArgList, 0);
		sprintf(blah, "*** %s", ArgList[0]);
		got_initial_version(blah);
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		break;

/* should do something with this some day, 2.8 had channel/user mode switches */
	case 004:	/* #define RPL_MYINFO           004 */
		PasteArgs(ArgList, 0);
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		break;

/*
 * this part of ircii has been broken for most of ircd 2.7, so someday I'll
 * make it work for ircd 2.8 ...  phone..
 */
#if 0
	case 251:		/* #define RPL_LUSERCLIENT      251 */
		display_msg(from, ArgList);
		if (server_list[from_server].connected)
			break;
		if ((from_server == primary_server) && ((sscanf(ArgList[1],
		    "There are %d users and %d invisible on %d servers",
		    &user_cnt, &inv_cnt, &server_cnt) == 3)||(sscanf(ArgList[1],
		    "There are %d users and %d invisible on %d servers",
		    &user_cnt, &inv_cnt, &server_cnt) == 3)))
		{
			user_cnt =+ inv_cnt;
			if ((server_cnt < get_int_var(MINIMUM_SERVERS_VAR)) ||
			    (user_cnt < get_int_var(MINIMUM_USERS_VAR)))
			{
				say("Trying better populated server...");
				get_connected(from_server + 1);
			}
		}
#endif
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
		whois_chop(from, ArgList);
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
		ArgList[3] = (char *) 0;
		funny_list(from, ArgList);
		break;

	case 322:		/* #define RPL_LIST             322 */
		funny_list(from, ArgList);
		break;

	case 324:		/* #define RPL_CHANNELMODEIS    324 */
		funny_mode(from, ArgList);
		break;

	case 341:		/* #define RPL_INVITING         341 */
		invite(from, ArgList);
		break;

	case 352:		/* #define RPL_WHOREPLY         352 */
		whoreply((char *) 0, ArgList);
		break;

	case 353:		/* #define RPL_NAMREPLY         353 */
		funny_namreply(from, ArgList);
		break;

	case 366:		/* #define RPL_ENDOFNAMES       366 */
		{
			int	flag;
			char	*tmp = (char *) 0,
				*chan;

			PasteArgs(ArgList, 0);
			malloc_strcpy(&tmp, ArgList[0]);
			chan = strtok(tmp," ");
			flag = do_hook(current_numeric, "%s %s", from, ArgList[0]);
			
			if (!in_join_list(chan, from_server))
				if (get_int_var(SHOW_END_OF_MSGS_VAR) && flag)
					display_msg(from, ArgList);
			else
				got_info(chan, from_server, GOTNAMES);

			new_free(&tmp);
		}
		break;

	case 381: 		/* #define RPL_YOUREOPER        381 */
		PasteArgs(ArgList, 0);
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		set_server_operator(parsing_server_index, 1);
#if 0
		/* 
		 * this should be set in parse.c:mode().  why was this ever done
		 * in here at all?  notably, it fails on irc 2.9 servers because
		 * they don't have a +s and operators don't become +w automatically
		 * anyway.  this is truly lame...
		 */
		set_server_flag(parsing_server_index, USER_MODE_S, 1);
		set_server_flag(parsing_server_index, USER_MODE_W, 1);
#endif
		update_all_status();	/* fix the status line */
		break;

	case 401:		/* #define ERR_NOSUCHNICK       401 */
		no_such_nickname(from, ArgList);
		break;

	case 421:		/* #define ERR_UNKNOWNCOMMAND   421 */
		if (check_screen_redirect(ArgList[0]))
			break;
		if (check_wait_command(ArgList[0]))
			break;
		PasteArgs(ArgList, 0);
		flag = do_hook(current_numeric, "%s %s", from, *ArgList);
		if (!strncmp("ISON", *ArgList, 4) || !strncmp("USERHOST",
		    *ArgList, 8))
		{
			set_server_2_6_2(parsing_server_index, 0);
			convert_to_whois();
		}
		else if (flag)
			display_msg(from, ArgList);
		break;

	case 432:		/* #define ERR_ERRONEUSNICKNAME 432 */
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		reset_nickname();
		break;

	case 433:		/* #define ERR_NICKNAMEINUSE    433 */ 
		nickname_in_use(from, ArgList);
		reset_nickname();
		break;

	case 463:		/* #define ERR_NOPERMFORHOST    463 */
		display_msg(from, ArgList);
		server_list[parsing_server_index].copy_from = -1;
		close_server(parsing_server_index, empty_string);
		save_chan_from = parsing_server_index;
		window_check_servers();
		if (!connected_to_server)
			get_connected(parsing_server_index + 1);
		else
			save_chan_from = -1;
		break;

	case 464:		/* #define ERR_PASSWDMISMATCH   464 */
		PasteArgs(ArgList, 0);
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
			int	klined_server = parsing_server_index;

			PasteArgs(ArgList, 0);
			if (do_hook(current_numeric, "%s %s", from, ArgList[0]))
				display_msg(from, ArgList);
			server_list[parsing_server_index].copy_from = -1;
			close_server(parsing_server_index, empty_string);
			save_chan_from = parsing_server_index;
			window_check_servers();
			if (number_of_servers > 1)
			{
				remove_from_server_list(klined_server);
				save_chan_from = -1;
			}
			if (!connected_to_server)
				say("You are not connected to a server. Use /SERVER to connect.");
			break;
		}

	case 484:		/* #define ERR_RESTRICTED       484 */
		if (do_hook(current_numeric, "%s %s", from, *ArgList))
			display_msg(from, ArgList);
		set_server_flag(parsing_server_index, USER_MODE_R, 1);
		break;

		/*
		 * The following accumulates the remaining arguments
		 * in ArgSpace for hook detection.  We can't use
		 * PasteArgs here because we still need the arguments
		 * separated for use elsewhere.
		 */
	default:
		{
			char	*ArgSpace = (char *) 0;
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
					message_from((char *) 0, LOG_CURRENT);
				return;
			}
			if (do_message_from)
				message_from((char *) 0, lastlog_level);
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
			put_it("%s Your user mode is \"%s\"", numeric_banner(),
				ArgList[0]);
			break;

		case 242:		/* #define RPL_STATSUPTIME      242 */
			PasteArgs(ArgList, 0);
			if (from && !my_strnicmp(get_server_itsname(parsing_server_index),
			    from, strlen(get_server_itsname(parsing_server_index))))
				from = NULL;
			if (from)
				put_it("%s %s from (%s)", numeric_banner(),
					*ArgList, from);
			else
				put_it("%s %s", numeric_banner(), *ArgList);
			break;

		case 332:		/* #define RPL_TOPIC            332 */
			channel_topic(from, ArgList);
			break;

		case 351:		/* #define RPL_VERSION          351 */
			version(from, ArgList);
			break;

		case 364:		/* #define RPL_LINKS            364 */
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
			break;

		case 372:		/* #define RPL_MOTD             372 */
			if (!get_int_var(SUPPRESS_SERVER_MOTD_VAR) ||
			    !get_server_motd(parsing_server_index))
			{
				PasteArgs(ArgList, 0);
				put_it("%s %s", numeric_banner(), ArgList[0]);
			}
			break;

		case 375:		/* #define RPL_MOTDSTART        375 */
			if (!get_int_var(SUPPRESS_SERVER_MOTD_VAR) ||
			    !get_server_motd(parsing_server_index))
			{
				PasteArgs(ArgList, 0);
				put_it("%s %s", numeric_banner(), ArgList[0]);
			}
			break;

		case 376:		/* #define RPL_ENDOFMOTD        376 */
			if (get_int_var(SHOW_END_OF_MSGS_VAR) &&
			    (!get_int_var(SUPPRESS_SERVER_MOTD_VAR) ||
			    !get_server_motd(parsing_server_index)))
			{
				PasteArgs(ArgList, 0);
				put_it("%s %s", numeric_banner(), ArgList[0]);
			}
			set_server_motd(parsing_server_index, 0);
			break;

		case 384:		/* #define RPL_MYPORTIS         384 */
			PasteArgs(ArgList, 0);
			put_it("%s %s %s", numeric_banner(), ArgList[0], user);
			break;

		case 385:		/* #define RPL_NOTOPERANYMORE   385 */
			set_server_operator(parsing_server_index, 0);
			display_msg(from, ArgList);
			update_all_status();
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
			send_to_server("USER %s %s . :%s", username,
				(send_umode && *send_umode) ? send_umode : ".",
				realname);
			send_to_server("NICK %s",
				get_server_nickname(parsing_server_index));
			break;

		case 462:		/* #define ERR_ALREADYREGISTRED 462 */
			send_to_server("NICK %s",
				get_server_nickname(parsing_server_index));
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
			if (cannot_open != (char *) 0)
				yell("Cannot open: %s", cannot_open);

		case 323:               /* #define RPL_LISTEND          323 */
			funny_print_widelist();

		case 219:		/* #define RPL_ENDOFSTATS       219 */
		case 232:		/* #define RPL_ENDOFSERVICES    232 */
		case 365:		/* #define RPL_ENDOFLINKS       365 */
		case 368:		/* #define RPL_ENDOFBANLIST     368 */
		case 369:		/* #define RPL_ENDOFWHOWAS      369 */
		case 374:		/* #define RPL_ENDOFINFO        374 */
#if 0	/* this case needs special handing - see above */
		case 376:		/* #define RPL_ENDOFMOTD        376 */
#endif
		case 394:		/* #define RPL_ENDOFUSERS       394 */
			if (!get_int_var(SHOW_END_OF_MSGS_VAR))
				break;
		default:
			display_msg(from, ArgList);
		}
	}
	set_lastlog_msg_level(lastlog_level);
	message_from((char *) 0, LOG_CURRENT);
}
