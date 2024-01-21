/*
 * notice.c: special stuff for parsing NOTICEs
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1991
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: notice.c,v 1.26.2.2 1996/07/20 19:24:10 mrg Exp $";
#endif

#include "irc.h"

#include "whois.h"
#include "ctcp.h"
#include "window.h"
#include "lastlog.h"
#include "flood.h"
#include "vars.h"
#include "ircaux.h"
#include "hook.h"
#include "ignore.h"
#include "server.h"
#include "funny.h"
#include "output.h"
#include "names.h"
#include "parse.h"
#include "notify.h"

extern	char	*FromUserHost;

static	void	parse_note _((char *, char *));
static	void	parse_server_notice _((char *, char *));

/*
 * parse_note: handles the parsing of irc note messages which are sent as
 * NOTICES.  The notice() function determines which notices are note messages
 * and send that info to parse_note() 
 */
static	void
parse_note(server, line)
	char	*server;
	char	*line;
{
	char	*date,
		*nick,
		*flags,
		*high,
		*name,
		*message;
	int	ign1,
		ign2,
		level;
	time_t	time;

	flags = next_arg(line, &date);	/* what to do with these flags */
	nick = next_arg(date, &date);
	name = next_arg(date, &date);
	if ((message = index(date, '*')) != NULL)
		*message = (char) 0;
	if (((ign1 = is_ignored(nick, IGNORE_NOTES)) == IGNORED) ||
			((ign2 = is_ignored(name, IGNORE_NOTES)) == IGNORED))
		return;
	if ((ign1 == HIGHLIGHTED) || (ign2 == HIGHLIGHTED))
		high = &highlight_char;
	else
		high = empty_string;
	time = atol(date);
	date = ctime(&time);
	date[24] = (char) 0;
	level = set_lastlog_msg_level(LOG_NOTES);
	if (do_hook(NOTE_LIST, "%s %s %s %s %s %s", nick, name, flags, date,
		server, message + 2))
	{
		put_it("Note from %s (%s) %s", nick, name, flags);
		put_it("It was queued %s from server %s", date, server);
		put_it("%s[%s]%s %s", high, nick, high, message + 2);
	}
	if (beep_on_level & LOG_NOTES)
		beep_em(1);
	set_lastlog_msg_level(level);
}

static	void
parse_server_notice(from, line)
	char	*from,
		*line;
{
	char	server[81],
		version[21];
	int	user_cnt,
		server_cnt,
		lastlog_level;
	int	flag;

	if (!from || !*from)
		from = server_list[parsing_server_index].itsname ?
			server_list[parsing_server_index].itsname :
			server_list[parsing_server_index].name;
	if (get_int_var(SUPPRESS_SERVER_MOTD_VAR) &&
		get_server_motd(parsing_server_index))
	{
		if (strncmp("*** Message-of-today", line, 20) == 0)
		{
			set_server_motd(parsing_server_index, 0);
			return;
		}
		if (strncmp("MOTD ", line, 5) == 0)
		{
			set_server_motd(parsing_server_index, 1);
			return;
		}
		if (strcmp("* End of /MOTD command.", line) == 0)
		{
			set_server_motd(parsing_server_index, 0);
			return;
		}
	}
	if (!strncmp(line, "*** Notice --", 13))
	{
		message_from((char *) 0, LOG_OPNOTE);
		lastlog_level = set_lastlog_msg_level(LOG_OPNOTE);
	}
	else
	{
		message_from((char *) 0, LOG_SNOTE);
		lastlog_level = set_lastlog_msg_level(LOG_SNOTE);
	}
	if (get_server_version(parsing_server_index) >= Server2_7 && 
	    *line != '*'  && *line != '#' && strncmp(line, "MOTD ", 4))
		flag = 1;
	else
		flag = 0;
	if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s"
					     : "%s %s", from, line))
		put_it(flag ? "*** %s" : "%s", line);
	if ((parsing_server_index == primary_server) &&
			((sscanf(line, "*** There are %d users on %d servers",
			&user_cnt, &server_cnt) == 2) ||
			(sscanf(line, "There are %d users on %d servers",
			&user_cnt, &server_cnt) == 2)))
	{
		if ((server_cnt < get_int_var(MINIMUM_SERVERS_VAR)) ||
				(user_cnt < get_int_var(MINIMUM_USERS_VAR)))
		{
			say("Trying better populated server...");
			get_connected(parsing_server_index + 1);
		}
	}
#ifdef BROKEN_SCANF
	else if (!strncmp(line, "*** Your host is ", 17))
#else
	else if ((sscanf(line, "*** Your host is %80s running version %20s",
			server, version) == 2))
#endif /* BROKEN_SCANF */
	{
		if (get_server_version(parsing_server_index) < Server2_8)
			got_initial_version(line);
	}
	if (lastlog_level)
	{
		set_lastlog_msg_level(lastlog_level);
		message_from((char *) 0, lastlog_level);
		/*message_from((char *) 0, LOG_CURRENT);*/	/* XXX should use this instead */
	}
}

void 
parse_notice(from, Args)
	char 	*from;
	char 	**Args;
{
	int	level,
		type;
	char	*to;
	int	no_flooding;
	int	flag;
	char	*high,
		not_from_server = 1;
	char	*line;

	PasteArgs(Args, 1);
	to = Args[0];
	line = Args[1];
	if (!to || !line)
		return;
	if (*to)
	{
		if (is_channel(to))
		{
			message_from(to, LOG_NOTICE);
			type = PUBLIC_NOTICE_LIST;
		}
		else
		{
			message_from(from, LOG_NOTICE);
			type = NOTICE_LIST;
		}
		if (from && *from && strcmp(get_server_itsname(parsing_server_index), from))
		{
			flag = double_ignore(from, FromUserHost,IGNORE_NOTICES);
			if (flag != IGNORED)
			{
				if (flag == HIGHLIGHTED)
					high = &highlight_char;
				else
					high = empty_string;
				if (index(from, '.'))
				{
		/*
		 * only dots in servernames, right ?
		 *
		 * But a server name doesn't nessicarily have to have
		 * a 'dot' in it..  - phone, jan 1993.
		 */
					not_from_server = 0;
					if (strncmp(line, "*/", 2) == 0)
					{
						parse_note(from, line + 1);
						message_from((char *) 0, LOG_CURRENT);
						return;
					}
				}
				if (not_from_server && (flag != DONT_IGNORE) && !FromUserHost &&
							(ignore_usernames & IGNORE_NOTICES))
					add_to_whois_queue(from, whois_ignore_notices, "%s", line);
				else
				{
					line = do_notice_ctcp(from, to, line);
					if (*line == '\0')
					{
						message_from((char *) 0, LOG_CURRENT);
						return;
					}
					level = set_lastlog_msg_level(LOG_NOTICE);
					no_flooding = check_flooding(from, NOTICE_FLOOD, line);

					if (sed == 1 && !do_hook(ENCRYPTED_NOTICE_LIST, "%s %s %s", from, to, line))
						sed = 0;
					else
					{
						if (type == NOTICE_LIST && no_flooding)
						{
							if (do_hook(type, "%s %s", from, line))
								put_it("%s-%s-%s %s", high, from, high, line);
						}
						else
						{
							if (do_hook(type, "%s %s %s", from, to, line))
								put_it("%s-%s:%s-%s %s", high, from, to, high, line);
						}
						if (beep_on_level & LOG_NOTICE)
							beep_em(1);
						set_lastlog_msg_level(level);
						if (not_from_server)
							notify_mark(from, 1, 0);
					}
				}
			}
		}
		else 
			parse_server_notice(from, line);
	}
	else
		put_it("%s", line + 1);
	message_from((char *) 0, LOG_CURRENT);
}

/*
 * load the initial .ircrc
 */
void
load_ircrc()
{
	static	int done = 0;

	if (done++)
		return;
	if (access(ircrc_file, R_OK) == 0)
	{
		char buffer[BIG_BUFFER_SIZE+1];

		strmcpy(buffer,ircrc_file,BIG_BUFFER_SIZE);
		strmcat(buffer," ",BIG_BUFFER_SIZE);
		strmcat(buffer,args_str,BIG_BUFFER_SIZE);
		load(empty_string, buffer, empty_string);
	}
	else if (get_int_var(NOVICE_VAR))
		say("If you have not already done so, please read the new user information with /HELP NEWUSER");
}

/*
 * got_initial_version: this is called when ircii get the NOTICE in
 * all version of ircd before 2.8, or the 002 numeric for 2.8 and
 * beyond.  I guess its handled rather badly at the moment....
 * added by phone, late 1992.
 */
void
got_initial_version(line)
	char	*line;
{
	char	server[81],
		version[21];
	char	*c;

	/*
	 * BROKEN_SCANF crap here provided by Aiken <adrum@u.washington.edu>
	 * sometime 1993...
	 */

#ifdef BROKEN_SCANF
	if (strncmp(line, "*** Your host is ", 17))
		return;
	strncpy(server, &line[17], 80);

	server[79] = 0;
	if(c = index(server, ','))
		*c = 0;
	if(c = index(server, ' '))
		*c = 0;
	version[0] = 0;

	if(c = index(&line[17], ' ')) 
	{
		if(!strncmp(c, " running version ", 17))
		{
			strncpy(version, &c[17], 20);
			version[19] = 0;
		}
                else return;
	}
	else return;
#else
	if ((sscanf(line, "*** Your host is %80s running version %20s",
			server, version)) != 2)
		return;
	server[strlen(server) - 1] = '\0';
#endif /* BROKEN_SCANF */
	attempting_to_connect--;
	set_server_motd(parsing_server_index, 1);
	server_is_connected(parsing_server_index, 1);
	if ((c = (char *) index(server, '[')) != NULL)
		*c = '\0';	/*
				 * Handles the case where the server name is
				 * different to the host name.
				 */
	if (!strncmp(version, "2.5", 3))
		set_server_version(parsing_server_index, Server2_5);
	else if (!strncmp(version, "2.6", 3))
		set_server_version(parsing_server_index, Server2_6);
	else if (!strncmp(version, "2.7", 3))
		set_server_version(parsing_server_index, Server2_7);
	else if (!strncmp(version, "2.8", 3))
		set_server_version(parsing_server_index, Server2_8);
	else if (!strncmp(version, "2.9", 3))
		set_server_version(parsing_server_index, Server2_9);
	else if (!strncmp(version, "2.10", 4))
		set_server_version(parsing_server_index, Server2_10);
	else
		set_server_version(parsing_server_index, Server2_11);
	malloc_strcpy(&server_list[parsing_server_index].version_string, version);
	set_server_itsname(parsing_server_index, server);
	reconnect_all_channels(parsing_server_index);
	reinstate_user_modes(/* parsing_server_index */); /* XXX */
	if (never_connected)
	{
		never_connected = 0;
		loading_global = 1;
		load(empty_string, "global", empty_string);
		loading_global = 0;
		/* read the .ircrc file */
		if (!qflag)
			load_ircrc();
	}
	else if (server_list[parsing_server_index].away)
		send_to_server("AWAY :%s",
			server_list[parsing_server_index].away);
	update_all_status();
	do_hook(CONNECT_LIST, "%s %d", get_server_name(parsing_server_index),
		get_server_port(parsing_server_index));
}
