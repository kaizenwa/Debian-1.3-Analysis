/*
 * parse.c: handles messages from the server.   Believe it or not.  I
 * certainly wouldn't if I were you. 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#include "irc.h"

#include "alias.h"
#include "server.h"
#include "names.h"
#include "vars.h"
#include "ctcp.h"
#include "hook.h"
#include "edit.h"
#include "ignore.h"
#include "whois.h"
#include "lastlog.h"
#include "input.h"
#include "ircaux.h"
#include "funny.h"
#include "crypt.h"
#include "input.h"
#include "ircterm.h"
#include "flood.h"
#include "window.h"
#include "screen.h"
#include "output.h"
#include "numbers.h"
#include "parse.h"
#include "notify.h"
#include "status.h"
#include "list.h"
#include "userlist.h"
#include "bot.h"
#include "misc.h"
#include "whowas.h"
#include "tcl_bx.h"

#define STRING_CHANNEL '+'
#define MULTI_CHANNEL '#'
#define LOCAL_CHANNEL '&'

#define	MAXPARA	15	/* Taken from the ircd */
#define space ' '
extern	char	*forwardnick;
static	void	strip_modes _((char *, char *, char *));

extern char * do_nslookup _((char *));
extern void do_newuser _((char *, char *, char *));
extern char * random_str _((int, int));
extern char *msgcdcc _((char *, char *, char *));

extern char *channel_key _((char *));
extern char *last_topic;
extern char *auto_str;

	char *last_split_server = NULL;
	char *last_split_from = NULL;
extern 	char *last_wall;
	int in_server_ping = 0;

extern	int grab_http _((char *, char *, char *));

/*
 * joined_nick: the nickname of the last person who joined the current
 * channel 
 */
	char	*joined_nick = NULL;

/* public_nick: nick of the last person to send a message to your channel */
	char	*public_nick = NULL;

/* User and host information from server 2.7 */
	char	*FromUserHost = empty_string;

/* doing a PRIVMSG */
	int	doing_privmsg = 0;
	int	chan_who = 0;
	
	int	who_on_join = 0;
	
extern	int	sed;
extern	int	in_e_nick;



/* returns 1 if the ban is on the channel already, 0 if not */
BanList *ban_is_on_channel(char *ban, ChannelList *chan)
{
register BanList *bans;
	context;
	for (bans = chan->bans; bans; bans = bans->next) 
	{
		if (match(bans->ban, ban) || match(ban, bans->ban))
			return bans;
		else if (!my_stricmp(bans->ban, ban))
			return bans;
	}
	return NULL;
}


void fake _((void)) 
{
	bitchsay("--- Fake Message recieved!!! ---");
	return;
}

int check_auto_reply(char *str)
{
char *p = NULL;
char *q = NULL;
char *pat;
	if (!str || !*str)
		return 0;
	if (auto_str && *auto_str && get_int_var(NICK_COMPLETION_VAR))
	{
		q = p = m_strdup(auto_str);
		while ((pat = next_arg(p, &p)))
		{
			switch(get_int_var(NICK_COMPLETION_TYPE_VAR))
			{
				case 2:
					if (wild_match(pat, str))
						goto found;
					continue;
				case 1:
					if (stristr(str, pat))
						goto found;
					continue;
				default:
				case 0:
					if (!my_strnicmp(str, pat, strlen(pat)))
						goto found;
					continue;
			}
		}
	}
	new_free(&q);
	return 0;
found:
	new_free(&q);
	return 1;
}

int annoy_kicks(int list_type, char *to, char *from, char *ptr, NickList *nick)
{
int kick_em = 0;

if (get_int_var(ANNOY_KICK_VAR))
{	
	if (nick && ((nick->userlist && nick->userlist->level > 49) || nick->botlist))
		return 0;
	if ((strchr(ptr, '\002') || strchr(ptr, '\007')
		|| strchr(ptr, '\026') || strchr(ptr, '\037')
		|| caps_fucknut(ptr)) && are_you_opped(to)
		&& !nick->chanop && check_channel_match(get_string_var(PROTECT_CHANNELS_VAR), to))
	{
		if (strchr(ptr, '\002'))
			send_to_server("KICK %s %s :%s",
    			to, from, "autokick for \002bold\002");
		else if (strchr(ptr, '\007'))
			send_to_server("KICK %s %s :%s",
			to, from, "autokick for beeping\007");
		else if (strchr(ptr, '\037'))
			send_to_server("KICK %s %s :%s",
			to, from, "autokick for \037underline\037");
		else if (strchr(ptr, '\026'))
			send_to_server("KICK %s %s :%s",
			to, from, "autokick for \026inverse\026");
		else if (strstr(ptr, "\e["))
			send_to_server("KICK %s %s :%s",
			to, from, "autokick for \026Ansi Flash\026");
		else {
			send_to_server("KICK %s %s :%s",
			to, from, "autokick for CAPS LOCK");
		}
		kick_em = 1;
	}
	if (strstr(ptr, "0000027fed") && are_you_opped(to) && !nick->chanop && check_channel_match(get_string_var(PROTECT_CHANNELS_VAR), to))
	{
		char *host = NULL, *p;
		malloc_strcpy(&host, FromUserHost);
		p = strchr(host, '@'); *p++ = '\0';
		send_to_server("MODE %s -o+b %s *!*%s",
		    to, from, cluster(FromUserHost));
		send_to_server("KICK %s %s :%s",
		    to, from, "\002Zmodem rocks\002");
		new_free(&host);
		kick_em = 1;
	}
}
	if (ban_words)
	{
		WordKickList *word;
		int ops = get_int_var(KICK_OPS_VAR);
		for (word = ban_words; word; word = word->next)
		{
			if (strstr(ptr, word->string))
			{
				if (match(word->channel, to) && are_you_opped(to))
				{
					if (!ops && (nick->chanop || nick->voice))
						break;
					send_to_server("KICK %s %s :%s %s", to, from, "\002BitchX BWK\002: ", word->string);
					kick_em = 1;
				}
			}
		}
	}
	return kick_em;
}

/*
 * is_channel: determines if the argument is a channel.  If it's a number,
 * begins with MULTI_CHANNEL and has no '*', or STRING_CHANNEL, then its a
 * channel 
 */
int
is_channel(to)
char	*to;
{
	int	version;
	context;
	if (!to || !*to)
		return 0;
	version = get_server_version(from_server);
        return ( (*to == MULTI_CHANNEL)
                 || (*to == LOCAL_CHANNEL)
                || (version == Server2_8 && *to == '0'));
}


char	* PasteArgs(Args, StartPoint)
	char	**Args;
	int	StartPoint;
{
	int	i;

	for (; StartPoint; Args++, StartPoint--)
		if (!*Args)
			return NULL;
	for (i = 0; Args[i] && Args[i+1]; i++)
		Args[i][strlen(Args[i])] = ' ';
	Args[1] = NULL;
	return Args[0];
}

/*
 * BreakArgs: breaks up the line from the server, in to where its from,
 * setting FromUserHost if it should be, and returns all the arguements
 * that are there.   Re-written by phone, dec 1992.
 */
static	void BreakArgs(Input, Sender, OutPut)
	char	*Input;
	char	**Sender;
	char	**OutPut;
{
	int	ArgCount = 0;

	/*
	 * The RFC describes it fully, but in a short form, a line looks like:
	 * [:sender[!user@host]] COMMAND ARGUMENT [[:]ARGUMENT]{0..14}
	 */

	/*
	 * Look to see if the optional :sender is present.
	 */
	if (*Input == ':')
	{
		*Sender = ++Input;
		while (*Input && *Input != space)
			Input++;
		if (*Input == space)
			*Input++ = 0;

		/*
		 * Look to see if the optional !user@host is present.
		 */
		FromUserHost = *Sender;
		while (*FromUserHost && *FromUserHost != '!')
			FromUserHost++;
		if (*FromUserHost == '!')
			*FromUserHost++ = 0;
	}
	/*
	 * No sender present.
	 */
	else
		*Sender = FromUserHost = empty_string;

	/*
	 * Now we go through the argument list...
	 */
	for (;;)
	{
		while (*Input && *Input == space)
			Input++;

		if (!*Input)
			break;

		if (*Input == ':')
		{
			OutPut[ArgCount++] = ++Input;
			break;
		}

		OutPut[ArgCount++] = Input;
		if (ArgCount >= MAXPARA)
			break;

		while (*Input && *Input != space)
			(void) *Input++;
		if (*Input == space)
			*Input++ = 0;
	}
	OutPut[ArgCount] = NULL;
}

/* in response to a TOPIC message from the server */
static	void
p_topic(from, ArgList)
	char	*from,
		**ArgList;
{
ChannelList *tmp;

	context;
	if (!ArgList[1])
		{ fake(); return; }
	tmp = lookup_channel(ArgList[0], from_server, CHAN_NOUNLINK);
	update_stats(TOPICLIST, ArgList[0], from, tmp, 0);			
	if (tmp->topic_lock)
	{
		if (my_stricmp(from, get_server_nickname(from_server)))
		{
			if (tmp->topic)
				send_to_server("TOPIC %s :%s", tmp->channel, tmp->topic);
		} else 
			malloc_sprintf(&tmp->topic, "%s", ArgList[1]);
	} else
		malloc_strcpy(&tmp->topic, ArgList[1]);
	malloc_strcpy(&last_topic, ArgList[1]);
	if (check_ignore(from, FromUserHost, tmp->channel, IGNORE_CRAP) != IGNORED)
	{
		message_from(ArgList[0], LOG_CRAP);
		if (do_hook(TOPIC_LIST, "%s %s %s", from, ArgList[0], ArgList[1]))
			if (ArgList[1] && *ArgList[1])
			{
				if (get_string_var(FORMAT_TOPIC_CHANGE_HEADER_VAR))
					put_it("%s",convert_output_format(get_string_var(FORMAT_TOPIC_CHANGE_HEADER_VAR), "%s %s %s %s", update_clock(GET_TIME), from, ArgList[0], ArgList[1]));
				put_it("%s",convert_output_format(get_string_var(FORMAT_TOPIC_CHANGE_VAR), "%s %s %s %s", update_clock(GET_TIME), from, ArgList[0], ArgList[1]));
			} else
				put_it("%s",convert_output_format(get_string_var(FORMAT_TOPIC_UNSET_VAR), "%s %s %s", update_clock(GET_TIME), from, ArgList[0]));
			
		message_from(NULL, LOG_CURRENT);
	}
	update_all_status(curr_scr_win, NULL, 0);
}

#ifdef __STDC__
static	void p_wallops(char *from, char **ArgList)
#else
static	void
p_wallops(from, ArgList)
	char	*from,
		**ArgList;
#endif
{
	char	*line;
	int	autorep = 0;
	int	from_server = index(from, '.') ? 1 : 0;

	context;
	if (!(line = PasteArgs(ArgList, 0)))
		{ fake(); return; }

	if (from_server || check_flooding(from, WALLOP_FLOOD,line, NULL))
	{
	/* The old server check, don't use the whois stuff for servers */
		int	level;
		char	*high;
                switch (check_ignore(from, FromUserHost, NULL, IGNORE_WALLOPS))
                {
                        case (IGNORED):
                                return;
                        case (HIGHLIGHTED):
                                high = highlight_char;
                                break;
                        default:
                                high = empty_string;
                                break;
                }
		message_from(from, LOG_WALLOP);
		level = set_lastlog_msg_level(LOG_WALLOP);
		autorep = check_auto_reply(line);
		malloc_sprintf(&last_wall, "%s %c %s", from, from_server?'S':'!', line);
		if (do_hook(WALLOP_LIST, "%s %c %s", from, from_server ? 'S': '*',line))
			put_it("%s",convert_output_format(get_string_var(from_server? FORMAT_WALLOP_VAR: (autorep? FORMAT_WALL_AR_VAR:FORMAT_WALL_VAR)), 
			"%s %s %s %s", update_clock(GET_TIME),
			 from, from_server?"!":"*", line));
		if (beep_on_level & LOG_WALLOP)
			beep_em(1);
		set_lastlog_msg_level(level);
		message_from(NULL, LOG_CRAP);
	}
}

/*ARGSUSED*/
#ifdef __STDC__
void whoreply(char *from, char **ArgList)
#else
void whoreply(from, ArgList)
	char	**ArgList,
		*from;
#endif
{
	static	char	format[40];
	static	int	last_width = -1;
	int	ok = 1, voice, opped;
	char	*channel,
		*user,
		*host,
		*server,
		*nick,
		*stat,
		*name;
	ChannelList *chan = NULL;
	FILE	*fip;
	char	buf_data[BIG_BUFFER_SIZE+1];


	context;
	if (!ArgList[5])
		{ fake(); return; }

	if (last_width != get_int_var(CHANNEL_NAME_WIDTH_VAR))
	{
		if ((last_width = get_int_var(CHANNEL_NAME_WIDTH_VAR)) != 0)
		    sprintf(format, "%%-%u.%us \002%%-9s\002 %%-3s %%s@%%s (%%s)",
					(unsigned char) last_width,
					(unsigned char) last_width);
		else
		    strcpy(format, "%s\t\002%-9s\002 %-3s %s@%s (%s)");
	}
	channel = ArgList[0];
	user = ArgList[1];
	host = ArgList[2];
	server = ArgList[3];
	nick = ArgList[4];
	stat = ArgList[5];
	PasteArgs(ArgList, 6);

	message_from(channel, LOG_CRAP);
	
	strcpy(buf_data, user);
	strcat(buf_data, "@");
	strcat(buf_data, host);
	voice = (strchr(stat, '+') != NULL); 
	opped = (strchr(stat, '@') != NULL); 
	
	if (*stat == 'S')	/* this only true for the header WHOREPLY */
	{
		channel = "Channel";
		if (((who_mask & WHO_FILE) == 0) || (fopen (who_file, "r")))
		{
			if (do_hook(WHO_LIST, "%s %s %s %s %s %s %s", channel,
					nick, stat, user, host, server, ArgList[6]))
				put_it("%s",convert_output_format(get_string_var(FORMAT_WHO_VAR), "%s %s %s %s %s %s %s", channel, nick, stat, user, host, server, ArgList[6]));
			message_from(NULL, LOG_CRAP);
			return;
		}
	} 

	name = ArgList[6];
	if (last_split_server && server)
	{
		if (match(server, last_split_server))
		{
put_it("%s", convert_output_format(get_string_var(FORMAT_NETJOIN_VAR), "%s %s %s %d", update_clock(GET_TIME), last_split_from, last_split_server, 0));
			new_free(&last_split_server);
			new_free(&last_split_from);
#ifdef WANT_TCL
			check_tcl_rejoin(nick, FromUserHost, nick, channel);
#endif
		}
	}
	chan = add_to_channel(channel, nick, from_server, opped, voice, buf_data, server, stat);

	if (in_join_list(channel, from_server) || who_on_join)
	{
		message_from(NULL, LOG_CRAP);
		return;
	}
	if (who_mask)
	{
		if (who_mask & WHO_HERE)
			ok = ok && (*stat == 'H');
		if (who_mask & WHO_AWAY)
			ok = ok && (*stat == 'G');
		if (who_mask & WHO_OPS)
			ok = ok && (*(stat + 1) == '*');
		if (who_mask & WHO_LUSERS)
			ok = ok && (*(stat + 1) != '*');
		if (who_mask & WHO_CHOPS)
			ok = ok && ((*(stat + 1) == '@') ||
			(*(stat + 2) == '@'));
		if (who_mask & WHO_NAME)
			ok = ok && wild_match(who_name, user);
		if (who_mask & WHO_NICK)
			ok = ok && wild_match(who_nick, nick);
		if (who_mask & WHO_HOST)
			ok = ok && wild_match(who_host, host);
		if (who_mask & WHO_REAL)
			ok = ok && wild_match(who_real, name);
		if (who_mask & WHO_SERVER)
			ok = ok && wild_match(who_server, server);
		if (who_mask & WHO_FILE)
		{
			ok = 0;
			cannot_open = NULL;
			if ((fip = fopen (who_file, "r")) != NULL)
			{
				while (fgets (buf_data, BUFSIZ, fip) !=	NULL)
				{
					buf_data[strlen(buf_data)-1] = '\0';
					ok = ok || wild_match(buf_data, nick);
				}
				fclose (fip);
			} else
				cannot_open = who_file;
		}
	}
	if (ok)
	{
		if (do_hook(WHO_LIST, "%s %s %s %s %s %s %s", channel, nick,
				stat, user, host, name, server))
		{
			if (get_int_var(SHOW_WHO_HOPCOUNT_VAR))
				put_it("%s",convert_output_format(get_string_var(FORMAT_WHO_VAR), "%s %s %s %s %s %s %s", channel, nick, stat, user, host, server, name));
			else
			{
				char	*tmp;

				if ((tmp = (char *) index(name, ' ')) != NULL)
					tmp++;
				else
					tmp = name;
				put_it("%s",convert_output_format(get_string_var(FORMAT_WHO_VAR), "%s %s %s %s %s %s %s", channel, nick, stat, user, host, server, tmp));
			}
		}
	}
	message_from(NULL, LOG_CRAP);
}

static	void p_privmsg(char *from, char **Args)
{
	int	level,
		list_type,
		flood_type,
		log_type,
		ar_true = 0;

	unsigned char	ignore_type;
	char	*ptr,
		*to;
	char	*high;
	
	int	no_flood = 1, do_beep = 1;
	static int com_do_log, com_lines = 0;
	
	ChannelList *channel = NULL;
	NickList *tmpnick = NULL;
	

	context;
	if (!from)
		return;
	PasteArgs(Args, 1);
	to = Args[0];
	ptr = Args[1];
	if (!to || !ptr)
		{ fake(); return; }
	doing_privmsg = 1;
	
	if (is_channel(to) && im_on_channel(to))
	{
		message_from(to, LOG_MSG);
		malloc_strcpy(&public_nick, from);
		log_type = LOG_PUBLIC;
		ignore_type = IGNORE_PUBLIC;
		flood_type = PUBLIC_FLOOD;
		if (!is_on_channel(to, from_server, from))
			list_type = PUBLIC_MSG_LIST;
		else
		{
			if (is_current_channel(to, from_server, 0))
				list_type = PUBLIC_LIST;
			else
				list_type = PUBLIC_OTHER_LIST;
			channel = lookup_channel(to, from_server, CHAN_NOUNLINK);
			if (channel)
				tmpnick = (NickList *)find_in_list((List **)&channel->nicks, from, 0);
		}
	}
	else
	{
		message_from(from, LOG_MSG);
		flood_type = MSG_FLOOD;
		if (my_stricmp(to, get_server_nickname(from_server)))
		{
			log_type = LOG_WALL;
			ignore_type = IGNORE_WALLS;
			list_type = MSG_GROUP_LIST;
		}
		else
		{
			log_type = LOG_MSG;
			ignore_type = IGNORE_MSGS;
			list_type = MSG_LIST;
		}
	}
	switch (check_ignore(from, FromUserHost, to, ignore_type))
	{
	case IGNORED:
		if ((list_type == MSG_LIST) && get_int_var(SEND_IGNORE_MSG_VAR))
			send_to_server("NOTICE %s :%s is ignoring you", from, get_server_nickname(from_server));
		doing_privmsg = 0;
		return;
	case HIGHLIGHTED:
		high = highlight_char;
		break;
	default:
		high = empty_string;
		break;
	}

	ptr = do_ctcp(from, to, ptr);
	if (!ptr || !*ptr)
	{
		doing_privmsg = 0;
		return;
	} 
#ifdef WANT_TCL
	else 
	{
		int x = 0;
		char *cmd = NULL;
		switch(list_type)
		{
			case MSG_LIST:
			case MSG_GROUP_LIST:
			{
				char *ctcp_ptr = NULL, *free_me;
				malloc_strcpy(&ctcp_ptr, ptr);
				free_me = ctcp_ptr;
				cmd = next_arg(ctcp_ptr, &ctcp_ptr);
				x = check_tcl_msg(cmd, from, FromUserHost, from, ctcp_ptr);
				if (!x)
					check_tcl_msgm(cmd, from, FromUserHost, from, ctcp_ptr);
				new_free(&free_me);
				break;
			}
			case PUBLIC_MSG_LIST:
			case PUBLIC_LIST:
			case PUBLIC_OTHER_LIST:
			{
				x = check_tcl_pub(from, FromUserHost, to, ptr);
				if (!x)
					check_tcl_pubm(from, FromUserHost, to, ptr);
				break;
			}
		}
	}
#endif
	update_stats(PUBLICLIST, to, from, channel, 0);

	level = set_lastlog_msg_level(log_type);
	com_do_log = 0;
	if (flood_type == PUBLIC_FLOOD)
	{
		if (is_other_flood(channel, tmpnick, PUBLIC_FLOOD))
		{
			no_flood = 0;
			flood_prot(tmpnick->nick, FromUserHost, "PUBLIC", flood_type, cget_int_var(PUBFLOOD_IGNORE_TIME_CVAR, channel->channel), channel->channel);
		}
	} else
		no_flood = check_flooding(from, flood_type, ptr, NULL);

	if (sed == 1)
	{
		if (do_hook(ENCRYPTED_PRIVMSG_LIST,"%s %s %s",from, to, ptr))
			put_it("%s",convert_output_format(get_string_var(FORMAT_ENCRYPTED_PRIVMSG_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, to, ptr));
			sed = 0;
	}
	else
	{
		if (list_type == PUBLIC_LIST || list_type == PUBLIC_OTHER_LIST || list_type == PUBLIC_MSG_LIST)
		{
			if (check_auto_reply(ptr))
			{
				addtabkey(from, 1);
				com_do_log = 1;
				com_lines = 0;
				ar_true = 1;
			}
		}

		switch (list_type)
		{
		case PUBLIC_MSG_LIST:
			if (no_flood && do_hook(list_type, "%s %s %s", from, to, ptr))
			    put_it("%s",convert_output_format(get_string_var(ar_true?FORMAT_PUBLIC_MSG_AR_VAR:FORMAT_PUBLIC_MSG_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, to, ptr));
			break;
		case MSG_GROUP_LIST:
			if (no_flood && do_hook(list_type, "%s %s %s", from, to, ptr))
			    put_it("%s",convert_output_format(get_string_var(FORMAT_MSG_GROUP_VAR), "%s %s %s %s", update_clock(GET_TIME), from, to, ptr));
			break;
		case MSG_LIST:
		{
			if (!no_flood)
				break;
			malloc_strcpy(&recv_nick, from);
			if (away_set)
			{
				do_beep = 0;
				beep_em(get_int_var(BEEP_WHEN_AWAY_VAR));
				set_int_var(MSGCOUNT_VAR, get_int_var(MSGCOUNT_VAR)+1);
			}
			if ((msgcdcc(from, to, ptr)) == NULL)
				break;
			addtabkey(from, 0);
			logmsg(0, LOG_MSG, "*", from, ptr, FromUserHost, 0);

			if (forwardnick)
				send_to_server("NOTICE %s :*%s* %s", forwardnick, from, ptr);

			if (do_hook(list_type, "%s %s", from, ptr))
				put_it("%s",convert_output_format(get_string_var(FORMAT_MSG_VAR), "%s %s %s %s", update_clock(GET_TIME), from, FromUserHost, ptr));

			if (from_server > -1 && server_list[from_server].away && get_int_var(SEND_AWAY_MSG_VAR))
			{
				if (!check_last_type(&last_msg[0], from, FromUserHost))
					my_send_to_server(from_server, "NOTICE %s :%s", from, stripansicodes(convert_output_format(get_string_var(FORMAT_SEND_AWAY_VAR), "%d %d %s", time(NULL), server_list[from_server].awaytime, get_int_var(MSGLOG_VAR)?"On":"Off")));
			}
			add_last_type(&last_msg[0], from, FromUserHost, to, ptr);
			break;
		}
		case PUBLIC_LIST:
		{
               		annoy_kicks(list_type, to, from, ptr, tmpnick);
			if (ar_true)
				list_type = AR_PUBLIC_LIST;
			if (no_flood && do_hook(list_type, "%s %s %s", from, to, ptr))
				put_it("%s",convert_output_format(get_string_var((list_type == AR_PUBLIC_LIST)? FORMAT_PUBLIC_AR_VAR:FORMAT_PUBLIC_VAR), "%s %s %s %s", update_clock(GET_TIME), from, to, ptr));
			break;
		}
		case PUBLIC_OTHER_LIST:
		{
                	annoy_kicks(list_type, to, from, ptr, tmpnick);

			if (ar_true)
				list_type = AR_PUBLIC_OTHER_LIST;

			if (no_flood && do_hook(list_type, "%s %s %s", from, to, ptr))
				put_it("%s",convert_output_format(get_string_var(list_type==AR_PUBLIC_OTHER_LIST?FORMAT_PUBLIC_OTHER_AR_VAR:FORMAT_PUBLIC_OTHER_VAR), "%s %s %s %s", update_clock(GET_TIME),from,to,ptr));
			break;
		} /* case */
		} /* switch */
	}
	if (beep_on_level & log_type && do_beep)
		beep_em(1);
	grab_http(from, to, ptr);
	set_lastlog_msg_level(level);
	message_from(NULL, LOG_CURRENT);
	doing_privmsg = 0;
}

/*ARGSUSED*/
static	void
p_quit(from, ArgList)
	char	*from,
		**ArgList;
{
	int	one_prints = 0;
	char	*chan = NULL;
	char	*Reason;
	ChannelList *tmpc;
	int netsplit = 0;
	int ignore;
				

	context;
	PasteArgs(ArgList, 0);
	if (ArgList[0])
	{
		Reason = ArgList[0];
		netsplit = check_split(from, Reason, chan);
				
	}
	else
		Reason = "?";
		
	ignore = check_ignore(from, FromUserHost, NULL, IGNORE_CRAP);
	for (chan = walk_channels(from, 1, from_server); chan; chan = walk_channels(from, 0, -1))
	{
		if ((tmpc = lookup_channel(chan, from_server, CHAN_NOUNLINK)))
		{
			update_stats(CHANNELSIGNOFFLIST, chan, from, tmpc, netsplit);
#ifdef WANT_TCL
			if (netsplit)
				check_tcl_split(from, FromUserHost, from, chan);
			else
				check_tcl_sign(from, FromUserHost, from, chan, Reason);
#endif
		}
		if (ignore != IGNORED)
		{
			message_from(chan, LOG_CRAP);
			if (do_hook(CHANNEL_SIGNOFF_LIST, "%s %s %s", chan, from, Reason))
				one_prints = 1;
		}
	}
	if (one_prints)
	{
		message_from(what_channel(from, from_server), LOG_CRAP);
		if ((ignore != IGNORED) && do_hook(SIGNOFF_LIST, "%s %s", from, Reason))
			if (!netsplit/* && !cget_int_var(HACKING_CVAR,what_channel(from, from_server))*/) 
				put_it("%s",convert_output_format(get_string_var(FORMAT_CHANNEL_SIGNOFF_VAR), "%s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, what_channel(from, from_server), Reason));
	}
	notify_mark(from, 0, 0);
	remove_from_channel(NULL, from, from_server, netsplit, Reason);
	message_from(NULL, LOG_CURRENT);
	update_all_status(curr_scr_win, NULL, 0);
}

/*ARGSUSED*/
static	void p_pong(char *from, char **ArgList)
{
	extern time_t in_sping;
	int i;	

	context;
	if (!ArgList[0])
		return;
	if (in_server_ping && match("*.*", ArgList[0]))
	{
		for (i = 0; i < number_of_servers; i++)
		{
			if ((!my_stricmp(ArgList[0], get_server_name(i)) || !my_stricmp(ArgList[0], get_server_itsname(i))))
			{
				time_t old_lag = server_list[i].lag;
				server_list[i].lag = time(NULL) - server_list[i].lag_time;
				in_server_ping--;
				if (old_lag != server_list[i].lag)
					status_update(1);
				return;
			}
		}
	}
	if (check_ignore(from, FromUserHost, NULL, IGNORE_CRAP) != IGNORED)
	{
		if (index(ArgList[0], '.'))
		{ 
			if (!ArgList[1])
				say("%s: PONG received from %s", ArgList[0], from);
			else if (!my_stricmp(ArgList[1], get_server_nickname(from_server)))
			{
				say("ping time for \002%s\002: (%ld seconds)",from, time(NULL)-in_sping);
				in_sping = 0;
			}
		}
	}
}
		

/*ARGSUSED*/
static	void
p_error(from, ArgList)
	char	*from,
		**ArgList;
{

	context;
	PasteArgs(ArgList, 0);
	if (!ArgList[0])
		{ fake(); return; }
	say("%s", ArgList[0]);
}

static	void p_channel(from, ArgList)
	char	*from;
	char	**ArgList;
{
	char	*channel;
	ChannelList *chan = NULL;
	NickList *tmpnick = NULL;		
				

	context;
	if (!strcmp(ArgList[0], "0"))
		{ fake(); return; }

	channel = ArgList[0];
	message_from(channel, LOG_CRAP);
	malloc_strcpy(&joined_nick, from);
	notify_mark(from, 1, 0);

	if (!my_stricmp(from, get_server_nickname(from_server)))
	{
		chan = add_channel(channel, from_server);
		if (!in_join_list(channel, from_server))
			add_to_join_list(channel, from_server, 0);
		send_to_server("MODE %s\r\nWHO %s\r\nMODE %s b", channel, channel, channel);
		who_on_join++;
		(void)do_hook(JOIN_ME_LIST, "%s", channel);
	}
	else 
	{
		int op = 0, vo = 0;
		char *c;
                if (get_int_var(AUTO_WHO_ON_JOIN_VAR))
		{
			who_on_join++;
			send_to_server("WHO %s", from);
		}
                /*
		 * Workaround for gratuitous protocol change in ef2.9
		 */
		if ((c = index(channel, '\007')))
		{
			for (*c++ = 0; *c; c++)
			{
				     if (*c == 'o') op = 1;
				else if (*c == 'v') vo = 1;
			}
		}

		chan = add_to_channel(channel, from, from_server, op, vo, FromUserHost, NULL, NULL);
#ifdef WANT_TCL
		check_tcl_join(from, FromUserHost, from, channel);
#endif
	}
			
	if (!in_join_list(channel, from_server))
		tmpnick = check_auto(channel, from, FromUserHost, chan);
	flush_mode_all(chan);

	if (check_ignore(from, FromUserHost, channel, IGNORE_CRAP) != IGNORED && chan)
	{
		char * tmp, *tmp2 = NULL;
		int t = 0;
		UserList *tmpuser = NULL;
		tmp = strchr(FromUserHost, '@');
		tmp++;
		if (get_int_var(AUTO_NSLOOKUP_VAR) && *tmp && isdigit(*(tmp + strlen(tmp) - 1))) 
			tmp2 = do_nslookup(tmp);
		message_from(channel, LOG_CRAP);
		if (do_hook(JOIN_LIST, "%s %s %s", from, channel, tmp2 ? tmp2 : FromUserHost? FromUserHost : "UnKnown"))
		{
			if (chan)
				tmpnick = (NickList *)find_in_list((List **)&chan->nicks, from, 0);

			if (tmpnick && tmpnick->userlist && check_channel_match(tmpnick->userlist->channels, channel))
				put_it("%s",convert_output_format(get_string_var(FORMAT_FRIEND_JOIN_VAR), "%s %s %s %s",update_clock(GET_TIME),from,tmp2?tmp2:FromUserHost?FromUserHost:"UnKnown",channel));
			else if (tmpnick && tmpnick->botlist && check_channel_match(tmpnick->botlist->channels, channel))
				put_it("%s",convert_output_format(get_string_var(FORMAT_FRIEND_JOIN_VAR), "%s %s %s %s",update_clock(GET_TIME),from,tmp2?tmp2:FromUserHost?FromUserHost:"UnKnown",channel));
			else 
				put_it("%s",convert_output_format(get_string_var(FORMAT_JOIN_VAR), "%s %s %s %s",update_clock(GET_TIME),from,tmp2?tmp2:FromUserHost?FromUserHost:"UnKnown",channel));

			if ((t = is_other_flood(chan, tmpnick, JOIN_FLOOD)))
			{
				if (chan->chop && !tmpuser)
				{
					send_to_server("MODE %s -o+b %s *!*%s", chan->channel, from, FromUserHost);
					send_to_server("KICK %s %s :\002Join Flood\002 (%d joins in %dsecs of %dsecs", chan->channel, from, chan->set_kick_on_joinflood, t, chan->set_kickflood_time);
				}
			}
			if (chan->chop && chan->set_lamelist)
			{
				if (lame_list && find_in_list((List **)&lame_list, from, 0))
				{
					send_to_server("MODE %s -o+b %s %s!*", chan->channel, from, from);
					send_to_server("KICK %s %s :\002Lame Nick detected\002", chan->channel, from);
				}
			}
		}
		message_from(NULL, LOG_CURRENT);
	}
	set_input_prompt(curr_scr_win, get_string_var(INPUT_PROMPT_VAR), 0);
	update_all_status(curr_scr_win, NULL, 0);
}

static	void
p_invite(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*high;
	UserList *p = NULL;
extern char *last_invite_channel;	

	context;
	switch (check_ignore(from, FromUserHost, ArgList[1] ? ArgList[1] : NULL, IGNORE_INVITES))
	{
		case IGNORED:
			if (get_int_var(SEND_IGNORE_MSG_VAR))
				send_to_server("NOTICE %s :%s is ignoring you",
					from, get_server_nickname(from_server));
			return;
		case HIGHLIGHTED:
			high = highlight_char;
			break;
		default:
			high = empty_string;
			break;
	}
	if (ArgList[0] && ArgList[1])
	{
		message_from(from, LOG_CRAP);
		malloc_strcpy(&invite_channel, ArgList[1]);
		if (check_flooding(from, INVITE_FLOOD, ArgList[1], NULL) && 
		   do_hook(INVITE_LIST, "%s %s", from, ArgList[1]))
		{
			put_it("%s",convert_output_format(get_string_var(FORMAT_INVITE_VAR), "%s %s %s",update_clock(GET_TIME), from, ArgList[1]));
			bitchsay("Press Ctrl-K to join %s", invite_channel);
		}
		if (cget_int_var(AUTO_REJOIN_VAR, invite_channel) && invite_channel && ((p = lookup_userlevelc(from, FromUserHost, invite_channel, Bot_list)) != NULL))
		{
			if (!in_join_list(invite_channel, from_server))
				send_to_server("JOIN %s %s", invite_channel, ArgList[2]? ArgList[2]: "");
		}
		malloc_strcpy(&recv_nick, from);
		malloc_sprintf(&last_invite_channel, "%s %s", from, ArgList[1]);
	}
}

#ifdef __STDC__
static void p_silence (char *from, char **ArgList)
#else
static	void	p_silence (from, ArgList)
char	*from,
	**ArgList;
#endif
{
	char *target = ArgList[0];
	char *mag = target++;

	context;
	if (do_hook(SILENCE_LIST, "%c %s", mag, target))
		put_it("%s", convert_output_format(get_string_var(FORMAT_SILENCE_VAR), "%s %c %s", update_clock(GET_TIME), *mag, target));
}


static	void
p_kill(from, ArgList)
	char	*from,
		**ArgList;
{
        char sc[80];
	int port;
	int local = 0;
		

	context;
	port = get_server_port(from_server);
	if (ArgList[1] && strstr(ArgList[1], get_server_name(from_server)))
		if (!index(from, '.'))
			local = 1;
	sprintf(sc, "+%i %d", from_server, port);

	close_server(from_server,empty_string);
	clean_whois_queue();
	window_check_servers();
	set_input_prompt(curr_scr_win, get_string_var(INPUT_PROMPT_VAR), 0);
		
	if (index(from, '.'))
        {
		say("Server [%s] has rejected you (probably due to a nick collision)", from);
        	server (NULL, sc, empty_string);
	}
	else
	{
		if (local && get_int_var(NEXT_SERVER_ON_LOCAL_KILL_VAR))
		{
			int i = from_server + 1;
			if (i >= number_of_servers)
				i = 0;
			sprintf(sc, "+%i", i);
			from_server = -1;
		}
		if (do_hook(DISCONNECT_LIST,"Killed by %s (%s)",from,
				ArgList[1] ? ArgList[1] : "(No Reason Given)"))
			put_it("%s", convert_output_format(get_string_var(FORMAT_KILL_VAR), "%s %s", update_clock(GET_TIME), ArgList[1]? ArgList[1] : "You have been Killed"));
		if (get_int_var(AUTO_RECONNECT_VAR))
			server (NULL, sc, empty_string);
	}
	update_all_status(curr_scr_win, NULL, 0);
}

static	void
p_ping(ArgList)
	char	**ArgList;
{

	context;
	PasteArgs(ArgList, 0);
	send_to_server("PONG %s", ArgList[0]);
}

static	void
p_nick(from, ArgList)
	char	*from,
		**ArgList;
{
	int	one_prints = 0,
		its_me = 0;
ChannelList	*chan;
	char	*line;


	context;
	line = ArgList[0];
	if (my_stricmp(from, get_server_nickname(from_server)) == 0){
		if (from_server == primary_server)
			strmcpy(nickname, line, NICKNAME_LEN);
		set_server_nickname(from_server, line);
		its_me = 1;
		in_e_nick = 0;
	}
	if (check_ignore(from, FromUserHost, NULL, IGNORE_CRAP)!= IGNORED)
	{
		for (chan = server_list[from_server].chan_list; chan; chan = chan->next)
		{
			if (!its_me)
			{
#ifdef WANT_TCL
				check_tcl_nick(from, FromUserHost, from, chan->channel, line);
#endif
			}
			message_from(chan->channel, LOG_CRAP);
			if (do_hook(CHANNEL_NICK_LIST, "%s %s %s", chan, from, line))
				one_prints = 1;
		}
		if (one_prints)
		{
			if (its_me)
			{
				set_string_var(AUTO_RESPONSE_VAR, line);
				message_from(NULL, LOG_CRAP);
			} else
				message_from(what_channel(from, from_server), LOG_CRAP);

			if (do_hook(NICKNAME_LIST, "%s %s", from, line))
				put_it("%s",convert_output_format(get_string_var(its_me?FORMAT_NICKNAME_USER_VAR:im_on_channel(what_channel(from, from_server))?FORMAT_NICKNAME_VAR:FORMAT_NICKNAME_OTHER_VAR), "%s %s %s %s", update_clock(GET_TIME),from, "-", line));
		}
	}
	rename_nick(from, line, from_server);
	if (my_stricmp(from, line))
	{
		notify_mark(from, 0, 0);
		notify_mark(line, 1, 0);
	}
	
}

static int check_mode_change(NickList *nick, char type_mode, char *from, char *this_nick, char *channel)
{
time_t right_now = time(NULL);
int found = 0;
	if (!nick->userlist && !nick->botlist && !isme(nick->nick))
	{
		if ((!nick->chanop && type_mode == '+') || (nick->chanop && type_mode == '-'))
		{
			switch(type_mode)
			{
				case '-':
					if (nick->sent_deop > 4 && right_now - nick->sent_deop_time < 10)
						return 0;
					nick->sent_deop++;
					nick->sent_deop_time = right_now;	
					break;
				case '+':
					if (nick->sent_reop > 4 && right_now - nick->sent_reop_time < 10)
						return 0;
					nick->sent_reop++;
					nick->sent_reop_time = right_now;
					break;
				default:
					break;
			}
			if (my_stricmp(this_nick, from))
			{
				send_to_server("MODE %s %co %s", channel, type_mode, this_nick);
				found++;
			}
		}
	}
	return found;
}

static void check_bitch_mode(char *from, char *uh, char *channel, char *line, ChannelList *chan)
{
NickList *nick;
UserList *user;
char *new_mode = NULL;
char *n = NULL;
time_t right_now;

	context;
	if (!from || !chan || (chan && (!chan->bitch_mode || !chan->chop)))
		return;
	if (match("%.%", from))
		return;
	if (!(nick = (NickList *)find_in_list((List **)&chan->nicks, from,  0)))
		return;
	message_from(channel, LOG_CRAP);
	if (!(user = nick->userlist))
		user = nick->botlist;
	new_mode = m_strdup(line);
	new_mode = next_arg(new_mode, &n);
	if (!user || !check_channel_match(user->channels, channel))
	{
		char *p;
		char type_mode = '%' , *this_nick, *list_nicks, *free_nicks;
		int found = 0;
		free_nicks = list_nicks = m_strdup(n);
		right_now = time(NULL);
		for (p = new_mode; *p; p++)
		{
			switch(*p)
			{
				case '-':
					type_mode = '+';
					break;
				case '+':
					type_mode = '-';
					break;
				case 'o':
					this_nick = next_arg(list_nicks, &list_nicks);
					nick = (NickList *)find_in_list((List **)&chan->nicks, this_nick,  0);
					found += check_mode_change(nick, type_mode, from, this_nick, channel);
					break;
				default:
					break;
			} 
		}
		new_free(&free_nicks);
		if (found)
			put_it("%s", convert_output_format(get_string_var(FORMAT_BITCH_VAR), "%s %s %s %s %s %s", update_clock(GET_TIME), from, uh, channel, new_mode, n));
	}
	new_free(&new_mode);
	message_from(NULL, LOG_CURRENT);
}

static	void p_mode(char *from, char **ArgList)
{
	char	*channel;
	char	*line;
	int	flag;
	int	server;
	
	ChannelList *chan = NULL;
	char buffer[BIG_BUFFER_SIZE+1];		

	context;
	PasteArgs(ArgList, 1);
	channel = ArgList[0];
	line = ArgList[1];

	flag = check_ignore(from, FromUserHost, channel, IGNORE_CRAP);

	message_from(channel, LOG_CRAP);
	if (channel && line)
	{
		char *smode = strchr(from, '.');
		strcpy(buffer, line);
		if (get_int_var(MODE_STRIPPER_VAR))
			strip_modes(from,channel,line);
		if (is_channel(channel))
		{
			chan = prepare_command(&server, channel, NO_OP);
			/* CDE handle mode protection here instead of later */
			update_channel_mode(from, channel, from_server, buffer, chan);

#ifdef WANT_TCL
			check_tcl_mode(from, FromUserHost, from, channel, line);
#endif			
			if (my_stricmp(from, get_server_nickname(from_server))) 
			{
				check_mode_lock(channel, line, from_server);
				check_bitch_mode(from, FromUserHost, channel, line, chan);
			}


			if (flag != IGNORED && do_hook(MODE_LIST, "%s %s %s", from, channel, line))
				put_it("%s",convert_output_format(get_string_var(!smode?FORMAT_MODE_CHANNEL_VAR:FORMAT_SMODE_VAR), "%s %s %s %s %s",update_clock(GET_TIME), from, !smode?FromUserHost:"*", channel, line));
		}
		else
		{
			if (flag != IGNORED && do_hook(MODE_LIST, "%s %s %s", from, channel, line))
			if (!my_stricmp(from, channel))
			{
				if (!my_stricmp(from, get_server_nickname(from_server)))
					put_it("%s",convert_output_format(get_string_var(FORMAT_USERMODE_VAR), "%s %s %s %s %s",update_clock(GET_TIME), from, "*",  channel, line));
				else
					put_it("%s",convert_output_format(get_string_var(FORMAT_USERMODE_VAR), "%s %s %s %s %s",update_clock(GET_TIME), from, !smode?FromUserHost:"*", channel, line));
			}
			else
				put_it("%s",convert_output_format(get_string_var(FORMAT_MODE_VAR), "%s %s %s %s %s",update_clock(GET_TIME), from, !smode?FromUserHost:"*", channel, line));

			update_user_mode(line);
		}
		update_all_status(curr_scr_win, NULL, 0);
	}
	message_from(NULL, LOG_CURRENT);
}

#ifdef __STDC__
static void strip_modes (char *from, char *channel, char *line)
#else
static  void    strip_modes (from, channel, line)
char    *from;
char    *channel;
char    *line;
#endif
{
	char	*mode;
	char 	*pointer;
	char	mag = '+'; /* XXXX Bogus */
        char    *copy = NULL;
        char    *free_copy;
	

	context;
        malloc_strcpy(&copy, line);
        free_copy = copy;
	mode = next_arg(copy, &copy);
	if (is_channel(channel))
	{
		for (pointer = mode;*pointer;pointer++)
		{
			char	c = *pointer;
			switch (c) {
				case '+' :
				case '-' : mag = c; break;
				case 'l' : if (mag == '+')
						do_hook(MODE_STRIPPED_LIST,"%s %s %c%c %s",
						  from,channel,mag,c,next_arg(copy,&copy));
					   else
						do_hook(MODE_STRIPPED_LIST,"%s %s %c%c",
						  from,channel,mag,c);
					   break;
				case 'a' :
				case 'i' :
				case 'm' :
				case 'n' :
				case 'p' :
				case 's' :
				case 't' : do_hook(MODE_STRIPPED_LIST,"%s %s %c%c",from,
						channel,mag,c);
					   break;
				case 'b' :
				case 'k' :
				case 'o' :
				case 'v' : do_hook(MODE_STRIPPED_LIST,"%s %s %c%c %s",from,
						channel,mag,c,next_arg(copy,&copy));
					   break;
				}
		}
	}
	else /* User mode */
	{
		for (pointer = mode;*pointer;pointer++)
		{
			char	c = *pointer;
			switch (c) {
				case '+' :
				case '-' : mag = c; break;
				default  : do_hook(MODE_STRIPPED_LIST,"%s %s %c%c",from, channel, mag, c);
					   break;
			}
		}
	}
        new_free(&free_copy);
}

static	void
p_kick(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*channel,
		*who,
		*comment;
	char	*chankey = NULL;
	ChannelList *chan = NULL;
	NickList *tmpnick = NULL;
	int	t = 0;
	

	context;
	channel = ArgList[0];
	who = ArgList[1];
	comment = ArgList[2] ? ArgList[2] : "(no comment)";

	if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
		tmpnick = (NickList *)find_in_list((List **)&chan->nicks, from, 0);

	message_from(channel, LOG_CRAP);
	if (channel && who && chan)
	{
		update_stats(KICKLIST, channel, from, chan, 0);
#ifdef WANT_TCL
		check_tcl_kick(from, FromUserHost, from, channel, who, comment);
#endif
		if (my_stricmp(who, get_server_nickname(from_server)) == 0)
		{

			char *username = NULL, *ptr;
			Window *window = chan->window;
			Window *old_window = curr_scr_win;
			int server = from_server;
  
			curr_scr_win = window;
			if (chan->key)
				malloc_strcpy(&chankey, chan->key);
			ptr = NULL;

			from_server = window ? window->server : from_server;

			switch(cget_int_var(AUTO_REJOIN_CVAR, channel))
			{
				case 0:
				case 1:
					break;
				case 2:
					if (FromUserHost && strchr(FromUserHost, '@'))
					{
						malloc_strcpy(&username, FromUserHost);
						*strchr(username, '@') = '\0';
						ptr = username;
						if (ptr && (*ptr == '~'))
							ptr++;
						if (ptr && (*ptr == '#'))
							ptr++;
					}
					do_newuser(NULL, ptr, NULL);
					new_free(&username);
					break;
					
				case 3:
					send_to_server("NICK %s", random_str(3,9));
					break;
				case 4:
					do_newuser(NULL, random_str(2,9), NULL);
				case 5:
				default:
						send_to_server("NICK %s", random_str(3,9));
					break;
			}
			if (cget_int_var(AUTO_REJOIN_CVAR, channel))
			{
				send_to_server("JOIN %s %s", channel, chankey ? chankey: empty_string);
				add_to_join_list(channel, from_server, window ? window->refnum : 0);
			}			
			new_free(&chankey);
			remove_channel(channel, from_server);
			update_all_status(curr_scr_win, NULL, 0);

			if (do_hook(KICK_LIST, "%s %s %s %s", who, from, channel, comment?comment:empty_string))
				put_it("%s",convert_output_format(get_string_var(FORMAT_KICK_USER_VAR),"%s %s %s %s %s",update_clock(GET_TIME),from, channel, who, comment));
			curr_scr_win = old_window;
			set_input_prompt(curr_scr_win, get_string_var(INPUT_PROMPT_VAR), 0);
			from_server = server;
		}
		else
		{
			if ((check_ignore(from, FromUserHost, channel, IGNORE_CRAP) != IGNORED) && 
			     do_hook(KICK_LIST, "%s %s %s %s", who, from, channel, comment))
				put_it("%s",convert_output_format(get_string_var(FORMAT_KICK_VAR),"%s %s %s %s %s",update_clock(GET_TIME),from, channel, who, comment));
			if (my_stricmp(from, get_server_nickname(from_server)) && (t = is_other_flood(chan, tmpnick, KICK_FLOOD)))
			{
				if (chan && chan->set_kickflood && chan->chop && tmpnick)
				{
					if (chan->set_kick_on_kickflood > chan->set_deop_on_kickflood)
						send_to_server("MODE %s -o %s", chan->channel, from);
					else 
						send_to_server("KICK %s %s :\002Mass kick detected - (%d kicks in %dsecs)\002", chan->channel, from, chan->set_kick_on_kickflood, t);
				}
			}
			remove_from_channel(channel, who, from_server, 0, NULL);
		}

	}
	update_all_status(curr_scr_win, NULL, 0);
	message_from(NULL, LOG_CURRENT);
}

static	void p_part(char *from, char **ArgList)
{
	char	*channel;
	ChannelList *tmpc;
	

	context;
	if (!from || !*from)
		return;
	channel = ArgList[0];

	message_from(channel, LOG_CRAP);
	in_on_who = 1;

	tmpc = lookup_channel(channel, from_server, CHAN_NOUNLINK);
	update_stats(LEAVELIST, channel, from, tmpc, 0);

	if ((check_ignore(from, FromUserHost, channel, IGNORE_CRAP) != IGNORED) &&
		do_hook(LEAVE_LIST, "%s %s %s", from, channel, FromUserHost))
		put_it("%s",convert_output_format(get_string_var(FORMAT_LEAVE_VAR), "%s %s %s %s", update_clock(GET_TIME), from, FromUserHost, channel));
	if (!my_stricmp(from, get_server_nickname(from_server)))
	{
		remove_channel(channel, from_server);
		remove_from_mode_list(channel, from_server);
		remove_from_join_list(channel, from_server);
		set_input_prompt(curr_scr_win, get_string_var(INPUT_PROMPT_VAR), 0);
	}
	else
	{
		remove_from_channel(channel, from, from_server, 0, NULL);
#ifdef WANT_TCL
		check_tcl_part(from, FromUserHost, from, channel);
#endif
	}
	update_all_status(curr_scr_win, NULL, 0);
	update_input(UPDATE_ALL);
	message_from(NULL, LOG_CURRENT);
	in_on_who = 0;
}

static void p_odd (char *from, char *comm, char **ArgList)
{
	PasteArgs(ArgList, 0);
	if (do_hook(ODD_SERVER_STUFF_LIST, "%s %s %s", from ? from : "*", comm, ArgList[0]))
	{
		if (from)
			say("Odd server stuff: \"%s %s\" (%s)", comm, ArgList[0], from);
		else
			say("Odd server stuff: \"%s %s\"", comm, ArgList[0]);
	}
}

void parse_server(char *line)
{
	char	*from,
		*comm,
		*end;
	int	numeric;
	char	**ArgList;
	char	*TrueArgs[MAXPARA + 1] = {NULL};


	context;
	if (!line || !*line)
		return;

	end = strlen(line) + line;
	if (*--end == '\n')
		*end-- = '\0';
	if (*end == '\r')
		*end-- = '\0';

	if (*line == ':')
	{
		if (!do_hook(RAW_IRC_LIST, "%s", line + 1))
			return;
	}
	else if (!do_hook(RAW_IRC_LIST, "%s %s", "*", line))
		return;

	ArgList = TrueArgs;
#ifdef WANT_TCL
	check_tcl_raw(line);
#endif
	BreakArgs(line, &from, ArgList);

	if (!(comm = (*ArgList++)) || !from || !*ArgList)
		return;		/* Empty line from server - ByeBye */

	/* 
	 * I reformatted these in may '96 by using the output of /stats m
	 * from a few busy servers.  They are arranged so that the most 
	 * common types are high on the list (to save the average number
	 * of compares.)  I will be doing more testing in the future on
	 * a live client to see if this is a reasonable order.
	 */
	if ((numeric = atoi(comm)))
		numbered_command(from, numeric, ArgList);

	/* There are the core msgs for most non-numeric traffic. */
	else if (!strcmp(comm, "PRIVMSG")) 	p_privmsg(from, ArgList);
	else if (!strcmp(comm, "JOIN")) 	p_channel(from, ArgList);
	else if (!strcmp(comm, "NICK")) 	p_nick(from, ArgList);
	else if (!strcmp(comm, "PART")) 	p_part(from, ArgList);
	else if (!strcmp(comm, "MODE")) 	p_mode(from, ArgList);
	else if (!strcmp(comm, "QUIT")) 	p_quit(from, ArgList);
	else if (!strcmp(comm, "NOTICE")) 	parse_notice(from, ArgList);
	else if (!strcmp(comm, "TOPIC")) 	p_topic(from, ArgList);
	else if (!strcmp(comm, "KICK")) 	p_kick(from, ArgList);
	else if (!strcmp(comm, "INVITE")) 	p_invite(from, ArgList);

	/* These are used, but not nearly as much as ones above */
	else if (!strcmp(comm, "WALLOPS")) 	p_wallops(from, ArgList);
	else if (!strcmp(comm, "ERROR")) 	p_error(from, ArgList);
	else if (!strcmp(comm, "ERROR:")) 	p_error(from, ArgList);
	else if (!strcmp(comm, "SILENCE")) 	p_silence(from, ArgList);
	else if (!strcmp(comm, "KILL")) 	p_kill(from, ArgList);
	else if (!strcmp(comm, "PONG")) 	p_pong(from, ArgList);
	else if (!strcmp(comm, "PING")) 	p_ping(ArgList);

	/* Some kind of unrecognized/unsupported command */
	else
						p_odd(from, comm, ArgList);
	from_server = -1;
}
