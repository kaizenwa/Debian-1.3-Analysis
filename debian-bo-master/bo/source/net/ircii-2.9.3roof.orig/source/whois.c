/*
 * whois.c: Some tricky routines for querying the server for information
 * about a nickname using WHOIS.... all the time hiding this from the user.  
 *
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: whois.c,v 1.25.2.1 1996/07/20 19:14:51 mrg Exp $";
#endif

#undef MONITOR_Q /* this one is for monitoring of the 'whois queue' (debug) */

#include "irc.h"

#include "whois.h"
#include "hook.h"
#include "lastlog.h"
#include "vars.h"
#include "server.h"
#include "ignore.h"
#include "ircaux.h"
#include "notify.h"
#include "numbers.h"
#include "window.h"
#include "edit.h"
#include "output.h"
#include "parse.h"
#include "ctcp.h"

char	whois_nick[] = "#WHOIS#";
char	wait_nick[] = "#WAIT#";
char	redirect_nick[] = "#RED#";

/* current setting for BEEP_ON_MSG */
int	beep_on_level;

static	int	ignore_whois_crap = 0;
static	int	eat_away = 0;
/*
static	WhoisStuff whois_stuff =
{
	(char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0,
	(char *) 0, (char *) 0, (char *) 0, (char *) 0, 0, 0, 0
};
*/

/* WQ_head and WQ_tail point to the head and tail of the whois queue */
WhoisQueue *WQ_head = (WhoisQueue *) 0;
WhoisQueue *WQ_tail = (WhoisQueue *) 0;

static	char	show_away_flag = 0;

#ifdef HAVE_STDARG_H
static	void	typed_add_to_whois_queue _((int, char *, void (*)(WhoisStuff *, char *, char *), char *, va_list));
#else
static  void	typed_add_to_whois_queue();
#endif
static	char	* whois_queue_head _((int));
static	int	whois_type_head _((int));
static	void	(*whois_func_head _((int))) _((WhoisStuff *, char *, char *));
static	WhoisQueue	*remove_from_whois_queue _((int));

void
set_beep_on_msg(str)
	char	*str;
{
	beep_on_level = parse_lastlog_level(str);
	set_string_var(BEEP_ON_MSG_VAR, bits_to_lastlog_level(beep_on_level));
}

/*
 * whois_queue_head: returns the nickname at the head of the whois queue, or
 * NULL if the queue is empty.  It does not modify the queue in any way. 
 */
static	char	*
whois_queue_head(server_index)
int	server_index;
{
	if ((WQ_head = (WhoisQueue *) get_server_qhead(server_index)) != NULL)
		return (WQ_head->nick);
	else
		return ((char *) 0);
}

static	int
whois_type_head(server_index)
int	server_index;
{
	if ((WQ_head = (WhoisQueue *) get_server_qhead(server_index)) != NULL)
		return (WQ_head->type);
	else
		return -1;
}

static	void (*
whois_func_head (server_index)) ()
int	server_index;
{
	if ((WQ_head = (WhoisQueue *) get_server_qhead(server_index)) != NULL)
		return (WQ_head->func);
	else
		return NULL;
}

/*
 * remove_from_whois_queue: removes the top element of the whois queue and
 * returns the element as its function value.  This routine repairs handles
 * all queue stuff, but the returned element is mallocd and must be freed by
 * the calling routine 
 */
static	WhoisQueue *
remove_from_whois_queue(server_index)
int	server_index;
{
	WhoisQueue *new;

	new = (WhoisQueue *) get_server_qhead(server_index);
	set_server_qhead(server_index, new->next);
	if (new->next == (WhoisQueue *) 0)
		set_server_qtail(server_index, (WhoisQueue *) 0);
	return (new);
}

/*
 * clean_whois_queue: this empties out the whois queue.  This is used after
 * server reconnection to assure that no bogus entries are left in the whois
 * queue 
 */
void
clean_whois_queue()
{
	WhoisQueue *thing;

	while (whois_queue_head(from_server))
	{
		thing = remove_from_whois_queue(from_server);
		new_free(&thing->nick);
		new_free(&thing->text);
		new_free(&thing);
	}
	ignore_whois_crap = 0;
	eat_away = 0;
}
/* ison_returned: this is called when numeric 303 is received in
 * numbers.c. ISON must always be the property of the WHOIS queue.
 * Although we will check first that the top element expected is
 * actually an ISON.
 */
/*ARGSUSED*/
void
ison_returned(from, ArgList)
	char	*from,
		**ArgList;
{
	WhoisQueue *thing;

	if (whois_type_head(parsing_server_index) == WHOIS_ISON)
	{
		thing = remove_from_whois_queue(parsing_server_index);
		thing->func((WhoisStuff *) 0, thing->nick, ArgList[0]? ArgList[0] : empty_string);
		new_free(&thing->nick);
		new_free(&thing->text);
		new_free(&thing);
	}
	else
		ison_now((WhoisStuff *) 0, ArgList[0] ? ArgList[0] : empty_string, (char *) 0);
}

/* userhost_returned: this is called when numeric 302 is received in
 * numbers.c. USERHOST must also remain the property of the WHOIS
 * queue. Sending it without going via the WHOIS queue will cause
 * the queue to become corrupted.
 *
 * While USERHOST gives us similar information to WHOIS, this routine's
 * format is a little different to those that handle the WHOIS numerics.
 * A list of nicks can be supplied to USERHOST, and any of those
 * nicks are not currently signed on, it will just omit reporting it.
 * this means we must go through the list of nicks returned and check
 * them for missing entries. This means stepping through the requested
 * nicks one at a time.
 *
 * A side effect of this is that user initiated USERHOST requests get
 * reported as separate replies even though one request gets made and
 * only one reply is actually received. This should make it easier for
 * people wanting to use USERHOST for their own purposes.
 *
 * In this routine, cnick points to all the information in the next
 * entry from the returned data.
 */
/*ARGSUSED*/
void
userhost_returned(from, ArgList)
	char	*from,
		**ArgList;
{
	WhoisQueue *thing;
	WhoisStuff *whois_stuff = NULL;
	char	*nick,
		*cnick = NULL,
		*tnick;
	char	*user,
		*cuser = (char *) 0;
	char	*host,
		*chost = (char *) 0;
	int	ishere,
		cishere = 1;
	int	isoper,
		cisoper = 0;
	int	noton,
		isuser,
		parsed;
	char	*queue_nicks;

	if (!ArgList[0])
		return;
	if (whois_type_head(parsing_server_index) == WHOIS_USERHOST)
	{
		isuser = (whois_func_head(parsing_server_index) == USERHOST_USERHOST);
		whois_stuff = get_server_whois_stuff(parsing_server_index);
		thing = remove_from_whois_queue(parsing_server_index);
		queue_nicks = thing->nick;
	}
	else
	{
		isuser = 1;
		thing = (WhoisQueue *) 0;
		queue_nicks = (char *) 0;
		whois_stuff = NULL;
	}
	parsed = 0;
	while ((parsed || (cnick = next_arg(ArgList[0], ArgList))) ||
			queue_nicks)
	{
		if (queue_nicks)
		{
			tnick = next_arg(queue_nicks, &queue_nicks);
			if (!*queue_nicks)
				queue_nicks = (char *) 0;
		}
		else
			tnick = NULL;
		if (cnick && !parsed)
		{
			if (!(cuser = index(cnick,'=')))
				break;
			if (*(cuser - 1) == '*')
			{
				*(cuser - 1) = '\0';
				cisoper = 1;
			}
			else
				cisoper = 0;
			*cuser++ = '\0';
			if (*cuser++ == '+')
				cishere = 1;
			else
				cishere = 0;
			if (!(chost = index(cuser, '@')))
				break;
			*chost++ = '\0';
			parsed = 1;
		}
		if (!cnick || (tnick && my_stricmp(cnick, tnick)))
		{
			if (tnick)
				nick = tnick;
			else
				nick = cnick;
			user = host = "<UNKNOWN>";
			isoper = 0;
			ishere = 1;
			noton = 1;
		}
		else
		{
			nick = cnick;
			user = cuser;
			host = chost;
			isoper = cisoper;
			ishere = cishere;
			noton = parsed = 0;
		}
		if (!isuser)
		{
			malloc_strcpy(&whois_stuff->nick, nick);
			malloc_strcpy(&whois_stuff->user, user);
			malloc_strcpy(&whois_stuff->host, host);
			whois_stuff->oper = isoper;
			whois_stuff->not_on = noton;
			if (!ishere)
				malloc_strcpy(&whois_stuff->away, empty_string);
			else
				new_free(&whois_stuff->away);
			thing->func(whois_stuff, tnick, thing->text);
			new_free(&whois_stuff->away);
		}
		else
		{
			if (do_hook(current_numeric, "%s %s %s %s %s", nick,
			    isoper ? "+" : "-", ishere ? "+" : "-", user, host))
				put_it("%s %s is %s@%s%s%s", numeric_banner(),
					nick, user, host, isoper ?
					" (Is an IRC operator)" : empty_string,
					ishere ? empty_string : " (away)");
		}
	}
	if (thing)
	{
		new_free(&thing->nick);
		new_free(&thing->text);
		new_free(&thing);
	}
}

/*
 * whois_name: routine that is called when numeric 311 is received in
 * numbers.c. This routine parses out the information in the numeric and
 * saves it until needed (see whois_server()).  If the whois queue is empty,
 * this routine simply displays the data in the normal fashion.  Why would
 * the queue ever be empty, you ask? If the user does a "WHOIS *" or any
 * other whois with a wildcard, you will get multiple returns from the
 * server.  So, instead of attempting to handle each of these, only the first
 * is handled, and the others fall through.  It is up to the programmer to
 * prevent wildcards from interfering with what they want done.  See
 * channel() in edit.c 
 */
void
whois_name(from, ArgList)
	char	*from;
	char	**ArgList;
{
	char	*nick,
		*user,
		*host,
		*channel,
		*ptr,
		*name;
	WhoisStuff *whois_stuff;

	PasteArgs(ArgList, 4);
	nick = ArgList[0];
	user = ArgList[1];
	host = ArgList[2];
	channel = ArgList[3];
	name = ArgList[4];
	if (!nick || !user || !host || !channel || !name)
		return;
	whois_stuff = get_server_whois_stuff(parsing_server_index);
	if ((ptr = whois_queue_head(parsing_server_index))
	 && (whois_type_head(parsing_server_index) & (WHOIS_WHOIS|WHOIS_ISON2))
	 && (my_stricmp(ptr, nick) == 0))
	{
		malloc_strcpy(&whois_stuff->nick, nick);
		malloc_strcpy(&whois_stuff->user, user);
		malloc_strcpy(&whois_stuff->host, host);
		malloc_strcpy(&whois_stuff->name, name);
		malloc_strcpy(&whois_stuff->channel, channel);
		new_free(&whois_stuff->away);
		whois_stuff->oper = 0;
		whois_stuff->chop = 0;
		whois_stuff->not_on = 0;
		ignore_whois_crap = 1;
		eat_away = 1;
	}
	else
	{
		ignore_whois_crap = 0;
		eat_away = 0;
		message_from((char *) 0, LOG_CRAP);
		if (do_hook(current_numeric, "%s %s %s %s %s %s", from, nick,
				user, host, channel, name))
			put_it("%s %s is %s@%s (%s)", numeric_banner(), nick,
					user, host, name);
		message_from((char *) 0, LOG_CURRENT);
	}
}

/*
 * whowas_name: same as whois_name() above but it is called with a numeric of
 * 314 when the user does a WHOWAS or when a WHOIS'd user is no longer on IRC 
 * and has set the AUTO_WHOWAS variable.
 */
void
whowas_name(from, ArgList)
	char	*from;
	char	**ArgList;
{
	char	*nick,
		*user,
		*host,
		*channel,
		*ptr,
		*name;
	WhoisStuff *whois_stuff;
	int	lastlog_level;

	PasteArgs(ArgList, 4);
	nick = ArgList[0];
	user = ArgList[1];
	host = ArgList[2];
	channel = ArgList[3];
	name = ArgList[4];
	if (!nick || !user || !host || !channel || !name)
		return;

	lastlog_level = set_lastlog_msg_level(LOG_CRAP);
	whois_stuff = get_server_whois_stuff(parsing_server_index);
	if ((ptr = whois_queue_head(parsing_server_index))
	 && (whois_type_head(parsing_server_index) & (WHOIS_WHOIS|WHOIS_ISON2))
	 && (my_stricmp(ptr, nick) == 0))
	{
		malloc_strcpy(&whois_stuff->nick, nick);
		malloc_strcpy(&whois_stuff->user, user);
		malloc_strcpy(&whois_stuff->host, host);
		malloc_strcpy(&whois_stuff->name, name);
		malloc_strcpy(&whois_stuff->channel, channel);
		new_free(&whois_stuff->away);
		whois_stuff->oper = 0;
		whois_stuff->chop = 0;
		whois_stuff->not_on = 1;
		ignore_whois_crap = 1;
	}
	else
	{
		ignore_whois_crap = 0;
		if (do_hook(current_numeric, "%s %s %s %s %s %s", from, nick,
				user, host, channel, name))
			put_it("%s %s was %s@%s (%s) on channel %s",
				numeric_banner(), nick, user, host, name,
				(*channel == '*') ? "*private*" : channel);
	}
	set_lastlog_msg_level(lastlog_level);
}

void
whois_channels(from, ArgList)
	char	*from;
	char	**ArgList;
{
	char	*ptr;
	char	*line;
	WhoisStuff *whois_stuff;

	PasteArgs(ArgList, 1);
	line = ArgList[1];
	whois_stuff = get_server_whois_stuff(parsing_server_index);
	if ((ptr = whois_queue_head(parsing_server_index))
	 && (whois_type_head(parsing_server_index) & (WHOIS_WHOIS|WHOIS_ISON2))
	 && whois_stuff->nick
	 && (my_stricmp(ptr, whois_stuff->nick) == 0))
	{
		if (whois_stuff->channels == (char *) 0)
			malloc_strcpy(&whois_stuff->channels, line);
		else
			malloc_strcat(&whois_stuff->channels, line);
	}
	else
	{
		if (do_hook(current_numeric, "%s %s", from, line))
			put_it("%s on channels: %s", numeric_banner(), line);
	}
}

/*
 * whois_server: Called in numbers.c when a numeric of 312 is received.  If
 * all went well, this routine collects the needed information, pops the top
 * element off the queue and calls the function as described above in
 * WhoisQueue.  It then releases all the mallocd data.  If the queue is empty
 * (same case as described in whois_name() above), the information is simply
 * displayed in the normal fashion. Added a check to see if whois_stuff->nick
 * is NULL. This can happen if something is added to an empty whois queue
 * between the whois name being received and the server.
 */
void
whois_server(from, ArgList)
	char	*from;
	char	**ArgList;
{
	char	*server,
		*ptr;
	char	*line;
	WhoisStuff *whois_stuff;

	show_away_flag = 1;
	if (!ArgList[0] || !ArgList[1])
		return;
	if (ArgList[2])
	{
		server = ArgList[1];
		line = ArgList[2];
	}
	else
	{
		server = ArgList[0];
		line = ArgList[1];
	}
	whois_stuff = get_server_whois_stuff(parsing_server_index);
	if ((ptr = whois_queue_head(parsing_server_index))
	 && (whois_type_head(parsing_server_index) & (WHOIS_WHOIS|WHOIS_ISON2))
	 && whois_stuff->nick && /* This is *weird* */
	(my_stricmp(ptr, whois_stuff->nick) == 0))
	{
		malloc_strcpy(&whois_stuff->server, server);
		malloc_strcpy(&whois_stuff->server_stuff, line);
	}
	else
	{
		if (do_hook(current_numeric, "%s %s %s", from, server, line))
			put_it("%s on irc via server %s (%s)",
				numeric_banner(), server, line);
	}
}

/*
 * whois_oper: This displays the operator status of a user, as returned by
 * numeric 313 from the server.  If the ignore_whois_crap flag is set,
 * nothing is dispayed. 
 */
void
whois_oper(from, ArgList)
	char	*from;
	char	**ArgList;
{
	WhoisStuff *whois_stuff;

	whois_stuff = get_server_whois_stuff(parsing_server_index);
	PasteArgs(ArgList, 1);
	if (ignore_whois_crap)
		whois_stuff->oper = 1;
	else
	{
		char	*nick;

		if ((nick = ArgList[0]) != NULL)
		{
			if (do_hook(current_numeric, "%s %s %s", from, nick,
					ArgList[1]))
				put_it("%s %s %s%s", numeric_banner(), nick,
					ArgList[1], (server_list[parsing_server_index].version >
					Server2_7) ? empty_string
						   : " (is an IRC operator)");
		}
	}
}

void
whois_lastcom(from, ArgList)
	char	*from;
	char	**ArgList;
{
	if (!ignore_whois_crap)
	{
		char	flag, *nick, *idle_str;
		int	idle;

		PasteArgs(ArgList, 2);
		if ((nick = ArgList[0]) && (idle_str = ArgList[1]) &&
				do_hook(current_numeric, "%s %s %s %s", from,
					nick, idle_str, ArgList[2]))
		{
			if ((idle = atoi(idle_str)) > 59)
			{
				idle = idle/60;
				flag = 1;
			}
			else
				flag = 0;
			put_it("%s %s has been idle %d %ss", numeric_banner(),
				nick, idle, flag? "minute": "second");
		}
	}
}

/*
 * whois_chop: This displays the operator status of a user, as returned by
 * numeric 313 from the server.  If the ignore_whois_crap flag is set,
 * nothing is dispayed. 
 */
void
whois_chop(from, ArgList)
	char	*from;
	char	**ArgList;
{
	WhoisStuff *whois_stuff;

	whois_stuff = get_server_whois_stuff(parsing_server_index);
	PasteArgs(ArgList, 1);
	if (ignore_whois_crap)
		whois_stuff->chop = 1;
	else
	{
		char	*nick;

		if ((nick = ArgList[0]) != NULL)
		{
			if (do_hook(current_numeric, "%s %s %s",from, nick,
					ArgList[1]))
				put_it("%s %s (is a channel operator)",
					numeric_banner(), nick, ArgList[1]);
		}
	}
}

void
end_of_whois(from, ArgList)
	char	*from;
	char	**ArgList;
{
	char	*nick;
	char	*ptr;
	WhoisStuff *whois_stuff;

	whois_stuff = get_server_whois_stuff(parsing_server_index);

	show_away_flag = 0;
	set_server_whois(parsing_server_index,1);
	if ((nick = ArgList[0]) != NULL)
	{
		ptr = whois_queue_head(parsing_server_index);
		if (ptr && (whois_type_head(parsing_server_index) & (WHOIS_WHOIS|WHOIS_ISON2)) && (my_stricmp(ptr, nick) == 0))
		{
			WhoisQueue *thing;

			thing = remove_from_whois_queue(parsing_server_index);
			whois_stuff->not_on = 0;
			thing->func(whois_stuff, thing->nick, thing->text);
			new_free(&whois_stuff->channels);
			new_free(&thing->nick);
			new_free(&thing->text);
			new_free(&thing);
			ignore_whois_crap = 0;
			return;
		}
		PasteArgs(ArgList, 0);
		if (do_hook(current_numeric, "%s %s", from, ArgList[0]))
			if (get_int_var(SHOW_END_OF_MSGS_VAR))
				display_msg(from, ArgList);
	}
}

/*
 * no_such_nickname: Handler for numeric 401, the no such nickname error. If
 * the nickname given is at the head of the queue, then this routine pops the
 * top element from the queue, sets the whois_stuff->flag to indicate that the
 * user is no longer on irc, then calls the func() as normal.  It is up to
 * that function to set the ignore_whois_crap variable which will determine
 * if any other information is displayed or not. 
 *
 * that is, it used to.  now it does bugger all, seeing all the functions that
 * used to use it, now use no such command.  -phone, april 1993.
 */
/*ARGSUSED*/
void
no_such_nickname(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*nick;
	char		*ptr;
	WhoisStuff	*whois_stuff;

	whois_stuff = get_server_whois_stuff(parsing_server_index);
	ptr = whois_queue_head(parsing_server_index);
	PasteArgs(ArgList, 1);
	nick = ArgList[0];
	if (*nick == '!')
	{
		char	*name = nick+1;

		if (ptr && (whois_type_head(parsing_server_index) & (WHOIS_WHOIS|WHOIS_ISON2)) && !strcmp(ptr, name))
		{
			WhoisQueue *thing;

			/* There's a query in the WhoisQueue : assume it's
			   completed and remove it from the queue -Sol */

			thing = remove_from_whois_queue(parsing_server_index);
			whois_stuff->not_on = 0;
			thing->func(whois_stuff, thing->nick, thing->text);
			new_free(&whois_stuff->channels);
			new_free(&thing->nick);
			new_free(&thing->text);
			new_free(&thing);
			ignore_whois_crap = 0;
			return;
		}
		return;
	}
	notify_mark(nick, 0, 0);
	if (ptr && (whois_type_head(parsing_server_index) == WHOIS_ISON2) &&
	    !strcmp(ptr, nick))
	{
		WhoisQueue *thing;

		/* Remove query from queue. Don't display anything. -Sol */

		thing = remove_from_whois_queue(parsing_server_index);
		new_free(&whois_stuff->channels);
		new_free(&thing->nick);
		new_free(&thing->text);
		new_free(&thing);
		ignore_whois_crap = 0;
		return;
	}
	if (do_hook(current_numeric, "%s %s %s", from, nick, ArgList[1]))
		put_it("%s %s: %s", numeric_banner(), nick, ArgList[1]);
	if ((get_server_version(parsing_server_index) > Server2_5) &&
	    get_int_var(AUTO_WHOWAS_VAR))
		send_to_server("WHOWAS %s", nick);
}

/*
 * user_is_away: called when a 301 numeric is received.  Nothing is displayed
 * by this routine if the ignore_whois_crap flag is set 
 */
/*ARGSUSED*/
void
user_is_away(from, ArgList)
	char	*from,
		**ArgList;
{
	static	char	*last_away_msg = (char *) 0,
			*last_away_nick = (char *) 0;
	char	*message,
		*who;
	WhoisStuff *whois_stuff;

if (!from)
	return;

	PasteArgs(ArgList, 1);
	whois_stuff = get_server_whois_stuff(parsing_server_index);

	if ((who = ArgList[0]) && (message = ArgList[1]))
	{
		if (whois_stuff->nick && (!strcmp(who, whois_stuff->nick)) &&
				eat_away)
			malloc_strcpy(&whois_stuff->away, message);
		else
		{
			if (!show_away_flag && get_int_var(SHOW_AWAY_ONCE_VAR))
			{
				if (!last_away_msg || strcmp(last_away_nick,
					from) || strcmp(last_away_msg, message))
				{
					malloc_strcpy(&last_away_nick, from);
					malloc_strcpy(&last_away_msg, message);
				}
				else return;
			}
			if (do_hook(current_numeric, "%s %s", who, message))
				put_it("%s %s is away: %s",numeric_banner(),
					who, message);
		}
		eat_away = 0;
	}
}

/*
 * The stuff below this point are all routines suitable for use in the
 * add_to_whois_queue() call as the func parameter 
 */

/*
 * whois_ignore_msgs: This is used when you are ignoring MSGs using the
 * user@hostname format 
 */
void
whois_ignore_msgs(stuff, nick, text)
	WhoisStuff *stuff;
	char	*nick;
	char	*text;
{
	char	*ptr;
	int	level;

	if (stuff)
	{
		ptr = (char *) new_malloc(strlen(stuff->user) +
			strlen(stuff->host) + 2);
		strcpy(ptr, stuff->user);
		strcat(ptr, "@");
		strcat(ptr, stuff->host);
		if (is_ignored(ptr, IGNORE_MSGS) != IGNORED)
		{
			level = set_lastlog_msg_level(LOG_MSG);
			message_from(stuff->nick, LOG_MSG);
			if (sed == 1 && !do_hook(ENCRYPTED_PRIVMSG_LIST,"%s %s", stuff->nick,text))
			{
				sed = 0;
				set_lastlog_msg_level(level);
				message_from((char *) 0, LOG_CURRENT);
				return;
			}
			if (do_hook(MSG_LIST, "%s %s", stuff->nick, text))
			{
				if (away_set)
				{
					time_t	t;
					char	*msg = (char *) 0;

					t = time(0);
					msg = (char *) new_malloc(strlen(text)
						+ 20);
					sprintf(msg, "%s <%.16s>", text,
						ctime(&t));
					put_it("*%s* %s", stuff->nick, msg);
					new_free(&msg);
					beep_em(get_int_var(BEEP_WHEN_AWAY_VAR));
				}
				else
				{
					put_it("*%s* %s", stuff->nick, text);
					beep_em(get_int_var(BEEP_ON_MSG_VAR));
				}
			}
			if (beep_on_level & LOG_MSG)
				beep_em(1);
			set_lastlog_msg_level(level);
			message_from((char *) 0, LOG_CURRENT);
			sed = 0;
			notify_mark(nick, 1, 0);
		}
		else
			send_to_server("NOTICE %s :%s is ignoring you.",
				nick, get_server_nickname(parsing_server_index));
		new_free(&ptr);
	}
}

/*ARGSUSED*/
void
whois_nickname(stuff,nick,text)
	WhoisStuff *stuff;
	char	*nick;
	char	*text;
	{
	if (stuff)
	{
		if (!(my_stricmp(stuff->user,username)) &&
				(!my_stricmp(stuff->host,hostname)))
			set_server_nickname(parsing_server_index,nick);
	}
}

/*
 * whois_ignore_notices: This is used when you are ignoring NOTICEs using the
 * user@hostname format 
 */
/*ARGSUSED*/
void
whois_ignore_notices(stuff, nick, text)
	WhoisStuff *stuff;
	char	*nick;
	char	*text;
{
	char	*ptr;
	int	level;

	if (stuff)
	{
		ptr = (char *) new_malloc(strlen(stuff->user) +
			strlen(stuff->host) + 2);
		strcpy(ptr, stuff->user);
		strcat(ptr, "@");
		strcat(ptr, stuff->host);
		if (is_ignored(ptr, IGNORE_NOTICES) != IGNORED)
		{
			level = set_lastlog_msg_level(LOG_NOTICE);
			message_from(stuff->nick, LOG_NOTICE);
			if (sed == 0 && !do_hook(ENCRYPTED_NOTICE_LIST,"%s %s", stuff->nick, text))
			{
				sed = 0;
				return;
			}
			if (do_hook(NOTICE_LIST, "%s %s", stuff->nick, text))
				put_it("-%s- %s", stuff->nick, text);
			set_lastlog_msg_level(level);
			message_from((char *) 0, LOG_CURRENT);
			sed = 0;
		}
		new_free(&ptr);
	}
}

/*
 * whois_ignore_invites: This is used when you are ignoring INVITES using the
 * user@hostname format 
 */
void
whois_ignore_invites(stuff, nick, text)
	WhoisStuff *stuff;
	char	*nick;
	char	*text;
{
	char	*ptr;

	if (stuff)
	{
		ptr = (char *) new_malloc(strlen(stuff->user) +
			strlen(stuff->host) + 2);
		strcpy(ptr, stuff->user);
		strcat(ptr, "@");
		strcat(ptr, stuff->host);
		if (is_ignored(ptr, IGNORE_INVITES) != IGNORED)
		{
			if (do_hook(INVITE_LIST, "%s %s", stuff->nick, text))
				say("%s invites you to channel %s",
					stuff->nick, text);
			malloc_strcpy(&invite_channel, text);
		}
		else
			send_to_server("NOTICE %s :%s is ignoring you.",
				nick, get_server_nickname(parsing_server_index));
		new_free(&ptr);
	}
}

/*
 * whois_ignore_walls: This is used when you are ignoring WALLS using the
 * user@hostname format 
 */
/*ARGSUSED*/
void
whois_ignore_walls(stuff, nick, text)
	WhoisStuff *stuff;
	char	*nick;
	char	*text;
{
	char	*ptr;
	int	level;

	level = set_lastlog_msg_level(LOG_WALL);
	message_from(stuff->nick, LOG_WALL);
	if (stuff)
	{
		ptr = (char *) new_malloc(strlen(stuff->user) +
			strlen(stuff->host) + 2);
		strcpy(ptr, stuff->user);
		strcat(ptr, "@");
		strcat(ptr, stuff->host);
		if (is_ignored(ptr, IGNORE_WALLS) != IGNORED)
		{
			if (do_hook(WALL_LIST, "%s %s", stuff->nick, text))
				put_it("#%s# %s", stuff->nick, text);
			if (beep_on_level & LOG_WALL)
				beep_em(1);
		}
		new_free(&ptr);
	}
	set_lastlog_msg_level(level);
	message_from((char *) 0, LOG_CURRENT);
}

void
convert_to_whois()
{
	char	*NextAsked;
	char	*Names;
	WhoisQueue *thing;
	void	(*func) _((WhoisStuff *, char *, char *));

	if (!(whois_type_head(parsing_server_index) & (WHOIS_USERHOST|WHOIS_WHOIS|WHOIS_ISON2)))
	{
		say("Server does not support USERHOST");
		return; /* USERHOST sent interactively. */
	}
	thing = remove_from_whois_queue(parsing_server_index);
	switch(thing->type)
	{
	case WHOIS_ISON:
		Names = thing->nick;
		while ( (NextAsked = next_arg(Names, &Names)))
		{
			if (thing->func == ison_notify)
			{
				func = (void (*) _((WhoisStuff *, char *, char *))) whois_notify;
				add_to_whois_queue(NextAsked, func, "%s", NextAsked);
			}
			else
				say("Server does not support ISON");
		}
		break;
	case WHOIS_USERHOST:
		add_to_whois_queue(thing->nick, thing->func, "%s", thing->text);
		break;
	}
	new_free(&thing->nick);
	new_free(&thing->text);
	new_free(&thing);
}

void
ison_notify(unused, AskedFor, AreOn)
	WhoisStuff *unused;
	char	*AskedFor;
	char	*AreOn;
{
	char	*NextAsked;
	char	*NextGot;

	NextGot = next_arg(AreOn, &AreOn);
	while ((NextAsked = next_arg(AskedFor, &AskedFor)) != NULL)
	{
		if (NextGot && !my_stricmp(NextAsked, NextGot))
		{
			notify_mark(NextAsked, 1, 1);
			NextGot = next_arg(AreOn, &AreOn);
		}
		else
			notify_mark(NextAsked, 0, 1);
	}
}

/*
 * whois_notify: used by the routines in notify.c to tell when someone has
 * signed on or off irc 
 */
/*ARGSUSED*/
void
whois_notify(stuff, nick, text)
	WhoisStuff *stuff;
	char	*nick;
	char	*text;
{
	int	level;

	level = set_lastlog_msg_level(LOG_CRAP);
	if (stuff)
		notify_mark(stuff->nick, 1, 1);
	else
		notify_mark(nick, 0, 1);
	set_lastlog_msg_level(level);
}

void
whois_new_wallops(stuff, nick, text)
	WhoisStuff *stuff;
	char	*nick;
	char	*text;
{
	int	flag,
	level;
	char	*high;

	flag = is_ignored(nick, IGNORE_WALLOPS);
	if (flag != IGNORED)
	{
		if (flag == HIGHLIGHTED)
			high = &highlight_char;
		else
			high = empty_string;
		if (stuff && (ignore_usernames & IGNORE_WALLOPS))
		{
			char	*ptr;

			ptr = (char *) new_malloc(strlen(stuff->user) +
				strlen(stuff->host) + 2);
			strcpy(ptr, stuff->user);
			strcat(ptr, "@");
			strcat(ptr, stuff->host);
			if (is_ignored(ptr, IGNORE_WALLOPS) == IGNORED)
			{
				new_free(&ptr);
				return;
			}
			new_free(&ptr);
		}
		message_from(nick, LOG_WALLOP);
		level = set_lastlog_msg_level(LOG_WALLOP);
		if (stuff)
		{
			if (do_hook(WALLOP_LIST, "%s %s %s", nick,
					(stuff->oper ? "+" : "-"), text))
				put_it("%s!%s%s!%s %s", high, nick,
					stuff->oper ? "*" : empty_string,
					high, text);
		}
		else
		{
			if (do_hook(WALLOP_LIST, "%s - %s", nick, text))
				put_it("%s!%s!%s %s", high, nick, high, text);
		}
		if (beep_on_level & LOG_WALLOP)
			beep_em(1);
		set_lastlog_msg_level(level);
		message_from((char *) 0, LOG_CURRENT);
	}
}

/* I put the next routine down here to keep my compile quiet */

/*
 * add_to_whois_queue: This routine is called whenever you want to do a WHOIS
 * or WHOWAS.  What happens is this... each time this function is called it
 * adds a new element to the whois queue using the nick and func as in
 * WhoisQueue, and creating the text element using the format and args.  It
 * then issues the WHOIS or WHOWAS.  
 */
void
#ifdef HAVE_STDARG_H
add_to_whois_queue(char *nick, void (*func) _((WhoisStuff *, char *, char *)), char *format, ...)
#else
add_to_whois_queue(nick, func, format, arg1, arg2,
		arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
	void	(*func) _((WhoisStuff *, char *, char *));
	char	*format,
		*nick;
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
	int	Type;
#ifdef HAVE_STDARG_H
	va_list	vlist;

	va_start(vlist, format);
#endif

	if (func == USERHOST_USERHOST || func == userhost_cmd_returned)
		Type = WHOIS_USERHOST;
	else if (func == whois_notify)
		Type = WHOIS_ISON2;
	else
		Type = WHOIS_WHOIS;
#ifdef HAVE_STDARG_H
	typed_add_to_whois_queue(Type, nick, func, format, vlist);
	va_end(vlist);
#else
	typed_add_to_whois_queue(Type, nick, func, format, arg1, arg2,
	    arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
#endif
}

void
add_ison_to_whois(nick, func)
	void	(*func) _((WhoisStuff *, char *, char *));
	char	*nick;
{
	typed_add_to_whois_queue(WHOIS_ISON, nick, func, (char *) 0, 0);
}

static void
#ifdef HAVE_STDARG_H
typed_add_to_whois_queue(int type, char *nick, void (*func) _((WhoisStuff *, char *, char *)), char *format, va_list vlist)
#else
typed_add_to_whois_queue(type, nick, func, format,
		arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
	void	(*func) _((WhoisStuff *, char *, char *));
	char	*format,
		*nick;
	int	type;
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
	char	buffer[BIG_BUFFER_SIZE + 1];
	WhoisQueue *new;
	char	*p = nick;

	if ((nick == (char *) 0) || ! is_server_connected(from_server))
		return;

	for (; *p == ' ' || *p == '\t'; p++);
	if (!*p)
		return;	/* nick should always be a non-blank string, but
			   I'd rather check because of a "ISON not enough
			   parameters" coming from the server -Sol */

	if (index(nick, '*') == (char *) 0)
	{
		new = (WhoisQueue *) new_malloc(sizeof(WhoisQueue));
		new->text = (char *) 0;
		new->nick = (char *) 0;
		new->func = func;
		new->next = (WhoisQueue *) 0;
		new->type = type;
		if (format)
		{
#ifdef HAVE_STDARG_H
			vsprintf(buffer, format, vlist);
#else
			sprintf(buffer, format, arg1, arg2, arg3, arg4, arg5,
			    arg6, arg7, arg8, arg9, arg10);
#endif
			malloc_strcpy(&(new->text), buffer);
		}
		malloc_strcpy(&(new->nick), nick);
		if ((void *) get_server_qhead(from_server) == (void *) 0)
			set_server_qhead(from_server, new);
		if (get_server_qtail(from_server))
			((WhoisQueue *) get_server_qtail(from_server))->next = new;
		set_server_qtail(from_server, new);
		switch(type)
		{
		case WHOIS_ISON:
#ifdef MONITOR_Q
			put_it("+++ ISON %s", nick);
#endif
			send_to_server("ISON %s", nick);
			break;
		case WHOIS_USERHOST:
#ifdef MONITOR_Q
			put_it("+++ USERHOST %s", nick);
#endif
			send_to_server("USERHOST %s", nick);
			break;
		case WHOIS_WHOIS:
		case WHOIS_ISON2:
#ifdef MONITOR_Q
			put_it("+++ WHOIS %s", nick);
#endif
			send_to_server("WHOIS %s", nick);
			if (!get_server_whois(from_server))
				send_to_server("WHOIS !%s", nick);
				/* postfix with nick so we know who we're
				   talking about -Sol */
				/* send "WHOIS !nick" and expect
				   "!nick: No such nick/channel" :
				   it means the real query was completed
				   and the dummy query is to be ignored
				   in no_such_nickname() -Sol */
			break;
		}
	}
}

extern	void
userhost_cmd_returned(stuff, nick, text)
	WhoisStuff	*stuff;
	char 	*nick;
	char 	*text;
{
	char	args[BIG_BUFFER_SIZE + 1];

	strcpy(args, stuff->nick ? stuff->nick : empty_string);
	strcat(args, stuff->oper ? " + " : " - ");
	strcat(args, stuff->away ? "+ " : "- ");
	strcat(args, stuff->user ? stuff->user : empty_string);
	strcat(args, " ");
	strcat(args, stuff->host ? stuff->host : empty_string);
	parse_line((char *) 0, text, args, 0, 0);
}
