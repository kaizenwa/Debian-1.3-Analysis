/*
 * ctcp.c:handles the client-to-client protocol(ctcp). 
 *
 * Written By Michael Sandrof 
 * Copyright(c) 1990, 1995 Michael Sandroff and others
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * Serious cleanup by jfn (August 1996)
 */


#include "irc.h"
#include "irc_std.h"

#ifndef WINNT
#include <pwd.h>
#endif

#ifdef HAVE_UNAME
# include <sys/utsname.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "crypt.h"
#include "ctcp.h"
#include "dcc.h"
#include "edit.h"
#include "flood.h"
#include "hook.h"
#include "if.h"
#include "ignore.h"
#include "ircaux.h"
#include "lastlog.h"
#include "list.h"
#include "names.h"
#include "output.h"
#include "parse.h"
#include "server.h"
#include "status.h"
#include "vars.h"
#include "window.h"
#include "cdcc.h"
#include "misc.h"
#include "userlist.h"
#include "bot.h"
#ifdef WANT_TCL
#include "tcl_bx.h"
#endif

extern char *last_ctcp;

static	void 	split_CTCP _((char *, char *, char *));

/*
 * ctcp_entry: the format for each ctcp function.   note that the function
 * described takes 4 parameters, a pointer to the ctcp entry, who the message
 * was from, who the message was to (nickname, channel, etc), and the rest of
 * the ctcp message.  it can return null, or it can return a malloced string
 * that will be inserted into the oringal message at the point of the ctcp.
 * if null is returned, nothing is added to the original message
 */

/* forward declarations for the built in CTCP functions */
static	char	*do_sed 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_version 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_clientinfo 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_ping 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_echo 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_userinfo 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_finger 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_time 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_atmosphere 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_dcc 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_utc 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_dcc_reply 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_ping_reply 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_bdcc 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_cinvite 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_whoami 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_ctcpops 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_ctcpunban 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_cavlink 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_botlink 	_((CtcpEntry *, char *, char *, char *));
static	char	*do_botlink_rep	_((CtcpEntry *, char *, char *, char *));


static CtcpEntry ctcp_cmd[] =
{
	{ "SED",	CTCP_SED, 	CTCP_INLINE | CTCP_NOLIMIT,
		"contains simple_encrypted_data",
		do_sed, 	do_sed },
	{ "UTC",	CTCP_UTC, 	CTCP_INLINE | CTCP_NOLIMIT,
		"substitutes the local timezone",
		do_utc, 	do_utc },
	{ "ACTION",	CTCP_ACTION, 	CTCP_SPECIAL,
		"contains action descriptions for atmosphere",
		do_atmosphere, 	do_atmosphere },

	{ "DCC",	CTCP_DCC, 	CTCP_SPECIAL | CTCP_TELLUSER,
		"requests a direct_client_connection",
		do_dcc, 	do_dcc_reply },

	{ "CDCC",	CTCP_CDCC, 	CTCP_SPECIAL | CTCP_TELLUSER,
		"checks cdcc info for you",
		do_bdcc,	NULL },
	{ "BDCC",	CTCP_CDCC1, 	CTCP_SPECIAL | CTCP_TELLUSER,
		"checks cdcc info for you",
		do_bdcc,	NULL },
	{ "XDCC",	CTCP_CDCC2, 	CTCP_SPECIAL | CTCP_TELLUSER,
		"checks cdcc info for you",
		do_bdcc,	NULL },

	{ "VERSION",	CTCP_VERSION,	CTCP_REPLY | CTCP_TELLUSER,
		"shows client type, version and environment",
		do_version, 	NULL },

	{ "CLIENTINFO",	CTCP_CLIENTINFO,CTCP_REPLY | CTCP_TELLUSER,
		"gives information about available CTCP commands",
		do_clientinfo, 	NULL },
	{ "USERINFO",	CTCP_USERINFO, 	CTCP_REPLY | CTCP_TELLUSER,
		"returns user settable information",
		do_userinfo, 	NULL },
	{ "ERRMSG",	CTCP_ERRMSG, 	CTCP_REPLY | CTCP_TELLUSER,
		"returns error messages",
		do_echo, 	NULL },
	{ "FINGER",	CTCP_FINGER, 	CTCP_REPLY | CTCP_TELLUSER,
		"shows real name, login name and idle time of user", 
		do_finger, 	NULL },
	{ "TIME",	CTCP_TIME, 	CTCP_REPLY | CTCP_TELLUSER,
		"tells you the time on the user's host",
		do_time, 	NULL },
	{ "PING", 	CTCP_PING, 	CTCP_REPLY | CTCP_TELLUSER,
		"returns the arguments it receives",
		do_ping, 	do_ping_reply },
	{ "ECHO", 	CTCP_ECHO, 	CTCP_REPLY | CTCP_TELLUSER,
		"returns the arguments it receives",
		do_echo, 	NULL },

	{ "INVITE",	CTCP_INVITE, 	CTCP_SPECIAL,
		"invite to channel specified",
		 do_cinvite, 	NULL },
	{ "WHOAMI",	CTCP_WHOAMI,	CTCP_SPECIAL,
		"user list information",
		do_whoami,	NULL },
	{ "OP",		CTCP_OPS,	CTCP_SPECIAL,
		"ops the person if on userlist",
		do_ctcpops,	NULL },
	{ "OPS",	CTCP_OPS,	CTCP_SPECIAL,
		 "ops the person if on userlist",
		do_ctcpops,	NULL },
	{ "UNBAN",	CTCP_UNBAN,	CTCP_SPECIAL,
		"unbans the person from channel",
		do_ctcpunban,	NULL },
	{ "CLINK",	CTCP_CAVLINK,	CTCP_SPECIAL | CTCP_TELLUSER,
		"hADES lame CavLink",
		do_cavlink,	NULL },
	{ "XLINK",	CTCP_BOTLINK,	CTCP_SPECIAL | CTCP_TELLUSER,
		"x-filez rule",
		do_botlink,	do_botlink_rep },

	{ NULL,		CTCP_CUSTOM,	CTCP_REPLY | CTCP_TELLUSER,
		NULL,
		NULL, NULL }
};

#ifdef WANT_DLL
CtcpEntryDll *dll_ctcp = NULL;
#endif

static char	*ctcp_type[] =
{
	"PRIVMSG",
	"NOTICE"
};

/* This is set to one if we parsed an SED */
int     sed = 0;

/*
 * in_ctcp_flag is set to true when IRCII is handling a CTCP request.  This
 * is used by the ctcp() sending function to force NOTICEs to be used in any
 * CTCP REPLY 
 */
int	in_ctcp_flag = 0;

int	not_warned = 0;

#ifdef __STDC__
#define CTCP_HANDLER(x) \
	static char * x (CtcpEntry *ctcp, char *from, char *to, char *cmd)
#else
#define CTCP_HANDLER(x) 			\
	static char *x (ctcp, from, to, cmd)	\
	CtcpEntry   *ctcp;			\
	char	    *from, *to, *cmd;
#endif

BanList *ban_is_on_channel _((char *, ChannelList *));

/**************************** CTCP PARSERS ****************************/

/********** INLINE EXPANSION CTCPS ***************/

/* parse a remote ctcp CDCC command */
CTCP_HANDLER(do_bdcc)
{
	int i;
	int secure;
extern	pack *offerlist;
extern	remote_cmd remote[];
	char *rest, *arg, *temp = NULL, *q;
	
	if ((check_ignore(from, FromUserHost, to, IGNORE_CDCC) == IGNORED))
		return NULL;
		
	if (!offerlist)
		return NULL;

	if ((secure = get_int_var(CDCC_SECURITY_VAR)))
	{
		UserList *tmp;
		if (!(tmp = lookup_userlevelc("*", FromUserHost, "*", user_list)) || tmp->level < secure)
			return NULL;
	}

	malloc_strcpy(&temp, cmd);
	q = temp;
 	
	arg = next_arg(temp, &temp);
	if (!arg) {
		new_free(&q);
		return NULL;
	}
	rest = temp;
	for (i = 0; *remote[i].name; i++) 
	{
		if (!my_stricmp(arg, remote[i].name)) 
		{
			remote[i].function(from, rest);
			new_free(&q);
			return NULL;
		}
	}
	send_to_server("NOTICE %s :try /ctcp %s cdcc help", from, nickname);
	new_free(&q);
	 
	return NULL;
}


CTCP_HANDLER(do_cavlink)
{
	if (get_int_var(CLOAK_VAR))
		return NULL;
	send_to_server("NOTICE %s :\002%s\002: hADES lamah detected", from, version);
	return NULL;
}


CTCP_HANDLER(do_botlink)
{
char *nick = NULL, *password = NULL, *port = NULL;
	nick = next_arg(cmd, &cmd);
	password = next_arg(cmd, &cmd);
	port = next_arg(cmd, &cmd);
	if (nick && password)
	{
		char *t = NULL;
		UserList *n = NULL;
		if (get_string_var(BOT_PASSWD_VAR) || (n = lookup_userlevelc(nick, FromUserHost, "*", Bot_list)))
		{
			char *pass;
			if (!n)
				pass = get_string_var(BOT_PASSWD_VAR);
			else
				pass = n->password;
			if (pass && !my_stricmp(pass, password))
			{
				if (port)
					malloc_sprintf(&t, "%s -p %s -e %s", nick, port, password);
				else
					malloc_sprintf(&t, "%s -e %s", nick, password);
				dcc_chatbot(t);
				new_free(&t);
				set_int_var(BOT_MODE_VAR, 1);
			}
		}
	}
	else
		return m_sprintf("Invalid Bot Link request %s %s %s", nick?nick:"null", password?password:"-", port?port:"*");
	return NULL;
}

CTCP_HANDLER(do_botlink_rep)
{
char *type, *description, *inetaddr, *port, *extra_flags;
	if (my_stricmp(to, get_server_nickname(from_server)))
		return NULL;

	if     (!(type = next_arg(cmd, &cmd)) ||
		!(description = next_arg(cmd, &cmd)) ||
		!(inetaddr = next_arg(cmd, &cmd)) ||
		!(port = next_arg(cmd, &cmd)))
			return NULL;

	extra_flags = next_arg(cmd, &cmd);
	set_int_var(BOT_MODE_VAR, 1);
	register_dcc_offer(from, type, description, inetaddr, port, NULL, extra_flags, FromUserHost);
	return NULL;
}

CTCP_HANDLER(do_cinvite)
{
UserList *tmp;
char *channel;
char *password = NULL;
ChannelList *chan;
int serv;
	message_from(from, LOG_CURRENT);
	put_it("%s", convert_output_format(get_string_var(is_channel(to)?FORMAT_CTCP_CLOAK_FUNC_VAR:FORMAT_CTCP_CLOAK_FUNC_USER_VAR),
		"%s %s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, to, "Invite to ", *cmd? cmd : empty_string));

	message_from(NULL, LOG_CRAP);
	channel = next_arg(cmd, &cmd);
	if (cmd && *cmd)
		password = next_arg(cmd, &cmd);
	if (is_channel(to) || !channel || (channel && *channel && !is_channel(channel)))
	{
		
		return NULL;
	}
	if (!(tmp = lookup_userlevelc(from, FromUserHost, channel, Bot_list)))
		tmp = lookup_userlevelc("*", FromUserHost, channel, user_list);
	chan = prepare_command(&serv, channel, 3);
	if (chan && tmp && tmp->level > 24 && (check_channel_match(tmp->channels, channel)))
	{
		if ((tmp->password && !password) || (tmp->password && password && strcmp(password, tmp->password)))
			return NULL;
		if (im_on_channel(channel) && is_chanop(channel, get_server_nickname(from_server)))
		{
			if (chan->key)
				my_send_to_server(serv, "NOTICE %s :\002Ctcp-inviting you to %s. Key is [%s]\002", from, channel, chan->key);
			else
				my_send_to_server(serv, "NOTICE %s :\002Ctcp-inviting you to %s\002", from, channel);
			my_send_to_server(serv, "INVITE %s %s", from, channel);
		}
		else
			my_send_to_server(serv, "NOTICE %s :\002%s\002: I'm not on %s, or I'm not opped", from, version, channel);
	} 
	else if (!get_int_var(CLOAK_VAR))
		if (!chan)
			my_send_to_server(serv, "NOTICE %s :\002%s\002: I'm not on that channel", from, version);
		else
			my_send_to_server(serv, "NOTICE %s :\002%s\002: Access Denied", from, version);
	return NULL;
}

CTCP_HANDLER(do_whoami)
{
UserList *Nick = NULL;
	message_from(from, LOG_CURRENT);
	put_it("%s", convert_output_format(get_string_var(is_channel(to)?FORMAT_CTCP_CLOAK_FUNC_VAR:FORMAT_CTCP_CLOAK_FUNC_USER_VAR), 
		"%s %s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, to, "WhoAmI", *cmd? cmd : empty_string));
	message_from(NULL, LOG_CRAP);
	if (is_channel(to))
		return NULL;

	if (!(Nick = lookup_userlevelc(from, FromUserHost, "*", Bot_list)))
		Nick = lookup_userlevelc("*", FromUserHost, "*", user_list);
	if (Nick && Nick->level)
	{
		send_to_server("NOTICE %s :\002%s\002: Userlevel - %d Protection - %d Auto-Op %s", 
			from, version, Nick->level, Nick->prot, Nick->aop ? "Yes": "No"); 
		send_to_server("NOTICE %s :\002%s\002: Host Mask - %s Channels Allowed - %s   %s", 
			from, version, Nick->host, Nick->channels, Nick->password?"Password Required":"");
	} else if (!get_int_var(CLOAK_VAR))
		send_to_server("NOTICE %s :\002%s\002: Access Denied", from, version);
	return NULL;
}

CTCP_HANDLER(do_ctcpops)
{
UserList *Nick = NULL;
char *channel;
char *password = NULL;
ChannelList *chan;
int serv;
	message_from(from, LOG_CURRENT);
	put_it("%s", convert_output_format(get_string_var(is_channel(to)?FORMAT_CTCP_CLOAK_FUNC_VAR:FORMAT_CTCP_CLOAK_FUNC_USER_VAR), 
		"%s %s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, to, "Ops on", *cmd? cmd : empty_string));
	message_from(NULL, LOG_CRAP);
	if (is_channel(to))
		return NULL;
	channel = next_arg(cmd, &cmd);
	if (cmd && *cmd)
		password = next_arg(cmd, &cmd);
	if (!(Nick = lookup_userlevelc(from, FromUserHost, channel, Bot_list)))
		Nick = lookup_userlevelc("*", FromUserHost, channel, user_list);
	chan = prepare_command(&serv, channel, 3);
	if (chan && Nick && Nick->level > 49)
	{
		if ((Nick->password && !password) || (Nick->password && password && strcmp(password, Nick->password)))
			return NULL;
		if (im_on_channel(channel) && is_chanop(channel, get_server_nickname(from_server)))
			my_send_to_server(serv, "MODE %s +o %s", channel, from); 
		else
			my_send_to_server(serv, "NOTICE %s :\002%s\002: I'm not on %s, or I'm not opped", from, version, channel);
	} else if (chan && Nick && Nick->level < 50 && Nick->level > 29)
	{
		if ((Nick->password && !password) || (Nick->password && password && strcmp(password, Nick->password)))
			return NULL;
		if (im_on_channel(channel) && is_chanop(channel, get_server_nickname(from_server)))
			my_send_to_server(serv, "MODE %s +v %s", channel, from); 
		else
			my_send_to_server(serv, "NOTICE %s :\002%s\002: I'm not on %s, or I'm not opped", from, version, channel);
	}
	else if (!get_int_var(CLOAK_VAR))
		if (!chan)
			my_send_to_server(serv, "NOTICE %s :\002%s\002: I'm not on that channel", from, version);
		else
			my_send_to_server(serv, "NOTICE %s :\002%s\002: Access Denied", from, version);
	return NULL;
}

CTCP_HANDLER(do_ctcpunban)
{
UserList *Nick = NULL;
char *channel;
char *password = NULL;
char ban[BIG_BUFFER_SIZE];
ChannelList *chan;
int server;
	message_from(from, LOG_CURRENT);
	put_it("%s", convert_output_format(get_string_var(is_channel(to)?FORMAT_CTCP_CLOAK_FUNC_VAR:FORMAT_CTCP_CLOAK_FUNC_USER_VAR), 
		"%s %s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, to, "UnBan on", *cmd? cmd : empty_string));
	message_from(NULL, LOG_CRAP);
	if (is_channel(to))
		return NULL;

	channel = next_arg(cmd, &cmd);
	if (cmd && *cmd)
		password = next_arg(cmd, &cmd);

	if (!(Nick = lookup_userlevelc(from, FromUserHost, channel, Bot_list)))
		Nick = lookup_userlevelc("*", FromUserHost, channel, user_list);
	chan = prepare_command(&server, channel, 3);

	if (chan && Nick && Nick->level > 49)
	{
		BanList *b = NULL;
		if ((Nick->password && !password) || (Nick->password && password && strcmp(password, Nick->password)))
			return NULL;
		sprintf(ban, "%s!%s", from, FromUserHost);
		if (chan && chan->chop)
		{
			if ((b = ban_is_on_channel(ban, chan)))
				my_send_to_server(server, "MODE %s -b %s", channel, b->ban); 
			else
				my_send_to_server(server, "NOTICE %s :\002%s\002: you %s are not banned on %s", from, version, ban, channel);
		} else
			my_send_to_server(server, "NOTICE %s :\002%s\002: I'm not on %s, or I'm not opped", from, version, channel);
	}
	else if (!get_int_var(CLOAK_VAR))
		if (!chan)
			my_send_to_server(server, "NOTICE %s :\002%s\002: I'm not on that channel", from, version);
		else
			my_send_to_server(server, "NOTICE %s :\002%s\002: Access Denied", from, version);
	return NULL;
}



/*
 * do_sed: Performs the Simple Encrypted Data trasfer for ctcp.  Returns in a
 * malloc string the decryped message (if a key is set for that user) or the
 * text "[ENCRYPTED MESSAGE]" 
 */
CTCP_HANDLER(do_sed)
{
	char	*key = NULL,
		*crypt_who;
	char	*ret = NULL, *ret2 = NULL;

	if (my_stricmp(to, get_server_nickname(from_server)))
		crypt_who = to;
	else
		crypt_who = from;

	if ((key = is_crypted(crypt_who)))
		ret = decrypt_msg(cmd, key);

	if (!key || !ret)
		malloc_strcpy(&ret2, "[ENCRYPTED MESSAGE]");
	else
	{
		/* 
		 * There might be a CTCP message in there,
		 * so we see if we can find it.
		 */
		ret2 = m_strdup(do_ctcp(from, to, ret));
		sed = 1;
	}

	new_free(&ret);
	return ret2;
}

CTCP_HANDLER(do_utc)
{
	if (get_int_var(CLOAK_VAR))
		return m_strdup(empty_string);
	if (!cmd || !*cmd)
		return m_strdup(empty_string);

	return m_strdup(my_ctime(my_atol(cmd)));
}


/*
 * do_atmosphere: does the CTCP ACTION command --- done by lynX
 * Changed this to make the default look less offensive to people
 * who don't like it and added a /on ACTION. This is more in keeping
 * with the design philosophy behind IRCII
 */
CTCP_HANDLER(do_atmosphere)
{
	int old;
	int ac_reply;
	
	if (!cmd || !*cmd)
		return NULL;

	if ((ac_reply = check_auto_reply(cmd)))
		addtabkey(from,1);

	old = set_lastlog_msg_level(LOG_ACTION);
	if (is_channel(to))
	{
		message_from(to, LOG_ACTION);
		if (do_hook(ACTION_LIST, "%s %s %s", from, to, cmd))
		{
			if (is_current_channel(to, from_server, 0))
				put_it("%s", convert_output_format(get_string_var(ac_reply?(lookup_userlevelc("*", FromUserHost, to, user_list)?FORMAT_ACTION_USER_AR_VAR:FORMAT_ACTION_AR_VAR):FORMAT_ACTION_VAR), "%s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, to, cmd));
			else
				put_it("%s", convert_output_format(get_string_var(ac_reply?FORMAT_ACTION_OTHER_AR_VAR:FORMAT_ACTION_OTHER_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, to, cmd));
			
		}
	}
	else
	{
		message_from(from, LOG_ACTION);
		if (do_hook(ACTION_LIST, "%s %s %s", from, to, cmd))
			put_it("%s", convert_output_format(get_string_var(FORMAT_ACTION_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, to, cmd));
	}

	message_from(NULL, LOG_CRAP);
	set_lastlog_msg_level(old);
	return NULL;
}

/*
 * do_dcc: Records data on an incoming DCC offer. Makes sure it's a
 *	user->user CTCP, as channel DCCs don't make any sense whatsoever
 */
CTCP_HANDLER(do_dcc)
{
	char	*type;
	char	*description;
	char	*inetaddr;
	char	*port;
	char	*size;
	char	*extra_flags;

	if (my_stricmp(to, get_server_nickname(from_server)))
		return NULL;

	if     (!(type = next_arg(cmd, &cmd)) ||
		!(description = next_arg(cmd, &cmd)) ||
		!(inetaddr = next_arg(cmd, &cmd)) ||
		!(port = next_arg(cmd, &cmd)))
			return NULL;

	size = next_arg(cmd, &cmd);
	extra_flags = next_arg(cmd, &cmd);

	register_dcc_offer(from, type, description, inetaddr, port, size, extra_flags, FromUserHost);
	return NULL;
}



/*************** REPLY-GENERATING CTCPS *****************/

/*
 * do_clientinfo: performs the CLIENTINFO CTCP.  If cmd is empty, returns the
 * list of all CTCPs currently recognized by IRCII.  If an arg is supplied,
 * it returns specific information on that CTCP.  If a matching CTCP is not
 * found, an ERRMSG ctcp is returned 
 */
CTCP_HANDLER(do_clientinfo)
{
	int	i;
#ifdef WANT_DLL
	CtcpEntryDll *dll = NULL;
#endif	
	if (get_int_var(CLOAK_VAR))
		return NULL;
	if (cmd && *cmd)
	{
#ifdef WANT_DLL
		for (dll = dll_ctcp; dll; dll = dll->next)
		{
			if (my_stricmp(cmd, dll->name) == 0)
			{
				send_ctcp(CTCP_NOTICE, from, CTCP_CLIENTINFO, 
					"%s %s", 
					dll->name, dll->desc?dll->desc:"none");
				return NULL;
			}
		}
#endif
		for (i = 0; i < NUMBER_OF_CTCPS; i++)
		{
			if (my_stricmp(cmd, ctcp_cmd[i].name) == 0)
			{
				send_ctcp(CTCP_NOTICE, from, CTCP_CLIENTINFO, 
					"%s %s", 
					ctcp_cmd[i].name, ctcp_cmd[i].desc);
				return NULL;
			}
		}
		send_ctcp(CTCP_NOTICE, from, CTCP_ERRMSG,
				"%s: %s is not a valid function",
				ctcp_cmd[CTCP_CLIENTINFO].name, cmd);
	}
	else
	{
		char buffer[BIG_BUFFER_SIZE + 1];
		*buffer = '\0';

		for (i = 0; i < NUMBER_OF_CTCPS; i++)
		{
			strmcat(buffer, ctcp_cmd[i].name, BIG_BUFFER_SIZE);
			strmcat(buffer, " ", BIG_BUFFER_SIZE);
		}
#ifdef WANT_DLL
		for (dll = dll_ctcp; dll; dll = dll->next)
		{
			strmcat(buffer, dll->name, BIG_BUFFER_SIZE);
			strmcat(buffer, " ", BIG_BUFFER_SIZE);
		}
#endif
		send_ctcp(CTCP_NOTICE, from, CTCP_CLIENTINFO,
			"%s :Use CLIENTINFO <COMMAND> to get more specific information", 
			buffer);
	}
	return NULL;
}

/* do_version: does the CTCP VERSION command */
CTCP_HANDLER(do_version)
{
	char	*tmp;
	char	*version_reply = NULL;
	/*
	 * The old way seemed lame to me... let's show system name and
	 * release information as well.  This will surely help out
	 * experts/gurus answer newbie questions.  -- Jake [WinterHawk] Khuon
	 *
	 * For the paranoid, UNAME_HACK hides the gory details of your OS.
	 */

#if defined(HAVE_UNAME) && !defined(UNAME_HACK)
	struct utsname un;
	char	*the_unix,
		*the_version;

	if (get_int_var(AUTOKICK_ON_VERSION_VAR))
	{
		char *channel = get_channel_by_refnum(0);
		if (channel)
		{
			ChannelList *chan;
			if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
			{
				NickList *nick;
				if ((nick = (NickList *)find_in_list((List **)&chan->nicks, from, 0)) && (!nick->chanop || !nick->voice) )
					send_to_server("KICK %s %s :%s", channel, from, "/VER is lame");
			}
		}
		return NULL;
	}
	if (get_int_var(CLOAK_VAR))
		return NULL;
	if (uname(&un) < 0)
	{
		the_version = empty_string;
		the_unix = "unknown";
	}
	else
	{
		the_version = un.release;
		the_unix = un.sysname;
	}

	malloc_strcpy(&version_reply, stripansicodes(convert_output_format(get_string_var(FORMAT_VERSION_VAR), "%s %s %s %s", irc_version, internal_version, the_unix, the_version)));
	send_ctcp(CTCP_NOTICE, from, CTCP_VERSION, "%s :%s", version_reply, 
#else
	if (get_int_var(AUTOKICK_ON_VERSION_VAR))
	{
		char *channel = get_channel_by_refnum(0);
		if (channel)
		{
			ChannelList *chan;
			if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
			{
				NickList *nick;
				if ((nick = (NickList *)find_in_list((List **)&chan->nicks, from, 0)) && (!nick->chanop || !nick->voice) )
					send_to_server("KICK %s %s :%s", channel, from, "/VER is lame");
			}
		}
		return NULL;
	}
	if (get_int_var(CLOAK_VAR))
		return NULL;
	malloc_strcpy(&version_reply, stripansicodes(convert_output_format(get_string_var(FORMAT_VERSION_VAR), "%s %s %s", irc_version, internal_version, "*IX")));
	send_ctcp(CTCP_NOTICE, from, CTCP_VERSION, "%s :%s", version_reply, 
#endif
		(tmp = get_string_var(CLIENTINFO_VAR)) ?  tmp : IRCII_COMMENT);
	return NULL;
}

/* do_time: does the CTCP TIME command --- done by Veggen */
CTCP_HANDLER(do_time)
{

	if (get_int_var(CLOAK_VAR))
		return NULL;
	send_ctcp(CTCP_NOTICE, from, CTCP_TIME, 
			"%s", my_ctime(time(NULL)));
	return NULL;
}

/* do_userinfo: does the CTCP USERINFO command */
CTCP_HANDLER(do_userinfo)
{

	if (get_int_var(CLOAK_VAR))
		return NULL;
	send_ctcp(CTCP_NOTICE, from, CTCP_USERINFO,
			"%s", get_string_var(USERINFO_VAR));
	return NULL;
}

/*
 * do_echo: does the CTCP ERRMSG and CTCP ECHO commands. Does
 * not send an error for ERRMSG and if the CTCP was sent to a channel.
 */
CTCP_HANDLER(do_echo)
{

	if (get_int_var(CLOAK_VAR))
		return NULL;
	if (!is_channel(to))
	{
		if (strlen(cmd) > 60)
		{
			bitchsay("ctcp echo request longer than 60 chars. truncating");
			cmd[60] = 0;
		}
		send_ctcp(CTCP_NOTICE, from, CTCP_ECHO, "%s", cmd);
	}
	return NULL;
}

CTCP_HANDLER(do_ping)
{

	if (get_int_var(CLOAK_VAR) == 2)
		return NULL;
	send_ctcp(CTCP_NOTICE, from, CTCP_PING, "%s", cmd ? cmd : empty_string);
	return NULL;
}


/* 
 * Does the CTCP FINGER reply 
 */
CTCP_HANDLER(do_finger)
{
	struct	passwd	*pwd;
	time_t	diff;
	char	*tmp;
	char	*ctcpuser,
		*ctcpfinger;

#ifdef WINNT
	return NULL;
#else
	if (!get_int_var(CLOAK_VAR))
		return NULL;
	diff = time(NULL) - idle_time;

	if (!(pwd = getpwuid(getuid())))
		return NULL;

#ifndef GECOS_DELIMITER
#define GECOS_DELIMITER ','
#endif

	if ((tmp = index(pwd->pw_gecos, GECOS_DELIMITER)) != NULL)
		*tmp = '\0';

/* 
 * Three optionsn for handling CTCP replies
 *  + Fascist Bastard Way -- normal, non-hackable fashion
 *  + Winterhawk way (default) -- allows hacking through IRCUSER and
 *	IRCFINGER environment variables
 *  + hop way -- returns a blank always
 */
	/* 
	 * It would be pretty pointless to allow for customisable
	 * usernames if they can track via IRCNAME from the
	 * /etc/passwd file...  We therefore need to either disable
	 * CTCP_FINGER or also make it customisable.  Let's do the
	 * latter because it invokes less suspicion in the long run
	 *				-- Jake [WinterHawk] Khuon
	 */
	if ((ctcpuser = getenv("IRCUSER"))) 
		strmcpy(pwd->pw_name, ctcpuser, NAME_LEN);
	if ((ctcpfinger = getenv("IRCFINGER"))) 
		strmcpy(pwd->pw_gecos, ctcpfinger, NAME_LEN);

	send_ctcp(CTCP_NOTICE, from, CTCP_FINGER, 
		"%s (%s@%s) Idle %ld second%s", 
		pwd->pw_gecos, pwd->pw_name, hostname, diff, plural(diff));
	return NULL;
#endif
}


/* 
 * If we recieve a CTCP DCC REJECT in a notice, then we want to remove
 * the offending DCC request
 */
CTCP_HANDLER(do_dcc_reply)
{
	char *subcmd = NULL;
	char *type = NULL;

	if (is_channel(to))
		return NULL;

	if (cmd && *cmd)
		subcmd = next_arg(cmd, &cmd);
	if (cmd && *cmd)
		type = next_arg(cmd, &cmd);

	if (subcmd && type && cmd && !strcmp(subcmd, "REJECT"))
		dcc_reject (from, type, cmd);

	return NULL;
}


/*
 * Handles CTCP PING replies.
 */
CTCP_HANDLER(do_ping_reply)
{
	char *sec, *usec = NULL;
	struct timeval t;
	time_t tsec = 0, tusec = 0;

	if (!cmd || !*cmd)
		return NULL;		/* This is a fake -- cant happen. */

	gettimeofday(&t, NULL);

       /* We've already checked 'cmd' here, so its safe. */
        sec = cmd;
	tsec = t.tv_sec - my_atol(sec);
        
	if ((usec = index(sec, ' ')))
	{
		*usec++ = 0;
		tusec = t.tv_usec - my_atol(usec);
	}
                        
	/*
	 * 'cmd', a variable passed in from do_notice_ctcp()
	 * points to a buffer which is MUCH larger than the
	 * string 'cmd' points at.  So this is safe, even
	 * if it looks "unsafe".
	 */
	sprintf(cmd, "%5.3f seconds", (float)(tsec + (tusec / 1000000.0)));
	return NULL;
}


/************************************************************************/
/*
 * do_ctcp: a re-entrant form of a CTCP parser.  The old one was lame,
 * so i took a hatchet to it so it didnt suck.
 */
#ifdef __STDC__
extern 	char *do_ctcp (char *from, char *to, char *str)
#else
extern	char *do_ctcp (from, to, str)
	char *from, *to, *str;
#endif
{
	int 	flag;
	int	lastlog_level;
	char 	local_ctcp_buffer [BIG_BUFFER_SIZE + 1],
		the_ctcp          [IRCD_BUFFER_SIZE + 1],
		last              [IRCD_BUFFER_SIZE + 1];
	char	*ctcp_command,
		*ctcp_argument;
	int	i;
	char	*ptr = NULL;
	int	allow_ctcp_reply = 1;
#ifdef WANT_DLL
	CtcpEntryDll  *dll = NULL;
#endif
	int delim_char = charcount(str, CTCP_DELIM_CHAR);

	if (delim_char < 2)
		return str;             /* No CTCPs. */
	if (delim_char > 8)
		allow_ctcp_reply = 0;   /* Historical limit of 4 CTCPs */

	flag = check_ignore(from, FromUserHost, to, IGNORE_CTCPS);          

	if (!in_ctcp_flag)
		in_ctcp_flag = 1;
	allow_ctcp_reply = 1;
	strmcpy(local_ctcp_buffer, str, IRCD_BUFFER_SIZE);

	for (;;strmcat(local_ctcp_buffer, last, IRCD_BUFFER_SIZE))
	{
		split_CTCP(local_ctcp_buffer, the_ctcp, last);

		if (!*the_ctcp)
			break;		/* all done! */
		/*
		 * Apply some integrety rules:
		 * -- If we've already replied to a CTCP, ignore it.
		 * -- If user is ignoring sender, ignore it.
		 * -- If we're being flooded, ignore it.
		 * -- If CTCP was a global msg, ignore it.
		 */

		/*
		 * Yes, this intentionally ignores "unlimited" CTCPs like
		 * UTC and SED.  Ultimately, we have to make sure that
		 * CTCP expansions dont overrun any buffers that might
		 * contain this string down the road.  So by allowing up to
		 * 4 CTCPs, we know we cant overflow -- but if we have more
		 * than 40, it might overflow, and its probably a spam, so
		 * no need to shed tears over ignoring them.  Also makes
		 * the sanity checking much simpler.
		 */
		if (!allow_ctcp_reply)
			continue;

		/*
		 * Check to see if the user is ignoring person.
		 */

		if (flag == IGNORED)
		{
			allow_ctcp_reply = 0;
			continue;
		}
					
		ctcp_command = the_ctcp;
		ctcp_argument = index(the_ctcp, ' ');
		if (ctcp_argument)
			*ctcp_argument++ = 0;
		else
			ctcp_argument = empty_string;

		/* Global messages -- just drop the CTCP */
		if (*to == '$' || (*to == '#' && !lookup_channel(to, from_server, 0)))
		{
			allow_ctcp_reply = 0;
			continue;
		}
#ifdef WANT_DLL
		/* Find the correct CTCP and run it. */
		for (dll = dll_ctcp; dll; dll = dll->next)
			if (!strcmp(dll->name, ctcp_command))
				break;  			
#endif
		for (i = 0; i < NUMBER_OF_CTCPS; i++)
			if (!strcmp(ctcp_command, ctcp_cmd[i].name))
				break;

#ifdef WANT_DLL
		if (!dll && ctcp_cmd[i].id == CTCP_ACTION)
			check_flooding(from, CTCP_ACTION_FLOOD, ctcp_argument, is_channel(to)?to:NULL);
		else if (!dll && ctcp_cmd[i].id == CTCP_DCC)
			check_flooding(from, CTCP_FLOOD, ctcp_argument, is_channel(to)?to:NULL);
#else
		if (ctcp_cmd[i].id == CTCP_ACTION)
			check_flooding(from, CTCP_ACTION_FLOOD, ctcp_argument, is_channel(to)?to:NULL);
		else if (ctcp_cmd[i].id == CTCP_DCC)
			check_flooding(from, CTCP_FLOOD, ctcp_argument, is_channel(to)?to:NULL);
#endif
		else
		{
			check_flooding(from, CTCP_FLOOD, ctcp_argument, is_channel(to)?to:NULL);
			if (get_int_var(NO_CTCP_FLOOD_VAR) && (time(NULL) - server_list[from_server].ctcp_last_reply_time < get_int_var(CTCP_DELAY_VAR))/* && ctcp_cmd[i].id !=CTCP_DCC*/)
			{
				if (get_int_var(FLOOD_WARNING_VAR))
					put_it("%s", convert_output_format(get_string_var(FORMAT_FLOOD_VAR), "%s %s %s %s %s", update_clock(GET_TIME),ctcp_command,from, FromUserHost, to));
				time(&server_list[from_server].ctcp_last_reply_time);
				allow_ctcp_reply = 0;
				continue;
			}
		}
		/* Did the CTCP search work? */
#ifdef WANT_DLL
		if (i == NUMBER_OF_CTCPS && !dll)
#else
		if (i == NUMBER_OF_CTCPS)
#endif
		{
			/*
			 * Offer it to the user.
			 * Maybe they know what to do with it.
			 */
#ifdef WANT_TCL
			if (check_tcl_ctcp(from, FromUserHost, from, to, ctcp_command, ctcp_argument))
				continue;
#endif

			if (do_hook(CTCP_LIST, "%s %s %s %s", from, to, ctcp_command, ctcp_argument))
			{
				if (allow_ctcp_reply && get_int_var(VERBOSE_CTCP_VAR))
					if (lookup_userlevelc("*", FromUserHost, "*", user_list))
						put_it("%s", convert_output_format(get_string_var(get_int_var(CLOAK_VAR)? FORMAT_CTCP_CLOAK_UNKNOWN_USER_VAR:FORMAT_CTCP_UNKNOWN_USER_VAR),
							"%s %s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, to, ctcp_command, *ctcp_argument? ctcp_argument : empty_string));
					else
						put_it("%s", convert_output_format(get_string_var(get_int_var(CLOAK_VAR)? FORMAT_CTCP_CLOAK_UNKNOWN_VAR:FORMAT_CTCP_UNKNOWN_VAR),
							"%s %s %s %s %s %s",update_clock(GET_TIME), from, FromUserHost, to, ctcp_command, *ctcp_argument? ctcp_argument : empty_string));
			}
			allow_ctcp_reply = 0;
			continue;
		}
#ifdef WANT_DLL
		if (dll)
			ptr = (dll->func) (dll, from, to, ctcp_argument);
		else
#endif
			ptr = ctcp_cmd[i].func(ctcp_cmd + i, from, to, ctcp_argument);

#ifdef WANT_DLL
		if (!(ctcp_cmd[i].flag & CTCP_NOLIMIT) || (dll && !(dll->flag & CTCP_NOLIMIT)))
#else
		if (!(ctcp_cmd[i].flag & CTCP_NOLIMIT))
#endif
		{
			time(&server_list[from_server].ctcp_last_reply_time);
			allow_ctcp_reply = 0;
		}

		/*
		 * We've only gotten to this point if its a valid CTCP
		 * query and we decided to parse it.
		 */

		/* If its an inline CTCP paste it back in */
#ifdef WANT_DLL
		if ((ctcp_cmd[i].flag & CTCP_INLINE) || (dll && (dll->flag & CTCP_INLINE)))
			strmcat(local_ctcp_buffer, ptr, BIG_BUFFER_SIZE);
#else
		if ((ctcp_cmd[i].flag & CTCP_INLINE))
			strmcat(local_ctcp_buffer, ptr, BIG_BUFFER_SIZE);
#endif
		/* If its interesting, tell the user. */
#ifdef WANT_DLL
		if ((ctcp_cmd[i].flag & CTCP_TELLUSER) || (dll && (dll->flag & CTCP_TELLUSER)))
#else
		if ((ctcp_cmd[i].flag & CTCP_TELLUSER))
#endif
		{
			if (do_hook(CTCP_LIST, "%s %s %s %s", from, to, ctcp_command, ctcp_argument))
			{
				/* Set up the window level/logging */
				lastlog_level = set_lastlog_msg_level(LOG_CTCP);
				message_from(from, LOG_CTCP);

				if (get_int_var(VERBOSE_CTCP_VAR))
					put_it("%s", convert_output_format(get_string_var((!lookup_userlevelc("*",FromUserHost, "*", user_list))? get_int_var(CLOAK_VAR)?FORMAT_CTCP_CLOAK_VAR:FORMAT_CTCP_VAR:get_int_var(CLOAK_VAR)?FORMAT_CTCP_CLOAK_USER_VAR:FORMAT_CTCP_USER_VAR),
						"%s %s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, to, ctcp_command, *ctcp_argument? ctcp_argument : empty_string));
				/* Reset the window level/logging */
				message_from(NULL, LOG_CTCP);
				set_lastlog_msg_level(lastlog_level);
			}
		}

		new_free(&ptr);
	}

	if (in_ctcp_flag == 1)
		in_ctcp_flag = 0;
	return strcpy(str, local_ctcp_buffer);
}



/*
 * do_notice_ctcp: a re-entrant form of a CTCP reply parser.
 */
#ifdef __STDC__
extern 	char *do_notice_ctcp (char *from, char *to, char *str)
#else
extern	char *do_notice_ctcp (from, to, str)
	char *from, *to, *str;
#endif
{
	int 	flag;
	int	lastlog_level;
	char 	local_ctcp_buffer [BIG_BUFFER_SIZE + 1],
		the_ctcp          [IRCD_BUFFER_SIZE + 1],
		last              [IRCD_BUFFER_SIZE + 1];
	char	*ctcp_command,
		*ctcp_argument;
	int	i;
	char	*ptr;
	int	allow_ctcp_reply = 1;

#ifdef WANT_DLL
	CtcpEntryDll  *dll = NULL;
#endif

	int delim_char = charcount(str, CTCP_DELIM_CHAR);

	if (delim_char < 2)
		return str;		/* No CTCPs. */
	if (delim_char > 8)
		allow_ctcp_reply = 0;	/* Ignore all the CTCPs. */

	flag = check_ignore(from, FromUserHost, to, IGNORE_CTCPS);
	if (!in_ctcp_flag)
		in_ctcp_flag = -1;
	strmcpy(local_ctcp_buffer, str, IRCD_BUFFER_SIZE);

	for (;;strmcat(local_ctcp_buffer, last, BIG_BUFFER_SIZE))
	{
		split_CTCP(local_ctcp_buffer, the_ctcp, last);
		if (!*the_ctcp)
			break;		/* all done! */

		if (!allow_ctcp_reply)
			continue;
			
		if (flag == IGNORED)
		{
			allow_ctcp_reply = 0;
			continue;
		}

		/* Global messages -- just drop the CTCP */
		if (*to == '$' || (*to == '#' && !lookup_channel(to, from_server, 0)))
		{
			allow_ctcp_reply = 0;
			continue;
		}

		ctcp_command = the_ctcp;
		ctcp_argument = index(the_ctcp, ' ');
		if (ctcp_argument)
			*ctcp_argument++ = 0;
		else
			ctcp_argument = empty_string;
		
#ifdef WANT_DLL
		/* Find the correct CTCP and run it. */
		for (dll = dll_ctcp; dll; dll = dll->next)
			if (!strcmp(dll->name, ctcp_command))
				break;  			
#endif
		/* Find the correct CTCP and run it. */
		for (i = 0; i < NUMBER_OF_CTCPS; i++)
			if (!strcmp(ctcp_command, ctcp_cmd[i].name))
				break;

		/* 
		 * If we've already parsed one, and there is a limit
		 * on this CTCP, then just punt it.
		 */
#ifdef WANT_DLL
		if (i < NUMBER_OF_CTCPS && !dll && ctcp_cmd[i].repl)
#else
		if (i < NUMBER_OF_CTCPS && ctcp_cmd[i].repl)
#endif
		{
			if ((ptr = ctcp_cmd[i].repl(ctcp_cmd + i, from, to, ctcp_argument)))
			{
				strmcat(local_ctcp_buffer, ptr, BIG_BUFFER_SIZE);
				new_free(&ptr);
				continue;
			}
		}
#ifdef WANT_DLL
		if (dll && dll->repl)
		{
			if ((ptr = dll->repl(dll, from, to, ctcp_argument)))
			{
				strmcat(local_ctcp_buffer, ptr, BIG_BUFFER_SIZE);
				new_free(&ptr);
				continue;
			}
		}
#endif
#ifdef WANT_TCL
		check_tcl_ctcr(from, FromUserHost, from, to, ctcp_command, ctcp_argument?ctcp_argument:"");
#endif
		/* Toss it at the user.  */
		if (do_hook(CTCP_REPLY_LIST, "%s %s %s", from, ctcp_command, ctcp_argument))
		{
			/* Set up the window level/logging */
			lastlog_level = set_lastlog_msg_level(LOG_CTCP);
			message_from(from, LOG_CTCP);

			put_it("%s", convert_output_format(get_string_var(FORMAT_CTCP_REPLY_VAR),"%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, ctcp_command, ctcp_argument));
			malloc_strcpy(&last_ctcp, convert_output_format(get_string_var(FORMAT_CTCP_REPLY_VAR),"%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, ctcp_command, ctcp_argument));
			
			/* Reset the window level/logging */
			message_from(NULL, LOG_CTCP);
			set_lastlog_msg_level(lastlog_level);
		}

		allow_ctcp_reply = 0;
	}

	if (in_ctcp_flag == -1)
		in_ctcp_flag = 0;

	return strcpy(str, local_ctcp_buffer);
}



/* in_ctcp: simply returns the value of the ctcp flag */
extern int	in_ctcp _((void)) { return (in_ctcp_flag); }



/*
 * This is no longer directly sends information to its target.
 * As part of a larger attempt to consolidate all data transmission
 * into send_text, this function was modified so as to use send_text().
 * This function can send both direct CTCP requests, as well as the
 * appropriate CTCP replies.  By its use of send_text(), it can send
 * CTCPs to DCC CHAT and irc nickname peers, and handles encryption
 * transparantly.  This greatly reduces the logic, complexity, and
 * possibility for error in this function.
 */
#if defined(__STDC__) && defined(HAVE_STDARG_H)
extern	void	send_ctcp _((int type, char *to, int datatag, char *format, ...))
#else
extern	void	send_ctcp(type, to, datatag, format, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	int	type, datatag;
	char 	*to, *format,
     		*arg0, *arg1, *arg2, *arg3, *arg4, 
		*arg5, *arg6, *arg7, *arg8, *arg9;
#endif
{
	char putbuf [BIG_BUFFER_SIZE + 1],
	     putbuf2[BIG_BUFFER_SIZE + 1];

	if (in_on_who)
		return;

	if (format)
	{
#if defined(__STDC__) && defined(HAVE_STDARG_H)
		va_list args;
		va_start(args, format);
		vsprintf(putbuf, format, args);
		va_end(args);
#else
		sprintf(putbuf, format, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
#endif

		sprintf(putbuf2, "%c%s %s%c", CTCP_DELIM_CHAR, ctcp_cmd[datatag].name, putbuf, CTCP_DELIM_CHAR);
	}
	else
		sprintf(putbuf2, "%c%s%c", CTCP_DELIM_CHAR, ctcp_cmd[datatag].name, CTCP_DELIM_CHAR);


	send_text(to, putbuf2, ctcp_type[type], 0, 0);
}


/*
 * quote_it: This quotes the given string making it sendable via irc.  A
 * pointer to the length of the data is required and the data need not be
 * null terminated (it can contain nulls).  Returned is a malloced, null
 * terminated string.   
 */
extern 	char	*ctcp_quote_it _((char *str, int len))
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	char	*ptr;
	int	i;

	ptr = buffer;
	for (i = 0; i < len; i++)
	{
		switch (str[i])
		{
			case CTCP_DELIM_CHAR:	*ptr++ = CTCP_QUOTE_CHAR;
						*ptr++ = 'a';
						break;
			case '\n':		*ptr++ = CTCP_QUOTE_CHAR;
						*ptr++ = 'n';
						break;
			case '\r':		*ptr++ = CTCP_QUOTE_CHAR;
						*ptr++ = 'r';
						break;
			case CTCP_QUOTE_CHAR:	*ptr++ = CTCP_QUOTE_CHAR;
						*ptr++ = CTCP_QUOTE_CHAR;
						break;
			case '\0':		*ptr++ = CTCP_QUOTE_CHAR;
						*ptr++ = '0';
						break;
			default:		*ptr++ = str[i];
						break;
		}
	}
	*ptr = '\0';
	return m_strdup(buffer);
}

/*
 * ctcp_unquote_it: This takes a null terminated string that had previously
 * been quoted using ctcp_quote_it and unquotes it.  Returned is a malloced
 * space pointing to the unquoted string.  NOTE: a trailing null is added for
 * convenied, but the returned data may contain nulls!.  The len is modified
 * to contain the size of the data returned. 
 */
#ifdef __STDC__
extern	char	*ctcp_unquote_it (char *str, int *len)
#else
extern	char	*ctcp_unquote_it(str, len)
	char	*str;
	int	*len;
#endif
{
	char	*buffer;
	char	*ptr;
	char	c;
	int	i,
		new_size = 0;

	buffer = (char *) new_malloc(sizeof(char) * *len);
	ptr = buffer;
	i = 0;
	while (i < *len)
	{
		if ((c = str[i++]) == CTCP_QUOTE_CHAR)
		{
			switch (c = str[i++])
			{
				case CTCP_QUOTE_CHAR:
					*ptr++ = CTCP_QUOTE_CHAR;
					break;
				case 'a':
					*ptr++ = CTCP_DELIM_CHAR;
					break;
				case 'n':
					*ptr++ = '\n';
					break;
				case 'r':
					*ptr++ = '\r';
					break;
				case '0':
					*ptr++ = '\0';
					break;
				default:
					*ptr++ = c;
					break;
			}
		}
		else
			*ptr++ = c;
		new_size++;
	}
	*ptr = '\0';
	*len = new_size;
	return (buffer);
}

int get_ctcp_val (char *str)
{
	int i;

	for (i = 0; i < NUMBER_OF_CTCPS; i++)
		if (!strcmp(str, ctcp_cmd[i].name))
			return i;

	/*
	 * This is *dangerous*, but it works.  The only place that
	 * calls this function is edit.c:ctcp(), and it immediately
	 * calls send_ctcp().  So the pointer that is being passed
	 * to us is globally allocated at a level higher then ctcp().
	 * so it wont be bogus until some time after ctcp() returns,
	 * but at that point, we dont care any more.
	 */
	ctcp_cmd[CTCP_CUSTOM].name = str;
	return CTCP_CUSTOM;
}



/*
 * XXXX -- some may call this a hack, but if youve got a better
 * way to handle this job, id love to use it.
 */
void split_CTCP(char *raw_message, char *ctcp_dest, char *after_ctcp)
{
	char *ctcp_start, *ctcp_end;

	*ctcp_dest = *after_ctcp = 0;
	ctcp_start = index(raw_message, CTCP_DELIM_CHAR);
	if (!ctcp_start)
		return;		/* No CTCPs present. */

	*ctcp_start++ = 0;
	ctcp_end = index(ctcp_start, CTCP_DELIM_CHAR);
	if (!ctcp_end)
	{
		*--ctcp_start = CTCP_DELIM_CHAR;
		return;		/* Thats _not_ a CTCP. */
	}

	*ctcp_end++ = 0;
	strmcpy(ctcp_dest, ctcp_start, IRCD_BUFFER_SIZE);
	strmcpy(after_ctcp, ctcp_end, IRCD_BUFFER_SIZE);

	return;		/* All done! */
}
