/*
 * Copyright Colten Edwards. Oct 96
 */
 
#include "irc.h"
#include "ircaux.h"
#include "bot.h"
#include "dcc.h"
#include "server.h"
#include "output.h"
#include "list.h"
#include "whois.h"
#include "ctcp.h"
#include "tcl_bx.h"
#include "status.h"
#include "vars.h"
#include "misc.h"
#include "userlist.h"

static int clink_active = 0;
static	int xlink_commands = 0;

int send_who _((int, char *));
int send_whom _((int, char *));
int tell_who _((int, char *));
int tell_whom _((int, char *));

int tand_zapf _((int, char *));
int tand_zapfbroad _((int, char *));
int tand_chan _((int, char *));
int tand_join _((int, char *));
int tand_part _((int, char *));
int tand_who _((int, char *));
int tand_whom _((int, char *));
int tand_priv _((int, char *));
int tand_boot _((int, char *));
int tand_privmsg _((int, char *));
int tand_command _((int, char *));

int cmd_act _((int, char *));
int cmd_msg _((int, char *));
int cmd_say _((int, char *));
int send_who _((int, char *));
int send_whom _((int, char *));
int msg_die _((int, char *));
int cmd_tcl _((int, char *));
int cmd_boot _((int, char *));
int cmd_cmsg _((int, char *));
int cmd_help _((int, char *));
int cmd_chat _((int, char *));
int cmd_quit _((int, char *));
int cmd_invite _((int, char *));
int cmd_echo _((int, char *));
int send_command _((int, char *));

#ifndef WANT_TCL
#define TCL_OK 0
#define TCL_ERROR 1
#else
extern Tcl_Interp *interp;
#endif

extern cmd_t C_msg[];

cmd_t C_dcc[] =
{
	{ "act",	cmd_act,	 0,	"Perform action on a channel"},
	{ "boot",	cmd_boot,	 1,	"boot user off the botnet" },
	{ "chat",	cmd_chat,	 0,	"add you to the chat network" },
	{ "cmsg",	cmd_cmsg,	 0,	"send a privmsg to someone on botnet" },
	{ "echo",	cmd_echo,	 0,	"turn echo on/off" },
	{ "help",	cmd_help,	 0,	"help information [option cmd]" },
	{ "invite",	cmd_invite,	 0,	"invite <nick> to the chat network" },
	{ "msg",	cmd_msg,	 40,	"send msg to someone" },
	{ "quit",	cmd_quit,	 0,	"remove from chat network" },
	{ "say",	cmd_say,	 50,	"say something on a channel" },
	{ "tcl",	cmd_tcl,	 100,	"set a tcl variable" },
	{ "who",	send_who,	 0,	"find out who is on [option botnick]" },
	{ "whom",	send_whom,	 0,	"find out who is on the botnet. global" },
	{ "xlink",	send_command,	 0,	"send command to all on link" },
	{ NULL,		NULL,		-1,	NULL} 
};

cmd_t C_tand[] =
{
	{ "zapf",	tand_zapf,	 0,	NULL },
	{ "zapf-broad", tand_zapfbroad,  0,	NULL },
	{ "chan",	tand_chan,	 0,	NULL },
	{ "part",	tand_part,	 0,	NULL },
	{ "join",	tand_join,	 0,	NULL },
	{ "who",	tand_who,	 0,	NULL },
	{ "whom",	tand_whom,	 0,	NULL },
	{ "priv",	tand_priv,	 0,	NULL },
	{ "boot",	tand_boot,	 0,	NULL },
	{ "privmsg",	tand_privmsg,	 0,	NULL },
	{ "clink_com",	tand_command,	 0,	NULL },
	{ NULL,		NULL,		-1,	NULL }
};

int dcc_printf(int idx, char *format, ...)
{
va_list args;
char putbuf[BIG_BUFFER_SIZE+1];
int len = 0;
	va_start(args, format);
	vsprintf(putbuf, format, args);
	va_end(args);
	if (strlen(putbuf) > 510)
		putbuf[510] = '\n';
	putbuf[511] = '\0';
	if (idx == -1)
		put_it("%s", chop(putbuf, 1));
	else
	{
		len = strlen(putbuf);
		send(idx, putbuf, len, 0);
	}
	return len;
}

void tandout_but(int idx, char *format, ...)
{
va_list args;
char putbuf[BIG_BUFFER_SIZE+1];
DCC_list *Client = ClientList;

	va_start(args, format);
	vsprintf(putbuf, format, args);
	va_end(args);

	while (Client)
	{
		if (idx != Client->read && Client->read != -1 && Client->flags & DCC_ACTIVE)
		{
			if ((Client->flags & DCC_TYPES) == DCC_BOTMODE)
				send(Client->read, putbuf, strlen(putbuf), 0);
		}
		Client = Client->next;
	}
}

void chanout_but(int idx, char *format, ...)
{
va_list args;
char putbuf[BIG_BUFFER_SIZE+1];
DCC_list *Client = ClientList;
	va_start(args, format);
	vsprintf(putbuf, format, args);
	va_end(args);

	while (Client)
	{
		if (((Client->flags & DCC_TYPES) == DCC_CHAT) && idx != Client->read && Client->read != -1 && Client->flags & DCC_ACTIVE && Client->in_dcc_chat)
		{
			send(Client->read, putbuf, strlen(putbuf), 0);
		} else if (idx == Client->read && Client->echo)
			send(Client->read, putbuf, strlen(putbuf), 0);
		Client = Client->next;
	}
}

void csay _((char *command, char *args, char *subargs))
{
DCC_list *tmp;
int found = 0;
	if (args && *args)
	{
		char *t = NULL;
		for (tmp = ClientList; tmp; tmp = tmp->next)
		{
			if (tmp->write != -1 && tmp->flags & DCC_ACTIVE)
			{
				if ((tmp->flags & DCC_TYPES) == DCC_BOTMODE)
				{
					tmp->bytes_sent += dcc_printf(tmp->write, "chan %s %d %s@%s %s\n", nickname, 0, nickname, nickname, args);
					found++;
				}
				else if ((tmp->flags & DCC_TYPES) == DCC_CHAT && tmp->in_dcc_chat)
				{
					tmp->bytes_sent += dcc_printf(tmp->write, "[%s@%s] %s\n", nickname, nickname, args);
					found++;
				}
			}
		}
		new_free(&t);

		if (!found)
			bitchsay("No Active bot link");
		else

			put_it("%s", convert_output_format("%K(%R$1%K(%rxlink%K))%n $2-", "%s %s %s", update_clock(GET_TIME), nickname, args));
	} else 
		userage("Csay", "<text>");
}

void cwho _((char *command, char *args, char *subargs))
{
DCC_list *tmp;
int whom = 0;
	whom = !my_stricmp(command, "cwhom");
	whom ? tell_whom(-1, NULL) : tell_who(-1, NULL);
	for (tmp = ClientList; tmp; tmp = tmp->next)
	{
		if (tmp->write != -1 && tmp->flags & DCC_ACTIVE)
		{
			if ((tmp->flags & DCC_TYPES) == DCC_BOTMODE)
			{
				if (whom)
					dcc_printf(tmp->read, "whom %d:%s@%s %s %d\n",
						-1, nickname, nickname, args?args:"", 0);
				else			
					send_who(-1, "");
			}
		}
	}
}

void cboot _((char *command, char *args, char *subargs))
{
DCC_list *tmp;
char *nick, *reason;
	nick = next_arg(args, &args);
	reason = args ? args: "We don't like you";
	if (!nick)
	{
		userage("Cboot", "[nick] <reason>");
		return;
	}
	for (tmp = ClientList; tmp; tmp = tmp->next)
	{
		if (tmp->write != -1 && tmp->flags & DCC_ACTIVE)
		{
			if ((tmp->flags & DCC_TYPES) == DCC_CHAT && tmp->in_dcc_chat)
			{
				dcc_printf(tmp->write, ".boot %s %s %s\n", nickname, nick, reason);
			}
		}
	}
}

void toggle_xlink _((char *commands, char *args, char *subargs))
{
	xlink_commands ^= 1;
	put_it("X-link commands toggled %s", on_off(xlink_commands));
}

void cmsg _((char *command, char *args, char *subargs))
{
DCC_list *tmp;
char *nick, *reason;
	nick = next_arg(args, &args);
	reason = args;
	if (!nick)
	{
		userage("Cmsg", "[nick] <mesg>");
		return;
	}
	for (tmp = ClientList; tmp; tmp = tmp->next)
	{
		if (tmp->write != -1 && tmp->flags & DCC_ACTIVE)
		{
			if ((tmp->flags & DCC_TYPES) == DCC_CHAT && tmp->in_dcc_chat)
				dcc_printf(tmp->write, ".cmsg %s %s\n", nick, reason);
		}
	}
}

void userhost_clink(WhoisStuff *stuff, char *nick, char *args)
{
	if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick))
	{
		bitchsay("No information for %s found", nick);
		return;
	}
	bitchsay("Attempting Clink to %s!%s@%s", nick, stuff->user, stuff->host);
	send_ctcp(CTCP_PRIVMSG, nick, CTCP_BOTLINK, "%s", args);
	clink_active++;
}

void clink _((char *command, char *args, char *subargs))
{
char *nick;
NickList *Nick = NULL;
ChannelList *chan;
int i = 0;
	nick = next_arg(args, &args);
	if (nick)
	{
		for (i = 0; i < number_of_servers; i++)
			for (chan = server_list[i].chan_list; chan; chan = chan->next)
				if ((Nick = (NickList *) find_in_list((List **)chan->nicks, nick, 0)))
					break;
		if (Nick && Nick->botlist)
		{
			bitchsay("Attempting clink with %s!%s", Nick->nick, Nick->host);
			send_ctcp(CTCP_PRIVMSG, nick, CTCP_BOTLINK, "%s", args);
			clink_active++;
		}
		else	
			add_to_whois_queue(nick, userhost_clink, "%s", args);
	}
	else
		userage("Clink", "nick <port> <password>");
}

int handle_tcl_chan(int idx, char *user, char *host, char *text)
{
	tandout_but(idx, "chan %s %d %s@%s %s\n", nickname, 0, user, nickname, text);
	chanout_but(idx, "[%s@%s] %s\n", user, nickname, text);
	put_it("%s", convert_output_format("%K(%R$1%K(%rxlink%K))%n $2-", "%s %s %s", update_clock(GET_TIME), user, text));
	return 0;
}

int tand_chan(int idx, char *par)
{
char *bot;
char *chan;
char *who;
char *uhost;
/*
	 tand_chan (idx=7, par=0x8140009 "panasync 0 sabina@botnick testing\r")
*/
	bot = next_arg(par, &par);
	chan = next_arg(par, &par);
	who = next_arg(par, &par);
	if (!who || !chan || !bot)
		return 0;
	tandout_but(idx, "chan %s %d %s %s\n", nickname, 0, who, par);
	chanout_but(idx, "%s %s\n", who, par);
	if ((uhost = strchr(who, '@')))
		*uhost++ = '\0';
	put_it("%s", convert_output_format("%K[%RxLink%Y$1%K(%r$2%K)] %n$3-", "%s %s %s %s", update_clock(GET_TIME), who, uhost?uhost:"u@h", par));
	return 0;
}

int tand_zapf(int idx, char *par)
{
char *from, *to; 
DCC_list *Client;
#ifdef WANT_TCL
char *opcode;
#endif
	context;
	from = next_arg(par, &par);
	to = next_arg(par, &par);
	Client = dcc_searchlist("chat", from, DCC_BOTMODE, 0, NULL, NULL, 1);
	if (!Client || Client->read != idx)
		return 0;	
#ifdef WANT_TCL
	if (!my_stricmp(to, get_server_nickname(from_server)))
	{
		opcode = next_arg(par, &par);
		check_tcl_tand(from, opcode, par);
		return 0;
	}
#endif
	Client = dcc_searchlist("chat", to, DCC_BOTMODE, 0, NULL, NULL, 1);
	if (!Client)
		tandout_but(idx, "zapf %s %s %s\n", from, to, par);
	else 
		dcc_printf(Client->read, "zapf %s %s %s\n", from, to, par);
	return 0;
}

/* used to send a global msg from Tcl on one bot to every other bot */
/* zapf-broad <frombot> <code [param]> */
int tand_zapfbroad(int idx, char *par)
{
	char *from, *opcode;

	context;
	from = next_arg(par, &par);
	opcode = next_arg(par, &par);

#ifdef WANT_TCL
	check_tcl_tand(from, opcode, par);
#endif
	tandout_but(idx, "zapf-broad %s %s %s\n", from, opcode, par);
	return 0;
}


int handle_dcc_bot(int idx, char *param)
{
char *code;
int i = 0;
int found = 0;

	context;
	code = next_arg(param, &param);
	if (!code)
		return 0;
	while (C_tand[i].access != -1)
	{
		if (!my_stricmp(code, C_tand[i].name))
		{
			(C_tand[i].func)(idx, param);
			found = 1;
			break;
		}
		i++;
	}
	return found;
}


int tand_join (int idx, char *args)
{
char *bot, *nick;
	bot = next_arg(args, &args);
	nick = next_arg(args, &args);
	tandout_but(idx, "join %s %s %d\n", bot, nick, 0);
	chanout_but(idx, "[%s@%s] has entered the TwilightZone\n", nick, bot);
	put_it("%s", convert_output_format("%K[%RxLink%Y$1%K(%r$2%K)] %n$3-", "%s %s %s %s", update_clock(GET_TIME), nick, bot, "entered the TwilightZone"));
	return 0;
}

int tand_part (int idx, char *args)
{
char *bot, *nick;
	bot = next_arg(args, &args);
	nick = next_arg(args, &args);
	tandout_but(idx, "part %s %s\n", bot, nick);
	chanout_but(idx, "[%s@%s] has left the TwilightZone\n", nick, bot);
	put_it("%s", convert_output_format("%K[%RxLink%Y$1%K(%r$2%K)] %n$3-", "%s %s %s %s", update_clock(GET_TIME), nick, bot, "left the TwilightZone"));
	return 0;
}

int send_who_to(int idx, char *from, int arg)
{
/*	dcc_printf(idx, "priv %s %s testing\n", nickname, from);*/
DCC_list *Client;
int found = 0;
	for (Client = ClientList; Client; Client = Client->next)
	{
		if ((Client->flags & DCC_TYPES) == DCC_CHAT && Client->in_dcc_chat)
		{
			if (!found++)
				dcc_printf(idx, "priv %s %s %-10s  %-10s  %-25s\n", nickname, from, "Who", "Bot", "Host");
			dcc_printf(idx, "priv %s %s %-10s  %-10s  %-25s\n", nickname, from, Client->user, nickname, Client->userhost);
		}
	} 
	return 0;
}

int tand_who (int idx, char *args)
{
char *from, *to, *p = NULL;
	from = m_strdup(next_arg(args, &args));
	if ((p = strchr(from, '@')) == NULL)
	{
		p = m_sprintf("%s@%s", from, nickname);
		new_free(&from);
		from = p;
	}
	to = next_arg(args, &args);
	if (!my_stricmp(to, nickname))
		send_who_to(idx, from, atoi(args));
	else
		tandout_but(idx, "who %s %s %s\n", from, to, args);
	new_free(&from);
	return 0;   
}

int tand_whom (int idx, char *args)
{
char *bot, *nick, *from, *p;
	from = m_strdup(next_arg(args, &args));
	if ((p = strchr(from, '@')) == NULL)
	{
		p = m_sprintf("%s@%s", from, nickname);
		new_free(&from);
		from = p;
	}
	nick = next_arg(args, &args);
	bot = next_arg(args, &args);
	send_who_to(idx, from, 0);
	tandout_but(idx, "whom %s %s %s %s\n", from, nick, bot, args);
	return 0;
	new_free(&from);
}

int tell_who(int idx, char *arg)
{
DCC_list *Client;
int found = 0;
	for (Client = ClientList; Client; Client = Client->next)
	{
		if ((Client->flags & DCC_TYPES) == DCC_CHAT && Client->in_dcc_chat)
		{
			if (!found++)
				dcc_printf(idx, "%-10s  %-10s\n", "Who", "Bot");
			dcc_printf(idx, "%-10s  %-10s\n", Client->user, nickname);
		}
	} 
	if (!found)
		dcc_printf(idx, "No Clients connected\n");
	found = 0;
	for (Client = ClientList; Client; Client = Client->next)
	{
		if ((Client->flags & DCC_TYPES) == DCC_BOTMODE)
		{
			if (!found++)
				dcc_printf(idx, "%-10s  %-15s\n", "Bots", "Connected");
			dcc_printf(idx, "%-10s  %10s\n", Client->user, nickname);
		}
	} 
	if (!found)
		dcc_printf(idx, "No Bots connected\n");
	return 0;
}

int send_who(int idx, char *arg)
{
int found = 0;
DCC_list *Client, *C = NULL;
	if (arg && *arg)
	{
		if (!my_stricmp(arg, nickname))
			tell_who(idx, NULL);
		else
		{
			for (C = ClientList; C; C = C->next)
				if (idx == C->read)
					break;
			for (Client = ClientList; Client; Client = Client->next)
			{
				if (!(Client->flags & DCC_ACTIVE))
					continue;
				if (C && !my_stricmp(Client->user, arg) && Client->read != -1 && (Client->flags & DCC_TYPES) == DCC_BOTMODE)
				{
					dcc_printf(Client->read, "who %d:%s@%s %s %d\n",
						idx, C->user, nickname, arg, 0);
					found = 1;
				}
				
			}
			if (!found)
				dcc_printf(idx, "Not found %s\n", arg);
		}
	} else
		tell_who(idx, NULL);
	return 0;
}

int tell_whom(int idx, char *arg)
{
DCC_list *Client;
int found = 0;
	for (Client = ClientList; Client; Client = Client->next)
	{
		if ((Client->flags & DCC_TYPES) == DCC_CHAT && Client->in_dcc_chat)
		{
			if (!found++)
				dcc_printf(idx, "%-10s  %-10s  %-25s\n", "Who", "Bot", "Host");
			dcc_printf(idx, "%-10s  %-10s  %-25s\n", Client->user, nickname, Client->userhost?Client->userhost:"Unknown");
		}
	} 
	return 0;
}

int send_whom(int idx, char *arg)
{
int found = 0;
DCC_list *Client, *C = NULL;

	tell_whom(idx, NULL);
	for (C = ClientList; C; C = C->next)
		if (idx == C->read)
			break;
	for (Client = ClientList; Client; Client = Client->next)
	{
		if (!(Client->flags & DCC_ACTIVE))
			continue;
		if (C && Client->read != -1 && (Client->flags & DCC_TYPES) == DCC_BOTMODE)
		{
			dcc_printf(Client->read, "whom %d:%s@%s %s %d\n",
				idx, C->user, nickname, arg, 0);
			found = 1;
		}
		
	}
	return 0;
}

int tand_priv (int idx, char *args)
{
char *to, *from, *p, *i_dx;
	from = next_arg(args, &args);
	to = next_arg(args, &args);
	p = strchr(to, '@');
	if (p && !my_stricmp(p+1, nickname))
	{
		char *t = strchr(to, ':');
		i_dx = to;
		if (t)
			*t = 0;
		/* this ones for me */
		dcc_printf(atoi(i_dx), "%s\n", args);		
	} 
	else
	{
		tandout_but(idx, "%s\n", args);
	}
	return 0;
}

int tand_boot (int idx, char *args)
{
DCC_list *Client;
char *nick;
char *from;
char *reason;
	from = next_arg(args, &args);
	nick = next_arg(args, &args);
	reason = args;
	Client = dcc_searchlist("chat", nick, DCC_CHAT, 0, NULL, NULL, 1);
	if (Client)
	{
		dcc_printf(Client->read, "You have been booted by %s for %s\n", from, reason);
		chanout_but(Client->read, "%s has booted %s out of the TwilightZone for (%s)\n", from, Client->user, reason);
		if (Client->flags & DCC_WAIT || Client->flags & DCC_ACTIVE)
		{
			FD_CLR(Client->read, &readables);
			new_close(Client->read);
		}
		dcc_erase(Client);
	}
	else
		tandout_but(idx, "boot %s %s %s\n", from, nick, args);	
	return 0;
}

int tand_privmsg(int idx, char *par)
{
char *to, *from;
DCC_list *Client;
	context;
	to = next_arg(par, &par);
	from = next_arg(par, &par);
	Client= dcc_searchlist("chat", to, DCC_CHAT, 0, NULL, NULL, 1);
	if (Client && Client->in_dcc_chat)
		dcc_printf(Client->read, "[%s] %s\n", from, par?par:"");
	else
		tandout_but(idx, "privmsg %s %s %s\n", to, from, par);
	return 0;
}

int cmd_cmsg(int idx, char *par)
{
char *p, *nick, *user;
DCC_list *tmp, *Client;

	context;
	p = next_arg(par, &par);
	nick = m_strdup(p);
	if ((user = strchr(nick, '@')))
		*user++ = '\0';
	if (nick)
	{
		for (Client = ClientList; Client; Client = Client->next)
			if (idx == Client->read)
				break;
		for (tmp = ClientList; tmp; tmp = tmp->next)
		{
			if (((tmp->flags & DCC_TYPES) == DCC_CHAT) && tmp->flags & DCC_ACTIVE)
				if (!my_stricmp(tmp->user, nick))
					break;
		}
		if (tmp) 
		{
			if (tmp->flags & DCC_ACTIVE && tmp->read != -1)
				dcc_printf(tmp->read, "[%s@%s] %s\n", Client->user, user?user:nickname, par);
		} else
			tandout_but(idx, "privmsg %s %s@%s %s\n", nick, Client->user, user?user:nickname, par);
	}
	new_free(&nick);
	return TCL_OK;
}

int cmd_boot(int idx, char *par)
{
	char *nick = next_arg(par, &par);
	char *reason;
	DCC_list *Client, *tmp = NULL;
	context;
	Client = dcc_searchlist("chat", nick, DCC_CHAT, 0, NULL, NULL, 1);
	for (tmp = ClientList; tmp; tmp = tmp->next)
	{
		if (tmp->read == idx)
			break;
	}
	if (my_stricmp(nick, nickname))
	{
		if (!par || !*par)
			reason = "We don't like you";
		else
			reason = par;
		if (!Client)
			tandout_but(idx, "boot %s %s %s\n", nickname, nick, reason);
		else
		{
			if (Client->read > -1)
			{
				dcc_printf(Client->read, "You have been booted by %s for (%s)\n", tmp? tmp->user:nickname, reason);
				chanout_but(Client->read, "%s booting %s out of the TwilightZone (%s)\n", tmp? tmp->user:nickname, Client->user, reason);
				if (Client->flags & DCC_WAIT || Client->flags & DCC_ACTIVE)
				{
					FD_CLR(Client->read, &readables);
					new_close(Client->read);
				}
				dcc_erase(Client);
			} else
				dcc_printf(idx, "Error in cmd_boot\n");
		}
	} else
		dcc_printf(idx, "Can't boot a bot\n");
	return TCL_OK;
}

int cmd_act(int idx, char *par)
{
DCC_list *tmp;
char *nick;
	context;
/*
	send_to_server("PRIVMSG %s :\001ACTION %s\001\n", nick, par);
*/
	for (tmp = ClientList; tmp; tmp = tmp->next)
	{
		if (tmp->read == idx)
			break;
	}
	if (idx > -1)
		nick = tmp->user;
	else
		nick = empty_string;
	tandout_but(idx, "  * %s %s\n", nick, par);
	chanout_but(idx, "  * %s %s\n", nick, par);	
	return TCL_OK;
}

int cmd_help(int idx, char *par)
{
	char *command;
	int i;
	context;
	command = next_arg(par, &par);
	if (!command)
	{
		dcc_printf(idx, "dCC commands :\n");
		for (i = 1; C_dcc[i-1].name; i++)
		{
			dcc_printf(idx, "%4d %-10s", C_dcc[i-1].access, C_dcc[i-1].name);
			if (!(i % 3))
				dcc_printf(idx, "\n");
		}
		dcc_printf(idx, "\n");
#ifdef WANT_TCL
		dcc_printf(idx, "mSG commands :\n");
		dcc_printf(idx, "%4d %-10s\n", C_msg[0].access, C_msg[0].name);
#endif
	}
	else
	{
		for (i = 0; C_dcc[i].name; i++)
			if (!my_stricmp(C_dcc[i].name, command))
				break;
		if (C_dcc[i].name)
			dcc_printf(idx, "%4d %-10s %s\n", C_dcc[i].access, C_dcc[i].name, C_dcc[i].help);
#ifdef WANT_TCL
		else if (!my_stricmp(C_msg[0].name, command))
			dcc_printf(idx, "%4d %10s %s\n", C_msg[0].access, C_msg[0].name, C_msg[0].help);
#endif
		else
			dcc_printf(idx, "No such command [%s]\n", command);
	}
	return TCL_OK;
}

int cmd_msg(int idx, char *par)
{
	char *nick = next_arg(par, &par);
	context;
	send_to_server("PRIVMSG %s :%s\n",nick, par);
	return TCL_OK;
}

int cmd_say(int idx, char *par)
{
	char *nick = next_arg(par, &par);

	context;
	send_to_server("PRIVMSG %s :%s\n",nick, par);
	return TCL_OK;
}

int cmd_tcl(int idx, char *par)
{

	context;
#ifdef WANT_TCL
	if ((Tcl_Eval(interp, par)) == TCL_OK)
	{
		dcc_printf(idx, "Tcl: %s\n", interp->result);
	} else 
		dcc_printf(idx, "Tcl Error: %s\n", interp->result);
#else
		dcc_printf(idx, "Not implemented in this client\n");
#endif
	return TCL_OK;
}

int cmd_chat(int idx, char *par)
{
DCC_list *tmp;
char buffer[BIG_BUFFER_SIZE+1];
	for (tmp = ClientList; tmp; tmp = tmp->next)
	{
		if (tmp->read == idx && !tmp->in_dcc_chat)
		{
			tmp->in_dcc_chat = 1;
			sprintf(buffer, "%s %s %s", nickname, tmp->user, tmp->userhost);
			tand_join(idx, buffer);
			dcc_printf(idx, "Welcome to the TwilightZone\n");
			break;
		}
	}
	return TCL_OK;
}

int cmd_quit(int idx, char *par)
{
DCC_list *tmp;
char buffer[BIG_BUFFER_SIZE+1];
	for (tmp = ClientList; tmp; tmp = tmp->next)
	{
		if (tmp->read == idx && tmp->in_dcc_chat)
		{
			tmp->in_dcc_chat = 0;
			sprintf(buffer, "%s %s %s", nickname, tmp->user, tmp->userhost);
			tand_part(idx, buffer);
			dcc_printf(tmp->read, "\002GoodBye %s.\002\n", nickname);
			break;
		}
	}
	return TCL_OK;
}

void invite_dcc_chat(WhoisStuff *stuff, char *nick, char *args)
{
char *id;
int idx = -1;
DCC_list *Client;

	id = next_arg(args, &args);
	idx = atol(id);
        if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick))
        {
                dcc_printf(idx, "No such for user %s\n", nick);
                return;
        }
	dcc_chat(nick);
	for (Client = ClientList; Client; Client = Client->next)
	{
		if (Client->user && !my_stricmp(Client->user, nick))
			if ((Client->flags & DCC_TYPES) == DCC_CHAT && !Client->userhost)
				malloc_sprintf(&Client->userhost, "%s@%s", stuff->user, stuff->host);
	}
	send_to_server("NOTICE %s :You've been invited to a TwilightZone", nick);
	send_to_server("NOTICE %s :Please type .chat in dcc chat to start", nick);
	chanout_but(idx, "Inviting %s to TwilightZone\n", nick);
	return;
}

int cmd_invite(int idx, char *par)
{
int old_server = from_server;
	from_server = get_window_server(0);
	if (par && *par)
	{
		char *nick = next_arg(par, &par);
		add_to_userhost_queue(nick, invite_dcc_chat, "%d %s", idx, nick);
	}
	from_server = old_server;
	return TCL_OK;
}

int cmd_echo(int idx, char *par)
{
DCC_list *tmp;
		
	for (tmp = ClientList; tmp; tmp = tmp->next)
		if (tmp->read == idx)
			break;
	if (idx == -1 || !tmp)
		return TCL_ERROR;
	
	if (par && *par)
	{
		if (!my_stricmp(par, "off"))
			tmp->echo = 0;
		else
			tmp->echo = 1;
	} else
		tmp->echo = (tmp->echo == 1) ? 0 : 1;		
	dcc_printf(idx, " echo is now %s\n", on_off(tmp->echo));
	return TCL_OK;
}

int send_command (int idx, char *par)
{
	if (par && *par && xlink_commands)
	{
		tandout_but(idx, "clink_com %s\n", par);
/*		dcc_printf(idx, " command %s sent to botnet\n", par);*/
		if (xlink_commands)
			parse_command(par, 0, empty_string);
		return TCL_OK;
	}
	return TCL_ERROR;
}

int tand_command (int idx, char *par)
{
	chanout_but(idx, ".xlink %s\n", par);
	tandout_but(idx, "clink_com %s\n", par);
	if (xlink_commands)
		parse_command(par, 0, empty_string);
/*	put_it("%s", convert_output_format("%K[%RxLink%Y$1%K(%r$2%K)] %n$3-", "%s %s %s %s", update_clock(GET_TIME), nick, bot, "left the TwilightZone"));*/
	return TCL_OK;
}

#ifndef WANT_TCL
int check_tcl_dcc(char *cmd, char *nick, char *host, int idx, DCC_list *client)
{
	int x, atr = 1;
	int old_server = from_server;
	char *c, *args;
	UserList *n;

	context;
	if (from_server == -1)
		from_server = get_window_server(0);
	if ((n = lookup_userlevelc("*", host, "*", user_list)))
		atr = n->level;
	else if ((n = lookup_userlevelc("*", host, "*", Bot_list)))
		atr = 100;

	if (!cmd || !*cmd)
		return 0;
	c = next_arg(cmd, &cmd);
	if (!c || !*c)
	{
		from_server = old_server;
		return 0;
	}
	if (my_strnicmp(c, "chat", 3) && !client->in_dcc_chat)
	{
		from_server = old_server;
		return 0;
	}
	args = cmd;
	for (x = 0; C_dcc[x].func; x++) 
	{
		if (!my_stricmp(c, C_dcc[x].name) && atr > C_dcc[x].access)
		{
			(C_dcc[x].func)(idx,args);
			from_server = old_server;
			return 1;
		}
	}
	from_server = old_server;
	return 1;
}

#endif
