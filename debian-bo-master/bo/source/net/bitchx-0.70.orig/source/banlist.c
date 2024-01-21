/*
 * this code is Copyright Colten Edwards (c) 96
 */
 
#include "irc.h"
#include "edit.h"
#include "list.h"
#include "hook.h"
#include "ignore.h"
#include "ircaux.h"
#include "output.h"
#include "screen.h"
#include "server.h"
#include "struct.h"
#include "window.h"
#include "whois.h"
#include "whowas.h"
#include "vars.h"
#include "userlist.h"
#include "misc.h"

int defban = 2;

static char *mode_buf = NULL;
static int mode_len = 0;

static char *mode_str = NULL;
static char *user = NULL;
static int mode_str_len = 0;
static int len = 0;
static char plus_mode[20] = "\0";

void add_mode_buffer( char *buffer, int mode_str_len)
{
	context;
	malloc_strcat(&mode_buf, buffer);
	mode_len += len;
}

void flush_mode(ChannelList *chan)
{
	context;
	if (mode_buf)
		send_to_server(mode_buf);
	new_free(&mode_buf);
	mode_len = 0;
}

int delay_flush_all _((void *arg))
{
char buffer[BIG_BUFFER_SIZE+1];
char * old_arg=(char *)arg;
char * args = NULL;
char * serv_num = NULL;
char * channel = NULL;
int this_server = from_server;

	context;
	channel = next_arg(args, &args);
	if ((serv_num = next_arg(args, &args)))
		from_server = atoi(serv_num);
        if (channel && *channel && mode_str && user)
        {
                len = sprintf(buffer, "MODE %s %s%s %s\r\n", channel, plus_mode, mode_str, user);
                add_mode_buffer(buffer, len);
                mode_str_len = 0;
                new_free(&mode_str);
                new_free(&user);
                bzero(plus_mode, sizeof(plus_mode));
                len = 0;
        }
        flush_mode(NULL);
	new_free(&old_arg);
	from_server = this_server;
	return 0;
}

void flush_mode_all(ChannelList *chan)
{
char buffer[BIG_BUFFER_SIZE+1];

	context;
	if (mode_str && user)
	{
		len = sprintf(buffer, "MODE %s %s%s %s\r\n", chan->channel, plus_mode, mode_str, user);
		add_mode_buffer(buffer, len);
		mode_str_len = 0;
		new_free(&mode_str);
		new_free(&user);
		bzero(plus_mode, sizeof(plus_mode));
		len = 0;
	}
	flush_mode(chan);
}


void add_mode(ChannelList *chan, char *mode, int plus, char *nick, char *reason, int max_modes)
{
char buffer[BIG_BUFFER_SIZE+1];
/*
KICK $C nick :reason
MODE $C +/-o nick
MODE $C +/-b userhost
*/

	context;
	if (mode_len >= (IRCD_BUFFER_SIZE-100))
	{
		flush_mode(chan);
		len = 0;
	}

	if (reason)
	{
		len = sprintf(buffer, "KICK %s %s :%s\r\n", chan->channel, nick, reason);
		add_mode_buffer(buffer, len);
	}
	else
	{
#if 0
		malloc_strcat(&mode_str, mode);
		malloc_strcat(&user, nick);
		malloc_strcat(&user, " ");
		plus_mode[mode_str_len++]  = plus?'+':'-';
#else
		mode_str_len++;
		malloc_strcat(&mode_str,plus?"+":"-");	
		malloc_strcat(&mode_str, mode);
		malloc_strcat(&user, nick);
		malloc_strcat(&user, " ");

#endif
		if (mode_str_len >= max_modes)
		{
			len = sprintf(buffer, "MODE %s %s %s\r\n", chan->channel, mode_str, user);
			add_mode_buffer(buffer, len);
			new_free(&mode_str);
			new_free(&user);
			bzero(plus_mode, sizeof(plus_mode));
			mode_str_len = len = 0;
		}
	}
}


void fuckem (char *command, char *args, char *subargs)
{
char c;
ChannelList *chan;
int server;
char buffer[BIG_BUFFER_SIZE];
BanList *Bans;
	if (!(chan = prepare_command(&server, NULL, NEED_OP)))
		return;
	for (Bans = chan->bans; Bans; Bans = Bans->next)
		add_mode(chan, "b", 0, Bans->ban, NULL, get_int_var(NUM_BANMODES_VAR));
	for (c = 'a'; c <= 'z'; c++)
	{
		sprintf(buffer, "*!*@*%c*", c);
		add_mode(chan, "b", 1, buffer, NULL, get_int_var(NUM_BANMODES_VAR));
	}         
	flush_mode_all(chan);
}


/*
 * Lamer Kick!   Kicks All UnOpped People from Current Channel        
 */
void LameKick(char *command, char *args, char *subargs)
{
	char *channel = NULL;
	ChannelList *chan;
	NickList *tmp;
	char	*buffer = NULL;
	int	old_server = from_server;

	context;
	if (args && *args && is_channel(args))
		channel = next_arg(args, &args);	
	if ((chan = prepare_command(&from_server, channel, NEED_OP)))
	{
		malloc_sprintf(&buffer, "<\002BX\002-LK> %s", args && *args? args:"");
		for (tmp = chan->nicks; tmp; tmp = tmp->next)
		{
			if (!tmp->chanop && !tmp->voice && ((tmp->userlist && !tmp->userlist->prot) || !tmp->userlist))
			{
#if 0
				send_to_server("%s %s %s :%s", "KICK", chan->channel, tmp->nick, buffer);
#endif
				add_mode(chan, NULL, 0, tmp->nick, buffer, 0);
			}
		}
		flush_mode_all(chan);
		new_free(&buffer);
		say("Sent the Server all the Lamer Kicks, Sit back and Watch!");
	}
	from_server = old_server;
}

void shitlist_erase(ShitList **clientlist)
{
	ShitList	*Client, *tmp;
	context;
	for (Client = *clientlist; Client;)
	{
		new_free(&Client->filter);
		new_free(&Client->reason);
		tmp = Client->next;
		new_free((char **)&Client);
		Client = tmp;
	}
	*clientlist = NULL;
}

char *screw(char *user)
{
char *p;
	for (p = user; p && *p;)
	{
		switch(*p)
		{
			case '.':
			case '*':
			case '@':
			case '!':
				p+=1;
				break;
			default:
				*p = '?';
				if (*(p+1) && *(p+2))
					p+=2;
				else
					p++;
		}
	}
	return user;
}

char * ban_it(char *nick, char *user, char *host)
{
static char banstr[BIG_BUFFER_SIZE+1];
char *tmpstr = NULL;
char *t = user;
	context;
	*banstr = 0;
	if (t && *t == '~')
		user++;
	if (t && *t == '+')
		t++;
	switch (defban) 
	{
		case 2: /* Better 	*/
			sprintf(banstr, "*!*%s@%s", t, cluster(host));
			break;
		case 3: /* Host 	*/
			sprintf(banstr, "*!*@%s", cluster(host));
			break;
		case 4: /* Domain	*/
			sprintf(banstr, "*!*@*%s", rindex(host, '.'));
			break;
		case 5: /* User		*/
			sprintf(banstr, "*!%s@%s", t, cluster(host));
			break;
		case 6: /* Screw 	*/
			malloc_sprintf(&tmpstr, "*!*%s@%s", t, host);
			strcpy(banstr, screw(tmpstr));
			new_free(&tmpstr);
			break;
		case 1:	/* Normal 	*/
		default:
			sprintf(banstr, "%s!*%s@%s", nick, t, host);
			break;
	}
	return banstr;
}

void userhost_unban (WhoisStuff *stuff, char *nick1, char *args)
{
char *tmp;
ChannelList *chan;
BanList *bans;

char *host = NULL;
WhowasList *whowas = NULL;

int count = 0;
int old_server = from_server;

	context;
	if (!stuff || !stuff->nick || !nick1 || 
		!strcmp(stuff->user, "<UNKNOWN>") || 
		my_stricmp(stuff->nick, nick1))
	{
		if ((whowas = check_whowas_nick_buffer(nick1, args, 0)))
		{
			malloc_sprintf(&host, "%s!%s", whowas->nicklist->nick, whowas->nicklist->host);
			bitchsay("Using WhoWas info for unban of %s ", nick1);
		}
		else
		{
			bitchsay("No match for the unban of %s on %s", nick1, args);
			return;
		}
	}
	else
	{
		tmp = clear_server_flags(stuff->user);
		malloc_sprintf(&host, "%s!%s@%s",stuff->nick, tmp, stuff->host); 
	}

	if (!(chan = prepare_command(&from_server, NULL, NEED_OP)))
	{
		new_free(&host);
		return;
	}


	for (bans = chan->bans; bans; bans = bans->next)
	{
		if (!bans->sent_unban && (match(bans->ban, host) || match(host, bans->ban)))
		{
#if 0
			send_to_server("MODE %s -b %s", args, bans->ban);
#endif
			add_mode(chan, "b", 0, bans->ban, NULL, get_int_var(NUM_BANMODES_VAR));
			bans->sent_unban++;
			count++;
		}			
	}	

	flush_mode_all(chan);
	if (!count)
		bitchsay("No Match for Unban of %s on %s", nick1, args);
	new_free(&host);
	from_server = old_server;
}


void userhost_ban(WhoisStuff *stuff, char *nick1, char *args)
{
	char *temp;
	char *str= NULL;
	char *channel;


	char *ob = "-o+b";
	char *b = "+b";

	char *host = NULL, *nick = NULL, *user = NULL;
	WhowasList *whowas = NULL;
		
	int fuck = 0;
	int set_ignore = 0;
	
	context;
	channel = next_arg(args, &args);
	temp = next_arg(args, &args);

	fuck = !my_stricmp("FUCK", args);
	set_ignore = !my_stricmp("BKI", args);
	
	if (!stuff || !stuff->nick || !nick1 || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick1))
	{
		if ((whowas = check_whowas_nick_buffer(nick1, channel, 0)))
		{
			nick = whowas->nicklist->nick;
			user = m_strdup(whowas->nicklist->host);
			host = strchr(user, '@');
			*host++ = 0;
			bitchsay("Using WhoWas info for ban of %s ", nick1);
		}
		else
		{
			bitchsay("No match for the %s of %s on %s", fuck ? "Fuck":"Ban", nick1, channel);
			return;
		}
	} 
	else
	{
		nick = stuff->nick;
		user = m_strdup(clear_server_flags(stuff->user));
		host = stuff->host;
	}

	if (!(my_stricmp(nick, get_server_nickname(from_server))))
	{
		bitchsay("Try to kick yourself again!!");
		new_free(&user);
		return;
	}

	send_to_server("MODE %s %s %s", channel, stuff->channel ? ob : b, ban_it(nick, user, host));
	if (fuck)
	{
		malloc_sprintf(&str, "%s!*%s@%s %s 3 Auto-Shit", nick, user, host, channel);
		add_shit(NULL, str, NULL);
		new_free(&str);
	} else if (set_ignore)
		ignore_nickname(ban_it("*", user, host)	, IGNORE_ALL, 0);
	new_free(&user);
}

void multkick(char *command, char *args, char *subargs)
{
	char *to = NULL, *temp = NULL, *reason = NULL;
	ChannelList *chan;
	int server = from_server;
	int	filter = 0;
	context;

	if (command && *command)
		filter = 1;
		
	if (!(to = next_arg(args, &args)))
		to = NULL;
	
	if (to && !is_channel(to))
	{
		temp = to;
		if (args && *args)
			*(temp + strlen(temp)) = ' ';
		to = NULL;
	}
	else
		temp = args;
		
	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!temp || !*temp)
	{
		say("Who am I supposed to %s?", command?"filterkick":"multikick");
		return;
	}
	reason = strchr(temp, ':');

	if (reason)
		*reason++ = 0;

	if (!reason || !*reason)
		reason = get_reason(NULL);

	while (temp && *temp)
		my_send_to_server(server, "KICK %s %s :\002%s\002", chan->channel,
			       next_arg(temp, &temp), reason);
}

void massdeop(char *command, char *args, char *subargs)
{
	ChannelList *chan;

register NickList *nicks;

	char *spec, *rest, *to;
	int maxmodes, count, all = 0;
	char	buffer[BIG_BUFFER_SIZE + 1];
	int isvoice = 0;
	int old_server = from_server;
			
	context;
	maxmodes = get_int_var(NUM_OPMODES_VAR);

	if (command && !my_stricmp(command, "mdvoice"))
		isvoice = 1;
	
	rest = NULL;
	spec = NULL;

	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}
	if (!(chan = prepare_command(&from_server, to, NEED_OP)))
		return;


	if (!spec && !(spec = next_arg(args, &args)))
		spec = "*!*@*";
	if (*spec == '-')
	{
		rest = spec;
		spec = "*!*@*";
	}
	else
		rest = args;
	if (rest && !my_stricmp(rest, "-all"))
		all = 1;


	nicks = chan->nicks;
	count = 0;
#if 0
	while (nicks)
	{
		i = 0;
		*modebuf = 0;
		while (nicks && (i < maxmodes))
		{
			*buffer = 0;
			sprintf(buffer, "%s!%s", nicks->nick, nicks->host);
			if ((all || (!isvoice && nicks->chanop) || (isvoice && nicks->voice)) &&
			    my_stricmp(nicks->nick, get_server_nickname(from_server)) &&
			    match(spec, buffer))
			{
				count++;
				i++;
				strcat(modebuf, " ");
				strcat(modebuf, nicks->nick);
			}
			nicks = nicks->next;
		}
		if (i)
		{
			len += sprintf(buffer, "MODE %s -%s %s\r\n", chan->channel, strfill(isvoice?'v':'o', count), modebuf);
			malloc_strcat(&send_buf, buffer);
		}
#if 0
			send_to_server("MODE %s -%s %s", chan->channel, strfill(isvoice?'v':'o',count), modebuf);
#endif
		if (len >= IRCD_BUFFER_SIZE - 100)
		{
			len = 0;
			send_to_server(send_buf);
			new_free(&send_buf);
		}
	}
	if (send_buf)
		send_to_server(send_buf);
	new_free(&send_buf);
#else
	while (nicks)
	{
		sprintf(buffer, "%s!%s", nicks->nick, nicks->host);
		if ((all || (!isvoice && nicks->chanop) || (isvoice && nicks->voice)) &&
		    my_stricmp(nicks->nick, get_server_nickname(from_server)) &&
		    match(spec, buffer))
		{
			add_mode(chan, isvoice? "v":"o", 0, nicks->nick, NULL, maxmodes);
			count++;
		}
		nicks = nicks->next;
	}
	flush_mode_all(chan);
#endif
	from_server = old_server;
	if (!count)
		say("No matches for %s of %s on %s", command?command:"massdeop", spec, chan->channel);
}

void doop(char *command, char *args, char *subargs)
{
	char	*to = NULL, 
		*temp = NULL;
ChannelList	*chan = NULL;
	int	count,
		max = get_int_var(NUM_OPMODES_VAR);
	char	buffer[BIG_BUFFER_SIZE + 1];
	int	old_server = from_server;
	int	voice = 0;
	
	context;
	if (command)
		voice = 1;
	count = 0;
	*buffer = 0;

	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		temp = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&from_server, to, NEED_OP)))
		return;

	if (!temp)
		temp = next_arg(args, &args);

	while (temp && *temp)
	{
		count++;
#if 0
		strncat(buffer, temp, BIG_BUFFER_SIZE);
		if (count == max)
		{
			send_to_server("MODE %s +%s %s", chan->channel, strfill(voice? 'v':'o', count), buffer);
			count = 0;
			*buffer = '\0';
		}
		else
			strncat(buffer, " ", BIG_BUFFER_SIZE);
#else
		add_mode(chan, voice?"v":"o", 1, temp, NULL, max);
#endif
		temp = next_arg(args, &args);
	}
	flush_mode_all(chan);
#if 0
	if (count)
		send_to_server("MODE %s +%s %s", chan->channel, strfill(voice?'v':'o', count), buffer);
#endif
	from_server = old_server;
}

void dodeop(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	char *to = NULL, *temp;
	int count, max;
	ChannelList *chan;
	char	buffer[BIG_BUFFER_SIZE + 1];
	int isvoice = 0;
	int server = from_server;
		
	context;
	count = 0;
	temp = NULL;
	max = get_int_var(NUM_OPMODES_VAR);

	*buffer = 0;

	if (command && (!my_stricmp(command, "unvoice") || !my_stricmp(command, "dvoice")))
		isvoice = 1;
	
	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		temp = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!temp)
		temp = next_arg(args, &args);

	while (temp && *temp)
	{
		count++;
#if 0
		strncat(buffer, temp, BIG_BUFFER_SIZE);
		if (count == max)
		{
			send_to_server("MODE %s -%s %s", chan->channel, strfill(isvoice?'v':'o', count), buffer);
			count = 0;
			*buffer = '\0';
		}
		else
			strncat(buffer, " ", BIG_BUFFER_SIZE);
#else
		add_mode(chan, isvoice?"v":"o", 0, temp, NULL, max);
#endif
		temp = next_arg(args, &args);
	}
	flush_mode_all(chan);
#if 0	
	if (count)
		send_to_server("MODE %s -%s %s", chan->channel, strfill(isvoice?'v':'o', count), buffer);
#endif
}

void massop(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	ChannelList *chan;
	
	register NickList *nicks;

	char	*to = NULL, 
		*spec, 
		*rest;
	char	buffer[BIG_BUFFER_SIZE + 1];
	
	int	maxmodes = get_int_var(NUM_OPMODES_VAR), 
		count, 
		i, 
		all = 0,
		massvoice =0;
	int	server = 0;
		
	context;
	if (command)
		massvoice = 1;

	maxmodes = get_int_var(NUM_OPMODES_VAR);

	rest = NULL;
	spec = NULL;

	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to) )
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!spec && !(spec = next_arg(args, &args)))
		spec = "*!*@*";
	if (*spec == '-')
	{
		rest = spec;
		spec = "*!*@*";
	}
	else
		rest = args;

	if (rest && !my_stricmp(rest, "-all"))
		all = 1;

	nicks = chan->nicks;
	count = 0;
	while (nicks)
	{
		i = 0;
#if 0
		*modebuf = '\0';
		while (nicks && (i < maxmodes))
		{
			sprintf(buffer, "%s!%s", nicks->nick, nicks->host);
			if ((all || (!massvoice && !nicks->chanop) || (massvoice && !nicks->voice && !nicks->chanop)) &&
			    my_stricmp(nicks->nick, get_server_nickname(from_server)) &&
			    match(spec, buffer))
			{
				count++;
				i++;
				strncat(modebuf, " ", BIG_BUFFER_SIZE);
				strncat(modebuf, nicks->nick, BIG_BUFFER_SIZE);
			}
			nicks = nicks->next;
		}
		if (i)
			send_to_server("MODE %s +%s %s", chan->channel, strfill(massvoice? 'v':'o',count), modebuf);
#else
		sprintf(buffer, "%s!%s", nicks->nick, nicks->host);
		if ((my_stricmp(nicks->nick, get_server_nickname(from_server)) && match(spec, buffer)))
		{
			if ((massvoice && !nicks->voice && !nicks->chanop) || !nicks->chanop)
			{
				add_mode(chan, massvoice?"v":"o", 1, nicks->nick, NULL, maxmodes);
				count++;
			}
		}
		nicks = nicks->next;
#endif
	}
	flush_mode_all(chan);
	if (!count)
		say("No matches for %s of %s on %s", command? command : "massop", spec, chan->channel);
}

void masskick(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	ChannelList *chan;
register NickList *nicks;
	ShitList *masskick_list = NULL, *new;
	char *to, *spec, *rest, *buffer = NULL, *q;
	int server = from_server;

	int all = 0;

	context;
	rest = NULL;
	spec = NULL;

	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!spec && !(spec = next_arg(args, &args)))
	{
		say("No pattern given for the %s", !command ? "masskick":command);
		return;
	}
	if (args && !strncmp(args, "-all", 4))
	{
		all = 1;
		(void) next_arg(args, &args);
	}

	rest = args;
	if (rest && !*rest)
		rest = NULL;

	nicks = chan->nicks;

	while (nicks)
	{
		q = nicks->host;
		if (q && *q == '~') q++;
		if (q && *q == '#') q++;
		malloc_sprintf(&buffer, "%s!*%s", nicks->nick, q);		
		if ((all || !nicks->chanop || get_int_var(KICK_OPS_VAR)) &&
		    my_stricmp(nicks->nick, get_server_nickname(from_server)) &&
		    (match(spec, buffer) || match(nicks->nick, spec)))
		{
			new = (ShitList *)new_malloc(sizeof(ShitList));
			malloc_sprintf(&new->filter, "%s", nicks->nick);
			malloc_strcpy(&new->reason, rest? rest : get_reason(nicks->nick));
			add_to_list((List **)&masskick_list, (List *)new);
		}
		nicks = nicks->next;
		new_free(&buffer);
	}

	if (masskick_list)
	{
		int len = 0;
		char *send_buf = NULL;
		char buf[BIG_BUFFER_SIZE + 1];
						
		bitchsay("Performing (%s) MaSS KicKs on %s", all? "opz/non-opz":"non-opz", chan->channel);
		for (new = masskick_list; new; new = new->next)
		{
			len += sprintf(buf, "KICK %s %s :\002%s\002\r\n", chan->channel, new->filter, new->reason);
			malloc_strcat(&send_buf, buf);
			if (len >= IRCD_BUFFER_SIZE - 100)
			{
				send_to_server(send_buf);
				len = 0;
				new_free(&send_buf);
			}
		}
#if 0
			send_to_server("KICK %s %s :\002%s\002", chan->channel, new->filter, new->reason);
#endif
		if (send_buf)
			send_to_server(send_buf);
		new_free(&send_buf);
		shitlist_erase(&masskick_list);
	}
	else
		bitchsay("No matches for masskick of %s on %s", spec, chan->channel);
}

void mknu(command, args, subargs)
char *command, *args, *subargs;
{
	ChannelList *chan;
register NickList *nicks;
	char *to = NULL, *rest;
	int count;
	int server = from_server;
	
	context;
	if (!args || !*args)
		to = NULL;
	else if (*args == '#' || *args == '&' || !strncmp(args, "* ", 2) ||
		 !strcmp(args, "*"))
		to = next_arg(args, &args);
	else
		to = NULL;

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	rest = args;
	if (rest && !*rest)
		rest = NULL;

	nicks = chan->nicks;
	count = 0;
	while (nicks)
	{
		if (!nicks->chanop && my_stricmp(nicks->nick, get_server_nickname(from_server)))
		{
			count++;
			send_to_server("KICK %s %s :(non-users) \002%s\002", chan->channel, nicks->nick, rest ? rest : get_reason(nicks->nick));
		}
		nicks = nicks->next;
	}
	if (!count)
		say("No matches for masskick of non-users on %s", chan->channel);
}

void masskickban(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	ChannelList *chan;
register NickList *nicks;
	char *to = NULL, *spec, *rest;
	int count, all = 0;
	int server = from_server;
	char	buffer[BIG_BUFFER_SIZE + 1];
	
	context;
	rest = NULL;
	spec = NULL;

	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server,to, NEED_OP)))
		return;

	if (!spec && !(spec = next_arg(args, &args)))
	{
		say("No pattern given for the masskick");
		return;
	}
	if (args && !strncmp(args, "-all", 4))
	{
		all = 1;
		(void) next_arg(args, &args);
	}

	rest = args;
	if (rest && !*rest)
		rest = NULL;

	nicks = chan->nicks;
	count = 0;
	if (!strchr(spec, '!'))
	{
		char tempbuf[BIG_BUFFER_SIZE+1];

		strcpy(tempbuf, "*!");
		if (!strchr(spec, '@'))
			strcat(tempbuf, "*@");
		strcat(tempbuf, spec);
		send_to_server("MODE %s +b %s", chan->channel, tempbuf);
	}
	else
		send_to_server("MODE %s +b %s", chan->channel, spec);

	while (nicks)
	{
		*buffer = '\0';
		strcat(buffer, nicks->nick);
		strcat(buffer, "!");
		strcat(buffer, nicks->host);
		if ((all || !nicks->chanop || get_int_var(KICK_OPS_VAR)) &&
		    my_stricmp(nicks->nick, get_server_nickname(from_server)) &&
		    match(spec, buffer))
		{
			count++;
			send_to_server("KICK %s %s :(%s) \002%s\002", chan->channel, nicks->nick, spec, rest ? rest : get_reason(nicks->nick));
		}
		nicks = nicks->next;
	}
	if (!count)
		say("No matches for masskickban of %s on %s", spec, chan->channel);
}

void massban(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	ChannelList *chan;
register NickList *nicks;
	ShitList *massban_list = NULL, *tmp;
	char *to = NULL, *spec, *rest;
	char *buffer = NULL;
	int server = from_server;

	int maxmodes, all = 0;

	context;
	maxmodes = get_int_var(NUM_BANMODES_VAR);

	rest = NULL;
	spec = NULL;

	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!spec && !(spec = next_arg(args, &args)))
		spec = "*!*@*";
	if (*spec == '-')
	{
		rest = spec;
		spec = "*!*@*";
	}
	else
		rest = args;

	if (rest && !my_stricmp(rest, "-all"))
		all = 1;

	nicks = chan->nicks;

	while (nicks)
	{
		new_free(&buffer);
		malloc_sprintf(&buffer, "%s!%s", nicks->nick, nicks->host);

		if ((all || !nicks->chanop || get_int_var(KICK_OPS_VAR)) &&
		    my_stricmp(nicks->nick, get_server_nickname(from_server)) &&
		    match(spec, buffer))
		{
			char *temp = NULL;
			char *p, *q;
			ShitList *new;
			malloc_strcpy(&temp, nicks->host);

			q = nicks->host;
			if (q && (*q == '~'))
				q++;
			if (q && (*q == '#'))
				q++;
			p = strchr(nicks->host, '@');
			*p++ = '\0';
		
			new = (ShitList *)new_malloc(sizeof(ShitList));
			malloc_sprintf(&new->filter, "*!*%s@%s ", q, cluster(p));
			add_to_list((List **)&massban_list, (List *)new);
			new_free(&temp);
		}
		nicks = nicks->next;
	}
	new_free(&buffer);
	if (massban_list)
	{
		char modestr[100];
		int i = 0;
		
		bitchsay("Performing Mass Bans on %s", to);
		for (tmp = massban_list; tmp; tmp = tmp->next)
		{
			malloc_strcat(&buffer, tmp->filter);
			modestr[i] = 'b';
			i++;
			if (i > maxmodes)
			{
				modestr[i] = '\0';
				send_to_server("MODE %s +%s %s", chan->channel, modestr, buffer);
				i = 0;
				new_free(&buffer);
			}
		}
		modestr[i] = '\0';
		if (buffer && *buffer)
		{
			send_to_server("MODE %s +%s %s", chan->channel, modestr, buffer);
			new_free(&buffer);
		}
		shitlist_erase(&massban_list);
	} else
		say("No matches for massban of %s on %s", spec, chan->channel);
}

void unban(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	char *to, *spec, *host;
	ChannelList *chan;
	BanList *bans;
	int count = 0;
	int server = from_server;
			
	context;
	to = spec = host = NULL;

	if (!(to = new_next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!spec && !(spec = next_arg(args, &args)))
	{
		spec = "*";
		host = "*@*";
	}

	if (spec && *spec == '#')
	{
		count = atoi(spec);	
	}
	if (!strchr(spec, '*'))
	{
		add_to_userhost_queue(spec, userhost_unban, "%s", chan->channel);
		return;
	}

	if (count)
	{
		int tmp = 1;
		for (bans = chan->bans; bans; bans = bans->next)
		{
			if (tmp == count)
			{
				if (bans->sent_unban == 0)
				{
					send_to_server("MODE %s -b %s", chan->channel, bans->ban);
					bans->sent_unban++;
					tmp = 0;
					break;
				}
			} else
				tmp++;
		}
		if (tmp != 0)
			count = 0;
	} 
	else 
	{
		char *banstring = NULL;
		int num = 0;
		count = 0;	
		for (bans = chan->bans; bans; bans = bans->next)
		{
			if (match(bans->ban, spec) || match(spec, bans->ban))
			{
				if (bans->sent_unban == 0)
				{
					malloc_strcat(&banstring, bans->ban);
					malloc_strcat(&banstring, " ");
					bans->sent_unban++;
					count++;
					num++;
				}
			}
			if (count && (count % get_int_var(NUM_BANMODES_VAR) == 0))
			{
				send_to_server("MODE %s -%s %s", chan->channel, strfill('b', num), banstring);
				new_free(&banstring);
				num = 0;
			}
		}
		if (banstring && num)
			send_to_server("MODE %s -%s %s", chan->channel, strfill('b', num), banstring);
		new_free(&banstring);
	}
	if (!count)
		bitchsay("No ban matching %s found", spec);

}

void dokick(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	char	*to = NULL, 
		*spec = NULL,
		*reason = NULL;
	ChannelList *chan;
	int server = from_server;

	context;
	if (!(to = next_arg(args, &args)))
		to = NULL;

	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!spec && !(spec = next_arg(args, &args)))
	{
		userage("kick", "<channel\002|\002*> <nick> [reason]");
		return;
	}
	if (args && *args)
		reason = args;
	else
		reason = get_reason(spec);

	send_to_server("KICK %s %s :%s", chan->channel, spec, reason);
}

void kickban(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	char	*to = NULL, 
		*spec = NULL, 
		*rest = NULL;

	ChannelList *chan;
	NickList *nicks;
	int count = 0;
	int server = from_server;
	int set_ignore = 0;
		
	context;
	
	if (!(to = next_arg(args, &args)))
		to = NULL;

	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;
	if (command)
		set_ignore = !my_stricmp(command, "BKI");
	if (!spec && !(spec = next_arg(args, &args)))
	{
		userage(command?command:"KickBan", "<nick\002|\002nick!user@host>");
		return;
	}
	rest = args;
	if (rest && !*rest)
		rest = NULL;

	nicks = chan->nicks;
	while (nicks)
	{
		if (!my_stricmp(spec, nicks->nick))
		{
			char *temp, *user;
			char *host;
			char *t = NULL;

			malloc_strcpy(&t, nicks->host);
			temp = t;
			if (temp && (*temp == '~'))
				temp++;
			if (temp && (*temp == '+'))
				temp++;
			if (temp && (*temp == '#'))
				temp++;
			user = temp;
			host = strchr(temp, '@');
			*host++ = '\0';

			send_to_server("MODE %s -o+b %s %s", chan->channel, nicks->nick, ban_it(nicks->nick, user, host));
			send_to_server("KICK %s %s :%s", chan->channel, nicks->nick,
				       rest ? rest : get_reason(nicks->nick));
			count++;
			temp = NULL;
			if (command && !my_stricmp(command, "FUCK"))
			{
				malloc_sprintf(&temp, "%s!*%s@%s %s 3 Auto-Shit", nicks->nick, user, host, chan->channel);
				add_shit(NULL, temp, NULL);
				new_free(&temp);
			} else if (set_ignore)
				ignore_nickname(ban_it("*", user, host), IGNORE_ALL, 0);
			new_free(&t);
		}
		
		nicks = nicks->next;
	}
	if (!count)
		add_to_userhost_queue(spec, userhost_ban, "%s %s %s", chan->channel, spec, command ? (!strcmp(command, "FUCK") ? "FUCK": set_ignore ? "BKI":""):"");
}

void ban(command, args, subargs)
char *command;
char *args;
char *subargs;
{
	char	*to = NULL, 
		*spec = NULL, 
		*rest = NULL;
	ChannelList *chan;
	NickList *nicks;
	int server = from_server;
	int found = 0;
	
	context;
	if (!(to = next_arg(args, &args)))
		to = NULL;

	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;

	if (!spec && !(spec = new_next_arg(args, &args)))
	{
		userage("ban", "<nick\002|\002nick!*@*>");
		return;
	}
	rest = args;

	if (rest && !*rest)
		rest = NULL;

	nicks = chan->nicks;

	while (nicks)
	{
		if (!my_stricmp(spec, nicks->nick))
		{
			char *temp;
			char *t = NULL, *host, *user;
			
			malloc_strcpy(&t, nicks->host);
			temp = t;
			while (temp && ((*temp == '~') || (*temp == '+') || (*temp == '-') || (*temp == '#')))
				temp++;
			user = temp;
			host = strchr(temp, '@');
			*host++ = '\0';

			send_to_server("MODE %s -o+b %s %s", chan->channel, nicks->nick, ban_it(nicks->nick, user, host));
			new_free(&t);
			found++;
			
		}
		nicks = nicks->next;
	}
	if (!found)
	{
		if (strchr(spec, '!') && strchr(spec, '@'))
			send_to_server("MODE %s +b %s", chan->channel, spec);
		else
			add_to_userhost_queue(spec, userhost_ban, "%s %s", chan->channel, spec);
	}
}

void banstat(char *command, char *args, char *subargs)
{
char *channel = NULL, *tmp = NULL, *check = NULL;
ChannelList *chan;
BanList *tmpc;
int count = 1;
int server;

	context;
	if (args && *args)
	{
		tmp = next_arg(args, &args);
		if (*tmp == '#' && is_channel(tmp))
			malloc_strcpy(&channel, tmp);
		else
			malloc_strcpy(&check, tmp);
		if (args && *args && channel)
		{
			tmp = next_arg(args, &args);			
			malloc_strcpy(&check, tmp);
		}		
	}

	if ((chan = prepare_command(&server, channel, NO_OP)))
	{
		if (!chan->bans)
		{
			bitchsay("No bans on %s", chan->channel);
			return;
		}
		if ((do_hook(BANS_HEADER_LIST, "%s %s %s %s %s", "#", "Channel", "Ban", "SetBy", "Seconds")))
			put_it("%s", convert_output_format(get_string_var(FORMAT_BANS_HEADER_VAR), NULL));
		for (tmpc = chan->bans; tmpc; tmpc = tmpc->next, count++)
		{
			if (check && (!match(check, tmpc->ban) || !match(tmpc->ban, check)))
				continue;
			if (do_hook(BANS_LIST, "%d %s %s %s %lu", count, chan->channel, tmpc->ban, tmpc->setby?tmpc->setby:get_server_name(from_server), (unsigned long)tmpc->time))
				put_it("%s", convert_output_format(get_string_var(FORMAT_BANS_VAR), "%d %s %s %s %u", count, chan->channel, tmpc->ban, tmpc->setby?tmpc->setby:get_server_name(from_server), tmpc->time));
		}
		new_free(&check);
		new_free(&channel);
	} 
}

/*
 * Stolen from cdcc 1.5 from Sheik/FliEr. With permission.
 */
 
void remove_bans _((char *stuff, char *line))
{
ChannelList *chan;
int count = 1;
BanList *tmpc, *next;
char *banstring = NULL;
int num = 0;
int server = from_server;

	context;
	if (stuff && (chan = prepare_command(&server, stuff, NEED_OP)))
	{
	
		if (!chan->bans)
		{
			bitchsay("No bans on %s", stuff);
			return;
		}

		for (tmpc = chan->bans; tmpc; tmpc = tmpc->next, count++)
			if (!tmpc->sent_unban && (matchmcommand(line, count)))
			{
				malloc_strcat(&banstring, tmpc->ban);
				malloc_strcat(&banstring, " ");
				num++;
				tmpc->sent_unban++;
				if (num % get_int_var(NUM_BANMODES_VAR) == 0)
				{
					send_to_server("MODE %s -%s %s", stuff, strfill('b', num), banstring);
					new_free(&banstring);
					num = 0;
				}
			}
		if (banstring && num)
			send_to_server("MODE %s -%s %s", stuff, strfill('b', num), banstring);
		for (tmpc = chan->bans; tmpc; tmpc = next)
		{
			next = tmpc->next;
			if (tmpc->sent_unban)
			{
				if ((tmpc = (BanList *)remove_from_list((List**)&chan->bans, tmpc->ban)))
				{
					new_free(&tmpc->ban);
					new_free(&tmpc->setby);
					new_free((char **)&tmpc);
					tmpc = NULL;
				}
			}
		}
		new_free(&banstring);
	}	
}

void tban(char *command, char *args, char *subargs)
{
ChannelList *chan;
int count = 1;
BanList *tmpc;
int server;

	context;
	if ((chan = prepare_command(&server, NULL, NEED_OP)))
	{
	
		if (!chan->bans)
		{
			bitchsay("No bans on %s", chan->channel);
			return;
		}
		if ((do_hook(BANS_HEADER_LIST, "%s %s %s %s %s", "#", "Channel", "Ban", "SetBy", "Seconds")))
			put_it("%s", convert_output_format(get_string_var(FORMAT_BANS_HEADER_VAR), NULL));
		for (tmpc = chan->bans; tmpc; tmpc = tmpc->next, count++)
			if (do_hook(BANS_LIST, "%d %s %s %s %lu", count, chan->channel, tmpc->ban, tmpc->setby?tmpc->setby:get_server_name(from_server), (unsigned long)tmpc->time))
				put_it("%s", convert_output_format(get_string_var(FORMAT_BANS_VAR), "%d %s %s %s %u", count, chan->channel, tmpc->ban, tmpc->setby?tmpc->setby:get_server_name(from_server), tmpc->time));
		add_wait_prompt("Which ban to delete (-2, 2-5, ...) ? ", remove_bans, chan->channel, WAIT_PROMPT_LINE);
	} 
}

void set_default_bantype(char value)
{
extern int defban;
int oldbantype = defban;
	context;
	switch(toupper(value))
	{
		case 'B':
			defban = 2;
			break;
		case 'H':
			defban = 3;
			break;
		case 'D':
			defban = 4;
			break;
		case 'S':
			defban = 6;
			break;
		case 'U':
			defban = 5;
			break;
		case 'N':
			defban = 1;
			break;
		default :
			userage("BanType", "<\002N\002ormal|\002B\002etter|\002H\002ost|\002U\002ser|\002D\002omain|\002S\002crew>");
			defban = oldbantype;
			return;
			break;
	}
	bitchsay("BanType set to %s", (defban == 1) ? "\002N\002ormal":(defban ==2)?"\002B\002etter":(defban==3)?"\002H\002ost":(defban==4)?"\002D\002omain":(defban==5)?"\002U\002ser":"\002S\002crew");
}

void bantype (char *command, char *args, char *subargs)
{
	context;
	if (args && *args)
	{
		set_default_bantype(*args);
	} else
		userage("BanType", "<\002N\002ormal|\002B\002etter|\002H\002ost|\002U\002ser|\002D\002omain|\002S\002crew>");
}
