/* 
 *  Copyright Colten Edwards (c) 1996
 */
#include <stdio.h>
#include <ctype.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <unistd.h>

#if defined(sparc) && defined(sun4c)
#include <sys/rusage.h>
#endif


#include "irc.h"
#include "server.h"
#include "edit.h"
#include "crypt.h"
#include "vars.h"
#include "ircaux.h"
#include "lastlog.h"
#include "window.h"
#include "screen.h"
#include "whois.h"
#include "hook.h"
#include "input.h"
#include "ignore.h"
#include "keys.h"
#include "names.h"
#include "alias.h"
#include "history.h"
#include "funny.h"
#include "ctcp.h"
#include "dcc.h"
#include "output.h"
#include "exec.h"
#include "notify.h"
#include "numbers.h"
#include "status.h"
#include "list.h"
#include "timer.h"
#include "userlist.h"
#include "bot.h"
#include "misc.h"
#include "flood.h"
#include "parse.h"
#include "whowas.h"

char	*alias_special_char _((char **, char *, char *, char *, int *));

extern int user_count;
extern int shit_count;
extern int bot_count;
extern time_t start_time;
extern int in_server_ping;

int split_watch = 0;
int serv_action = 0;
int first_time = 0;

LastMsg last_msg[MAX_LAST_MSG] = { { NULL } };
LastMsg last_notice[MAX_LAST_MSG] = { { NULL } };
LastMsg last_servermsg[MAX_LAST_MSG] = { { NULL } };
char *last_sent_msg = NULL;
char *last_sent_notice = NULL;
char *last_sent_topic = NULL;
char *last_sent_wall = NULL;
char *last_topic = NULL;
char *last_wall = NULL;
char *last_invite_channel = NULL;
char *convertstring = NULL;
extern char *last_ctcp;
extern char *last_sent_ctcp;
extern int in_cparse;

#define SPLIT 1

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE ~FALSE
#endif



ChannelList default_statchan = { 0 };
ChannelList *idlechan_list = NULL;

extern NickTab *tabkey_array, *autoreply_array;

CloneList *clones = NULL;

SocketList sockets[FD_SETSIZE] = {{ 0, NULL }};

static char *null  = "(null)";

extern Ignore *ignored_nicks;

char *color_str[] = {
"[0;30m","[0;34m","[0;32m","[0;36m","[0;31m","[0;35m","[0;33m","[0m",
"[1;30m","[1;34m","[1;32m","[1;36m","[1;31m","[1;35m","[1;33m","[1;37m", "[0m",
"[0;40m", "[0;41m", "[0;42m","[0;43m", "[0;44m","[0;45m","[0;46m", "[0;47m",
"[1;40m", "[1;41m", "[1;42m","[1;43m", "[1;44m","[1;45m","[1;46m", "[1;47m",
"[7m", "[1m", "[5m", "[4m"};

irc_server *tmplink = NULL, *server_last = NULL;

#define getrandom(min, max) ((rand() % (int)(((max)+1) - (min))) + (min))

char *awaymsg = NULL;

char *convert_time _((time_t ltime))
{
	time_t  days = 0,hours = 0,minutes = 0,seconds = 0;
	static char buffer[100];

	context;
	*buffer = '\0';
	seconds = ltime % 60;
	ltime = (ltime - seconds) / 60;
	minutes = ltime%60;
	ltime = (ltime - minutes) / 60;
	hours = ltime % 24;
	days = (ltime - hours) / 24;
	sprintf(buffer, "%2ldd %2ldh %2ldm %2lds", days, hours, minutes, seconds);
	return(*buffer ? buffer : empty_string);
}

void do_uptime  _((char *command, char *args, char *subargs))
{
	context;
	put_it("%s",convert_output_format("%GÚÄ[ %WBitchX%gÄ%wClient%gÄ%RStatistics %G]ÄÄÄÄ---%gÄ--ÄÄ%K-%gÄÄÄÄÄ--%GÄ--ÄÄ%K-%gÄÄÄÄÄÄÄ--- %K--%g  -",NULL));
	put_it("%s",convert_output_format("%G| %CClient Version: %W$0 $1","%s %s", irc_version, internal_version));
	put_it("%s",convert_output_format("%G³ %CClient Running Since %W$0-","%s",my_ctime(start_time)));
	put_it("%s",convert_output_format("%G| %CClient Uptime: %W$0-","%s",convert_time(time(NULL)-start_time)));
	put_it("%s",convert_output_format("%G³ %CCurrent UserName: %W$0-","%s", username));
	put_it("%s",convert_output_format("%G: %CCurrent RealName: %W$0-","%s", realname));
	put_it("%s",convert_output_format("%G. %CLast Recv Message: %W$0-","%s",last_msg[0].last_msg?last_msg[0].last_msg:"None"));
	put_it("%s",convert_output_format("%G: %CLast Recv Notice: %W$0-","%s",last_notice[0].last_msg?last_notice[0].last_msg:"None"));
	put_it("%s",convert_output_format("%G. %CLast Sent Msg: %W$0-","%s",last_sent_msg?last_sent_msg:"None"));
	put_it("%s",convert_output_format("%G: %CLast Sent Notice: %W$0-","%s",last_sent_notice?last_sent_notice:"None"));
	put_it("%s",convert_output_format("%G³ %CLast Channel invited to: %R$0-","%s",invite_channel?invite_channel:"None"));
	put_it("%s",convert_output_format("%G| %cTotal Users on Userlist: %K[%R$0%K]","%d",user_count));
	put_it("%s",convert_output_format("%G³ %cTotal Users on Shitlist: %K[%R$0%K]","%d",shit_count));
	put_it("%s",convert_output_format("%G| %cTotal Bots on Botlist:   %K[%R$0%K]","%d",bot_count));
}

/* extern_write -- controls whether others may write to our terminal or not. */
/* This is basically stolen from bsd -- so its under the bsd copyright */
void extern_write  _((char *command, char *args, char *subargs))
{
	char *tty;
	struct stat sbuf;
	const int OTHER_WRITE = 020;
	int on = 0;
	  
	if (!(tty = ttyname(2)))
	{
		yell("Internal error: notify %s", "edwac@sasknet.sk.ca");
		yell("Error in ttyname()");
		return;
	}
	if (stat(tty, &sbuf) < 0)
	{
		yell("Internal error: notify %s", "edwac@sasknet.sk.ca");
		yell("Error in stat()");
		return;
	}
	if (!args || !*args)
	{
		if (sbuf.st_mode & 020)
			bitchsay("Mesg is \002On\002");
		else
			bitchsay("Mesg is \002Off\002");
		return;
	}
	if (!my_stricmp(args, "ON") || !my_stricmp(args, "YES"))
		on = 1;
	else if (!my_stricmp(args, "OFF") || !my_stricmp(args, "NO"))
		on = 0;
	else
	{
		userage("Mesg","<Yes\002|\002No\002|\002On\002|\002Off>");
		return;
	}		
	switch (on)
	{
		case 1 :
			if (chmod(tty, sbuf.st_mode | OTHER_WRITE) < 0)
			{
				yell("Sorry, couldnt set your tty's mode");
				return;
			}
			bitchsay("Mesg is \002On\002");
			break;
		case 0 :
			if (chmod(tty, sbuf.st_mode &~ OTHER_WRITE) < 0)
			{
				yell("Sorry, couldnt set your tty's mode");
				return;
			}
			bitchsay("Mesg is \002Off\002");
			break;
	}
	
}


int check_serverlag _((void *args))
{
	char *servern = (char *)args;
	context;
	if (servern && *servern)
	{
		int i = 0;
		for (i = 0; i < number_of_servers; i++)
		{
			if ((!my_stricmp(servern, get_server_itsname(i)) || !my_stricmp(servern, get_server_name(i))) && is_server_connected(i))
			{
				server_list[i].lag_time = time(NULL);
				send_to_server("PING %lu %s", server_list[i].lag_time, servern);
				in_server_ping++;
				server_list[i].lag = -1;
				break;
			}
		}
	}
	return 0;
}

int timer_unban _((void *args))
{
	char *p = (char *)args;
	char *channel;
	char *nick;

	context;
	channel = next_arg(p, &p);
	nick = next_arg(p, &p);
	send_to_server("MODE %s -b %s", channel, nick);
	new_free(&channel);
	return 0;
}

int timer_idlekick _((void *args))
{
char *channel = (char *)args;
ChannelList *tmp = NULL;
int kick_count = 0;
UserList *user = NULL, *bot = NULL;

	context;
	if (channel && (tmp = lookup_channel(channel, from_server, CHAN_NOUNLINK)) && tmp->chop && tmp->max_idle && tmp->check_idle)
	{
		NickList *nick;
		for (nick = tmp->nicks; nick; nick = nick->next)
		{
			if (!my_stricmp(nick->nick, get_server_nickname(from_server)))
				continue;
			if ((nick->chanop || nick->voice) && !get_int_var(KICK_OPS_VAR))
				continue;
			if ((user=nick->userlist) && check_channel_match(user->channels, channel))
				continue;			
			if ((bot = nick->botlist) && check_channel_match(bot->channels, channel)) 
				continue;
			if (time(NULL) - nick->idle_time >= tmp->max_idle)
			{
				if (kick_count <= get_int_var(MAX_IDLEKICKS_VAR))
				{
					char *p = NULL;
					malloc_sprintf(&p, "%s %s*!*%s", channel, nick->nick, nick->host);
					send_to_server("MODE %s +b %s*!*%s", channel, nick->nick, nick->host);
					send_to_server("KICK %s %s :\002%s\002: (Idle Channel User)", channel, nick->nick, version);
					add_timer("", 60, timer_unban, p, NULL);
				}
				else
					break;
				kick_count++;
			}
		}
	}		
	if (tmp && tmp->max_idle && tmp->check_idle)
		add_timer("", get_int_var(IDLE_CHECK_VAR), timer_idlekick, channel, NULL);
	else
		new_free(&channel);

	return 0;
}

void addidle  _((char *command, char *args, char *subargs))
{
time_t default_idle = 10 * 60;
char *channel = NULL, *p;
time_t seconds = 0;
ChannelList *tmp, *new = NULL;

	context;
	if (args && *args)
	{
		p = next_arg(args, &args);
		if (*p == '#')
			malloc_sprintf(&channel, "%s", p);
		else
			malloc_sprintf(&channel, "#%s", p); 
		if (args && *args)
			seconds = atol(args);

		if (seconds < default_idle)
			seconds = default_idle;

		if (!(new = (ChannelList *)find_in_list((List **)&idlechan_list, channel, 0)))
		{
			new = (ChannelList *)new_malloc(sizeof(ChannelList));
			memcpy(new, &default_statchan, sizeof(ChannelList));
			malloc_strcpy(&new->channel, channel);
			add_to_list((List **)&idlechan_list, (List *)new);
		} 
		new->max_idle = seconds;
		new->check_idle = (my_strnicmp(command, "UN", 2) == 0) ? 0: 1;

		if (!new->check_idle)
		{
			new_free(&channel);
			new->max_idle = 0;
		} 
		else
		{
			if ((tmp = lookup_channel(channel, from_server, CHAN_NOUNLINK)))
			{
				if (new && new->check_idle)
				{
					tmp->max_idle = new->max_idle;
					tmp->check_idle = new->check_idle;
					add_timer("", get_int_var(IDLE_CHECK_VAR), timer_idlekick, channel, NULL);
					bitchsay("Idle checking turned %s for %s %d mins",tmp->check_idle? "On":"Off", channel, tmp->max_idle/60); 
				} 
			} else
				new_free(&channel);
		}
	} else
		userage(!strcmp(command,"UnIdle") ? "UnIdle":"AddIdle", my_stricmp(command,"AddIdle") ? "<channel>" : "<channel> [seconds -default 10min]");
}

void showidle  _((char *command, char *args, char *subargs))
{
ChannelList *tmp;
char *channel = NULL;
int count = 0;
NickList *nick;
time_t ltime;
int server;

	context;
	if (args && *args)
		channel = next_arg(args, &args);
	if (!(tmp = prepare_command(&server, channel, NO_OP)))
		return;


	for (nick = tmp->nicks; nick; nick = nick->next)
	{
		if (!count && do_hook(SHOWIDLE_HEADER_LIST, "%s %d", tmp->channel, tmp->max_idle))
			bitchsay("Idle check for %s Max Idle Time [ %s ]", tmp->channel,convert_time(tmp->max_idle) );
		ltime = time(NULL) - nick->idle_time;
		if (do_hook(SHOWIDLE_LIST, "%s %s %d %d", nick->nick, nick->host, find_user_level(nick->nick, nick->host, tmp->channel), ltime))
			put_it("%-20s Idle: [%s]", nick->nick, convert_time(ltime));
		count++;
	}
}

void kickidle  _((char *command, char *args, char *subargs))
{
char *channel = NULL;
ChannelList *tmp;
int kick_count = 0;
int server;

	context;
	if (args && *args)
		channel = next_arg(args, &args);

	if ((tmp = prepare_command(&server, channel, NEED_OP)) && tmp->max_idle)
	{
		NickList *nick;
		for (nick = tmp->nicks; nick; nick = nick->next)
		{
			if (!my_stricmp(nick->nick, get_server_nickname(from_server)))
				continue;
			if (nick->userlist && check_channel_match(nick->userlist->channels, tmp->channel))
				continue;			
			if (nick->botlist && check_channel_match(nick->botlist->channels, tmp->channel))
				continue;
			if (time(NULL) - nick->idle_time >= tmp->max_idle)
			{
				if (kick_count <= get_int_var(MAX_IDLEKICKS_VAR))
					my_send_to_server(server, "KICK %s %s :\002%s\002: (Idle Channel User)", tmp->channel, nick->nick, version);
				else
					bitchsay(" found idle user %-12s channel %s", nick->nick, tmp->channel);
				kick_count++;
			}
		}
	} else
		userage("KickIdle", "<channel>");
}

void save_idle(FILE *output)
{
ChannelList *chan;
int count = 0;

	context;
	if (!output)
		return;
	if (idlechan_list)
	{
		fprintf(output, "# %s Idle Channel list\n", version);
		for (chan = idlechan_list; chan; chan = chan->next)
		{
			if (chan->max_idle)
			{
				fprintf(output, "ADDIDLE %s %d\n", chan->channel, (int)chan->max_idle); 
				count++;
			}
		}
	}
	if (do_hook(SAVEFILE_LIST, "Idle %d", count))
		bitchsay("Saved %d Idle channels", count);
}

void channel_stats  _((char *command, char *args, char *subargs))
{
ChannelList *new = NULL;
char *channel = NULL;
WhowasChanList *new1 = NULL;
int numircops = 0;
int usershere = 0;
int usersaway = 0;
int chanops = 0;
int chanunop = 0;
char *ircops = NULL;
int server;

NickList *l; long nick_mem = 0, ban_mem = 0; BanList *b;


	context;
	if (args && *args)
		channel = next_arg(args, &args);
	if (!(new = prepare_command(&server, channel, NO_OP)) || (channel && !(new1 = check_whowas_chan_buffer(channel, 0))))
		return;

#if 0
	if (my_strnicmp(channel, "-A", 2)==0)
	{
	
	int stats_ops= 0, stats_dops = 0, stats_bans = 0, stats_unbans = 0;
	int stats_topics = 0, stats_kicks = 0, stats_pubs = 0, stats_parts = 0;
	int stats_signoffs = 0, stats_joins = 0;
	int total_nicks = 0, max_nicks = 0, total_bans = 0, max_bans = 0;
	int stats_sops = 0, stats_sdops = 0, stats_sbans = 0, stats_sunbans = 0;	
		
	NickList *l; 
	BanList *b;
	long nick_mem = 0, chan_mem = 0, ban_mem = 0;
	
		for (tmp = statchan_list; tmp; tmp = tmp->next)
		{

	if (from_server >= 0)
	{
		for (new = server_list[from_server].chan_list; new; new = new->next)
		{		
			for (l = new->nicks; l; l = l->next)
				nick_mem += sizeof(NickList);
			for (b = new->bans; b; b = b->next)
				ban_mem += sizeof(BanList);
			put_it("  MEM usage: Total: %ld bytes   [Nicks %ld b Chan %ld b Bans %ld b]", nick_mem+sizeof(ChannelList)+ban_mem, nick_mem, sizeof(ChannelList), ban_mem);
		}
	}
			chan_mem += sizeof(ChannelList);
			stats_ops += tmp->stats_ops;
			stats_dops += tmp->stats_dops;
			stats_bans += tmp->stats_bans;
			stats_unbans += tmp->stats_unbans;
			stats_topics += tmp->stats_topics;
			stats_kicks += tmp->stats_kicks;
			stats_pubs += tmp->stats_pubs;
			stats_parts += tmp->stats_parts;
			stats_signoffs += tmp->stats_signoffs;
			stats_joins += tmp->stats_joins;

			total_nicks += tmp->totalnicks;
			max_nicks += tmp->maxnicks;
			total_bans += tmp->totalbans;
			max_bans += tmp->maxbans;
			stats_sops += tmp->stats_sops;
			stats_sdops += tmp->stats_sdops;
			stats_sbans += tmp->stats_sbans;
			stats_sunbans += tmp->stats_sunbans;						
		}
		bitchsay("Stats for all channels since %s", my_ctime(start_time));
		put_it("  Time connected %s", convert_time(time(NULL)-start_time));
		put_it("  MEM Usage: Total: %ld bytes   [Nicks %ld b Chan %ld b Bans %ld b]", nick_mem+chan_mem+ban_mem, nick_mem, chan_mem, ban_mem);
		put_it("I've seen - ");
		put_it("   %03d - ops       %03d - de-ops    %03d - bans     %03d - unbans ",stats_ops, stats_dops, stats_bans, stats_unbans);
		put_it("   %03d - topics    %03d - kicks     %03d - publics  %03d - parts", stats_topics, stats_kicks, stats_pubs, stats_parts);
		put_it("   %03d - signoffs  %03d - joins", stats_signoffs, stats_joins);
		put_it("   %03d - serv ops  %03d - serv deop %03d - serv ban %03d - serv unban", stats_sops, stats_sdops,stats_sbans, stats_sunbans);
		put_it("   %03d - total nicks  %03d - max nicks", total_nicks, max_nicks);
		put_it("   %03d - max bans     %03d - total bans", max_bans, total_bans);
		bitchsay("End of All channel stats");
		return;		
	}
#endif
	if (!new && new1)
		new = new1->channellist;
		
	if (new)
	{
		for (l = new->nicks; l; l = l->next)
		{
			nick_mem += sizeof(NickList);
			switch(l->away)
			{
				case 'H':
					usershere++;
					break;
				case 'G':
					usersaway++;
				default:
					break;
			}
			if (l->ircop)
			{
				numircops++;
				malloc_strcat(&ircops, " (");
				malloc_strcat(&ircops, l->nick);
				malloc_strcat(&ircops, ")");
			}
			if (l->chanop)
				chanops++;
			else
				chanunop++;					
		}
		for (b = new->bans; b; b = b->next)
			ban_mem += sizeof(BanList);
	}
	if (!ircops)
		malloc_strcat(&ircops, " ");
	if (do_hook(CHANNEL_STATS_LIST, "%s %s %s %ld %ld %ld %ld %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %s", 
		new->channel, my_ctime(new->channel_create.tv_sec), convert_time(time(NULL)-new->channel_create.tv_sec),
		nick_mem+sizeof(ChannelList)+ban_mem, nick_mem, 
		sizeof(ChannelList),ban_mem, 
		new->stats_ops, new->stats_dops, new->stats_bans, new->stats_unbans,
		new->stats_ops, new->stats_dops, new->stats_bans, new->stats_unbans,
		new->stats_signoffs, new->stats_joins, new->totalbans, new->maxbans,
		new->stats_sops, new->stats_sdops,new->stats_sbans, new->stats_sunbans,
		usershere, usersaway, chanops, chanunop,new->totalnicks,new->maxnicks,
		numircops, ircops))
	{
		bitchsay("Information for channel : %s from %s", new->channel, my_ctime(new->channel_create.tv_sec)); 
		bitchsay("Channel created : %s", convert_time(time(NULL)-new->channel_create.tv_sec));
		put_it("     MEM usage: Total: %ld bytes   [Nicks %ld b Chan %ld b Bans %ld b]", nick_mem+sizeof(ChannelList)+ban_mem, nick_mem, sizeof(ChannelList), ban_mem);
		put_it("Ops        : %03d  De-Ops     : %03d  Bans       : %03d  Unbans     : %03d",new->stats_ops, new->stats_dops, new->stats_bans, new->stats_unbans);
		put_it("Topics     : %03d  Kicks      : %03d  Publics    : %03d  Parts      : %03d", new->stats_topics, new->stats_kicks, new->stats_pubs, new->stats_parts);
		put_it("Signoffs   : %03d  Joins      : %03d  TotalBans  : %03d  MaxBans    : %03d", new->stats_signoffs, new->stats_joins, new->totalbans, new->maxbans);
		put_it("ServOps    : %03d  ServDeop   : %03d  ServBans   : %03d  ServUB     : %03d", new->stats_sops, new->stats_sdops,new->stats_sbans, new->stats_sunbans);
		put_it("Users Here : %03d  Users Away : %03d  Opped      : %03d  Unopped    : %03d" ,usershere, usersaway, chanops, chanunop);
		put_it("TotalNicks : %03d  MaxNicks   : %03d", new->totalnicks,new->maxnicks);
		put_it("IRCops     : %03d%s", numircops, ircops); 

		put_it("  There is %s limit and limit checking is %s", new->limit ? ltoa(new->limit): "no", new->tog_limit?"Enabled":"Disabled");
		put_it("  I %s scanning and idle checking is %s", new->do_scan? "am":"am not",new->check_idle?"Enabled":"Disabled");
		bitchsay("End of channel stats for %s", new->channel);
	}
	new_free(&ircops);

}

void update_stats(int what, char *channel, char *who, ChannelList *chan, int splitter)
{
NickList *nick = NULL;
time_t this_time = time(NULL);
int t = 0;


	context;
	if (!chan || !who || !*who)
		return;
	
	if (!splitter && chan->nicks)
		nick = (NickList *)find_in_list((List **)&chan->nicks, who, 0);

	switch (what)
	{
		case KICKLIST:
		{
			chan->stats_kicks++;
			chan->totalnicks--;
			if (nick) nick->stat_kicks++;
			break;
		}

		case LEAVELIST:
		{
			chan->stats_parts++;
			chan->totalnicks--;
			break;
		}
		case JOINLIST:
		{
			chan->stats_joins++;
			chan->totalnicks++;
			if (chan->totalnicks > chan->maxnicks)
			{
				chan->maxnicks = chan->totalnicks;
				chan->maxnickstime = this_time;
			}
			if (nick)
			{
				if (nick) nick->joincount++;
				if (my_stricmp(who, get_server_nickname(from_server)) &&
					chan->chop && nick && (t = is_other_flood(chan, nick, DEOP_FLOOD)))
				{
					if (chan->set_joinflood && chan->set_kick_on_joinflood)
						send_to_server("KICK %s %s :\002Join flud. (%d in %dsecs)", channel, who, chan->set_kick_on_joinflood, t); 
				} 
			}
			break;
		}
		case CHANNELSIGNOFFLIST:
		{
			chan->stats_signoffs++;
			chan->totalnicks--;
			break;
		}
		case PUBLICLIST:
		case PUBLICOTHERLIST:
		case PUBLICNOTICELIST:
		case NOTICELIST:
		{
			chan->stats_pubs++;
			if (nick) nick->stat_pub++;
			break;
		}
		case TOPICLIST:
		{
			chan->stats_topics++;
			break; 
		}
		case MODEOPLIST:
			if (splitter)
				chan->stats_sops++;
			else
			{
				if (nick) nick->stat_ops++;		
				chan->stats_ops++;
			}
			break;
		case MODEDEOPLIST:
			if (splitter)
				chan->stats_sdops++;
			else
			{
				chan->stats_dops++;		
				if (nick) nick->stat_dops++;
			}

			if (nick)
			{
				if (my_stricmp(who, get_server_nickname(from_server)) && 
					chan->chop && nick && (t = is_other_flood(chan, nick, DEOP_FLOOD)))
				{
					if (chan->set_deop_on_deopflood < chan->set_kick_on_deopflood)
						send_to_server("MODE %s -o %s", channel, who);
					else
						send_to_server("KICK %s %s :\002De-op flud. (%d in %dsecs)", channel, who, chan->set_kick_on_deopflood, chan->set_kickflood_time); 
				} 
			}

			break;
		case MODEBANLIST:
			if (splitter)
				chan->stats_sbans++;
			else
			{
				if (nick) nick->stat_bans++;		
				chan->stats_bans++;
			}
			chan->totalbans++;
			if (chan->stats_bans > chan->maxbans)
			{
				chan->maxbans = chan->stats_bans;
				chan->maxbanstime = this_time;
			}
			break;
		case MODEUNBANLIST:
			if (splitter)
				chan->stats_sunbans++;
			else
			{
				if (nick) nick->stat_unbans++;
				chan->stats_unbans++;
			}
			if (chan->totalbans) chan->totalbans--;
			break;
		default:
			bitchsay("Illegal what %d passed to update_stats", what);
			break;
	}
}

void usage _((char *command, char *args, char *subargs))
{
#if defined(HAVE_GETRUSAGE)
struct rusage r_usage;

	context;
	if ((0 == getrusage(RUSAGE_SELF, &r_usage)))
	{
		/* struct timeval ru_utime; user time used 
                 * struct timeval ru_stime;  system time used 
		 */                
		int secs = r_usage.ru_utime.tv_sec + r_usage.ru_stime.tv_sec;
		if (secs == 0)
			secs =1;
		
		put_it("%s", convert_output_format("%GÚÄ%WBitchX%GÄ%WUsage%GÄ%WStatistics%GÄÄÄÄ%g---%GÄ%g--%GÄÄ%g-%GÄÄÄÄÄÄ%g---%KÄ%g--%KÄÄ%g-%KÄÄÄÄÄÄÄÄ", NULL));
		put_it("%s",convert_output_format("%G| %CCPU %cUsage:  Secs %W$[-2]0%n:%W$[-2]1%n     %K[%CU%cser %W$[-2]2%n:%W$[-2]3   %CS%cystem %W$[-2]4%n:%W$[-2]5%K]","%d %d %d %d %d %d", secs/60,secs%60,r_usage.ru_utime.tv_sec/60, r_usage.ru_utime.tv_sec%60,r_usage.ru_stime.tv_sec/60, r_usage.ru_stime.tv_sec%60));
		put_it("%s",convert_output_format("%g³ %CMEM %cUsage:  MaXRSS %W$0   %cShMem %W$1  %cData %W$2  %cStack %W$3","%l %l %l %l", r_usage.ru_maxrss, r_usage.ru_ixrss, r_usage.ru_idrss,r_usage.ru_isrss));
		put_it("%s",convert_output_format("%g| %CSwaps %W$[-8]0   %CReclaims %W$[-8]1   %CFaults %W$[-8]2","%l %l %l", r_usage.ru_nswap, r_usage.ru_minflt, r_usage.ru_majflt));
		put_it("%s",convert_output_format("%K³ %CBlock %K[%cin  %W$[-8]0  %cout %W$[-8]1%K]","%l %l", r_usage.ru_inblock, r_usage.ru_oublock));
		put_it("%s",convert_output_format("%K: %CMsg   %K[%cRcv %W$[-8]0 %cSend %W$[-8]1%K]","%l %l", r_usage.ru_msgrcv, r_usage.ru_msgsnd));
		put_it("%s",convert_output_format("%K. %CSignals %W$[-8]0   %CContext %cVol. %W$[-8]1   %cInvol %W$[-8]2","%l %l %l", r_usage.ru_nsignals, r_usage.ru_nvcsw, r_usage.ru_nivcsw));
	}
#else
	bitchsay("Lack of getrusage(). This function needed to be disabled on your client");
#endif
}

void add_env _((char * args))
{
char *com;

	context;
	if ((com = next_arg(args, &args)))
	{
		if (!my_stricmp(com, "AWAYMSG"))
		{
			if (from_server >=0)
				malloc_strcpy(&awaymsg, args);
		}
		else
			bitchsay("Unknown ENV var");
	}
	else
		userage("env", "AWAYMSG <args>");
}

char *clear_server_flags _((char *userhost))
{
register char *uh = userhost;
	while(uh && (*uh == '~' || *uh == '#' || *uh == '+' || *uh == '-' || *uh == '=' || *uh == '^'))
		uh++;
	return uh;
}

static int is_reason(char *string)
{
	return (*string == '');
}

#include "dich_conf.h"

aConfItem *host_match = NULL;

char *user_hostserv = NULL;
int stats_k_found = 0;

aConfList       KList1 = { 0, NULL };   /* ordered */
aConfList       KList2 = { 0, NULL };   /* ordered, reversed */
aConfList       KList3 = { 0, NULL };   /* what we can't sort */

void stat_k_userhost(WhoisStuff *stuff, char *nick, char *args)
{
	context;
	if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick))
	{
		say("No match for user %s", nick);
		new_free(&user_hostserv);
		new_free(&host_match->host);
		new_free(&host_match->nick);
		new_free(&host_match->passwd);
		new_free(&host_match->name);
		new_free((char **)&host_match);
		return;
	}
	host_match->host = m_strdup(cluster(stuff->host));
	host_match->nick = m_strdup(stuff->nick);
	host_match->name = m_strdup(clear_server_flags(stuff->user));
	send_to_server("STATS K%s%s", user_hostserv?" ":"", user_hostserv?user_hostserv : "");
	new_free(&user_hostserv);
}

void statkgrep  _((char *command, char *args, char *subargs))
{
char *person;

	context;
	if (args && *args)
	{
		person = next_arg(args, &args);
		if (person && *person)
		{
			stats_k_found = 0;
			host_match = make_conf();
			
			if (strchr(person, '!') && strchr(person, '@'))
			{
				char *user, *host, *nick;
				nick = person;
				user = strchr(person, '!');
				*user++ = 0;
				host = strchr(user, '@');
				*host++ = 0;
				host_match->nick = m_strdup(nick);
				host_match->name = m_strdup(user);
				host_match->host = m_strdup(host);
			}
			else if (strchr(person, '@'))
			{
				char *user, *host;
				user = person;
				host = strchr(person, '@');
				*host++ = 0;
				host_match->nick = m_strdup("*");
				host_match->name = m_strdup(user);
				host_match->host = m_strdup(host);
			}
			else if (strchr(person, '!'))
			{
				char *user, *nick;
				nick = person;
				user = strchr(person, '!');
				*user++ = 0;
				host_match->nick = m_strdup(nick);
				host_match->name = m_strdup(user);
				host_match->host = m_strdup("*");
			}
			else if (strchr(person, '.'))
			{
				host_match->nick = m_strdup("*");
				host_match->name = m_strdup("*");
				host_match->host = m_strdup(person);
			}
			else
			{
				if (args && *args)
					malloc_strcpy(&user_hostserv, args);
				add_to_userhost_queue(person, stat_k_userhost, person);
				return;
			}
			send_to_server("STATS K %s", args && *args ? args : "");
		}		
	}
	else
		userage("FkLine","<nick\002|\002user@hostname> [servername]");
}

int stats_k_grep(char **args)
{
/*
 * arg[1] has the banned host
 * arg[3] has the username.
 * arg[4+] has the reason.
 */
aConfItem *aconf;
char *host;
	/* if host_match is null just return, as we are not doing a grep */
	context;

	if (!args || !args[1] || !args[3] || !host_match)
		return 0;

	aconf = make_conf();
	aconf->host = m_strdup(args[1]);
	aconf->nick = m_strdup(args[2]);
	aconf->name = m_strdup(args[3]);
	PasteArgs(args, 4);
	aconf->passwd = m_strdup(args[4]);
	host = host_field(aconf);
	switch(sortable(host))
	{
		case 0:
			l_addto_conf_list(&KList3, aconf, host_field);
			break;
		case 1:
			addto_conf_list(&KList1, aconf, host_field);
			break;
		case -1:
			addto_conf_list(&KList2, aconf, rev_host_field);
			break;
	}
	new_free(&host);
	stats_k_found++;
	return 1;
}

void stats_k_grep_end(void)
{
int match_k = 0;
char *rev;
aConfList *list;
aConfItem *tmp;
	context;
	if (stats_k_found && host_match)
	{
		rev = (char *) new_malloc(strlen(host_match->host)+1);
		reverse(rev, host_match->host);

		/* Start with hostnames of the form "*word" (most frequent) -Sol */
		list = &KList2;
		while ((tmp = find_matching_conf(list, rev)) != NULL)
		{
			if (tmp->name && (!host_match->name || match(tmp->name, host_match->name)))
			{
				match_k++;
				bitchsay("Found matching %s@%s kline [%s]", tmp->name, tmp->host, tmp->passwd?is_reason(tmp->passwd)?tmp->passwd:null:null);
			}
			list = NULL;
		}

		/* Try hostnames of the form "word*" -Sol */
		list = &KList1;
		while ((tmp = find_matching_conf(list, host_match->host)) != NULL)
		{
			if (tmp->name && (!host_match->name || match(tmp->name, host_match->name)))
			{
				match_k++;
				bitchsay("Found matching %s@%s kline [%s]", tmp->name, tmp->host, tmp->passwd?is_reason(tmp->passwd)?tmp->passwd:null:null);
			}
			list = NULL;
		}

		/* If none of the above worked, try non-sorted entries -Sol */
		list = &KList3;
		while ((tmp = l_find_matching_conf(list, host_match->host)) != NULL)
		{
			if (tmp->host && tmp->name && (!host_match->name || match(tmp->name, host_match->name)))
			{
				match_k++;
				bitchsay("Found matching %s@%s kline [%s]", tmp->name, tmp->host, tmp->passwd?is_reason(tmp->passwd)?tmp->passwd:null:null);
			}
			list = NULL;
		}

		new_free(&rev);
		if (!match_k)
			bitchsay("Found %d klines. No K-Line matching [%s!%s@%s].", stats_k_found, host_match->nick, host_match->name, host_match->host);
		else
			bitchsay("Found %d klines and %d matching %s@%s kline%s", stats_k_found, match_k, host_match->name, host_match->host, plural(stats_k_found));
		new_free(&host_match->host);
		new_free(&host_match->nick);
		new_free(&host_match->passwd);
		new_free(&host_match->name);
		new_free((char **)&host_match);
	}
	new_free(&user_hostserv);
	clear_conf_list(&KList1);
	clear_conf_list(&KList2);
	clear_conf_list(&KList3);
	return;
}

static char newline[2*BIG_BUFFER_SIZE+1];

/* Borrowed with permission from FLiER */
char *stripansicodes(char *line)
{
register char *tstr;
register char *nstr;
int  gotansi=0;

	tstr=line;
	nstr=newline;
	while (*tstr) 
	{
		if (*tstr==0x1B) 
			gotansi=1;
		if (gotansi && isalpha(*tstr)) 
			gotansi = 0;
		else if (!gotansi) 
		{
			*nstr = *tstr;
			nstr++;
		}
		tstr++;
	}
	*nstr = 0;
	return newline;
}

char *stripansi(char *line)
{
register char    *cp;
	strncpy(newline, line, sizeof(newline)-1);        
	for (cp = newline; *cp; cp++)
		if (*cp < 31 && *cp > 13)
			if (*cp != 1 && *cp != 15 && *cp !=22)
				*cp = (*cp & 127) | 64;
	return newline;
}


int check_split(char *nick, char *reason, char *chan)
{
char *bogus = get_string_var(FAKE_SPLIT_PATS_VAR);
	context;
	if (word_count(reason) > 3)
		return 0;
	if (match("%.% %.%", reason) && !strstr(reason, "))") )
	{
		char *host1 = next_arg(reason, &reason);
		char *host2 = next_arg(reason, &reason);
		if (!my_stricmp(host1, host2))
			return 0;
		if (match(host1, "*..*") || match(host2, "*..*"))
			return 0;
		if (bogus)
		{
			char *copy = NULL;
			char *b_check;
			char *temp;
			malloc_strcpy(&copy, bogus);
			temp = copy;
			while((b_check = next_arg(copy, &copy)))
			{
				if (match(b_check, host1) || match(b_check, host2))
				{
					new_free(&temp);
					return 1;
				}
			}
			new_free(&temp);
			return 0;
		}
		return 1;
	} else 
		return 0;
}

void clear_array(NickTab **tmp)
{
NickTab *t, *q;
	context;
	for (t = *tmp; t; )
	{
		q = t->next;
		new_free(&t->nick);
		new_free((char **)&t);
		t = q;
	}
	*tmp = NULL;
}

void clear_tab  _((char *command, char *args, char *subargs))
{
NickTab **tmp = &tabkey_array;
	if (command && *command && !my_stricmp(command, "CLEARAUTO"))
		tmp = &autoreply_array;		
	clear_array(tmp);
}

void userage(char *command, char *use)
{

	context;
	if (do_hook(USAGE_LIST, "%s %s", command, use))
		put_it("%s", convert_output_format(get_string_var(FORMAT_USAGE_VAR), "%s %s", command, use));
}

char *random_str(int min, int max)
{
	int i, ii;
	static char str[BIG_BUFFER_SIZE+1];


	context;
	i = getrandom(min, max);
	for (ii = 0; ii < i; ii++)
		str[ii] = (char) getrandom(97, 122);
	str[ii] = '\0';
	return str;
}

void do_clones (fd_set *rd, fd_set *wr)
{
CloneList *new = NULL;

	context;
	for (new = clones; new; new = new->next)
	{
		if (!new->warn && FD_ISSET(new->socket_num, rd))
		{
			int old_timeout = dgets_timeout(1);
			char buffer[IRCD_BUFFER_SIZE + 1];
			char *str = buffer;
			switch(dgets(str, IRCD_BUFFER_SIZE-2, new->socket_num, NULL))
			{
				case -1:
					break;
				case 0:
					break;
				default:
				if ((buffer[strlen(buffer)-1] == '') || (buffer[strlen(buffer)-1] == '\r') || (buffer[strlen(buffer)-1] == '\n'))
					buffer[strlen(buffer)-1] = 0;
				if ((buffer[strlen(buffer)-1] == '') || (buffer[strlen(buffer)-1] == '\r') || (buffer[strlen(buffer)-1] == '\n'))
					buffer[strlen(buffer)-1] = 0;
				do_hook(CLONE_READ_LIST, "%d %d %s %s", new->socket_num, new->port, new->server, buffer);
			}
			dgets_timeout(old_timeout);
		}
	}
}

void auto_away(unsigned long value)
{
extern void away _((char *, char *, char *));

	context;
	if (get_int_var(AUTO_AWAY_VAR) && !away_set)
	{
		char *msg = NULL;
		if (awaymsg)
			malloc_sprintf(&msg, "%s: [%d mins]", convert_output_format(awaymsg, NULL), get_int_var(AUTO_AWAY_TIME_VAR)/60);
		else
			malloc_sprintf(&msg, "Auto-Away after %d mins", get_int_var(AUTO_AWAY_TIME_VAR)/60);
		away(NULL, msg, NULL);
		new_free(&msg);		
	}
}

char *logfile[] = { "tcl.log", "msg.log", NULL };

/* putlog(level,channel_name,format,...);  */
void putlog(int type, ...)
{
va_list va; 
time_t	t;
char	*format,
	*chname,
	*logfilen = NULL, 
	s[BIG_BUFFER_SIZE+1],
	s1[40],
	s2[BIG_BUFFER_SIZE+1];
FILE	*f; 
	if (!get_int_var(BOT_LOG_VAR))
		return;
	if (!(logfilen = get_string_var(BOT_LOGFILE_VAR)))
		return;
			
	va_start(va, type); 
	t = time(NULL);
	strftime(s1, 30, "%I:%M%p", localtime(&t));
	chname=va_arg(va,char *);
	format=va_arg(va,char *);
	vsprintf(s,format,va);
	
	if (!*s) 
		strcpy(s2,empty_string);
	else 
		sprintf(s2,"[%s] %s",s1,s); 

	if (chname && *chname =='*')
	{
		if ((f=fopen(logfilen, "a+")) != NULL)
		{
			fprintf(f,"%s\n",s2); 
			fclose(f);
		}
	}
}

int rename_file (char *old_file, char **new_file)
{
	char *tmp = NULL, *new_f = NULL;
	char c = 'a';
	FILE *fp;
			

	context;
	if (get_string_var(DCC_DLDIR_VAR))
		malloc_sprintf(&tmp, "%s/%%c%s", get_string_var(DCC_DLDIR_VAR), *new_file);
	else
		malloc_sprintf(&tmp, "%%c%s",*new_file); 
	malloc_sprintf(&new_f, tmp, c);
	while ((fp = fopen(new_f, "r")) != NULL)
	{
		fclose(fp);
		c++;
		sprintf(new_f, tmp, c);
	}
	if (fp != NULL)
		fclose(fp);
	new_free(&tmp);
	new_free(&new_f);
	malloc_sprintf(new_file, "%c%s", c, *new_file);
	return 0;
}

int isme(char *nick)
{
	return ((my_stricmp(nick, get_server_nickname(from_server)) == 0) ? 1 : 0);
}

void clear_link(irc_server **serv1)
{
	irc_server *temp = *serv1, *hold;

	while (temp != NULL)
	{
		hold = temp->next;
		new_free(&temp->name);
		new_free(&temp->link);
		new_free(&temp->time);
		new_free((char **) &temp);
		temp = hold;
	}
	*serv1 = NULL;
}

void add_server(irc_server **serv1, char *channel, char *arg, int hops, char *time)
{
	irc_server *serv2;
	serv2 = (irc_server *) new_malloc(sizeof (irc_server));
	serv2->next = *serv1;
	malloc_strcpy(&serv2->name, channel);
	malloc_strcpy(&serv2->link, arg);
	serv2->hopcount = hops;
	serv2->time = m_strdup(time);
	*serv1 = serv2;
}

int find_server(irc_server *serv1, char *channel)
{
	irc_server *temp;

	for (temp = serv1; temp; temp = temp->next)
	{
		if (!my_stricmp(temp->name, channel))
			return TRUE;
	}
	return FALSE;
}

void parse_364(char *channel, char *args, char *subargs)
{
	if (!*channel || !*args)
		return;

	add_server(&tmplink, channel, args, atol(subargs), update_clock(GET_TIME));
}

void parse_365(char *channel, char *args, char *subargs)
{
	irc_server *serv1;

	for (serv1 = server_last; serv1; serv1 = serv1->next)
	{
		if (!find_server(tmplink, serv1->name))
		{
			if (!(serv1->status & SPLIT))
				serv1->status = SPLIT;
			if (serv1->count)
				continue;
			serv1->time = m_strdup(update_clock(GET_TIME));
			if (do_hook(LLOOK_SPLIT_LIST, "%s %s %d %s", serv1->name, serv1->link, serv1->hopcount, serv1->time))
				put_it("%s", convert_output_format(get_string_var(FORMAT_NETSPLIT_VAR), "%s %s %s %d", serv1->time, serv1->name, serv1->link, serv1->hopcount));
			serv1->count++;
		}
		else
		{
			if (serv1->status & SPLIT)
			{
				serv1->status = ~SPLIT;
				if (do_hook(LLOOK_JOIN_LIST, "%s %s %d", serv1->name, serv1->link, serv1->hopcount, serv1->time))
					put_it("%s", convert_output_format(get_string_var(FORMAT_NETJOIN_VAR), "%s %s %s %d", serv1->time, serv1->name, serv1->link, serv1->hopcount));
				serv1->count = 0;
			}
		}
	}
	for (serv1 = tmplink; serv1; serv1 = serv1->next)
	{
		if (!find_server(server_last, serv1->name)) {
			if (first_time == 1)
			{
				if (do_hook(LLOOK_ADDED_LIST, "%s %s %d", serv1->name, serv1->link, serv1->hopcount))
					put_it("%s", convert_output_format(get_string_var(FORMAT_NETADD_VAR), "%s %s %s %d", serv1->time, serv1->name, serv1->link, serv1->hopcount));
				serv1->count = 0;
			}
			add_server(&server_last, serv1->name, serv1->link, serv1->hopcount, update_clock(GET_TIME));
		}
	}
	first_time = 1;
	clear_link(&tmplink);
}

/*
 * find split servers we hope 
 */
void linklook  _((char *command, char *args, char *subargs))
{
struct server_split *serv = server_last;
int count;
	if (serv == NULL)
	{
		bitchsay("Link Look not active!");
		return;
	}
	
	count = 0;
	while (serv)
	{
		if (serv->status & SPLIT)
		{
			if (!count)
				put_it("%s", convert_output_format(get_string_var(FORMAT_NETSPLIT_HEADER_VAR), "%s %s %s %s", "time","server","uplink","hops"));
			if (do_hook(LLOOK_SPLIT_LIST, "%s %s %d", serv->name, serv->link, serv->hopcount))
				put_it("%s", convert_output_format(get_string_var(FORMAT_NETSPLIT_VAR), "%s %s %s %d", serv->time, serv->name, serv->link, serv->hopcount));
			count++;
		}
		serv = serv->next;
	}
	if (count)
		bitchsay("There %s %d split servers", (count == 1) ? "is": "are", count);
	else
		bitchsay("No split servers found");
}

void send_last_type(char *channel, char *text, char *command)
{
	if (channel && is_channel(channel) && im_on_channel(channel))
	{
		if (command)
			my_send_to_server(from_server, "TOPIC %s :%s", channel, text);
		else
			send_text(channel, stripansicodes(text), NULL, 1, 1);
	}
	else if (channel)
		send_text(channel, text, "PRIVMSG", 1, 1);
}

int setup_last_type(char *args, char **chan)
{
	char *channel = NULL;
	char *tmp = NULL;
	int count = 0;
	tmp = next_arg(args, &args);
	if (!tmp)
		channel = get_channel_by_refnum(0);
	else
	{
		channel = get_channel_by_refnum(0);
		if (is_channel(tmp))
		{
			channel = tmp;
			tmp = next_arg(args, &args);

			if (tmp && isdigit(*tmp))
				count = atol(tmp);

		} else if (isdigit(*tmp))
			count = atol(tmp);
		else
			channel = tmp;
		if (count > MAX_LAST_MSG)
			count = 0;
	}
	*chan = channel;
	return count;
}
void do_dirsentlasttopic  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_sent_topic)
		send_last_type(channel, last_sent_topic, command);
}

void do_dirsentlastwall  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_sent_wall)
		send_last_type(channel, last_sent_wall, command);
}

void do_dirlasttopic  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_topic)
		send_last_type(channel, last_topic, command);
}

void do_dirlastwall  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_wall)
		send_last_type(channel, last_wall, command);
}

void do_dirlastctcp  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_sent_ctcp)
		send_last_type(channel, last_sent_ctcp, command);
}

void do_dirlastctcpreply  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_ctcp)
		send_last_type(channel, last_ctcp, command);
}

void do_dirlastinvite  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (invite_channel)
		send_last_type(channel, convert_output_format(get_string_var(FORMAT_INVITE_VAR),"%s %s", update_clock(GET_TIME), last_invite_channel), command);
}

void do_dirlastnotice  _((char *command, char *args, char *subargs))
{
	char *channel = NULL; int count = -1;
	if (args && *args && *args == '-' && *(args+1) == 'l')
	{
		for(count = 0; count < MAX_LAST_MSG; count++)
		{
			if (!last_notice[count].last_msg)
				break;
			put_it("%2d %s", count, convert_output_format(get_string_var(FORMAT_RELN_VAR), "%s %s %s %s %s", last_notice[count].time, last_notice[count].from, last_notice[count].uh, last_notice[count].to, last_notice[count].last_msg));
		}
		return;
	}
	count = setup_last_type(args, &channel);
	if (count != -1 && last_notice[count].last_msg)
		send_last_type(channel, convert_output_format(get_string_var(FORMAT_RELN_VAR), "%s %s %s %s %s", last_notice[count].time, last_notice[count].from, last_notice[count].uh, last_notice[count].to, last_notice[count].last_msg), command);
}

void do_dirlistmsg  _((char *command, char *args, char *subargs))
{
/* Sorry Wangel. had to hack your code you sent to me. */
int count;
	for(count = 0; count < MAX_LAST_MSG; count++)
	{
		if (!last_msg[count].last_msg)
			break;
		put_it("%2d %s", count, convert_output_format(get_string_var(FORMAT_REL_VAR), "%s %s %s %s %s", last_msg[count].time, last_msg[count].from, last_msg[count].uh, last_msg[count].to, last_msg[count].last_msg));
	}
	return;
}

void do_dirlastmsg  _((char *command, char *args, char *subargs))
{
int count = -1; char *channel = NULL;
	if (args && *args && *args == '-' && *(args+1) == 'l')
	{
		for(count = 0; count < MAX_LAST_MSG; count++)
		{
			if (!last_msg[count].last_msg)
				break;
			put_it("%2d %s", count, convert_output_format(get_string_var(FORMAT_RELM_VAR), "%s %s %s %s %s", last_msg[count].time, last_msg[count].from, last_msg[count].uh, last_msg[count].to, last_msg[count].last_msg));
		}
		return;
	}
	count = setup_last_type(args, &channel);
	if (count != -1 && last_msg[count].last_msg)
		send_last_type(channel, convert_output_format(get_string_var(FORMAT_RELM_VAR), "%s %s %s %s %s", last_msg[count].time, last_msg[count].from, last_msg[count].uh, last_msg[count].to, last_msg[count].last_msg), command);
}

void do_dirlastserver  _((char *command, char *args, char *subargs))
{
int count = -1; char *channel = NULL;
	if (args && *args && *args == '-' && *(args+1) == 'l')
	{
		for (count = 0; count < MAX_LAST_MSG; count++)
		{
			if (!last_servermsg[count].last_msg)
				break;
			put_it("%2d %s", count, convert_output_format(get_string_var(FORMAT_RELS_VAR), "%s %s", last_servermsg[count].time, last_servermsg[count].last_msg));
		}
		return;
	}
	count = setup_last_type(args, &channel);
	if (count != -1 && last_servermsg[count].last_msg)
		send_last_type(channel, convert_output_format(get_string_var(FORMAT_RELS_VAR), "%s %s", last_servermsg[count].time, last_servermsg[count].last_msg), command);
}

void do_dirsentlastnotice  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_sent_notice)
		send_last_type(channel, convert_output_format(get_string_var(FORMAT_RELSN_VAR), "%s",last_sent_notice), command);
}

void do_dirsentlastmsg  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;

	if (args && *args)
		channel = next_arg(args, &args);
	else
		channel = get_channel_by_refnum(0);
	if (last_sent_msg)
		send_last_type(channel, convert_output_format(get_string_var(FORMAT_RELSM_VAR), "%s",last_sent_msg), command);
}

/*
 * Wallop   Sends NOTICE to all ops of Current Channel!       
 */
void ChanWallOp  _((char *command, char *args, char *subargs))
{
	char *channel = NULL;
	char *chops = NULL;
	char *include = NULL;
	char *exclude = NULL;
	ChannelList *chan;
	NickList *tmp;
	char	buffer[BIG_BUFFER_SIZE + 1];
	
	if (!args || (args && !*args))
	{
		userage("WALLMSG","[-nick] [+nick] <msg>");
		return;
	}
	if (get_channel_by_refnum(0))
	{
		int count = 0;
		int i = 0;
		char *nick = NULL;
		malloc_strcpy(&channel, get_channel_by_refnum(0));
		chan = lookup_channel(channel, curr_scr_win->server, 0);
		while (args && (*args == '-' || *args == '+'))
		{
			nick = next_arg(args, &args);
			if (*nick == '-')
			{
				malloc_strcat(&exclude, nick+1);
				malloc_strcat(&exclude, " ");
			} 
			else 
			{
				malloc_strcat(&include, nick+1);
				malloc_strcat(&include, " ");
			}
		}
		message_from(channel, LOG_NOTICE);
		sprintf(buffer, "[\002BX-Wall\002/\002%s\002] %s", channel, args);

		for (tmp = chan->nicks; tmp; tmp = tmp->next)
		{
			if (!my_stricmp(tmp->nick, nickname))
				continue;
			if (exclude && stristr(exclude, tmp->nick))
				continue;
			if (tmp->chanop == 1 || (include && stristr(include, tmp->nick)))
			{
				if (chops)
					malloc_strcat(&chops, ",");
				malloc_strcat(&chops, tmp->nick);
				count++;
			}
			if (count >= 8 && chops)
			{
				send_to_server("%s %s :%s", "NOTICE", chops, buffer);
				i+=count;
				count = 0;
				new_free(&chops);
			}
		}
		i+=count;
		if (chops)
			send_to_server("%s %s :%s", "NOTICE", chops, buffer);
		if (i) 
		{
			put_it("%s", buffer);
			malloc_strcpy(&last_sent_wall, buffer);
		}
		message_from(NULL, LOG_CRAP);
	}
	else
		say("No Current Channel for this Window.");
	new_free(&include);
	new_free(&channel);
	new_free(&chops);
	new_free(&exclude);
}

void log_toggle(int flag)
{
	char *logfile;

	if (((logfile = get_string_var(MSGLOGFILE_VAR)) == NULL) || !get_string_var(CTOOLZ_DIR_VAR))
	{
		bitchsay("You must set the MSGLOGFILE and CTOOLZ_DIR variables first!");
		set_int_var(MSGLOG_VAR, 0);
		return;
	}
	logmsg(0, LOG_CURRENT, NULL, NULL, NULL, NULL, flag ? 1 : 2);
}

void not_on_a_channel(Window *win)
{
	if (win)
		message_to(win->refnum);
	bitchsay("You're not on a channel!");
	message_to(0);
}

int are_you_opped(char *channel)
{
	return is_chanop(channel, get_server_nickname(from_server));
}

void error_not_opped(char *channel)
{
	say("You're not opped on %s", channel);
}

int freadln(FILE *stream, char *lin)
{
	char *p;

	do
		p = fgets(lin, BIG_BUFFER_SIZE, stream);
	while (p && (*lin == '#'));

	if (!p)
		return 0;
	chop(lin, 1);
	return 1;
}

char *randreason(char *filename)
{
	int count, min, i;
	FILE *bleah;
	static char buffer[BIG_BUFFER_SIZE + 1];

	min = 1;
	count = 0;

	buffer[0] = '\0';

	if (!(bleah = fopen(filename, "r")))
		return NULL;

	while (!feof(bleah))
		if (freadln(bleah, buffer))
			count++;
	fseek(bleah, 0, 0);
	i = getrandom(1, count);
	count = 0;

	while (!feof(bleah) && (count < i))
		if (freadln(bleah, buffer))
			count++;
	fclose(bleah);

	if (*buffer)
		return buffer;
	return NULL;
}

char *get_reason(char *nickname)
{
	char *temp, *p;
	char *filename = NULL;
static char reason[BIG_BUFFER_SIZE + 1];
	*reason = '\0';	
	malloc_sprintf(&filename, "%s/%s.%s", get_string_var(CTOOLZ_DIR_VAR), version, "reasons");
	p = expand_twiddle(filename);
	temp = randreason(p);
	new_free(&filename);
	if ((!temp || !*temp) && get_string_var(DEFAULT_REASON_VAR))
		temp = get_string_var(DEFAULT_REASON_VAR);
	strncpy(reason, stripansicodes(convert_output_format(temp, "%s %s", nickname? nickname: "error", get_server_nickname(from_server))), sizeof(reason)-1);
	return reason;
}

char *get_signoffreason(char *nickname)
{
	char *temp, *p;
	char *filename = NULL;
static char reason[BIG_BUFFER_SIZE + 1];

	*reason = '\0';	

	malloc_sprintf(&filename, "%s/%s.%s", get_string_var(CTOOLZ_DIR_VAR), version, "quit");
	p = expand_twiddle(filename);
	temp = randreason(p);
	new_free(&filename);

	if (!temp || !*temp)
		temp = "$0 has no reason";

	strncpy(reason, stripansicodes(convert_output_format(temp, "%s %s", nickname? nickname: "error", get_server_nickname(from_server))), sizeof(reason)-1);
	return reason;
}

char *do_nslookup(char *host)
{
struct hostent *temp;
struct in_addr temp1;
	if (!host)
		return NULL;
	
	if (isdigit(*(host + strlen(host) - 1)))
	{
		temp1.s_addr = inet_addr(host);
		alarm(1);
		temp = gethostbyaddr((char *)&temp1.s_addr, sizeof(temp1.s_addr), AF_INET);
		alarm(0);
	}
	else
	{
		alarm(1);
		temp = gethostbyname(host);
		alarm(0);
	}
	if (do_hook(NSLOOKUP_LIST, "%s %s %s", host, temp?temp->h_name:"", temp?(char *)inet_ntoa(*(struct in_addr *)temp->h_addr):""))
	{
		if (!temp)
			bitchsay("Error looking up %s", host);
		else
			bitchsay("%s is %s (%s)", host, temp->h_name, (char *)inet_ntoa(*(struct in_addr *)temp->h_addr));
	}
	return temp ? temp->h_name: host;
}

void do_newuser  _((char *command, char *args, char *subargs))
{
char *newusername;
	newusername = next_arg(args, &args);
	if (newusername)
	{
#ifdef IDENT_FAKE
		FILE *outfile;
		char *p = NULL, *q = NULL;
		malloc_sprintf(&p, "~/%s", get_string_var(IDENT_HACK_VAR));
		q = expand_twiddle(p);
		outfile = fopen(q,"w");
		fprintf(outfile,"%s", newusername);
		fclose(outfile);
#endif
		if (!newusername)
			newusername = empty_string;
		strmcpy(username, newusername, NAME_LEN);
		if (args && *args)
		{
#ifdef IDENT_FAKE
                        FILE *outfile;
                        strmcpy(realname, args, REALNAME_LEN);
                        outfile = fopen(q,"w");
                        fprintf(outfile,"%s", newusername);
                        fclose(outfile);
#else
			strmcpy(realname, args, REALNAME_LEN);
#endif
		}
#ifdef IDENT_FAKE
		new_free(&p); new_free(&q);
#endif
		reconnect_cmd(NULL, newusername, NULL);
	}
	else
		userage("newuser", "<username> [ircname]");
}

char *rights(string, num)
char *string;
int num;
{
	if (strlen(string) < num)
		return string;
	return (string + strlen(string) - num);
}

int numchar(char *string, char c)
{
	int num = 0;

	while (*string)
	{
		if (tolower(*string) == tolower(c))
			num++;
		string++;
	}
	return num;
}

char *cluster _((char *hostname))
{
	static char result[BIG_BUFFER_SIZE + 1];
	char temphost[BIG_BUFFER_SIZE + 1];
	char *host;

	if (!hostname)
		return NULL;
	host = temphost;
	*result = 0;
	memset(result, 0, sizeof(result));
	memset(temphost, 0, sizeof(temphost));
	if (strchr(hostname, '@'))
	{
		if (*hostname == '~')
			hostname++;
		strcpy(result, hostname);
		*strchr(result, '@') = '\0';
		if (strlen(result) > 9)
		{
			result[8] = '*';
			result[9] = '\0';
		}
		strcat(result, "@");
		if (!(hostname = strchr(hostname, '@')))
			return NULL;
		hostname++;
	}
	strcpy(host, hostname);

	if (*host && isdigit(*(host + strlen(host) - 1)))
	{
	/* Thanks icebreak for this small patch which fixes this function */
                int i;
                char *tmp;
                char count=0;

                tmp = host;
                while((tmp-host)<strlen(host))
                {
	                if((tmp=strchr(tmp,'.'))==NULL) 
				break;
        	        count++;
                	tmp++;
                }
                tmp = host;
                for (i = 0; i < count; i++)
                        tmp = strchr(tmp, '.') + 1;
                *tmp = '\0';
                strcat(result, host);
                strcat(result, "*");
	}
	else
	{
		char *tmp;
		int num;

		num = 1;
		tmp = rights(host, 3);
		if (my_stricmp(tmp, "com") &&
		    my_stricmp(tmp, "edu") &&
		    my_stricmp(tmp, "net") &&
		    (stristr(host, "com") ||
		     stristr(host, "edu")))
			num = 2;
		while (host && *host && (numchar(host, '.') > num))
		{
			if ((host = strchr(host, '.')) != NULL)
				host++;
			else
				return (char *) NULL;
		}
		strcat(result, "*");
		if (my_stricmp(host, temphost))
			strcat(result, ".");
		strcat(result, host);
	}
	return result;
}

void set_socket_read (fd_set *rd, fd_set *wr)
{
register int i;
	for (i = 0; i < FD_SETSIZE; i++)
	{
		if (sockets[i].is_open)
			FD_SET(i, rd);
	}
	
}

void scan_sockets(fd_set *rd, fd_set *wr)
{
register int i;
	for (i = 0; i < FD_SETSIZE; i++)
	{
		if (sockets[i].is_open && FD_ISSET(i, rd))
			(sockets[i].func) (i);
		if (sockets[i].is_open && FD_ISSET(i, wr))
			(sockets[i].func) (i);
	}
}

extern int dgets_errno;

void read_netfinger(int s)
{
char tmpstr[BIG_BUFFER_SIZE+1];
register unsigned char *p = tmpstr;
	*tmpstr = 0;
	dgets_timeout(1);
	switch(dgets(tmpstr, BIG_BUFFER_SIZE, s, NULL))
	{
		case -1:
			break;
		case 0:
			FD_CLR(s, &readables);
			sockets[s].func = NULL;
			sockets[s].is_open = 0;
			close(s);
			if (dgets_errno == -1)
				bitchsay("Remote closed connection");
			else
				bitchsay("%s", strerror(dgets_errno));
			break;
		default:
		{
			chop(tmpstr, 1);
			while (*p)
			{
				switch(*p)
				{
					case 0210:
					case 0211:
					case 0212:
					case 0214:
						*p -= 0200;
						break;
					case '\n':
					case '\r':
						*p = '\0';
						break;
					default:
						if (!isprint(*p))
							*p = (*p & 0x7f) | 0x40;
						break;
				}
				p++;
			}
			put_it("%s", tmpstr);
		}
	}
	dgets_timeout(0);
}

void netfinger _((char *name))
{
	char *host = NULL;
	unsigned short port = 79;
	int s;
	
	if (name)
		host = rindex(name, '@');
	*host++ = 0;
	if (!name || !*name || !host || !*host)
	{
		say("Invalid user@host.");
		return;
	}
	
	if ((s = connect_by_number(host, &port, SERVICE_CLIENT, PROTOCOL_TCP, 0)) < 0)
	{
		bitchsay("Finger connect error on %s@%s", name, host);
		return;
	}
	write(s, name, strlen(name));
	write(s, "\r\n", 2);

	sockets[s].func = read_netfinger;
	sockets[s].is_open = s;
	return;
}

void userhost_nsl(WhoisStuff *stuff, char *nick, char *args)
{
	char *nsl;

	if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick))
	{
		say("No information for %s", nick);
		return;
	}
	nsl = do_nslookup(stuff->host);
}

void userhost_ignore _((WhoisStuff *stuff, char *nick1, char *args))
{
	char *p;
	char *nick = NULL, *user = NULL, *host = NULL;
	int old_window_display;
	char ignorebuf[BIG_BUFFER_SIZE+1];
	Ignore *igptr, *igtmp;
	WhowasList *whowas;
	
	if (!stuff || !stuff->nick || !nick1 || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick1))
	{
                if ((whowas = check_whowas_nick_buffer(nick1, args, 0)))
		{
			bitchsay("Using WhoWas info for %s of %s ", args, nick1);
			user = host; host = strchr(host, '@'); *host++ = 0;
			nick = whowas->nicklist->nick;
		}
		else
		{                                                                                                                
			say("No match for user %s", nick1);
			return;
		}
	}
	else
	{
		user = clear_server_flags(stuff->user);
		host = stuff->host; nick = stuff->nick;
	}

	if (!my_stricmp(args, "+HOST"))
		sprintf(ignorebuf, "*!*@%s ALL -CRAP -PUBLIC", cluster(host));
	else if (!my_stricmp(args, "+USER"))
		sprintf(ignorebuf, "*%s@%s ALL -CRAP -PUBLIC", user, cluster(host));
	else if (!my_stricmp(args, "-USER") || !my_stricmp(args, "-HOST"))
	{
		int found = 0;
		if (!my_stricmp(args, "-HOST"))
			sprintf(ignorebuf, "*!*@%s", cluster(host));
		else
			sprintf(ignorebuf, "%s!%s@%s", nick, user, host);
		igptr = ignored_nicks;
		while (igptr != NULL)
		{
			igtmp = igptr->next;
			if (match(igptr->nick, ignorebuf) ||
			    match(nick, igptr->nick))
			{
				sprintf(ignorebuf, "%s NONE", igptr->nick);
				old_window_display = window_display;
				window_display = 0;
				ignore(NULL, ignorebuf, ignorebuf);
				window_display = old_window_display;
				bitchsay("Unignored %s!%s@%s", nick, user, host);
				found++;
			}
			igptr = igtmp;
		}
		if (!found)
			bitchsay("No Match for ignorance of %s", nick);
		return;
	}
	old_window_display = window_display;
	window_display = 0;
	ignore(NULL, ignorebuf, ignorebuf);
	window_display = old_window_display;

	if ((p = strchr(ignorebuf, ' ')))
		*p = 0;
	say("Now ignoring ALL except CRAP and PUBLIC from %s", ignorebuf);
	return;
}

void reset  _((char *command, char *args, char *subargs))
{
	refresh_screen(0, NULL);
}

void do_flood  _((char *command, char *args, char *subargs))
{
	char buffer[BIG_BUFFER_SIZE + 1];
	char *to = NULL, *what = NULL, *numb = NULL;
	int num = 0, repeat = 2;

	if (!(to = next_arg(args, &args)))
	{
		say("D'oh!  No object specified for the flood.");
		return;
	}
	else if (!(what = next_arg(args, &args)))
		;
	else
		numb = next_arg(args, &args);

	if (!strcmp(to, "*"))
	{
		if ((to = get_channel_by_refnum(0)) == NULL)
		{
			not_on_a_channel(curr_scr_win);
			return;
		}
	}
	
	what = what ? upper(what) : "\007";
	repeat = (numb && *numb) ? my_atol(numb) : 2;
	if (repeat < 0)
		repeat = 2;

	sprintf(buffer, "PRIVMSG %s :", to);
	num = (500 - strlen(buffer)) / (strlen(what) + 2);
	while (num--)
	{
		strcat(buffer, "\001");
		strcat(buffer, what);
		strcat(buffer, "\001");
	}
	while (repeat--)
	{
		say("Flooded the hell out of %s with %s", to, *what=='\007'? "BEL": what);
		send_to_server(buffer);
	}
}


extern char *channel_key _((char *));

void cycle _((char *command, char *args, char *subargs))
{
	char *to = NULL;
	int server;
	ChannelList *chan;
	
	if (args && args)
		to = next_arg(args, &args);
		
	if (!(chan = prepare_command(&server, to, NO_OP)))
		return;		
	my_send_to_server(server, "PART %s", chan->channel);
	my_send_to_server(server, "JOIN %s%s%s", chan->channel, chan->key?" ":"", chan->key?chan->key:"");
}

void bomb _((char *command, char *args, char *subargs))
{
	char *to, *tag;

	if ((to = next_arg(args, &args)))
	{
		if (!strcmp(to, "*"))
			if ((to = get_channel_by_refnum(0)) == NULL)
			{
				not_on_a_channel(curr_scr_win);
				return;
			}
		if (!(tag = next_arg(args, &args)))
			tag = "UTC";
		say("Bombed %s with %s %s", to, tag, (args && *args) ? args : "");
		if (args && *args)
			send_to_server("NOTICE %s :\001%s %s\001", to, tag, args);
		else
			send_to_server("NOTICE %s :\001%s\001", to, tag);
	}
	else
		say("No object specified to bomb");
}

void nslookup _((char *command, char *args, char *subargs))
{
	char *host, *hostname;

	if ((host = next_arg(args, &args)))
	{
		bitchsay("Checking tables...");
		if (!strchr(host, '.'))
			add_to_userhost_queue(host, userhost_nsl, "%s", host);
		else
			hostname = do_nslookup(host);
	}
	else
		userage("nslookup", "[nick|host]");
}

void newnick _((char *command, char *args, char *subargs))
{
	char *newnick, *newusername;

	if ((newnick = next_arg(args, &args)) &&
	    (newusername = next_arg(args, &args)))
		do_newuser(newnick, newusername, args);
	else
		say("You must specify a nick and username");
}


void newuser  _((char *command, char *args, char *subargs))
{
	char *newusername;

	if ((newusername = next_arg(args, &args)))
		do_newuser(NULL, newusername, args);
	else
		say("You must specify a username.");
}

void start_finger _((WhoisStuff *stuff, char *nick, char *args))
{
	char *finger_userhost = NULL;

	if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick))
	{
		say("No information for %s", nick);
		return;
	}

	malloc_sprintf(&finger_userhost, "%s@%s", stuff->user, stuff->host);
	if (*finger_userhost == '~' || *finger_userhost == '#')
	{
		say("Launching finger for %s (%s)", nick, finger_userhost+1);
		netfinger(finger_userhost+1);
		new_free(&finger_userhost);
		return;
	}
	say("Launching finger for %s (%s)", nick, finger_userhost);
	netfinger(finger_userhost);
	new_free(&finger_userhost);
}

void finger  _((char *command, char *args, char *subargs))
{
	char *userhost;

	if ((userhost = next_arg(args, &args)))
	{
		if (!strchr(userhost, '@'))
		{
			add_to_userhost_queue(userhost, start_finger, userhost);
			return;
		}
		netfinger(userhost);
	}
	else
		bitchsay("Please specify a user@host or nick to finger.");
}

void do_ig  _((char *command, char *args, char *subargs))
{
	char *nickname;
	static char ignore_type[6];
	int got_ignore_type = 0;
	if (!args || !*args)
		goto bad_ignore;

	while ((nickname = next_arg(args, &args)))
	{
		if (!nickname || !*nickname)
			goto bad_ignore;
		if (*nickname == '-' || *nickname == '+')
		{
			if (!my_stricmp(nickname, "-USER") || !my_stricmp(nickname, "+HOST") || !my_stricmp(nickname, "+USER") || !my_stricmp(nickname, "-HOST"))
				strcpy(ignore_type, nickname);
			if (!args || !*args)
				goto bad_ignore;
			got_ignore_type ++;
			continue;
		}
		else if (!got_ignore_type)
		{
			if (command && !my_stricmp(command, "IGH"))
				strcpy(ignore_type, "+HOST");
			else if (command && !my_stricmp(command, "IG"))
				strcpy(ignore_type, "+USER");
			if (command && !my_stricmp(command, "UNIGH"))
				strcpy(ignore_type, "-HOST");
			else if (command && !my_stricmp(command, "UNIG"))
				strcpy(ignore_type, "-USER");
		}
		add_to_userhost_queue(nickname, userhost_ignore, ignore_type);
	}
	return;
bad_ignore:
	userage(command, "+/-USER|+/-HOST nick|nick1,nick2..");
}



void users  _((char *command, char *args, char *subargs))
{
	ChannelList *chan;
	NickList *nicks;
	char *to, *spec, *rest, *temp1;
	char modebuf[BIG_BUFFER_SIZE + 1];
	char msgbuf[BIG_BUFFER_SIZE +1];
	int count, ops, msg;
	int hook = 0;
	int server; 
	
	rest = NULL;
	spec = NULL;
	temp1 = NULL;
	*msgbuf = 0;
	*modebuf = 0;
		
	if (!(to = next_arg(args, &args)))
		to = NULL;
	if (to && !is_channel(to))
	{
		spec = to;
		to = NULL;
	}

	if (!(chan = prepare_command(&server, to, NO_OP)))
		return;

	if (!spec && !(spec = next_arg(args, &args)))
		spec = "*!*@*";
	if (*spec == '-')
	{
		temp1 = spec;
		spec = "*!*@*";
	}
	else
		temp1 = next_arg(args, &args);
	ops = 0;
	msg = 0;
	if ( ((temp1 && (!my_strnicmp(temp1, "-ops", strlen(temp1)))) || (command && !my_stricmp(command, "CHOPS")) ))
		ops = 1;
	if ( ((temp1 && (!my_strnicmp(temp1, "-nonops", strlen(temp1)))) || (command && !my_stricmp(command, "NOPS")) ))
		ops = 2;
	if (ops)
		temp1 = next_arg(args, &args);

	if (temp1 && !my_strnicmp(temp1, "-msg", strlen(temp1)))
		msg = 1;
	if (temp1 && !my_strnicmp(temp1, "-notice", strlen(temp1)))
		msg = 2;
	if (temp1 && !my_strnicmp(temp1, "-nkill", strlen(temp1)))
		msg = 3;
	if (temp1 && !my_strnicmp(temp1, "-kill", strlen(temp1)))
		msg = 4;
	if (temp1 && !my_strnicmp(temp1, "-kick", strlen(temp1)))
		msg = 5;
	if (temp1 && !my_strnicmp(temp1, "-stats", strlen(temp1)))
		msg = 6;
	if (temp1 && !my_strnicmp(temp1, "-server", strlen(temp1)))
		msg = 7;
		
	if (msg && (msg != 3) && (msg != 4) && (msg != 5) && (msg != 6) && (msg != 7) && (!args || !*args))
	{
		say("No message given");
		return;
	}

	nicks = chan->nicks;
	count = 0;
	switch (msg)
	{
		case 6:
			if (do_hook(STAT_HEADER_LIST, "%s %s %s %s %s", "Nick", "dops", "kicks","nicks","publics"))
				put_it("Nick        dops  kicks  nicks  publics");
			break;
		case 7:
			if (do_hook(USERS_SERVER_HEADER_LIST, "%s %s", "Nick", "Server"))
				put_it("Nick        Server");
			break;
		default:
			break;
	}
	while (nicks)
	{
		sprintf(modebuf, "%s!%s", nicks->nick,
		      nicks->host ? nicks->host : "<UNKNOWN@UNKOWN>");
		if (match(spec, modebuf) && (!ops ||
					 ((ops == 1) && nicks->chanop) ||
					 ((ops == 2) && !nicks->chanop)))
		{
			if (msg == 3)
				count--;
			else if (msg == 4)
				if (!isme(nicks->nick))
					my_send_to_server(server, "KILL %s :%s (%i", nicks->nick,
						       args && *args ? args : get_reason(nicks->nick),
						       count + 1);
				else
					count--;
			else if (msg == 5)
				if (!isme(nicks->nick))
					my_send_to_server(server, "KICK %s %s :%s", chan->channel,
						       nicks->nick, (args && *args) ? args :
						       get_reason(nicks->nick));
				else
					count--;
			else if (msg == 6)
			{
				if (my_stricmp(nicks->nick, get_server_nickname(from_server)))
				{
					if (do_hook(STAT_LIST, "%s %d %d %d %d", nicks->nick, nicks->dopcount, nicks->kickcount, 
						nicks->nickcount, nicks->floodcount))
						put_it("%-10s  %4d   %4d   %4d     %4d", 
							nicks->nick, nicks->dopcount, nicks->kickcount, 
							nicks->nickcount, nicks->floodcount);
				}
			}	
			else if (msg == 7)
			{
				if (do_hook(USERS_SERVER_LIST, "%s %s", nicks->nick, nicks->server))
					put_it("%-11s %-40s", nicks->nick, nicks->server);
			}
			else if (msg == 1 || msg == 2)
			{
				if (count)
					strcat(msgbuf, ",");
				strcat(msgbuf, nicks->nick);
			}
			else
			{
				if (!count && do_hook(USERS_HEADER_LIST, "%s %s %s %s %s %s %s", "Level", "aop", "prot", "Channel", "Nick", "+o", "UserHost"))
					put_it("%s", convert_output_format(get_string_var(FORMAT_USERS_HEADER_VAR), "%s", chan->channel));
				
				if ((hook = do_hook(USERS_LIST, "%d %d %d %s %s %s %c",
					nicks->userlist? nicks->userlist->level:0, 
					nicks->userlist?nicks->userlist->aop:0,
					nicks->userlist?nicks->userlist->prot:0, 
					chan->channel, nicks->nick, 
					nicks->host, 
					nicks->chanop ? '@' : ' ')))
				{
					put_it("%s", convert_output_format(get_string_var(nicks->userlist?FORMAT_USERS_USER_VAR:nicks->botlist?FORMAT_USERS_BOT_VAR:nicks->shitlist?FORMAT_USERS_SHIT_VAR:FORMAT_USERS_VAR), "%d %d %d %s %s %s %s", 
						nicks->userlist ? nicks->userlist->level:nicks->botlist?0:nicks->shitlist?nicks->shitlist->level:0,
						nicks->userlist ? nicks->userlist->aop:nicks->botlist?nicks->botlist->aop:0,
						nicks->userlist ? nicks->userlist->prot:nicks->botlist?nicks->botlist->prot:0, 
						chan->channel, nicks->nick,
						nicks->host,
						nicks->chanop ? "@" : (nicks->voice? "v" : "ÿ")));
				}
			}
			count++;
		}
		nicks = nicks->next;
	}
	if (!count)
	{
		if (!command)
			say("No match of %s on %s", spec, chan->channel);
		else
			say("There are no [\002%s\002] on %s", command, chan->channel);
	}
	else if (!msg && !hook)
		bitchsay("End of UserList on %s %d counted", chan->channel, count);

	if (msg && (msg != 3) && (msg != 4) && (msg != 5) && (msg != 6) && (msg != 7) && count)
	{
		put_it("%s", convert_output_format(get_string_var((msg == 1)?FORMAT_SEND_MSG_VAR:FORMAT_SEND_NOTICE_VAR), "%s %s %s", update_clock(GET_TIME),msgbuf, args));
		my_send_to_server(server, "%s %s :%s", (msg == 1) ? "PRIVMSG" : "NOTICE", msgbuf, args);
	}
}

int caps_fucknut _((char *crap))
{
	int total = 0, allcaps = 0;
/* removed from ComStud client */
	while (*crap)
	{
		if ((*crap >= 'a' && *crap <= 'z') || (*crap >= 'A' && *crap <= 'Z'))
		{
			total++;
			if (toupper(*crap) == *crap)
				allcaps++;
		}
		crap++;
	}
	if (total)
		if ((float) allcaps / (float) total >= .75 && total > 3)
			return (1);
	return (0);
}

char *convert_output_format(char *format, char *str, ...)
{
static unsigned char buffer[2*BIG_BUFFER_SIZE+1];
char buffer2[2*BIG_BUFFER_SIZE+1];
enum color_attributes this_color = BLACK;
register unsigned char *t;
register unsigned char *s;
char *copy = NULL;
char *tmpc = NULL;
char *p;
int old_who_level = who_level;
int bold = 0;

va_list args;
int arg_flags;

	malloc_strcpy(&copy, format);
	bzero(buffer2, BIG_BUFFER_SIZE/4);
	bzero(buffer, BIG_BUFFER_SIZE);
	if (str && !in_cparse)
	{

		p = str;
		va_start(args, str);
		while(*p)
		{
			if (*p == '%')
			{
				switch(*++p)
				{
				case 's':
				{
					char *s = (char *)va_arg(args, char *);
					if (s)
						strcat(buffer2, s);
					break;
				}
				case 'd':
				{
					int d = (int) va_arg(args, int);
					strcat(buffer2, ltoa((long)d));
					break;
				}
				case 'c':
				{
					char c = (char )va_arg(args, int);
					buffer2[strlen(buffer2)] = c;
					break;
				}
				case 'u':
				{
					unsigned int d = (unsigned int) va_arg(args, unsigned int);
					strcat(buffer2, ltoa(d));
					break;
				}
				case 'l':
				{
					unsigned long int d = (unsigned long int) va_arg(args, unsigned long int);
					strcat(buffer2, ltoa(d));
					break;
				}
				case '%':
				{
					buffer2[strlen(buffer2)] = '%';
					p++;
					break;
				}
				default:
					strcat(buffer2, "%");
					buffer2[strlen(buffer2)] = *p;
				}
				p++;
			} else 
			{
				buffer2[strlen(buffer2)] = *p;
				p++;
			}
		}
		va_end(args);
	} 
	else if (in_cparse && str)
		strcpy(buffer2, str);

	s = buffer;
	tmpc = copy;
	if (!tmpc)
		goto done;
	while (*tmpc)
	{
		if (*tmpc == '%')
		{
			tmpc++;
			switch(*tmpc)
			{
				case '%':
					*s++ = *tmpc;
					break;
				case 'n':
					this_color = NO_COLOR;
					break;
				case 'W':
					this_color = WHITEB;
					break;
				case 'w':
					this_color = WHITE;
					break;
				case 'K':
					this_color = BLACKB;
					break;
				case 'k':
					this_color = BLACK;
					break;
				case 'G':
					this_color = GREENB;
					break;
				case 'g':
					this_color = GREEN;
					break;
				case 'Y':
					this_color = YELLOWB;
					break;
				case 'y':
					this_color = YELLOW;
					break;
				case 'C':
					this_color = CYANB;
					break;
				case 'c':
					this_color = CYAN;
					break;
				case 'B':
					this_color = BLUEB;
					break;
				case 'b':
					this_color = BLUE;
					break;
				case 'P':
				case 'M':
					this_color = MAGENTAB;
					break;
				case 'p':
				case 'm':
					this_color = MAGENTA;
					break;
				case 'R':
					this_color = REDB;
					break;
				case 'r':
					this_color = RED;
					break;
				case '0':
					this_color = bold? BACK_BBLACK:BACK_BLACK;
					bold = 0;
					break;
				case '1':
					this_color = bold? BACK_BRED:BACK_RED;
					bold = 0;
					break;
				case '2':
					this_color = bold? BACK_BGREEN:BACK_GREEN;
					bold = 0;
					break;
				case '3':
					this_color = bold? BACK_BYELLOW:BACK_YELLOW;
					bold = 0;
					break;
				case '4':
					this_color = bold? BACK_BBLUE:BACK_BLUE;
					bold = 0;
					break;
				case '5':
					this_color = bold? BACK_BMAGENTA:BACK_MAGENTA;
					bold = 0;
					break;
				case '6':
					this_color = bold? BACK_BCYAN:BACK_CYAN;
					bold = 0;
					break;
				case '7':
					this_color = bold? BACK_BWHITE:BACK_WHITE;
					bold = 0;
					break;
				case '8':
					this_color = REVERSE_COLOR;
					bold = 0;
					break;
				case '9':
					this_color = BOLD_COLOR;
					bold ^= 1;
					break;
				case 'F':
					this_color = BLINK_COLOR;
					break;
				case 'U':
					this_color = UNDERLINE_COLOR;
					break;
				default:
					*s++ = *tmpc;
					continue;
			}
			for (t = color_str[(int)this_color]; *t; t++, s++)
				*s = *t;					
			tmpc++;
			continue;
		}
		else if (*tmpc == '$')
		{
			char *new_str = NULL;
			tmpc++;
			in_cparse++;
			tmpc = alias_special_char(&new_str, tmpc, buffer2, NULL, &arg_flags);
			in_cparse--;
			if (new_str)
				strcat(s, new_str);
			new_free(&new_str);
			while (*s) s++;
			if (!tmpc) break;
			continue;
		} else
			*s = *tmpc;
		tmpc++; s++;
	}
	*s = 0;
done:
	if (*buffer) strcat(s, color_str[NO_COLOR]);
	who_level = old_who_level;
	new_free(&copy);
	if (in_cparse) 
		in_cparse--;
	if (!get_int_var(DISPLAY_ANSI_VAR))
		strcpy(buffer, stripansicodes(buffer));
	return buffer;
}

void add_last_type (LastMsg *array, char *from, char *uh, char *to, char *str)
{
int i;
	for (i = MAX_LAST_MSG-1; i > 0; i--)
	{
		
		malloc_strcpy(&(array[i].last_msg), array[i - 1].last_msg);
		malloc_strcpy(&(array[i].from), array[i - 1].from);
		malloc_strcpy(&(array[i].uh), array[i - 1].uh);
		malloc_strcpy(&(array[i].to), array[i - 1].to);
		malloc_strcpy(&(array[i].time), array[i - 1].time);
	}
	malloc_strcpy(&array->last_msg, str);
	malloc_strcpy(&array->from, from);
	malloc_strcpy(&array->to, to);
	malloc_strcpy(&array->uh, uh);
	malloc_strcpy(&array->time, update_clock(GET_TIME));
}

int check_last_type(LastMsg *array, char *from, char *uh)
{
int i;
	for (i = 0; i < MAX_LAST_MSG-1; i++)
	{
		if (array[i].from && array[i].uh && !my_stricmp(from, array[i].from) && !my_stricmp(uh, array[i].uh))
			return 1;
	}
	return 0;
}

int matchmcommand(char *origline,int count)
{
    int  startnum=0;
    int  endnum=0;
    char *tmpstr;
    char tmpbuf[BIG_BUFFER_SIZE];

	strcpy(tmpbuf,origline);
	tmpstr=tmpbuf;
	if (*tmpstr=='*') return(1);
	while (tmpstr && *tmpstr) 
	{
		startnum=0;
		endnum=0;
		if (tmpstr && *tmpstr && *tmpstr=='-') 
		{
			while (tmpstr && *tmpstr && !isdigit(*tmpstr)) 
				tmpstr++;
			endnum=atoi(tmpstr);
			startnum=1;
			while (tmpstr && *tmpstr && isdigit(*tmpstr)) 
				tmpstr++;
		}
		else 
		{
			while (tmpstr && *tmpstr && !isdigit(*tmpstr)) 
				tmpstr++;
			startnum=atoi(tmpstr);
			while (tmpstr && *tmpstr && isdigit(*tmpstr)) 
				tmpstr++;
			if (tmpstr && *tmpstr && *tmpstr=='-') {
				while (tmpstr && *tmpstr && !isdigit(*tmpstr)) 
					tmpstr++;
				endnum=atoi(tmpstr);
				if (!endnum) 
					endnum=1000;
				while (tmpstr && *tmpstr && isdigit(*tmpstr)) 
					tmpstr++;
			}
		}
		if (count==startnum || (count>=startnum && count<=endnum)) 
			return(1);
	}
	if (count==startnum || (count>=startnum && count<=endnum)) 
		return(1);
	return(0);
}

ChannelList *prepare_command(int *active_server, char *channel, int need_op)
{
int server = 0;
ChannelList *chan = NULL;

	if (!channel && !get_channel_by_refnum(0))
	{
		context;
		if (need_op != 3) 
			not_on_a_channel(curr_scr_win);
		return NULL;
	}
	server = curr_scr_win->server;
	*active_server = server;
	if (!(chan = lookup_channel(channel? channel : get_channel_by_refnum(0), server, 0)))
	{
		context;
		if (need_op != 3) 
			not_on_a_channel(curr_scr_win);
		return NULL;
	}
	if (need_op == NEED_OP && chan && !chan->chop) 
	{
		context;
		error_not_opped(chan->channel);
		return NULL;
	}
	return chan;
}
