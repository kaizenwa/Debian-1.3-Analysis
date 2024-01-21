/*
 * notice.c: special stuff for parsing NOTICEs
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1991
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

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
#include "misc.h"
#include "screen.h"
#include "status.h"

extern	char	*FromUserHost;
static	void	parse_server_notice _((char *, char *));
extern void reload_save _((char *, char *, char *));
extern	char	*last_wall;
int	doing_notice = 0;

int	oper_kills = 0,
	nick_collisions = 0,
	serv_fakes = 0,
	serv_unauth = 0,
	serv_split = 0,
	serv_rejoin = 0,
	client_connects = 0,
	serv_rehash = 0,
	client_exits = 0,
	serv_klines = 0,
	client_floods = 0,
	client_invalid = 0,
	stats_req = 0,
	client_bot = 0,
	client_bot_alarm = 0,
	oper_requests = 0;
	
static	void
parse_server_notice(from, line)
	char	*from,
		*line;
{
	int	lastlog_level = 0;
	int	flag = 0;
	int	up_status = 0;
	int	i;
	
				
	char *fr, *for_, *temp, *temp2;

	fr = for_ = temp = temp2 = NULL;
	
	if (!from || !*from)
		from = server_list[from_server].itsname ?
			server_list[from_server].itsname :
			server_list[from_server].name;
	if (!strncmp(line, "*** Notice --", 13))
	{
		message_from(NULL, LOG_OPNOTE);
		lastlog_level = set_lastlog_msg_level(LOG_OPNOTE);
	}

	message_from(NULL, LOG_SNOTE);
	lastlog_level = set_lastlog_msg_level(LOG_SNOTE);
	
	if (*line != '*'  && *line != '#' && strncmp(line, "MOTD ", 4))
		flag = 1;
	else
		flag = 0;

	
if (strstr(line, "Notice --") || strstr(line, "\002Notice\002 --"))
{
 	if (strstr(line, "-- Nick change collision") || strstr(line, "-- Nickname collision") || strstr(line, "-- Nick collision"))
	{
		if (!get_int_var(SHOW_SERVER_KILLS_VAR))
			goto done;  	
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i = 0; i < 4; i++) 
        	        	for_ = next_arg(line,&line);
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_NICK_COLLISION_VAR), "%s %s", update_clock(GET_TIME), line));
		}
		nick_collisions++;
		up_status++;
	}
	else if (strstr(line, "-- Received KILL message for"))
	{
		if (!get_int_var(SHOW_SERVER_KILLS_VAR))
			goto done;  	
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 8; i++)
	                	for_ = next_arg(line,&line);
			chop(for_, 1);

			temp2=next_arg(line,&line);
			temp2=next_arg(line,&line);

			temp= next_arg(line,&line);
			temp= next_arg(line,&line);

			if (line)
			{
				*(line++)=0;
				chop(line, 1);
			}
			if (match("%.%", temp2))
			{
				serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_NICK_COLLISION_VAR), "%s %s %s %s", update_clock(GET_TIME), temp2, for_, line));
				nick_collisions++;
			}
			else 
			{
				if (!my_strnicmp(get_server_name(from_server), temp, strlen(get_server_name(from_server))))
					serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_KILL_LOCAL_VAR), "%s %s %s %s", update_clock(GET_TIME), temp2, for_, line));
				else
					serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_KILL_VAR), "%s %s %s %s", update_clock(GET_TIME), temp2, for_, line));
				if (!my_stricmp(get_server_nickname(from_server), temp2)) 
					logmsg(0, LOG_KILL, "!", from, line, for_, 0);
				oper_kills++;
			}
		}
		up_status++;
	}
  	else if (match(line, "% % -- Fake: % MODE"))
  	{
		serv_fakes++;
		if (!get_int_var(SHOW_FAKES_VAR))
			goto done;
		for (i = 0; i < 7; i++)
			fr = next_arg(line, &line);
		if (lookup_channel(fr, from_server, CHAN_NOUNLINK))
			serversay(0, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_FAKE_VAR), "%s %s %s", update_clock(GET_TIME), fr, line));
		else 
			serversay(0, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_FAKE_VAR), "%s %s %s", update_clock(GET_TIME), fr, line));
  	}
  	else if (strstr(line, "-- Unauth"))
  	{
		serv_unauth++;
		if (!get_int_var(SHOW_UNAUTHS_VAR))
			goto done;
#if 0
$1 = 0xbffff26f "*** Notice -- Unauthorized connection from [Zi][LALA@cmodem58.lancite.net]."
#endif
		if (!get_int_var(SHOW_UNAUTHS_VAR))
			goto done;  	
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i = 0; i < 7; i++)
				for_ = next_arg(line, &line);
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_UNAUTH_VAR), "%s %s", update_clock(GET_TIME), for_));
		}
	}
	else if (strstr(line, "-- Entering"))
	{
 		serv_split++;
		if (!get_int_var(SHOW_TRAFFIC_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 8; i++)
	                	for_ = next_arg(line,&line);
			temp2= next_arg(line,&line);
			temp2= next_arg(line,&line);
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_TRAFFIC_HIGH_VAR), "%s %s %s", update_clock(GET_TIME), for_, temp2));
		}
	}
	else if (strstr(line, "-- Resuming standard operation"))
	{
		serv_rejoin++;
		if (!get_int_var(SHOW_TRAFFIC_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 8; i++)
	                	for_ = next_arg(line,&line);
			temp= next_arg(line,&line);
			temp2= next_arg(line,&line);
			if (!for_)
				for_ =" ";
			if (!temp)
				from = " ";
			if (!temp2)
				temp2 = " ";
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_TRAFFIC_NORM_VAR), "%s %s %s %s", update_clock(GET_TIME), for_, temp, temp2));
		}
	}
	else if (strstr(line, "-- Client connecting"))
	{
		client_connects++;
		if (!get_int_var(SHOW_CLIENT_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
#if 0
<Anybody> irc.klis.com *** Notice -- Client connecting: <DarkSun!anybody@ppp42.ocws.com>
#endif          
			for (i =0; i < 6; i++)
	                	for_ = next_arg(line,&line);
			if ((temp2= next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_CLIENT_CONNECT_VAR), "%s %s %s", update_clock(GET_TIME), for_, temp2));
		}
	}
	else if (strstr(line, "is rehashing Server config file"))
	{
		serv_rehash++;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 4; i++)
	                	for_ = next_arg(line,&line);
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_REHASH_VAR), "%s %s", update_clock(GET_TIME), for_));
		}
	}
	else if (strstr(line, "-- Client exiting"))
	{
		client_exits++;
		if (!get_int_var(SHOW_CLIENT_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 6; i++)
	                	for_ = next_arg(line,&line);
			if ((temp2= next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_CLIENT_EXIT_VAR), "%s %s %s", update_clock(GET_TIME), for_, temp2));
		}
	}
	else if (strstr(line, "added K-Line for"))
	{
		serv_klines++;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 4; i++)
	                	for_ = next_arg(line,&line);
			temp2= next_arg(line,&line);
			temp2= next_arg(line,&line);
			temp2= next_arg(line,&line);
			if ((temp2= next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1); /* 2? */
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_KLINE_VAR), "%s %s %s %s", update_clock(GET_TIME), for_, temp2, line?line:empty_string));
		}
	}
	else if (strstr(line, "Terminating client for excess"))
	{
		client_floods++;
		if (!get_int_var(SHOW_CLIENT_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 9; i++)
	                	for_ = next_arg(line,&line);
			if ((temp2= next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_CLIENT_TERM_VAR), "%s %s %s", update_clock(GET_TIME), for_, temp2));
		}
	}
	else if (strstr(line, "Invalid username"))
	{
		client_invalid++;
		if (!get_int_var(SHOW_CLIENT_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 6; i++)
	                	for_ = next_arg(line,&line);
			if ((temp2= next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_CLIENT_INVALID_VAR), "%s %s %s", update_clock(GET_TIME), for_, temp2));
		}
	}
	else if (strstr(line, "Notice -- STATS "))
	{
		stats_req++;
		if (!get_int_var(SHOW_CLIENT_VAR))
			goto done;
/*	
*** Notice -- STATS k requested by root (root@panasync.canu
*/
		for (i = 0; i < 5; i++)
			temp = next_arg(line, &line);
		for (i = 0; i < 3; i++)
			for_ = next_arg(line, &line);
		if ((temp2= next_arg(line,&line)))
		{
			*(temp2++)=0;
			chop(temp2, 1);
		} else temp2 = empty_string;
		serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_STATS_VAR), "%s %s %s %s", update_clock(GET_TIME), temp, for_, temp2));
	}
	else if (strstr(line, "Possible bot"))
	{
		client_bot++;
		if (!get_int_var(SHOW_BOTS_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 6; i++)
	                	for_ = next_arg(line,&line);
			if ((temp2= next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_BOT_VAR), "%s %s %s", update_clock(GET_TIME), for_, temp2));
		}
	}
	else if (match(line, "% % -- Possible % bot"))
	{
/*	
*** Notice -- Possible eggdrop bot: root (root@panasync.canu
*/
		client_bot++;
		if (!get_int_var(SHOW_BOTS_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			char *possible;
			for (i =0; i < 4; i++)
	                	for_ = next_arg(line,&line);
			possible = next_arg(line,&line);
			for_ = next_arg(line,&line);
			for_ = next_arg(line,&line);
			if ((temp2= next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_BOT1_VAR), "%s %s %s %s", update_clock(GET_TIME), possible?possible:"Unknown", for_, temp2));
		}
	}
	else if (match("% % -- JohBot alarm activated", line) || match("% % -- % alarm activated", line) || match("% % -- % % alarm activated", line) || match("% % -- Eggdrop % alarm % activated", line))
	{
		client_bot_alarm++;
		if (!get_int_var(SHOW_BOTS_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
/*	
*** Notice -- Possible eggdrop bot: root (root@panasync.canu
*/
			char *possible;
			for (i =0; i < 3; i++)
	                	for_ = next_arg(line,&line);
			possible = next_arg(line,&line);
			for (i =0; i < 3; i++)
	                	for_ = next_arg(line,&line);
			if ((temp = next_arg(line,&line)))
			{
				*(temp++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_BOT_ALARM_VAR), "%s %s %s %s", update_clock(GET_TIME), possible?possible:"Unknown", for_, temp));
		}
	}
	else if (stristr(line, "is now operator"))
	{
		oper_requests++;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 3; i++)
	                	for_ = next_arg(line,&line);
			fr=next_arg(line,&line);
			if ((temp2=next_arg(line,&line)))
			{
				*(temp2++)=0;
				chop(temp2, 1);
			} else temp2 = empty_string;
			serversay(1, "%s", convert_output_format(get_string_var(FORMAT_OPER_VAR), "%s %s %s", update_clock(GET_TIME), fr, temp2));
		}
/*
BlackJac (Jan  9 04:14): !ingenue.EECS.Berkeley.EDU Nick flooding detected by: KiNGPiN0 (falzar@204.174.128.37)
*/          
	}
	else 
	{

		if (!get_int_var(SHOW_SERVER_CRAP_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for (i =0; i < 3; i++)
	                	for_ = next_arg(line,&line);
			put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_VAR), "%s %s %s", update_clock(GET_TIME), from, stripansicodes(line)));
			add_last_type(&last_servermsg[0], NULL, NULL, NULL, line);
		}
	} 
}
	else if (strstr(line, "***"))
	{
		if (!get_int_var(SHOW_SERVER_CRAP_VAR))
			goto done;
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
		{
			for_ = next_arg(line,&line);
			put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_VAR), "%s %s %s", update_clock(GET_TIME), from, stripansicodes(line)));
			add_last_type(&last_servermsg[0], NULL, NULL, NULL, line);
		}
	}
	else
	{
		if (!get_int_var(SHOW_SERVER_CRAP_VAR))
			goto done;  	
		if (do_hook(SERVER_NOTICE_LIST, flag ? "%s *** %s" : "%s %s", from, line))
			put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_NOTICE_VAR), "%s %s %s", update_clock(GET_TIME), from, stripansicodes(line)));
		add_last_type(&last_servermsg[0], NULL, NULL, NULL, line);
	}
	if (up_status)
		update_all_status(curr_scr_win, NULL, 0);
done:	
	if (lastlog_level)
	{
		set_lastlog_msg_level(lastlog_level);
		message_from(NULL, lastlog_level);
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
	
	ChannelList *tmpc;

		
	PasteArgs(Args, 1);
	to = Args[0];
	line = Args[1];
	if (!to || !line)
		return;
	doing_notice = 1;
	
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
		tmpc = lookup_channel(to, from_server, CHAN_NOUNLINK);
		update_stats(NOTICELIST, to, from, tmpc, 0);		
		if (from && *from && strcmp(get_server_itsname(from_server), from))
		{
			int not_reply = 0;
			if (check_auto_reply(line))
				not_reply++;
			switch ((flag = check_ignore(from, FromUserHost, to, IGNORE_NOTICES)))
			{
				case IGNORED:
				{
					doing_notice = 0;
					return;
				}
				case HIGHLIGHTED:
					high = highlight_char;
					break;
				default:
					high = empty_string;
			}
		/*
		 * only dots in servernames, right ?
		 *
		 * But a server name doesn't nessicarily have to have
		 * a 'dot' in it..  - phone, jan 1993.
		 */
			if (index(from, '.'))
				not_from_server = 0;
			
			line = do_notice_ctcp(from, to, stripansi(line));
			if (!line || !*line)
			{
				doing_notice = 0;
				return;
			}
			level = set_lastlog_msg_level(LOG_NOTICE);
			no_flooding = check_flooding(from, NOTICE_FLOOD, line, NULL);
			
			if (sed == 1)
			{
				if (do_hook(ENCRYPTED_NOTICE_LIST, "%s %s %s", from, to, line))
					put_it("%s", convert_output_format(get_string_var(FORMAT_ENCRYPTED_NOTICE_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, line));
				sed = 0;
			}
			else
			{
				if (no_flooding)
				{
/*					if (!my_strnicmp("[\002BX-Wall", line, 7) || !my_strnicmp("[\002S-Wall", line, 7) || !my_strnicmp("[\002CWallop", line, 7))*/
					if (match("[\002*Wall*", line))
					{
/*-Raistlin(raistlin@dialup39.toledolink.com)- [BX-Wall/#continuum] test*/
						char *channel = NULL, *p, *q;
						q = p = next_arg(line, &line);
						if ((p = strchr(p, '/')))
						{
							channel = m_strdup(++p);
							if ((p = strchr(channel, ']')))
								*p++ = 0;
							q = channel;
						} 
						if (do_hook(type, "%s %s", from, line))
							put_it("%s", convert_output_format(get_string_var(FORMAT_BWALL_VAR), "%s %s %s %s %s", update_clock(GET_TIME), q, from, FromUserHost, stripansi(line)));
						malloc_sprintf(&last_wall, "%s %s %s %s", update_clock(GET_TIME), from, FromUserHost, line);
						logmsg(0, LOG_WALL, "-", from, line, FromUserHost, 0);
						new_free(&channel);
					}
					else if (type == NOTICE_LIST)
					{
						logmsg(0, LOG_NOTICE, "-", from, line, FromUserHost, 0);
						if (do_hook(type, "%s %s", from, line))
						put_it("%s", convert_output_format(get_string_var(FORMAT_NOTICE_VAR), "%s %s %s %s", update_clock(GET_TIME), from, FromUserHost, stripansi(line)));
						add_last_type(&last_notice[0], from, FromUserHost, to, line);
					}
					else
					{
						if (do_hook(type, "%s %s %s", from, to, line))
							put_it("%s", convert_output_format(get_string_var(not_reply?FORMAT_PUBLIC_NOTICE_AR_VAR:FORMAT_PUBLIC_NOTICE_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, FromUserHost, to, stripansi(line)));
						if (beep_on_level & LOG_NOTICE)
							beep_em(1);
						add_last_type(&last_notice[0], from, FromUserHost, to, line);
					}
				}
				set_lastlog_msg_level(level);
				if (not_from_server)
					notify_mark(from, 1, 0);
			}
		}
		else 
			parse_server_notice(from, line);
	}
	else
		put_it("%s", convert_output_format(get_string_var(FORMAT_SERVER_MSG2_VAR), "%s %s %s", update_clock(GET_TIME), from, line+1));
	doing_notice = 0;
	message_from(NULL, LOG_CRAP);
}


void load_scripts(void)
{
	char buffer[BIG_BUFFER_SIZE+1];
	static int done = 0;
	if (!done++)
	{
		
		strcpy(buffer, "bxglobal");
		never_connected = 0;

		loading_global = 1;
		load("LOAD", buffer, empty_string);
		loading_global = 0;

		/* read the .ircrc file */
		if (access(bircrc_file, R_OK) == 0 && !quick_startup)
			load("LOAD", bircrc_file, empty_string);
		else if (access(ircrc_file, R_OK) == 0 && !quick_startup)
			load("LOAD", ircrc_file, empty_string);
		else if (get_int_var(NOVICE_VAR))
			say("If you have not already done so, please read the new user information with /HELP NEWUSER");
		if (!quick_startup)
			reload_save(NULL, NULL, NULL);
	}
	if (never_connected && server_list[from_server].away)
		send_to_server("AWAY :%s", server_list[from_server].away);
}

/*
 * got_initial_version_28: this is called when ircii gets the serial
 * number 004 reply.  We do this becuase the 004 numeric gives us the
 * server name and version in a very easy to use fashion, and doesnt
 * rely on the syntax or construction of the 002 numeric.
 *
 * Hacked as neccesary by jfn, May 1995
 */
#ifdef __STDC__
extern void got_initial_version_28 (char **ArgList)
#else
extern void	got_initial_version_28(ArgList)
char	**ArgList;
#endif
{
	char *server, *sversion;

	server = ArgList[0];
	sversion = ArgList[1];
	
	attempting_to_connect--;
	set_server_motd(from_server, 1);
	server_is_connected(from_server, 1);

	if (!strncmp(version, "2.8", 3))
	{
		if (strstr(version, "mu") || strstr(version, "me"))
			set_server_version(from_server, Server_u2_8);
		else
			set_server_version(from_server, Server2_8);
	}
	else if (!strncmp(version, "u2.9", 4))
		set_server_version(from_server, Server_u2_9);
	else if (!strncmp(version, "u2.10", 4))
		set_server_version(from_server, Server_u2_10);
	else if (!strncmp(version, "u3.0", 4))
		set_server_version(from_server, Server_u3_0);
	else
		set_server_version(from_server, Server2_8);

	malloc_strcpy(&server_list[from_server].version_string, sversion);
	set_server_itsname(from_server, server);
	reconnect_all_channels(from_server);
	reinstate_user_modes();

	update_all_status(curr_scr_win, NULL, 0);
	do_hook(CONNECT_LIST, "%s %d", get_server_name(from_server), get_server_port(from_server));
}
