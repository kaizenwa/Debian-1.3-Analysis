/*
 * Routines within this files are Copyright Colten Edwards 1996
 * Aka panasync on irc.
 * Thanks to Shiek and Flier for helpful hints and suggestions. As well 
 * as code in some cases.
 */
 
#include "irc.h"

#include <sys/stat.h>

#ifdef ESIX
# include <lan/net_types.h>
#endif

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif

#include "parse.h"
#include "ircterm.h"
#include "server.h"
#include "chelp.h"
#include "edit.h"
#include "crypt.h"
#include "vars.h"
#include "ircaux.h"
#include "lastlog.h"
#include "log.h"
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
#include "if.h"
#include "help.h"
#include "stack.h"
#include "queue.h"
#include "timer.h"
#include "list.h"
#include "misc.h"
#include "userlist.h"
#include "whowas.h"
#include "window.h"

time_t in_sping = 0;
char *org_nick = NULL;

static void gain_nick _((WhoisStuff *, char *, char *));
static int delay_gain_nick _((void *));

void nwhois (char *command, char *args, char *subargs)
{
ChannelList *tmp;
NickList *user = NULL;
int once = 0;
char *nick;
int mem_use = 0, max_mem = 0;
	context;
	if (args && *args)
	{
		tmp = server_list[from_server].chan_list;
		if (!tmp)
		{
			bitchsay("No Channel");
			return;
		}
		nick = next_arg(args, &args);
		while (tmp)
		{
			for (user = tmp->nicks; user; user = user->next)
			{
				if (match(user->nick, nick) || match(nick, user->nick))
				{
					if (!once)
					{
put_it("Channel    @ v  UL: l   a  p  SL: l      +o   -o   +b   -b  kicks  nicks   pub");
						once++;
					}
put_it("%-10s %c %c    %3d  %2d %2d     %2d    %4d %4d %4d %4d  %5d  %5d %5d",
tmp->channel, user->chanop? '@':' ',user->voice?'v':' ',user->userlist?user->userlist->level:0, 
user->userlist?user->userlist->aop:0,user->userlist?user->userlist->prot:0, user->shitlist?user->shitlist->level: 0,
user->stat_ops,user->stat_dops, user->stat_bans, user->stat_unbans, 
user->stat_kicks, user->stat_nicks, user->stat_pub);
					mem_use = sizeof(NickList) + (user->userlist? sizeof(UserList): 0) + (user->shitlist? sizeof(ShitList):0) + (user->botlist?sizeof(UserList):0);
					if (max_mem < mem_use)
						max_mem = mem_use;
					break;
				}
			}				
			tmp = tmp->next;
		}
		if (once)
			bitchsay("End /NWhoIs list for %s [Mem Usage: %d bytes per chan]", nick, max_mem);
		else
			bitchsay("No such nick %s on any channel", nick);
	}
	else
		userage("NWhois", "<nick>");
}

void whowas _((char *command, char *args, char *subargs))
{
	context;
	if (!command)
		show_whowas();
	else
		show_wholeft(NULL);
}

void do_sk _((char *command, char *args, char *subargs))
{
char *serv = NULL;
char *to = NULL;
int count;
NickList *Nick;
ChannelList *chan;
int server = 0;

	context;
	if (!args || !*args)
	{
		userage("Sk", "<server> [channel]");
		return;
	}
	serv = next_arg(args, &args);
	if (*serv == '#')
	{
		bitchsay("You need to specify a server not a channel");
	}
	to = next_arg(args, &args);
	if (!(chan = prepare_command(&server, to, NEED_OP)))
		return;
		
	bitchsay("Performing Lame Server kicks on %s", chan->channel);
	for (Nick = chan->nicks, count = 0; Nick; Nick = Nick->next)
	{
		if (Nick->server && match(serv, Nick->server))
		{
			if (!isme(Nick->nick))
			{
				my_send_to_server(server, "KICK %s %s : Lame Server Kick", chan->channel, Nick->nick);
				count++;
			}
		}
	}
	if (count)
		bitchsay("Found %d lamerz on %s from %s", count, to, serv);
}

int scan(char *remote_host, char *remote_ip, int low_port, int high_port, struct hostent *host)
{
	int soc;
	struct sockaddr_in addr;
	struct servent *serv;
	int port,rc,addr_len,opt;
	int found = 0;
	context;

	sprintf(remote_ip,"%u.%u.%u.%u", (unsigned char) host->h_addr_list[0][0],
		(unsigned char) host->h_addr_list[0][1],
		(unsigned char) host->h_addr_list[0][2],
		(unsigned char) host->h_addr_list[0][3]);
	for (port = low_port;port <= high_port;port++) 
	{
		soc = socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);

		if (soc < 0) 
		{
			bitchsay("error: socket() failed");
			return(0);
		}

		rc = setsockopt(soc,SOL_SOCKET,SO_REUSEADDR,&opt,sizeof(opt));

		addr.sin_family = AF_INET;
		addr.sin_addr.s_addr = inet_addr(remote_ip);
		addr.sin_port = htons(port);

		addr_len = sizeof(addr);
		alarm(1);
		rc = connect(soc, (struct sockaddr*) &addr, addr_len);
		alarm(0);
		close(soc);

		if (rc >= 0)
		{
			serv = getservbyport(htons(port),"tcp");
			found++;
			put_it("port %d (%s) is running",port,(serv == NULL)?"UNKNOWN": serv->s_name);
		}
	}
	if (!found)
		bitchsay("Wonder what hostname that was. No ports found");
	return 1;
}

void findports(char *command, char *args, char *subargs)
{
char *remote_host;
char remote_ip[20] = "\0";
int low_port = 6660;
int high_port = 7000;
struct hostent *host;

	context;
	if (args && *args)
	{
		char *tmp = NULL;
		remote_host = next_arg(args, &args);
		if (args && *args)
		{
			tmp = next_arg(args, &args);
			low_port = strtoul(tmp, NULL, 10);	
			if (args && *args)
			{
				tmp = next_arg(args, &args);
				high_port = strtoul(tmp, NULL, 10);	
			}
			else
				high_port = low_port;
		}
		if ((host = resolv(remote_host)) /*check_args(remote_host, remote_ip)*/)
		{
			bitchsay("Scanning %s's tcp ports %d through %d",remote_host, low_port,high_port);
			scan(remote_host, remote_ip, low_port, high_port, host);
		} else
			bitchsay("No such host %s", remote_host);
	} else
		userage("Fports", "<hostname> [lowport] [high port]");
	return;
}

void add_ban_word(char *command, char *args, char *subargs)
{
char *word = NULL;
char *chan = NULL;
WordKickList *new;

	context;
	if (args && *args)
	{
		chan = next_arg(args, &args);
		if ((!is_channel(chan) && (chan && *chan != '*')) || !args || !*args)
		{
			userage(command ? command : "BanWords", "<channel\002|\002*> <word(s)>");
			return;
		}
		if (!command)
		{
			new = (WordKickList *)find_in_list((List **)&ban_words, args, 0);
			if (!ban_words || !new || (new && !match(new->channel, chan)))
			{
				new = (WordKickList *) new_malloc(sizeof(WordKickList));
				malloc_strcpy(&new->channel, chan);
				malloc_strcpy(&new->string, args);
				add_to_list((List **)&ban_words, (List *)new);
				bitchsay("Added %s to %s Banned Word List", new->string, new->channel);
			} else bitchsay("[%s] is already in the list for channel %s", args, chan);
		}
		else
		{
			malloc_strcpy(&word, args);
			if ((new = (WordKickList *) remove_from_list((List **)&ban_words, word)))
			{
				bitchsay("Removed %s Banned Word [%s]", new->channel, new->string);
				new_free(&new->channel);
				new_free(&new->string);
				new_free((char **)&new);
			} else 
				bitchsay("Banned Word %s not found.", word);
			new_free(&word);
		}
	} else
		userage(command ? command : "BanWords", "<channel\002|\002*> <word(s)>");
}

void show_word_kick (char *command, char *args, char *subargs)
{
WordKickList *new;

	context;
	if (ban_words) 
	{
		put_it("%14s %40s", "Channel", "Banned Word(s)");
		for (new = ban_words; new; new = new->next)
			put_it("%-14s %40s", new->channel, new->string);
	} else
		bitchsay("No Banned Words on list");
}

void save_banwords(FILE *outfile)
{
	int count = 0;
	WordKickList *new;

	context;
	if (ban_words)
	{
		fprintf(outfile, "# %s Banned Words\n", version);
		for (new = ban_words; new; new = new->next)
		{
			fprintf(outfile, "BANWORD %s %s\n", new->channel, new->string);
			count++;
		}
	}
	if (do_hook(SAVEFILE_LIST,"BanWords %d", count))
		bitchsay("Saved %d Banned Words List", count);
}

void whereis (char *command, char *args, char *subargs)
{
NickList *tmp = NULL;
WhowasList *tmp1;
ChannelList *chan;
char *nick;

	context;
	if (args && *args)
	{
		nick = next_arg(args, &args);
		for (chan = server_list[from_server].chan_list; chan; chan = chan->next)
			if ((tmp = (NickList *)find_in_list((List **)&chan->nicks, nick, 0)))
				break;
		if (!tmp)
		{
			tmp1 = find_in_whowas(nick);
			if (tmp1) tmp = tmp1->nicklist;
		}
		if (tmp && tmp->server)
		{
			char *p = NULL, *user = NULL;
			malloc_strcpy(&user, tmp->host);
			p = strchr(user, '@');
			*p++ = '\0';
			send_to_server("PRIVMSG %s@%s :%cVERSION%c",user, tmp->server,CTCP_DELIM_CHAR,CTCP_DELIM_CHAR);
			new_free(&user);
		}
		else bitchsay("No such user [%s].", nick);
	} else userage("Whereis", "<nick>");
}

char *recreate_saymode(ChannelList *chan)
{
static char mode_str[] = "iklmnpsta";
static char buffer[BIG_BUFFER_SIZE/4+1];
int mode_pos = 0;
int mode;
char *s;

	context;
        buffer[0] = '\0';
        s = buffer;
        mode = chan->modelock_val;
	while (mode)
	{
		if (mode % 2)
			*s++ = mode_str[mode_pos];
		mode /= 2;
		mode_pos++;
	}
	if (chan->key && *chan->key)
	{
		*s++ = ' ';
		strcpy(s, chan->key);
		s += strlen(chan->key);
	}
	if (chan->limit)
		sprintf(s, " %d", chan->limit);
	else
		*s = '\0';
	return buffer;
}

int check_mode_lock(char *channel, char *mode_list, int server)
{
ChannelList *chan;
char buffer[BIG_BUFFER_SIZE+1];
	context;
	if ((chan = lookup_channel(channel, server, 0)) && chan->modelock_key)
	{
		char *newmode;
		char *modelock = NULL;
		char *new_mode_list = NULL;
		char *save, *save1;
		char *args = NULL, *args1 = NULL;
		int add = 0;
		bzero(buffer, sizeof(buffer));
		context;
		
		malloc_strcpy(&modelock, chan->modelock_key);
		malloc_strcpy(&new_mode_list, mode_list);
		save1 = new_mode_list;
		save = modelock;
		new_mode_list = next_arg(new_mode_list, &args1);
		modelock = next_arg(modelock, &args);		

		while (*modelock)
		{
			newmode = strchr(mode_list, *modelock);
			switch(*modelock)
			{
				case '+':
					add = 1;
					break;
				case '-':
					add = 0;
					break;
				case 'k':
					if (newmode)
					{
						if (add)
						{
							char *key;
							key = next_arg(args1, &args1);
							if (chan->key)
							{
								strcat(buffer, "-k " );
								strcat(buffer, chan->key);
							}
							key = next_arg(args, &args);
							if (key)
							{
								strcat(buffer, " +k ");
								strcat(buffer, key);
								strcat(buffer, " ");
							}
						}
						else
						{
							if (!chan->key)
								break;
							strcat(buffer, "-k ");
							strcat(buffer, chan->key);
						}
						strcat(buffer, " ");
					}
					break;
				case 'l':
					if (newmode)
					{
						if (add)
						{
							int limit = 0;
							if (args && *args)
								limit = strtoul(args, &args, 10);
							if (limit > 0)
							{
								strcat(buffer, "+l ");
								strcat(buffer, ltoa(limit));
								strcat(buffer, " ");
							}
						}
						else
						{
							chan->limit = 0;
							strcat(buffer, "-l");
						}
					}
					break;
				default:
					if (newmode)
					{
						if (add)
						{
							strcat(buffer, "+");
						}
						else
						{
							strcat(buffer, "-");
						}
						buffer[strlen(buffer)] = *modelock;
						strcat(buffer, " ");
					}
					break;
			}
			modelock++;
		}
		if (chan && chan->chop && buffer)
			send_to_server("MODE %s %s", chan->channel, buffer);
		new_free(&save);
		new_free(&save1);
		return 1;
	}
	return 0;
}

void mode_lock _((char *command, char *args, char *subargs))
{
ChannelList *chan;
int i = 0;
char *channel = NULL;
u_long mode = 0;
int value = 0;

char *t = NULL;
char *key = NULL;

	context;
	if (command && *command && !strcmp(command, "ModeLock"))
	{
		if (!args || !*args)
		{
			userage(command, "[channel] <+/-><instampkl #>");
			return;
		}
		t = next_arg(args, &args);
		if (is_channel(t))
		{
			channel = t;		
			t = next_arg(args, &args);
		} else
			channel = get_channel_by_refnum(0);
		if (!t || !*t)
		{
			bitchsay("No Mode Specified");
			return;
		}
		if ((chan = lookup_channel(channel, from_server, 0)))
		{
			char valid_mode[BIG_BUFFER_SIZE + 1];
			char *buffer = NULL;
			int limit = -1;
			int add = 0;
			memset(valid_mode, 0, sizeof(valid_mode));
			while (*t && i < BIG_BUFFER_SIZE)
			{
				switch(*t)
				{
					case '+':
						add = 1;
						valid_mode[i++] = '+';
						break;
					case '-':
						add = 0;
						valid_mode[i++] = '-';
						break;
					case 'm':
						value = MODE_MODERATED;
						valid_mode[i++] = *t;
						break;
					case 'a':
						value = MODE_ANON;
						valid_mode[i++] = *t;
						break;
					case 'i':
						value = MODE_INVITE;
						valid_mode[i++] = *t;
						break;
					case 'n':
						value = MODE_MSGS;
						valid_mode[i++] = *t;
						break;
					case 's':
						value = MODE_SECRET;
						valid_mode[i++] = *t;
						break;
					case 't':
						value = MODE_TOPIC;
						valid_mode[i++] = *t;
						break;
					case 'p':
						value = MODE_PRIVATE;
						valid_mode[i++] = *t;
						break;
					case 'k':
						value = MODE_KEY;
						valid_mode[i++] = *t;
						if (add)
							key = next_arg(args, &args);
						else
							key = NULL;
						break;
						
					case 'l':
						valid_mode[i++] = *t;
						value = MODE_LIMIT;
						if (add)
							if (args && *args)
								limit = strtoul(args, &args, 10);
						else
							limit = 0;
						break;
					default:
						break;
				}
				if (add)
					mode |= value;
				else
					mode &= ~value;
		
				t++;
			}
			chan->modelock_val = mode;
			malloc_strcpy(&buffer, valid_mode);
			if (key && *key)
			{
				malloc_strcat(&buffer, " ");
				malloc_strcat(&buffer, key);
			}
			if (limit > 0)
			{
				malloc_strcat(&buffer, " ");
				malloc_strcat(&buffer, ltoa(limit));
			}
			if (*buffer)
			{
				malloc_strcpy(&chan->modelock_key, buffer);
				bitchsay("%s Mode Locked at: [%s] %s", chan->channel, buffer, (chan->limit > 0) ? "Users":"");
			} else
				bitchsay("Invalid Mode for [%s]", chan->channel);
		} else
			bitchsay("No Such Channel");
	}
	else if (command && *command && !my_stricmp(command, "ClearLock"))
	{

		if (!args || !*args)
		{
			userage(command, "[channel|*]");
			return;
		}
		t = next_arg(args, &args);
		if (t && *t && *t != '*')
		{
			if ((chan = lookup_channel(t, from_server, 0)))
			{
				new_free(&chan->modelock_key);
				chan->modelock_val = 0;
				bitchsay("Cleared %s Mode Lock", chan->channel);
			} else
				bitchsay("No such Channel [%s]", t);
		} 
		else if (t && *t && *t == '*')
		{
			for (chan = server_list[from_server].chan_list; chan; chan = chan->next)
			{
				new_free(&chan->modelock_key);
				chan->modelock_val = 0;
			}
			bitchsay("Cleared All Channel Mode Locks");
		} else
			userage(command, "[channel|*]");
	}
	else
	{
		for (i = 0; i < number_of_servers; i++ )
		{
			for (chan = server_list[i].chan_list; chan; chan = chan->next)
				bitchsay("Lock on %s: %s", chan->channel, chan->modelock_key ? chan->modelock_key : "none");
		}
	}
}

void randomnick _((char *command, char *args, char *subargs))
{
char *prefix = NULL, *p;
int count = 1;
int len = 0;

	context;
	while ((p = next_arg(args, &args)))
	{
		if (isdigit(*p))
			count = atol(p);
		else
			prefix = p;
	}
	if (prefix && (len = strlen(prefix)))
	{
		if (len > 5)
			*(prefix + 6) = 0;
	}
	while (count > 0)
	{
		send_to_server("NICK %s%s", prefix?prefix:"", random_str(3,9-len));
		count--;
	}
}

void topic_lock _((char *command, char *args, char *subargs))
{
ChannelList *chan;
char *t, *channel;

	context;
	if (!args || !*args)
	{
		userage(command, "<#channel> [on\002|\002off]");
		return;
	}
	t = next_arg(args, &args);
	if (args && *args)
	{
		channel = t;
		if (!is_channel(channel))
			return;
		t = next_arg(args, &args);
	} else 
		channel = get_channel_by_refnum(0);
		
	if ((chan = lookup_channel(channel, from_server, 0)))
	{
		if (t && *t && !my_stricmp(t, "ON"))
		{
			chan->topic_lock = 1;
		}
		else if (t && *t && !my_strnicmp(t, "OF", 2))
			chan->topic_lock = 0;
		else
			userage(command, "<#channel> [on\002|\002off]");
		bitchsay("Topic lock for [%s] - %s", chan->channel, on_off(chan->topic_lock));
	}
}

void sping (char *command, char *args, char *subargs)
{
	char *servern = next_arg(args, &args);

	context;
	if (!servern || !*servern)
		servern = get_server_name(from_server);
	if (servern && *servern && match("*.*", servern))
	{
		bitchsay("Sent PING to server [\002%s\002]", servern);
		if (!my_stricmp(servern, get_server_itsname(from_server)))
			send_to_server("PING %lu %s", time(NULL), servern);
		else
		{
			in_sping = time(NULL);
			send_to_server("PING %s %s", get_server_itsname(from_server), servern);
		}
	}
	else
		userage(command, "[servername]");
}

void tog_fprot(char *command, char *args, char *subargs)
{
static int here = 0;

	context;
	if (args && *args)
	{
		if (!my_stricmp(args, "ON"))
			here = 0;
		else if (!my_stricmp(args, "OFF"))
			here = 1;
		else
		{
			userage(command, "On|Off");
			return;
		}
	}		

	if (here)
	{
		set_int_var(CTCP_FLOOD_PROTECTION_VAR, 0);
		set_int_var(FLOOD_PROTECTION_VAR, 0);
		here = 0;
	} else
	{
		set_int_var(CTCP_FLOOD_PROTECTION_VAR, 1);
		set_int_var(FLOOD_PROTECTION_VAR, 1);
		here = 1;
	}
	bitchsay("Toggled flood protection - [%s]", on_off(here));
}

void do_toggle (char *command, char *args, char *subargs)
{
	context;
if (!args || !*args)
{

put_it("%s", convert_output_format("%GÚÄÄÄÄÄ---%gÄ%G-%K[ %WBitchX %wToggles %K]-%gÄÄ%G-%gÄÄÄÄÄÄ---%KÄ%g--%KÄÄ%g-%KÄÄÄÄÄÄÄÄÄ--- --  - --- -- -", NULL));
put_it("%s", convert_output_format("%G³   %Cauto_ns%clookup %K[%W$[-3]0%K]    %Cctcp_f%clood_protection %K[%W$[-3]1%K]    %Cbeep%c        %K[%W$[-3]2%K]", "%s %s %s", on_off(get_int_var(AUTO_NSLOOKUP_VAR)), on_off(get_int_var(CTCP_FLOOD_PROTECTION_VAR)), on_off(get_int_var(BEEP_VAR))));
put_it("%s", convert_output_format("%G³   %Cpub%cflood      %K[%W$[-3]0%K]    %Cflood_p%crotection      %K[%W$[-3]1%K]    %Ckickf%clood   %K[%W$[-3]2%K]", "%s %s %s", on_off(get_int_var(PUBFLOOD_VAR)), on_off(get_int_var(FLOOD_PROTECTION_VAR)), on_off(get_int_var(KICKFLOOD_VAR))));
put_it("%s", convert_output_format("%g³   %Cdcc%c_autoget   %K[%W$[-3]0%K]    %Cflood_k%cick            %K[%W$[-3]1%K]    %Cmsg%clog      %K[%W$[-3]2%K]", "%s %s %s", on_off(get_int_var(DCC_AUTOGET_VAR)), on_off(get_int_var(FLOOD_KICK_VAR)), on_off(get_int_var(MSGLOG_VAR))));
put_it("%s", convert_output_format("%G³   %Cll%cook         %K[%W$[-3]0%K]    %Cdeop%cflood             %K[%W$[-3]1%K]    %Cjoin%cflood   %K[%W$[-3]2%K]", "%s %s %s", on_off(get_int_var(LLOOK_VAR)), on_off(get_int_var(DEOPFLOOD_VAR)), on_off(get_int_var(JOINFLOOD_VAR))));
put_it("%s", convert_output_format("%g|   %Cauto_w%chowas   %K[%W$[-3]0%K]    %Cverb%cose_ctcp          %K[%W$[-3]1%K]    %Cnickfl%cood   %K[%W$[-3]2%K]", "%s %s %s", on_off(get_int_var(AUTO_WHOWAS_VAR)), on_off(get_int_var(VERBOSE_CTCP_VAR)), on_off(get_int_var(NICKFLOOD_VAR))));
put_it("%s", convert_output_format("%G:   %Ccl%coak         %K[%W$[-3]0%K]    %Coper%cview              %K[%W$[-3]1%K]    %Cshit%clist    %K[%W$[-3]2%K]", "%s %s %s", on_off(get_int_var(CLOAK_VAR)), on_off(get_int_var(OV_VAR)), on_off(get_int_var(SHITLIST_VAR))));
put_it("%s", convert_output_format("%G:   %Ckick_o%cps      %K[%W$[-3]0%K]    %Cannoy%c_kick            %K[%W$[-3]1%K]    %Cuser%clist    %K[%W$[-3]2%K]", "%s %s %s", on_off(get_int_var(KICK_OPS_VAR)), on_off(get_int_var(ANNOY_KICK_VAR)), on_off(get_int_var(USERLIST_VAR))));
put_it("%s", convert_output_format("%K|   %Chack%cing       %K[%W$[-3]0%K]    %Cnick_c%completion       %K[%W$[-3]1%K]    %Cauto_r%cejoin %K[%W$[-3]2%K]","%s %s %s", on_off(get_int_var(HACKING_VAR)),on_off(get_int_var(NICK_COMPLETION_VAR)), on_off((get_int_var(AUTO_REJOIN_VAR)?1:0)) ));
put_it("%s", convert_output_format("%g:   %Cdisp%clay_ansi  %K[%W$[-3]0%K]    %Wtype /toggle <setting>         %Clog         %K[%W$[-3]1%K]", "%s %s", on_off((get_int_var(AUTO_REJOIN_VAR)?1:0)),on_off((get_int_var(LOG_VAR)?1:0)) ));
} 
else
{
	if (!my_strnicmp(args, "auto_nslookup", 7))
	{
		set_int_var(AUTO_NSLOOKUP_VAR, (get_int_var(AUTO_NSLOOKUP_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GAuto-NSlookup %K[%W$[-3]0%K]","%s", on_off(get_int_var(AUTO_NSLOOKUP_VAR)) ));
	} else if (!my_strnicmp(args, "pubflood", 3))
	{
		set_int_var(PUBFLOOD_VAR, (get_int_var(PUBFLOOD_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GPub Flood %K[%W$[-3]0%K]","%s", on_off(get_int_var(PUBFLOOD_VAR))));
	} else if (!my_strnicmp(args, "dcc_autoget", 3))
	{
		set_int_var(DCC_AUTOGET_VAR, (get_int_var(DCC_AUTOGET_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GDCC Auto Get %K[%W$[-3]0%K]","%s", on_off(get_int_var(DCC_AUTOGET_VAR))));
	} else if (!my_strnicmp(args, "llook", 2))
	{
		set_int_var(LLOOK_VAR, (get_int_var(LLOOK_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GLink Look %K[%W$[-3]0%K]","%s", on_off(get_int_var(LLOOK_VAR))));
	} else if (!my_strnicmp(args, "auto_whowas", 6))
	{
		set_int_var(AUTO_WHOWAS_VAR, (get_int_var(AUTO_WHOWAS_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GAuto-WhoWas %K[%W$[-3]0%K]","%s", on_off(get_int_var(AUTO_WHOWAS_VAR))));
	} else if (!my_strnicmp(args, "cloak", 2))
	{
		set_int_var(CLOAK_VAR, (get_int_var(CLOAK_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GCloaking %K[%W$[-3]0%K]","%s", on_off(get_int_var(CLOAK_VAR))));
	} else if (!my_strnicmp(args, "kick_ops", 6))
	{
		set_int_var(KICK_OPS_VAR, (get_int_var(KICK_OPS_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GKick Ops %K[%W$[-3]0%K]","%s", on_off(get_int_var(KICK_OPS_VAR))));
	} else if (!my_strnicmp(args, "ctcp_flood_protection", 6))
	{
		set_int_var(CTCP_FLOOD_PROTECTION_VAR, (get_int_var(CTCP_FLOOD_PROTECTION_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GCtcp Flood Protection %K[%W$[-3]0%K]","%s", on_off(get_int_var(CTCP_FLOOD_PROTECTION_VAR))));
	} else if (!my_strnicmp(args, "flood_protection",7))
	{
		set_int_var(FLOOD_PROTECTION_VAR, (get_int_var(FLOOD_PROTECTION_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GFlood Protection %K[%W$[-3]0%K]","%s", on_off(get_int_var(FLOOD_PROTECTION_VAR))));
	} else if (!my_strnicmp(args, "flood_kick", 7))
	{
		set_int_var(FLOOD_KICK_VAR, (get_int_var(FLOOD_KICK_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GFlood Kicks %K[%W$[-3]0%K]","%s", on_off(get_int_var(FLOOD_KICK_VAR))));
	} else if (!my_strnicmp(args, "deopflood",4))
	{
		set_int_var(DEOPFLOOD_VAR, (get_int_var(DEOPFLOOD_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GDeOp Flood %K[%W$[-3]0%K]","%s", on_off(get_int_var(DEOPFLOOD_VAR))));
	} else if (!my_strnicmp(args, "verbose_ctcp",4))
	{
		set_int_var(VERBOSE_CTCP_VAR, (get_int_var(VERBOSE_CTCP_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GVerbose CTCP %K[%W$[-3]0%K]","%s", on_off(get_int_var(VERBOSE_CTCP_VAR))));
	} else if (!my_strnicmp(args, "operview", 4))
	{
		set_int_var(OV_VAR, (get_int_var(OV_VAR)));
		put_it("%s", convert_output_format("%cToggled %GOver View %K[%W$[-3]0%K]","%s", on_off(get_int_var(OV_VAR))));
	} else if (!my_strnicmp(args, "annoy_kick",4))
	{
		set_int_var(ANNOY_KICK_VAR, (get_int_var(ANNOY_KICK_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GAnnoy Kicks %K[%W$[-3]0%K]","%s", on_off(get_int_var(ANNOY_KICK_VAR))));
	} else if (!my_strnicmp(args, "beep",4))
	{
		set_int_var(BEEP_VAR, (get_int_var(BEEP_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GBeep %K[%W$[-3]0%K]","%s", on_off(get_int_var(BEEP_VAR))));

	} else if (!my_strnicmp(args, "kickflood",5))
	{
		set_int_var(KICKFLOOD_VAR, (get_int_var(KICKFLOOD_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GKick Flood %K[%W$[-3]0%K]","%s", on_off(get_int_var(KICKFLOOD_VAR))));
	} else if (!my_strnicmp(args, "msglog", 3))
	{
		set_int_var(MSGLOG_VAR, (get_int_var(MSGLOG_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GMSG log %K[%W$[-3]0%K]","%s", on_off(get_int_var(MSGLOG_VAR))));
	} else if (!my_strnicmp(args, "joinflood", 4))
	{
		set_int_var(JOINFLOOD_VAR, (get_int_var(JOINFLOOD_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GJoin Flood %K[%W$[-3]0%K]","%s", on_off(get_int_var(JOINFLOOD_VAR))));
	} else if (!my_strnicmp(args, "nickflood", 6))
	{
		set_int_var(NICKFLOOD_VAR, (get_int_var(NICKFLOOD_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GNick Flood %K[%W$[-3]0%K]","%s", on_off(get_int_var(NICKFLOOD_VAR))));
	} else if (!my_strnicmp(args, "shitlist", 4))
	{
		set_int_var(SHITLIST_VAR, (get_int_var(SHITLIST_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GShitList %K[%W$[-3]0%K]","%s", on_off(get_int_var(SHITLIST_VAR))));
	} else if (!my_strnicmp(args, "userlist", 4))
	{
		set_int_var(USERLIST_VAR, (get_int_var(USERLIST_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GUserList %K[%W$[-3]0%K]","%s", on_off(get_int_var(USERLIST_VAR))));
	} else if (!my_strnicmp(args, "hacking", 4))
	{
		set_int_var(HACKING_VAR, (get_int_var(HACKING_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GHacking %K[%W$[-3]0%K]","%s", on_off(get_int_var(HACKING_VAR))));
	} else if (!my_strnicmp(args, "auto_rejoin", 6))
	{
		set_int_var(AUTO_REJOIN_VAR, (get_int_var(AUTO_REJOIN_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GAuto_Rejoin %K[%W$[-3]0%K]","%s", on_off(get_int_var(AUTO_REJOIN_VAR))));
	} else if (!my_strnicmp(args, "nick_completion", 6))
	{
		set_int_var(NICK_COMPLETION_VAR, (get_int_var(NICK_COMPLETION_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GNick Completion %K[%W$[-3]0%K]","%s", on_off(get_int_var(NICK_COMPLETION_VAR))));
	} else if (!my_strnicmp(args, "display_ansi", 4))
	{
		set_int_var(DISPLAY_ANSI_VAR, (get_int_var(DISPLAY_ANSI_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GAnsi Display %K[%W$[-3]0%K]","%s", on_off(get_int_var(DISPLAY_ANSI_VAR))));
	} else if (!my_strnicmp(args, "log", 3))
	{
		int old_window_display = window_display;
		set_int_var(LOG_VAR, (get_int_var(LOG_VAR)?0:1));
		put_it("%s", convert_output_format("%cToggled %GLogging %K[%W$[-3]0%K]","%s", on_off(get_int_var(LOG_VAR))));
		window_display = 0;
		logger(curr_scr_win, NULL, get_int_var(LOG_VAR));
		window_display = old_window_display;
	} else
		put_it("   Unknown /toggle [%s]", args);
}

}


static char *nick_arg = NULL;
static char *reason = NULL;
int in_who_kill = 0;
static char *tnick_arg = NULL;
static char *treason = NULL;
int in_trace_kill = 0;

void who_handlekill(char *nick, char *user, char *host)
{
static int count = 0;

	context;
	if (!nick)
	{
		if (count == 0)
			bitchsay("No Match for kill of [%s]", nick_arg);
		new_free(&reason);
		new_free(&nick_arg);
		in_who_kill = 0;   
		count = 0;
		return;
        }
	if (!my_stricmp(nick, get_server_nickname(from_server)))
		return;
	bitchsay("Killing %s!%s@%s[%s] %d", nick, user, host, nick_arg, ++count);
	if (!reason)
		malloc_strcpy(&reason, get_reason(nick));
	send_to_server("KILL %s :%s (%d)", nick, reason, count);
}

void show_version (char *command, char *args, char *subargs)
{
char *nick;
char *version_buf = NULL;
#ifdef HAVE_UNAME
struct utsname buf;
	context;
	uname(&buf);
	malloc_strcpy(&version_buf, stripansicodes(convert_output_format(get_string_var(FORMAT_VERSION_VAR), "%s %s %s %s", irc_version, internal_version, buf.sysname, buf.release?buf.release:"")));
#else
	malloc_strcpy(&version_buf, stripansicodes(convert_output_format(get_string_var(FORMAT_VERSION_VAR), "%s %s %s %s", irc_version, internal_version, "unknown", "")));
#endif
	if (args && *args)
		nick = next_arg(args, &args);
	else
		nick = get_channel_by_refnum(0);
	send_text(nick, version_buf, "PRIVMSG", 1, 0);
	new_free(&version_buf);
}

void whokill (char *command, char *args, char *subargs)
{
	char *pattern;
	
	context;
	if (nick_arg && in_who_kill)
	{
		bitchsay("Already doing %s", command);
		return;
	}
        if (!(pattern=next_arg(args, &args)))
	{
		userage(command, "<pattern> [reason]");
		return;
	}

	if (args && *args)
		malloc_strcpy(&reason, args); 
	else
		new_free(&reason);
		
	if (*pattern != '*')
		malloc_strcpy(&nick_arg, "*");
	malloc_strcat(&nick_arg, pattern);
	if ((*(nick_arg+strlen(nick_arg)-1) != '*') || (strlen(nick_arg) == 1))
		malloc_strcat(&nick_arg, "*");
	send_to_server("WHO %s", nick_arg);
	in_who_kill = 1;
}

void trace_handlekill(char *nick)
{
static int count = 0;

	context;
	if (!nick)
	{
		if (count == 0 && in_trace_kill != 2)
		{
			bitchsay("No Match for trace kill of [%s]", tnick_arg);
		}
		new_free(&treason);
		new_free(&tnick_arg);
		in_trace_kill = 0;   
		count = 0;
		return;
        }
	if (!my_stricmp(nick, get_server_nickname(from_server)))
		return;
	bitchsay("Killing %s[%s] %d", nick, tnick_arg, ++count);
	if (!treason)
		malloc_strcpy(&treason, get_reason(nick));
	send_to_server("KILL %s :%s (%d)", nick, treason, count);
}

void handle_tracekill(char *nick, char *user, char *host)
{
	char temp[20];
	if (!nick)
	{
		trace_handlekill(NULL);
		return;
	}
	if (match(tnick_arg, nick))
	{
		if (in_trace_kill == 2)
			bitchsay("User: %s", nick);
		else
		{
			strncpy(temp, nick, 15);
			temp[15] = '\0';
			if (!strrchr(temp, '['))
				return;
			*strrchr(temp, '[') = '\0';
			if (!my_stricmp(temp, get_server_nickname(from_server)))
				return;
			trace_handlekill(temp);
		}
	}
}

void tracekill(char *command, char *args, char *subargs)
{
	char *pattern;

	context;
	if (in_trace_kill)
	{
		bitchsay("Already in %s", command);
		return;
	}
	if (!(pattern = next_arg(args, &args)))
	{
		userage(command, "<pattern> [reason]");
		return;
	}
	if (args && *args)
		malloc_strcpy(&treason, args);
	else
		new_free(&treason);

        if (*pattern != '*')
	{
                malloc_strcpy(&tnick_arg, "*");
	        malloc_strcat(&tnick_arg, pattern);
	} else malloc_strcpy(&tnick_arg, pattern);
        if ((*(tnick_arg+strlen(tnick_arg)-1) != '*') || (strlen(tnick_arg) == 1))
                malloc_strcat(&tnick_arg, "*");

	if (treason && *treason == '-')
		in_trace_kill = 2;
	else
		in_trace_kill = 1;
	send_to_server("TRACE");
}

void traceserv(char *command, char *args, char *subargs)
{
	char *server, *pattern;

	context;
	if (in_trace_kill)
	{
		bitchsay("Already in %s", command);
		return;
	}
	if (!(server = next_arg(args, &args)) ||
		!(pattern = next_arg(args, &args)))
	{
		userage(command, "<server> <pattern> [reason]");
		return;
	}
	if (args && *args)
	        malloc_strcpy(&treason, args);
	else
		new_free(&treason);
        if (*pattern != '*')
	{
                malloc_strcpy(&tnick_arg, "*");
	        malloc_strcat(&tnick_arg, pattern);
	} else malloc_strcpy(&tnick_arg, pattern);
        if ((*(tnick_arg+strlen(tnick_arg)-1) != '*') || (strlen(tnick_arg) == 1))
                malloc_strcat(&tnick_arg, "*");

	if (treason && *treason == '-') 
		in_trace_kill = 2;
	else
		in_trace_kill = 1;
        send_to_server("TRACE %s", server);
}

#if 0
FTP shell for irc
by R0M
v1.0a
^assign ftp_pgm ncftp

alias ftp {
 ^window new name FTP level crap lastlog all
 ^exec -window -name FTP $ftp_pgm $0- 
  hmo
 ^query %FTP
 ^on ^exec_exit FTP {hmf;window show ncftp kill}
 }

alias hmo ^set hold_mode on
alias hmf ^set hold_mode off
echo Welcome to ftp v1.0a by R0M
echo to connect simply type /ftp <site>
echo to find out which dir you are in simply type pwd
echo have fun :)
#endif

void ftp ( char *command, char *args, char *subargs)
{
Window *window, *tmp;
char name[40];
char *pgm = NULL;
int direct = 0;

	context;
	sprintf(name, "%%%s", command);
	if (command && !my_stricmp(command, "ftp"))
	{
		pgm = get_string_var(FTP_VAR);
		direct = 1;
	}
	else if (command && !my_stricmp(command, "shell"))
	{
		pgm = get_string_var(SHELL_VAR);
		direct = 1;
	}
	else
		pgm = command;
		
	if (!args || !*args)
	{
		if (!is_window_name_unique(name+1))
		{
			int logic = -1;
			if ((tmp = get_window_by_name(name+1)))
			{
				delete_window(tmp);
				if ((logic = logical_to_index(name+1)) > -1)
					kill_process(logic, 15);
				else bitchsay("No such process [%s]", name+1);
			}
			recalculate_windows();
			update_all_windows();
			return;
		}
	}
	if ((tmp = new_window()))
	{
		int refnum;
		char *p = NULL;
		window = tmp;
		if (is_window_name_unique(name+1))
		{
			malloc_strcpy(&window->name, name+1);
			window->update |= UPDATE_STATUS;
		}

		hide_window(window);
		recalculate_windows();
		set_current_window(window);
		window->window_level = LOG_NONE;
		revamp_window_levels(window);
		window->lastlog_level = LOG_NONE;
		bits_to_lastlog_level(window->lastlog_level);
		refnum = window->refnum;
		
		update_all_windows();
		malloc_sprintf(&p, "%s %s", pgm, args);
		start_process(p, name+1, NULL, NULL, refnum, direct);
		if (is_process(name))
		{
			NickList *tmp_nick = NULL;
			malloc_strcpy(&window->query_nick, name);
			tmp_nick = (NickList *)new_malloc(sizeof(NickList));
			bzero(tmp_nick, 0);
			malloc_strcpy(&tmp_nick->nick, name);
			add_to_list((List **)&window->nicks, (List *)tmp_nick);
		}
		new_free(&p);
	} else bitchsay("Unable to create a new window");	
}

void botlink (char *command, char *args, char *subargs)
{
	return;
}

void lkw (char *command, char *args, char *subargs)
{
	delete_window(curr_scr_win);
	update_all_windows();
}

void jnw (char *command, char *args, char *subargs)
{
char *channel;

	context;
	channel = next_arg(args, &args);
	if (channel && is_channel(channel))
	{
		Window *tmp;
		if ((tmp = new_window()))
		{
			int     server;
			server = from_server;
			from_server = tmp->server;
			if (*channel == '#' || *channel == '&')
				send_to_server("JOIN %s%s%s", channel, args?" ":"", args?args:"");
			else
				send_to_server("JOIN #%s%s%s", channel, args?" ":"", args?args:"");
			malloc_strcpy(&tmp->waiting_channel, channel);
			from_server = server;
			update_all_windows();
		}
	} else
		userage(command, "<#channel>");
}


int delay_gain_nick(void *arg)
{
	if (org_nick)
		add_to_userhost_queue(org_nick, gain_nick, "%s", org_nick);
	return 0;
}

void gain_nick (WhoisStuff *stuff, char *nick, char *args)
{
	if (!org_nick)
		return;
	if (!stuff || (stuff->nick  && !strcmp(stuff->user, "<UNKNOWN>") && !strcmp(stuff->host, "<UNKNOWN>")))
	{
		my_send_to_server(from_server, "NICK %s", org_nick);
		bitchsay("Regained nick [%s]", org_nick);
		new_free(&org_nick);
		update_all_status(curr_scr_win, NULL, 0);
		update_input(UPDATE_ALL);
		return;
	}
	add_timer("", 20, delay_gain_nick, NULL, NULL);
}

void orig_nick (char *command, char *args, char *subargs)
{
char *nick;
	if (!args || !*args)
	{
		userage("OrigNick", "<nickname>");
		return;
	}
	nick = next_arg(args, &args);
	if (nick && *nick == '-')
	{
		if (!org_nick)
			bitchsay("Not trying to gain a nick");
		else
		{
			bitchsay("Removing gain nick [%s]");
			new_free(&org_nick);
		}
	}
	else
	{
		if ((nick = check_nickname(nick)))
		{
			malloc_strcpy(&org_nick, nick);
			add_to_userhost_queue(org_nick,gain_nick, "%s", org_nick); 
			bitchsay("Trying to regain nick [%s]", org_nick);
		}
		else
			bitchsay("Nickname was all bad chars");
	}
}

void add_bad_nick _((char *command, char *args, char *subargs))
{
char *buffer = NULL;
LameList *lame_nick = NULL;
char *nick = NULL;
extern LameList *lame_list;
int add = 0;
	if (!args || !*args || !command)
	{
		if (!command)
		{
			int i = 0;
			if (!lame_list)
			{
				bitchsay("There are no nicks on your lame nick list");
				return;
			}
			bitchsay("Lame Nick list:");
			for (lame_nick = lame_list; lame_nick; lame_nick = lame_nick->next)
			{
				if (buffer)
					m_s3cat(&buffer, lame_nick->nick, "\t");
				else
					buffer = m_sprintf("%s\t", lame_nick->nick);
				i++;
				if (i == 6)
				{
					i = 0;
					put_it("%s", buffer);
					new_free(&buffer);
				}	
			}
			if (buffer)
				put_it("%s", buffer);
			new_free(&buffer);
		}
		else
			userage(command, " <nick>|<nick nick nick>");
		return;
	}
	add = !my_stricmp(command, "addlamenick") ? 1 : 0;
	bitchsay("%s %s LameNick list", add ? "Added":"Removed", add?"to":"from");
	while (args && *args)
	{
		nick = next_arg(args, &args);
		if (add && nick)
		{
			
			lame_nick = (LameList *)new_malloc(sizeof(LameList));
			malloc_strcpy(&lame_nick->nick, nick);
			add_to_list((List **)&lame_list, (List *)lame_nick);
		}
		else if (!add && nick)
		{
			lame_nick = (LameList *)remove_from_list((List **)&lame_list, nick);
			if (lame_nick)
			{
				new_free(&lame_nick->nick);
				new_free((char **)&lame_nick);
			}
			else
				nick = NULL;
		}
		if (nick && *nick)
			if (buffer)
				m_s3cat(&buffer, nick, "\t");
			else
				buffer = m_sprintf("%s\t", nick);
	}
	if (buffer)
		put_it("%s", buffer);
	new_free(&buffer);
}

int grab_http(char *from, char *to, char *text) 
{
	if (get_int_var(HTTP_GRAB_VAR) && stristr(text, "HTTP:"))
	{
		FILE *fp;
		char *filename = NULL;
		char buffer[BIG_BUFFER_SIZE+1];
		sprintf(buffer, "%s/BitchX.http", get_string_var(CTOOLZ_DIR_VAR));
		filename = expand_twiddle(buffer);
		if (filename && (fp = fopen(filename, "a+")))
		{
			fprintf(fp, "%s %s -- %s\n", from, to, text);
			fclose(fp);
		}
		new_free(&filename);
		return 1;
	}
	return 0;
}

void serv_stat _((char *command, char *args, char *subargs))
{
extern int nick_collisions, oper_kills, serv_fakes, serv_unauth, serv_split;
extern int serv_rejoin, client_connects, serv_rehash, client_exits,serv_klines;
extern int client_floods, client_invalid, stats_req, client_bot, client_bot_alarm;
extern int oper_requests;
put_it("%s", convert_output_format("%GÚÄÄÄÄÄ---%gÄ%G-%K[ %WServer %wStats %K]-%gÄÄ%G-%gÄÄÄÄÄÄ---%KÄ%g--%KÄÄ%g-%KÄÄÄÄÄÄÄÄÄ--- --  - --- -- -", NULL));
put_it("%s", convert_output_format("%G³ %CN%cick Collisions %K[%W$[-4]0%K]    %CO%cper Kills   %K[%W$[-4]1%K]", "%d %d", nick_collisions, oper_kills));
put_it("%s", convert_output_format("%G³ %CF%cake Modes      %K[%W$[-4]0%K]    %CU%cnauth       %K[%W$[-4]1%K]", "%d %d",serv_fakes, serv_unauth));
put_it("%s", convert_output_format("%g³ %CH%cigh Traffic    %K[%W$[-4]0%K]    %CN%corm Traffic %K[%W$[-4]1%K]", "%d %d",serv_split, serv_rejoin));
put_it("%s", convert_output_format("%G³ %CT%cotal Clients   %K[%W$[-4]0%K]    %CS%cerv rehash  %K[%W$[-4]1%K]", "%d %d",client_connects, serv_rehash));
put_it("%s", convert_output_format("%g| %CC%client exits    %K[%W$[-4]0%K]    %CK%c-lines adds %K[%W$[-4]1%K]", "%d %d",client_exits, serv_klines));
put_it("%s", convert_output_format("%G: %CC%client Floods   %K[%W$[-4]0%K]    %CS%ctats reqs   %K[%W$[-4]1%K]", "%d %d",client_floods, stats_req));
put_it("%s", convert_output_format("%G: %CI%cnvalid User    %K[%W$[-4]0%K]    %CO%cper Reqs    %K[%W$[-4]1%K]", "%d %d",client_invalid, oper_requests));
put_it("%s", convert_output_format("%K| %CP%cossible Bots   %K[%W$[-4]0%K]", "%d",client_bot));
put_it("%s", convert_output_format("%g: %CB%cot Alarms      %K[%W$[-4]0%K]", "%d",client_bot_alarm));

}

char *strip _((char *str, char *unwanted))
{
static char buffer[BIG_BUFFER_SIZE/4];
register char *cp, *dp;
	if (!str)
		return empty_string;
	for (cp = str, dp = buffer; *cp; cp++)
	{
		if (!index(unwanted, *cp))
			*dp++ = *cp;
	}	
	*dp = 0;
	return buffer;
}


void set_autoreply _((char *command, char *args, char *subargs))
{
char new_nick[NICKNAME_LEN+10];
char not_wanted1[] = "_^\\{}[]|-";
char not_wanted2[] = "_^\\{}[]|-0123456789";
extern char *auto_str;
	if (!args || !*args)
	{
		userage("SetAR", "-|d|pat1 pat2 ..");
		return;
	}
	if (*args == '-')
	{
		set_string_var(AUTO_RESPONSE_VAR, NULL);
		bitchsay("Auto-reply pats are deleted");
		new_free(&auto_str);
	}
	else
	{
		char *p, *new_args = NULL;
		p = next_arg(args, &args);
		if (*p == 'd' && strlen(p) == 1)
		{
			int len = strlen(nickname);
			m_e3cat(&new_args, nickname, " ");
			m_e3cat(&new_args, strip(nickname, not_wanted1), len > 4 ? " ": empty_string);
			if (len > 4)
			{
				bzero(new_nick, sizeof(new_nick));
				new_nick[0] = '*';
				strncpy(new_nick+1, strip(nickname, not_wanted1), 4);
				new_nick[5] = '*';
				if (!strstr(new_args, new_nick))
					m_e3cat(&new_args, new_nick, " ");
			}
			if (!strstr(new_args, strip(nickname, not_wanted2)))
				m_e3cat(&new_args, strip(nickname, not_wanted2), len > 4 ? " ":empty_string);
			if (len > 4)
			{
				bzero(new_nick, sizeof(new_nick));
				new_nick[0] = '*';
				strncpy(new_nick+1, strip(nickname, not_wanted2), 4);
				new_nick[5] = '*';
				if (!strstr(new_args, new_nick))
					malloc_strcat(&new_args, new_nick);
			}
			set_string_var(AUTO_RESPONSE_VAR, new_args);
			reinit_autoresponse(curr_scr_win, new_args, 0);
		}
		else
		{
			do {
				m_3cat(&new_args, p, args?" ":empty_string);
			} while ((p = next_arg(args, &args)));
			set_string_var(AUTO_RESPONSE_VAR, new_args);
			reinit_autoresponse(curr_scr_win, new_args, 0);
		}
		new_free(&new_args);
		bitchsay("Auto-Response now set to [%s]", get_string_var(AUTO_RESPONSE_VAR));
	}
}



#ifdef WANT_DLL

#ifdef NO_DLFCN_H
#   include "../compat/dlfcn.h"
#else
#if defined(__osf1__)
#include <loader.h>
#elif defined(HPUX)
#include <dl.h>
#else
#   include <dlfcn.h>
#endif
#endif

#ifndef RTLD_NOW
#   define RTLD_NOW 1
#endif

#ifndef RTLD_GLOBAL
#   define RTLD_GLOBAL 0
#endif


extern IrcCommandDll *dll_commands;



typedef int (Irc_PackageInitProc)  _((IrcCommandDll **interp));

typedef struct _package_installed {
	struct _package_installed *next;
	char	*name;
} Packages;           

Packages *install_pack = NULL;
                                             
void dll_load _((char *command, char *args, char *subargs))
{
#if defined(__osf__)  /*	OSF /1 1.0/1.1/1.2 and related systems */
ldr_module_t lm;
#elif defined(HPUX)  /* 	HP machines */
	shl_t handle;
#else		     /*		linux SunOS AIX etc */
void *handle = NULL;
#endif
    
char *filename = NULL;
Irc_PackageInitProc *proc1Ptr;
char *p, *procname = NULL;
int code = 0;

	if (command)
	{
		if (install_pack)
		{
			Packages *pkg = install_pack;
			bitchsay("DLL packages installed");
			for ( ; pkg; pkg = pkg->next)
				put_it("DLL %s", pkg->name);
		}
		else
			bitchsay("No dll's loaded");
		return;
	}
	if (!args || !*args)
	{
		userage("LoadDll", "filename.so");
		return;
	}

	filename = next_arg(args, &args);

#if defined(__osf__)
	if ((lm = (Tcl_PackageInitProc *) load(fileName, LDR_NOFLAGS)) == LDR_NULL_MODULE)
	{
		return;
	}       
#elif defined(HPUX)
	if ((handle = shl_load(filename, BIND_IMMEDIATE, 0L)) == NULL)
	{
		return;
	}	
#else
	handle = dlopen(filename, RTLD_NOW | RTLD_GLOBAL);
	if (handle == NULL)
	{
		bitchsay("couldn't load file: %s %s", filename, dlerror());
		return;
	}
#endif

	if ((p = strrchr(filename, '/')))	
		p++;
	else
		p = filename;

	procname  = m_strdup(p);
	if ((p = strchr(procname, '.')))
		*p = 0;

	p = procname;
	*p = toupper(*p);
	p++;
	while (*p)
	{
		*p = tolower(*p++);
	}
	malloc_strcat(&procname, "_Init");
	if (!procname)
		return;
	if (!find_in_list((List **)&install_pack, procname, 0))
	{	
#if defined(__osf__)
		/* OSF1 */
		*proc1Ptr = ldr_lookup_package(pkg, procname);
		code = (proc1Ptr)(&dll_commands);
#elif defined(HPUX)
		/* HPUX */
		if (shl_findsym(&handle, procname, (short) TYPE_PROCEDURE, (void *) proc1Ptr))
			code = (proc1Ptr)(&dll_commands);

#else
		if (!(proc1Ptr = (Irc_PackageInitProc *) dlsym(handle, (char *) procname)))
			bitchsay("UnSuccessful load");
		else
			code = (proc1Ptr)(&dll_commands);
#endif
		if (!code && proc1Ptr)
		{
			Packages *new;
			new = (Packages *) new_malloc(sizeof(Packages));
			new->name = m_strdup(procname);
			add_to_list((List **)&install_pack, (List *)new);
		}
		else if (code)
			bitchsay("Error initiliziing dll");
	}
	else
		bitchsay("Package Already installed");
	new_free(&procname);
}
#endif
