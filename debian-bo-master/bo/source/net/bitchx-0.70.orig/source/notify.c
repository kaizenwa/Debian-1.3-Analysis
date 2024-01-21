/*
 * notify.c: a few handy routines to notify you when people enter and leave irc 
 *
 * Written By Michael Sandrof
 * Copyright(c) 1990 
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * Modified Colten Edwards 96
 */


#include "irc.h"

#include "list.h"
#include "notify.h"
#include "ircaux.h"
#include "whois.h"
#include "hook.h"
#include "server.h"
#include "output.h"
#include "vars.h"
#include "timer.h"
#include "misc.h"
#include "status.h"
#include "userlist.h"

/* NotifyList: the structure for the notify stuff */
typedef	struct	notify_stru
{
	struct	notify_stru	*next;	/* pointer to next notify person */
	char	*nick;			/* nickname of person to notify about */
	char	*user;
	char	*host;
	int	flag;			/* 1=person on irc, 0=person not on irc */
	time_t  on;			/* time of last signon */
	time_t  off;			/* time of last signoff */
}	NotifyList;

extern Server *server_list;
extern int number_of_servers;

extern int no_hook_notify;

/*static int first_notify = 1;*/

/* set this on notify to send a whois nick in order to get there host info */

int delay_notify _((void *list))
{
	char *p = (char *)list, *q;
	char *serv;
	int  server;
	q = p;
	serv = next_arg(p, &p);
	server = my_atol(serv);
	if (is_server_connected(server) && p)
	{
		if (get_int_var(HARD_UH_NOTIFY_VAR))    
			add_to_userhost_queue(p, got_userhost, "%s", p);
		else
			add_ison_to_whois(p, ison_notify);
		server_list[server].in_delay_notify = 0;
	}
/*	first_notify = 0;*/
	new_free(&q);
	return 0;
}

void add_delay_notify(int from_server)
{
char *list = NULL;
NotifyList *new;
	if (from_server > -1 )
	{
		for (new = server_list[from_server].notify_list; new; new = new->next)
		{
			malloc_strcat(&list, new->nick);
			malloc_strcat(&list, " ");
		}
		if (list)
		{
			server_list[from_server].in_delay_notify = 1;
			add_timer("", 60, delay_notify, m_sprintf("%u %s", from_server, list), NULL);
		}
	}
	new_free(&list);
}

/* Rewritten, -lynx */
void
show_notify_list(all)
	int	all;
{
	NotifyList	*tmp;
	char	*list = NULL;
	malloc_strcpy(&list, empty_string);
	
	for (tmp = server_list[from_server].notify_list; tmp; tmp = tmp->next)
	{
		if (tmp->flag)
			put_it("%s", convert_output_format(get_string_var(FORMAT_NOTIFY_ON_VAR), "%s %s %s %d %d", tmp->nick, tmp->user?tmp->user:"?", tmp->host?tmp->host:"?", tmp->on, tmp->off));
	}
	if (all == 1)
	{
		malloc_strcpy(&list, empty_string);
		for (tmp = server_list[from_server].notify_list; tmp; tmp = tmp->next)
		{
			if (!(tmp->flag))
			{
				malloc_strcat(&list, " ");
				malloc_strcat(&list, tmp->nick);
			}
		}
		put_it("%s", convert_output_format(get_string_var(FORMAT_NOTIFY_OFF_VAR), "Currently Absent:%s", list));
	}
	new_free(&list);
}

int hard_uh_notify(int flag, char *nick, char *user, char *host)
{
int servnum = from_server;
NotifyList *new;
char *user1 = NULL, *host1 = NULL;
time_t	now = time(NULL);
	if (user && strcmp(user, "<UNKNOWN>"))
		user1 = user;
	if (host && strcmp(host, "<UNKNOWN>"))
		host1 = host;
	if (!nick)
		return 1;
	for (servnum = 0; servnum < number_of_servers; servnum++)
	{
		if (!(new = (NotifyList *) find_in_list((List **)&server_list[servnum].notify_list, nick, 0)))
		{
			new = (NotifyList *) new_malloc(sizeof(NotifyList));
			memset(new, 0, sizeof(NotifyList));
			new->nick = m_strdup(nick);
			new->user = m_strdup(user1);
			new->host = m_strdup(host1);
			new->flag = 0;
			new->off  = now;
			add_to_list((List **)&server_list[servnum].notify_list, (List *) new);
			return 1;		
		}
		if (!new->user || (user1 && strcmp(new->user, user1)))
			malloc_strcpy(&new->user, user1);
		if (!new->host || (user1 && strcmp(new->host, host1)))
			malloc_strcpy(&new->host, host1);
	}
	return 1;
}

/* notify: the NOTIFY command.  Does the whole ball-o-wax */
/*ARGSUSED*/
void
notify(command, args, subargs)
	char	*command,
		*args,
		*subargs;
{
	char	*nick,
		*list = NULL,
		*ptr;
	int	no_nicks = 1;
	int	do_ison = 0;
	int	servnum = from_server;
#ifdef EPIC_LAMER
	int	old_from_server = from_server;
#endif
	int	shown = 0;
	NotifyList	*new;

	malloc_strcpy(&list, empty_string);
	while ((nick = next_arg(args, &args)) != NULL)
	{
		no_nicks = 0;
		while (nick)
		{
			shown = 0;
			if ((ptr = index(nick, ',')) != NULL)
				*ptr++ = '\0';
			
			if (*nick == '-')
			{
				nick++;
				
				if (*nick)
				{
					for (servnum = 0; servnum < number_of_servers; servnum++)
					{
						if ((new = (NotifyList *) remove_from_list((List **)&(server_list[servnum].notify_list), nick)))
						{
							new_free(&(new->nick));
							new_free(&(new->host));
							new_free(&(new->user));
							new_free((char **)&new);
							
							if (!shown)
							{
								bitchsay("%s removed from notification list", nick);
								shown = 1;
							}
						}
						else
						{
							if (!shown)
							{
								bitchsay("%s is not on the notification list", nick);
								shown = 1;
							}
						}
					}
				}
				else
				{
					for (servnum = 0; servnum < number_of_servers; servnum++)
					{
						while ((new = server_list[servnum].notify_list))
						{
							server_list[servnum].notify_list = new->next;
							new_free(&new->nick);
							new_free(&(new->user));
							new_free(&new->host);
							new_free((char **)&new);
						}
					}
					bitchsay("Notify list cleared");
				}
			}
			else
			{
				/* compatibility */
				if (*nick == '+')
					nick++;

				if (*nick)
				{
					if (index(nick, '*'))
						bitchsay("Wildcards not allowed in NOTIFY nicknames!");
					else
					{
						do_ison = 1;
						for (servnum = 0; servnum < number_of_servers; servnum++)
						{
							if ((new = (NotifyList *) remove_from_list((List **)&server_list[servnum].notify_list, nick)) != NULL)
							{
								new_free(&(new->nick));
								new_free(&(new->user));
								new_free(&(new->host));
								new_free((char **)&new);
							}
							new = (NotifyList *) new_malloc(sizeof(NotifyList));
							memset(new, 0, sizeof(NotifyList));
							new->nick = m_strdup(nick);
							new->flag = 0;
							new->off = time(NULL);
							add_to_list((List **)&server_list[servnum].notify_list, (List *) new);
						}
						malloc_strcat(&list, new->nick);
						malloc_strcat(&list, " ");
						bitchsay("%s added to the notification list", nick);
					}
				} else
					show_notify_list(1);
			}
			nick = ptr;
		}
	}

	if (do_ison)
	{
#if 0
		if (first_notify)
			add_timer("", 2*60, delay_notify, m_sprintf("%u %s", from_server,list), NULL);

		else
#endif
		{
#ifdef EPIC_LAMER
			for (servnum = 0; servnum < number_of_servers; servnum++)
			{
				from_server = servnum;
#endif	
				if (is_server_connected(from_server) && !server_list[from_server].in_delay_notify)
				{
					if (get_int_var(HARD_UH_NOTIFY_VAR))
						add_to_userhost_queue(list, got_userhost, "%s", list);
					else
						add_ison_to_whois(list, ison_notify);
				}
#ifdef EPIC_LAMER
			}
			from_server = old_from_server;
#endif
		}
	}
	
	new_free(&list);	
	if (no_nicks)
		show_notify_list(0);
}

/*
 * do_notify: This simply goes through the notify list, sending out a WHOIS
 * for each person on it.  This uses the fancy whois stuff in whois.c to
 * figure things out.  Look there for more details, if you can figure it out.
 * I wrote it and I can't figure it out.
 *
 * Thank you Michael... leaving me bugs to fix :) Well I fixed them!
 */
void
do_notify()
{
	int	size;
	int	old_from_server = from_server;
	int	servnum = from_server;
	char	buf[BIG_BUFFER_SIZE+1];
	NotifyList	*tmp;

	if (!number_of_servers)
		return;


	tmp = server_list[0].notify_list;
	bzero(buf, sizeof(buf));
	while (tmp)
	{
		for (*buf = 0, size = 0; tmp; tmp = tmp->next)
		{
			size ++; 
			if (size < 5)
			{
				strcat(buf, " ");
				strcat(buf, tmp->nick);
			}
			else
				break;
		}

		if (!size)
			continue;
#ifdef EPIC_LAME
		for (servnum = 0; servnum < number_of_servers; servnum++)
		{
#endif
			if (is_server_connected(servnum) && !server_list[servnum].in_delay_notify && *buf)
			{
				from_server = servnum;
				if (get_int_var(HARD_UH_NOTIFY_VAR))
					add_to_userhost_queue(buf, got_userhost, "%s", buf);
				else
					add_ison_to_whois(buf, ison_notify);
			}
#ifdef EPIC_LAME
		}
#endif
	}
	from_server = old_from_server;
	return;
}

void check_auto_invite(char *nick, char *user, char *host)
{
char *channel;
ChannelList *tmp_chan;
char *uh = NULL;
UserList *tmp;
	channel = get_channel_by_refnum(0);
	malloc_sprintf(&uh, "%s@%s", user, host);
	if (channel && ((tmp = lookup_userlevelc("*", uh, channel, user_list))))
	{
		tmp_chan = (ChannelList *)find_in_list((List **)&(server_list[from_server].chan_list), channel, 0);
		if (tmp_chan && tmp_chan->set_ainv && tmp->level >=tmp_chan->set_ainv)
		{
			bitchsay("Auto-inviting %s to %s", nick, channel);
			send_to_server("NOTICE %s :Auto-invite from %s", nick, get_server_nickname(from_server));
			send_to_server("INVITE %s %s%s%s", nick, channel, tmp_chan->key?" ":"", tmp_chan->key?tmp_chan->key:"");
		}
	}
	new_free(&uh);
}

/*
 * notify_mark: This marks a given person on the notify list as either on irc
 * (if flag is 1), or not on irc (if flag is 0).  If the person's status has
 * changed since the last check, a message is displayed to that effect.  If
 * the person is not on the notify list, this call is ignored 
 * doit if passed as 0 means it comes from a join, or a msg, etc, not from
 * an ison reply.  1 is the other..
 */
void
notify_mark(nick, flag, doit)
	char	*nick;
	int	flag;
	int	doit;
{
	NotifyList	*tmp;
	time_t		now = time(NULL);
	char	*s = get_string_var(NOTIFY_HANDLER_VAR);

	if (from_server != primary_server && from_server != -1)
		return;

	if (!s || (!doit && ('O' == *s)))	/* old notify */
		return;	
		
	if ('N' == *s)			/* noisy notify */
		doit = 1;
	if (!nick)
		return;
	if ((tmp = (NotifyList *) list_lookup((List **) &server_list[from_server].notify_list, nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)) != NULL)
	{
		if (flag)
		{
			if (tmp->flag != 1)
			{
				if (get_int_var(HARD_UH_NOTIFY_VAR))
				{
					if ((tmp->flag != -1) && (do_hook(NOTIFY_SIGNON_UH_LIST, "%s %s %s", nick, tmp->user?tmp->user:"", tmp->host?tmp->host:"")) && doit)
					{
						put_it("%s", convert_output_format(get_string_var(FORMAT_NOTIFY_SIGNON_UH_VAR), "%s %s %s %s",update_clock(GET_TIME),nick,tmp->user,tmp->host));
						tmp->on = now;
					}
					if (tmp->flag != -1 && doit)
						check_auto_invite(nick, tmp->user, tmp->host);
					
				}
				else
					if ((tmp->flag != -1) && (do_hook(NOTIFY_SIGNON_LIST, "%s", nick)) && doit)
					{
						put_it("%s", convert_output_format(get_string_var(FORMAT_WHOIS_SIGNON_VAR), "%s %s",update_clock(GET_TIME), nick));
						tmp->on = now;
					}
				/*
				 * copy the correct case of the nick
				 * into our array  ;)
				 */
				malloc_strcpy(&(tmp->nick), nick);
				malloc_strcpy(&last_notify_nick, nick);
				tmp->flag = 1;
			}
		}
		else
		{
			if (get_int_var(HARD_UH_NOTIFY_VAR))
				if ((tmp->flag == 1) && (do_hook(NOTIFY_SIGNOFF_UH_LIST, "%s %s %s", nick, tmp->user?tmp->user:"", tmp->host?tmp->host:"")) && doit)
				{
					put_it("%s", convert_output_format(get_string_var(FORMAT_NOTIFY_SIGNOFF_UH_VAR), "%s %s %s %s",update_clock(GET_TIME),nick,tmp->user,tmp->host));
					tmp->off = now;
				}
			else
				if ((tmp->flag == 1) && (do_hook(NOTIFY_SIGNOFF_LIST, "%s", nick)) && doit)
				{
					put_it("%s", convert_output_format(get_string_var(FORMAT_NOTIFY_SIGNOFF_VAR), "%s %s",update_clock(GET_TIME),nick));
					tmp->off = now;
				}
			tmp->flag = 0;
		}
	}
}

void
save_notify(fp)
	FILE	*fp;
{
	int size = 0;
	int count = 0;
	NotifyList	*tmp;

	if (number_of_servers && server_list[0].notify_list)
	{
		fprintf(fp, "NOTIFY");
		for (tmp = server_list[0].notify_list; tmp; tmp = tmp->next)
		{
			size++; /* += (strlen(tmp->nick) + 1);*/
			fprintf(fp, " %s", tmp->nick);
			if (size >= 10)
			{
				fprintf(fp, "\n");
				if (tmp->next)
					fprintf(fp, "NOTIFY");
				size = 0;
			}
			count ++;
		}
		fprintf(fp, "\n");
	}
	if (do_hook(SAVEFILE_LIST, "Notify %d", count))
		bitchsay("Saved %d Notify entries", count);
}

/* I hate broken compilers -mrg */
static	char	*vals[] = { "NOISY", "QUIET", "OLD", NULL };

void
set_notify_handler(win, value, unused)
	Window	*win;
	char	*value;
	int	unused;
{
	int	len;
	int	i;
	char	*s;

	if (!value)
		value = empty_string;
	for (i = 0, len = strlen(value); (s = vals[i]); i++)
		if (0 == my_strnicmp(value, s, len))
			break;
	set_string_var(NOTIFY_HANDLER_VAR, s);
	return;
}

#ifdef __STDC__
void make_notify_list (int servnum)
#else
void make_notify_list (servnum)
	int servnum;
#endif
{
	NotifyList *tmp;
	char *list = NULL;

	server_list[servnum].notify_list = NULL;

	for (tmp = server_list[0].notify_list; tmp; tmp = tmp->next)
	{
		NotifyList *new = (NotifyList *)new_malloc(sizeof(NotifyList));
		new->nick = m_strdup(tmp->nick);
		new->host = m_strdup(tmp->host);
		new->user = m_strdup(tmp->user);
		new->flag = 0;
		add_to_list((List **)&server_list[servnum].notify_list, (List *)new);
		malloc_strcat(&list, new->nick);
		malloc_strcat(&list, " ");

	}

	if (get_int_var(HARD_UH_NOTIFY_VAR))
	{
		new_free(&list);
		for ( tmp = server_list[0].notify_list; tmp; tmp = tmp->next)
		{
			if (is_server_connected(servnum))
				add_to_userhost_queue(tmp->nick, got_userhost, "%s", tmp->nick);
		}
	}
	else
	{
		if (list)
		{
			if (is_server_connected(servnum))
				add_ison_to_whois(list, ison_notify);
			new_free(&list);
		}
	}

}

char *get_notify_nicks (int showserver, int showon)
{
	NotifyList *tmp;
	char *list = NULL;

	if (showserver >= number_of_servers)
		return m_strdup(empty_string);
 
	for (tmp = server_list[showserver].notify_list; tmp; tmp = tmp->next)
	{
		if (showon == -1 || showon == tmp->flag)
			m_s3cat(&list, " ", tmp->nick);
	}

	return list ? list : m_strdup(empty_string);
}
