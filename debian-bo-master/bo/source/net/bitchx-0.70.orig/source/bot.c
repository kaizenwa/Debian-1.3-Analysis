/*
 * This code is copyright Colten Edwards (c) 96
 */
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
#include "bot.h"
#include "misc.h"
#include "userlist.h"

UserList *Bot_list = NULL;
int bot_count = 0;

void bot _((char *, char *, char *));
void remove_all _((int));

void sync_botlist(UserList *new, int type)
{
ChannelList *chan;
NickList *nick;
int i;
	context;
	for (i = 0; i < number_of_servers; i++)
	{
		for (chan = server_list[i].chan_list; chan; chan = chan->next)
		{
			for (nick = chan->nicks; nick; nick = nick->next)
			{
				if (match(new->nick, nick->nick) && match(new->host, nick->host))
					if (type) 
						nick->botlist = new;
					else 
						nick->botlist = NULL;	
			}
		}
	}
}

void prepare_bothost(stuff, nick, args)
WhoisStuff *stuff;
char *nick, *args;
{
	UserList *new = NULL; 
	char *p;
	char *pass = NULL;
	char *uh;
	char *userhost = NULL;
	int remove_it = 0, aop = 0, prot = 0;

	context;
	if (*nick == '-') remove_it = 1;
        if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick))
        {
                say("No such nick [%s]", nick);
                return;
        }
	uh = stuff->user;
/*
	if (*uh == '~' || *uh == '#')
		uh++;
*/
	while (*uh == '~' || *uh == '#' || *uh == '+' || *uh == '-')
		uh++;
	p = next_arg(args, &args);
	remove_it = my_atol(p);
	p = next_arg(args, &args);
	if (args && *args)
		aop = my_atol(next_arg(args, &args));
	if (args && *args)
		prot = my_atol(next_arg(args, &args));
	if (args && *args)
		pass = next_arg(args, &args);

	malloc_sprintf(&userhost, "*%s@%s", uh, stuff->host);

	if (!remove_it) 
	{
		if ((new = lookup_userlevelc(nick, userhost, p, Bot_list)) == NULL)
		{
			new = (UserList *) new_malloc(sizeof(UserList));
			memset(new, 0, sizeof(UserList));
			malloc_strcpy(&new->nick, nick);
			malloc_strcpy(&new->host, userhost);
			malloc_strcpy(&new->channels, p);
			malloc_strcpy(&new->password, pass);
			new->aop = aop;
			new->prot = prot;
			bitchsay("Adding %s!%s to bot list", new->nick, userhost);
			bot_count++;
			add_to_list((List **)&Bot_list, (List *)new);
			sync_botlist(new, 1);
		} 
		else 
			bitchsay ("%s already in Bot list", nick);
	} 
	else 
	{
		if ((new = (UserList *) remove_from_list((List **) &Bot_list, nick)) != NULL) 
		{
			bitchsay("Removing %s from internal Bot list", nick);
			sync_botlist(new, 0);
			if (new->password) new_free(&new->password);
			if (new->host) new_free(&new->host);
			if (new->channels) new_free(&new->channels);
			if (new->nick) new_free(&new->nick);
			new_free((char **)&new);
			bot_count--;
		}
	}
	new_free(&userhost);
}


void bot (char *command, char *args, char *subargs) {
int remove_it = 0;
int showit = 0;
int aop = 0, prot = 0;
char *pass = NULL;
UserList *new = NULL;
char *nick = NULL, *channels= NULL, *host = NULL, *tmp = NULL;
char *p, *q;

	context;
	if (command && *command && (my_stricmp(command, "BOTLIST")==0))
		showit = 1;

	if (*args && showit == 0) {
		if (command && *command && (my_stricmp(command, "UNBOT") ==0))
			remove_it = 1;
		p = next_arg(args, &args);
		if (remove_it && p && *p && *p =='-' && *(p+1) && !my_stricmp(p+1, "ALL"))
		{
			bitchsay("Removing all bot's on internal list");
			remove_all(BOTLIST_REMOVE);
			return;
		}
		malloc_strcpy(&tmp, p);
		p = next_arg(args, &args);
		if (!p || !*p) 
			p = "*";
		malloc_strcpy(&channels, p);
		if (args && *args)
			aop = my_atol(next_arg(args, &args));
		if (args && *args)
			prot = my_atol(next_arg(args, &args));
		if (args && *args)
			pass = next_arg(args, &args);
			
		if (strchr(tmp, '!')) {
			if (strchr(tmp, '@')) {
				q = tmp;
				nick = q;
				host = strchr(q,'!');
				*host++ = '\0';
				if (remove_it) {
					while ((new = lookup_userlevelc(nick, host, channels, Bot_list)))
					{
						UserList *tbot = NULL;
						if ((tbot = (UserList *) remove_from_list((List **) &Bot_list, new->nick)) != NULL) 
						{
							sync_botlist(tbot, 0);
							new_free(&tbot->password);
							new_free(&tbot->host);
							new_free(&tbot->channels);
							new_free(&tbot->nick);
							new_free((char **)&tbot);
							new_free(&channels);
							new_free((char **)&tmp);
							bot_count--;
						}
					}
				} else {
					if ((new = lookup_userlevelc(nick, host, channels, Bot_list)) == NULL) {
						new = (UserList *) new_malloc(sizeof(UserList));
						memset(new, 0, sizeof(UserList));
						if (host) malloc_strcpy(&new->host, host);
						if (nick) malloc_strcpy(&new->nick, nick);
						if (channels) malloc_strcpy(&new->channels, channels);
						new_free(&tmp);
						new->aop = aop;
						new->prot = prot;
						malloc_strcpy(&new->password, pass);
						add_to_list((List **)&Bot_list, (List *)new);
						bot_count++;
						sync_botlist(new, 1);
					}
				}
			}
		}
		else
		{
			if (pass)
				add_to_userhost_queue(tmp, prepare_bothost, "%d %s %d %d %s", remove_it, channels? channels:"*", aop, prot, pass);
			else
				add_to_userhost_queue(tmp, prepare_bothost, "%d %s %d %d", remove_it, channels? channels:"*", aop, prot);
		}
				
	} 
	else if (showit) 
	{
		int first_time = 0;
		int hook = 0;
		new = Bot_list;
		if (!new) {
			bitchsay("No Botz on internal Bot list");
			return;
		}
		if (args && *args)
		{
			while ((p = next_arg(args, &args)))
			{
				if (is_channel(p))
					channels = p;
				else
					host = p;
			}
		}

		for (new = Bot_list; new; new = new->next) 
		{
			if (channels)
				if (!check_channel_match(channels, new->channels))
					continue;
			if (host)
				if (!wild_match(host, new->host))
					continue;
			if (!first_time)
				if ((hook = do_hook(BOTLIST_HEADER_LIST,"%s %s %s %s %s", "Aop","Prot","Bot","Channel","Hostname")))
					put_it("%s", convert_output_format(get_string_var(FORMAT_BOT_HEADER_VAR), NULL));
			if ((hook = do_hook(BOTLIST_LIST, "%d %d %s %s %s", new->aop, new->prot, new->nick, new->channels, new->host?new->host:"*")))
				put_it("%s", convert_output_format(get_string_var(FORMAT_BOT_VAR), "%d %d %s %s %s %s", new->aop, new->prot, new->nick, new->channels, new->host?new->host:"*", new->password?new->password:""));
			first_time++;
		}
		if (hook)
			put_it("%s", convert_output_format(get_string_var(FORMAT_BOT_FOOTER_VAR), "%s %d", update_clock(GET_TIME), first_time));
	} else
		userage(command ? command : "AddBot" , command ? "<nick\002|\002nick!user@host> <channel> [aop] [prot]":"<nick\002|\002nick!user@host> <channel>");
}

void save_bot(FILE *fp)
{
UserList *tmp = Bot_list;
int count = 0;
	context;
	if (!tmp)
		return;
	fprintf(fp, "# %s Bot list\n", version);
	while(tmp) {
		fprintf(fp, "BOT %s!%s %s %d %d %s\n", tmp->nick, tmp->host, tmp->channels, tmp->aop, tmp->prot, tmp->password?tmp->password:"");
		tmp = tmp->next;
		count++;
	}
	if (do_hook(SAVEFILE_LIST, "BotList %d", count))
		bitchsay("Saved %d BotList entries", count);
}
