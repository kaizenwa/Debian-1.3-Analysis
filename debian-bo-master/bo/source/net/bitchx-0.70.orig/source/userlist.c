/*
 *   CDE created userlist functions.
 *   Copyright Colten Edwards 04/10/96
 *
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
#include "struct.h"
#include "timer.h"
#include "bot.h"
#include "whowas.h"
#include "misc.h"
#include "userlist.h"
#include "parse.h"
#include <sys/types.h>
#include <sys/stat.h>


/*
 *  Shit levels
 *  1 no opz
 *  2 Auto Kick
 *  3 Auto Ban Kick
 *  4 perm ban. all times.
 *  5 perm ignore all
 *
 *  User levels
 *  25 ctcp invite and whoami
 *  50 ops, chops and unban
 *  90 no Kick/Dop flood checking
 *
 *  Auto-op levels
 *  0 auto-op off
 *  1 timed autoop
 *  2 instant op
 *  3 delay +v
 *  4 instant +v
 *
 *  Protection levels
 *  1 Reop if de-oped
 *  2 De-op offender
 *  3 Kick  offender
 *  4 KickBan offender
 */

int user_count = 0;
int shit_count = 0;
extern int bot_count;
extern char *FromUserHost;
extern char *awaymsg;
#define PERM_IGNORE 5
/* CDE this shouldn't be here at all */
#define IGNORE_REMOVE 1


UserList *user_list = NULL;
ShitList *shitlist_list = NULL;
LameList *lame_list = NULL;
WordKickList *ban_words = NULL;
extern AJoinList *ajoin_list;

extern void save_idle _((FILE *output));
extern void save_banwords _((FILE *output));
extern int  save_formats _((FILE *output));

char *kicker = NULL;

void sync_nicklist _((UserList *, int));
void sync_shitlist _((ShitList *, int));
void sync_botlist _((UserList *, int));

void prepare_userhost(stuff, nick, args)
WhoisStuff *stuff;
char *nick, *args;
{
	char listbuf[BIG_BUFFER_SIZE+1];
	char *uh;
	int thetype = 0;
	int level = 1, autoop = 0, prot = 0, shit = 0;
	char *channels = NULL, *passwd = NULL, *p = NULL;
	
	context;
        if (!stuff || !stuff->nick || !nick || !strcmp(stuff->user, "<UNKNOWN>") || my_stricmp(stuff->nick, nick))
        {
                say("No such for user %s", nick);
                return;
        }

	thetype = my_atol(args);

	uh = stuff->user;
	while (*uh == '~' || *uh == '#' || *uh == '+' || *uh == '-')
		uh++;
/*		
	if (*uh == '~' )
		uh++;
	if (*uh == '#')
		uh++;
*/

	if (!(thetype % 2))
		sprintf(listbuf, "*%s@%s", uh, stuff->host);
	else 
		sprintf(listbuf, "*%s@%s", uh, cluster(stuff->host));
	p = next_arg(args, &args);
	p = next_arg(args, &args);
	if (p && *p)
		level = my_atol(p);
	p = next_arg(args, &args);
	if (p && *p)
		autoop = my_atol(p);
	p = next_arg(args, &args);
	if (p && *p)
		prot = my_atol(p);
	p = next_arg(args, &args);
	if (p && *p)
		shit = my_atol(p);
	if (strchr(args, ' ')) 
	{
		p = next_arg(args, &args);
		malloc_strcpy(&channels, p);
		malloc_strcpy(&passwd, args);
	} else 
		malloc_strcpy(&channels, args);

	add_to_a_list(listbuf, thetype, stuff->nick, level, autoop, prot, channels, passwd, shit);
	if (thetype == USERLIST_ADD)
	{
		send_to_server("NOTICE %s :You have been added to my Userlist with a %d level", nick, level);
		send_to_server("NOTICE %s :you will %s auto-op'd", nick, autoop ? "be": "not be");
		send_to_server("NOTICE %s :you are %sprotected %s%s", nick, prot ? "": "not ", passwd?"Your password is ":"", passwd?passwd:"");
	}
}

void remove_all(int type)
{
UserList *tmp, *next;
ShitList *tmp_s, *next_s;
ChannelList *chan;
NickList *nick;
int i = 0;
	context;
	if (type == -1 || type == USERLIST_REMOVE)
	{
		for (tmp = user_list; tmp; tmp = next)
		{
			next = tmp->next;
			sync_nicklist(tmp, 0);
			new_free(&tmp->nick);
			new_free(&tmp->host);
			new_free(&tmp->channels);
			new_free(&tmp->password);
			new_free((char **)&tmp);
		}
		user_count  = 0;
		user_list = NULL;
		for (i = 0; i < number_of_servers; i ++)
		{
			for (chan = server_list[i].chan_list; chan; chan = chan->next)
			{
				for (nick = chan->nicks; nick; nick = nick->next)
						nick->userlist = NULL;	
			}
		}
	}
	if (type == -1 || type == BOTLIST_REMOVE)
	{
		for (tmp = Bot_list; tmp; tmp = next)
		{
			next = tmp->next;
			sync_botlist(tmp, 0);
			new_free(&tmp->nick);
			new_free(&tmp->host);
			new_free(&tmp->channels);
			new_free(&tmp->password);
			new_free((char **)&tmp);
		}
		for (i = 0; i < number_of_servers; i ++)
		{
			for (chan = server_list[i].chan_list; chan; chan = chan->next)
			{
				for (nick = chan->nicks; nick; nick = nick->next)
						nick->botlist = NULL;	
			}
		}
		Bot_list = NULL;
		bot_count = 0;
	}
	if (type == -1 || type == SHITLIST_REMOVE)
	{
		for (tmp_s = shitlist_list; tmp_s; tmp_s = next_s)
		{
			next_s = tmp_s->next;
			sync_shitlist(tmp_s, 0);
			new_free(&tmp_s->filter);
			new_free(&tmp_s->channels);
			new_free(&tmp_s->reason);
			new_free((char **)&tmp_s);
		}
		for (i = 0; i < number_of_servers; i ++)
		{
			for (chan = server_list[i].chan_list; chan; chan = chan->next)
			{
				for (nick = chan->nicks; nick; nick = nick->next)
						nick->shitlist = NULL;	
			}
		}
		shit_count = 0;
		shitlist_list = NULL;
		return;
	}
}

void add_user(char *command, char *args, char *subargs) 
{
	char *nick, *ptr;
	int level = 1, autoop = 0, prot = 0;
	char *passwd = NULL;
	char *channels = NULL;
	char *p = NULL; char * bang; char *temp = NULL;
	int type = USERLIST_ADD;

	context;
	if (command && *command && (my_stricmp(command,"UNUSER") == 0))
		type = USERLIST_REMOVE; 

	if (args && *args)
	{

		nick = next_arg(args, &args);
		if (!nick || !*nick) 
		{
			return;
		}
		if (type == USERLIST_REMOVE && *nick == '-' && *(nick+1) && !my_stricmp(nick+1, "ALL"))
		{
			bitchsay("Removing all Users on Userlist");
			remove_all(USERLIST_REMOVE);
			return;
		}
		ptr = next_arg(args, &args);
		if (!ptr || !*ptr) {
			bitchsay("Need a channel for %s", command ? "UnUser" : "AddUser");
			return;
		}
		malloc_strcpy(&channels, ptr);

		ptr = next_arg(args, &args);
		if (ptr && *ptr)
			level = my_atol(ptr);
	
		ptr = next_arg(args, &args);
		if (ptr && *ptr)
			autoop = my_atol(ptr);

		ptr = next_arg(args, &args);
		if (ptr && *ptr)
			prot = my_atol(ptr);

		ptr = next_arg(args, &args);
		if (ptr && *ptr)
			malloc_strcpy(&passwd, ptr);

		if ((bang = strchr(nick, '!')) && strchr(nick, '@'))
		{
			malloc_strcpy(&temp, nick);
			p = temp;
			bang = strchr(temp, '!');
			*bang++ = '\0';
			add_to_a_list(bang, type, p, level, autoop, prot, channels, passwd, 0);
			new_free(&p);
		}
		else
			if (passwd)
				add_to_userhost_queue(nick, prepare_userhost, "%d %d %d %d 0 %s %s", type, level, autoop, prot, channels, passwd);
			else
				add_to_userhost_queue(nick, prepare_userhost, "%d %d %d %d 0 %s", type, level, autoop, prot, channels);
	} else
		if (command)
			userage("UnUser", "<nick\002|\002nick!user@host> <channel\002|\002*>");
		else
			userage("AddUser", "<nick\002|\002nick!user@host> <channel\002|\002*> [level] [auto-op] [protectionlevel] [password]");
}


void add_shit(char *command, char *args, char *subargs) 
{
	char *nick, *ptr;
	int level = 1;
	char *reason = NULL;
	char *channels = NULL;
	char *p = NULL; char * bang; char *temp = NULL;

	int type = SHITLIST_ADD;
	

	context;
	if (command && *command && !my_stricmp(command, "UNSHIT"))
		type = SHITLIST_REMOVE;
	if (args && *args)
	{

		nick = next_arg(args, &args);
		if (!nick || !*nick) {
			return;
		}
		if (type == SHITLIST_REMOVE && *nick == '-' && *(nick+1) && !my_stricmp(nick+1, "ALL"))
		{
			bitchsay("Removing all Users on shitlist");
			remove_all(SHITLIST_REMOVE);
			return;
		}
		ptr = next_arg(args, &args);
		if (!ptr || !*ptr) {
			bitchsay("Need a channel for %s",command ? "UnShit": "AddShit");
			return;
		}
		malloc_strcpy(&channels, ptr);

		ptr = next_arg(args, &args);
		if (ptr && *ptr)
			level = my_atol(ptr);

		if (args && *args)
			malloc_strcpy(&reason, args);
		else
			malloc_strcpy(&reason, "ShitListz");


		if ((bang = strchr(nick, '!')) && strchr(nick, '@'))
		{
			malloc_strcpy(&temp, nick);
			p = temp;
			bang = strchr(temp, '!');
			*bang++ = '\0';
			add_to_a_list(bang, type, p, 0, 0, 0, channels, reason, level);
			new_free(&p);
		}
		else
			add_to_userhost_queue(nick, prepare_userhost, "%d %d %d %d %d %s %s", type, 0, 0, 0, level, channels, reason);
		new_free(&reason);
	} else
		if (command)
			userage("UnShit", "<nick\002|\002nick!user@host> <channel\002|\002*>");
		else
			userage("AddShit", "<nick\002|\002nick!user@host> <channel\002|\002*> <level> <reason>");
}

void sync_nicklist(UserList *removed, int type)
{
ChannelList *chan;
NickList *nick;
int i;

	context;
	for (i = 0; i < number_of_servers; i ++)
	{
		for (chan = server_list[i].chan_list; chan; chan = chan->next)
		{
			for (nick = chan->nicks; nick; nick = nick->next)
			{
				if (match(removed->nick, nick->nick) && match(removed->host, nick->host))
					if (type) 
						nick->userlist = removed;
					else 
						nick->userlist = NULL;	
			}
		}
	}
}

void sync_shitlist(ShitList *removed, int type)
{
ChannelList *chan;
NickList *nick;
int i;
char tmp[BIG_BUFFER_SIZE+1];

	context;
	for (i = 0; i < number_of_servers; i ++)
	{
		for (chan = server_list[i].chan_list; chan; chan = chan->next)
		{
			for (nick = chan->nicks; nick; nick = nick->next)
			{
				sprintf(tmp, "%s!%s", nick->nick, nick->host);
				if (match(removed->filter, tmp))
					if (type) 
						nick->shitlist = removed;
					else
						nick->shitlist = NULL;
			}
		}
	}
}

void add_to_a_list(thestring, thetype, nick, level, autoop, prot, channels, passwd, shitlevel)
char *thestring;
int thetype;
char *nick;
int level;
int autoop;
int prot;
char *channels;
char *passwd;
int shitlevel;
{
	void **TheList = NULL;
	UserList *uremove = NULL;
	ShitList *sremove = NULL;
	char which[16];
	char *theuh = NULL;
	int scount = 0;

	context;
		
	switch (thetype)
	{
		case SHITLIST_ADD:
		case SHITLIST_REMOVE:
			TheList = (void *)&shitlist_list;
			strcpy(which, "Shit");
			break;
		case USERLIST_ADD:
		case USERLIST_REMOVE:
		case USERLIST_MOD:
			TheList = (void *)&user_list;
			strcpy(which, "User");
			break;
		default:
			bitchsay("Error!! Something FUCKED UP somewhere, thetype was unknown in add_to_a_list(%d)", thetype);
			return;
	}
	switch(thetype)
	{
		case USERLIST_ADD:
		case USERLIST_MOD:
/*
			if (!(uremove = lookup_userlevelc("*", thestring, channels, *TheList)))
*/
			{
				user_count++;
				uremove = (UserList *) new_malloc(sizeof(UserList));
				uremove->level = level;
				uremove->aop = autoop;
				uremove->prot = prot;
				malloc_strcpy(&uremove->channels, channels);
				malloc_strcpy(&uremove->password, passwd);
				malloc_strcpy(&uremove->nick, nick);
				malloc_strcpy(&uremove->host, thestring);
				add_to_list((List **)TheList, (List *)uremove);
				sync_whowas_adduser(uremove);
				sync_nicklist(uremove, 1); 
			}
			break;
		case USERLIST_REMOVE:
			if ((uremove = lookup_userlevelc(nick, thestring, channels, *TheList)))
			{
				uremove = (UserList *)removewild_from_list((List **)TheList, uremove->nick);
				if (uremove)
				{
					user_count--;
					sync_whowas_unuser(uremove);
					sync_nicklist(uremove, 0);
					new_free(&uremove->host);
					new_free(&uremove->nick);
					new_free(&uremove->password);
					new_free(&uremove->channels);
				}
			}
			break;
		case SHITLIST_ADD:
			if (!(sremove = nickinshit(nick, thestring)))
			{
				shit_count++;
				sremove = (ShitList *) new_malloc(sizeof(ShitList));
				sremove->level = shitlevel;
				malloc_strcpy(&sremove->reason, passwd);
				malloc_strcpy(&sremove->channels, channels);
				malloc_sprintf(&sremove->filter, "*!%s", thestring);
				add_to_list((List **)TheList, (List *)sremove);
				sync_whowas_addshit(sremove);
				sync_shitlist(sremove, 1);
				if (shitlevel == PERM_IGNORE)
					ignore_nickname(sremove->filter, IGNORE_ALL, 0);
			}
			break;
		case SHITLIST_REMOVE:
			malloc_sprintf(&theuh, "%s!%s", nick, thestring);
			while ((sremove = (ShitList *)removewild_from_list((List **)TheList, theuh)))
			{
				shit_count--;
				scount++;
				if (sremove->level == PERM_IGNORE)
					ignore_nickname(sremove->filter, IGNORE_ALL, IGNORE_REMOVE); 
				sync_whowas_unshit(sremove);
				sync_shitlist(sremove, 0);
				new_free(&sremove->filter);
				new_free(&sremove->reason);
				new_free(&sremove->channels);
				new_free((char **)&sremove);
			}
			new_free(&theuh);
			break;
		default:
			break;
	}	

	if (uremove || sremove || scount)
	{
		if ((thetype == USERLIST_ADD) || (thetype == SHITLIST_ADD))
			bitchsay("Adding %s!%s to %s list", nick, thestring, which);
		else if (thetype == USERLIST_MOD)
			bitchsay("Modified %s!%s on %s list", nick, thestring, which);
		else if (thetype == SHITLIST_REMOVE || (thetype == USERLIST_REMOVE))
		{
			bitchsay("Deleting %s!%s from %s list", nick, thestring, which);
			if (thetype == SHITLIST_REMOVE)
				new_free((char **)&sremove);
			else
				new_free((char **)&uremove);
		}
	} 
	else
	{
		if ((thetype == USERLIST_ADD) || (thetype == SHITLIST_ADD))
			bitchsay ("%s!%s already on my %s list", nick,thestring, which);
		else
			bitchsay("Didnt find %s!%s on %s list", nick, thestring, which);
	}
}

void showuserlist(char *command, char *args, char *subargs)
{
	UserList *tmp = user_list;
	int first = 0;
	int hook = 0;
	char *p, *channel = NULL, *hostname = NULL;

	context;
	if (!tmp)
	{
		bitchsay("No Users on the Internal User list");
		return;
	}
	if (args && *args)
	{
		while ((p = next_arg(args, &args)))
		{
			if (is_channel(p))
				channel = p;
			else
				hostname = p;
		}
	}
	for (tmp = user_list; tmp; tmp = tmp->next)
	{
		if (channel)
			if (!match(tmp->channels, channel))
				continue;
		if (hostname)
			if (!match(hostname, tmp->host))
				continue;
				
		if (!first++ && (hook = do_hook(USERLIST_HEADER_LIST, "%s %s %s %s %s %s %s", "Level","Aop","Prot","Nick","Password","Host","Channels")))
			put_it("%s", convert_output_format(get_string_var(FORMAT_USERLIST_HEADER_VAR), NULL)); 
		if (do_hook(USERLIST_LIST, "%d %d %d %s %s %s %s", tmp->level, tmp->aop, tmp->prot,tmp->nick,tmp->host,tmp->channels,tmp->password?tmp->password:""))
			put_it("%s", convert_output_format(get_string_var(FORMAT_USERLIST_VAR), 
				"%d %d %d %s %s %s %s", tmp->level, tmp->aop, tmp->prot, tmp->nick, tmp->password?tmp->password:"<none>", tmp->host, tmp->channels));
	}
	if (first && hook)
		put_it("%s", convert_output_format(get_string_var(FORMAT_USERLIST_FOOTER_VAR), "%s %d", update_clock(GET_TIME), first)); 
}

void showshitlist(char *command, char *args, char *subargs)
{
	ShitList *tmp = shitlist_list;
	int first = 0;
	int hook = 0;

	context;
	if (!tmp)
	{
		bitchsay("No entries in Shit list");
		return;
	}
	while (tmp)
	{
		if (!first++ && (hook = do_hook(SHITLIST_HEADER_LIST,"%s %s %s %s %s %s", "Lvl","Nick","Channels","Reason", "", "")))
			put_it("%s", convert_output_format(get_string_var(FORMAT_SHITLIST_HEADER_VAR), NULL)); 
		if (do_hook(SHITLIST_LIST, "%d %s %s %s %s %s", tmp->level, tmp->filter,tmp->channels, tmp->reason?tmp->reason:"<none>", "", ""))
		{
			put_it("%s", convert_output_format(get_string_var(FORMAT_SHITLIST_VAR), 
				"%d %s %s %s", tmp->level, tmp->filter, tmp->channels, tmp->reason?tmp->reason:"<No Reason"));
		}
		tmp = tmp->next;
	}
	if (first && hook)
		put_it("%s", convert_output_format(get_string_var(FORMAT_SHITLIST_FOOTER_VAR), "%s %d", update_clock(GET_TIME), shit_count)); 
}

void set_user_info(char *command, char *args, char *subargs)
{

	context;

}

int check_channel_match(char *tmp, char *channel)
{
	char *p, *q, *chan = NULL;
	int wmatch = 0;

	context;
	if (!tmp || !channel)
		return 0;
	if (*channel == '*' && (strlen(channel)==1))
		return 1;
	if ((strchr(tmp, ',')))
	{
		malloc_strcpy(&chan, tmp);
		p = q = chan;
		while(chan && (p = strchr(chan, ',')))
		{
			*p = 0;
			if ((wmatch = match(chan, channel)))
				break;
			chan = ++p;
		}
		if (!wmatch)
			wmatch = match(chan, channel);
		new_free(&q);
	} 
	else
		wmatch = match(tmp, channel);
	return wmatch;
}

/*
 * Function courtesy of Sheik. From his CtoolZ client.
 * but modified a little by panasync
 */
UserList *lookup_userlevelc(char *nick, char *userhost, char *channel, UserList * search_list)
{
register UserList *tmp = NULL;
UserList *best = NULL;
int	best_user_match = 0, 
	best_chan_match = 0, 
	best_nick_match = 0,
	chan_match, 
	nick_match,
	user_match;

	context;
	if (!nick || !userhost)
		return NULL;
	for (tmp = search_list; tmp; tmp = tmp->next) 
	{
		if ((chan_match = check_channel_match(tmp->channels, channel)) > 0) 
		{
			user_match = wild_match(tmp->host, userhost);
			if (!(nick_match = wild_match(tmp->nick, nick)))
				nick_match = wild_match(nick, tmp->nick);
			if (user_match > best_user_match && nick_match > best_nick_match)
			{
				best_chan_match = chan_match;
				best_user_match = user_match;
				best_nick_match = nick_match;
				best = tmp;
			}
			else if (best_user_match && user_match == best_user_match && best_nick_match && nick_match == best_nick_match)
			{
				if (chan_match > best_chan_match) 
				{
					best_chan_match = chan_match;
					best_user_match = user_match;
					best_nick_match = nick_match;
					best = tmp;
				}
			}
		}
	}
	return best;
}

#if 0
UserList *nickinuser(char *niq, char *uh)
{
	char theuh[BIG_BUFFER_SIZE+1];	
register UserList *thisptr = user_list;
	char *u;
	
	if (!uh || !niq)
		return NULL;
	u = uh;
	while (*u == '~' || *u == '+' || *u == '-' || *u == '#')
		u++;
	sprintf(theuh, "*%s", u);
	while (thisptr)
	{
		if (!thisptr->host)
			continue;
		if (!strcmp(u, thisptr->host) || match(u, thisptr->host) || match(thisptr->host, u))
			return(thisptr);
		thisptr = thisptr->next;
	}
	return NULL;
}

UserList *nickinbot(char *niq, char *uh)
{
	char theuh[BIG_BUFFER_SIZE+1];
register UserList *thisptr = Bot_list;
	char *u;
	
	if (!uh || !niq)
		return NULL;
	u = uh;
	while (*u == '~' || *u == '+' || *u == '-' || *u == '#')
		u++;
	
	sprintf(theuh, "*%s", uh);
	while (thisptr)
	{
		if (!thisptr->host)
			continue;
		if ((!strcmp(u, thisptr->host) || match(thisptr->host, u)))
			return(thisptr);
		thisptr = thisptr->next;
	}
	return NULL;
}
#endif

ShitList *nickinshit(char *niq, char *uh)
{
	char theuh[BIG_BUFFER_SIZE+1];
register ShitList *thisptr = shitlist_list;
	char *u = uh;
	

	context;
	if (!uh || !niq)
		return NULL;
	while (*u == '~' || *u == '+' || *u == '-' || *u == '#')
		u++;
	sprintf(theuh, "%s!%s", niq, u);
	while (thisptr)
	{
		if (!strcmp(thisptr->filter, theuh) || (/*match(theuh, thisptr->filter) || */match(thisptr->filter, theuh))) 
			return(thisptr);
		thisptr = thisptr->next;
	}
	return NULL;
}

int find_user_level(char *from, char *host, char *channel)
{
register UserList * tmp = user_list;

	context;
	if (!tmp)
		return 0;
	if ((tmp = (UserList *) lookup_userlevelc("*", host, channel, user_list)))
		return tmp->level;
	return 0;
}

int find_shit_level(char *from, char *host, char *channel)
{
register ShitList * tmp = shitlist_list;

	context;
	if (!shitlist_list)
		return 0;
	tmp = (ShitList *) nickinshit(from, host);
	if (!tmp)
		return 0;
	if (check_channel_match(tmp->channels, channel))
		return tmp->level;
	return 0;
}

void reload_save(char *command, char *args, char *subargs)
{
char *p = NULL;
char *buffer = NULL;

	context;
	if (get_string_var(CTOOLZ_DIR_VAR))
	{
		malloc_sprintf(&buffer, "%s", get_string_var(CTOOLZ_DIR_VAR));
		p = expand_twiddle(buffer);
		if (access(p, F_OK) != 0)
		{
			bitchsay("Created directory %s", p);
			mkdir(p, S_IWUSR|S_IRUSR|S_IXUSR);
		}
		new_free(&p);
		malloc_sprintf(&buffer, "%s/%s.sav", get_string_var(CTOOLZ_DIR_VAR), version);
	}
	else
		malloc_sprintf(&buffer, "~/%s.sav", version);
	remove_all(-1);
	p = expand_twiddle(buffer);
	load(empty_string, p, empty_string); /*CDE XXX p */
	new_free(&buffer);
	new_free(&p);
}

void savelists(char *command, char *args, char *subargs)
{
	char thefile[BIG_BUFFER_SIZE+1];
	char *p = NULL;
	FILE *outfile;
	extern int defban;
	ShitList *slist = shitlist_list;
	UserList *ulist = user_list;
	LameList *lame_nick = lame_list;
	AJoinList *ajoin = ajoin_list;
	int count = 0;

	context;
	sprintf(thefile, "%s/%s.sav", get_string_var(CTOOLZ_DIR_VAR), version);
	p = expand_twiddle(thefile);
	outfile = fopen(p, "w");
	if (!outfile)
	{
		bitchsay("Cannot open file %s for saving!", thefile);
		new_free(&p);
		return;
	}
	if (do_hook(SAVEFILEPRE_LIST, "%s %s", thefile, p)) 
		bitchsay("Saving All Your Settings to %s", thefile);

	if (ulist)
		fprintf(outfile, "# %s UserList\n", version);
	count = 0;
        while(ulist)
        {
                fprintf(outfile, "ADDUSER %s!%s %s %d %d %d %s\n", ulist->nick, ulist->host, ulist->channels, ulist->level, ulist->aop, ulist->prot, ulist->password? ulist->password : "");
                ulist = ulist->next;
		count ++;
        }
	if (do_hook(SAVEFILE_LIST, "UserList %d", count)) 
	        bitchsay("Saved %d UserList entries", count);

	count = 0;
	if (slist)
		fprintf(outfile, "# %s ShitList\n", version);
        while(slist)
        {
                fprintf(outfile, "ADDSHIT %s %s %d %s\n", slist->filter, slist->channels, slist->level, slist->reason? slist->reason : "");
                slist = slist->next;
		count ++;
        }
	if (do_hook(SAVEFILE_LIST, "ShitList %d", count)) 
	        bitchsay("Saved %d ShitList entries", count);

	count = 0;
	if (lame_list)
		fprintf(outfile, "# %s LameNick List\n", version);
        while(lame_nick)
        {
                fprintf(outfile, "ADDLAMENICK %s\n", lame_nick->nick);
                lame_nick = lame_nick->next;
		count ++;
        }
	if (do_hook(SAVEFILE_LIST, "LameNick List %d", count)) 
	        bitchsay("Saved %d LameNick entries", count);

	if (ajoin)
		fprintf(outfile, "# %s Auto-Join List\n", version);
	count = 0;
        while (ajoin)
        {
         	if (ajoin->ajoin_list == 1) /* actual auto-join entry */
         	{
	                fprintf(outfile, "AJOIN %s%s%s\n", ajoin->name, ajoin->key?" ":"", ajoin->key?ajoin->key:"");
			count ++;
		}
       	        ajoin = ajoin->next;
        }
	if (do_hook(SAVEFILE_LIST, "AutoJoin %d", count)) 
	        bitchsay("Saved %d AutoJoin entries", count);

	save_bot(outfile);
	save_notify(outfile);
	save_idle(outfile);
	save_banwords(outfile);
	savebitchx_variables(outfile);
	
	if (awaymsg)
		fprintf(outfile, "ENV AWAYMSG %s\n", awaymsg);
	fprintf(outfile, "BANTYPE %c\n", defban == 1? 'N':defban == 2? 'B':defban==3?'H':defban==4?'D':defban==5?'U':'S');
	if (do_hook(SAVEFILEPOST_LIST, "%s %s", thefile, p))
		bitchsay("Done Saving to file %s", thefile);
	
	fclose(outfile);
	new_free(&p);
	save_formats(outfile);
}

int real_check_auto _((void *arg))
{
	char *nick, *host, *channel;
	char *p = (char *)arg;
	char *args = (char *)arg;
	char *serv_num = NULL;
	int  this_server = from_server;	

	context;
	
	channel = next_arg(args, &args);
	nick = next_arg(args, &args);
	host = next_arg(args, &args);
	if ((serv_num = next_arg(args, &args)))
		from_server = my_atol(serv_num);
	
	if (channel && *channel && nick && *nick && host && *host)
		check_auto(channel, nick, host, NULL);

	this_server = from_server;
	new_free(&p);
	return 0;
}

int delay_check_auto _((char *channel))
{
	ChannelList *chan = NULL;
	char *buffer = NULL;
	NickList *possible;	

	context;
	if (!channel || !*channel)
		return -1;
		
	if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)) == NULL)	
		return -1;

	for (possible = chan->nicks; possible; possible = possible->next)
	{
		if ((possible->shitlist || possible->userlist || possible->botlist) && (!(possible->sent_reop < 4) || !(possible->sent_deop < 4)))
		{
			malloc_sprintf(&buffer, "%s %s %s %d", channel, possible->nick, possible->host, from_server);
			add_timer("", 3, real_check_auto, buffer, NULL);
/* this looks like a memory leak but it isn't.
 * add_timer takes this buffer and saves the pointer to it.
 * the function is responsible for cleaning up after it.
 */
			buffer = NULL;
		}
	}
	add_timer("", 5, delay_flush_all, m_sprintf("%s %d", channel, from_server), NULL);
	return 0;
}

int delay_opz _((void *arg))
{
char * args = (char *)arg;
char * from = NULL;
char * channel = NULL;	
char * mode = NULL;
char * serv_num = NULL;
int	this_server = from_server;
char *p = (char *) arg; /* original args unmodified  so we can free them */
	context;

	channel = next_arg(args, &args);
	from = next_arg(args, &args);
	mode = next_arg(args, &args);

	if ((serv_num = next_arg(args, &args)))
		this_server = my_atol(serv_num);
	if (is_on_channel(channel, from_server, from) && is_chanop(channel, nickname)) 
	{
		if (!is_chanop(channel, from))
		{
			my_send_to_server(this_server, "MODE %s +%s %s", channel, mode, from); 
			my_send_to_server(this_server, "NOTICE %s :You have been delay Auto-%s'd", from, *mode == 'o'? "op":"voice");
		}
	}
	new_free(&p);
	return 0;
}


static char *protected = NULL;

int delay_kick _((void *arg))
{
char * args = (char *)arg;
char * from = NULL;
char * channel = NULL;	
char * serv_num = NULL;
int this_server = from_server;
int server;
ChannelList *chan;
char *p = (char *) arg; /* original args unmodified  so we can free them */


	context;
	if (protected)
	{
		from = next_arg(args, &args);
		channel = next_arg(args, &args);
		if ((serv_num = next_arg(args, &args)))
			this_server = my_atol(serv_num);
		if ((chan = prepare_command(&server, channel, 3)))
			my_send_to_server(this_server, "KICK %s %s :\002%s\002 Kick/ban me will ya", channel, from, _VERSION_);
		new_free(&protected);
	}
	new_free(&p);
	return 0;
}

NickList *check_auto(char *channel, char *nick, char *host, ChannelList *chan)
{

ShitList *shitptr = NULL;
UserList *userptr = NULL, *botptr = NULL;
NickList *nicklist = NULL;
ChannelList *chan_ptr =  NULL;
char *ban;

	context;
	if (!channel || !*channel || !nick || !host)
		return NULL;
	
	if (!chan)
		chan_ptr = (ChannelList *) find_in_list((List **)&(server_list[from_server].chan_list), channel, 0);
	else
		chan_ptr = chan;

	if (!chan_ptr)
		return NULL;		
	if (!(nicklist = (NickList *)find_in_list((List **)&chan_ptr->nicks, nick, 0)))
		return NULL;	

	userptr = nicklist->userlist;
	shitptr = nicklist->shitlist;
	botptr	= nicklist->botlist;

	if (userptr && !check_channel_match(userptr->channels, channel))
		userptr = NULL;
	if (botptr && !check_channel_match(botptr->channels, channel))
		botptr = NULL;
	if (shitptr && !check_channel_match(shitptr->channels, channel))
		shitptr = NULL;
		
	if (chan_ptr->chop && chan_ptr->set_shitlist && (shitptr != NULL) && (userptr == NULL && botptr == NULL))
	{
		char *theshit;
		theshit = get_string_var(SHITLIST_REASON_VAR);
		switch(shitptr->level)
		{
		
			case 0:
				return nicklist;
				break;
			case 1:/* never give opz */
				if (nicklist->sent_deop < 4)
				{
/*					send_to_server("MODE %s -o %s", channel, nick);*/
					add_mode(chan_ptr, "o", 0, nick, NULL, get_int_var(NUM_OPMODES_VAR));
					nicklist->sent_deop++;
					nicklist->sent_deop_time = time(NULL);
				}
				break;				
			case 2: /* Auto Kick  offender */

				add_mode(chan_ptr, NULL, 0, nick, shitptr->reason?shitptr->reason:theshit, 0);
/*				send_to_server("KICK %s %s :%s", channel,
					nick, (shitptr->reason && *shitptr->reason) ? shitptr->reason : theshit);*/
				break;
			case 3: /* kick ban the offender */
			case 4:	/* perm ban on offender */
				if (nicklist->sent_deop  < 4 || (nicklist->sent_deop < 4 && shitptr->level == 4))
				{
/*
					add_mode(chan_ptr, "o", 0, nick, NULL, get_int_var(NUM_OPMODES_VAR));
			                add_mode(chan_ptr, "b", 1, shitptr->filter, NULL, get_int_var(NUM_BANMODES_VAR)); 
*/
					send_to_server("MODE %s -o+b %s %s", channel, nick, shitptr->filter);
					nicklist->sent_deop++;
					nicklist->sent_deop_time = time(NULL);
				}
/*				add_mode(chan_ptr, NULL, 0, nick, (shitptr->reason && *shitptr->reason) ? shitptr->reason : theshit,0);*/
				send_to_server("KICK %s %s :%s", channel,
					nick, (shitptr->reason && *shitptr->reason) ? shitptr->reason : theshit);
			default:
				break;
		}
		return nicklist;
	} 
	if (protected && chan_ptr->set_kick_if_banned)
	{
		if (!my_stricmp(nick, protected))
		{
#if 0
			if (chan_ptr->chop)
			{
				send_to_server("KICK %s %s \002BitchX\002 Kick/ban me will ya", chan_ptr->channel, protected);
				new_free(&protected);
			}
			else
				add_timer("", 15, delay_kick, m_sprintf("%s %s %d", protected, channel, from_server), NULL);
#endif
		}
	}
	if (chan_ptr->chop && chan_ptr->set_aop && chan_ptr->set_userlist && (userptr || botptr)) 
	{	
		char *buffer = NULL;
		UserList *user = userptr;
		if (!user && botptr)
			user = botptr;
		switch (user->aop)
		{
			case 1:
				if (nicklist->sent_reop)
					break;
				if (!nicklist->chanop)
				{
					nicklist->sent_reop++;
					nicklist->sent_reop_time = time(NULL);
					malloc_sprintf(&buffer, "%s %s %s %d", channel, nick, "o", from_server);
						add_timer("", 10, delay_opz, buffer, NULL);
				}
				break;
			case 2:
				if (nicklist->sent_reop)
					break;
				if (!nicklist->chanop)
				{
					nicklist->sent_reop++;
					nicklist->sent_reop_time = time(NULL);
					add_mode(chan_ptr, "o", 1, nick, NULL, get_int_var(NUM_OPMODES_VAR));
					flush_mode_all(chan_ptr);
/*					malloc_sprintf(&buffer, " MODE %s +o %s", channel, nick);
					send_to_server(buffer+1);*/
				}
				new_free(&buffer);
				break;
			case 3:
				if (!nicklist->voice)
				{
					malloc_sprintf(&buffer, "%s %s %s %d", channel, nick, "v", from_server);
						add_timer("", 10, delay_opz, buffer, NULL);
				}
				break;
			case 4:
				if (!nicklist->voice)
				{
					add_mode(chan_ptr, "v", 1, nick, NULL, get_int_var(NUM_OPMODES_VAR));
					flush_mode_all(chan_ptr);
/*					malloc_sprintf(&buffer, " MODE %s +o %s", channel, nick);
					send_to_server(buffer+1);*/
				}
				new_free(&buffer);
			case 0:
			default:
				break;
		}

	}
	ban = m_sprintf("%s!%s", nick, host);
	if ((ban_is_on_channel(ban, chan_ptr)))
	{
		my_send_to_server(from_server, "KICK %s %s :Banned", chan_ptr->channel, nick);
	}
	new_free(&ban);
	return nicklist;
}

void check_hack(char *nick, ChannelList *channel, NickList *ThisNick, char *from)
{
UserList *userptr = NULL, *botptr = NULL;
ShitList *shitptr = NULL;
WhowasList *check_op = NULL;

int	flag;


	context;
	if (from && *from && !strchr(from, '.'))
		return;

	if (!channel || !are_you_opped(channel->channel) || !nick || !ThisNick || match(nick, get_server_nickname(from_server)))
		return;

	if (!(flag = channel->set_hacking))
		return;

	if ((ThisNick->sent_deop) && (time(NULL) - ThisNick->sent_deop_time < 10))
		return;

#if 0
	if (ThisNick->sent_reop)
		ThisNick->sent_reop--;
#endif	
	userptr = ThisNick->userlist;
	botptr = ThisNick->botlist;
	shitptr = ThisNick->shitlist;
	check_op = check_whosplitin_buffer(nick, ThisNick->host, channel->channel, 0);
	if (check_op && check_op->has_ops)
		return;
		
	if ( (!userptr && !botptr) || (userptr && !check_channel_match(userptr->channels, channel->channel)) ||
		(botptr && !check_channel_match(botptr->channels, channel->channel)) ||
		(shitptr && check_channel_match(shitptr->channels, channel->channel)))
	{
		switch(flag)
		{
			case 1:
			case 3:
				if (is_chanop(channel->channel, nick))
/*					my_send_to_server(from_server, "MODE %s -o %s", channel->channel, nick);*/
					add_mode(channel, "o", 0, nick, NULL, get_int_var(NUM_OPMODES_VAR));
				ThisNick->sent_deop++;
				ThisNick->sent_deop_time = time(NULL);
			case 2:
				if (flag != 1)
					bitchsay("NetHack detected on %s by %s!%s", channel->channel, nick, ThisNick->host); 
			case 0:
			default:
				break;
		}
	}
}

/*
 *  Protection levels
 *  1 Reop if de-oped
 *  2 De-op offender
 *  3 Kick  offender
 *  4 KickBan offender
 */
int check_prot(char *from, char *person, ChannelList *chan, BanList *thisban, NickList *n)
{
NickList *tmp;
NickList *kicker;
char *bans = NULL;
char *tmp_ban = NULL;
char *nick = NULL, *userhost = NULL, *p;
int full_match = 0;


	context;
	if (!from || !*from || !person || !*person || !chan)
		return 0;

	if (!my_stricmp(person, from))
	{
		return 0;
	}

	malloc_sprintf(&tmp_ban, person);
	if ((p = strchr(tmp_ban, '!')))
	{
		nick = tmp_ban;
		*p++ = '\0';
		userhost  = p;
	} else nick = person;

	for (tmp = chan->nicks; tmp; tmp = tmp->next)
	{
		if (match(nick, tmp->nick))
		{
			if (userhost && tmp)
			{
				if (match(userhost, tmp->host))
				{
					full_match = 1;
					break;
				}
			} 
		}
	}

	if (thisban && full_match)
	{ 
		if (chan->set_kick_if_banned)
		{
#if 0
			if(chan->chop && protected)
			{
				add_mode(chan, "b", 0, thisban->ban, NULL, get_int_var(NUM_BANMODES_VAR));
				add_mode(chan, NULL, 0, protected, "\002BitchX\002 Protected User", 0);
			} 
			else
			{
				malloc_strcpy(&protected, from);
				add_timer("", 15, delay_kick, m_sprintf("%s %s %d", protected, chan->channel, from_server), NULL);
			}
#endif
		}
	}

#if 0
	if (tmp && tmp->sent_deop)
		tmp->sent_deop--;
#endif
	if (n && (n->userlist || n->botlist) && chan->chop)
	{
		UserList *user = n->userlist;		
		if (!user)
			user = n->botlist;		
		if (!user || (user && !check_channel_match(user->channels, chan->channel)))
			return 0;
		kicker = (NickList *) find_in_list((List **)&chan->nicks, from, 0);
		switch(user->prot)
		{
			case 2:
			{
				if (kicker->sent_deop < 4)
				{
					if (!kicker->userlist || !kicker->botlist)
						add_mode(chan, "o", 0, from, NULL, get_int_var(NUM_OPMODES_VAR));
					else if ((kicker->userlist && !kicker->userlist->prot) || (kicker->botlist && !kicker->botlist->prot))
						add_mode(chan, "o", 0, from, NULL, get_int_var(NUM_OPMODES_VAR));
					kicker->sent_deop++;
				}
				goto reop_user;
					
			}
			case 5:
			{
				
				if (thisban && !thisban->sent_unban)
				{
					thisban->sent_unban++;
					thisban->sent_unban_time = time(NULL);
					add_mode(chan, "b", 0, thisban->ban, NULL, get_int_var(NUM_BANMODES_VAR));
					send_to_server("INVITE %s %s", chan->channel, person);  
				}
			}
			case 4:
			{
				if (kicker && (kicker->userlist || kicker->botlist))
					add_mode(chan, "o", 0, from, NULL, get_int_var(NUM_OPMODES_VAR));
				else if (kicker)
				{
					char *new_kick = NULL, *h, *u;
					u = new_kick = m_strdup(kicker->host);
					h = strchr(u, '@');
					*h++ = 0;
					add_mode(chan, "o", 0, from, NULL, get_int_var(NUM_OPMODES_VAR));
					add_mode(chan, "b", 1, ban_it(kicker->nick, u, h), NULL, get_int_var(NUM_BANMODES_VAR));
					new_free(&u);
				}
			}
			case 3:
			{
				if (kicker)
					add_mode(chan, NULL, 0, kicker->nick, "\002BitchX\002 Protected User", 0);
			}
			case 1:
			{
reop_user:
				/* added by Sergs serg@gamma.niimm.spb.su */
			        if (thisban)
				{
					if (thisban->sent_unban_time - time(NULL) > 10)
					{
						thisban->sent_unban++;
						thisban->sent_unban_time = time(NULL);
						add_mode(chan, "b", 0, thisban->ban, NULL, get_int_var(NUM_BANMODES_VAR));
					} 
					else if (thisban->sent_unban < 3)
					{
						thisban->sent_unban++;
						thisban->sent_unban_time = time(NULL);
						add_mode(chan, "b", 0, thisban->ban, NULL, get_int_var(NUM_BANMODES_VAR));
					}
				}
				else
				if (!n->sent_reop)
				{
					add_mode(chan, "o", 1, n->nick, NULL, get_int_var(NUM_OPMODES_VAR));
					n->sent_reop++;
					n->sent_reop_time = time(NULL);
				}
				break;
			}
			default:
				break;
		}
		new_free(&bans);
		return 1;
	}
	new_free(&bans);
	return 0;
}

void check_shit(ChannelList *chan)
{
ShitList *Bans;
	if (!chan || !chan->chop || !shitlist_list || chan->server == -1)
		return;
	for (Bans = shitlist_list; Bans; Bans = Bans->next)
	{
		/* this is a permanent ban */
		if (!check_channel_match(Bans->channels, chan->channel))
			continue;
		if (Bans->level == 4 && !ban_is_on_channel(Bans->filter, chan))
		{
			add_mode(chan, "b", 1, Bans->filter, NULL, get_int_var(NUM_BANMODES_VAR));
/*			my_send_to_server(chan->server, "MODE %s +b %s", chan->channel, Bans->filter);*/
		}
	}
	flush_mode_all(chan);
}

void change_user(char *command, char *args, char *subargs)
{
int comm = 0;
char *user, *what;
char err_msg[5][50] = { "", "No Level Specified", "No Protection level Specified", "No Channel(s) Specified", "No Auto-op Specified" };

	context;
	if (!my_stricmp(command, "CHGLEVEL"))
		comm = 1;
	else if (!my_stricmp(command, "CHGPROT"))
		comm = 2;
	else if (!my_stricmp(command, "CHGCHAN"))
		comm = 3;
	else if (!my_stricmp(command, "CHGAOP"))
		comm = 4;
	if (comm && (user = next_arg(args, &args)))
	{
		UserList *ThisNick;
		int prot, level, autoop;
		char *channels = NULL;
		char *password = NULL;
		if (!(what = next_arg(args, &args)))
		{
			bitchsay("%s %s", command, err_msg[comm]);
			return;
		}
		if (!(ThisNick = (UserList *)remove_from_list((List **)&user_list, user)))
		{
			bitchsay("Nick [%s] was not found on userlist", user);
			return;
		}	
		sync_nicklist(ThisNick, 0);
		prot = ThisNick->prot;
		level = ThisNick->level;
		autoop = ThisNick->aop;
		channels = ThisNick->channels;
		password = ThisNick->password;					

		switch(comm)
		{
			case 1:
				level = my_atol(what);
				break;
			case 2:
				prot = my_atol(what);
				break;
			case 3:
				ThisNick->channels = NULL;
				malloc_strcpy(&channels, what);
				break;
			case 4:
				autoop = my_atol(what);
				break;
			default:
				break;
		}
		if (password)
			add_to_userhost_queue(ThisNick->nick, prepare_userhost, "%d %d %d %d 0 %s %s", USERLIST_MOD, level, autoop, prot, channels, password);
		else
			add_to_userhost_queue(ThisNick->nick, prepare_userhost, "%d %d %d %d 0 %s", USERLIST_MOD, level, autoop, prot, channels);
		new_free(&ThisNick->nick);
		new_free(&ThisNick->host);
		new_free(&ThisNick->channels);
		new_free(&ThisNick->password);
		new_free((char **)&ThisNick);
	}		
	else
		userage(command , (comm == 1) ? "<nick> <level>" : (comm == 2)? "<nick> <prot>":(comm == 3)?"<nick> <channel\002|\002channel,channel>":"<nick> <auto-op>");
	return;
}
