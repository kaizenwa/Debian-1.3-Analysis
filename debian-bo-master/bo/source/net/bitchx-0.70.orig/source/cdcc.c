/* CDCC v2 1.00 (c) Copyright 1996 William Glozer  */
/* ----------------------------------------------- */
/* Last revision 04.19.96 by Ananda                */

/* New CDCC for BitchX and any other clients that deserve to run it :) */
/* Note that I did use a lot of code/ideas from a copy of CDCC written */
/* for BitchX by panasync, so thanks to him for alot of the code and   */
/* ideas :)  I would appreciate any bugs reported to me as soon as is  */
/* possible, and cdcc.c + cdcc.h for any mods you do... if you modify  */
/* it for your client, I can add those mods as #ifdefs so the next ver */
/* will work with your client and have all the new stuff.  -Ananda '96 */
/* Modifed even more by panasync (edwac@sasknet.sk.ca) to interface    */
/* cleanly and nicely with BitchX. Blame all bugs on me instead of     */
/* Ananda */

#define BITCHX
#define CDCC_FLUD
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "irc.h"
#include "ircaux.h"
#include "ignore.h"
#include "ctcp.h"
#include "hook.h"
#include "dcc.h"
#include "flood.h"
#include "screen.h"
#include "parse.h"
#include "output.h"
#include "input.h"
#include "server.h"
#include "vars.h"
#include "userlist.h"
#include "misc.h"
#include "whois.h"

#include "cdcc.h"
#ifdef BITCHX
#include "misc.h"
#endif

/* external ircII stuff */
extern int dcc_active_count;
extern void dcc_filesend(char *);
extern void dcc_resend(char *);
int l_timer _((char *, char *));

/* local commands */
local_cmd local[] = {
	{ "CHANNEL",	l_channel,	"public timer channel" },
	{ "DOFFER",	l_doffer,	"remove pack from the offer list" },
	{ "LIST",	l_list,		"list the packs you have offered" },
	{ "LOAD",	l_load,		"load packs saved to .cdcc.save or specified name" },
	{ "MINSPEED",	l_minspeed,	"minspeed for cdcc ( #.##)" },
	{ "NOTICE",	l_notice,	"notify the channel of offered packs" },
	{ "OFFER",	l_offer,	"add a pack to the offer list" },
	{ "PLIST",	l_plist,	"publicly list your offered packs" },
	{ "QUEUE",	l_queue,	"view entries in the send queue" },
	{ "SAVE",	l_save,		"save your offerlist to .cdcc.save or specified name" },
	{ "SEND",	l_send,		"send a pack to user" },
	{ "RESEND",	l_resend,	"re-send a pack to user" },
	{ "TIMER",	l_timer,	"public list timer in minutes" },
	{ "NOTE",	l_note,		"add note to pack number"},
	{ "TYPE",	l_type,		"toggle between public and notice" },
	{ "ECHO",	l_echo,		"toggle echo on/off" },
	{ "",		NULL,		"" } 
};
#define NUM_LOCAL (sizeof(local) / sizeof(local_cmd))

/* remote commands */
remote_cmd remote[] = {
	{ "HELP",	r_help,		"get help on CDCC commands" },
	{ "RESEND",	r_rsend,	"have CDCC re-send you pack #N"},
	{ "SEND",	r_send,		"have CDCC send you pack #N" },
	{ "LIST",	r_list,		"get a list of offered packs" },
	{ "",		NULL,		"" }, 
};
#define NUM_REMOTE (sizeof(remote) / sizeof(remote_cmd))

/* ahh yes, global variables :( well, interfacing with ircII is a pain in  */
/* the ass, and I couldn't figure out a better way... more than one of my  */
/* routines need these... the static will keep them from being used by any */
/* functions outside of cdcc.c                                             */

static int numpacks = 0;
pack *offerlist = NULL;
static pack *newpack;

static queue *queuelist = NULL;
static unsigned long total_size_of_packs = 0;
static int numqueue = 0;
static int ptimer = 0;
static int do_notice_list = 0;
static int do_cdcc_echo = 1;

double cdcc_minspeed = 0.0;
static char *public_channel = NULL;


#ifdef BITCHX
extern double dcc_max_rate_out, dcc_bytes_out, dcc_max_rate_in, dcc_bytes_in;
#endif

#define cparse(s) convert_output_format(s, NULL, NULL)

/* parse a users CDCC command */
void cdcc(char *command, char *args, char *blah)
{
	int i;
	char *cmd, *rest;
	
	cmd = next_arg(args, &args);
	rest = next_arg(args, &args);
	if (!cmd) {
		l_help(NULL, NULL);
		return;
	}
	
	for (i = 0; *local[i].name; i++) {
		if (!my_stricmp(local[i].name, cmd)) {  
			local[i].function(rest, args);
			return;
		}
	}
	put_it("%s: unknown command \002%s\002", cparse(get_string_var(CDCC_PROMPT_VAR)), cmd);
	return;
}
			
/* parse a remote message CDCC command */
char *msgcdcc(char *from, char *to, char *args)
{
	int i;
	int secure = 0;
	char *cdcc, *rest, *cmd, *temp = NULL, *q;

	malloc_strcpy(&temp, args);
	
	q = temp;
	cdcc = next_arg(temp, &temp);
	if (!cdcc || my_strnicmp(++cdcc, "DCC", 3)) {
	 	new_free(&q);
	 	return args;
	 } 
	
	if ((check_ignore(from, FromUserHost, to, IGNORE_CDCC) == IGNORED))
	{
		new_free(&q);
		return args;
	}
			
	if (!check_flooding(from, CDCC_FLOOD, args, NULL))
	{
		new_free(&q);
		return NULL;
	}
	if (!offerlist)
	{
		new_free(&q);
		return NULL;
	}
	if ((secure = get_int_var(CDCC_SECURITY_VAR)))
	{
		UserList *tmp;
		if (!(tmp = lookup_userlevelc("*", FromUserHost, "*", user_list)) || tmp->level < secure)
		{
			new_free(&q);
			return args;
		}
	}

	cmd = next_arg(temp, &temp);
	if (!cmd) {
		new_free(&q);
		return args;
	}

	rest = temp;

	for (i = 0; *remote[i].name; i++) {
		if (!my_stricmp(cmd, remote[i].name)) {
			remote[i].function(from, rest);
			new_free(&q);
			return NULL;
		}
	}
	send_to_server("NOTICE %s :try /ctcp %s cdcc help",
		from, nickname);
	new_free(&q);
	return args;
}
 
int l_send(char *args, char *rest)
{
	pack *ptr;
	char *temp = NULL, *file = NULL, *dccinfo = NULL, *q = NULL, *p;
	int maxdcc, maxqueue;
	
	if (!args || !*args)
	{
		userage("cdcc", "send <nickname> <#\002|\002filename>");
		return 0;
	}

	maxdcc = get_int_var(DCC_SEND_LIMIT_VAR);
	maxqueue = get_int_var(DCC_QUEUE_LIMIT_VAR);
	
	while (1)
	{
		if (!(temp = next_arg(rest, &rest)))
			break;

		for (ptr = offerlist; ptr; ptr = ptr->next)
			if (matchmcommand(temp, ptr->num))
				break;
		
		if (ptr)
		{

			if (maxdcc && dcc_active_count >= maxdcc) 
			{
				if (maxqueue && (numqueue >= maxqueue)) 
				{
					put_it("%s: all dcc and queue slots full", cparse(get_string_var(CDCC_PROMPT_VAR)));
					return 0;
				}
				add_to_queue(args, ptr);
				continue;
			}
	
			put_it("%s: sending %s%s%s pack #\002%d\002 (\002%d\002 file%s)", cparse(get_string_var(CDCC_PROMPT_VAR)),
				UND_TOG_STR, args, UND_TOG_STR, ptr->num, ptr->numfiles,
				plural(ptr->numfiles));
			malloc_strcpy(&file, ptr->file);
			q = file;

			for (p = next_arg(file, &file); p && *p; p = next_arg(file, &file)) 
			{
				malloc_sprintf(&dccinfo, "%s %s", args, p);
				dcc_filesend(dccinfo);
			}
			ptr->gets++;
		}
		else
		{
			if (offerlist && (isdigit(*temp) || (*(temp+1) && (isdigit(*(temp+1))))))
				put_it("%s: No such pack number", cparse(get_string_var(CDCC_PROMPT_VAR)));
			else
			{
				malloc_sprintf(&dccinfo, "%s %s", args, temp);
				dcc_filesend(dccinfo);
				new_free(&dccinfo);
			}
		}  
		new_free(&q);
		file = NULL;
	}
	new_free(&dccinfo);
	new_free(&q);
	return 0;
}

int l_resend(char *args, char *rest)
{
	pack *ptr;
	char *temp = NULL, *file = NULL, *dccinfo = NULL, *q = NULL, *p;
	int maxdcc, maxqueue;
	if (!args || !*args)
	{
		userage("cdcc", "resend <nickname> <#\002|\002filename>");
		return 0;
	}
	maxdcc = get_int_var(DCC_SEND_LIMIT_VAR);
	maxqueue = get_int_var(DCC_QUEUE_LIMIT_VAR);

	while (1)
	{
		if (!(temp = next_arg(rest, &rest)))
			break;

		for (ptr = offerlist; ptr; ptr = ptr->next)
			if (matchmcommand(temp, ptr->num))
				break;
		
		if (ptr)
		{

			if (maxdcc && dcc_active_count >= maxdcc) 
			{
				if (maxqueue && (numqueue >= maxqueue)) 
				{
					put_it("%s: all dcc and queue slots full", cparse(get_string_var(CDCC_PROMPT_VAR)));
					return 0;
				}
				add_to_queue(args, ptr);
				continue;
			}
	
			put_it("%s: resending %s%s%s pack #\002%d\002 (\002%d\002 file%s)", cparse(get_string_var(CDCC_PROMPT_VAR)),
				UND_TOG_STR, args, UND_TOG_STR, ptr->num, ptr->numfiles,
				plural(ptr->numfiles));
			malloc_strcpy(&file, ptr->file);
			q = file;

			for (p = next_arg(file, &file); p && *p; p = next_arg(file, &file)) 
			{
				malloc_sprintf(&dccinfo, "%s %s", args, p);
				dcc_resend(dccinfo);
			}
			ptr->gets++;
		}
		else
		{
			if (offerlist && (isdigit(*temp) || (*(temp+1) && (isdigit(*(temp+1))))))
				put_it("%s: No such pack number", cparse(get_string_var(CDCC_PROMPT_VAR)));
			else
			{
				malloc_sprintf(&dccinfo, "%s %s", args, temp);
				dcc_resend(dccinfo);
				new_free(&dccinfo);
			}
		}  
		new_free(&q);
		file = NULL;
	}

	new_free(&dccinfo);
	new_free(&q);
	return 0;
}

/*resends a pack to the requestee*/
/*Added by Wicked Angel: wangel@wgrobez1.remote.louisville.edu*/
/*It wasn't hard ... but hey ... it seemed like a good idea :>*/
int r_rsend(char *from, char *args)
{
	pack *ptr;
	char *temp = NULL, *file = NULL, *dccinfo = NULL, *q = NULL, *p;
	int maxdcc, maxqueue;
#if 0	
	temp = strchr(args, '#');
	if (!temp)
		temp = args;
	else
		temp++;

	packnum = atoi(temp);
	if (!packnum || packnum > numpacks) {
		send_to_server("NOTICE %s :\002CDCC\002: invalid pack number", from);
		return 0;
	}
	maxdcc = get_int_var(DCC_SEND_LIMIT_VAR);
	maxqueue = get_int_var(DCC_QUEUE_LIMIT_VAR);
	
	for (ptr = offerlist; ptr; ptr = ptr->next) {
		if (ptr->num == packnum)
			break;
	}
	if (!ptr)
	{
		put_it("%s: No such pack number", cparse(get_string_var(CDCC_PROMPT_VAR)));
		return 0;
	}
	if (maxdcc && dcc_active_count >= maxdcc) 
	{
		if (maxqueue && (numqueue >= maxqueue)) {
			send_to_server("NOTICE %s: \002CDCC\002: all dcc and queue slots full, sorry", from);
			return 0;
		}
		add_to_queue(from, ptr);			
		return 0;
	}
	
	put_it("%s: resending %s%s%s pack #\002%d\002 (\002%d\002 file%s)",cparse(get_string_var(CDCC_PROMPT_VAR)),
		UND_TOG_STR, from, UND_TOG_STR, packnum, ptr->numfiles,
		plural(ptr->numfiles));
	malloc_strcpy(&file, ptr->file);
	q = file;
	for (temp = next_arg(file, &file); temp && *temp; temp = next_arg(file, &file)) {
		malloc_sprintf(&dccinfo, "%s %s", from, temp);
		dcc_resend(dccinfo);
	}
#endif

	maxdcc = get_int_var(DCC_SEND_LIMIT_VAR);
	maxqueue = get_int_var(DCC_QUEUE_LIMIT_VAR);

	while (1)
	{
		if (!(temp = next_arg(args, &args)))
			break;

		for (ptr = offerlist; ptr; ptr = ptr->next)
			if (matchmcommand(temp, ptr->num))
				break;
		
		if (ptr)
		{

			if (maxdcc && ((maxdcc - dcc_active_count) < ptr->numfiles || dcc_active_count >= maxdcc)) 
			{
				if (maxqueue && (numqueue >= maxqueue)) 
				{
					send_to_server("NOTICE %s: \002CDCC\002: all dcc and queue slots full, sorry", from);
					return 0;
				}
				add_to_queue(from, ptr);
				continue;
			}
	
			put_it("%s: resending %s%s%s pack #\002%d\002 (\002%d\002 file%s)", cparse(get_string_var(CDCC_PROMPT_VAR)),
				UND_TOG_STR, from, UND_TOG_STR, ptr->num, ptr->numfiles,
				plural(ptr->numfiles));
			malloc_strcpy(&file, ptr->file);
			q = file;

			for (p = next_arg(file, &file); p && *p; p = next_arg(file, &file)) 
			{
				malloc_sprintf(&dccinfo, "%s %s", from, p);
				dcc_resend(dccinfo);
			}
			ptr->gets++;
		}
		else
			send_to_server("NOTICE %s :\002CDCC\002: invalid pack number", from);
		new_free(&q);
		file = NULL;
	}

	new_free(&dccinfo);
	new_free(&q);
	return 0;
}

/* send a pack to the remote user */
int r_send(char *from, char *args)
{
	pack *ptr;
	char *temp = NULL, *file = NULL, *dccinfo = NULL, *q = NULL, *p;
	int maxdcc, maxqueue;

	maxdcc = get_int_var(DCC_SEND_LIMIT_VAR);
	maxqueue = get_int_var(DCC_QUEUE_LIMIT_VAR);

	while (1)
	{
		if (!(temp = next_arg(args, &args)))
			break;

		for (ptr = offerlist; ptr; ptr = ptr->next)
			if (matchmcommand(temp, ptr->num))
				break;
		
		if (ptr)
		{

			if (maxdcc && dcc_active_count >= maxdcc) 
			{
				if (maxqueue && (numqueue >= maxqueue)) 
				{
					send_to_server("NOTICE %s: \002CDCC\002: all dcc and queue slots full, sorry", from);
					return 0;
				}
				add_to_queue(from, ptr);
				continue;
			}
	
			put_it("%s: sending %s%s%s pack #\002%d\002 (\002%d\002 file%s)", cparse(get_string_var(CDCC_PROMPT_VAR)),
				UND_TOG_STR, from, UND_TOG_STR, ptr->num, ptr->numfiles,
				plural(ptr->numfiles));
			malloc_strcpy(&file, ptr->file);
			q = file;

			for (p = next_arg(file, &file); p && *p; p = next_arg(file, &file)) 
			{
				malloc_sprintf(&dccinfo, "%s %s", from, p);
				dcc_filesend(dccinfo);
			}
			ptr->gets++;
		}
		else
			send_to_server("NOTICE %s :\002CDCC\002: invalid pack number", from);
		new_free(&q);
		file = NULL;
	}
	
	new_free(&dccinfo);
	new_free(&q);
	return 0;
}
			

/* remote pack list */
int r_list(char *from, char *args)
{
	pack *ptr;
	char size[30];
	char mrate_out[30];
	char mrate_in[30];
	char bytes_out[30];
	char bytes_in[30];
	char speed_out[30];
		
	int once = 0;
	sprintf(mrate_out, "%1.3g", dcc_max_rate_out);
	sprintf(mrate_in, "%1.3g", dcc_max_rate_in);
	sprintf(bytes_out, "%1.3g", dcc_bytes_out);
	sprintf(bytes_in, "%1.3g", dcc_bytes_in);
	sprintf(speed_out, "%1.3g", cdcc_minspeed);

	for (ptr = offerlist; ptr; ptr = ptr->next) {
#ifdef BITCHX
		if (!once && do_hook(CDCC_PREPACK_LIST, "%s %s %d %d %d %d %d %s %s %s %s %lu %s", 
			from, nickname, numpacks, 
			get_int_var(DCC_SEND_LIMIT_VAR)-dcc_active_count, 
			get_int_var(DCC_SEND_LIMIT_VAR), numqueue, 
			get_int_var(DCC_QUEUE_LIMIT_VAR), 
			mrate_out, bytes_out, mrate_in, bytes_in, 
			total_size_of_packs, speed_out))
		{
			send_to_server("NOTICE %s :Files Offered: /ctcp %s CDCC send #N for pack N", from, nickname);
			if (get_int_var(DCC_SEND_LIMIT_VAR))
				send_to_server("NOTICE %s :    [%d pack%s %d/%d slots open]", from, 
					numpacks, plural(numpacks), get_int_var(DCC_SEND_LIMIT_VAR)-dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR));
			else
				send_to_server("NOTICE %s :    [%d pack%s]", from, numpacks,plural(numpacks));
		}
#endif
	 	if (ptr->size / 1024 > 999)
			sprintf(size, "\002%4.1f\002mb",
				(((double)ptr->size) / 1024) / 1024);
		else
			sprintf(size, "\002%4.1f\002kb", (((double)ptr->size) / 1024)); 
				
#ifdef BITCHX
		if (do_hook(CDCC_PACK_LIST, "%s %d %d %lu %d %s", 
			from, ptr->num, ptr->numfiles, ptr->size, ptr->gets, ptr->desc))
		{
#endif
			send_to_server("NOTICE %s :#%d \037(\037%10s\037:\037\002%4d\002 get%s\037)\037 %-40.40s",
				from, ptr->num, size, ptr->gets, plural(ptr->gets), 
				ptr->desc ? ptr->desc : "no description");
			if (ptr->notes)
				send_to_server("NOTICE %s :      %s", from, ptr->notes);
#ifdef BITCHX
		}
		once++;
#endif
	}
#ifdef BITCHX
	if (once)
		do_hook(CDCC_POSTPACK_LIST, "%s %s %d %d %d %d %d %s %s %s %s %lu %s", from, nickname, numpacks, get_int_var(DCC_SEND_LIMIT_VAR)-dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR), numqueue, get_int_var(DCC_QUEUE_LIMIT_VAR), 
			mrate_out, bytes_out, mrate_in, bytes_in, total_size_of_packs, speed_out);
#endif
	return 0;
}		

/* remote help display  */
int r_help(char *from, char *args)
{
	int i;
#ifdef CDCC_FLUD
	for (i = 0; *remote[i].name; i++) {
		send_to_server("NOTICE %s :\002CDCC\002: %-4s - %s",
			from, remote[i].name, remote[i].help);
	}
#endif
	return 0;
}


/* remove a pack or all packs from the offerlist */
int l_doffer(char *args, char *rest)
{
	if (!numpacks) {
		put_it("%s: you have no packs offered", cparse(get_string_var(CDCC_PROMPT_VAR)));
		return 0;
	}
	if (args && *args)
	{
		char * temp = NULL;
		malloc_sprintf(&temp, "%s %s", args, rest ? rest : "");
		del_pack(NULL, temp);
		new_free(&temp);
		l_list(NULL, NULL);
	}
	else 
	{
		l_list(NULL, NULL);
		add_wait_prompt("Remove pack [* for all packs]: ", del_pack, "", WAIT_PROMPT_LINE);
	}
	return 0;
}

/* localy list the packs you have offered */
int l_list(char *args, char *rest)
{
	pack *ptr;
	char size[30];

	if (!numpacks) {
		put_it("%s: you have no packs offered", cparse(get_string_var(CDCC_PROMPT_VAR)));
		return 0;
	}
	if (args)
	{
		int it = atoi(args);
		if (it <= 0)
			return 0;
		for (ptr = offerlist; ptr; ptr= ptr->next)
		{
			if (it == ptr->num)
			{
				char *temp = NULL;
				char *p, *q;
				malloc_strcpy(&temp, ptr->file);
				q = temp;				
				while (temp && *temp)
				{
					p = next_arg(temp, &temp);
					put_it("#%-2d %-2d gets   %s",
						ptr->num, ptr->gets, p);
				}
				
				new_free(&q);
				break;
			}
		}
	}
	else 
	{
		for (ptr = offerlist; ptr; ptr = ptr->next) 
		{
			if (ptr->size / 1024 > 999)
				sprintf(size, "\002%3.2f\002mb",
					(double) (ptr->size / 1024) / 1024);
			else
				sprintf(size, "\002%3.2f\002kb", (double) ptr->size / 1024); 
	
			put_it("#%-2d \002%3d\002 file%s %10s \002%4d\002 get%s %-39.39s",
				ptr->num, ptr->numfiles, plural(ptr->numfiles),
				size, ptr->gets, plural(ptr->gets), ptr->desc);
		}
	}
	return 0;
}

/* add a pack to the offer list */
int l_offer(char *args, char *rest)
{
char *tmp = NULL;
	if (args && *args)
	{
		tmp = m_sprintf("%s %s", args, rest?rest:"");
		add_files(NULL, tmp);
	}
	else
	{
		malloc_sprintf(&tmp, "Add file(s) to pack%s #%d : ", plural(numpacks), numpacks+1);
		add_wait_prompt(tmp, add_files, "", WAIT_PROMPT_LINE);
	}
	new_free(&tmp);
	return 0;
}

/* display the offerlist to current channel */
int l_plist(char *args, char *rest)
{
	pack *ptr;
	char *chan = NULL, *string = NULL;
	char size[20];
	char mrate_out[30];
	char mrate_in[30];
	char bytes_out[30];
	char bytes_in[30];
	char speed_out[30];
		
	int maxdccs, blocksize, maxqueue;
	
	if (!get_channel_by_refnum(0) || !numpacks || (args && *args && !is_channel(args))) {
		put_it("%s: you %s",cparse(get_string_var(CDCC_PROMPT_VAR)),
			numpacks ? "are not on a channel!" :
				   "have no packs offered!");
		return 0;
	}
	if (args && *args)
		malloc_strcpy(&chan, args);
	else	
		malloc_strcpy(&chan, get_channel_by_refnum(0));

	maxdccs = get_int_var(DCC_SEND_LIMIT_VAR);
	blocksize = get_int_var(DCC_BLOCK_SIZE_VAR);
	maxqueue = get_int_var(DCC_QUEUE_LIMIT_VAR);
	message_from(chan, LOG_CRAP);
#ifdef BITCHX
	sprintf(mrate_out, "%1.3g", dcc_max_rate_out);
	sprintf(mrate_in, "%1.3g", dcc_max_rate_in);
	sprintf(bytes_out, "%1.3g", dcc_bytes_out);
	sprintf(bytes_in, "%1.3g", dcc_bytes_in);
	sprintf(speed_out, "%1.3g", cdcc_minspeed);
	if (do_hook(CDCC_PREPACK_LIST, "%s %s %d %d %d %d %d %s %s %s %s %lu %s", chan, nickname, numpacks, get_int_var(DCC_SEND_LIMIT_VAR)-dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR), numqueue, get_int_var(DCC_QUEUE_LIMIT_VAR), mrate_out, bytes_out, mrate_in, bytes_in, total_size_of_packs, speed_out))
	{
#endif
		malloc_sprintf(&string, "\037[\037cdcc\037]\037 \002%d\002 file%s offered\037-\037 /ctcp \002%s\002 cdcc send #x for pack #x",
			numpacks, plural(numpacks), nickname);
		send_text(chan, string, NULL, do_cdcc_echo, 0);		
		malloc_sprintf(&string, "\037[\037cdcc\037]\037 dcc block size\037:\037 \002%d\002, slots open\037:\037 \002%2d\002/\002%2d\002, dcc queue\037:\037 \002%2d\002/\002%2d\002",
			(blocksize) ? blocksize : 1024, maxdccs - dcc_active_count,
			maxdccs, maxqueue - numqueue, maxqueue);
		send_text(chan, string, NULL, do_cdcc_echo, 0);
#ifdef BITCHX
	}
#endif	
	for (ptr = offerlist; ptr; ptr = ptr->next) {
	 	if (ptr->size / 1024 > 999)
			sprintf(size, "\002%3.2f\002mb",
				(float) (ptr->size / 1024) / 1024);
		else
			sprintf(size, "\002%3.2f\002kb", (float) ptr->size / 1024); 
				
#ifdef BITCHX
		if (do_hook(CDCC_PACK_LIST, "%s %d %d %lu %d %s", 
			chan, ptr->num, ptr->numfiles, ptr->size, ptr->gets, ptr->desc))
		{
#endif
			malloc_sprintf(&string, "\037%%\037 #%-2d \037(\037%10s\037:\037\002%4d\002 get%s\037)\037 %-39.39s",
				ptr->num, size, ptr->gets, plural(ptr->gets), 
				ptr->desc ? ptr->desc : "no description");
			send_text(chan, string, NULL, do_cdcc_echo, 0);
			if (ptr->notes)
			{
				malloc_sprintf(&string, "         %s", ptr->notes);
				send_text(chan, string, NULL, do_cdcc_echo, 0);
			}
#ifdef BITCHX
		}
#endif
	}
#ifdef BITCHX
	do_hook(CDCC_POSTPACK_LIST, "%s %s %d %d %d %d %d %s %s %s %s %lu %s", chan, nickname, numpacks, get_int_var(DCC_SEND_LIMIT_VAR)-dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR), numqueue, get_int_var(DCC_QUEUE_LIMIT_VAR), 
		mrate_out, bytes_out, mrate_in, bytes_in, total_size_of_packs, speed_out);
#endif
	message_from(NULL, LOG_CRAP);				
	new_free(&chan);
	new_free(&string);
	return 0;
}
		
/* display CDCC help */
int l_help(char *args, char *rest)
{
	int i;
	
	if (args) {
		for (i = 0; *local[i].name; i++) {
			if (!my_stricmp(args, local[i].name)) {
				put_it("%s: %-9s - %s", cparse(get_string_var(CDCC_PROMPT_VAR)),local[i].name, local[i].help);
				return 0;
			}
		}
		put_it("%s: unknown command \002%s\002", cparse(get_string_var(CDCC_PROMPT_VAR)),args);
		return 0;
	} else 
		for (i = 0; *local[i].name; i++)
			put_it("%s: %-9s - %s", cparse(get_string_var(CDCC_PROMPT_VAR)),local[i].name, local[i].help);
	
	return 0;
}

/* notify the current channel that packs are offered */
int l_notice(char *args, char *rest)
{
	char *string = NULL, *chan = NULL;
	char mrate_out[30];
	char mrate_in[30];
	char bytes_out[30];
	char bytes_in[30];
	char speed_out[30];	
	
	if (!get_channel_by_refnum(0) || !numpacks || (args && *args && !is_channel(args))) {
		put_it("%s: you %s",cparse(get_string_var(CDCC_PROMPT_VAR)),
			numpacks ? "are not on a channel!" :
				   "have no packs offered!");
		return 0;
	}
	
	if (args && *args)
		malloc_strcpy(&chan, args);
	else
		malloc_strcpy(&chan, get_channel_by_refnum(0));
	message_from(chan, LOG_CRAP);	
#ifdef BITCHX
	sprintf(mrate_out, "%1.3g", dcc_max_rate_out);
	sprintf(mrate_in, "%1.3g", dcc_max_rate_in);
	sprintf(bytes_out, "%1.3g", dcc_bytes_out);
	sprintf(bytes_in, "%1.3g", dcc_bytes_in);
	sprintf(speed_out, "%1.3g", cdcc_minspeed);
	if (do_hook(CDCC_PREPACK_LIST, "%s %s %d %d %d %d %d %s %s %s %s %lu %s", chan, nickname, numpacks, get_int_var(DCC_SEND_LIMIT_VAR)-dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR), numqueue, get_int_var(DCC_QUEUE_LIMIT_VAR), mrate_out, bytes_out, mrate_in, bytes_in, total_size_of_packs, speed_out))
	{
#endif
		malloc_sprintf(&string, "\037[\037cdcc\037]\037 \002%d\002 file%s offered\037-\037 \037\"\037/ctcp \002%s\002 cdcc list\037\"\037 for pack list",   
			numpacks, plural(numpacks), nickname);
		send_text(chan, string, "NOTICE", do_cdcc_echo, 0);
	
#ifdef BITCHX
	}
#endif	
#ifdef BITCHX
	do_hook(CDCC_POSTPACK_LIST, "%s %s %d %d %d %d %d %s %s %s %s %lu %s", chan, nickname, numpacks, get_int_var(DCC_SEND_LIMIT_VAR)-dcc_active_count, get_int_var(DCC_SEND_LIMIT_VAR), numqueue, get_int_var(DCC_QUEUE_LIMIT_VAR), 
		mrate_out, bytes_out, mrate_in, bytes_in, total_size_of_packs, speed_out);
#endif
	message_from(NULL, LOG_CRAP);
	new_free(&chan);
	new_free(&string);
	return 0;		
}
		
/* display entries in your send queue */
int l_queue(char *args, char *rest)
{
	queue *ptr;
	int num = 1;
	
	if (!queuelist) {
		put_it("%s: there are no queue entries",cparse(get_string_var(CDCC_PROMPT_VAR)));
		return 0;
	}

	for (ptr = queuelist; ptr; ptr = ptr->next, num++) {
#ifdef BITCHX
		if (do_hook(CDCC_QUEUE_LIST, "%s %s %d %d %s", ptr->nick, my_ctime(ptr->time), ptr->num, ptr->numfiles, ptr->desc))
#endif
			put_it("#\002%2d\002 nick: \037%9s\037 (\002%2d\002 file%s)",
				num, ptr->nick, ptr->numfiles,
				plural(ptr->numfiles));
	}
	
	return 0;
} 

/* save all of your offered packs */
int l_save(char *args, char *rest)
{
	FILE *file;
	char *name = NULL, *expand = NULL;
	pack *ptr;
	int count = 0;
	
	if (!offerlist) {
		put_it("%s: you have no packs offered", cparse(get_string_var(CDCC_PROMPT_VAR)));
		return 0;
	}
	
	malloc_sprintf(&expand, "~/%s", args ? args : ".cdcc.save");
	name = expand_twiddle(expand);
	new_free(&expand);

	if (!name || !(file = fopen(name, "wt"))) {
		put_it("%s: couldn't open \"%s\"", cparse(get_string_var(CDCC_PROMPT_VAR)),name);
		new_free(&name);
		return 0;
	}
	
	for (ptr = offerlist; ptr; ptr = ptr->next) {
		fprintf(file, "%s\n", ptr->file);
		fprintf(file, "%s %d\n", ptr->desc, ptr->numfiles);
		fprintf(file, "%s\n", ptr->notes?ptr->notes:empty_string);
		fprintf(file, "%d %ld %1.2f\n", ptr->gets, ptr->size, ptr->minspeed);
		count++;
	}
		
	fclose(file);
	put_it("%s: \002%d\002 pack%s saved to %s", cparse(get_string_var(CDCC_PROMPT_VAR)),
		count, plural(count), name);
	new_free(&name);

	return 0;
} 

/* load packs from cdcc.save */
int l_load(char *args, char *rest)
{
	FILE *file;
	char *buffer = NULL, *expand = NULL, *temp;
	pack *ptr, *last = NULL;
	int count = 0;
	
	malloc_sprintf(&expand, "~/%s", args ? args: ".cdcc.save");
	buffer = expand_twiddle(expand);
	new_free(&expand);

	if (!(file = fopen(buffer, "rt"))) {
		put_it("%s: couldn't open \"%s\"",  cparse(get_string_var(CDCC_PROMPT_VAR)), buffer);
		new_free(&buffer);
		return 0;
	}

	for (ptr = offerlist; ptr; ptr = ptr->next)
		last = ptr;
	
	new_free(&buffer);
	buffer = new_malloc(BIG_BUFFER_SIZE+1);
		
	while (fgets(buffer, BIG_BUFFER_SIZE, file)) {
		ptr = (pack *) new_malloc(sizeof(pack));
		ptr->num = ++numpacks;
		chop(buffer, 1);
		malloc_strcpy(&ptr->file, buffer);

		fgets(buffer, BIG_BUFFER_SIZE, file);
		chop(buffer, 1);

		temp = rindex(buffer, ' ');
		*temp++ = '\0';
		malloc_strcpy(&ptr->desc, buffer);
		ptr->numfiles = atoi(temp);

		fgets(buffer, BIG_BUFFER_SIZE, file);
		chop(buffer, 1);
		if (*buffer)
			malloc_strcpy(&ptr->notes, buffer);
	
		fgets(buffer, BIG_BUFFER_SIZE, file);
		chop(buffer, 1);
		ptr->gets = atoi(strtok(buffer, " "));
		ptr->size = atol(strtok(NULL, " "));		
		ptr->minspeed = atof(strtok(NULL,"\r")); 
		total_size_of_packs += ptr->size;	
		ptr->next = NULL;
		
		if (last) {
			last->next = ptr;
			last = ptr;
		} else {
			offerlist = ptr;
			last = offerlist;
		}
		count++;
	}
		
	fclose(file);
	put_it("%s: \002%d\002 pack%s loaded",  cparse(get_string_var(CDCC_PROMPT_VAR)), count, plural(count));
	set_int_var(_CDCC_PACKS_OFFERED_VAR, numpacks);
	new_free(&buffer);

	return 0;
} 
	
	
 
/* --- Misc functions --- */

/* add file/files to a pack */
void add_files(char *args, char *rest)
{
	char *thefile = NULL, *expand = NULL, *path = NULL, *pptr;
	char *temp = NULL, *filebuf = NULL;
	char *fptr = NULL, *f_path = NULL;

	DIR *dptr;
	struct dirent *dir;
	struct stat statbuf;

	pptr = path = m_strdup(rest);
	temp = new_malloc(BIG_BUFFER_SIZE + 1);
		
	newpack = (pack *) new_malloc(sizeof(pack));

	while((thefile = next_arg(path, &path)))
	{
		if ((fptr = strrchr(thefile, '/')))
		{
			*fptr++ = '\0';
			f_path = m_strdup(thefile);
		}
		else 
		{
			fptr = thefile;
			f_path = m_strdup("~");
		}
		expand = expand_twiddle(f_path);
		dptr = opendir(expand);
		new_free(&f_path);
		if (!dptr) {
			put_it("%s: you cannot access dir %s",  cparse(get_string_var(CDCC_PROMPT_VAR)),expand);
			new_free(&expand);
			new_free(&pptr); /* path free */
			new_free(&filebuf);
			new_free(&temp); 
			new_free((char **)&newpack);
			return;
		}
		while ((dir = readdir(dptr))) {
			if (!dir->d_ino || !match(fptr, dir->d_name))
				continue;
			sprintf(temp, "%s/%s", expand, dir->d_name);
			if (filebuf)
				malloc_strcat(&filebuf, " ");
			malloc_strcat(&filebuf, temp);
			stat(temp, &statbuf);
			if (S_ISDIR(statbuf.st_mode))
				continue;
			newpack->size += statbuf.st_size;
			newpack->numfiles++;
			total_size_of_packs += statbuf.st_size;
		}
	}

	if (!newpack->numfiles) {
		put_it("%s: no files found, aborting...",  cparse(get_string_var(CDCC_PROMPT_VAR)));
		new_free(&expand);
		new_free(&pptr);
		new_free(&filebuf);
		new_free(&temp); 
		new_free((char **) &newpack);
		return;
	}
	
	newpack->file = new_malloc(strlen(filebuf) + 1);
	newpack->num = ++numpacks;
	set_int_var(_CDCC_PACKS_OFFERED_VAR, numpacks);
	strcpy(newpack->file, filebuf);
	malloc_sprintf(&temp, "Description of pack #%d : ", numpacks);
	add_wait_prompt(temp, add_desc, "", WAIT_PROMPT_LINE);
	new_free(&expand);
	new_free(&filebuf);
	new_free(&pptr);
	new_free(&temp);
	
	return;
}

/* add a notes type description to the pack */
void add_note(char *args, char *rest)
{
	if (rest && *rest)
	{
		malloc_strcpy(&newpack->notes, rest);
		put_it("%s: added note to pack #\002%d\002 %s",  cparse(get_string_var(CDCC_PROMPT_VAR)), newpack->num, newpack->notes);
	}
}

/* add a description to the new pack, and add to list */
void add_desc(char *args, char *rest)
{
	pack *ptr, *last = NULL; 
	char size[20];
	char *temp = NULL;
		
	malloc_strcpy(&newpack->desc, rest);

	for (ptr = offerlist; ptr; ptr = ptr->next)
		last = ptr;

	if (last)
		last->next = newpack;
	else
		offerlist = newpack;
	newpack->next = NULL;
			
	if (newpack->size / 1024 > 999)
		sprintf(size, "\002%3.2f\002mb",
		(double) (newpack->size / 1024) / 1024);
	else
		sprintf(size, "\002%3.2f\002kb",
			(double) newpack->size / 1024); 
	put_it("%s: added pack #\002%d\002, \002%d\002 file%s (%s)", cparse(get_string_var(CDCC_PROMPT_VAR)),
		newpack->num, newpack->numfiles,
		plural(newpack->numfiles == 1), size);
	malloc_sprintf(&temp, "Notes for pack #%d : ", numpacks);
	add_wait_prompt(temp, add_note, "", WAIT_PROMPT_LINE);
	new_free(&temp);
	return;
}

/* handle the actual removing of packs / all packs */
void del_pack(char *args, char *rest)
{
	pack *ptr, *last = offerlist;
	int packnum;
	int num = 0;
			
	if (!rest || !*rest)
	{
		put_it("%s: No pack specified for removal", cparse(get_string_var(CDCC_PROMPT_VAR)));
		return;
	}
	if (*rest == '*') {
		numpacks = 0;
		for (ptr = last = offerlist; last;) {
			ptr = last->next;
			new_free(&last->desc);
			new_free(&last->notes);
			new_free(&last->file);
			new_free((char **) &last);
			last = ptr;
		}
		offerlist = NULL;
		total_size_of_packs = 0;
		set_int_var(_CDCC_PACKS_OFFERED_VAR, 0);
		put_it("%s: removed all packs from offer list", cparse(get_string_var(CDCC_PROMPT_VAR)));
		return;
	}
	
	while (rest && *rest)
	{
		packnum = atoi(next_arg(rest, &rest));
		if (packnum <= 0) 
		{
			put_it("%s: invalid pack specification", cparse(get_string_var(CDCC_PROMPT_VAR)));
			continue;
		}

		for (ptr = offerlist; ptr; ptr = ptr->next) 
		{
			if (ptr->num == packnum) 
			{
				if (ptr != offerlist)
					last->next = ptr->next;
				else
					offerlist = ptr->next;

				new_free(&ptr->desc);
				new_free(&ptr->file);
				new_free(&ptr->notes);
				total_size_of_packs -= ptr->size;
				new_free((char **) &ptr);
				numpacks--;
				set_int_var(_CDCC_PACKS_OFFERED_VAR, numpacks);
				put_it("%s: removed pack \002%d\002 from offer list",cparse(get_string_var(CDCC_PROMPT_VAR)), packnum);	
				num++;
				break;
			}
			last = ptr;
		}
	}
	if (num)
	{
		for (ptr = offerlist,num = 1; ptr; ptr = ptr->next)
			ptr->num = num++;
	}
	else
		put_it("%s: pack \002%s\002 does not exist", cparse(get_string_var(CDCC_PROMPT_VAR)),rest); 
	return;
}

/* add a person to the dcc send queue */
int add_to_queue(char *nick, pack *sendpack)
{
	queue *ptr = NULL, *last = NULL, *new = NULL;
	
	new = (queue *) new_malloc(sizeof(queue));
	malloc_strcpy(&new->nick, nick);
	malloc_strcpy(&new->file, sendpack->file);
	malloc_strcpy(&new->desc, sendpack->desc);
	new->time = time(NULL);
	new->numfiles = sendpack->numfiles;
	new->next = NULL;
	sendpack->gets++;
	
	for (ptr = queuelist; ptr; ptr = ptr->next)
		last = ptr;

	if (last)
		last->next = new;
	else
		queuelist = new;
	numqueue++;
	send_to_server("NOTICE %s :\002CDCC\002: all slots full... added you to the queue", nick);
	
	return 0;
}
	
/* check queue & send files... called by irc.c io() */
void dcc_sendfrom_queue(void) 
{
	queue *ptr = queuelist;
	char *dccinfo = NULL, *file = NULL, *q, *temp = NULL;

	if (!ptr)
		return;	
	if (ptr->numfiles > (get_int_var(DCC_SEND_LIMIT_VAR) - dcc_active_count))
		return;	

	put_it("%s: sending \037%s\037 \002%d\002 file%s from queue", cparse(get_string_var(CDCC_PROMPT_VAR)),
		ptr->nick, ptr->numfiles,
		plural(ptr->numfiles));

	malloc_strcpy(&file, ptr->file);
	q = temp = file;
	for (temp = next_arg(file, &file); temp && *temp; temp = next_arg(file, &file)) {
		malloc_sprintf(&dccinfo, "%s %s", ptr->nick, temp);
		dcc_filesend(dccinfo);
	}
	new_free(&dccinfo);

	queuelist = ptr->next;
	new_free(&ptr->nick);
	new_free(&ptr->file);
	new_free((char **) &ptr);
	new_free(&q);
	numqueue--;
	
	return;
}

static time_t plist_last_time = 0;
void get_minspeed(char *args, char *rest)
{
	if (rest && *rest)
		cdcc_minspeed = strtod(rest, NULL);
	if (cdcc_minspeed)
	{
		put_it("%s: Minspeed value to %1.4g KB/s", cparse(get_string_var(CDCC_PROMPT_VAR)),cdcc_minspeed);
		if (!get_int_var(_CDCC_MINSPEED_TIME_VAR))
			put_it("%s: Make sure and /set _CDCC_MINSPEED_TIME as well", cparse(get_string_var(CDCC_PROMPT_VAR)));
	}
	else
		cdcc_minspeed = 0.0;
	
}

int l_minspeed(char *args, char *rest)
{
char *temp = NULL;
	malloc_sprintf(&temp, "%s min-speed (0 to disable): ", cparse(get_string_var(CDCC_PROMPT_VAR)));
	if (args && *args)
		get_minspeed(NULL, args);
	else
		add_wait_prompt(temp, get_minspeed, "", WAIT_PROMPT_LINE);
	new_free(&temp);
	return 0;
}

void get_ptimer(char *args, char *rest)
{
	if (rest && *rest)
		ptimer = strtoul(rest, NULL, 10);
	ptimer *= 60;
	if (ptimer)
		put_it("%s: Ptimer interval %d minutes", cparse(get_string_var(CDCC_PROMPT_VAR)),ptimer/60);
	else
		plist_last_time = 0;
}

int l_timer(char *args, char *rest)
{
char *temp = NULL;
	malloc_sprintf(&temp, "%s p-timer interval(s) (0 to disable): ", cparse(get_string_var(CDCC_PROMPT_VAR)));
	if (args && *args)
		get_ptimer(NULL, args);
	else
		add_wait_prompt(temp, get_ptimer, "", WAIT_PROMPT_LINE);
	new_free(&temp);
	return 0;
}

void get_pchannel(char *args, char *rest)
{
	if (rest && *rest && is_channel(rest))
		malloc_strcpy(&public_channel, rest);
	else if (rest && *rest && *rest == '*')
	{
		int i = get_window_server(0);
		ChannelList *chan;
		if (i != -1)
		{
			new_free(&public_channel);
			for (chan = server_list[i].chan_list; chan; chan = chan->next)
			{
				malloc_strcat(&public_channel, chan->channel);
				if (chan->next)
					malloc_strcat(&public_channel, ",");
			}
		} else
			new_free(&public_channel);
	}
	else
		new_free(&public_channel);
	if (public_channel)
		put_it("%s: Public timer channel(s) are [%s]", cparse(get_string_var(CDCC_PROMPT_VAR)),public_channel);
	else
		put_it("%s: Disabled %s public timer channel(s)", cparse(get_string_var(CDCC_PROMPT_VAR)), cparse(get_string_var(CDCC_PROMPT_VAR)));
}

int l_channel(char *args, char *rest)
{
	if (args && *args)
		get_pchannel(NULL, args);
	else
		if (public_channel)
			put_it("%s: Public timer channel is [%s]", cparse(get_string_var(CDCC_PROMPT_VAR)),public_channel);
		else
			put_it("%s: Disabled %s public timer channel", cparse(get_string_var(CDCC_PROMPT_VAR)), cparse(get_string_var(CDCC_PROMPT_VAR)));
	return 0;
}

void cdcc_timer_offer(void)
{
	time_t timenow = time(NULL);
	if (!offerlist || !ptimer)
		return;
	if (timenow - plist_last_time > ptimer)
	{
		plist_last_time = timenow;
		if (do_notice_list)
			l_notice(public_channel, NULL);
		else
			l_plist(public_channel, NULL);
	}
}

void add_note1(unsigned long pnum, char *note)
{
pack *this_pack = NULL;
int i;
	if (pnum && note)
	{
		for (i = 1, this_pack = offerlist; this_pack; this_pack = this_pack->next, i++)
			if (i == pnum)
				break;
		if (this_pack)
			malloc_strcpy(&this_pack->notes, note);
		else
			put_it("%s: Invalid pack number %d", cparse(get_string_var(CDCC_PROMPT_VAR)), pnum);

	}
	else
		put_it("%s: Invalid pack number %d", cparse(get_string_var(CDCC_PROMPT_VAR)), pnum);
}

static unsigned long got_pnum = 0;

void get_pnote1(char *args, char *rest)
{
	if (got_pnum && rest)
		add_note1(got_pnum, rest);
	got_pnum = 0;
}

void get_pnote(char *args, char *rest)
{
unsigned long pnum = 0;
char *temp = NULL;
char *p;
	if ((p = next_arg(rest, &rest)))
		pnum = strtoul(p, NULL, 10);
	if (rest && *rest)
		add_note1(pnum, rest);
	else
	{
		got_pnum = pnum;
		malloc_sprintf(&temp, "%s note to add to pack %d ", cparse(get_string_var(CDCC_PROMPT_VAR)), pnum);
		add_wait_prompt(temp, get_pnote1, "", WAIT_PROMPT_LINE);	
		new_free(&temp);
	}
}

int l_note(char *args, char *rest)
{
char *temp = NULL;
	if (!offerlist)
		return 0;
	malloc_sprintf(&temp, "%s add note to pack # ", cparse(get_string_var(CDCC_PROMPT_VAR)));
	if (args && *args)
		get_pnote(NULL, args);
	else
		add_wait_prompt(temp, get_pnote, "", WAIT_PROMPT_LINE);
	new_free(&temp);
	return 1;
}

int l_type(char *args, char *rest)
{
	do_notice_list = do_notice_list ? 0 : 1;
	put_it("%s: type of output set to [%s]", cparse(get_string_var(CDCC_PROMPT_VAR)), do_notice_list? "Notice":"privmsg");	
	return 0;
}

int l_echo(char *args, char *rest)
{
	do_cdcc_echo = do_cdcc_echo ? 0 : 1;
	put_it("%s: local echo set to [%s]", cparse(get_string_var(CDCC_PROMPT_VAR)), on_off(do_cdcc_echo));
	return 0;
}
