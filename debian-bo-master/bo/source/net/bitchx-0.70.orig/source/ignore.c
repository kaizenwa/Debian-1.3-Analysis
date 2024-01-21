/*
 * ignore.c: handles the ingore command for irc 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#include "ignore.h"
#include "ircaux.h"
#include "list.h"
#include "input.h"
#include "screen.h"
#include "misc.h"
#include "vars.h"
#include "output.h"

#define NUMBER_OF_IGNORE_LEVELS 9

int	ignore_usernames = 0;
char	*highlight_char = NULL;

static	int	remove_ignore _((char *));
static	void	ignore_list _((char *));
	void	ignore_nickname _((char *, int, int));
static char *cut_n_fix_glob _(( char * ));

/* ignored_nicks: pointer to the head of the ignore list */
Ignore *ignored_nicks = NULL;

#define IGNORE_REMOVE 1
#define IGNORE_DONT 2
#define IGNORE_HIGH -1


/*
 * ignore_nickname: adds nick to the ignore list, using type as the type of
 * ignorance to take place.  
 */
void ignore_nickname(char *nick, int type, int flag)
{
	Ignore	*new, *newc;
	char	*msg,
		*ptr;
	char *new_nick = NULL;
	char buffer[BIG_BUFFER_SIZE + 1];
	int count;
	
	while (nick)
	{
		if ((ptr = index(nick, ',')) != NULL)
			*ptr = '\0';

		if (*nick)
		{
			new_nick = is_channel(nick) ? m_strdup(nick) : cut_n_fix_glob(nick);

			if (!(new = (Ignore *) list_lookup((List **) &ignored_nicks, new_nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)))
			{
				if (flag == IGNORE_REMOVE)
				{
					say("%s is not on the ignorance list", nick);
					if (ptr)
						*(ptr++) = ',';
					nick = ptr;
					continue;
				}
				else
				{
					if ((new = (Ignore *) remove_from_list((List **) &ignored_nicks, nick)) != NULL)
					{
						new_free(&(new->nick));
						new_free((char **)&new);
					}
					new = (Ignore *) new_malloc(sizeof(Ignore));
					new->nick = new_nick;
					add_to_list((List **) &ignored_nicks, (List *) new);
				}
				for (newc = ignored_nicks, count = 1; newc; newc = newc->next, count++)
					newc->num = count;
			}
			switch (flag)
			{
				case IGNORE_REMOVE:
					new->type &= (~type);
					new->high &= (~type);
					new->dont &= (~type);
					msg = "Not ignoring";
					break;
				case IGNORE_DONT:
					new->dont |= type;
					new->type &= (~type);
					new->high &= (~type);
					msg = "Never ignoring";
					break;
				case IGNORE_HIGH:
					new->high |= type;
					new->type &= (~type);
					new->dont &= (~type);
					msg = "Highlighting";
					break;
				default:
					new->type |= type;
					new->high &= (~type);
					new->dont &= (~type);
					msg = "Ignoring";
					break;
			}
			if (type == IGNORE_ALL)
			{
				switch (flag)
				{
				case IGNORE_REMOVE:
					say("%s removed from ignorance list", new->nick);
					remove_ignore(new->nick);
					break;
				case IGNORE_HIGH:
				    say("Highlighting ALL messages from %s", new->nick);
					break;
				case IGNORE_DONT:
				    say("Never ignoring messages from %s", new->nick);
					break;
				default:
				    say("Ignoring ALL messages from %s", new->nick);
					break;
				}
				return;
			}
			else if (type)
			{
				strcpy(buffer, msg);
				if (type & IGNORE_MSGS)
					strcat(buffer, " MSGS");
				if (type & IGNORE_PUBLIC)
					strcat(buffer, " PUBLIC");
				if (type & IGNORE_WALLS)
					strcat(buffer, " WALLS");
				if (type & IGNORE_WALLOPS)
					strcat(buffer, " WALLOPS");
				if (type & IGNORE_INVITES)
					strcat(buffer, " INVITES");
				if (type & IGNORE_NOTICES)
					strcat(buffer, " NOTICES");
				if (type & IGNORE_NOTES)
					strcat(buffer, " NOTES");
				if (type & IGNORE_CTCPS)
					strcat(buffer, " CTCPS");
				if (type & IGNORE_CRAP)
					strcat(buffer, " CRAP");
				if (type & IGNORE_CDCC)
					strcat(buffer, " CDCC");
				say("%s from %s", buffer, new->nick);
			}
		}
		if (ptr)
			*(ptr++) = ',';
		nick = ptr;
	}
}

/*
 * remove_ignore: removes the given nick from the ignore list and returns 0.
 * If the nick wasn't in the ignore list to begin with, 1 is returned. 
 */
static	int remove_ignore(char *nick)
{
	Ignore	*tmp;
	char *new_nick = NULL;

	new_nick = is_channel(nick) ? m_strdup(nick) : cut_n_fix_glob(nick);
	
	if ((tmp = (Ignore *) list_lookup((List **) &ignored_nicks, new_nick, !USE_WILDCARDS, REMOVE_FROM_LIST)) != NULL)
	{
		new_free(&(tmp->nick));
		new_free((char **)&tmp);
		new_free(&new_nick);
		return (0);
	}
	new_free(&new_nick);
	return (1);
}

/*
 * is_ignored: checks to see if nick is being ignored (poor nick).  Checks
 * against type to see if ignorance is to take place.  If nick is marked as
 * IGNORE_ALL or ignorace types match, 1 is returned, otherwise 0 is
 * returned.  
 */
int is_ignored(char *nick, int type)
{
	Ignore	*tmp;

	if (ignored_nicks)
	{
		if ((tmp = (Ignore *) list_lookup((List **) &ignored_nicks, nick, USE_WILDCARDS, !REMOVE_FROM_LIST)) != NULL)
		{
			if (tmp->dont & type)
				return (DONT_IGNORE);
			if (tmp->type & type)
				return (IGNORED);
			if (tmp->high & type)
				return (HIGHLIGHTED);
		}
	}
	return (0);
}

/* ignore_list: shows the entired ignorance list */
static	void ignore_list(char *nick)
{
	Ignore	*tmp;
	int	len = 0;
	char buffer[BIG_BUFFER_SIZE + 1];
	if (ignored_nicks)
	{
		say("Ignorance List:");
		if (nick)
			len = strlen(nick);
		for (tmp = ignored_nicks; tmp; tmp = tmp->next)
		{

			if (nick)
			{
				if (strncmp(nick, tmp->nick, len))
					continue;
			}
			*buffer = '\0';
			if (tmp->type == IGNORE_ALL)
				strmcat(buffer," ALL",BIG_BUFFER_SIZE);
			else if (tmp->high == IGNORE_ALL)
				sprintf(buffer, " %sALL%s", highlight_char, highlight_char);
			else if (tmp->dont == IGNORE_ALL)
				strmcat(buffer," DONT-ALL", BIG_BUFFER_SIZE);
			else
			{
	if (tmp->type & IGNORE_PUBLIC)
		strmcat(buffer, " PUBLIC", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_PUBLIC)
		sprintf(buffer, " %sPUBLIC%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_PUBLIC)
		strmcat(buffer, " DONT-PUBLIC", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_MSGS)
		strmcat(buffer, " MSGS", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_MSGS)
		sprintf(buffer, " %sMSGS%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_MSGS)
		strmcat(buffer, " DONT-MSGS", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_WALLS)
		strmcat(buffer, " WALLS", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_WALLS)
		sprintf(buffer, " %sWALLS%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_WALLS)
		strmcat(buffer, " DONT-WALLS", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_WALLOPS)
		strmcat(buffer, " WALLOPS", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_WALLOPS)
		sprintf(buffer, " %sWALLOPS%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_WALLOPS)
		strmcat(buffer, " DONT-WALLOPS", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_INVITES)
		strmcat(buffer, " INVITES", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_INVITES)
		sprintf(buffer, " %sINVITES%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_INVITES)
		strmcat(buffer, " DONT-INVITES", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_NOTICES)
		strmcat(buffer, " NOTICES", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_NOTICES)
		sprintf(buffer, " %sNOTICES%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_NOTICES)
		strmcat(buffer, " DONT-NOTICES", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_NOTES)
		strmcat(buffer, " NOTES", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_NOTES)
		sprintf(buffer, " %sNOTES%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_NOTES)
		strmcat(buffer, " DONT-NOTES", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_CTCPS)
		strmcat(buffer, " CTCPS", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_CTCPS)
		sprintf(buffer, " %sCTCPS%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_CTCPS)
		strmcat(buffer, " DONT-CTCPS", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_CRAP)
		strmcat(buffer, " CRAP", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_CRAP)
		sprintf(buffer, " %sCRAP%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_CRAP)
		strmcat(buffer, " DONT-CRAP", BIG_BUFFER_SIZE);

	if (tmp->type & IGNORE_CDCC)
		strmcat(buffer, " CDCC", BIG_BUFFER_SIZE);
	else if (tmp->high & IGNORE_CDCC)
		sprintf(buffer, " %sCDCC%s", highlight_char, highlight_char);
	else if (tmp->dont & IGNORE_CDCC)
		strmcat(buffer, " DONT-CDCC", BIG_BUFFER_SIZE);
			}
			put_it("%s", convert_output_format("  %K[%G$[-2]0%K] %C$[-25]1%W:%n $2-", "%d %s %s", tmp->num, tmp->nick, buffer));
		}
	}
	else
		bitchsay("There are no nicknames or channels being ignored");
}

/*
 * ignore: does the /IGNORE command.  Figures out what type of ignoring the
 * user wants to do and calls the proper ignorance command to do it. 
 */
void ignore(char *command, char *args, char *subargs)
{
	char	*nick,
		*type;
	int	len;
	int	flag,
		no_flags;

	if ((nick = next_arg(args, &args)) != NULL)
	{
		no_flags = 1;
		while ((type = next_arg(args, &args)) != NULL)
		{
			no_flags = 0;
			upper(type);
			switch (*type)
			{
			case '^':
				flag = IGNORE_DONT;
				type++;
				break;
			case '-':
				flag = IGNORE_REMOVE;
				type++;
				break;
			case '+':
				flag = IGNORE_HIGH;
				type++;
				break;
			default:
				flag = 0;
				break;
			}
			if ((len = strlen(type)) == 0)
			{
				say("You must specify one of the following:");
				say("\tALL MSGS PUBLIC WALLS WALLOPS INVITES \
NOTICES NOTES CTCPS CDCC CRAP NONE");
				return;
			}
			if (strncmp(type, "ALL", len) == 0)
				ignore_nickname(nick, IGNORE_ALL, flag);
			else if (strncmp(type, "MSGS", len) == 0)
				ignore_nickname(nick, IGNORE_MSGS, flag);
			else if (strncmp(type, "PUBLIC", len) == 0)
				ignore_nickname(nick, IGNORE_PUBLIC, flag);
			else if (strncmp(type, "WALLS", len) == 0)
				ignore_nickname(nick, IGNORE_WALLS, flag);
			else if (strncmp(type, "WALLOPS", len) == 0)
				ignore_nickname(nick, IGNORE_WALLOPS, flag);
			else if (strncmp(type, "INVITES", len) == 0)
				ignore_nickname(nick, IGNORE_INVITES, flag);
			else if (strncmp(type, "NOTICES", len) == 0)
				ignore_nickname(nick, IGNORE_NOTICES, flag);
			else if (strncmp(type, "NOTES", len) == 0)
				ignore_nickname(nick, IGNORE_NOTES, flag);
			else if (strncmp(type, "CTCPS", len) == 0)
				ignore_nickname(nick, IGNORE_CTCPS, flag);
			else if (strncmp(type, "CDCC", len) == 0)
				ignore_nickname(nick, IGNORE_CDCC, flag);
			else if (strncmp(type, "CRAP", len) == 0)
				ignore_nickname(nick, IGNORE_CRAP, flag);
			else if (strncmp(type, "NONE", len) == 0)
			{
				char	*ptr;

				while (nick)
				{
					if ((ptr = index(nick, ',')) != NULL)
						*ptr = '\0';
					if (*nick)
					{
						if (remove_ignore(nick))
							bitchsay("%s is not in the ignorance list!", nick);
						else
							bitchsay("%s removed from ignorance list", nick);
					}
					if (ptr)
						*(ptr++) = ',';
					nick = ptr;
				}
			}
			else
			{
				bitchsay("You must specify one of the following:");
				say("\tALL MSGS PUBLIC WALLS WALLOPS INVITES \
NOTICES NOTES CTCPS CDCC CRAP NONE");
			}
		}
		if (no_flags)
			ignore_list(nick);
	} else
		ignore_list(NULL);
}

/*
 * set_highlight_char: what the name says..  the character to use
 * for highlighting..  either BOLD, INVERSE, or UNDERLINE..
 */
void set_highlight_char(Window *win, char *s, int unused)
{
	int	len = strlen(s);

	if (!my_strnicmp(s, "BOLD", len))
		malloc_strcpy(&highlight_char, BOLD_TOG_STR);
	else if (!my_strnicmp(s, "INVERSE", len))
		malloc_strcpy(&highlight_char, REV_TOG_STR);
	else if (!my_strnicmp(s, "UNDERLINE", len))
		malloc_strcpy(&highlight_char, UND_TOG_STR);
	else
		malloc_strcpy(&highlight_char, s);
}

/* check_ignore -- replaces the old double_ignore
 *   Why did i change the name?
 *      * double_ignore isnt really applicable any more becuase it doesnt
 *        do two ignore lookups, it only does one.
 *      * This function doesnt look anything like the old double_ignore
 *      * This function works for the new *!*@* patterns stored by
 *        ignore instead of the old nick and userhost patterns.
 * (jfn may 1995)
 */
int	check_ignore (char *nick, char *userhost, char *channel, int type)
{
	char *nickuserhost = NULL;
	Ignore	*tmp;

	malloc_sprintf(&nickuserhost, "%s!%s", nick ? nick : "*", userhost ? userhost : "*");

	if (ignored_nicks)
	{
		if ((tmp = (Ignore *) list_lookup((List **)&ignored_nicks, nickuserhost,
				USE_WILDCARDS, !REMOVE_FROM_LIST)))
		{
			new_free(&nickuserhost); /* so i did forget to free
							this! >;-) */
			if (tmp->dont & type)
				return(DONT_IGNORE);
			if (tmp->type & type)
				return (IGNORED);
			if (tmp->high & type)
				return (HIGHLIGHTED);
		}
		new_free(&nickuserhost);
		if (channel && is_channel(channel) && 
			(tmp = (Ignore *) list_lookup((List **)&ignored_nicks, channel, USE_WILDCARDS, !REMOVE_FROM_LIST)))
		{
			if (tmp->dont & type)
				return(DONT_IGNORE);
			if (tmp->type & type)
				return (IGNORED);
			if (tmp->high & type)
				return (HIGHLIGHTED);
		}
	}
	new_free(&nickuserhost);
	return (DONT_IGNORE);
}

int ignore_combo(int flag1, int flag2)
{
        if (flag1 == DONT_IGNORE || flag2 == DONT_IGNORE)
                return DONT_IGNORE;
        if (flag1 == IGNORED || flag2 == IGNORED)
                return IGNORED;
        if (flag1 == HIGHLIGHTED || flag2 == HIGHLIGHTED)
                return HIGHLIGHTED;
        return 0;
}

/* Written by hop in April 1995 -- taken from SIRP */
/* MALLOCED */
static char *cut_n_fix_glob ( char *nickuserhost )
{
	char *nick, *userhost = (char *) 0,
	     *user = (char *) 0, *host = (char *) 0;
	char *final_stuff;
	char	*copy = NULL;
	
	/* patch by texaco makes this work right */
	copy = m_strdup(nickuserhost);
	nick = copy;

	if ((userhost = index(copy, '!')))
	{
		/* NICK IS CORRECT HERE */

		*userhost++ = 0;

		/* doh! */
		user = userhost;

		if ((host = sindex(userhost, "@")))
			/* USER IS CORRECT HERE */
			*host++ = 0;

		else if (sindex(userhost, "."))
		{
			user = "*";
			host = userhost;
		}
		/* fixed by sheik */
		if (!user)
			user = "*";
		if (!host)
			host = "*";
	}
	else
	{
		user = copy;
		if ((host = sindex(user, "@")))
		{
			nick = "*";
			*host++ = 0;
		}
		else
		{
			if (sindex(user, "."))
			{
				nick = "*";
				host = user;
				user = "*";
			}
			else
			{
				nick = user;
				user = "*";
				host = "*";
			}
		}
	}
	final_stuff = m_opendup(nick, "!", user, "@", host, NULL);
	new_free(&copy);
	return final_stuff;
}

void tremove_ignore _((char *stuff, char *line))
{
int count = 0;
Ignore *new, *newc;
char *p;
	if (!line)
		return;
	while ((p = next_arg(line, &line)))
	{
		for (new = ignored_nicks; new; new = newc)
		{
			newc = new->next;
			if (matchmcommand(p, new->num) || !my_stricmp(new->nick, p))
			{
				remove_ignore(new->nick);
				count++;
			}
		}
	}
	if (count)
		bitchsay("Removed %d ignores", count);
	else
		bitchsay("No matching ignorance");
	for (new = ignored_nicks, count = 1; new; new = new->next, count++)
		new->num = count;
}

void tignore _((char *command, char *args, char *subargs))
{
	ignore_list(NULL);
	if (ignored_nicks)
		if (args && *args)
			tremove_ignore(NULL, args);
		else
			add_wait_prompt("Which ignore to delete (-2, 2-5, ...) ? ", tremove_ignore, args, WAIT_PROMPT_LINE);
}