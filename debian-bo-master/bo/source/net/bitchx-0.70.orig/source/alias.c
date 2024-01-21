/*
 * alias.c Handles command aliases for irc.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990, 1995 Michael Sandroff and others 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#include "irc.h"
#include "alias.h"
#include "array.h"
#include "dcc.h"
#include "edit.h"
#include "files.h"
#include "history.h"
#include "hook.h"
#include "input.h"
#include "ircaux.h"
#include "names.h"
#include "notify.h"
#include "numbers.h"
#include "output.h"
#include "parse.h"
#include "screen.h"
#include "server.h"
#include "status.h"
#include "vars.h"
#include "window.h"
#include "ircterm.h"
#include "server.h"
#include "struct.h"
#include "list.h"
#include "keys.h"
#include "bot.h"
#include "userlist.h"
#include "misc.h"
#include "newio.h"
#include "stack.h"
#include <sys/stat.h>

/* XXXX */
#ifndef MAXPATHLEN
#define MAXPATHLEN PATHSIZE
#endif


extern int start_time;

#ifdef WANT_DLL
BuiltInDllFunctions *dll_functions = NULL;
#endif

/* alias_illegals: characters that are illegal in alias names */
	char	alias_illegals[] = " #+-*/\\()={}[]<>!@$%^~`,?;:|'\"";

Alias	*alias_list[] =
{
	NULL, NULL, NULL, NULL
};

	Alias **find_local_variable 	(char *, Alias **, int);
	void 	add_local_variable 	(Alias **, Alias *);
static	void 	delete_alias		(int type, char *name);
static	void	do_alias_string 	(char, char *);
static	void	TruncateAndQuote	(char **, char *, int, char *, char);
static  void	add_stub 		(int, char *, char *);
static  void 	list_aliases		(int, char *);
static	void 	destroy_aliases 	(Alias **list);
static	char	*next_unit _((char *, char *, int *, int));
static	char	*find_inline_with_args (char *, char *, int *);

/* alias_string: the thing that gets replaced by the $"..." construct */
static	char	*alias_string = NULL;

/* function_stack and function_stkptr - hold the return values from functions */
static	char	*function_stack[128] = { NULL };
static	int	function_stkptr = 0;
	char *next_arg _((char *, char **));

/*
 * aliascmd: The user interface for /ALIAS, /ASSIGN, /LOCAL, and /STUB
 */
void	aliascmd	(char *command, char *args, char *subargs)
{
	char	*name = NULL,
		*rest = NULL;
	int	type = -1;
	int	stub = 0;


	/*
	 * Is it ASSIGN, ALIAS, STUB, or LOCAL ?
	 */
	if (isdigit(*command))
		type = *command - 48;

	else if (!strcmp(command, "LOCAL"))
		type = VAR_ALIAS_LOCAL;

	else if (!strcmp(command, "STUB"))
	{
		stub = 1;
		command = next_arg(args, &args);

		if (command)
		{
			if (!my_stricmp(command, "ALIAS"))
				type = COMMAND_ALIAS;
			else if (!my_stricmp(command, "ASSIGN"))
				type = VAR_ALIAS;
		}

		if (type == -1)
		{
			yell("Usage: /STUB (ALIAS|ASSIGN) alias-name filename");
			return;
		}
	}

	/*
	 * What is the alias name?
	 */
	if (!((name = next_arg(args, &rest)) != NULL))
	{
		list_aliases(type, NULL);
		return;
	}

	/*
	 * Now we get the body of the alias.
	 * Check to see if there is a "body".  Handle if not.
	 */
	while (my_isspace(*rest))
		rest++;

	/*
	 * Now we see if its an alias surrounded by braces
	 */
	if (type == COMMAND_ALIAS && *rest == LEFT_BRACE)
	{
		char	*ptr = MatchingBracket(++rest, LEFT_BRACE, RIGHT_BRACE);

		if (!ptr)
			say("Unmatched brace in %s", command);
		else 
		{
			*ptr++ = 0;
			while (*ptr && my_isspace(*ptr))
				ptr++;

			if (*ptr)
				say("Junk after closing brace in %s", command);

			while (*rest && my_isspace(*rest))
				rest++;

		}
	}

	while (name && *name)
	{
		char	*ArrayIndex;
		char	*EndIndex;
		char 	*next_name = NULL;

		if ((next_name = index(name, ',')))
			*next_name++ = 0;

		/*
		 * We cant call remove_brackets() here, because the user
		 * has already expanded any expandoes.. *shrug*.
		 */
		while ((ArrayIndex = (char *) index(name, '[')) != NULL)
		{
			*ArrayIndex++ = '.';
			if ((EndIndex = MatchingBracket(ArrayIndex, LEFT_BRACKET, RIGHT_BRACKET)) != NULL)
			{
				*EndIndex++ = '\0';
				strcat(name, EndIndex);
			}
			else
				break;
		}

		/*
		 * Must want to delete aliases
		 */
		if (!rest || !*rest)
		{
			/*
			 * Can remove an alias by assigning to it prepended with
			 * a hyphen (eg, /assign -foobar
			 */
			if (name[0] == '-')
			{
				if (name[1])
					delete_alias(type, name + 1);
				else
					say("You must specify an alias to be removed.");

				name = next_name;
				continue;
			}
			else if (type != VAR_ALIAS_LOCAL)
			{
				list_aliases(type, name);

				name = next_name;
				continue;
			}
		}

		if (stub)
			add_stub(type, name, rest);
		else
			add_alias(type, name, rest);

		name = next_name;
	}
}


/*
 * find_alias: looks up name in in alias list.  Returns the Alias entry if
 * found, or null if not found.   If unlink is set, the found entry is
 * removed from the list as well.  If match is null, only perfect matches
 * will return anything.  Otherwise, the number of matches will be returned. 
 *
 * The argument ``name'' must already be in canonical ``dot'' format.
 * That means you must have already called remove_brackets() on it.
 * Otherwise, you wont get anywhere.
 */
Alias 	*find_alias 	(Alias **list, char *name, int unlink, int *match, int unstub)
{
	Alias	*tmp,
		*last = NULL;
	int	cmp,
		len;
	int 	(*cmp_func) ();

	if (match)
	{
		*match = 0;
		cmp_func = strncmp;
	}
	else
		cmp_func = strcmp;

	if (name)
	{
		/*
		 * We dont call remove_brackets() here becuase we dont
		 * need to any more.
		 */

		len = strlen(name);
		for (tmp = *list; tmp; tmp = tmp->next)
		{
			if ((cmp = cmp_func(name, tmp->name, len)) == 0)
			{
				static already_looking = 0;

				if (tmp->stub && unstub)
				{
					/* 
					 * If already_looking is 1, then
					 * we are un-stubbing this alias
					 * because we are loading a file
					 * that presumably it must be in.
					 * So we dont load it again (duh).
					 */
					if (!already_looking)
					{
						already_looking = 1;
						load("LOAD", tmp->stub, empty_string);
						already_looking = 0;
					}

					/*
					 * Clean up the stub (its been loaded)
					 */
					new_free(&(tmp->stub));

					/*
					 * If the user didnt assign the alias
					 * in the loaded file, then there isnt
					 * any need for it to stick around.
					 */
					if (!tmp->stuff)
						unlink = 1;
				}

				if (unlink)
				{
					if (last)
						last->next = tmp->next;
					else
						*list = tmp->next;
				}
				if (match)
				{
					(*match)++;
					if (strlen(tmp->name) == len)
					{
						*match = 0;
						return (tmp);
					}
				}
				else
					return (tmp);
			}
			else if (cmp < 0)
				break;
			last = tmp;
		}
	}

	if (match && (*match == 1))
		return (last);
	else
		return (NULL);
}

/*
 * insert_alias: adds the given alias to the alias list.  The alias list is
 * alphabetized by name 
 */
extern	void	insert_alias (Alias **list, Alias *alias)
{
	Alias	*tmp,
		*last,
		*foo;

	last = NULL;
	for (tmp = *list; tmp; tmp = tmp->next)
	{
		if (strcmp(alias->name, tmp->name) < 0)
			break;
		last = tmp;
	}
	if (last)
	{
		foo = last->next;
		last->next = alias;
		alias->next = foo;
	}
	else
	{
		alias->next = *list;
		*list = alias;
	}
}

/*
 * add_alias: given the alias name and "stuff" that makes up the alias,
 * add_alias() first checks to see if the alias name is already in use... if
 * so, the old alias is replaced with the new one.  If the alias is not
 * already in use, it is added. 
 */
extern void	add_alias (int type, char *name, char *stuff)
{
	Alias	**list = NULL;
	Alias	*tmp = NULL;
	char	*ptr = NULL;
	int	af;
	int	global = 0;
		
	upper(name);
	name = remove_brackets(name, NULL, &af);
	
	if (name[0] == ':' && name[1] == ':')
		name++, name++, global = 1, type = VAR_ALIAS;

	if ((type == VAR_ALIAS || type == VAR_ALIAS_LOCAL) && (ptr = sindex(name, alias_illegals)) != NULL)
	{
		yell("ASSIGN names may not contain '%c'", *ptr);
		new_free(&name);
		return;
	}

	/* FUNCTION_RETURN must die die die. */
	if ((type == VAR_ALIAS || type == VAR_ALIAS_LOCAL) && !strcmp(name, "FUNCTION_RETURN"))
	{
		if (function_stack[function_stkptr])
			new_free(&function_stack[function_stkptr]);
		malloc_strcpy(&function_stack[function_stkptr], stuff);
		new_free(&name);
		return;
	}
	/*
	 * This is pretty straightforward:
	 *
	 *  If this is a variable reference
	 *    Then look to see if there is a local variable by that name
	 *	if there is, then use the local variable (replace it)
	 *	if there is not,
	 *	  if this is not expliclity a local variable
	 *	    look for a global variable
	 *
	 * (What it amounts to is VAR_ALIAS looks for a local variable, and
	 * not finding one looks for a global variable.  VAR_ALIAS_LOCAL
	 * always looks for a local variable, never global.
	 */
	if ((type == VAR_ALIAS && !global) || type == VAR_ALIAS_LOCAL)
	{
		list = find_local_variable(name, &tmp, 1);
		if (list)
			type = VAR_ALIAS_LOCAL;
		else if (type != VAR_ALIAS_LOCAL)
			tmp = find_alias(&alias_list[type], name, 1, NULL, 0);
	}
	else
		tmp = find_alias(&alias_list[type], name, 1, NULL, 0);

	/* 
	 * Looks around -- Hmmm. noone's looking... Ill sneak this in
	 * here and hope noone notices. >;-)   This fixes a whole world
	 * of problems...  Of course we have to make sure we dont
	 * do this for FUNCTION_RETURN. *sigh*
 	 *
	 * This has to be done here, because we have to know whether
	 * 'name' is a local variable and so we have to wait until
	 * 'type' is set to a correct value!
	 */
	if (tmp)
	{
		if ((type != VAR_ALIAS_LOCAL) && (!stuff || !*stuff))
		{
			delete_alias(type, name);
			return;
		}
	}
	else
	{
		tmp = (Alias *) new_malloc(sizeof(Alias));
		bzero(tmp, sizeof(Alias));
	}

	malloc_strcpy(&(tmp->name), name);
	malloc_strcpy(&(tmp->stuff), stuff);
	new_free(&tmp->stub);
	tmp->mark = 0;
	tmp->global = loading_global;

	if (type == VAR_ALIAS_LOCAL)
		add_local_variable(list, tmp);
	else
		insert_alias(&(alias_list[type]), tmp);

	if (type == COMMAND_ALIAS)
		say("Alias	%s added [%s]", name, stuff);
	else if (type == VAR_ALIAS)
		say("Assign %s added [%s]", name, stuff);
	else
		say("Assign %s (local) added [%s]", name, stuff);

	new_free(&name);
}

/*
 * add_stub: You give it an alias name and a filename, and it generates
 * a stub for that alias.  If an alias is already in place for that name,
 * it is duly clobbered.  If it isnt already in place, this makes a new one.
 */
static void	add_stub (int type, char *name, char *file)
{
	Alias	*tmp;
	char	*ptr;

	upper(name);
	if (type == COMMAND_ALIAS)
put_it("%s", convert_output_format(get_string_var(FORMAT_ALIAS_VAR), "%s %s", name, file));
	else
	{
		if (!strcmp(name, "FUNCTION_RETURN"))
		{
			yell("You cannot stub the FUNCTION_RETURN variable");
			return;
		}
		if ((ptr = sindex(name, alias_illegals)) != NULL)
		{
			yell("Assign names may not contain '%c'", *ptr);
			return;
		}
		if (type == VAR_ALIAS_LOCAL)
		{
			yell("You may not stub local variables");
			return;
		}
put_it("%s", convert_output_format(get_string_var(FORMAT_ASSIGN_VAR), "%s %s", name, file));
	}
	if ((tmp = find_alias(&(alias_list[type]), name, 1, NULL, 0)) == NULL)
	{
		tmp = (Alias *)new_malloc(sizeof(Alias));
		memset(tmp, 0, sizeof(Alias));


	}
	malloc_strcpy(&(tmp->name), name);
	malloc_strcpy(&(tmp->stub), file);
	new_free(&(tmp->stuff));
	tmp->stuff = NULL;
	tmp->mark = 0;
	tmp->global = loading_global;
	insert_alias(&(alias_list[type]), tmp);
}

/*
 * find_inline: This simply looks up the given str.  It first checks to see
 * if its a user variable and returns it if so.  If not, it checks to see if
 * it's an IRC variable and returns it if so.  If not, it checks to see if
 * its and environment variable and returns it if so.  If not, it returns
 * null.  It mallocs the returned string 
 */

char	*find_inline (char *str)
{
	int af;
	return find_inline_with_args(str, NULL, &af);
}

static	char	*find_inline_with_args (char *str, char *args, int *args_flag)
{
	Alias	*alias;
	char	*ret = NULL;
	char	*name = NULL;
	
/*	name = upper(remove_brackets(str, args, args_flag));*/
	name = upper(str);
	if (find_local_variable(str, &alias, 0))
		ret = alias->stuff;
	else if ((alias = find_alias(&(alias_list[VAR_ALIAS]), name, 0, NULL, 1)) != NULL)
		ret = alias->stuff;
	else if ((strlen(str) == 1) && (ret = built_in_alias(*str)))
		return ret;
	else if ((ret = make_string_var(str)))
		;
	else    /* even if there is no environment var we get a empty string
		 * so we can skip the test and fall through as normal
		 */
		ret = getenv(str);

	return m_strdup(ret);
}

static void	do_alias_string (char unused, char *not_used)
{
	malloc_strcpy(&alias_string, get_input());
}

/************************************************************************/
/*
 * call_user_function: Executes a user alias (by way of parse_command.
 * The actual function ends up being routed through execute_alias (below)
 * and we just keep track of the retval and stuff.  I dont know that anyone
 * depends on command completion with functions, so we can save a lot of
 * CPU time by just calling execute_alias() directly.
 */
char 	*call_user_function	(char *alias_name, char *args)
{
	char *result;
	char *sub_buffer;
	int cnt;
	char *full_name = NULL;

	function_stkptr++;
	function_stack[function_stkptr] = NULL;

	sub_buffer = get_alias(COMMAND_ALIAS, alias_name, &cnt, &full_name);
	if (cnt != 1)
		execute_alias(full_name, sub_buffer, args);

	result = function_stack[function_stkptr];
	function_stack[function_stkptr] = NULL;
	if (!result)
		malloc_strcpy(&result, empty_string);
	function_stkptr--;
	return result;
}


#include "expr.c"

/*
 * get_alias: returns the alias matching 'name' as the function value. 'args'
 * are expanded as needed, etc.  If no matching alias is found, null is
 * returned, cnt is 0, and full_name is null.  If one matching alias is
 * found, it is retuned, with cnt set to 1 and full_name set to the full name
 * of the alias.  If more than 1 match are found, null is returned, cnt is
 * set to the number of matches, and fullname is null. NOTE: get_alias()
 * mallocs the space for the full_name, but returns the actual value of the
 * alias if found! 
 */
extern char	*get_alias(int type, char *name, int *cnt, char **full_name)
{
	Alias	**list;
	Alias	*tmp;

	*full_name = NULL;
	if (!name || !*name)
	{
		*cnt = 0;
		return NULL;
	}
	if (type == VAR_ALIAS_LOCAL)
		list = find_local_variable(name, NULL, 0);
	else
		list = &(alias_list[type]);
		
	if ((tmp = find_alias(list, name, 0, cnt, 1)) != NULL)
	{
		if (*cnt < 2)
		{
			malloc_strcpy(full_name, tmp->name);
			return (tmp->stuff);
		}
	}
	return NULL;
}

/*
 * match_alias: this returns a list of alias names that match the given name.
 * This is used for command completion etc.  Note that the returned array is
 * malloced in this routine.  Returns null if no matches are found 
 */
extern char	**match_alias(char *name, int *cnt, int type)
{
	Alias	*tmp;
	char	**matches = NULL;
	int	matches_size = 5;
	int	len;
	char	*last_match = NULL;
	char	*dot;

	len = strlen(name);
	*cnt = 0;
	matches = (char	**) new_malloc(sizeof(char *) * matches_size);

	if (type == -1)
	{
		matches = get_builtins(name, len, matches, matches_size, cnt);
		if (*cnt)
		{
			matches = (char	**) new_realloc((char *)matches, sizeof(char *) * (*cnt + 1));
			matches[*cnt] = NULL;
		}
		else
			new_free((char **)&matches);
		return (matches);
	}
	if (type == VAR_ALIAS_LOCAL)
		tmp = *find_local_variable(name, NULL, 0);
	else
		tmp = alias_list[type];

	for ( ; tmp; tmp = tmp->next)
	{
		if (strncmp(name, tmp->name, len) == 0)
		{
			if ((dot = (char *) index(tmp->name+len, '.')) != NULL)
			{
				if (type == COMMAND_ALIAS)
					continue;
				else
				{
					*dot = '\0';
					if (last_match && !strcmp(last_match, tmp->name))
					{
						*dot = '.';
						continue;
					}
				}
			}
			matches[*cnt] = NULL;
			malloc_strcpy(&(matches[*cnt]), tmp->name);
			last_match = matches[*cnt];
			if (dot)
				*dot = '.';
			if (++(*cnt) == matches_size)
			{
				matches_size += 5;
				matches = (char	**) new_realloc((char *)matches,
					sizeof(char *) * matches_size);
			}
		}
		else if (*cnt)
			break;
	}
	if (*cnt)
	{
		matches = (char	**) new_realloc((char *)matches, sizeof(char *) * (*cnt + 1));
		matches[*cnt] = NULL;
	}
	else
		new_free((char **)&matches);
	return (matches);
}

/* delete_alias: The alias name is removed from the alias list. */
static void delete_alias(int type, char *name)
{
	Alias	*tmp;
	Alias	**list;
	
	upper(name);

	if (type == VAR_ALIAS_LOCAL)
		list = find_local_variable(name, NULL, 1);
	else
		list = &alias_list[type];

	/* we dont need to unstub it if we're just going to delete it */
	if ((tmp = find_alias(list, name, 1,  NULL, 0)) != NULL)
	{
		new_free(&(tmp->name));
		new_free(&(tmp->stuff));
		new_free((char **)&tmp);
		if (type == COMMAND_ALIAS)
			put_it("%s", convert_output_format(get_string_var(FORMAT_ALIAS_VAR), "%s %s", name, "Removed"));
		else
			put_it("%s", convert_output_format(get_string_var(FORMAT_ASSIGN_VAR), "%s %s", name, "Removed"));
	}
	else
		put_it("%s", convert_output_format(get_string_var(FORMAT_ALIAS_VAR), "%s %s", name, "None"));
}

/*
 * list_aliases: Lists all aliases matching 'name'.  If name is null, all
 * aliases are listed 
 */
static void list_aliases(int type, char *name)
{
	Alias	*tmp;
	Alias	*list;

	int	len;
	int	DotLoc,
		LastDotLoc = 0;
	char	*LastStructName = NULL;
	char	*s;

	if (type == COMMAND_ALIAS)
		say("Aliases:");
	else
		say("Assigns:");

	if (name)
	{
		upper(name);
		len = strlen(name);
	}
	else
		len = 0;

	if (type == VAR_ALIAS_LOCAL)
	{
		Alias **tmp = find_local_variable(name, NULL, 0);
		if (!tmp)
		{
			say("No local variables...");
			return;
		}
		list = *tmp;
	}
	else
		list = alias_list[type];

	for (tmp = list; tmp; tmp = tmp->next)
	{
		if (!name || !strncmp(tmp->name, name, len))
		{
			s = index(tmp->name + len, '.');
			if (!s)
			{
				if (tmp->stub)
					put_it("%s", convert_output_format(get_string_var(FORMAT_ALIAS_VAR), "%s %s Stub", tmp->name, tmp->stub));
				else
				{
					char temp[BIG_BUFFER_SIZE+1];
					strncpy(temp, tmp->stuff, BIG_BUFFER_SIZE-1);
					put_it("%s", convert_output_format(get_string_var(FORMAT_ALIAS_VAR), "%s %s", tmp->name, temp));
				}
			}
			else
			{
				DotLoc = s - tmp->name;
				if (!LastStructName || (DotLoc != LastDotLoc) || strncmp(tmp->name, LastStructName, DotLoc))
				{
					put_it("%s", convert_output_format(get_string_var(FORMAT_ASSIGN_VAR), "%s <Structure>", tmp->name));
					LastStructName = tmp->name;
					LastDotLoc = DotLoc;
				}
			}
		}
	}
}

/*
 * mark_alias: sets the mark field of the given alias to 'flag', and returns
 * the previous value of the mark.  If the name is not found, -1 is returned.
 * This is used to prevent recursive aliases by marking and unmarking
 * aliases, and not reusing an alias that has previously been marked.  I'll
 * explain later 
 */
static int mark_alias(char *name, int flag)
{
	int	old_mark;
	Alias	*tmp;
	int	match;

	if ((tmp = find_alias(&(alias_list[COMMAND_ALIAS]), name, 0, &match, 1)) != NULL)
	{
		if (match < 2)
		{
			old_mark = tmp->mark;
		/* New handling of recursion */
			if (flag)
			{
				int	i;
				/* Count recursion */

				tmp->mark = tmp->mark + flag;
				if ((i = get_int_var(MAX_RECURSIONS_VAR)) > 1)
				{
					if (tmp->mark > i)
					{
						tmp->mark = 0;
						return(1); /* MAX exceeded. */
					}
					else return(0);
				/* In recursion but it's ok */
				}
				else
				{
					if (tmp->mark > 1)
					{
						tmp->mark = 0;
						return(1);
				/* max of 1 here.. exceeded */
					}
					else return(0);
				/* In recursion but it's ok */
				}
			}
			else
		/* Not in recursion at all */
			{
				tmp->mark = 0;
				return(old_mark);
			/* This one gets ignored anyway */
			}
		}
	}
	return (-1);
}

/*
 * execute_alias: After an alias has been identified and expanded, it is sent
 * here for proper execution.  This routine mainly prevents recursive
 * aliasing.  The name is the full name of the alias, and the alias is
 * already expanded alias (both of these parameters are returned by
 * get_alias()) 
 */
extern void execute_alias(char *alias_name, char *alias, char *args)
{
	if (mark_alias(alias_name, 1))
		say("Maximum recursion count exceeded in: %s", alias_name);
	else
	{
		parse_line(alias_name, alias, args, 0,1);
		mark_alias(alias_name, 0);
	}
}

/*
 * save_aliases: This will write all of the aliases to the FILE pointer fp in
 * such a way that they can be read back in using LOAD or the -l switch 
 */
extern void save_aliases(FILE *fp, int do_all)
{
	Alias	*tmp;

	for (tmp = alias_list[VAR_ALIAS]; tmp; tmp = tmp->next)
	{
		if (!tmp->global || do_all)
		{
			if (tmp->stub)
				fprintf(fp, "STUB ASSIGN %s %s\n", tmp->name, tmp->stub);
			else
				fprintf(fp, "ASSIGN %s %s\n", tmp->name, tmp->stuff);
		}
	}
	for (tmp = alias_list[COMMAND_ALIAS]; tmp; tmp = tmp->next)
	{
		if (!tmp->global || do_all)
		{
			if (tmp->stub)
				fprintf(fp, "STUB ALIAS %s %s\n", tmp->name, tmp->stub);
			else
				fprintf(fp, "ALIAS %s %s\n", tmp->name, tmp->stuff);
		}
	}	
}


extern  void	dumpcmd(char *command,char *args, char *subargs)
{
	FILE *fp;
	char *filename = NULL;
	char *expand = NULL;
	char 	*blah;

	if (!args || !*args)
	{
		userage("dump", "<BIND\002|\002VAR\002|\002ALIAS\002|\002ON\002|\002ALL\002|\002FILE>");
		return;
	}

	while ((blah = next_arg(args, &args)))
	{
		if (!my_strnicmp(blah,"FI",2))
		{
			malloc_sprintf(&filename, "~/%s.dump", version);
			expand = expand_twiddle(filename);
			new_free(&filename);
			if ((fp = fopen(expand, "w")) != NULL)
			{
				save_bindings(fp, 0);
				save_hooks(fp, 0);
				save_variables(fp, 0);
				save_aliases(fp, 0);
				fclose(fp);
			}
			bitchsay("Saved to ~/%s.dump", version);
			new_free(&expand);
		}
		else if (!my_strnicmp(blah,"B",1))
			init_keys_1();
		else if (!my_strnicmp(blah,"V",1))			
			destroy_aliases(&alias_list[VAR_ALIAS]);
		else if (!my_strnicmp(blah,"ALI",3))
			destroy_aliases(&alias_list[COMMAND_ALIAS]);
		else if (!my_strnicmp(blah,"O",1))
			flush_on_hooks();
		else if (!my_strnicmp(blah,"ALL",3))
		{
			init_keys_1();
			destroy_aliases(&alias_list[COMMAND_ALIAS]);
			destroy_aliases(&alias_list[VAR_ALIAS]);
			flush_on_hooks();
		}
	}
}

void destroy_aliases (Alias **list)
{
	Alias 	*tmp;
	Alias   *ntmp;

	for (tmp = *list; tmp; )
	{
		new_free(&tmp->stuff);
		new_free(&tmp->name);
		new_free(&tmp->stub);
		ntmp = tmp->next;
		new_free((char **)&tmp);
		tmp = ntmp;
	}
	*list = NULL;
}

/****************************** ALIASCTL ************************************/
#define EMPTY empty_string
#define RETURN_EMPTY return m_strdup(EMPTY)
#define RETURN_IF_EMPTY(x) if (empty( x )) RETURN_EMPTY
#define GET_INT_ARG(x, y) {RETURN_IF_EMPTY(y); x = my_atol(safe_new_next_arg(y, &y));}
#define GET_FLOAT_ARG(x, y) {RETURN_IF_EMPTY(y); x = atof(safe_new_next_arg(y, &y));}
#define GET_STR_ARG(x, y) {RETURN_IF_EMPTY(y); x = new_next_arg(y, &y);RETURN_IF_EMPTY(x);}
#define RETURN_STR(x) return m_strdup(x ? x : EMPTY)
#define RETURN_INT(x) return m_strdup(ltoa(x));

/* Used by function_aliasctl */
char 	*aliasctl 	(char *input)
{
	int list = -1;
	char *listc = NULL;
	enum { GET, SET, MATCH } op;

	GET_STR_ARG(listc, input);
	RETURN_IF_EMPTY(listc);
	if (!my_strnicmp(listc, "AS", 2))
		list = VAR_ALIAS;
	else if (!my_strnicmp(listc, "AL", 2))
		list = COMMAND_ALIAS;
	else if (!my_strnicmp(listc, "LO", 2))
		list = VAR_ALIAS_LOCAL;
	else
		RETURN_EMPTY;

	GET_STR_ARG(listc, input);
	RETURN_IF_EMPTY(listc);
	if (!my_strnicmp(listc, "G", 1))
		op = GET;
	else if (!my_strnicmp(listc, "S", 1))
		op = SET;
	else if (!my_strnicmp(listc, "M", 1))
		op = MATCH;
	else
		RETURN_EMPTY;

	GET_STR_ARG(listc, input);
	RETURN_IF_EMPTY(listc);

	switch (op)
	{
		case (GET) :
		{
			Alias *alias;
			Alias **a_list;

			if (list == VAR_ALIAS_LOCAL)
				a_list = find_local_variable(listc, NULL, 0);
			else
				a_list = &(alias_list[list]);

			if ((alias = find_alias(a_list, listc, 0, NULL, 1)))
				RETURN_STR(alias->stuff);
			else
				RETURN_EMPTY;
		}
		case (SET) :
		{
			add_alias(list, listc, input);
			RETURN_INT(1)
		}
		case (MATCH) :
		{
			char **mlist;
			char *mylist = NULL;
			int num, ctr;

			if (!my_stricmp(listc, "*"))
				listc = empty_string;

			upper(listc);
			mlist = match_alias(listc, &num, list);
			for (ctr = 0; ctr < num; ctr++)
			{
				if (mylist)
					malloc_strcat(&mylist, space);
				malloc_strcat(&mylist, mlist[ctr]);
				new_free((char **)&mlist[ctr]);
			}
			new_free((char **)&mlist);
			if (mylist)
				return mylist;
			RETURN_EMPTY;
		}
		default :
			yell("aliasctl: Error");
			RETURN_EMPTY;
	}
	RETURN_EMPTY;
}

/***************************************************************************/
/* XXXX - im sure this doesnt belong here, but for some reason i dont care. */
static RuntimeStack *call_stack = NULL;
int max_wind = -1;
int wind_index = -1;
void dump_call_stack _((void));

void make_local_stack (char *name)
{
	wind_index++;

	if (wind_index >= max_wind)
	{
		int tmp_wind = wind_index;

		if (max_wind == -1)
			max_wind = get_int_var(MAX_RECURSIONS_VAR);
		else
			max_wind *= 2;

		call_stack = (RuntimeStack *)new_realloc((void *)call_stack, sizeof(RuntimeStack) * max_wind);
		for (; wind_index < max_wind; wind_index++)
		{
			call_stack[wind_index].alias = NULL;
			call_stack[wind_index].current = NULL;
			call_stack[wind_index].name = NULL;
		}
		wind_index = tmp_wind;
	}

	/* Just in case... */
	if (call_stack[wind_index].alias)
		destroy_aliases(&call_stack[wind_index].alias);
	if (call_stack[wind_index].current)
		call_stack[wind_index].current = 0;
	if (call_stack[wind_index].name)
		call_stack[wind_index].name = 0;

	if (name)
		call_stack[wind_index].name = name;
	else
		call_stack[wind_index].name = empty_string;
}

void destroy_local_stack (void)
{
	/*
	 * We clean up as best we can here...
	 */
	if (call_stack[wind_index].alias)
		destroy_aliases(&call_stack[wind_index].alias);
	if (call_stack[wind_index].current)
		call_stack[wind_index].current = 0;
	if (call_stack[wind_index].name)
		call_stack[wind_index].name = 0;

	wind_index--;
}

#ifdef __STDC__
void set_current_command (char *line)
#else
void set_current_command (line)
char *line;
#endif
{
	call_stack[wind_index].current = line;
}

void unset_current_command _((void))
{
	call_stack[wind_index].current = NULL;
}

void add_local_variable (Alias **list, Alias *item)
{
	if (list)
		insert_alias(list, item);
	else
		insert_alias(&call_stack[wind_index].alias, item);
}

/* XXXX - maybe this doesnt belong here. */
Alias **find_local_variable (char *name, Alias **ali, int unlink)
{
	Alias *alias = NULL;
	int c = wind_index;
	RuntimeStack *stack = &call_stack[c];

	/*
	 * Search our current local variable stack, and wind our way
	 * backwards until we find a NAMED stack -- that is the enclosing
	 * alias or ON call.  If we find a variable in one of those enclosing
	 * stacks, then we use it.  If we dont, we progress.
	 *
	 * This needs to be optimized for the degenerate case, when there
	 * is no local variable available...  It will be true 99.999% of
	 * the time.
	 */
	while (c >= 0)
	{
		if (call_stack[c].alias)
			alias = find_alias(&(call_stack[c].alias), name, unlink, NULL, 0);

		if (alias)
			break;

		if (*call_stack[c].name)
			break;

		c--;
	}

	if (alias)
	{
		if (ali)
			*ali = alias;
		return &stack->alias;
	}

	return NULL;
}

void dump_call_stack (void)
{
	int my_wind_index = wind_index;
	say("Call stack");
	while (my_wind_index--)
		say("[%3d] %s", my_wind_index, call_stack[my_wind_index].name);
	say("End of call stack");
}

/* XXXX */
void panic_dump_call_stack (void)
{
	int my_wind_index = wind_index;
	printf("Call stack\n");
	while (my_wind_index--)
		printf("[%3d] %s\n", my_wind_index, call_stack[my_wind_index].name);
	printf("End of call stack\n");
}


/*
 * You may NOT call this unless youre about to exit.
 * If you do (call this when youre not about to exit), and you do it
 * very often, max_wind will get absurdly large.  So dont do it.
 *
 * XXXX - this doesnt clean up everything -- but do i care?
 */
void destroy_call_stack (void)
{
	wind_index = 0;
	new_free((char **)&call_stack);
}

static  AliasStack *    alias_stack = NULL;
static  AliasStack *    assign_stack = NULL;
                                
void	do_stack_alias (int type, char *args, int which)
{
	char	*name;
	AliasStack	*aptr,
			**aptrptr;
	int my_which = 0;
	
	if (which == STACK_DO_ALIAS)
	{
		name = "ALIAS";
		aptrptr = &alias_stack;
		my_which = COMMAND_ALIAS;
	}
	else
	{
		name = "ASSIGN";
		aptrptr = &assign_stack;
		my_which = VAR_ALIAS;
	}
	
	if (!*aptrptr && (type == STACK_POP || type == STACK_LIST))
	{
		say("%s stack is empty!", name);
		return;
	}

	if (STACK_PUSH == type)
	{
		/* Dont need to unstub it, we're not actually using it. */
		Alias *alptr = find_alias(&(alias_list[my_which]), args, 1, NULL, 0);
		aptr = (AliasStack *)new_malloc(sizeof(AliasStack));
		aptr->list = alptr;
		aptr->name = m_strdup(args);
		aptr->next = aptrptr ? *aptrptr : NULL;
		*aptrptr = aptr;
		return;
	}

	if (STACK_POP == type)
	{
		Alias *alptr;
		AliasStack *prev = NULL;
		for (aptr = *aptrptr; aptr; prev = aptr, aptr = aptr->next)
		{
			/* have we found it on the stack? */
			if (!my_stricmp(args, aptr->name))
			{
				/* remove it from the list */
				if (prev == NULL)
					*aptrptr = aptr->next;
				else
					prev->next = aptr->next;

				/* throw away anything we already have */
				/* dont need to unstub it if we're tossing it */
				alptr = find_alias(&(alias_list[my_which]),args, 1, NULL, 0);
				if (alptr)
				{
					new_free((char **)&(alptr->name));
					new_free((char **)&alptr);
				}

				/* put the new one in. */
				if (aptr->list)
					insert_alias(&(alias_list[my_which]), aptr->list);

				/* free it */
				new_free((char **)&aptr->name);
				new_free((char **)&aptr);
				return;
			}
		}
		say("%s is not on the %s stack!", args, name);
		return;
	}
	if (STACK_LIST == type)
	{
#if 0
		int	slevel = 0;
		OnStack	*osptr;

		for (osptr = on_stack; osptr; osptr = osptr->next)
		{
			if (osptr->which == which)
			{
				Hook	*hptr;

				slevel++;
				say("Level %d stack", slevel);
				for (hptr = osptr->list; hptr; hptr = hptr->next)
					show_hook(hptr, args);
			}
		}

		if (!slevel)
			say("The STACK ON %s list is empty", args);
		return;
#endif
		AliasStack	*tmp;

		say("%s STACK LIST", name);
		for (tmp = *aptrptr; tmp; tmp = tmp->next)
		{
			if (!tmp->list)
				say("\t%s\t<Placeholder>", tmp->name);
			else if (tmp->list->stub)
				say("\t%s STUBBED TO %s", tmp->name, tmp->list->stub);
			else
				say("\t%s\t%s", tmp->name, tmp->list->stuff);
		}
		return;

	}
	say("Unknown STACK type ??");
}

