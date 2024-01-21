/*
 * stack.c - does the handling of stack functions
 *
 * written by matthew green
 * finished by Jeremy Nelson (ESL)
 * modified Colten Edwards 1996 for BitchX
 * copyright (C) 1993.
 */

#include "irc.h"

#include "vars.h"
#include "stack.h"
#include "window.h"
#include "hook.h"
#include "ircaux.h"
#include "output.h"
#include "list.h"
#include "misc.h"

static	OnStack	*	on_stack = NULL;
	AliasStack *	set_stack = NULL;

extern void do_stack_set _((int, char *));

static	void	do_stack_on (int type, char *args)
{
	char	foo[4];
	int	len, cnt, i, which = 0;
	Hook	*list;
	NumericList *nhook, *nptr, *ntmp = NULL;

	if (!on_stack && (type == STACK_POP || type == STACK_LIST))
	{
		say("ON stack is empty!");
		return;
	}
	if (!args || !*args)
	{
		say("Missing event type for STACK ON");
		return;
	}
	len = strlen(args);
	for (cnt = 0, i = 0; i < NUMBER_OF_LISTS; i++)
	{
		if (!my_strnicmp(args, hook_functions[i].name, len))
		{
			if (strlen(hook_functions[i].name) == len)
			{
				cnt = 1;
				which = i;
				break;
			}
			else
			{
				cnt++;
				which = i;
			}
		}
		else if (cnt)
			break;
	}
	if (!cnt)
	{
		if (is_number(args))
		{
			which = my_atol(args);
			if (which < 1 || which > 999)
			{
				say("Numerics must be between 001 and 999");
				return;
			}
			which = -which;
		}
		else
		{
			say("No such ON function: %s", args);
			return;
		}
	}
	if (which < 0)
	{
		sprintf(foo, "%3.3u", -which);
		if ((nhook = (NumericList *) find_in_list((List **)&numeric_list, foo, 0)) != NULL)
			list = nhook->list;
		else
			list = NULL;
	}
	else
		list = hook_functions[which].list;

	if (type == STACK_PUSH)
	{
		OnStack	*new;

		new = (OnStack *) new_malloc(sizeof(OnStack));
		new->next = on_stack;
		on_stack = new;
		new->which = which;
		new->list = list;
		if (which < 0)
		{
			if (nhook == numeric_list)
			{
				if (nhook)
				{
					numeric_list = nhook->next;
					new_free(&nhook->name);
					new_free((char **)&nhook);
				}
				return;
			}
			for (nptr = numeric_list; nptr; ntmp = nptr, nptr = nptr->next)
			{
				if (nptr == nhook)
				{
					ntmp->next = nptr->next;
					new_free(&nptr->name);
					new_free((char **)&nptr);
					return;
				}
			}
		}
		else
			hook_functions[which].list = NULL;
		return;
	}
	else if (type == STACK_POP)
	{
		OnStack	*p, *tmp = NULL;

		for (p = on_stack; p; tmp = p, p = p->next)
		{
			if (p->which == which)
			{
				if (p == on_stack)
					on_stack = p->next;
				else
					tmp->next = p->next;
				break;
			}
		}
		if (!p)
		{
			say("No %s on the stack", args);
			return;
		}
		if ((which < 0 && nhook) || hook_functions[which].list)
			remove_hook(which, NULL, 0, 0, 1);	/* free hooks */
		if (which < 0)
		{
			/* look -- do we have any hooks already for this numeric? */
			if ((nptr = (NumericList *) find_in_list((List **)&numeric_list, foo, 0)) == NULL)
			{
				if (p->list)	/* not just a placeholder? */
				{
					/* No. make a new list and put the stack on it */
					nptr = (NumericList *) new_malloc(sizeof(NumericList));
					nptr->name = NULL;
					nptr->list = p->list;
					malloc_strcpy(&(nptr->name), foo);
					add_to_list((List **)&numeric_list, (List *)nptr);
				}
			}
			else
			{
				Hook *tmp, *next;

				/* If there is already a list, we have to clobber it */
				for(tmp = nptr->list; tmp; tmp = next)
				{
					next = tmp->next;
					tmp->not = 1;
					new_free(&(tmp->nick));
					new_free(&(tmp->stuff));
					wait_new_free((char **)&tmp);
				}
				nptr->list = p->list;
			}
		}
		else
			hook_functions[which].list = p->list;
		new_free((char **)&p);
		return;
	}
	else if (type == STACK_LIST)
	{
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
	}
	say("Unknown STACK ON type ??");
}

extern	void	stackcmd (char *command, char *args, char *subargs)
{
	char	*arg;
	int	len, type;

	if ((arg = next_arg(args, &args)) != NULL)
	{
		len = strlen(arg);
		if (!my_strnicmp(arg, "PUSH", len))
			type = STACK_PUSH;
		else if (!my_strnicmp(arg, "POP", len))
			type = STACK_POP;
		else if (!my_strnicmp(arg, "LIST", len))
			type = STACK_LIST;
		else
		{
			say("%s is unknown stack verb", arg);
			return;
		}
	}
	else
	{
		userage("Stack", "[push|pop|list] [on|alias|assign|set]");
		return;
	}
	if ((arg = next_arg(args, &args)) != NULL)
	{
		len = strlen(arg);
		if (!my_strnicmp(arg, "ON", len))
			do_stack_on(type, args);
		else if (!my_strnicmp(arg, "ALIAS", len))
			do_stack_alias(type, args, STACK_DO_ALIAS);
		else if (!my_strnicmp(arg, "ASSIGN", len))
			do_stack_alias(type, args, STACK_DO_ASSIGN);
		else if (!my_strnicmp(arg, "SET", len))
			do_stack_set(type, args);
		else
		{
			say("%s is not a valid STACK type", arg);
			return;
		}
	}
	else
	{
		userage("Stack", "[push|pop|list] [on|alias|assign|set]");
		return;
	}
}
