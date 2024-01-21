/*
 * if.c: handles the IF command for IRCII 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990, 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: if.c,v 1.15 1995/10/12 15:02:11 mrg Exp $";
#endif

#include "irc.h"
#include "alias.h"
#include "ircaux.h"
#include "window.h"
#include "vars.h"
#include "output.h"
#include "if.h"

static	int	charcount _((char *, char));

/*
 * next_expr finds the next expression delimited by brackets. The type
 * of bracket expected is passed as a parameter. Returns NULL on error.
 */
char	*
#ifdef __STDC__
next_expr(char **args, char type)
#else
next_expr(args, type)
	char	**args;
	char	type;
#endif
{
	char	*ptr,
		*ptr2,
		*ptr3;

	if (!*args)
		return NULL;
	ptr2 = *args;
	if (!*ptr2)
		return 0;
	if (*ptr2 != type)
	{
		say("Expression syntax");
		return 0;
	}							/* { */
	ptr = MatchingBracket(ptr2 + 1, type, (type == '(') ? ')' : '}');
	if (!ptr)
	{
		say("Unmatched '%c'", type);
		return 0;
	}
	*ptr = '\0';
	do
	{
		ptr2++;
	}
	while (isspace(*ptr2));
	ptr3 = ptr+1;
	while (isspace(*ptr3))
		ptr3++;
	*args = ptr3;
	if (*ptr2)
	{
		ptr--;
		while (isspace(*ptr))
			*ptr-- = '\0';
	}
	return ptr2;
}

/*ARGSUSED*/
void
ifcmd(command, args, subargs)
	char	*command,
		*args;
	char	*subargs;
{
	char	*exp;
	char	*sub;
	int	flag = 0;
	int	result;

	if (!(exp = next_expr(&args, '(')))
	{
		yell("Missing CONDITION in IF");
		return;
	}
	sub = parse_inline(exp, subargs?subargs:empty_string, &flag);
	if (get_int_var(DEBUG_VAR) & DEBUG_EXPANSIONS)
		yell("If expression expands to: (%s)", sub);
	if (!*sub || *sub == '0')
		result = 0;
	else
		result = 1;
	new_free(&sub);
	if (!(exp = next_expr(&args, '{')))
	{
		yell("Missing THEN portion in IF");
		return;
	}
	if (!result && !(exp = next_expr(&args, '{')))
		return;
	parse_line((char *) 0, exp, subargs ? subargs : empty_string, 0, 0);
		return;
}

/*ARGSUSED*/
void
whilecmd(command, args, subargs)
	char	*command,
		*args;
	char	*subargs;
{
	char	*exp = (char *) 0,
		*ptr,
		*body = (char *) 0,
		*newexp = (char *) 0;
	int	args_used;	/* this isn't used here, but is passed
				 * to expand_alias() */

	if ((ptr = next_expr(&args, '(')) == (char *) 0)
	{
		yell("WHILE: missing boolean expression");
		return;
	}
	malloc_strcpy(&exp, ptr);
	if ((ptr = next_expr(&args, '{')) == (char *) 0)
	{
		say("WHILE: missing expression");
		new_free(&exp);
		return;
	}
	malloc_strcpy(&body, ptr);
	while (1)
	{
		malloc_strcpy(&newexp, exp);
		ptr = parse_inline(newexp, subargs ? subargs : empty_string,
			&args_used);
		if (*ptr && *ptr !='0')
		{
			new_free(&ptr);
			parse_line((char *) 0, body, subargs ?
				subargs : empty_string, 0, 0);
		}
		else
			break;
	}
	new_free(&newexp);
	new_free(&ptr);
	new_free(&exp);
	new_free(&body);
}

static int    
#ifdef __STDC__
charcount(char *string, char what)
#else
charcount(string, what)
	char    *string,
		what;
#endif
{
	int     x       = 0;
	char    *place  = string - 1;

	while ((place = index(place + 1, what)))
		x++;
	return x;
}

/*
 * How it works -- if There are no parenthesis, it must be a
 * foreach array command.  If there are parenthesis, and there are
 * exactly two commas, it must be a C-like for command, else it must
 * must be an foreach word command
 */
void
foreach_handler(command,args,subargs)
	char	*command,
		*args,
		*subargs;
{
	char    *temp = (char *) 0;
	char    *placeholder;
	char    *temp2 = (char *) 0;

	malloc_strcpy(&temp, args);
	placeholder = temp;
	if (*temp == '(')
	{
		if ((temp2 = next_expr(&temp,'(')) == (char *) 0) {
			new_free(&placeholder);
			return;
		}
		if (charcount(temp2,',') == 2)
			forcmd(command,args,subargs);
		else
			fe(command,args,subargs);
	}
	else
		foreach(command,args,subargs);
	new_free(&placeholder);
}

/*ARGSUSED*/
void
foreach(command, args, subargs)
	char	*command,
		*args;
	char	*subargs;
{
	char	*struc = (char *) 0,
		*ptr,
		*body = (char *) 0,
		*var = (char *) 0;
	char	**sublist;
	int	total;
	int	i;
	int	slen;
	int	old_display;

	if ((ptr = new_next_arg(args, &args)) == (char *) 0)
	{
		yell("FOREACH: missing structure expression");
		return;
	}
	malloc_strcpy(&struc, ptr);
	malloc_strcat(&struc, ".");
	upper(struc);
	if ((var = next_arg(args, &args)) == (char *) 0)
	{
		new_free(&struc);
		yell("FOREACH: missing variable");
		return;
	}
	while (isspace(*args))
		args++;
	if ((body = next_expr(&args, '{')) == (char *) 0)	/* } */
	{
		new_free(&struc);
		yell("FOREACH: missing statement");
		return;
	}
	sublist=match_alias(struc, &total, VAR_ALIAS);
	slen=strlen(struc);
	old_display=window_display;
	for (i=0;i<total;i++)
	{
		window_display=0;
		add_alias(VAR_ALIAS, var, sublist[i]+slen);
		window_display=old_display;
		parse_line((char *) 0, body, subargs ?
		    subargs : empty_string, 0, 0);
		new_free(&sublist[i]);
	}
	new_free(&sublist);
	new_free(&struc);
}

/*
 * FE:  Written by Jeremy Nelson (jnelson@iastate.edu)
 *
 * FE: replaces recursion
 *
 * The thing about it is that you can nest variables, as this command calls
 * expand_alias until the list doesnt change.  So you can nest lists in
 * lists, and hopefully that will work.  However, it also makes it 
 * impossible to have $s anywhere in the list.  Maybe ill change that
 * some day.
 */

void
fe(command, args, subargs)
	char    *command,
		*args,
		*subargs;
{
	char    *list = (char *) 0,
		*templist = (char *) 0,
		*placeholder,
		*oldlist = (char *) 0,
		*sa,
		*vars,
		*var[255],
		*word = (char *) 0,
		*todo = (char *) 0;
	int     ind, x, y, blah,args_flag;
	int     old_display;

	for (x = 0; x <= 255; var[x++] = (char *) 0)
		;

	list = next_expr(&args, '(');	/* ) */
	if (!list)
	{
		yell ("FE: Missing List for /FE");
		return;
	}

	sa = subargs ? subargs : " ";
	malloc_strcpy(&templist, list);
	do 
	{
		malloc_strcpy(&oldlist, templist);
		new_free(&templist);
		templist = expand_alias((char *) 0,oldlist,sa,&args_flag, (char **) 0);
	} while (strcmp(templist, oldlist));

	new_free(&oldlist);

	if (*templist == '\0')
	{
		new_free(&templist);
		return;
	}

	vars = args;
	if (!(args = index(args, '{')))		/* } */
	{
		yell ("FE: Missing commands");
		new_free(&templist);
		return;
	}
	*(args-1) = '\0';
	ind = 0;
	while ((var[ind++] = next_arg(vars, &vars)))
	{
		if (ind == 255)
		{
			yell ("FE: Too many variables");
			new_free(&templist);
			return;
		}
	}
	ind = ind ? ind - 1: 0;

	if (!(todo = next_expr(&args, '{')))		/* } { */
	{
		yell ("FE: Missing }");		
		new_free(&templist);
		return;
	}

	blah = word_count(templist);
	old_display = window_display;
	placeholder = templist;
	for ( x = 0 ; x < blah ; )
	{
		window_display = 0;
		for ( y = 0 ; y < ind ; y++ )
		{
			word = ((x+y)<blah)
			    ? next_arg(templist, &templist)
			    : (char *) 0;
			add_alias(VAR_ALIAS, var[y], word);
		}
		window_display = old_display;
		x += ind;
		parse_line((char *) 0, todo, 
		    subargs?subargs:empty_string, 0, 0);
	}
	window_display = 0;
	for (y=0;y<ind;y++)  {
		delete_alias(VAR_ALIAS,var[y]);
	}
	window_display = old_display;
	new_free(&placeholder);
}

/* FOR command..... prototype: 
 *  for (commence,evaluation,iteration)
 * in the same style of C's for, the for loop is just a specific
 * type of WHILE loop.
 *
 * IMPORTANT: Since ircII uses ; as a delimeter between commands,
 * commas were chosen to be the delimiter between expressions,
 * so that semicolons may be used in the expressions (think of this
 * as the reverse as C, where commas seperate commands in expressions,
 * and semicolons end expressions.
 */
/*  I suppose someone could make a case that since the
 *  foreach_handler() routine weeds out any for command that doesnt have
 *  two commans, that checking for those 2 commas is a waste.  I suppose.
 */
void
forcmd(command, args, subargs)
	char    *command;
	char    *args;
	char    *subargs;
{
	char        *working        = (char *) 0;
	char        *commence       = (char *) 0;
	char        *evaluation     = (char *) 0;
	char        *lameeval       = (char *) 0;
	char        *iteration      = (char *) 0;
	char        *sa             = (char *) 0;
	int         argsused        = 0;
	char        *blah           = (char *) 0;
	char        *commands       = (char *) 0;

	/* Get the whole () thing */
	if ((working = next_expr(&args, '(')) == (char *) 0)	/* ) */
	{
		yell("FOR: missing closing parenthesis");
		return;
	}
	malloc_strcpy(&commence, working);

	/* Find the beginning of the second expression */
	evaluation = index(commence, ',');
	if (!evaluation)
	{
		yell("FOR: no components!");
		new_free(&commence);
		return;
	}
	do 
		*evaluation++ = '\0';
	while (isspace(*evaluation));

	/* Find the beginning of the third expression */
	iteration = index(evaluation, ',');
	if (!iteration)
	{
		yell("FOR: Only two components!");
		new_free(&commence);
		return;
	}
	do 
	{
		*iteration++ = '\0';
	}
	while (isspace(*iteration));

	working = args;
	while (isspace(*working))
		*working++ = '\0';

	if ((working = next_expr(&working, '{')) == (char *) 0)		/* } */
	{
		yell("FOR: badly formed commands");
		new_free(&commence);
		return;
	}

	malloc_strcpy(&commands, working);

	sa = subargs?subargs:empty_string;
	parse_line((char *) 0, commence, sa, 0, 0);

	while (1)
	{
		malloc_strcpy(&lameeval, evaluation);
		blah = parse_inline(lameeval,sa,&argsused);
		if (*blah && *blah != '0')
		{
			new_free(&blah);
			parse_line((char *) 0, commands, sa, 0, 0);
			parse_line((char *) 0, iteration, sa, 0, 0);
		}
		else break;
	}
	new_free(&blah);
	new_free(&lameeval);
	new_free(&commence);
	new_free(&commands);
}

/* fec - iterate over a list of characters */

extern	void
fec(command, args, subargs)
	char	*command,
	*args,
	*subargs;
{
	char    *pointer;
	char    *list = (char *) 0;
	char    *var = (char *) 0;
	char    booya[2];
	int     args_flag = 0, old_display;
	char    *sa,*todo;

	list = next_expr(&args, '(');		/* ) */
	if (list == (char *) 0)
	{
		yell ("FEC: Missing List for /FEC");
		return;
	}

	sa = subargs ? subargs : empty_string;
	list = expand_alias((char *) 0,list,sa,&args_flag,(char **) 0);
	pointer = list;

	var = next_arg(args, &args);
	args = index(args, '{');		/* } */

	if ((todo = next_expr(&args, '{')) == (char *) 0)
	{
		yell ("FE: Missing }");
		return;
	}

	booya[1] = '\0';
	old_display = window_display;

	while (*pointer)
	{
		window_display = 0;
		booya[0] = *pointer++;
		add_alias(VAR_ALIAS, var, booya);
		window_display = old_display;
		parse_line((char *) 0, todo, 
		    subargs?subargs:empty_string, 0, 0);
	}
	window_display = 0;
	delete_alias(VAR_ALIAS,var);
	window_display = old_display;

	new_free(&list);
}
