/*
 * if.c: handles the IF command for IRCII 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990, 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"
#include "alias.h"
#include "ircaux.h"
#include "window.h"
#include "vars.h"
#include "output.h"
#include "if.h"

/*
 * next_expr finds the next expression delimited by brackets. The type
 * of bracket expected is passed as a parameter. Returns NULL on error.
 */
char	*my_next_expr(char **args, char type, int whine)
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
		if (whine)
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
		ptr2++;
	while (my_isspace(*ptr2));

	ptr3 = ptr+1;
	while (my_isspace(*ptr3))
		ptr3++;
	*args = ptr3;
	if (*ptr2)
	{
		ptr--;
		while (my_isspace(*ptr))
			*ptr-- = '\0';
	}
	return ptr2;
}

extern char *next_expr_failok (char **args, char type)
{
	return my_next_expr (args, type, 0);
}

extern char *next_expr (char **args, char type)
{
	return my_next_expr (args, type, 1);
}

/*ARGSUSED*/
void ifcmd(char *command, char *args, char *subargs)
{
	char	*exp = NULL;
	char	*sub = NULL;
	int	flag = 0;
	int	result;
	int	falseval;
	
	falseval = my_stricmp(command, "IF") ? 1 : 0;
	
	if (!(exp = next_expr(&args, '(')))
	{
		yell("Missing CONDITION in %s", command);
		return;
	}
	sub = parse_inline(exp, subargs?subargs:empty_string, &flag);
	if (get_int_var(DEBUG_VAR) & DEBUG_EXPANSIONS)
		yell("%s expression expands to: (%s)", command, sub);

	result = check_val(sub);
	new_free(&sub);

	if (!(exp = next_expr_failok(&args, '{')))
	{
		exp = args;
		args = NULL;
	}

	if (falseval == result)
		exp = args ? next_expr(&args, '{') : NULL;

	if (!exp)
		return;

	parse_line(NULL, exp, subargs ? subargs : empty_string, 0, 0);
	return;
}

extern void docmd (char *command, char *args, char *subargs)
{
	char *body, *expr, *cmd, *ptr;
	char *newexp = NULL;
	int args_used = 0;
	int result;

	if (*args == '{')
	{
		body = next_expr(&args, '{');
		if (body == NULL)
		{
			yell("DO: unbalanced {");
			return;
		}	
		if (args && *args && (cmd = next_arg(args, &args)) && 
		     !my_stricmp (cmd, "while"))
		{
			expr = next_expr(&args, '(');
			if (expr == NULL)
			{
				yell("DO: unbalanced (");
				return;
			}
			while (1)
			{
				parse_line (NULL, body, subargs ? subargs : empty_string, 0, 0);
				malloc_strcpy(&newexp, expr);
				ptr = parse_inline(newexp, subargs ? subargs : empty_string,
					&args_used);
				result = (!ptr || *ptr == '0') ? 0 : (!*ptr ? 0 : 1);
				if (result)
					new_free(&ptr);
				else
				{
					new_free(&newexp);
					return;
				}
			}	
		}
		/* falls through to here if its /do {...} */
		parse_line (NULL, body, subargs ? subargs : empty_string, 0, 0);
	}
	/* falls through to here if it its /do ... */
	parse_line (NULL, args, subargs ? subargs : empty_string, 0, 0);
}

/*ARGSUSED*/
void whilecmd(char *command, char *args, char *subargs)
{
	char	*exp = NULL,
		*ptr = NULL,
		*body = NULL,
		*newexp = NULL;
	int	args_used;	/* this isn't used here, but is passed
				 * to expand_alias() */
	int whileval;

	whileval = !my_stricmp(command, "WHILE");

	if ((ptr = next_expr(&args, '(')) == (char *) 0)
	{
		yell("WHILE: missing boolean expression");
		return;
	}
	malloc_strcpy(&exp, ptr);
	if ((ptr = next_expr_failok(&args, '{')) == (char *) 0)
		ptr = args;

	malloc_strcpy(&body, ptr);
	while (1)
	{
		malloc_strcpy(&newexp, exp);
		ptr = parse_inline(newexp, subargs ? subargs : empty_string, &args_used);
		if (check_val(ptr) == whileval)
		{
			new_free(&ptr);
			parse_line(NULL, body, subargs ?  subargs : empty_string, 0, 0);
		}
		else
			break;
	}
	new_free(&newexp);
	new_free(&ptr);
	new_free(&exp);
	new_free(&body);
}

int charcount(char *string, char what)
{
        int x = 0;
	char *place = string;
                
	while (*place)
		if (*place++ == what)
			x++;

	return x;
}

void foreach(char *command, char *args, char *subargs)
{
	char	*struc = NULL,
		*ptr,
		*body = NULL,
		*var = NULL;
	char	**sublist;
	int	total;
	int	i;
	int	slen;
	int	old_display;
	int     list = VAR_ALIAS;
	int	af;
	char *remove_brackets(char *, char *, int *);
	
        while (args && my_isspace(*args))
        	args++;

        if (*args == '-')
                args++, list = COMMAND_ALIAS;

	if ((ptr = new_next_arg(args, &args)) == NULL)
	{
		yell("FOREACH: missing structure expression");
		return;
	}

	struc = remove_brackets(ptr, subargs, &af);
	malloc_strcat(&struc, ".");
	upper(struc);

	if ((var = next_arg(args, &args)) == NULL)
	{
		new_free(&struc);
		yell("FOREACH: missing variable");
		return;
	}
	while (my_isspace(*args))
		args++;
	if ((body = next_expr(&args, '{')) == NULL)	/* } */
	{
		new_free(&struc);
		yell("FOREACH: missing statement");
		return;
	}

	sublist=match_alias(struc, &total, list);
	slen=strlen(struc);
	old_display=window_display;

	for (i=0;i<total;i++)
	{
		window_display=0;
		add_alias(VAR_ALIAS_LOCAL, var, sublist[i]+slen);
		window_display=old_display;
		parse_line(NULL, body, subargs ? subargs:empty_string, 0, 0);
		new_free(&sublist[i]);
	}

	new_free((char **)&sublist);
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

void fe(char *command, char *args, char *subargs)
{
	char    *list = NULL,
		*templist = NULL,
		*placeholder,
		*sa,
		*vars,
		*var[255],
		*word = NULL,
		*todo = NULL,
		fec_buffer[2];
	int     ind, x, y, blah = 0, args_flag;
	int     old_display;
	int	doing_fe = !my_stricmp(command, "FE");

	for (x = 0; x <= 254; var[x++] = NULL)
		;

	list = next_expr(&args, '(');

	if (!list)
	{
		yell ("%s: Missing List for /%s", command, command);
		return;
	}

	sa = subargs ? subargs : " ";

	templist = expand_alias(NULL, list, sa, &args_flag, NULL);
	if (templist && *templist == '\0')
	{
		new_free(&templist);
		return;
	}

	vars = args;
	if (!(args = index(args, '{')))		/* } */
	{
		yell ("%s: Missing commands", command);
		new_free(&templist);
		return;
	}
	*(args-1) = '\0';
	ind = 0;

	while ((var[ind++] = next_arg(vars, &vars)))
	{
		if (ind == 255)
		{
			yell ("%s: Too many variables", command);
			new_free(&templist);
			return;
		}
	}
	ind = ind ? ind - 1: 0;

	if (!(todo = next_expr(&args, '{')))		/* } { */
	{
		yell ("%s: Missing }", command);		
		new_free(&templist);
		return;
	}

	old_display = window_display;

	if (!doing_fe)
		{ word = fec_buffer; word[1] = 0; }
		
	blah = ((doing_fe) ? (word_count(templist)) : (strlen(templist)));
	placeholder = templist;
	for ( x = 0 ; x < blah ; )
	{
		window_display = 0;
		for ( y = 0 ; y < ind ; y++ )
		{
			if (doing_fe)
				word = ((x+y) < blah)
				    ? new_next_arg(templist, &templist)
				    : empty_string;
			else
				word[0] = ((x+y) < blah)
				    ? templist[x+y] : 0;

			add_alias(VAR_ALIAS_LOCAL, var[y], word);
		}
		window_display = old_display;
		x += ind;
		parse_line(NULL, todo, subargs?subargs:empty_string, 0, 0);
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
void forcmd(char *command, char *args, char *subargs)
{
	char        *working        = NULL;
	char        *commence       = NULL;
	char        *evaluation     = NULL;
	char        *lameeval       = NULL;
	char        *iteration      = NULL;
	char        *sa             = NULL;
	int         argsused        = 0;
	char        *blah           = NULL;
	char        *commands       = NULL;

	/* Get the whole () thing */
	if ((working = next_expr(&args, '(')) == NULL)	/* ) */
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
	while (my_isspace(*evaluation));

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
	while (my_isspace(*iteration));

	working = args;
	while (my_isspace(*working))
		*working++ = '\0';

	if ((working = next_expr(&working, '{')) == NULL)		/* } */
	{
		yell("FOR: badly formed commands");
		new_free(&commence);
		return;
	}

	malloc_strcpy(&commands, working);

	sa = subargs?subargs:empty_string;
	parse_line(NULL, commence, sa, 0, 0);

	while (1)
	{
		malloc_strcpy(&lameeval, evaluation);
		blah = parse_inline(lameeval,sa,&argsused);
		if (*blah && *blah != '0')
		{
			new_free(&blah);
			parse_line(NULL, commands, sa, 0, 0);
			parse_line(NULL, iteration, sa, 0, 0);
		}
		else break;
	}
	new_free(&blah);
	new_free(&lameeval);
	new_free(&commence);
	new_free(&commands);
}

/*

  Need to support something like this:

	switch (text to be matched)
	{
		(sample text)
		{
			...
		}
		(sample text2)
		(sample text3)
		{
			...
		}
		...
	}

How it works:

	The command is technically made up a single (...) expression and
	a single {...} expression.  The (...) expression is taken to be
	regular expando-text (much like the (...) body of /fe.

	The {...} body is taken to be a series of [(...)] {...} pairs.
	The [(...)] expression is taken to be one or more consecutive
	(...) structures, which are taken to be text expressions to match
	against the header text.  If any of the (...) expressions are found
	to match, then the commands in the {...} body are executed.

	There may be as many such [(...)] {...} pairs as you need.  However,
	only the *first* pair found to be matching is actually executed,
	and the others are ignored, so placement of your switches are
	rather important:  Put your most general ones last.

*/
void switchcmd(char *command, char *args, char *subargs)
{
	char *control, *body, *header, *commands;
	int af;

	control = next_expr(&args, '(');
	if (!control)
	{
		say("SWITCH: String to be matched not found where expected");
		return;
	}
	control = expand_alias(NULL, control, subargs, &af, NULL);
	if (get_int_var(DEBUG_VAR) & DEBUG_EXPANSIONS)
		yell("%s expression expands to: (%s)", command, control);

	if (!(body = next_expr(&args, '{')))
	{
		say("SWITCH: Execution body not found where expected");
		new_free(&control);
		return;
	}

	while (body && *body)
	{
		int hooked = 0;

		while (*body == '(')
		{
			if (!(header = next_expr(&body, '(')))
			{
				say("SWITCH: Case label not found where expected");
				new_free(&control);
				return;
			}
			header = expand_alias(NULL, header, subargs, &af, NULL);
			if (get_int_var(DEBUG_VAR) & DEBUG_EXPANSIONS)
				yell("%s expression expands to: (%s)", command, header);
			if (wild_match(header, control))
				hooked = 1;
			new_free(&header);
			if (*body == ';')
				body++;		/* ugh. */
		}
		if (!(commands = next_expr(&body, '{')))
		{
			say("SWITCH: case body not found where expected");
			return;
		}

		if (hooked)
		{
			parse_line(NULL, commands, subargs, 0, 0);
			return;
		}

		if (*body == ';')
			body++;		/* grumble */
	}
}

void repeatcmd(char *command, char *args, char *subargs)
{
	char *num_expr = NULL;
	int value;

	while (isspace(*args))
		args++;

	if (*args == '(')
	{
		char *tmp_val;
		char *dumb_copy;
		int argsused;
		char *sa = subargs ? subargs : empty_string;

		num_expr = next_expr(&args, '(');
		dumb_copy = m_strdup(num_expr);

		tmp_val = parse_inline(dumb_copy,sa,&argsused);
		value = my_atol(tmp_val);
		new_free(&tmp_val);
		new_free(&dumb_copy);
	}
	else
	{
		char *tmp_val;
		int af;

		num_expr = new_next_arg(args, &args);
		tmp_val = expand_alias(NULL, num_expr, subargs, &af, NULL);
		value = my_atol(tmp_val);
		new_free(&tmp_val);
	}

	if (value <= 0)
		return;

	while (value--)
		parse_line(NULL, args, subargs, 0, 0);

	return;
}
