/*
 * stack.h - header for stack.c
 *
 * written by matthew green
 *
 * copyright (c) 1993, 1994.
 *
 * @(#)$Id: stack.h,v 1.11 1995/08/31 03:51:57 scottr Exp $
 */

#ifndef __stack_h_
# define __stack_h_

#include "hook.h"
#include "alias.h"

	void	stackcmd  _((char *, char *, char *));

#define STACK_POP 0
#define STACK_PUSH 1
#define STACK_SWAP 2
#define STACK_LIST 3

#define STACK_DO_ALIAS	0x0001
#define STACK_DO_ASSIGN	0x0002

typedef	struct	setstacklist
{
	int	which;
	Hook	*list;
	struct setstacklist *next;
}	SetStack;

typedef	struct	aliasstacklist
{
	int	which;
	char	*name;
	IrcVariable *set;
	enum	VAR_TYPES var_index;
	Alias	*list;
	struct aliasstacklist *next;
}	AliasStack;

typedef	struct	onstacklist
{
	int	which;
	Hook	*list;
	struct onstacklist *next;
}	OnStack;

#endif /* __stack_h_ */
