/*
 * alias.h: header for alias.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: alias.h,v 1.5 1994/07/02 02:38:10 mrg Exp $
 */

#ifndef _ALIAS_H_
#define _ALIAS_H_

#include <stdio.h>	/* needed for save_aliases() */
#include "irc_std.h"

#define COMMAND_ALIAS 0
#define VAR_ALIAS 1
#define VAR_ALIAS_LOCAL 2
#define STRUCT_ALIAS 3

#define DCC_STRUCT 0
#define USERLIST_STRUCT 1
#define NICK_STRUCT 2
#define SERVER_STRUCT 3

#define LEFT_BRACE '{'
#define RIGHT_BRACE '}'
#define LEFT_BRACKET '['
#define RIGHT_BRACKET ']'
#define LEFT_PAREN '('
#define RIGHT_PAREN ')'
#define DOUBLE_QUOTE '"'

#define MAX_CMD_ARGS 5

extern	char	alias_illegals[];

struct	ArgPosTag
{
	char *ArgStart;
	int ArgLen;
	char *FirstComp;
};

typedef	struct ArgPosTag	ArgPos;

/* Alias: structure of each alias entry */
typedef	struct	AliasStru
{
	char	*name;			/* name of alias */
	char	*stuff;			/* what the alias is */
	char	*stub;			/* the file its stubbed to */
	int	mark;			/* used to prevent recursive aliasing */
	int	global;			/* set if loaded from `global' */
	void	*what;		/* pointer to structure */
	int	struct_type;		/* type of structure */
	struct	AliasStru *next;	/* pointer to next alias in list */
}	Alias;
/* RuntimeStack -- runtime execution stack */
typedef struct RuntimeStackStru
{
	char    *name;                  /* Name of the stack */
	char    *current;               /* Current cmd being executed */
	Alias   *alias;                 /* Local variables */
} RuntimeStack;


extern	void	add_alias _((int, char *, char *));
extern	void	alias _((char *, char *, char *));
extern	char *	expand_alias _((char *, char *, char *, int *, char **));
extern	void	execute_alias _((char *, char *, char *));
extern	void	flush_aliases _((char *, char *, char *));
extern	char *	get_alias _((int, char *, int *, char **));
extern	char ** match_alias _((char *, int *, int));
extern	char *	MatchingBracket _((char *, char, char));
extern	char *	MatchingBracketBackwards _((char *, char *, char, char));
extern	char *	parse_inline _((char *, char *, int *));
extern	void	save_aliases _((FILE *, int));
extern	void	stubcmd _((char *, char *, char *));
extern	int	word_count _((char *));
extern	char *	function_push _((char *));
extern	char *	function_pop _((char *));
extern	char *	function_shift _((char *));
extern	char *	function_unshift _((char *));
extern	char *	host_to_ip _((const char *));
extern	char *	ip_to_host _((const char *));
extern	char *	one_to_another _((const char *));
extern	char *	built_in_alias _((char));
extern	char *	call_function _((char *, char *, char *, int *));
extern  char ** get_builtins _((char *, int, char **, int, int *));
extern  Alias	*find_alias	_((Alias **, char *, int, int *, int)); 
extern	int	parse_number _((char **));
Alias	*find_alias_with_args (Alias **, char *, int, int *, int, char *, int *);
char	*find_inline _((char *));
	void	insert_alias 		(Alias **, Alias *); /* static */
	char	*alias_special_char	(char **, char *, char *, char *, int *);
	char	*call_user_function	_((char *, char *));
	void	do_stack_alias		_((int, char *, int));
	
extern Alias *alias_list[];
extern void set_current_command _((char *));
extern void unset_current_command _((void));
 
extern BuiltInDllFunctions *dll_functions;

/*
 * These are the three primitives for runtime stacks.
 * XXXX these are in edit.c, and dont belong here. 
 */
extern	RuntimeStack	*call_stack;
extern	int		wind_index;
extern	void	make_local_stack _((char *));
extern  void	add_local_variable _((Alias **, Alias *));
extern	Alias **find_local_variable _((char *, Alias **, int));
extern	void	destroy_local_stack _((void));

#endif /* _ALIAS_H_ */
