/*
 * alias.h: header for alias.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: alias.h,v 1.13 1995/09/03 13:45:14 mrg Exp $
 */

#ifndef __alias_h_
#define __alias_h_

#define COMMAND_ALIAS 0
#define VAR_ALIAS 1

#define LEFT_BRACE '{'
#define RIGHT_BRACE '}'
#define LEFT_BRACKET '['
#define RIGHT_BRACKET ']'
#define LEFT_PAREN '('
#define RIGHT_PAREN ')'
#define DOUBLE_QUOTE '"'

	void	add_alias _((int, char *, char *));
	char	*get_alias _((int, char *, int *, char **));
	char	*expand_alias _((char *, char *, char *, int *, char **));
	void	execute_alias _((char *, char *, char *));
	void	list_aliases _((int, char *));
	int	mark_alias _((char *, int));
	void	delete_alias _((int, char *));
	char	**match_alias _((char *, int *, int));
	void	alias _((char *, char *, char *));
	char	*parse_inline _((char *, char *, int *));
	char	*MatchingBracket _((char *, char, char));
	void	save_aliases _((FILE *, int));
	int	word_count _((char *));

extern	char	alias_illegals[];
extern	char	command_line[];

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
	int	mark;			/* used to prevent recursive aliasing */
	int	global;			/* set if loaded from `global' */
	struct	AliasStru *next;	/* pointer to next alias in list */
}	Alias;

#define MAX_CMD_ARGS 5

#endif /* __alias_h_ */
