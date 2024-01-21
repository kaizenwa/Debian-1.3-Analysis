/*
 * if.h: header for if.c
 *
 * copyright(c) 1994 matthew green
 *
 * See the copyright file, or do a help ircii copyright 
 *
 * @(#)$Id: if.h,v 1.5 1995/08/31 03:51:31 scottr Exp $
 */

#ifndef __if_h
# define __if_h

	char	*next_expr _((char **, char));
	int	char_count _((char *, char *));
	void	ifcmd _((char *, char *, char *));
	void	whilecmd _((char *, char *, char *));
	void	foreach_handler _((char *, char *, char *));
	void	foreach _((char *, char *, char *));
	void	fe _((char *, char *, char *));
	void	forcmd _((char *, char *, char *));
	void	fec _((char *, char *, char *));
	void	docmd _((char *, char *, char *));
		void	switchcmd _((char *, char *, char *));
extern	char *  next_expr       	_((char **, char));
extern	char *  next_expr_failok        _((char **, char));
	int	charcount _((char *, char ));
  		
#endif /* __if_h */
