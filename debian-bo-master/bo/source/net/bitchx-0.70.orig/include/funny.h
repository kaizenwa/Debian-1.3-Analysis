/*
 * funny.h: header for funny.c
 *
 * written by michael sandrof
 *
 * copyright(c) 1990 
 *
 * see the copyright file, or do a help ircii copyright 
 *
 * @(#)$Id: funny.h,v 1.8 1995/08/31 03:51:25 scottr Exp $
 */

#ifndef __funny_h_
#define __funny_h_

#define FUNNY_PUBLIC 1
#define FUNNY_PRIVATE 2
#define FUNNY_TOPIC  4
#define FUNNY_WIDE   8
#define FUNNY_USERS  16
#define FUNNY_NAME   32

	void	set_funny_flags _((int, int, int));
	void	funny_set_ignore_mode _((void));
	int	funny_is_ignore_channel _((void));
	void	funny_set_ignore_channel _((char *));
	void	funny_match _((char *));
	void	reinstate_user_modes _((void));
	void	funny_print_widelist _((void));
	void	funny_list _((char *, char **));
	void	funny_mode _((char *, char **));
	void	funny_namreply _((char *, char **));
	void	update_user_mode _((char *));

#endif /* __funny_h_ */
