/*
 * whois.h: header for whois.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: whois.h,v 1.14 1996/03/01 10:34:35 mrg Exp $
 */

#ifndef __whois_h_
# define __whois_h_

#ifdef HAVE_STDARG_H
	void	add_to_whois_queue _((char *, void (*) (WhoisStuff *, char *, char *), char *, ...));
# else
	void	add_to_whois_queue _(());
#endif /* HAVE_STDARG_H */
	void	add_ison_to_whois _((char *, void (*) (WhoisStuff *, char *, char *)));
	void	whois_name _((char *, char **));
	void	whowas_name _((char *, char **));
	void	whois_channels _((char *, char **));
	void	whois_server _((char *, char **));
	void	whois_oper _((char *, char **));
	void	whois_lastcom _((char *, char **));
	void	whois_nickname _((WhoisStuff *, char *, char *));
	void	whois_ignore_msgs _((WhoisStuff *, char *, char *));
	void	whois_ignore_notices _((WhoisStuff *, char *, char *));
	void	whois_ignore_walls _((WhoisStuff *, char *, char *));
	void	whois_ignore_invites _((WhoisStuff *, char *, char *));
	void	whois_notify _((WhoisStuff *, char *, char *));
	void	whois_new_wallops _((WhoisStuff *, char *, char *));
	void	clean_whois_queue _((void));
	void	set_beep_on_msg _((char *));
	void    userhost_cmd_returned _((WhoisStuff *, char *, char *));
	void	user_is_away _((char *, char **));
	void	userhost_returned _((char *, char **));
	void	ison_returned _((char *, char **));
	void	whois_chop _((char *, char **));
	void	end_of_whois _((char *, char **));
	void	whoreply _((char *, char **));
	void	convert_to_whois _((void));
	void	ison_notify _((WhoisStuff *, char *, char *));
	void	no_such_nickname _((char *, char **));

extern	int	beep_on_level;
extern	char	*redirect_format;

#define	WHOIS_WHOIS	0x01
#define	WHOIS_USERHOST	0x02
#define	WHOIS_ISON	0x04
#define WHOIS_ISON2	0x08

#define	USERHOST_USERHOST ((void (*)_((WhoisStuff *, char *, char *))) 1)

#endif /* __whois_h_ */
