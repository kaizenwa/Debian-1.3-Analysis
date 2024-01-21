/*
 * parse.h
 *
 * written by matthew green
 * copyright (c) 1993
 *
 * @(#)$Id: parse.h,v 1.8 1995/10/12 15:19:54 mrg Exp $
 */

#ifndef __parse_h_
# define __parse_h_

	char	*PasteArgs _((char **, int));
	void	parse_server _((char *));
	void	irc2_parse_server _((char *));
	int	annoy_kicks _((int, char *, char *, char *, NickList *));
	int	ctcp_flood_check _((char *, char *, char *));
	void	load_scripts _((void));
	int	check_auto_reply _((char *));
	BanList *ban_is_on_channel _((char *ban, ChannelList *));
					
extern	char	*FromUserHost;

extern	int	doing_privmsg;
#define WAIT_WHO 0
#define WAIT_BANS 1
#define WAIT_MODE 2


#endif /* __parse_h_ */
