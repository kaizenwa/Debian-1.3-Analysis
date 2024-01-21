/*
 * names.h: Header for names.c
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: names.h,v 1.20 1995/09/03 13:45:22 mrg Exp $
 */

#ifndef __names_h_
#define __names_h_

#include "window.h"
#include "irc.h"

/* from names.c - "iklmnpst" */
#define MODE_INVITE	((u_long) 0x0001)
#define MODE_KEY	((u_long) 0x0002)
#define MODE_LIMIT	((u_long) 0x0004)
#define MODE_MODERATED	((u_long) 0x0008)
#define MODE_MSGS	((u_long) 0x0010)
#define MODE_PRIVATE	((u_long) 0x0020)
#define MODE_SECRET	((u_long) 0x0040)
#define MODE_TOPIC	((u_long) 0x0080)

/* for lookup_channel() */
#define	CHAN_NOUNLINK	1
#define CHAN_UNLINK	2

#define	GOTNAMES	0x01
#define	GOTMODE		0x02

	void	add_to_join_list _((char *, int, int));
	void	remove_from_join_list _((char *, int));
	char	*get_chan_from_join_list _((int));
	int	get_win_from_join_list _((char *, int));
	int	in_join_list _((char *, int));
	void	got_info _((char *, int, int));
	
	int	is_channel_mode _((char *, int, int));
	int	is_chanop _((char *, char *));
	ChannelList	*lookup_channel _((char *, int, int));
	char	*get_channel_mode _((char *, int));
#ifdef	INCLUDE_UNUSED_FUNCTIONS
	void	set_channel_mode _((char *, int, char *));
#endif /* INCLUDE_UNUSED_FUNCTIONS */
	void	add_channel _((char *, int));
	void	add_to_channel _((char *, char *, int, int, int));
	void	remove_channel _((char *, int));
	void	remove_from_channel _((char *, char *, int));
	int	is_on_channel _((char *, int, char *));
	void	list_channels _((void));
	void	reconnect_all_channels _((int));
	void	switch_channels _((unsigned char, char *));
	char	*what_channel _((char *, int));
	char	*walk_channels _((char *, int, int));
	char	*real_channel _((void));
	void	rename_nick _((char *, char *, int));
	void	update_channel_mode _((char *, int, char *));
	void	set_channel_window _((Window *, char *, int));
	char	*create_channel_list _((Window *));
	int	get_channel_oper _((char *, int));
	void	channel_server_delete _((int));
	void	change_server_channels _((int, int));
	void	clear_channel_list _((int));
	void	set_waiting_channel _((int));
	void	remove_from_mode_list _((char *, int));
	int	chan_is_connected _((char *, int));

#endif /* __names_h_ */
