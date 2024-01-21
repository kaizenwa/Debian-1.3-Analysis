/*
 * dcc.h: Things dealing client to client connections. 
 *
 * Written By Troy Rollo <troy@plod.cbme.unsw.oz.au> 
 *
 * Copyright(c) 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: dcc.h,v 1.16 1995/11/04 11:32:13 mrg Exp $
 */

/*
 * this file must be included after irc.h as i needed <sys/types.h>
 * <netinet/in.h> and <apra/inet.h> and, possibly, <sys/select.h>
 */

#ifndef __dcc_h_
#define __dcc_h_

#define DCC_CHAT	((unsigned) 0x0001)
#define DCC_FILEOFFER	((unsigned) 0x0002)
#define DCC_FILEREAD	((unsigned) 0x0003)
#define DCC_TALK	((unsigned) 0x0004)
#define DCC_SUMMON	((unsigned) 0x0005)
#define	DCC_RAW_LISTEN	((unsigned) 0x0006)
#define	DCC_RAW		((unsigned) 0x0007)
#define DCC_TYPES	((unsigned) 0x000f)

#define DCC_WAIT	((unsigned) 0x0010)
#define DCC_ACTIVE	((unsigned) 0x0020)
#define DCC_OFFER	((unsigned) 0x0040)
#define DCC_DELETE	((unsigned) 0x0080)
#define DCC_TWOCLIENTS	((unsigned) 0x0100)
#ifdef NON_BLOCKING_CONNECTS
#define DCC_CNCT_PEND	((unsigned) 0x0200)
#endif
#define DCC_STATES	((unsigned) 0xfff0)

#define DCC_TALK_CHECK 0
#define DCC_TALK_INVITE 1
#define DCC_TALK_ANNOUNCE 2
#define DCC_TALK_DELETE_LOCAL 3
#define DCC_TALK_DELETE_REMOTE 4
#define DCC_TALK_SUMMON 5
#define DCC_TALK_DELETE_SUMMON 6

	DCC_list * dcc_searchlist _((char *, char *, int, int, char *));
	void	dcc_erase _((DCC_list *));
	void	register_dcc_offer _((char *, char *, char *, char *, char *, char *));
	void	process_dcc _((char *));
	char	*dcc_raw_connect _((char *, u_short));
	char	*dcc_raw_listen _((u_short));
	void	dcc_list _((char *));
	void	dcc_chat_transmit _((char *, char *));
	void	dcc_message_transmit _((char *, char *, int, int));
	int	send_talk_control _((DCC_list *, int));
	void	close_all_dcc _((void));
	void	set_dcc_bits _((fd_set *, fd_set *));
	void	dcc_check _((fd_set *, fd_set *));

#endif /* __dcc_h_ */
