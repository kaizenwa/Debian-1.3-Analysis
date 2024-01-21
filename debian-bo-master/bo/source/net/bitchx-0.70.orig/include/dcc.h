/*
 * dcc.h: Things dealing client to client connections. 
 *
 * Written By Troy Rollo <troy@plod.cbme.unsw.oz.au> 
 *
 * Copyright(c) 1991 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: dcc.h,v 1.15.2.1 1995/10/25 18:56:26 glen Exp $
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
#define	DCC_RAW_LISTEN	((unsigned) 0x0004)
#define	DCC_RAW		((unsigned) 0x0005)
#define DCC_RESENDOFFER	((unsigned) 0x0006)
#define DCC_REGETFILE	((unsigned) 0x0007)
#define DCC_BOTMODE	((unsigned) 0x0008)
#define DCC_TYPES	((unsigned) 0x000f)

#define DCC_WAIT	((unsigned) 0x0010)
#define DCC_ACTIVE	((unsigned) 0x0020)
#define DCC_OFFER	((unsigned) 0x0040)
#define DCC_DELETE	((unsigned) 0x0080)
#define DCC_TWOCLIENTS	((unsigned) 0x0100)
#ifdef NON_BLOCKING_CONNECTS
#define DCC_CNCT_PEND	((unsigned) 0x0200)
#endif
#define DCC_QUEUE	((unsigned) 0x0400)
#define DCC_STATES	((unsigned) 0xfff0)

#define DCC_PACKETID  0xfeab
#define MAX_DCC_BLOCK_SIZE 4096

	void	register_dcc_offer _((char *, char *, char *, char *, char *, char *, char *, char *));
	void	process_dcc _((char *));
	char	*dcc_raw_connect _((char *, u_short));
	char	*dcc_raw_listen _((int));
	void	dcc_list _((char *));
	void	dcc_chat_transmit _((char *, char *, char *));
	void	dcc_message_transmit _((char *, char *, int, int, char *));
	void	close_all_dcc _((void));
	void	dcc_check _((fd_set *, fd_set *));
	int	dcc_active _((char *));
	void	dcc_chat _((char *));
	void	dcc_reject _((char *, char *, char *));
	void	set_dcc_bits _((fd_set *, fd_set *));
	void	dcc_sendfrom_queue _((void));	
	void	dcc_check_idle _((void));
	void	dcc_glist _((char *));
	void	dcc_stats _((char *));
	void	dcc_chatbot _((char *));
DCC_list	*dcc_searchlist _((char *, char *, int, int, char *, char *, int));
	void	dcc_chat_crash_transmit _((char *, char *));
	int	dcc_erase _((DCC_list *));
	void	dcc_chat _((char *));
	void	dcc_filesend _((char *));
	void	dcc_list _((char *));
	char	*dcc_time _((time_t));
	void	dcc_stats _((char *));
	void	multiget _((char *, char *));
	void	multisend _((char *, char*));
	void	dcc_resend _((char *));
	void	dcc_regetfile _((char *));
	int	dcc_activebot _((char *));
	void	dcc_bot_transmit _((char *, char *, char *));
	                	                        
	extern	DCC_list *ClientList;

	int	dcc_printf _((int, char *format, ...));
	void	tandout_but _((int, char *format, ...));
	void	chanout_but _((int, char *format, ...));
	void	userhost_clink _((WhoisStuff *, char *, char *));
	int	handle_tcl_chan _((int, char *, char *, char *));
	int	tand_chan _((int, char *));
	int	tand_zapf _((int, char *));
	int	tand_zapfbroad _((int, char *));
	int	handle_dcc_bot _((int, char *));
	int	tandem_join _((int, char *));
	int	tandem_part _((int, char *));
	int	send_who_to _((int, char *from, int));
	int	tand_who _((int, char *));
	int	tand_whom _((int, char *));
	int	tell_who _((int, char *));
	int	send_who _((int, char *));
	int	tell_whom _((int, char *));
	int	send_whom _((int, char *));
	int	tand_priv _((int, char *));
	int	tand_boot _((int, char *));
	int	tand_privmsg _((int, char *));
	int	cmd_cmsg _((int, char *));
	int	cmd_cboot _((int, char *));
	int	cmd_act _((int, char *));
	int	cmd_help _((int, char *));
	int	cmd_msg _((int, char *));
	int	cmd_say _((int, char *));
	int	cmd_tcl _((int, char *));
	int	cmd_chat _((int, char *));
	int	cmd_quit _((int, char *));
	void	invite_dcc_chat _((WhoisStuff *, char *, char *));
	int	cmd_invite _((int, char *));
	int	cmd_echo _((int, char *));
				
#endif /* __dcc_h_ */
