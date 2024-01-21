/*
 * flood.h: header file for flood.c
 *
 * @(#)$Id: flood.h,v 1.7 1995/08/31 03:51:24 scottr Exp $
 */

#ifndef __flood_h_
#define __flood_h_

	int	check_flooding _((char *, int, char *, char *));
	int	is_other_flood _((ChannelList *, NickList *, int));
	void	flood_prot _((char *, char *, char *, int, int, char *));
		
#define MSG_FLOOD 0
#define PUBLIC_FLOOD 1
#define NOTICE_FLOOD 2
#define WALL_FLOOD 3
#define WALLOP_FLOOD 4
#define CTCP_FLOOD 5
#define INVITE_FLOOD 6
#define CDCC_FLOOD 7
#define CTCP_ACTION_FLOOD 8
#define NUMBER_OF_FLOODS 9
#define NICK_FLOOD 9
#define DEOP_FLOOD 10
#define KICK_FLOOD 11
#define JOIN_FLOOD 12

#endif /* __flood_h_ */
