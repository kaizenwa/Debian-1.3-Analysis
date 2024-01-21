/*
 * flood.h: header file for flood.c
 *
 * @(#)$Id: flood.h,v 1.7 1995/08/31 03:51:24 scottr Exp $
 */

#ifndef __flood_h_
#define __flood_h_

	int	check_flooding _((char *, int, char *));

#define MSG_FLOOD 0
#define PUBLIC_FLOOD 1
#define NOTICE_FLOOD 2
#define WALL_FLOOD 3
#define WALLOP_FLOOD 4
#define NUMBER_OF_FLOODS 5

#endif /* __flood_h_ */
