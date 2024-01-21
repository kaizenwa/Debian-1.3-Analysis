/*
 *  linux/ibcs/sockdom.c
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: socktype.inc,v 1.5 1995/01/27 13:40:09 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/maps/socktype.inc,v $
 */

static char type_svr4_to_linux_seg1[] = {
	SOCK_DGRAM,
	SOCK_STREAM,
	0,
	SOCK_RAW,
	SOCK_RDM,
	SOCK_SEQPACKET
};

static struct map_segment type_svr4_to_linux[] = {
	/* 1 to 6 are remapped as indicated. Nothing else is valid. */
	{ 1, 6, type_svr4_to_linux_seg1 },
	{ -1 }
};

struct map_segment *type_map[] = {
	NULL,			/* Linux */
	type_svr4_to_linux,	/* SVR4 */
	NULL,			/* SVR3 */
	NULL,			/* SCO 3.2.[24] */
	NULL,			/* Wyse Unix V/386 3.2.1 */
	NULL,			/* ISC */
	NULL,			/* BSD-ish */
	NULL			/* Xenix */
};
