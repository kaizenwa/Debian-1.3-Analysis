/*
 *  linux/ibcs/sockopt.inc
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: sockopt.inc,v 1.5 1995/03/02 11:48:42 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/maps/sockopt.inc,v $
 */

/* This is yukky - because the iBCS option codes use bit positions
 * we have to map each one individually rather than using range
 * vectors.
 */
static struct map_segment sopt_ibcs_to_linux[] =  {
	{ 0x0001, 0x0001, (char *)SO_DEBUG },
	{ 0x0002, 0x0002, (char *)SO_ACCEPTCONN },
	{ 0x0004, 0x0004, (char *)SO_REUSEADDR },
	{ 0x0008, 0x0008, (char *)SO_KEEPALIVE },
	{ 0x0010, 0x0010, (char *)SO_DONTROUTE },
	{ 0x0020, 0x0020, (char *)SO_BROADCAST },
	{ 0x0040, 0x0040, (char *)SO_USELOOPBACK },
	{ 0x0080, 0x0080, (char *)SO_LINGER },
	{ 0x0100, 0x0100, (char *)SO_OOBINLINE },
	{ 0x0200, 0x0200, (char *)SO_ORDREL },
	{ 0x0400, 0x0400, (char *)SO_IMASOCKET },
	{ 0x1001, 0x1001, (char *)SO_SNDBUF },
	{ 0x1002, 0x1002, (char *)SO_RCVBUF },
	{ 0x1003, 0x1003, (char *)SO_SNDLOWAT },
	{ 0x1004, 0x1004, (char *)SO_RCVLOWAT },
	{ 0x1005, 0x1005, (char *)SO_SNDTIMEO },
	{ 0x1006, 0x1006, (char *)SO_RCVTIMEO },
	{ 0x1007, 0x1007, (char *)SO_ERROR },
	{ 0x1008, 0x1008, (char *)SO_TYPE },
	{ 0x1009, 0x1009, (char *)SO_PROTOTYPE },
	{ -1 }
};


struct map_segment *sopt_map[] = {
	NULL,			/* Linux */
	sopt_ibcs_to_linux,	/* SVR4 */
	sopt_ibcs_to_linux,	/* SVR3 */
	sopt_ibcs_to_linux,	/* SCO 3.2.[24] */
	sopt_ibcs_to_linux,	/* Wyse Unix V/386 3.2.1 */
	sopt_ibcs_to_linux,	/* ISC */
	sopt_ibcs_to_linux,	/* BSD-ish */
	sopt_ibcs_to_linux	/* Xenix */
};
