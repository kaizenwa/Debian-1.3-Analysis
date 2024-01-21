/*
 *  linux/ibcs/sockaf.inc
 *
 *  Copyright (C) 1994  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: sockaf.inc,v 1.4 1995/01/27 13:40:08 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/maps/sockaf.inc,v $
 */

static struct map_segment af_ibcs_to_linux[] =  {
	/* The first three entries (AF_UNSPEC, AF_UNIX and AF_INET)
	 * are identity mapped. All others aren't available under
	 * Linux, nor are Linux's AF_AX25 and AF_IPX available from
	 * SCO as far as I know.
	 */
	{ 0, 2, NULL },
	{ -1 }
};


struct map_segment *af_map[] = {
	NULL,			/* Linux */
	af_ibcs_to_linux,	/* SVR4 */
	af_ibcs_to_linux,	/* SVR3 */
	af_ibcs_to_linux,	/* SCO 3.2.[24] */
	af_ibcs_to_linux,	/* Wyse Unix V/386 3.2.1 */
	af_ibcs_to_linux,	/* ISC */
	af_ibcs_to_linux,	/* BSD-ish */
	af_ibcs_to_linux	/* Xenix */
};
