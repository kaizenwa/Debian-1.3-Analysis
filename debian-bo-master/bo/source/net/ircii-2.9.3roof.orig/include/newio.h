/*
 * newio.h - header for newio.c
 *
 * written by matthew green
 *
 * copyright (c) 1995;
 *
 * @(#)$Id: newio.h,v 1.1 1995/09/03 02:04:22 mrg Exp $
 */

#ifndef __newio_h_
# define __newio_h_

#ifdef ESIX
	void	mark_socket _((int));
	void	unmark_socket _((int));
#endif
	time_t	dgets_timeout _((int));
	int	dgets _((char *, int, int, char *));
	int	new_select _((fd_set *, fd_set *, struct timeval *));
	void	new_close _((int));
	void	set_socket_options _((int));

#endif /* __newio_h_ */
