/*
 * output.h: header for output.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: output.h,v 1.13 1995/09/06 22:05:07 scottr Exp $
 */

#ifndef __output_h_
#define __output_h_

#ifdef HAVE_STDARG_H
	void	put_it _((char *, ...));
	void	send_to_server _((char *, ...));
	void	say _((char *, ...));
	void	bitchsay _((char *, ...));
	void	serversay _((int, char *, ...));
	void	yell _((char *, ...));
	void	help_put_it _((char *, char *, ...));
# else
	void	put_it _(());
	void	send_to_server _(());
	void	say _(());
	void	bitchsay _(());
	void	yell _(());
	void	help_put_it _(());
#endif /* HAVE_STDARG_H */

        RETSIGTYPE    sig_refresh_screen _((int));
	void	refresh_screen _((unsigned char, char *));
	void	init_screen _((void));
	void	put_file _((char *));

extern	FILE	*irclog_fp;

#endif /* __output_h_ */
