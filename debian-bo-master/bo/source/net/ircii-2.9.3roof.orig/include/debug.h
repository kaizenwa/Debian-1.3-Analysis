/*
 * debug.h - header file for phone's debug routine..
 *
 * Copyright (C) 1993, Matthew Green.
 *
 * @(#)$Id: debug.h,v 1.14 1995/09/06 22:05:03 scottr Exp $
 */

#ifndef __debug_h_
# define __debug_h_ 

# ifdef DEBUG
#  define Debug(x) debug x

#ifdef HAVE_STDARG_H
	void    debug _((int, char *, ...));
#  else
	void    debug _(());
#endif /* HAVE_STDARG_H */
	int	setdlevel _((int));
	int	getdlevel _((void));

extern	int	debuglevel;

# else
#  define Debug(x)
# endif

#endif /* __debug_h_ */
