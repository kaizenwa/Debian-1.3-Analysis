/* @(#)types.h	1.2 87/11/07 3.9 RPCSRC */
/*      @(#)types.h 1.18 87/07/24 SMI      */
/*
 * Rpc additions to <sys/types.h>
 */
#ifndef __TYPES_RPC_HEADER__
#define __TYPES_RPC_HEADER__

#define	bool_t	int
#define	enum_t	int
#undef FALSE
#define	FALSE	(0)
#undef TRUE
#define	TRUE	(1)
#define __dontcare__	-1
#ifndef NULL
#	define NULL 0
#endif

#ifdef __STDC__
#undef STDC_INCLUDES
#define STDC_INCLUDES
#ifndef unix
long ntohl(long);
long htonl(long);
#endif /* !unix */
#endif

#ifdef STDC_INCLUDES
#include <stddef.h>
#include <stdlib.h>
#endif
#if UD_NO_MALLOC_DECL
extern char *malloc();
#endif
#define mem_alloc(bsize)	malloc(bsize)
#define mem_free(ptr, bsize)	free(ptr)

#ifdef unix
#   ifndef makedev /* ie, we haven't already included it */
#   	if __DJGPP__ <= 1
#	    include <sys/types.h>
#   	else
	    typedef	unsigned char	u_char;
	    typedef	unsigned short	u_short;
	    typedef	unsigned int	u_int;
	    typedef	unsigned long	u_long;
	    typedef	char *		caddr_t;
#       endif	/* __DJGPP__ > 1 */
#   endif
#else /* unix */
#   if defined(OS2) && defined(__GNUC__)
#	include <sys/types.h>
#   else
	typedef	unsigned char	u_char;
	typedef	unsigned short	u_short;
	typedef	unsigned int	u_int;
	typedef	unsigned long	u_long;
	typedef	char *		caddr_t;
#   endif /* OS2 && __GNUC__ */
#endif /* unix */

#ifdef vms
#define __SOCKET_TYPEDEFS
#endif

#endif /* !__TYPES_RPC_HEADER__ */
