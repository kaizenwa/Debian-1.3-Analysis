/*
 * irc_std.h: header to define things used in all the programs ircii
 * comes with
 *
 * hacked together from various other files by matthew green
 * copyright(c) 1993 
 *
 * See the copyright file, or do a help ircii copyright 
 *
 * @(#)$Id: irc_std.h,v 1.20.2.1 1995/10/25 19:03:59 glen Exp $
 */

#ifndef __irc_std_h
#define __irc_std_h

#undef _
#undef const
#undef volatile
#if defined(__STDC__)
# define _(a) a
#else
# define _(a) ()
# define const
# define volatile
#endif

#ifndef __GNUC__
#define __inline
#endif

#ifdef sparc
#include <fcntl.h>
#endif

#ifndef WINNT
#ifdef _IBMR2
# include <sys/errno.h>
# include <sys/select.h>
#else
# include <errno.h>
extern	int	errno;
#endif /* _IBMR2 */

#ifndef NBBY
# define NBBY	8		/* number of bits in a byte */
#endif /* NBBY */

#ifndef NFDBITS
# define NFDBITS	(sizeof(long) * NBBY)	/* bits per mask */
#endif /* NFDBITS */

/* Most BSD systems have getdtablesize() which is the best way
to get the number of FDs available..  */
#ifdef HAVE_SYS_SYSLIMITS_H
# include <sys/syslimits.h>
#endif
   
/* Fetch the value of ULONG_MAX */
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
   
#ifndef OPEN_MAX
# define OPEN_MAX 64
# undef FD_SETSIZE
# define FD_SETSIZE 64
#endif
   
#ifdef FD_SET
#undef FD_SET
#endif /* FD_SET */
#define FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))

#ifdef FD_CLR
#undef FD_CLR
#endif /* FD_CLR */
#define FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))

#ifdef FD_ISSET
#undef FD_ISSET
#endif /* FD_ISSET */
#define FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))

#ifdef FD_ZERO
#undef FD_ZERO
#endif /* FD_ZERO */
#define FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

#ifndef	FD_SETSIZE
#define FD_SETSIZE	32
#endif

#ifdef HAVE_SIGACTION

typedef RETSIGTYPE sigfunc _((int));
sigfunc *my_signal _((int, sigfunc *, int));
# define MY_SIGNAL(s_n, s_h, m_f) my_signal(s_n, (sigfunc *)s_h, m_f)
#else
# if HAVE_SIGSET
#  define MY_SIGNAL(s_n, s_h, m_f) sigset(s_n, s_h)
# else
#  define MY_SIGNAL(s_n, s_h, m_f) signal(s_n, s_h)
# endif /* USE_SIGSET */
#endif /* USE_SIGACTION */

#if defined(HAVE_SIGACTION) || defined(HAVE_SIGSET)
# undef SYSVSIGNALS
#endif

#if defined(__svr4__) && !defined(SVR4)
# define SVR4
#else
# if defined(SVR4) && !defined(__svr4__)
#  define __svr4__
# endif
#endif

#ifdef _SEQUENT_
# define	u_short	ushort
# define	u_char	unchar
# define	u_long	ulong
# define	u_int	uint
# define	USE_TERMIO
# ifndef POSIX
#  define POSIX
# endif
#endif /* _SEQUENT_ */

#ifndef NeXT
# if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#  include <string.h>
#  if defined(STDC_HEADERS)
#   include <stdlib.h>
#  endif /* HAVE_STDLIB_H */
#  if defined(HAVE_MEMORY_H)
#   include <memory.h>
#  endif /* HAVE_MEMORY_H */
#  undef index
#  undef rindex
#  undef bcopy
#  undef bzero
#  undef bcmp
#  define index strchr
#  define rindex strrchr
#  ifdef HAVE_MEMMOVE
#   define bcopy(s, d, n) memmove((d), (s), (n))
#  else
#   define bcopy(s, d, n) memcpy ((d), (s), (n))
#  endif
#  define bcmp(s, t, n) memcmp ((s), (t), (n))
#  define bzero(s, n) memset ((s), 0, (n))
# else /* STDC_HEADERS || HAVE_STRING_H */
#  include <strings.h>
# endif /* STDC_HEADERS || HAVE_STRING_H */
#endif /* !NeXT */

#ifndef SYS_ERRLIST_DECLARED
extern	char	*sys_errlist[];
extern	int	sys_nerr;
#endif

#ifdef HAVE_BSDGETTIMEOFDAY
#define gettimeofday BSDgettimeofday
#endif

#ifdef GETTOD_NOT_DECLARED
extern	int	gettimeofday(struct timeval *tv, struct timezone *tz);
#endif

#ifdef NEED_STRERROR
# undef strerror
# define strerror(e) ((e) < 0 || (e) >= sys_nerr ? "(unknown)" : sys_errlist[e])
#endif

/* we need an unsigned 32 bit integer for dcc, how lame */

#ifdef UNSIGNED_LONG32

typedef		unsigned long		u_32int_t;

#else
# ifdef UNSIGNED_INT32

typedef		unsigned int		u_32int_t;

# else

typedef		unsigned long		u_32int_t;

# endif /* UNSIGNED_INT32 */
#endif /* UNSIGNED_LONG32 */

#ifdef SVR4
#include <crypt.h>
#endif

#if defined(_AIX)
int getpeername _((int s, struct sockaddr *, int *));
int getsockname _((int s, struct sockaddr *, int *));
int socket _((int, int, int));
int bind _((int, struct sockaddr *, int));
int listen _((int, int));
int accept _((int, struct sockaddr *, int *));
int recv _((int, void *, int, unsigned int));
int send _((int, void *, int, unsigned int));
int gettimeofday _((struct timeval *, struct timezone *));
int gethostname _((char *, int));
int setsockopt _((int, int, int, void *, int));
int setitimer _((int, struct itimerval *, struct itimerval *));
int ioctl _((int, int, ...));

#endif
#endif /* WINNT */

#endif /* __irc_std_h */

