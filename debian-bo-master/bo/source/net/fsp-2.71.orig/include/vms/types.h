/*-
 *	@(#)types.h	7.17 (Berkeley) 5/6/91
 *	VMS to Unix conversion.
 */

#ifndef _TYPES_H_
#define	_TYPES_H_

/* for normal VMS typedefs */
/*#include <types.h>
 */
typedef unsigned long int time_t;

/* for type u_char, u_short and u_long */
#include "socket.h"

/* for typedef off_t,ino_t and dev_t */
#include "stat.h"

/** Extra definitions for BSD <sys/types.h> compatibility **/

/* in types.h from iutelnet */
#ifdef vax
typedef	struct	_physadr { int r[1]; } *physadr;
typedef	struct	label_t	{
	int	val[14];
} label_t;
#endif

typedef	unsigned int	u_int;
typedef	unsigned short	ushort;		/* Sys V compatibility */

typedef	long	daddr_t;		/* disk address */
typedef	u_short	nlink_t;		/* link count */
typedef	long	swblk_t;		/* swap offset */
typedef	long	segsz_t;		/* segment size */
typedef	u_short	uid_t;			/* user id */
typedef	u_short	gid_t;			/* group id */
typedef	short	pid_t;			/* process id */
typedef	u_short	mode_t;			/* permissions */
typedef u_long	fixpt_t;		/* fixed point number */

#ifndef _POSIX_SOURCE
typedef	struct	_uquad	{ u_long val[2]; } u_quad;
typedef	struct	_quad	{   long val[2]; } quad;
typedef	long *	qaddr_t;	/* should be typedef quad * qaddr_t; */

#define	major(x)	((int)(((u_int)(x) >> 8)&0xff))	/* major number */
#define	minor(x)	((int)((x)&0xff))		/* minor number */
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))	/* create dev_t */
#endif

#ifdef	_CLOCK_T_
typedef	_CLOCK_T_	clock_t;
#undef	_CLOCK_T_
#endif

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

#ifdef	_TIME_T_
typedef	_TIME_T_	time_t;
#undef	_TIME_T_
#endif

#ifndef _POSIX_SOURCE
#define	NBBY	8		/* number of bits in a byte */

/*
 * Select uses bit masks of file descriptors in longs.  These macros
 * manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here should
 * be enough for most uses.
 */
#ifndef	FD_SETSIZE
#define	FD_SETSIZE	256
#endif

typedef long	fd_mask;
#define NFDBITS	(sizeof(fd_mask) * NBBY)	/* bits per mask */

#ifndef howmany
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#endif

typedef	struct fd_set {
	fd_mask	fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define	FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

#endif /* !_POSIX_SOURCE */
#endif /* !_TYPES_H_ */
