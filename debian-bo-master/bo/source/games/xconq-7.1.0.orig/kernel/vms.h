/* VMS Stuff
   Copyright (C) 1992, 1993, 1994 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* fd_set for select.  */

/* Number of descriptors that can fit in an `fd_set'.  */
#define	__FD_SETSIZE	256

/* It's easier to assume 8-bit bytes than to get CHAR_BIT.  */
#define	__NFDBITS	(sizeof(unsigned long int) * 8)
#define	__FDELT(d)	((d) / __NFDBITS)
#define	__FDMASK(d)	(1 << ((d) % __NFDBITS))

#define	__ptr_t		char *

typedef struct
{
  unsigned long int __bits[(__FD_SETSIZE + (__NFDBITS - 1)) / __NFDBITS];
} __fd_set;

/* This line MUST be split!  Otherwise m4 will not change it.  */
#define	__FD_ZERO(set)	\
  ((void) memset((__ptr_t) (set), 0, sizeof(fd_set)))
#define	__FD_SET(d, set)	((set)->__bits[__FDELT(d)] |= __FDMASK(d))
#define	__FD_CLR(d, set)	((set)->__bits[__FDELT(d)] &= ~__FDMASK(d))
#define	__FD_ISSET(d, set)	((set)->__bits[__FDELT(d)] & __FDMASK(d))

#define	FD_SETSIZE	__FD_SETSIZE
#define	fd_set		__fd_set
#define	FD_ZERO(set)	__FD_ZERO(set)
#define	FD_SET(d, set)	__FD_SET((d), (set))
#define	FD_CLR(d, set)	__FD_CLR((d), (set))
#define	FD_ISSET(d, set)__FD_ISSET((d), (set))
