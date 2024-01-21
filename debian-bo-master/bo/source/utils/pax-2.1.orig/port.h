/* $Source: /usr/local/src/pax/RCS/port.h,v $
 *
 * $Revision: 2.1 $
 *
 * port.h - defnitions for portability library
 *
 * DESCRIPTION
 *
 *	Header for maintaing portablilty across operating system and
 *	version boundries.  For the most part, this file contains
 *	definitions which map functions which have the same functionality
 *	but different names on different systems, to have the same name.
 *
 * AUTHORS
 *
 *	Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
 *	John Gilmore (gnu@hoptoad)
 *
 * COPYRIGHT
 *
 *	Copyright (c) 1989 Mark H. Colburn.  All rights reserved.
 *
 *	Redistribution and use in source and binary forms are permitted
 *	provided that the above copyright notice and this paragraph are
 *	duplicated in all such forms and that any documentation,
 *	advertising materials, and other materials related to such
 *	distribution and use acknowledge that the software was developed
 *	by Mark H. Colburn.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef _PAX_PORT_H
#define _PAX_PORT_H

/*
 * Everybody does wait() differently.  There seem to be no definitions for
 * this in V7 (e.g. you are supposed to shift and mask things out using
 * constant shifts and masks.)  In order to provide the functionality, here
 * are some non standard but portable macros.  Don't change to a "union wait"
 * based approach -- the ordering of the elements of the struct depends on the
 * byte-sex of the machine.
 */

#define	TERM_SIGNAL(status)	((status) & 0x7F)
#define TERM_VALUE(status)	((status) >> 8)

#ifdef MSDOS
#include <io.h>
#define	major(x)	(0)
#define	minor(x)	(0)
#define	makedev(x,y)	(0)
#endif

#ifndef HAVE_PWD_H
struct passwd {
    char	       *pw_name;
    char	       *pw_passwd;
    int			pw_uid;
    int			pw_gid;
    int			pw_quota;
    char	       *pw_comment;
    char	       *pw_gecos;
    char	       *pw_dir;
    char	       *pw_shell;
};
#endif

#ifndef HAVE_GRP_H
struct group {
    char	       *gr_name;
    char	       *gr_passwd;
    int			gr_gid;
    char	      **gr_mem;
};
#endif

#if defined (HAVE_STRCHR)
#  undef  index
#  define index strchr
#  undef  rindex
#  define rindex strrchr
#endif


/*
 * Tape/Disk raw I/O
 */

#ifdef MSDOS

#if defined(DIO)

#define OPEN2(p,f) \
	( (dio_open_check(p) < 0) ? dio_open2(p,f) : open(p,f) )
#define OPEN3(p,f,m) \
	( (dio_open_check(p) < 0) ? dio_open3(p,f,m) : open(p,f,m) )
#define CLOSE(h) \
	( (h < 0) ? dio_close(h) : close(h) )
#define READ(h,b,c) \
	( (h < 0) ? dio_read(h,b,c) : read(h,b,c) )
#define WRITE(h,b,c) \
	( (h < 0) ? dio_write(h,b,c) : write(h,b,c) )
#define LSEEK(h,o,r) \
	( (h < 0) ? dio_lseek(h,o,r) : lseek(h,o,r) )

#elif defined(DISKACC)

#define _DSKBIAS                128

#define dskdev(path)            ((path[1] == ':' && path[2] == 0))
#define isdsk(fd)               ((fd) >= _DSKBIAS)

#define OPEN2(path,oflag)       (dskdev(path) ? dsk_open(path, oflag, 0) + _DSKBIAS : open(path, oflag))
#define OPEN3(path,oflag,mode)  (dskdev(path) ? dsk_open(path, oflag, mode) + _DSKBIAS : open(path, oflag, mode))
#define READ(fd, buf, n)        (isdsk(fd) ? dsk_read(fd - _DSKBIAS, buf, n) : read(fd, buf, n))
#define WRITE(fd, buf, n)       (isdsk(fd) ? dsk_write(fd - _DSKBIAS, buf, n) : write(fd, buf, n))
#define LSEEK(fd, off, wh)      (isdsk(fd) ? dsk_lseek(fd - _DSKBIAS, off, wh) : lseek(fd, off, wh))
#define CLOSE(fd)               (isdsk(fd) ? dsk_close(fd - _DSKBIAS) : close(fd))

#else /* NOT DISKACC */

#define OPEN2(p,f) open(p,f)
#define OPEN3(p,f,m) open(p,f,m)
#define CLOSE(h) close(h)
#define READ(h,b,c) read(h,b,c)
#define WRITE(h,b,c) write(h,b,c)
#define LSEEK(h,o,r) lseek(h,o,r)

#endif

#else /* NOT MSDOS */

#define OPEN2(p,f) open(p,f)
#define OPEN3(p,f,m) open(p,f,m)
#define CLOSE(h) close(h)
#define READ(h,b,c) read(h,b,c)
#define WRITE(h,b,c) write(h,b,c)
#define LSEEK(h,o,r) lseek(h,o,r)

#endif /* MSDOS */
#endif /* _PAX_PORT_H */
