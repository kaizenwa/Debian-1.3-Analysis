/* $Source: /usr/local/src/pax/RCS/func.h,v $
 *
 * $Revision: 2.2 $
 *
 * func.h - function type and argument declarations
 *
 * DESCRIPTION
 *
 *	This file contains function delcarations in both ANSI style
 *	(function prototypes) and traditional style.
 *
 * AUTHOR
 *
 *     Mark H. Colburn, Open Systems Architects, Inc. (mark@minnetech.mn.org)
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

#ifndef _PAX_FUNC_H
#define _PAX_FUNC_H

/* Function Prototypes */

#ifndef __P
#  ifdef __STDC__
#    define __P(a)		a
#  else
#    define __P(a)		()
#  endif
#else
#endif

extern void		linkleft __P((void));
extern void		do_pax  __P((int, char **));
extern void		do_tar  __P ((int, char **));
extern void		add_name __P ((char *));
extern void		add_replstr __P ((char *));
extern void		do_cpio __P ((int, char **));
extern void		rpl_name __P ((char *));
extern int		get_disposition __P ((char *, char *));
extern Link		*linkfrom __P((char *, Stat *));
extern Link		*linkto __P((char *, Stat *));
extern char		*mem_get __P((uint));
extern char		*mem_str __P((char *));
extern int		ar_read __P((void));
extern int		buf_read __P((char *, uint));
extern int		buf_skip __P((off_t));
extern int		create_archive __P((void));
extern int		dirneed __P((char *));
extern void		read_archive __P((void));
extern int		inentry __P((char *, Stat *));
extern int		lineget __P((FILE *, char *));
extern int		name_match __P((char *));
extern int		name_next __P((char *, Stat *));
extern int		nameopt __P((char *));
extern int		open_archive __P((int));
extern int		open_tty __P((void));
extern int		openin __P((char *, Stat *));
extern int		openout __P((char *, Stat *, Link *, int));
extern void		pass __P((char *));
extern int		passitem __P((char *, Stat *, int, char *));
extern int		read_header __P((char *, Stat *));
extern int		wildmat __P((char *, char *));
extern void		buf_allocate __P((off_t));
extern void		close_archive __P((void));
extern void		fatal __P((char *));
extern void		name_gather __P((void));
extern void		name_init __P((int, char **));
extern void		notfound __P((void));
extern void		next __P((int));
extern int		nextask __P((char *, char *, int));
extern void		outdata __P((int, char *, off_t));
extern void		outwrite __P((char *, uint));
extern void		passdata __P((char *, int, char *, int));
extern void		print_entry __P((char *, Stat *));
extern void		warn __P((char *, char *));
extern void		warnarch __P((char *, off_t));
extern void		write_eot __P((void));
extern void		get_archive_type __P((void));
extern Link		*islink __P((char *, Stat *));
extern char		*finduname __P((uid_t));
extern char		*findgname __P((gid_t));
extern gid_t		findgid __P((char *));
extern uid_t		finduid __P((char *));
extern void		append_archive __P ((void));
extern int		get_header __P ((char *, Stat *));
extern int		dirmake __P ((char *, Stat *));
extern int		get_newname __P ((char *, int));

/*
 * REGEX functions from regexp.h
 */

extern regexp		*regcomp __P ((char *));
extern int		regexec __P ((regexp *, char *));
extern void		regsub __P ((regexp *, char *, char *));
extern void		regerror __P ((char *));

/*
 * MSDOS DIO Library
 */

#if defined (MSDOS) && defined(DIO)
extern int		dio_write  __P((int, char *, unsigned int));
extern int		dio_read  __P((int, char *, unsigned int));
extern int		dio_open_check  __P((char *));
extern int		dio_open2  __P((char *, int));
extern int		dio_open3  __P((char *, int, int));
extern int		dio_close  __P((int));
extern long		dio_lseek  __P((int, long, int));
extern void		dio_str  __P((char *));
extern int		dio_to_binary  __P ((int));
#endif /* MSDOS */

#if defined (MSDOS) && defined(DISKACC)
extern int		dsk_open  __P((char *, int, int));
extern int		dsk_close __P((int));
extern int		dsk_read __P((int, char *, unsigned));
extern int		dsk_write __P((int, char *, unsigned));
extern long		dsk_lseek __P((int, long, int));
#endif /* MSDOS */

#ifndef HAVE_MKDIR 
extern int		mkdir  __P ((char *, int));
#endif

#ifndef HAVE_RMDIR
extern int		rmdir  __P ((char *));
#endif

#ifndef HAVE_STRERROR
extern char		*strerror  __P ((int));
#endif

#ifndef HAVE_GETOPT
extern int		getopt  __P ((int, char **, char *));
#endif

#ifndef HAVE_MEMCPY
extern void		memcpy  __P ((void *, void *, unsigned int));
#endif

#ifndef HAVE_MEMSET
extern void		*memset  __P ((void *, int, unsigned int));
#endif

#ifndef HAVE_STRICMP
#  ifdef STRCASECMP
#    define stricmp(a,b)	strcasecmp ((a), (b))
#  else
extern int		stricmp  __P ((const char *, const char *));
#  endif
#endif

#ifndef HAVE_GETPWNAM
extern struct passwd	*getpwuid __P ((uid_t));
extern struct passwd	*getpwnam __P ((char *));
#endif

#ifndef HAVE_GETGRNAM
extern struct group	*getgrgid __P ((gid_t));
extern struct group	*getgrnam __P ((char *));
extern int		setgrent __P ((void));
#endif

#ifndef HAVE_GETUID
extern uid_t	getuid  __P ((void));
#endif

#ifndef HAVE_GETGID
extern gid_t	getgid  __P ((void));
#endif

#ifndef HAVE_LINK
extern int	link  __P ((char *, char *));
#endif

#ifndef HAVE_CHOWN 
extern int	chown  __P ((char *, uid_t, gid_t));
#endif

#endif /* _PAX_FUNC_H */
