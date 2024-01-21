/*
 * $Source: /usr/local/src/pax/RCS/dirent.h,v $
 *
 * $Revision: 2.1 $
 *
 * dirent.h - MSDOS/OS/2 definitions for SVR3 directory access routines
 *
 * AUTHOR
 *
 *      Michael Rendell ({uunet,utai}michael@garfield),
 *	Modified by Ian Stewartson (IanStewartson@dial.pipex.com)
 *
 * COPYRIGHT
 *
 *	Redistribution and use in source and binary forms are permitted
 *	provided that the above copyright notice and this paragraph are
 *	duplicated in all such forms and that any documentation,
 *	advertising materials, and other materials related to such
 *	distribution and use acknowledge that the software was developed
 *	by Michael Rendell.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * $Log: dirent.h,v $
 * Revision 2.1  1996/10/18  21:41:08  istewart
 * Initial 2.1 port
 *
 */

#ifndef _DIRENT_H
#  define _DIRENT_H

#  include <limits.h>

#  define MAXNAMLEN	512	/* maximum filename length		*/

#  ifndef NAME_MAX
#    define NAME_MAX	(MAXNAMLEN - 1)
#  endif

struct dirent			/* data from getdents()/readdir()	*/
{
    ino_t		d_ino;	/* inode number of entry		*/
    off_t		d_off;	/* offset of disk directory entry	*/
    unsigned short	d_reclen;/* length of this record		*/
    char		d_name[MAXNAMLEN + 1];
};

/*
 * Prototype definitions for Standard and Non-standard compilers
 */

#undef _CDECL
#undef _FAR_
#define _CDECL
#define _FAR_

#if defined (MSDOS) && !defined (WIN32)

#  if defined(_DLL) && !defined(_MT)
#    error Cannot define _DLL without _MT
#  endif

#  ifdef _MT
#    undef _FAR_
#    define _FAR_	_far
#  endif

#  ifndef __STDC__
#    define __STDC__	1
#  endif

#  ifndef __WATCOMC__
#    undef _CDECL
#    define _CDECL	cdecl
#  endif
#endif

#if defined (WIN32)
#  undef _CDECL
#  define _CDECL	__cdecl
#endif

#ifdef __TURBOC__
#  undef _CDECL
#  define _CDECL	_Cdecl
#endif

#if !defined (__STDC__) && !defined (__TURBOC__)
#  undef  const
#  undef  volatile
#endif

/*
 * Define NULL
 */

#ifndef NULL
#  ifdef MSDOS
#    if (_MSC_VER >= 600)
#      define NULL	((void *)0)

#    elif (defined(M_I86SM) || defined(M_I86MM))
#      define NULL	0

#    else
#      define NULL	0L
#    endif
#  elif defined(__STDC__)
#      define NULL	((void *)0)
#  else
#      define NULL	0
#  endif
#endif

/* Need size_t definition */

#  if !defined(_SIZE_T_DEFINED) && !defined(__size_t) && !defined(_SIZE_T) && !defined(_SIZE_T_DEFINED_) 

typedef unsigned int	size_t;

#    define _SIZE_T_DEFINED
#    define _SIZE_T_DEFINED_
#    define _SIZE_T
#    define __size_t
#  endif

#  ifndef _BOOL_T_DEFINED
typedef unsigned char	bool;
#    define _BOOL_T_DEFINED
#  endif

#  ifndef FALSE
#    define FALSE	((bool)0)	/* Boolean 'false'		*/
#  endif
#  ifndef TRUE
#    define TRUE	((bool)1)	/* Boolean 'true'		*/
#  endif

/* MSDOS versions and OS2 ?.x version */

struct _dircontents {
    char		*_d_entry;
    struct _dircontents	*_d_next;
};

typedef struct _dirdesc {
    int			dd_id;	/* uniquely identify each open directory */
    long		dd_loc;	/* where we are in directory entry is this */
    struct _dircontents	*dd_contents;	/* pointer to contents of dir	*/
    struct _dircontents	*dd_cp;		/* pointer to current position	*/
} DIR;

/* Functions */

extern DIR _FAR_ * _FAR_ _CDECL	opendir	(const char _FAR_ *);
extern struct dirent _FAR_ * _FAR_ _CDECL readdir (DIR _FAR_ *);
extern void _FAR_ _CDECL	rewinddir (DIR _FAR_ *);

extern int _FAR_ _CDECL		closedir (DIR _FAR_ *);
extern void _FAR_ _CDECL	seekdir	(DIR _FAR_ *, off_t);
extern off_t _FAR_ _CDECL	telldir	(DIR _FAR_ *);

extern int _FAR_ _CDECL		chdir (const char _FAR_ *);
extern char _FAR_ * _FAR_ _CDECL getcwd (char _FAR_ *, size_t);

#  ifdef __TURBOC__
extern int _FAR_ _CDECL		mkdir (const char _FAR_ *);
#  elif defined (__32BIT__) && defined (__EMX__)
extern int _FAR_ _CDECL		mkdir (const char _FAR_ *, long);
#  else
extern int _FAR_ _CDECL		mkdir (const char _FAR_ *, int);
#  endif

extern int _FAR_ _CDECL		rmdir (const char _FAR_ *);

#  if defined (MSDOS) || defined (__OS2__) || defined (WIN32)
extern int _FAR_ _CDECL		_chdrive (int);
extern int _FAR_ _CDECL		_getdrive (void);
extern char _FAR_ * _FAR_ _CDECL _getdcwd (int, char _FAR_ *, int);
#  endif

#  if defined (OS2) || defined (__OS2__) || defined (WIN32) || defined (__EMX__)
extern bool			IsHPFSFileSystem (char _FAR_ *);
#  endif
#endif
