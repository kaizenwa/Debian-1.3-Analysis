/*
 * $Source: /usr/local/src/pax/RCS/ms_dir.c,v $
 *
 * $Revision: 2.1 $
 *
 * ms_dir.c - MSDOS/OS/2 SVR3 directory access routines
 *
 * AUTHOR
 *
 *      Michael Rendell ({uunet,utai}michael@garfield),
 *	Modified by Ian Stewartson (IanStewartson@dial.pipex.com)
 *
 *  Updates:  1.  To support OS/2 1.x
 *	      2.  To support HPFS long filenames
 *	      3.  To support OS/2 2.x
 *	      4.  To support TurboC
 *	      5.  To support Windows NT
 *	      6.  To support Windows 95 Long filenames
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
 * $Log: ms_dir.c,v $
 * Revision 2.1  1996/10/18  21:36:18  istewart
 * Initial 2.1 port
 *
 *
 */

#ifndef lint
static char        *ident = "$Id: ms_dir.c,v 2.1 1996/10/18 21:36:18 istewart Exp $";
#endif /* ! lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef __TURBOC__
#  include <malloc.h>
#endif

#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>

#ifdef __TURBOC__
#  include <dir.h>
#endif

#if defined (OS2) || defined (__OS2__)
#  define INCL_DOSFILEMGR
#  define INCL_DOSMISC
#  define INCL_DOSERRORS

#  ifdef NULL
#    undef NULL
#  endif

#  include <os2.h>
#  include <bseerr.h>

#  if defined (__OS2__)
#    define DISABLE_HARD_ERRORS	DosError (FERR_DISABLEHARDERR)
#    define ENABLE_HARD_ERRORS	DosError (FERR_ENABLEHARDERR)
#    define FIND_BUFFER		FILEFINDBUF3
#  else
#    ifndef HARDERROR_DISABLE
#      define HARDERROR_DISABLE	0
#    endif
#    ifndef HARDERROR_ENABLE
#      define HARDERROR_ENABLE	1
#    endif
#    define DISABLE_HARD_ERRORS	DosError (HARDERROR_DISABLE)
#    define ENABLE_HARD_ERRORS	DosError (HARDERROR_ENABLE)
#    define FIND_BUFFER		FILEFINDBUF
#  endif

#  define ERROR_EMPTY_DIR	ERROR_NO_MORE_FILES

#  ifndef HDIR_SYSTEM
#    define HDIR_SYSTEM		1
#  endif

#  ifndef FSAIL_QUERYNAME
#    define FSAIL_QUERYNAME	1
#  endif

#elif defined (WIN32)
#  include <windows.h>
#  define DISABLE_HARD_ERRORS	SetErrorMode (0)
#  define ENABLE_HARD_ERRORS	SetErrorMode (SEM_FAILCRITICALERRORS | \
					      SEM_NOOPENFILEERRORBOX);

#  define ERROR_EMPTY_DIR	ERROR_FILE_NOT_FOUND

#else
#  include <dos.h>
#  define DISABLE_HARD_ERRORS
#  define ENABLE_HARD_ERRORS
#  define ERROR_EMPTY_DIR	18
#  define ERROR_NO_MORE_FILES	18

#endif

#if defined (OS2) || defined (__OS2__)
#  ifndef FILE_NORMAL
#    define FILE_NORMAL		0x0000
#    define FILE_READONLY	0x0001
#    define FILE_HIDDEN		0x0002
#    define FILE_SYSTEM		0x0004
#    define FILE_DIRECTORY	0x0010
#    define FILE_ARCHIVED	0x0020
#  endif

#  define ATTRIBUTES		(FILE_DIRECTORY | FILE_HIDDEN | FILE_SYSTEM | \
				 FILE_NORMAL | FILE_READONLY | FILE_ARCHIVED)
#elif defined (__TURBOC__)
#  define ATTRIBUTES		(FA_RDONLY | FA_HIDDEN | FA_SYSTEM | \
				 FA_DIREC | FA_ARCH)
#else
#  define ATTRIBUTES		(_A_SUBDIR | _A_HIDDEN | _A_SYSTEM | \
				 _A_NORMAL | _A_RDONLY | _A_ARCH)
#endif

#include "dirent.h"

/*
 * OS/2 2.x has these missing
 */

#ifndef ENOTDIR
#  define ENOTDIR	120	/* Not a directory			*/
#endif

#ifndef S_IFMT
#  define	S_IFMT	0xf000	/* type of file				*/
#endif

#ifndef S_ISDIR
#  define S_ISDIR(m)	((((m) & S_IFMT) == S_IFDIR))
#endif

/*
 * Internals
 */

typedef struct _dircontents	DIRCONT;
static void			free_dircontents (DIRCONT *);
static char			*_OpenDirCommon (const char *);

/*
 * Disable Windows 95 support - requires some assember stuff
 */

#define	NO_WINDOWS95_SUPPORT

/*
 * Some common front-end processing.
 */

static char		*_OpenDirCommon (const char *name)
{
    struct stat		statb;
    char		*last;
    char		*nbuf;
    int			rc = strlen (name);

    if (!rc)
    {
	errno = ENOTDIR;
	return (char *)NULL;
    }

    if ((nbuf = malloc (rc + 5)) == (char *)NULL)
	return (char *) NULL;

    strcpy (nbuf, name);
    last = &nbuf[rc - 1];

/* Ok, DOS is very picky about its directory names.  The following are
 * valid.
 *
 *  c:/
 *  c:.
 *  c:name/name1
 *
 *  c:name/ is not valid
 */

    if (((*last == '\\') || (*last == '/')) && (rc > 1) &&
	(!((rc == 3) && (name[1] == ':'))))
	*(last--) = 0;

/* Check its a directory */

    DISABLE_HARD_ERRORS;
    rc = stat (nbuf, &statb);
    ENABLE_HARD_ERRORS;

    if (rc)
    {
	free (nbuf);
	return (char *) NULL;
    }

    if (!S_ISDIR (statb.st_mode))
    {
	free (nbuf);
	errno = ENOTDIR;
	return (char *)NULL;
    }

/* Set up to find everything */

    if ((*last != '\\') && (*last != '/'))
	strcat (last, "/");

    strcat (last, "*.*");

    return nbuf;
}

/*
 * OS/2 version.  Open the directory stream
 */

#if defined (__OS2__) || defined (OS2)

DIR		*opendir (name)
const char	*name;
{
    DIR			*dirp;
    DIRCONT		*dp;
    char		*nbuf;
    int			len = strlen (name);
    unsigned long	rc;
    FIND_BUFFER		dtabuf;
#if defined (__OS2__)
    HDIR		d_handle = HDIR_SYSTEM;
    ULONG		d_count = 1;
    bool		HPFS = FALSE;
#elif defined (OS2)
    HDIR		d_handle = HDIR_SYSTEM;
    USHORT		d_count = 1;
    bool		HPFS = FALSE;
#endif

    if ((nbuf = _OpenDirCommon (name)) == (char *)NULL)
	return (DIR *) NULL;

    if ((dirp = (DIR *) calloc (sizeof (DIR), 1)) == (DIR *) NULL)
    {
	free (nbuf);
	return (DIR *) NULL;
    }

/* Find the file system type */

    HPFS = IsHPFSFileSystem (nbuf);

    DISABLE_HARD_ERRORS;

#if defined (__OS2__)
    rc = DosFindFirst (nbuf, &d_handle, ATTRIBUTES, &dtabuf,
		       sizeof (FILEFINDBUF3), &d_count, FIL_STANDARD);
#else
    rc = DosFindFirst (nbuf, &d_handle, ATTRIBUTES, &dtabuf,
		       sizeof (FILEFINDBUF), &d_count, (ULONG)0);
#endif

    ENABLE_HARD_ERRORS;

/* Check for errors */

    if (rc)
    {
	free (nbuf);

/* Empty directory */

#if defined (ERROR_EMPTY_DIR)
	if (rc == ERROR_EMPTY_DIR)
	    return dirp;
#endif

	free (dirp);
	return (DIR *) NULL;
    }

/* Process the directory */

    do
    {
	if (((dp = (DIRCONT *) malloc (sizeof (DIRCONT))) == (DIRCONT *)NULL) ||
	    ((dp->_d_entry = strdup (dtabuf.achName)) == (char *) NULL))
	{
	    if (dp->_d_entry != (char *)NULL)
		free ((char *)dp);

	    free (nbuf);
	    free_dircontents (dirp->dd_contents);

	    DosFindClose (d_handle);
	    return (DIR *) NULL;
	}

	if (!HPFS)
	    strlwr (dp->_d_entry);

	if (dirp->dd_contents != (DIRCONT *) NULL)
	    dirp->dd_cp = dirp->dd_cp->_d_next = dp;

	else
	    dirp->dd_contents = dirp->dd_cp = dp;

	dp->_d_next = (DIRCONT *) NULL;

	d_count = 1;
    } while (DosFindNext (d_handle, &dtabuf, sizeof (FILEFINDBUF),
			  &d_count) == 0);

    dirp->dd_cp = dirp->dd_contents;
    free (nbuf);

    DosFindClose (d_handle);
    return dirp;
}

/*
 * Windows 32 - Open the directory stream
 */

#elif defined (WIN32)
DIR		*opendir (name)
const char	*name;
{
    DIR			*dirp;
    DIRCONT		*dp;
    char		*nbuf;
    unsigned long	rc;
    WIN32_FIND_DATA	dtabuf;
    HANDLE		d_handle;
    bool		HPFS = FALSE;

    if ((nbuf = _OpenDirCommon (name)) == (char *)NULL)
	return (DIR *) NULL;

    if ((dirp = (DIR *) calloc (sizeof (DIR), 1)) == (DIR *) NULL)
    {
	free (nbuf);
	return (DIR *) NULL;
    }

/* Find the file system type */

    HPFS = IsHPFSFileSystem (nbuf);

    DISABLE_HARD_ERRORS;
    d_handle = FindFirstFile (nbuf, &dtabuf);
    rc = (d_handle == INVALID_HANDLE_VALUE) ? GetLastError () : 0;
    ENABLE_HARD_ERRORS;

/* Check for errors */

    if (rc)
    {
	free (nbuf);

/* Empty directory */

#if defined (ERROR_EMPTY_DIR)
	if (rc == ERROR_EMPTY_DIR)
	    return dirp;
#endif

	free (dirp);
	return (DIR *) NULL;
    }

/* Process the directory */

    do
    {
	if (((dp = (DIRCONT *) malloc (sizeof (DIRCONT))) == (DIRCONT *)NULL) ||
	    ((dp->_d_entry = strdup (dtabuf.cFileName)) == (char *) NULL))
	{
	    if (dp->_d_entry != (char *)NULL)
		free ((char *)dp);

	    free (nbuf);
	    free_dircontents (dirp->dd_contents);

	    FindClose (d_handle);
	    return (DIR *) NULL;
	}

	if (!HPFS)
	    strlwr (dp->_d_entry);

	if (dirp->dd_contents != (DIRCONT *) NULL)
	    dirp->dd_cp = dirp->dd_cp->_d_next = dp;

	else
	    dirp->dd_contents = dirp->dd_cp = dp;

	dp->_d_next = (DIRCONT *) NULL;

    } while (FindNextFile (d_handle, &dtabuf));

    dirp->dd_cp = dirp->dd_contents;
    free (nbuf);

    FindClose (d_handle);
    return dirp;
}

/*
 * MSDOS Turbo Version - Open the directory stream
 */

#elif defined (__TURBOC__)
DIR		*opendir (name)
const char	*name;
{
    DIR			*dirp;
    DIRCONT		*dp;
    char		*nbuf;
    struct ffblk	dtabuf;

    if ((nbuf = _OpenDirCommon (name)) == (char *)NULL)
	return (DIR *) NULL;

    if ((dirp = (DIR *) calloc (sizeof (DIR), 1)) == (DIR *) NULL)
    {
	free (nbuf);
	return (DIR *) NULL;
    }

    if (findfirst (nbuf, &dtabuf, ATTRIBUTES))
    {
	free (nbuf);

/* Empty directory */

#if defined (ERROR_EMPTY_DIR)
	if (rc == ERROR_EMPTY_DIR)
	    return dirp;
#endif

	free (dirp);
	return (DIR *) NULL;
    }

/* Process the directory */

    do
    {
	if (((dp = (DIRCONT *) malloc (sizeof (DIRCONT))) == (DIRCONT *)NULL) ||
	    ((dp->_d_entry = strdup (dtabuf.ff_name)) == (char *) NULL))
	{
	    if (dp->_d_entry != (char *)NULL)
		free ((char *)dp);

	    free (nbuf);
	    free_dircontents (dirp->dd_contents);
	    return (DIR *) NULL;
	}

	strlwr (dp->_d_entry);

	if (dirp->dd_contents != (DIRCONT *) NULL)
	    dirp->dd_cp = dirp->dd_cp->_d_next = dp;

	else
	    dirp->dd_contents = dirp->dd_cp = dp;

	dp->_d_next = (DIRCONT *) NULL;
    } while (findnext (&dtabuf) == 0);

    dirp->dd_cp = dirp->dd_contents;
    free (nbuf);
    return dirp;
}

/*
 * MSDOS Microsoft C5 version.  Open the directory stream
 */

#else

DIR		*opendir (name)
const char	*name;
{
    DIR			*dirp;
    DIRCONT		*dp;
    char		*nbuf;
    int			rc;
    char		*np;
    struct find_t	dtabuf;
#ifndef	NO_WINDOWS95_SUPPORT
    struct find95_t	dta1buf;
    int			d_handle;
#endif
    if ((nbuf = _OpenDirCommon (name)) == (char *)NULL)
	return (DIR *) NULL;

    if ((dirp = (DIR *) calloc (sizeof (DIR), 1)) == (DIR *) NULL)
    {
	free (nbuf);
	return (DIR *) NULL;
    }

/* Win95? */

#ifndef	NO_WINDOWS95_SUPPORT
    rc = 0;

    if ((d_handle = _dos95_findfirst (nbuf, ATTRIBUTES, &dta1buf)) == -1)
	rc = _dos_findfirst (nbuf, ATTRIBUTES, &dtabuf);
#else
    rc = _dos_findfirst (nbuf, ATTRIBUTES, &dtabuf);
#endif

/* Check for errors */

    if (rc)
    {
	free (nbuf);

/* Empty directory */

#if defined (ERROR_EMPTY_DIR)
	if (rc == ERROR_EMPTY_DIR)
	    return dirp;
#endif

	return (DIR *) NULL;
    }

/* Process the directory */

    do
    {
#ifndef	NO_WINDOWS95_SUPPORT
	np = strdup ((d_handle != -1) ? dta1buf.FileLongName : dtabuf.name);
#else
	np = strdup (dtabuf.name);
#endif

	if (((dp = (DIRCONT *) malloc (sizeof (DIRCONT))) == (DIRCONT *)NULL) ||
	    ((dp->_d_entry = np) == (char *) NULL))
	{
	    if (dp != (DIRCONT *)NULL)
		free ((char *)dp);

	    if (np != (char *)NULL)
		free (np);

	    free (nbuf);
	    free_dircontents (dirp->dd_contents);

#ifndef	NO_WINDOWS95_SUPPORT
	    if (d_handle != -1)
		_dos95_findclose (d_handle);
#endif
	   
	    return (DIR *) NULL;
	}

#ifndef	NO_WINDOWS95_SUPPORT
	if ((d_handle == -1) || (strlen (dta1buf.FileShortName) == 0))
#endif
	    strlwr (dp->_d_entry);

	if (dirp->dd_contents != (DIRCONT *) NULL)
	    dirp->dd_cp = dirp->dd_cp->_d_next = dp;

	else
	    dirp->dd_contents = dirp->dd_cp = dp;

	dp->_d_next = (DIRCONT *) NULL;

#ifndef	NO_WINDOWS95_SUPPORT
	if (d_handle != -1)
	   rc = (_dos95_findnext (d_handle, &dta1buf) == -1) ? -1 : 0;
	
	else
#endif
	   rc = _dos_findnext (&dtabuf);

    } while (rc == 0);

    dirp->dd_cp = dirp->dd_contents;
    free (nbuf);

#ifndef	NO_WINDOWS95_SUPPORT
    if (d_handle != -1)
	_dos95_findclose (d_handle);
#endif

    return dirp;
}
#endif

/*
 * Close the directory stream
 */

int	closedir (dirp)
DIR	*dirp;
{
    if (dirp != (DIR *)NULL)
    {
	free_dircontents (dirp->dd_contents);
	free ((char *)dirp);
    }

    return 0;
}

/*
 * Read the next record from the stream
 */

struct dirent	*readdir (dirp)
DIR		*dirp;
{
    static struct dirent	dp;

    if ((dirp == (DIR *)NULL) || (dirp->dd_cp == (DIRCONT *) NULL))
	return (struct dirent *) NULL;

    dp.d_reclen = strlen (strcpy (dp.d_name, dirp->dd_cp->_d_entry));
    dp.d_off    = dirp->dd_loc * 32;
    dp.d_ino    = (ino_t)++dirp->dd_loc;
    dirp->dd_cp = dirp->dd_cp->_d_next;

    return &dp;
}

/*
 * Restart the directory stream
 */

void	rewinddir (dirp)
DIR	*dirp;
{
    seekdir (dirp, (off_t)0);
}

/*
 * Move to a know position in the stream
 */

void	seekdir (dirp, off)
DIR	*dirp;
off_t	off;
{
    long	i = off;
    DIRCONT	*dp;

    if ((dirp == (DIR *)NULL) || (off < 0L))
	return;

    for (dp = dirp->dd_contents; (--i >= 0) && (dp != (DIRCONT *)NULL);
	 dp = dp->_d_next)
	;

    dirp->dd_loc = off - (i + 1);
    dirp->dd_cp = dp;
}

/*
 * Get the current position
 */

off_t	telldir(dirp)
DIR	*dirp;
{
    return (dirp == (DIR *)NULL) ? (off_t) -1 : dirp->dd_loc;
}

/*
 * Release the internal structure
 */

static void	free_dircontents (dp)
DIRCONT		*dp;
{
    DIRCONT	*odp;

    while ((odp = dp) != (DIRCONT *)NULL)
    {
	if (dp->_d_entry != (char *)NULL)
	    free (dp->_d_entry);

	dp = dp->_d_next;
	free ((char *)odp);
    }
}

/*
 * For OS/2, we need to know if we have to convert to lower case.  This
 * only applies to non-HPFS (FAT, NETWARE etc) file systems.
 */

#if defined (OS2) || defined (__OS2__)

/*
 * Define the know FAT systems
 */

static char	*FATSystems[] = {"FAT", "NETWARE", (char *)NULL};

/*
 * Check for Long filenames
 */

bool		IsHPFSFileSystem (char *directory)
{
    ULONG		lMap;
    BYTE		bData[128];
    BYTE		bName[3];
    int			i;
    char		*FName;
    unsigned long	rc;
#if defined (__OS2__)
    ULONG		cbData;
    ULONG		nDrive;
    PFSQBUFFER2		pFSQ = (PFSQBUFFER2)bData;
#else
    USHORT		cbData;
    USHORT		nDrive;
#endif

#ifndef __WATCOMC__
    if ( _osmode == DOS_MODE )
	return FALSE;
#endif

/*
 * Mike tells me there are IFS calls to determine this, but he carn't
 * remember which.  So we read the partition info and check for HPFS.
 */

    if (isalpha (directory[0]) && (directory[1] == ':'))
	nDrive = toupper (directory[0]) - '@';

    else
	DosQCurDisk (&nDrive, &lMap);

/* Set up the drive name */

    bName[0] = (char) (nDrive + '@');
    bName[1] = ':';
    bName[2] = 0;

    cbData = sizeof (bData);

/* Read the info, if we fail - assume non-HPFS */

    DISABLE_HARD_ERRORS;

#  ifdef __OS2__
    rc = DosQFSAttach (bName, 0, FSAIL_QUERYNAME, pFSQ, &cbData);
#  else
    rc = DosQFSAttach (bName, 0, FSAIL_QUERYNAME, bData, &cbData, 0L);
#  endif

    ENABLE_HARD_ERRORS;

    if (rc)
	return FALSE;

#  ifdef __OS2__
    FName = pFSQ->szName + pFSQ->cbName + 1;
#  else
    FName = bData + (*((USHORT *) (bData + 2)) + 7);
#  endif

#ifdef TEST
    printf ("File System for <%s> = <%s>\n", directory, FName);
#endif

    for (i = 0; FATSystems[i] != (char *)NULL; i++)
    {
        if (stricmp (FName, FATSystems[i]) == 0)
	    return FALSE;
    }

    return TRUE;
}
#endif

/*
 * Windows NT version
 */

#if defined (WIN32) 
bool		IsHPFSFileSystem (char *directory)
{
    char		bName[4];
    DWORD		flags;
    DWORD		maxname;
    BOOL		rc;
    unsigned int	nDrive;
    char		szCurDir [MAX_PATH];

    if (isalpha (directory[0]) && (directory[1] == ':'))
	nDrive = toupper (directory[0]) - '@';

    else
    {
	GetCurrentDirectory (MAX_PATH, szCurDir);
	nDrive = szCurDir[0] - 'A' + 1;
    }

/* Set up the drive name */

    strcpy (bName, "x:\\");
    bName[0] = (char) (nDrive + '@');

/* Read the volume info, if we fail - assume non-HPFS */

    DISABLE_HARD_ERRORS;

    rc = GetVolumeInformation (bName, (LPTSTR)NULL, 0, (LPDWORD)NULL,
			       &maxname, &flags, (LPTSTR)NULL, 0);
    ENABLE_HARD_ERRORS;

#ifdef TEST
    printf ("File System flags for <%s> = <0x%.8lx> (%d)\n", directory,
	    flags, rc);
#endif

    return ((rc) && (flags & (FS_CASE_SENSITIVE | FS_CASE_IS_PRESERVED)))
    		? TRUE : FALSE;
}
#endif

/*
 * Test program
 */

#ifdef TEST
int	main (int argc, char **argv)
{
    int			i;
    struct dirent	*cdp;
    DIR			*dp;

    for (i = 1; i < argc; i++)
    {
#if defined (OS2) || defined (__OS2__) || defined (WIN32)
	printf ("IsHPFSFileSystem returns %d\n", IsHPFSFileSystem (argv[1]));
#endif

        if ((dp = opendir (argv[i])) == (DIR *)NULL)
	    printf ("Cannot open %s\n", argv[1]);

	else
	{
	    while ((cdp = readdir (dp)) != (struct dirent *)NULL)
		printf ("Found %s\n", cdp->d_name);

	    closedir (dp);
	}
    }

    return 0;
}
#endif
