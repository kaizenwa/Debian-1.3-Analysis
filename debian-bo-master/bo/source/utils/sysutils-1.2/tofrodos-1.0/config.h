/*
	config.h	Handles system dependencies.
	Copyright (c) 1996 by Christopher S L Heng. All rights reserved.

	$Id: config.h 1.3 1996/06/22 20:00:07 chris Exp $
*/

/*
	You need an ANSI C compiler. I assume this everywhere. If you
	have a pre-ANSI C compiler, it's likely that you have to make
	a lot of changes to the sources that you might as well just
	rewrite the program. It *is* afterall a trivial program.

	I have not specifically designed this program so that it is
	portable across systems. The comments below might help if you
	are using anything other than the compilers I used to develop
	the program. Note that the comments and macros in this file
	about system dependencies are not necessarily exhaustive.

	1. These macros are defined for the following systems:
	System					Macros defined
	------					--------------
	LINUX					LINUX, UNIX
	MSDOS					MSDOS

	2. You need a getopt() implementation. It must support the
	usual behaviour of the Unix getopt(), plus the variables
	optind, opterr, and optarg.

	If your system has the header <getopt.h>, define HAVE_GETOPT_H.
	I have defined this for the systems I compile for.

	Note that if you use Borland C/C++ 3.1, you actually have getopt.c
	on your system. (I don't know about later versions.) It's somewhere
	in your examples directory. You'll have to create your own header
	that declares getopt(), optind, opterr and optarg. With some
	modifications, you might be able to get the getopt.c thing to
	work for other DOS compilers too.

	If you want your DOS fromdos.exe and todos.exe to mimic the
	behaviour of the precompiled versions, you need a getopt() that
	can recognise both '-' and '/' as the option switch character.

	3. If your system has <unistd.h>, define HAVE_UNISTD_H. This is
	usually relevant only for Unix systems, although the DJGPP GNU C
	compiler has that too.

	4. Note that on MSDOS systems, you will need _splitpath()
	and _makepath(). This means that if you're compiling using
	DJGPP 1.X (GNU C), you'll need to either write replacements
	for these functions or rewrite the code so that you don't need
	them. I think the commercial C compilers have these functions.

	5. You will also need stricmp() and strnicmp() on MSDOS or
	strcasecmp() or strncasecmp() on Unix. If you have stricmp() and/or
	strnicmp() on a Unix system, define HAVE_STRICMP and/or
	HAVE_STRNICMP respectively. I assume stricmp() for all non-Unix
	systems so if you are neither compiling for Unix or MSDOS, you
	better check out my macros below.

	6. You will need a mktemp(). On Unix systems, this is probably
	declared in <unistd.h>.

	Borland declares mktemp() in dir.h. If you are writing or using
	a replacement mktemp() you should put the prototype in a
	header called mktemp.h and define HAVE_MKTEMP_H.

	If your compiler has mktemp() declared somewhere else (other
	than unistd.h on Unix), define MKTEMP_HEADER to be the name
	of the header, eg <whatever.h> (include the angle brackets or
	double quotes), and HAVE_MKTEMP_H to force inclusion of the
	header in the relevant files.
*/

#if !defined(CONFIG_H_INCLUDED)
#define	CONFIG_H_INCLUDED

#if defined(__cplusplus)
extern "C" {
#endif

/* define the systems */
#if defined(__linux__)	/* (predefined) */
#if !defined(LINUX)
#define	LINUX
#endif
#if !defined(UNIX)
#define	UNIX		/* make sure this is defined */
#endif
#endif

#if defined(__MSDOS__)
#if !defined(MSDOS)
#define	MSDOS		/* make sure this is defined */
#endif
#endif

/* define what headers we have (based on the systems) */
#if defined(LINUX)
#define HAVE_GETOPT_H
#define	HAVE_UNISTD_H
#endif

#if defined(__WATCOMC__) /* this works on my system only */
#if !defined(HAVE_GETOPT_H)
#define	HAVE_GETOPT_H
#endif
#if !defined(HAVE_MKTEMP_H)
#define	HAVE_MKTEMP_H
#endif
#if !defined(MKTEMP_HEADER)
#define	MKTEMP_HEADER	<mktemp.h>
#endif
#endif

#if defined(__BORLANDC__)	/* Borland declares mktemp() in dir.h */
#if !defined(HAVE_GETOPT_H)
#define	HAVE_GETOPT_H
#endif
#if !defined(HAVE_MKTEMP_H)
#define	HAVE_MKTEMP_H
#endif
#if !defined(MKTEMP_HEADER)
#define	MKTEMP_HEADER	<dir.h>
#endif
#endif

/* if we are in Unix define stricmp to be strcasecmp and strnicmp to */
/* be strncasecmp. I'm not sure if all Unices have these, but Linux */
/* does. */
#if defined(UNIX)
#if !defined(HAVE_STRICMP)
#define	stricmp 	strcasecmp
#endif
#if !defined(HAVE_STRNICMP)
#define	strnicmp	strncasecmp
#endif
#endif

/* Borland and Microsoft's compiler have S_IREAD and S_IWRITE in their */
/* sys/stat.h instead of S_IRUSR and S_IWUSR which are used by the Unix */
/* and Watcom compilers. */
#if defined(__BORLANDC__)
#define	S_IRUSR	S_IREAD
#define	S_IWUSR	S_IWRITE
#endif

#if defined(__cplusplus)
}
#endif

#endif
