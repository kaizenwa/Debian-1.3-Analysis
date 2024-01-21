/* +-------------------------------------------------------------------+ */
/* | Copyright 1993, David Koblas (koblas@netcom.com)		       | */
/* |								       | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.	 There is no	       | */
/* | representations about the suitability of this software for	       | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.					       | */
/* |								       | */
/* +-------------------------------------------------------------------+ */

/* $Id: rwTable.c,v 1.6 1996/06/09 17:31:28 torsten Exp $ */

#if defined(HAVE_PARAM_H)
#include <sys/param.h>
#endif

#include <stdio.h>
#include "image.h"
#include "rwTable.h"
#include <string.h>
#include <errno.h>

#ifdef MISSING_STDARG_H
#include <varargs.h>
#else
#include <stdarg.h>
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

typedef struct {
    char *name;
    RWreadFunc read;
    RWwriteFunc write;
    RWtestFunc test;
} ImageTypes;

static Image *readMagic(char *);
static char RWtableMsg[512];

/*
**  Define all the read/write functions here.
**    [ unfortunately most compilers won't take
**	the "variable" name version of these. ]
 */

extern Image *ReadXBM(char *);
extern Image *ReadXWD(char *);
extern Image *ReadPNM(char *);
extern Image *ReadPNG(char *);
extern Image *ReadGIF(char *);
extern Image *ReadSGI(char *);
extern Image *ReadXPM(char *);
extern Image *ReadTIFF(char *);
extern Image *ReadJPEG(char *);
extern int WriteXBM(char *, Image *);
extern int WriteXWD(char *, Image *);
extern int WritePNM(char *, Image *);
extern int WriteXPM(char *, Image *);
extern int WritePNGn(char *, Image *);
extern int WritePNGi(char *, Image *);
extern int WriteGIF(char *, Image *);
extern int WriteSGI(char *, Image *);
extern int WriteTIFF(char *, Image *);
extern int WriteJPEG(char *, Image *);
extern int WritePS(char *, Image *);
extern int TestPNM(char *);
extern int TestXWD(char *);
extern int TestXBM(char *);
extern int TestXPM(char *);
extern int TestPNG(char *);
extern int TestGIF(char *);
extern int TestSGI(char *);
extern int TestTIFF(char *);
extern int TestJPEG(char *);

#define DEF_READ_ENTRY	0
#define DEF_WRITE_ENTRY	1

/* GRR 960219:  added PNG, alphabetized image types: */
static ImageTypes RWtable[] =
{
    {"Best Guess",       readMagic, NULL,      NULL    },
    {"GIF Format",       ReadGIF,   WriteGIF,  TestGIF },
#ifdef HAVE_JPEG
    {"JPEG Format",      ReadJPEG,  WriteJPEG, TestJPEG},
#endif
#ifdef HAVE_PNG   /* ReadPNG does all PNG files; no need for two entries */
    {"PNG Format",       ReadPNG,   WritePNGn, TestPNG },
    {"PNG (interlaced)", NULL,      WritePNGi, TestPNG },
#endif
    {"PPM Format",       ReadPNM,   WritePNM,  TestPNM },
    {"PS Format",        NULL,      WritePS,   NULL    },
#ifdef HAVE_SGI
    {"SGI Format",       ReadSGI,   WriteSGI,  TestSGI },
#endif
#ifdef HAVE_TIFF
    {"TIFF Format",      ReadTIFF,  WriteTIFF, TestTIFF},
#endif
    {"XBM Format",       ReadXBM,   WriteXBM,  TestXBM },
    {"XPM Format",       ReadXPM,   WriteXPM,  TestXPM },
    {"XWD Format",       ReadXWD,   WriteXWD,  TestXWD },
};

#define	NUMBER	(sizeof(RWtable) / sizeof(RWtable[0]))

static char *readList[NUMBER + 1];
static char *writeList[NUMBER + 1];

/*
**  Special reader that uses the above information.
 */
static char *usedMagicReader = NULL;

static Image *
readMagic(char *file)
{
    extern int errno;
    int i;

    errno = 0;

    for (i = 0; i < NUMBER; i++) {
	if (RWtable[i].read == NULL || RWtable[i].test == NULL)
	    continue;
	if (!RWtable[i].test(file))
	    continue;
	usedMagicReader = RWtable[i].name;
	return RWtable[i].read(file);
    }

    if (errno == 0)
	RWSetMsg("Unknown image format");

    return NULL;
}

void *
RWtableGetReaderID()
{
    return (void *) usedMagicReader;
}


/*
**  Give a name, return an "opaque" handle to some information
 */
void *
RWtableGetEntry(char *name)
{
    int i;

    for (i = 0; i < NUMBER; i++)
	if (strcmp(name, RWtable[i].name) == 0)
	    return (void *) &RWtable[i];
    return NULL;
}

char *
RWtableGetId(void *v)
{
    ImageTypes *entry = (ImageTypes *) v;

    if (entry == NULL)
	return NULL;

    return entry->name;
}

RWreadFunc
RWtableGetReader(void *entry)
{
    RWtableMsg[0] = '\0';

    if (entry == NULL)
	return RWtable[DEF_READ_ENTRY].read;

    return ((ImageTypes *) entry)->read;
}

RWwriteFunc
RWtableGetWriter(void *entry)
{
    RWtableMsg[0] = '\0';

    if (entry == NULL)
	return RWtable[DEF_WRITE_ENTRY].write;

    return ((ImageTypes *) entry)->write;
}

char **
RWtableGetReaderList()
{
    static int done = FALSE;
    int i, idx = 0;

    if (!done) {
	for (i = 0; i < NUMBER; i++)
	    if (RWtable[i].read != NULL)
		readList[idx++] = RWtable[i].name;
	readList[idx++] = NULL;
	done = TRUE;
    }
    return readList;
}

char **
RWtableGetWriterList()
{
    static int done = FALSE;
    int i, idx = 0;

    if (!done) {
	for (i = 0; i < NUMBER; i++)
	    if (RWtable[i].write != NULL)
		writeList[idx++] = RWtable[i].name;
	writeList[idx++] = NULL;
	done = TRUE;
    }
    return writeList;
}

char *
RWGetMsg()
{
#if defined(BSD4_4)
    __const extern char *__const sys_errlist[];
#else
    extern char *sys_errlist[];
#endif
    extern int errno;

    if (RWtableMsg[0] == '\0') {
	if (errno == 0)
	    return "";
#if defined(__STDC__) && !defined(MISSING_STRERROR)
	return strerror(errno);
#else
	return sys_errlist[errno];
#endif
    }
    return RWtableMsg;
}

#ifdef MISSING_STDARG_H
void RWSetMsg(va_alist)
va_dcl
{
    va_list ap;
    char *fmt;

    va_start(ap);
    fmt = va_arg(ap, char *);
    vsprintf(RWtableMsg, fmt, ap);
}

#else
void RWSetMsg(char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vsprintf(RWtableMsg, fmt, ap);
}
#endif
