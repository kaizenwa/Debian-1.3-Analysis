/* xio.c -- Safe versions of the system io primitives.  It also includes a
   new version of the readlink system call that computes the number of
   characters required to hold the entire link.  */

/* Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Tudor Hulubei and Andrei Pitis.  */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>

#ifdef HAVE_STDDEF_H
#include <stddef.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifndef HAVE_GETCWD
#include <sys/param.h>
#ifdef MAXPATHLEN
#define MAXPATHSIZE MAXPATHLEN
#else
#ifdef PATH_MAX
#define MAXPATHSIZE PATH_MAX
#else
#define MAXPATHSIZE 1024
#endif /* PATH_MAX */
#endif /* MAXPATHLEN */
#endif /* HAVE_GETCWD */

#include <errno.h>

/* Not all systems declare ERRNO in errno.h... and some systems #define it! */
#if !defined (errno)
extern int errno;
#endif /* !errno */


#include "xmalloc.h"
#include "xstring.h"
#include "xio.h"


int
xread(fd, buf, count)
    int fd;
    char *buf;
    size_t count;
{
    int chars;
    int old_errno;

    if (count <= 0)
	return count;

#ifdef EINTR
    old_errno = errno;

    do
	chars = read(fd, buf, count);
    while (chars < 0 && errno == EINTR);

    errno = old_errno;
    return chars;
#else
    return read(fd, buf, count);
#endif
}


int
xwrite(fd, buf, count)
    int fd;
    const char *buf;
    size_t count;
{
    int chars;
    int old_errno;

    if (count <= 0)
	return count;

#ifdef EINTR
    old_errno = errno;

    do
	chars = write(fd, buf, count);
    while (chars < 0 && errno == EINTR);

    errno = old_errno;
    return chars;
#else
    return write(fd, buf, count);
#endif
}


#ifndef HAVE_RENAME

/*
 * This is a rename() function for the systems that don't have one.
 * Stolen from glibc-1.08.1.
 */

int
rename(old, new)
    const char *old;
    const char *new;
{
    int save = errno;

    if (link(old, new) < 0)
    {
	if (errno == EEXIST)
	{
	    errno = save;

	    /* Race condition, required for 1003.1 conformance.  */
	    if (unlink(new) < 0 || link(old, new) < 0)
		return -1;
	}
	else
	    return -1;
    }

    if (unlink(old) < 0)
    {
	save = errno;

	if (unlink(new) == 0)
	    errno = save;

	return -1;
    }

    return 0;
}
#endif /* HAVE_RENAME */


#ifndef HAVE_READLINK

/*
 * readlink() stub.  Just to make things compile.
 */

int
readlink(path, buf, bufsize)
    const char *path;
    char *buf;
    size_t bufsize;
{
    /* The underlying system doesn't have the readlink() system call.  */
    return -1;
}
#endif /* HAVE_READLINK */


int
__xreadlink(path, buf, size)
    const char *path;
    char *buf;
    size_t size;
{
    int chars;
    int old_errno;

    if (size <= 0)
	return size;

#ifdef EINTR
    old_errno = errno;

    do
    {
	chars = readlink(path, buf, size);
	old_errno = size;
    }
    while (chars < 0 && errno == EINTR);

    errno = old_errno;
    return chars;
#else
    return read(fd, buf, count);
#endif
}


int
xreadlink(filename)
    const char *filename;
{
    int size = 100;

    for (;;)
    {
	char *buffer = xmalloc(size);
	int nchars = __xreadlink(filename, buffer, (size_t)size);

	if (nchars < size)
	{
	    xfree(buffer);
	    return nchars;
	}

	xfree(buffer);
	size *= 2;
    }
}


int
xfstat(filedes, buf)
    int filedes;
    struct stat *buf;
{
    int result;

    do
	result = fstat(filedes, buf);
    while (result < 0 && errno == EINTR);

    return result;
}


int
xstat(filename, buf)
    const char *filename;
    struct stat *buf;
{
    int result;

    do
	result = stat(filename, buf);
    while (result < 0 && errno == EINTR);

    return result;
}


int
xlstat(filename, buf)
    const char *filename;
    struct stat *buf;
{
    int result;

    do
#ifdef HAVE_LSTAT
	result = lstat(filename, buf);
#else
	result = stat(filename, buf);
#endif /* HAVE_LSTAT */
    while (result < 0 && errno == EINTR);

    return result;
}


char *
xgetcwd()
{
    char *result;
    char *cwd;

    errno = 0;

#ifdef HAVE_GETCWD
    {
	size_t size;

	cwd = xmalloc(size = 64);

	while ((result = getcwd(cwd, size)) == NULL && errno == ERANGE)
	{
	    cwd = xrealloc(cwd, size += 64);
	    errno = 0;
	}
    }
#else
    {
	/* No getcwd() -> getwd(): BSD style... bleah!  */

	cwd    = xmalloc(MAXPATHSIZE + 2);
	result = getwd(cwd);

	if (result)
	    cwd = xrealloc(cwd, strlen(cwd) + 1);
    }
#endif  /* HAVE_GETCWD */

    if (result == NULL)
    {
	int old_errno = errno;
	xfree(cwd);
	errno = old_errno;
	return NULL;
    }

    return cwd;
}


char *
xbasename(name)
    char *name;
{
    char *base;
    size_t len = strlen(name);

    if (name[len - 1] == '/')
	name[len - 1] = '\0';

    base = strrchr(name, '/');
    return base ? base + 1 : name;
}
