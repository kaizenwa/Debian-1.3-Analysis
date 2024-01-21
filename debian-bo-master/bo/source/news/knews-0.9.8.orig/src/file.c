/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#include "global.h"
#include <sys/stat.h>
#include "expand.h"
#include "file.h"

#if 0
#  define FILE_MASK	0666
#  define DIR_MASK	0777
#else
#  define FILE_MASK	(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)
#  define DIR_MASK	((FILE_MASK)|S_IXUSR|S_IXGRP|S_IXOTH)
#endif

#ifndef O_ACCMODE
#  define O_ACCMODE 3
#endif

int open_mkdir(char *path, int flags, int report)
{
    struct stat	stat_buf;
    char	*c;
    int		fd;

    if (flags & O_CREAT)
	fd = open(path, flags, FILE_MASK);
    else
	fd = open(path, flags);

    if (fd >= 0)
	return fd;

    if (errno == ENOENT && (flags & O_ACCMODE) != O_RDONLY) {
	if (path[0] == '/')
	    c = strchr(path + 1, '/');
	else
	    c = strchr(path, '/');

	while (c) {
	    *c = '\0';
	    if (stat(path, &stat_buf) < 0 &&
		(errno != ENOENT || mkdir(path, DIR_MASK) < 0)) {
		*c++ = '/';
		return -1;
	    }

	    *c++ = '/';
	    c = strchr(c, '/');
	}
    }

    if (flags & O_CREAT)
	fd = open(path, flags, FILE_MASK);
    else
	fd = open(path, flags);

    if (fd < 0 && (report || errno != ENOENT)) {
	int oerrno = errno;
	perror(path);
	errno = oerrno;
    }

    return fd;
}

int open_expand(char *file_name, int flags, int report)
{
    char	*path;
    int		fd;

    path = expand_path(file_name);
    if (!path)
	return -1;

    fd = open_mkdir(path, flags, report);
    XtFree(path);

    return fd;
}

FILE *fopen_mkdir(char *path, char *mode, int report)
{
    int		fd, flags;
    FILE	*ret;

    switch (*mode) {
    case 'a':
	flags = O_WRONLY|O_APPEND|O_CREAT;
	break;
    case 'r':
	flags = O_RDONLY;
	break;
    case 'w':
	flags = O_WRONLY|O_TRUNC|O_CREAT;
	break;
    default:
	fputs("knews: invalid mode to fopen_mkdir.\n", stderr);
	return NULL;
    }

    fd = open_mkdir(path, flags, report);
    if (fd < 0)
	return NULL;

    ret = fdopen(fd, mode);
    if (!ret) {
	perror("fdopen");
	close(fd);
    }

    return ret;
}

FILE *fopen_expand(char *file_name, char *mode, int report)
{
    char	*path;
    FILE	*fp;

    path = expand_path(file_name);
    if (!path)
	return NULL;

    fp = fopen_mkdir(path, mode, report);
    XtFree(path);

    return fp;
}

int unlink_expand(char *file_name)
{
    char	*path;
    int		ret;

    path = expand_path(file_name);
    if (!path)
	return -1;

    ret = unlink(path);
    XtFree(path);

    return ret;
}

int chdir_mkdir(char *path)
{
    char	*c, *p;

    if (path[0] == '~' && path[1] == '/')
	path += 2;
    if (chdir(path) == 0)
	return 0;
    if (errno != ENOENT) {
	perror(path);
	return -1;
    }

    c = path;
    p = strchr(c, '/');
    for (;;) {
	int	tmp;

	if (p)
	    *p = '\0';
	tmp = chdir(c);
	if (tmp < 0 && errno == ENOENT && mkdir(c, DIR_MASK) == 0)
	    tmp = chdir(c);
	if (p)
	    *p++ = '/';
	if (tmp < 0) {
	    perror(path);
	    return -1;
	}
	c = p;
	if (!c)
	    break;
	p = strchr(c, '\n');
    }

    return 0;
}

int create_temp_fd(char **name)
{
    int	fd;

    *name = tmpnam(NULL);
    if (!*name)
	fd = -1;
    else {
	unlink(*name);
	fd = open(*name, O_RDWR|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR);
	if (fd < 0)
	    *name = NULL;
    }

    return fd;
}

FILE *create_temp_file(char **name)
{
    int	fd;

    fd = create_temp_fd(name);
    if (fd < 0)
	return NULL;

    return fdopen(fd, "w+");
}

char *snarf_file(int fd, long *lenp)
{
    struct stat	stat_buf;
    char	*buffer;
    long	len, pos, n;

    if (fstat(fd, &stat_buf) < 0) {
	perror("fstat");
	return NULL;
    }

    if (!S_ISREG(stat_buf.st_mode)) {
	fputs("snarf_file: not a regular file!\n", stderr);
	errno = EINVAL;
	return NULL;
    }

    pos = 0;
    len = stat_buf.st_size + 256;
    buffer = malloc(len + 1);
    if (!buffer) {
	perror("malloc");
	return NULL;
    }

    while ((n = read(fd, buffer + pos, len - pos)) != 0)
	if (n < 0)
	    if (errno == EINTR)
		continue;
	    else {
		perror("read");
		free(buffer);
		return NULL;
	    }
	else {
	    pos += n;
	    if (pos == len) {
		char	*tmp;

		len *= 2;
		tmp = realloc(buffer, len + 1);
		if (!tmp) {
		    perror("realloc");
		    free(buffer);
		    return NULL;
		}
		buffer = tmp;
	    }
	}

    buffer[pos] = '\0';
    if (lenp)
	*lenp = pos;

    return buffer;
}

int writen(int fd, char *buf, long n)
{
    long	i;

    while (n > 0)
	switch ((i = write(fd, buf, n))) {
	case -1:
	    perror("write");
	    return -1;
	case 0:
	    fputs("knews: write(..) = 0, weird...\n", stderr);
	    return -1;
	default:
	    buf += i;
	    n -= i;
	    break;
	}

    return 0;
}
