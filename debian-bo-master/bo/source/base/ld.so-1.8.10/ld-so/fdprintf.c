/*
 *  print.c
 *
 *  Copyright (C) 1991-1996  Linus Torvalds
 *
 *  Adapted by David Engel from Linus' printk.c.
 */

#include <stdarg.h>
#include <unistd.h>

extern int vsprintf(char * buf, const char * fmt, va_list args);

int fdprintf(int fd, const char *fmt, ...)
{
	va_list args;
	int i;
	char buf[1024];

	va_start(args, fmt);
	i=vsprintf(buf,fmt,args);
	va_end(args);
	write(fd, buf, i);

	return i;
}
