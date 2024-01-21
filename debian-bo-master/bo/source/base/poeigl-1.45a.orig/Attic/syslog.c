/*
 * syslog.c - a miserable hack at a syslog facility for Linux.
 *    Should be replaced with the real stuff as soon as possible.
 */

#define _POSIX_SOURCE 1
#include <stdarg.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

static char _syslogname[64];

void syslog(unsigned long flag, char *str, ...)
{
  int fd = open("/dev/console", O_WRONLY|O_NOCTTY);
  FILE *f;
  va_list vl;
  time_t tim;

  if(fd < 0) return;

  f = fdopen(fd, "w");
  if(!f) return;

  tim = time(NULL);
  fprintf(f, "\r\nsyslog: %.*s: %s: ", 19, ctime(&tim), _syslogname);
  va_start(vl, str);
  vfprintf(f, str, vl);
  va_end(vl);
  fprintf(f, "\r\n");

  fclose(f);
  close(fd);
}

void openlog(const char *name, int f1, int f2)
{
  strncpy(_syslogname, name, sizeof(_syslogname));
  _syslogname[63] = 0;
}
