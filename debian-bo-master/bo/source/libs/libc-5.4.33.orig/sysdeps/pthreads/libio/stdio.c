#include <libioP.h>
#include <stdio.h>

FILE *
fdopen (int fd, const char *mode)
{
  FILE *fp;
  __libc_lock_lock (__libc_libio_lock);
  fp = _IO_fdopen (fd, mode);
  __libc_lock_unlock (__libc_libio_lock);
  return fp;
}

FILE *
fopen (const char *filename, const char *mode)
{
  FILE *fp;
  __libc_lock_lock (__libc_libio_lock);
  fp = _IO_fopen (filename, mode);
  __libc_lock_unlock (__libc_libio_lock);
  return fp;
}

FILE *
popen (const char *command, const char *mode)
{
  FILE *fp;
  __libc_lock_lock (__libc_libio_lock);
  fp = _IO_popen (command, mode);
  __libc_lock_unlock (__libc_libio_lock);
  return fp;
}

int
fgetpos (FILE *fp, fpos_t *posp)
{
  int ret;
  flockfile (fp);
  ret = _IO_fgetpos (fp, posp);
  funlockfile (fp);
  return ret;
}

int
fsetpos (FILE *fp, const fpos_t *posp)
{
  int ret;
  flockfile (fp);
  ret = _IO_fsetpos (fp, posp);
  funlockfile (fp);
  return ret;
}

char *
fgets (char *buf, int n, FILE *fp)
{
  flockfile (fp);
  buf = _IO_fgets (buf, n, fp);
  funlockfile (fp);
  return buf;
}

int
fputs (const char *buf, FILE *fp)
{
  int ret;
  flockfile (fp);
  ret = _IO_fputs (buf, fp);
  funlockfile (fp);
  return ret;
}

size_t
fread (void *buf, size_t size, size_t count, FILE *fp)
{
  flockfile (fp);
  count = _IO_fread (buf, size, count, fp);
  funlockfile (fp);
  return count;
}

long int
ftell (FILE *fp)
{
  long int ret;
  flockfile (fp);
  ret = _IO_ftell (fp);
  funlockfile (fp);
  return ret;
}

size_t
fwrite (const void *buf, size_t size, size_t count, FILE *fp)
{
  flockfile (fp);
  count = _IO_fwrite (buf, size, count, fp);
  funlockfile (fp);
  return count;
}

ssize_t
getdelim (char **lineptr, size_t *n, int delimiter, FILE *fp)
{
  ssize_t ret;
  flockfile (fp);
  ret = _IO_getdelim (lineptr, n, delimiter, fp);
  funlockfile (fp);
  return ret;
}

char *
gets (char *buf)
{
  flockfile (_IO_stdin);
  buf = _IO_gets (buf);
  funlockfile (_IO_stdin);
  return buf;
}

int
puts (const char *buf)
{
  int ret;
  flockfile (_IO_stdout);
  ret = _IO_puts (buf);
  funlockfile (_IO_stdout);
  return ret;
}

void
setbuffer(FILE *fp, char *buf, int size)
{
  flockfile (fp);
  _IO_setbuffer (fp, buf, size);
  funlockfile (fp);
}

int
setvbuf (FILE *fp, char *buf, int mode, size_t size)
{
  flockfile (fp);
  mode = _IO_setvbuf (fp, buf, mode, size);
  funlockfile (fp);
  return mode;
}

int
ungetc (int c, FILE *fp)
{
  flockfile (fp);
  c = _IO_ungetc(c, fp);
  funlockfile (fp);
  return c;
}

int
vfprintf(FILE *s, const char *format, va_list ap)
{
  int ret;
  flockfile (s);
  ret = _IO_vfprintf (s, format, ap);
  funlockfile (s);
  return ret;
}
