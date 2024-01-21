#include <libioP.h>
#include <stdio.h>
#include <stdlib.h>

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

FILE*
freopen(const char* filename, const char* mode, FILE* fp)
{
  FILE *ret;
  CHECK_FILE(fp, NULL);
  flockfile (fp);
  if (!(fp->_flags & _IO_IS_FILEBUF))
  {
    funlockfile (fp);
    return NULL;
  }
  __libc_lock_lock (__libc_libio_lock);
  ret = _IO_freopen(filename, mode, fp);
  __libc_lock_unlock (__libc_libio_lock);
  funlockfile (fp);
  return ret;
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

int
fgetc(FILE *fp)
{
  int ret;
  CHECK_FILE(fp, EOF);
  flockfile (fp);
  ret = _IO_getc(fp);
  funlockfile (fp);
  return ret;
}

int
fputc(int c, FILE *fp)
{
  int ret;
  CHECK_FILE(fp, EOF);
  flockfile (fp);
  ret = _IO_putc(c, fp);
  funlockfile (fp);
  return ret;
}

#undef getc
int 
getc(FILE *fp)
{
  int ret;
  flockfile (fp);
  ret = _IO_getc(fp);
  funlockfile (fp);
  return ret;
}

#undef getchar
int
getchar ()
{
  int ret;
  flockfile (stdin);
  ret = _IO_getc(stdin);
  funlockfile (stdin);
  return ret;
}

int
getw(FILE *fp)
{
  int w;
  _IO_size_t bytes_read;
  CHECK_FILE(fp, EOF);
  flockfile (fp);
  bytes_read = _IO_sgetn (fp, (char*)&w, sizeof(w));
  funlockfile (fp);
  return sizeof(w) == bytes_read ? w : EOF;
}

#undef putc
int
putc(int c, FILE *stream)
{
  int ret;
  flockfile (stream);
  ret = _IO_putc(c, stream);
  funlockfile (stream);
  return ret;
}

#undef putchar
int
putchar(int c)
{
  int ret;
  flockfile (stdout);
  ret = _IO_putc(c, stdout);
  funlockfile (stdout);
  return ret;
}

int
putw(int w, FILE *fp)
{
  _IO_size_t written;
  CHECK_FILE(fp, EOF);
  flockfile (fp);
  written = _IO_sputn(fp, (const char *)&w, sizeof(w));
  funlockfile (fp);
  return written == sizeof(w) ? 0 : EOF;
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

int
fseek(FILE *fp, long int offset, int whence)
{
  CHECK_FILE(fp, -1);
  flockfile (fp);
  whence = _IO_fseek(fp, offset, whence);
  funlockfile (fp);
  return whence;
}

void
rewind(FILE* fp)
{
  CHECK_FILE(fp, );
  flockfile (fp);
  _IO_rewind(fp);
  funlockfile (fp);
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

void setlinebuf(FILE *stream)
{
        flockfile(stream);
        _IO_setvbuf(stream, NULL, 1, 0);
        funlockfile(stream);
}

void setbuf(FILE *fp, char *buf)
{
        flockfile(fp);
        _IO_setbuffer(fp, buf, _IO_BUFSIZ);
        funlockfile(fp);
}

int
ungetc (int c, FILE *fp)
{
  flockfile (fp);
  c = _IO_ungetc(c, fp);
  funlockfile (fp);
  return c;
}

#include <stdarg.h>

int
vfprintf(FILE *s, const char *format, va_list ap)
{
  int ret;
  flockfile (s);
  ret = _IO_vfprintf (s, format, ap);
  funlockfile (s);
  return ret;
}

int
vprintf(const char *format, va_list ap)
{
  return vfprintf(stdout, format, ap);
}

int
fprintf(FILE *s, const char *format, ...)
{
  int ret;
  va_list args;
  va_start(args, format);
  ret = vfprintf (s, format, args);
  va_end(args);
  return ret;
}

int
printf(const char *format, ...)
{
  int ret;
  va_list args;
  va_start(args, format);
  ret = vfprintf (stdout, format, args);
  va_end(args);
  return ret;
}


void perror(const char *s)
{
        flockfile(stderr);
        _IO_perror(s);
        funlockfile(stderr);
}
 
int
vfscanf(FILE *s, const char *format, va_list ap)
{
  int ret;
  CHECK_FILE(s, EOF);
  flockfile (s);
  ret = _IO_vfscanf (s, format, ap, NULL);
  funlockfile (s);
  return ret;
}

int
vscanf(const char *format, va_list ap)
{
  return vfscanf(stdin, format, ap);
}

int
fscanf(FILE *s, const char *format, ...)
{
  int ret;
  va_list args;
  va_start(args, format);
  ret = vfscanf (s, format, args);
  va_end(args);
  return ret;
}

int
scanf(const char *format, ...)
{
  int ret;
  va_list args;
  va_start(args, format);
  ret = vfscanf (stdin, format, args);
  va_end(args);
  return ret;
}

extern void ffreelockfile_np(int fd);

int
fclose(FILE *fp)
{
  int status, fd;
  CHECK_FILE(fp, EOF);
  /* Wait for other threads to finish. */
  flockfile (fp);
  fd = fileno (fp);
  __libc_lock_lock (__libc_libio_lock);
  if (fp->_IO_file_flags & _IO_IS_FILEBUF)
    status = _IO_file_close_it(fp);
  else
    status = fp->_flags & _IO_ERR_SEEN ? -1 : 0;
  _IO_FINISH (fp);
  __libc_lock_unlock (__libc_libio_lock);
  ffreelockfile_np (fd);
  if (fp != _IO_stdin && fp != _IO_stdout && fp != _IO_stderr)
    {
      free(fp);
    }

  return status;
}

int
pclose(FILE *fp)
{
#if 0
  /* Does not actually test that stream was created by popen(). Instead,
     it depends on the filebuf::sys_close() virtual to Do The Right Thing. */
  if (fp is not a proc_file)
    return -1;
#endif
  return fclose(fp);
}

ssize_t getline (char **lineptr, size_t *linelen, FILE *fp)
{
  _IO_ssize_t retval;

  flockfile(fp);
  retval = _IO_getdelim(lineptr, linelen, '\n', fp);
  funlockfile(fp);
  return retval;
}

int
fflush(FILE *fp)
{
  int result;
  if (fp == NULL)
  {
    __libc_lock_lock (__libc_libio_lock);
    result = _IO_flush_all();
    __libc_lock_unlock (__libc_libio_lock);
  }
  else
    {
      CHECK_FILE(fp, EOF);
      flockfile (fp);
      result = _IO_SYNC (fp) ? EOF : 0;
      funlockfile (fp);
    }
  return result;
}

void
setfileno(FILE* fp, int fd)
{
  CHECK_FILE(fp, );
  flockfile (fp);
  if ((fp->_flags & _IO_IS_FILEBUF) != 0)
    fp->_fileno = fd;
  funlockfile (fp);
}

void
clearerr(FILE* fp)
{
  CHECK_FILE(fp, /*nothing*/);
  flockfile (fp);
  _IO_clearerr(fp);
  funlockfile (fp);
}
