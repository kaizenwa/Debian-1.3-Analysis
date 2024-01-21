/* Checker stubs for functions defined in stdio.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
#include "available-stubs.h"

#ifdef HAVE_STDIO_H
#include <sys/types.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include "checker_api.h"
#include "check-printf.h"

#undef HAVE_fgetpos
#undef HAVE_fsetpos
#undef HAVE_setbuffer
#undef HAVE_getdelim

#if 0
#define HAVE_printf
#define HAVE_fprintf
#define HAVE_scanf
#define HAVE_sscanf
#define HAVE_vasprintf
#define HAVE_asprintf
#define HAVE_vsnprintf
#define HAVE_snprintf
#define HAVE_vsscanf
#define HAVE_vscanf
#define HAVE_vfscanf
#define HAVE_vsprintf
#define HAVE_vprintf
#define HAVE_vfprintf
#define HAVE_sprintf
#define HAVE_fscanf
#define HAVE_fgets
#define HAVE_fputs
#define HAVE_puts
#define HAVE_vfprintf
#define HAVE_fflush
#define HAVE_fputc
#define HAVE_putc
#define HAVE_setbuf
#define HAVE_sprintf
#define HAVE_putchar
#define HAVE_fileno
#define HAVE_getc
#define HAVE_fopen
#define HAVE_ungetc
#define HAVE_fclose
#define HAVE_perror
#define HAVE_getchar
#define HAVE_fwrite
#define HAVE_fread
#define HAVE_freopen
#define HAVE_ferror
#define HAVE_ftell
#define HAVE_vsprintf
#define HAVE_popen
#define HAVE_pclose
#define HAVE_fdopen
#define HAVE_rewind
#define HAVE_rename
#define HAVE_remove
#define HAVE_gets
#define HAVE_fseek
#define HAVE_fgetc
#define HAVE_feof
#define HAVE_setlinebuf
#define HAVE_tmpnam
#define HAVE_tempnam
#define HAVE_putw
#define HAVE_getw
#define HAVE_tmpfile
#define HAVE_setvbuf
#endif

#define CHKR_CHECK_STREAM(s) stubs_chkr_check_addr (s, sizeof (FILE), CHKR_TW, "stream")

#ifdef HAVE_printf
int
chkr$printf (char *format,...)
{
  va_list param;
  
  va_start (param, format);
  check_printf_format ("printf", format, param, TYPE_PRINTF, 0);
  return vprintf (format, param);
}
#endif /* HAVE_printf */

#ifdef HAVE_fprintf
int
chkr$fprintf (FILE *f, char *format,...)
{
  int res;
  va_list param;
  
  va_start (param, format);
  CHKR_CHECK_STREAM (f);
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("fprintf", format, param, TYPE_PRINTF, 0);
  res = vfprintf (f, format, param);
  va_end (param);
  return res;
}
#endif /* HAVE_fprintf */

#ifdef HAVE_scanf
int
chkr$scanf (char *format, ...)
{
  va_list param;
  int n;
  
  va_start (param, format);
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("scanf", format, param, TYPE_PRESCANF, 0);
  n = vscanf (format, param);
  if (n != EOF)
    check_printf_format ("scanf", format, param, n, 0);
  va_end (param);
  return n;
}
#endif /* HAVE_scanf */

/* compiled from: . */
#ifdef HAVE_clearerr
void
chkr$clearerr (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (clearerr);
#else
  clearerr (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_clearerr */

#ifdef HAVE_fclose
int
chkr$fclose (FILE *stream)
{
  int fd = fileno (stream);
  int res;
  CHKR_CHECK_STREAM (stream);
  res = fclose (stream);
  if (res == 0)
    fd_closed (fd);
  return res;
}
#endif /* HAVE_fclose */

#ifdef HAVE_feof
int
chkr$feof (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (feof);
#else
  return feof (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_feof */

#ifdef HAVE_ferror
int
chkr$ferror (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (ferror);
#else
  return ferror (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ferror */

#ifdef HAVE_fflush
int
chkr$fflush (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (fflush);
#else
  return fflush (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fflush */

#ifdef HAVE_fgetc
int
chkr$fgetc (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (fgetc);
#else
  return fgetc (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fgetc */

#ifdef HAVE_fgetpos
int
chkr$fgetpos (FILE * arg0, fpos_t * arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (FILE), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (fpos_t), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (fgetpos);
#else
  {
    int res;
    res = fgetpos (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fgetpos */

#ifdef HAVE_fgets
char *
chkr$fgets (char *buf, int maxlen, FILE *stream)
{
  char *res;
  size_t len;
  
  CHKR_CHECK_STREAM (stream);
  stubs_chkr_check_addr (buf, maxlen, CHKR_MW, "buf");
  res = fgets (buf, maxlen, stream);
  if (res)
    {
      len = strlen (buf) + 1;
      stubs_chkr_set_right (buf, len, CHKR_RW);
    }
  return res;
}
#endif /* HAVE_fgets */

#ifdef HAVE_fopen
FILE *
chkr$fopen (const char *path, const char *mode)
{
  FILE *res;
  
  stubs_chkr_check_str (path, CHKR_RO, "path");
  stubs_chkr_check_str (mode, CHKR_RO, "mode");
  res = fopen (path, mode);
  if (res != NULL)
    fd_returned_by_system (fileno (res));
  return res;
}
#endif /* HAVE_fopen */

#ifdef HAVE_fputc
int
chkr$fputc (int c, FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (fputc);
#else
  return fputc (c, stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fputc */

#ifdef HAVE_fputs
int
chkr$fputs (const char *s, FILE *stream)
{
  stubs_chkr_check_str (s, CHKR_RO, "s");
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (fputs);
#else
  return fputs (s, stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fputs */

#ifdef HAVE_fread
size_t
chkr$fread (void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  size_t res;
  
  if (size * nmemb)
    stubs_chkr_check_addr (ptr, size * nmemb, CHKR_MW, "ptr");
  CHKR_CHECK_STREAM (stream);
  res = fread (ptr, size, nmemb, stream);
  if (res > 0 && size)
    stubs_chkr_set_right (ptr, res * size, CHKR_RW);
  return res;
}
#endif /* HAVE_fread */

#ifdef HAVE_freopen
FILE *
chkr$freopen (const char *path, const char *mode, FILE *stream)
{
  FILE *res;
  int fd = fileno (stream);
  
  stubs_chkr_check_str (path, CHKR_RO, "path");
  stubs_chkr_check_str (mode, CHKR_RO, "mode");
  CHKR_CHECK_STREAM (stream);
  res = freopen (path, mode, stream);
  if (res != NULL)
    {
      fd_closed (fd);
      fd_returned_by_system (fileno (stream));
    }
  return res;
}
#endif /* HAVE_freopen */

#ifdef HAVE_fscanf
int
chkr$fscanf (FILE *stream, const char *format, ... )
{
  va_list param;
  int n;
  
  va_start (param, format);
  CHKR_CHECK_STREAM (stream);
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("fscanf", format, param, TYPE_PRESCANF, 0);
  n = vfscanf (stream, format, param);
  if (n != EOF)
    check_printf_format ("fscanf", format, param, n, 0);
  va_end (param);
  return n;
}
#endif /* HAVE_fscanf */

#ifdef HAVE_fseek
int
chkr$fseek (FILE *stream, long int offset, int whence)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (fseek);
#else
  return fseek (stream, offset, whence);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fseek */

#ifdef HAVE_fsetpos
int
chkr$fsetpos (FILE * arg0, const fpos_t * arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (FILE), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (fpos_t), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (fsetpos);
#else
  {
    int res;
    res = fsetpos (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fsetpos */

#ifdef HAVE_ftell
long int
chkr$ftell (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (ftell);
#else
  return ftell (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ftell */

#ifdef HAVE_fwrite
size_t
chkr$fwrite (const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  if (size * nmemb)
    stubs_chkr_check_addr (ptr, size * nmemb, CHKR_RO, "ptr");
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (fwrite);
#else
  return fwrite (ptr, size, nmemb, stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fwrite */

#ifdef HAVE_getc
int
chkr$getc (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (getc);
#else
  return getc (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getc */

#ifdef HAVE_getchar
int
chkr$getchar (void)
{
#if USE_BI_JUMP
  __builtin_jump (getchar);
#else
  return getchar ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getchar */

#ifdef HAVE_gets
char *
chkr$gets (char *buf)
{
  chkr_report (M_C_FUN_LB_ET);
  chkr_printf ("gets: This function is unreliable.  It is nonsense to use\n");
  chkr_printf ("  Checker with such a function.  Use `fgets' instead.\n");
  chkr_disp_call_chain ();
  chkr_abort ();
  return 0;
#if 0
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (gets);
#else
  {
    char * res;
    res = gets (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
#endif
}
#endif /* HAVE_gets */

#ifdef HAVE_perror
void
chkr$perror (const char *str)
{
  stubs_chkr_check_str (str, CHKR_RO, "str");
#if USE_BI_JUMP
  __builtin_jump (perror);
#else
  perror (str);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_perror */

#ifdef HAVE_putc
int
chkr$putc (int c, FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (putc);
#else
  return putc (c, stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_putc */

#ifdef HAVE_putchar
int
chkr$putchar (int c)
{
#if USE_BI_JUMP
  __builtin_jump (putchar);
#else
  return putchar (c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_putchar */

#ifdef HAVE_puts
int
chkr$puts (const char *s)
{
  stubs_chkr_check_str (s, CHKR_RO, "s");
#if USE_BI_JUMP
  __builtin_jump (puts);
#else
  return puts (s);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_puts */

#ifdef HAVE_remove
int
chkr$remove (const char *path)
{
  stubs_chkr_check_str (path, CHKR_RO, "path");
#if USE_BI_JUMP
  __builtin_jump (remove);
#else
  return remove (path);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_remove */

#ifdef HAVE_rename
int
chkr$rename (const char *oldname, const char *newname)
{
  stubs_chkr_check_str (oldname, CHKR_RO, "oldname");
  stubs_chkr_check_str (newname, CHKR_RO, "newname");
#if USE_BI_JUMP
  __builtin_jump (rename);
#else
  return rename (oldname, newname);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rename */

#ifdef HAVE_rewind
void
chkr$rewind (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (rewind);
#else
  rewind (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rewind */

#ifdef HAVE_setbuf
void
chkr$setbuf (FILE *stream, char *buf)
{
  CHKR_CHECK_STREAM (stream);
  if (buf)
    stubs_chkr_check_addr (buf, BUFSIZ, CHKR_RW, "buf");
  setbuf (stream, buf);
}
#endif /* HAVE_setbuf */

#ifdef HAVE_setlinebuf
void
chkr$setlinebuf (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (setlinebuf);
#else
  setlinebuf (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setlinebuf */

#ifdef HAVE_setbuffer
void
chkr$setbuffer (FILE * arg0, char * arg1, int arg2)
{
  CHKR_CHECK_STREAM (stream);
  stubs_chkr_check_addr (arg0, sizeof (FILE), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (setbuffer);
#else
  setbuffer (arg0, arg1, arg2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setbuffer */

#ifdef HAVE_setvbuf
int
chkr$setvbuf (FILE *stream, char *buf, int mode, size_t size)
{
  CHKR_CHECK_STREAM (stream);
  if (buf)
    stubs_chkr_check_addr (buf, size, CHKR_WO, "buf");
#if USE_BI_JUMP
  __builtin_jump (setvbuf);
#else
  return setvbuf (stream, buf, mode, size);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_setvbuf */

#ifdef HAVE_sprintf
/* FIXME */
int
chkr$sprintf (char *str, const char *format, ... )
{
  va_list param;
  int res;
  
  va_start (param, format);
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("sprintf", format, param, TYPE_PRINTF, 0);
  res = vsprintf (str, format, param);
  if (res == (int)str)
    stubs_chkr_set_right (str, strlen (str) + 1, CHKR_RW);
  else if (res >= 0)
    stubs_chkr_set_right (str, res + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_sprintf */

#ifdef HAVE_sscanf
int
chkr$sscanf (const char *str, const char *format, ... )
{
  va_list param;
  int n;
  
  va_start (param, format);
  stubs_chkr_check_str (str, CHKR_RO, "str");
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("sscanf", format, param, TYPE_PRESCANF, 0);
  n = vsscanf (str, format, param);
  if (n != EOF)
    check_printf_format ("sscanf", format, param, n, 0);
  va_end (param);
  return n;
}
#endif /* HAVE_sscanf */

#ifdef HAVE_tmpfile
FILE *
chkr$tmpfile (void)
{
  FILE *res;
  res = tmpfile ();
  if (res != NULL)
    fd_returned_by_system (fileno (res));
  return res;
}
#endif /* HAVE_tmpfile */

#ifdef HAVE_tmpnam
char *
chkr$tmpnam (char *s)
{
  char *res;
  
  if (s)
    stubs_chkr_check_str (s, CHKR_RO, "s");
  res = tmpnam (s);
  if (res)
    stubs_chkr_set_right (res, strlen (res) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_tmpnam */

#ifdef HAVE_ungetc
int
chkr$ungetc (int c, FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (ungetc);
#else
  return ungetc (c, stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ungetc */

#ifdef HAVE_vfprintf
int
chkr$vfprintf (FILE *f, const char *format, va_list args)
{
  CHKR_CHECK_STREAM (f);
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vfprintf", format, args, TYPE_PRINTF, 0);
  return vfprintf (f, format, args);
}
#endif /* HAVE_vfprintf */

#ifdef HAVE_vprintf
int
chkr$vprintf (const char *format, va_list param)
{
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vprintf", format, param, TYPE_PRINTF, 1);
  return vprintf (format, param);
}
#endif /* HAVE_vprintf */

#ifdef HAVE_vsprintf
int
chkr$vsprintf (char *str, const char *format, va_list param)
{
  int res;
  
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vsprintf", format, param, TYPE_PRINTF, 1);
  res = vsprintf (str, format, param);
  if (res == (int)str)
    stubs_chkr_set_right (str, strlen (str) + 1, CHKR_RW);
  else if (res >= 0)
    stubs_chkr_set_right (str, res + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_vsprintf */

#ifdef HAVE_vfscanf
int
chkr$vfscanf (FILE *stream, const char *format, va_list param)
{
  int n;
  
  CHKR_CHECK_STREAM (stream);
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vfscanf", format, param, TYPE_PRESCANF, 1);
  n = vfscanf (stream, format, param);
  if (n != EOF)
    check_printf_format ("vfscanf", format, param, n, 1);
  return n;
}
#endif /* HAVE_vfscanf */

#ifdef HAVE_vscanf
int
chkr$vscanf (const char *format, va_list param)
{
  int n;
  
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vscanf", format, param, TYPE_PRESCANF, 1);
  n = vscanf (format, param);
  if (n != EOF)
    check_printf_format ("vscanf", format, param, n, 1);
  return n;
}
#endif /* HAVE_vscanf */

#ifdef HAVE_vsscanf
int
chkr$vsscanf (const char *str, const char *format, va_list param)
{
  int n;
  
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vsscanf", format, param, TYPE_PRESCANF, 1);
  n = vsscanf (str, format, param);
  if (n != EOF)
    check_printf_format ("vsscanf", format, param, n, 1);
  return n;
}
#endif /* HAVE_vsscanf */

#ifdef HAVE_getw
int
chkr$getw (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (getw);
#else
  return getw (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getw */

#ifdef HAVE_putw
int
chkr$putw (int w, FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (putw);
#else
  return putw (w, stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_putw */

#ifdef HAVE_tempnam
char *
chkr$tempnam (const char *dir, const char *pfx)
{
  char *res;
  
  if (dir)
    stubs_chkr_check_str (dir, CHKR_RO, "dir");
  if (pfx)
    stubs_chkr_check_str (pfx, CHKR_RO, "pfx");
  res = tempnam (dir, pfx);
  if (res)
    stubs_chkr_set_right (res, strlen (res) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_tempnam */

#ifdef HAVE_getdelim
size_t
chkr$getdelim (char ** arg0, size_t * arg1, int arg2, FILE * arg3)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char *), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (size_t), CHKR_XX);
  stubs_chkr_check_addr (arg3, sizeof (FILE), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (getdelim);
#else
  {
    _G_ssize_t res;
    res = getdelim (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_getdelim */

#ifdef HAVE_snprintf
int
chkr$snprintf (char *str, size_t size, const char *format, ... )
{
  va_list param;
  int res;
  
  va_start (param, format);
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("snprintf", format, param, TYPE_PRINTF, 0);
  res = vsnprintf (str, size, format, param);
  if (res >= 0)
    stubs_chkr_set_right (str, res + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_snprintf */

#ifdef HAVE_vsnprintf
int
chkr$vsnprintf (char *str, size_t size, const char *format, va_list param)
{
  int res;
  
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vsnprintf", format, param, TYPE_PRINTF, 1);
  res = vsnprintf (str, size, format, param);
  if (res >= 0)
    stubs_chkr_set_right (str, res + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_vsnprintf */

#ifdef HAVE_asprintf
int
chkr$asprintf (char **ptr, const char *format, ... )
{
  va_list param;
  int res;
  
  va_start (param, format);
  stubs_chkr_check_addr (ptr, sizeof (char *), CHKR_WO, "ptr");
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("asprintf", format, param, TYPE_PRINTF, 0);
  res = vasprintf (ptr, format, param);
  if (res >= 0 && *ptr)
    stubs_chkr_set_right (*ptr, res + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_asprintf */

#ifdef HAVE_vasprintf
int
chkr$vasprintf (char **ptr, const char *format, va_list param)
{
  int res;
  
  stubs_chkr_check_addr (ptr, sizeof (char *), CHKR_WO, "ptr");
  stubs_chkr_check_str (format, CHKR_RO, "format");
  check_printf_format ("vasprintf", format, param, TYPE_PRINTF, 1);
  res = vasprintf (ptr, format, param);
  if (res >= 0 && *ptr)
    stubs_chkr_set_right (*ptr, res + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_vasprintf */

#ifdef HAVE_fdopen
FILE *
chkr$fdopen (int fd, const char *mode)
{
  stubs_chkr_check_str (mode, CHKR_RO, "mode");
  fd_used_by_prog (fd);
#if USE_BI_JUMP
  __builtin_jump (fdopen);
#else
  return fdopen (fd, mode);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fdopen */

#ifdef HAVE_fileno
int
chkr$fileno (FILE *stream)
{
  CHKR_CHECK_STREAM (stream);
#if USE_BI_JUMP
  __builtin_jump (fileno);
#else
  return fileno (stream);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_fileno */

#ifdef HAVE_popen
FILE *
chkr$popen (const char *command, const char *mode)
{
  FILE *res;
  stubs_chkr_check_str (command, CHKR_RO, "command");
  stubs_chkr_check_str (mode, CHKR_RO, "mode");
  res = popen (command, mode);
  if (res != NULL)
    fd_returned_by_system (fileno (res));
  return res;
}
#endif /* HAVE_popen */

#ifdef HAVE_pclose
int
chkr$pclose (FILE *stream)
{
  int res;
  int fd = fileno (stream);
  
  CHKR_CHECK_STREAM (stream);
  res = pclose (stream);
  if (res != -1)
    fd_closed (fd);
  return res;
}
#endif /* HAVE_pclose */

#endif /* HAVE_STDIO_H */
