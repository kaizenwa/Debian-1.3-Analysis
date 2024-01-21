/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - A D A I N T                              */
/*                                                                          */
/*                            $Revision: 1.74 $                             */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*          Copyright (C) 1992-1997, Free Software Foundation, Inc.         */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* As a  special  exception,  if you  link  this file  with other  files to */
/* produce an executable,  this file does not by itself cause the resulting */
/* executable to be covered by the GNU General Public License. This except- */
/* ion does not  however invalidate  any other reasons  why the  executable */
/* file might be covered by the  GNU Public License.                        */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/*  This file contains those routines named by Import pragmas in package    */
/*  GNAT.OS_Lib. Many of the subprograms in OS_Lib import standard library  */
/*  calls directly. This file contains all other routines.                  */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include "config.h"  /* this may define MSDOS */
#if defined (__EMX__) || defined (MSDOS)
#include <process.h>
#include <string.h>
#endif
#if defined (__EMX__)
#include <sys/emx.h>
#endif
#include "a-adaint.h"

/* Define symbols O_BINARY and O_TEXT as harmless zeroes if they are not
   defined in the current system. On DOS-like systems these flags control
   whether the file is opened/created in text-translation mode (CR/LF in
   external file mapped to LF in internal file), but in Unix-like systems,
   no text translation is required, so these flags have no effect.
*/


#if defined (__EMX__)
#include <os2.h>
#endif

#if defined (MSDOS)
#include <dos.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef O_TEXT
#define O_TEXT 0
#endif

extern char *getenv ();

#ifndef EXECUTABLE_SUFFIX
#define EXECUTABLE_SUFFIX ""
#endif

#ifndef OBJECT_SUFFIX
#define OBJECT_SUFFIX ".o"
#endif

#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ':'
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

void
to_gm_time (p_time, p_year, p_month, p_day, p_hours, p_mins, p_secs)
     time_t *p_time;
     int *p_year, *p_month, *p_day, *p_hours, *p_mins, *p_secs;
{
  struct tm *res = gmtime (p_time);

  *p_year = res->tm_year;
  *p_month = res->tm_mon;
  *p_day = res->tm_mday;
  *p_hours = res->tm_hour;
  *p_mins = res->tm_min;
  *p_secs = res->tm_sec;
}
/* Return the maximum file name length.  */

int
Get_Maximum_File_Name_Length ()
{
#if defined(MSDOS)
  return 8;
#elif defined (VMS)
  return 39;
#else
  return -1;
#endif
}

/* Return the default switch character.  */

char
Get_Switch_Character ()
{
  /* Under MSDOS, the switch character is not normally a hyphen, but this is
     the convention DJGPP uses. Similarly under OS2, the switch character is
     not normally a hypen, but this is the convention EMX uses.
   */
  return '-';
}

/* Return nonzero if file names are case sensitive.  */

int
Get_File_Names_Case_Sensitive ()
{
#if defined (__EMX__) || defined (MSDOS) || defined (VMS) || defined(WINNT)
  return 0;
#else
  return 1;
#endif
}

char
Get_Default_Identifier_Character_Set ()
{
#if defined (__EMX__) || defined (MSDOS)
  return 'p';
#else
  return '1';
#endif
}

char
Get_Dirsep_Char ()
{
  return DIR_SEPARATOR;
}

/* Return the path separator.   */

char
Get_Pathsep_Char ()
{
  return PATH_SEPARATOR;
}

/* Return the suffix for object files */
void
get_object_suffix_ptr (len, value)
     int *len;
     char **value;
{
  *value = OBJECT_SUFFIX;
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* Return the suffix for executable files */
void
get_executable_suffix_ptr (len, value)
     int *len;
     char **value;
{
  *value = EXECUTABLE_SUFFIX;
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* Return the suffix for debuggable files. Usually this is the same as the
   executable extension. */
void
get_debuggable_suffix_ptr (len, value)
     int *len;
     char **value;
{
#ifndef MSDOS
  *value = EXECUTABLE_SUFFIX;
#else
  /* On DOS, the extensionless COFF file is what gdb likes. */
  *value = "";
#endif
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

int
open_read (path, fmode)
     char *path;
     int fmode;
{
  int fd;

#ifdef VMS
  /* Optional arguments increase read performance, may be applicable to
     write too, but not sure. */
  if (fmode)
    fd = open (path, O_RDONLY | O_TEXT, 0444,
               "mbc=16", "deq=64", "fop=tef");
  else
    fd = open (path, O_RDONLY | O_BINARY, 0444,
               "mbc=16", "deq=64", "fop=tef");
#else
  if (fmode)
#ifdef __vxworks
    fd = open (path, O_RDONLY | O_TEXT, 0444);
#else
    fd = open (path, O_RDONLY | O_TEXT);
#endif
  else
#ifdef __vxworks
    fd = open (path, O_RDONLY | O_BINARY, 0444);
#else
    fd = open (path, O_RDONLY | O_BINARY);
#endif
#endif

  return fd < 0 ? -1 : fd;
}


#if defined (__EMX__)
#define PERM (S_IREAD | S_IWRITE)
#else
#define PERM (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#endif

int
open_rw (path, fmode)
     char *path;
     int  fmode;
{
  int fd;

  if (fmode)
    fd = open (path, O_RDWR | O_TEXT, PERM);
  else
    fd = open (path, O_RDWR | O_BINARY, PERM);

  return fd < 0 ? -1 : fd;
}

int
open_create (path, fmode)
     char *path;
     int  fmode;
{
  int fd;

  if (fmode)
    fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | O_TEXT, PERM);
  else
    fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, PERM);

  return fd < 0 ? -1 : fd;
}

/*  Open a new file.  Return error (-1) if the file already exists. */

int
open_new (path, fmode)
     char *path;
     int fmode;
{
  int fd;
  if (fmode)
    fd =  open (path, O_WRONLY | O_CREAT | O_EXCL | O_TEXT, PERM);
  else
    fd =  open (path, O_WRONLY | O_CREAT | O_EXCL | O_BINARY, PERM);

  return fd < 0 ? -1 : fd;
}

/* Return the number of bytes in the specified file. */

long
file_length (fd)
     int fd;
{
  int ret;
  struct stat statbuf;

  ret = fstat (fd, &statbuf);
  if (ret || !S_ISREG (statbuf.st_mode))
    return 0;

  return (statbuf.st_size);
}

/* Return a GNAT time stamp given a file name.  */

time_t
file_time_name (name)
     char *name;
{
  struct stat statbuf;

#if defined (__EMX__) || defined (MSDOS)
  int fd = open (name, O_RDONLY | O_BINARY);
  time_t ret = file_time_fd (fd);
  close (fd);
  return ret;

#else

  int ret = stat (name, &statbuf);
#ifdef VMS
  /* VMS has file versioning */
  return statbuf.st_ctime;
#else
  return statbuf.st_mtime;
#endif
#endif
}

/* Return a GNAT time stamp given a file descriptor.  */

time_t
file_time_fd (fd)
     int fd;
{
  /* The following workaround code is due to the fact that under EMX and DJGPP
     fstat attempts to convert time values to GMT rather than keep the actual
     OS timestamp of the file. By using the OS2/DOS functions directly the GNAT
     timestamp are independent of this behavior, which is desired to facilitate
     the distribution of GNAT compiled libraries. */

#if defined (__EMX__) || defined (MSDOS)
#ifdef __EMX__

  FILESTATUS fs;
  int ret = DosQueryFileInfo (fd, 1, (unsigned char *) &fs,
				sizeof (FILESTATUS));

  unsigned file_year  = fs.fdateLastWrite.year;
  unsigned file_month = fs.fdateLastWrite.month;
  unsigned file_day   = fs.fdateLastWrite.day;
  unsigned file_hour  = fs.ftimeLastWrite.hours;
  unsigned file_min   = fs.ftimeLastWrite.minutes;
  unsigned file_tsec  = fs.ftimeLastWrite.twosecs;

#else
  struct ftime fs;
  int ret = getftime (fd, &fs);

  unsigned file_year  = fs.ft_year;
  unsigned file_month = fs.ft_month;
  unsigned file_day   = fs.ft_day;
  unsigned file_hour  = fs.ft_hour;
  unsigned file_min   = fs.ft_min;
  unsigned file_tsec  = fs.ft_tsec;
#endif

  /* Calculate the seconds since epoch from the time components. First count
     the whole days passed.  The value for years returned by the DOS and OS2
     functions count years from 1980, so to compensate for the UNIX epoch which
     begins in 1970 start with 10 years worth of days and add days for each
     four year period since then. */

  time_t tot_secs;
  int cum_days [12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
  int days_passed = 3652 + (file_year / 4) * 1461;
  int years_since_leap = file_year % 4;
  if      (years_since_leap == 1) days_passed += 366;
  else if (years_since_leap == 2) days_passed += 731;
  else if (years_since_leap == 3) days_passed += 1096;
  if (file_year > 20) days_passed -= 1;
  days_passed += cum_days [file_month - 1];
  if (years_since_leap == 0 && file_year != 20
      && file_month > 2) days_passed++;
  days_passed += file_day - 1;

  /* OK - have whole days.  Multiply -- then add in other parts. */
  tot_secs  = days_passed               * 86400;
  tot_secs += file_hour   * 3600;
  tot_secs += file_min * 60;
  tot_secs += file_tsec * 2;

  return tot_secs;

#else
  struct stat statbuf;
  int ret = fstat (fd, &statbuf);
#ifdef VMS
  /* VMS has file versioning */
  return statbuf.st_ctime;
#else
  return statbuf.st_mtime;
#endif
#endif
}

void
get_env_value_ptr (name, len, value)
     char *name;
     int *len;
     char **value;
{
  *value = getenv (name);
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

int
is_regular_file (name)
     char *name;
{
  int ret;
  struct stat statbuf;

  ret = stat (name, &statbuf);
  return (!ret && S_ISREG (statbuf.st_mode));
}

int
is_directory (name)
     char *name;
{
  int ret;
  struct stat statbuf;

  ret = stat (name, &statbuf);
  return (!ret && S_ISDIR (statbuf.st_mode));
}

#ifdef VMS
/* Defined in VMS header files */
#define fork() (decc$$alloc_vfork_blocks() >= 0 ? \
               lib$get_current_invo_context(decc$$get_vfork_jmpbuf()) : -1)
#endif

int
portable_spawn (args)
    char *args[];
{
  int status;
  int finished;
  int pid;

#if defined (__EMX__) || defined (MSDOS)
  if (spawnvp (P_WAIT, args [0], args) != 0)
    return (4);
#else
  pid = fork ();
  if (pid == -1)
    return (4);
  if (pid == 0) {
    /* The child */
    execv (args [0], args);
    return (4);
  }

  /* The parent */
#ifdef VMS
  /* Wait doesn't do the right thing on VMS */
  finished = waitpid (-1, &status, 0);
#else
  finished = wait (&status);
#endif
  if (finished != pid || status & 0xffff)
    return 4;
#endif
  return 0;
}

int
portable_no_block_spawn (args)
    char *args[];
{
  int pid = 0;

#if defined (__EMX__) || defined (MSDOS)
  /* ??? For PC machines I (Franco) don't know the system calls to
     implement this routine. So I'll fake it as follows. This routine
     will behave exactly like the blocking portable_spawn and will
     systematically return a pid of 0 unless the spawned task did not
     complete successfully, in which case we return a pid of -1.  To
     synchronize with this the portable_wait below systematically
     returns a pid of 0 and reports that the subprocess terminated
     successfully. */

  if (spawnvp (P_WAIT, args [0], args) != 0)
    return (-1);
#else
  pid = fork ();

  if (pid == 0) {
    /* The child */
    execv (args [0], args);
    return (-1);
  }
#endif

  return pid;
}

int
portable_wait (process_status)
    int *process_status;
{
  int status = 0;
  int pid    = 0;

#if defined (__EMX__) || defined (MSDOS)
  /* ??? See corresponding comment in portable_no_block_spawn. */

#else
#ifdef VMS
  /* Wait doesn't do the right thing on VMS */
  pid    = waitpid (-1, &status, 0);
#else
  pid    = wait (&status);
#endif
  status = status & 0xffff;
#endif

  *process_status = status;
  return pid;
}

void
os_exit (status)
     int status;
{
#ifdef VMS
  /* Exit without changing 0 to 1 */
  __posix_exit (status);
#else
  exit (status);
#endif
}

/* Locate a regular file, give a Path value */

char *
locate_regular_file (file_name, path_val)
     char *file_name;
     char *path_val;
{
  int len;
  char *ptr;

  /* Handle absolute pathnames. */
  for (ptr = file_name; *ptr && *ptr != '/' && *ptr != DIR_SEPARATOR; ptr++)
    ;

  if (*ptr != 0
#if defined(__EMX__) || defined(MSDOS) || defined(WINNT)
      || isalpha (file_name [0]) && file_name [1] == ':'
#endif
     )
    {
      if (is_regular_file (file_name))
	return xstrdup (file_name);

      return 0;
    }

  if (path_val == 0)
    return 0;

  {
    /* The result has to be smaller than path_val + file_name.  */
    char *file_path = alloca (strlen (path_val) + strlen (file_name) + 1);
    char pathsep_char = Get_Pathsep_Char ();

    for (;;)
      {
	for (; *path_val == pathsep_char ; path_val++)
	  ;

      if (*path_val == 0)
	return 0;

      for (ptr = file_path; *path_val && *path_val != pathsep_char; )
        *ptr++ = *path_val++;

      ptr--;
      if (*ptr != '/' && *ptr != DIR_SEPARATOR)
        *++ptr = DIR_SEPARATOR;

      strcpy (++ptr, file_name);

      if (is_regular_file (file_path))
        return xstrdup (file_path);
      }
  }

  return 0;
}

/* Locate an executable given a Path argument. This routine is only used by
   gnatbl and should not be used otherwise.  Use locate_exec_on_path
   instead. */

char *
locate_exec (exec_name, path_val)
     char *exec_name;
     char *path_val;
{
  if (!strstr (exec_name, EXECUTABLE_SUFFIX))
    {
      char *full_exec_name
	= alloca (strlen(exec_name) + strlen(EXECUTABLE_SUFFIX) + 1);

      strcpy (full_exec_name, exec_name);
      strcat (full_exec_name, EXECUTABLE_SUFFIX);
      return locate_regular_file (full_exec_name, path_val);
    }
  else
    return locate_regular_file (exec_name, path_val);
}

/* Locate an executable using the Systems default PATH */

char *
locate_exec_on_path (exec_name)
     char *exec_name;
{
#ifdef VMS
  char *path_val = getenv ("VAXC$PATH");
#else
  char *path_val = getenv ("PATH");
#endif

  return locate_exec (exec_name, path_val);
}

#ifdef VMS
void
adjust_os_resource_limits ()
{
  sys$adjwsl (131072, 0);
}

#else
void
adjust_os_resource_limits () {;}
#endif
