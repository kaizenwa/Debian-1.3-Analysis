/* Various functions of utilitarian nature.
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* $Id: utils.c,v 1.1.1.1.2.1 1997/02/15 19:23:20 hniksic Exp $ */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif /* HAVE_STRING_H */
#include <ctype.h>

#ifdef HAVE_PWD_H
# include <pwd.h>
#endif

#ifdef WINDOWS
# include <direct.h>
# define mkdir(a, b) _mkdir(a)  /* bletch! */
#endif /* WINDOWS */

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <assert.h>
#include <limits.h>
#ifdef HAVE_UTIME_H
#  include <utime.h>
#endif
#ifdef HAVE_SYS_UTIME_H
# include <sys/utime.h>
#endif
#include <errno.h>

#include "wget.h"
#include "utils.h"
#include "options.h"
#include "mtch.h"

#ifndef errno
extern int errno;
#endif

extern struct options opt;


/* nmalloc, nrealloc and nstrdup exit the program if there is not
   enough memory. nstrdup also implements strdup on systems that
   do not have it. */
void *
nmalloc(size_t size)
{
   void *res;

   res = malloc(size);
   if (!res)
      memfatal("malloc");
   return res;
}

void *
nrealloc(void *obj, size_t size)
{
   void *res;

   /* Not all Unixes have the feature of realloc() that calling it
      with a NULL-pointer is the same as malloc(), but it is easy to
      simulate. */
   if (obj)
      res = realloc(obj, size);
   else
      res = malloc(size);
   if (!res)
      memfatal("realloc");
   return res;
}

char *
nstrdup(const char *s)
{
   char *s1;

#ifndef HAVE_STRDUP
   int l;
   
   l = strlen(s);
   s1 = malloc(l + 1);
   if (!s1)
      memfatal("strdup");
   memcpy(s1, s, l + 1);
   return s1;
#else
   s1 = strdup(s);
   if (!s1)
      memfatal("strdup");
   return s1;
#endif
}

/* Croak the fatal memory error and bail out with non-zero exit
   status. */
void
memfatal(const char *s)
{
   fprintf(opt.lfile, "%s: Not enough memory.\n", s);
   exit(1);
}

/* Copy the string formed by two pointers (one on the beginning, other
   on the char after the last char) to a new, malloc-ed
   location. 0-terminate it. */
char *
strdupdelim(const char *beg, const char *end)
{
   char *res;

   res = (char *)nmalloc(end - beg + 1);
   memcpy(res, beg, end - beg);
   res[end - beg] = '\0';
   return res;
}

/* Returns an error message for the error ERRNUM.  Requires more work.  */
const char *
uerrmsg(uerr_t errnum)
{
   switch (errnum)
   {
      case URLUNKNOWN:
	 return "Unknown/unsupported protocol";
	 break;
      case URLBADPORT:
	 return "Invalid port specification";
	 break;
      case URLBADHOST:
	 return "Invalid host name";
	 break;
      default:
	 assert(0);
   }
}
	 

/* Parse a string containing comma-separated elements, and return a
   vector of char pointers with the elements.  Spaces following the
   commas are ignored.  */
char **
sepstring(const char *s)
{
   char **res;
   const char *p;
   int i;

   if (!s || !*s)
      return NULL;
   res = NULL;
   p = s;
   i = 0;
   while (*s)
   {
      if (*s == ',')
      {
	 res = (char **)nrealloc(res, (i + 2) * sizeof(char *));
	 res[i] = strdupdelim(p, s);
	 res[++i] = NULL;
	 ++s;
	 /* Skip the blanks following the ','. */
	 while (isspace(*s))
	    ++s;
	 p = s;
      }
      else
	 ++s;
   }
   res = (char **)nrealloc(res, (i + 2) * sizeof(char *));
   res[i] = strdupdelim(p, s);
   res[++i] = NULL;
   return res;
}

/* Compare s1 and s2 frontally; s1 must be a subset of s2.  E.g. if s1
   is `/something', s2 must begin with `/something' to make frontcmp
   return 1.  Otherwise, frontcmp will return 0. */
int
frontcmp(const char *s1, const char *s2)
{
   for (; *s1 && *s2 && (*s1 == *s2); ++s1, ++s2);
   return !*s1;
}

/* A cuserid() immitation using getpwuid(), to avoid hassling with
   utmp.  Besides, not all systems have cuerid(). */

char *
mycuserid(char *where)
{

#ifdef WINDOWS
   if (where)
      return nstrdup("");
   else
      return NULL;
#else /* not WINDOWS */
   struct passwd *pwd;

   if (!(pwd = getpwuid(getuid())) || !pwd->pw_name)
      return NULL;
   if (where)
   {
      strcpy(where, pwd->pw_name);
      return where;
   }
   else
      return pwd->pw_name;
#endif /* not WINDOWS */
}

/* Canonicalize PATH, and return a new path.  The new path differs from PATH
   in that:
	Multple `/'s are collapsed to a single `/'.
	Leading `./'s and trailing `/.'s are removed.
	Trailing `/'s are removed.
	Non-leading `../'s and trailing `..'s are handled by removing
	portions of the path.

   E.g. "a/b/c/./../d/.." will yield "a/b".

   Changes by hniksic:
	Always use '/' as stub_char.
	Don't check for local things using canon_stat.
	Change the original string instead of strdup-ing.
	React correctly when beginning with `./' and `../'.  */
void
path_simplify(char *path)
{
   register int i, start, ddot;
   char stub_char;

   if (!*path)
      return;
   
   /*stub_char = (*path == '/') ? '/' : '.';*/
   stub_char = '/';

   /* Addition: Remove all `./'-s preceding the string.  If `../'-s
      precede, put `/' in front and remove them too.  */
   i = 0;
   ddot = 0;
   while (1)
   {
      if (path[i] == '.' && path[i + 1] == '/')
	 i += 2;
      else if (path[i] == '.' && path[i + 1] == '.' && path[i + 2] == '/')
      {
	 i += 3;
	 ddot = 1;
      }
      else
	 break;
   }
   if (i)
      strcpy(path, path + i - ddot);
   
   /* Replace single `.' or `..' with `/'. */
   if ((path[0] == '.' && path[1] == '\0')
       || (path[0] == '.' && path[1] == '.' && path[2] == '\0'))
   {
      path[0] = stub_char;
      path[1] = '\0';
      return;
   }
   /* Walk along PATH looking for things to compact. */
   i = 0;
   while (1)
   {
      if (!path[i])
	 break;

      while (path[i] && path[i] != '/')
	 i++;

      start = i++;

      /* If we didn't find any slashes, then there is nothing left to do. */
      if (!path[start])
	 break;

      /* Handle multiple `/'s in a row. */
      while (path[i] == '/')
	 i++;

      if ((start + 1) != i)
      {
	 strcpy (path + start + 1, path + i);
	 i = start + 1;
      }

      /* Check for trailing `/'. */
      if (start && !path[i])
      {
	zero_last:
	 path[--i] = '\0';
	 break;
      }

      /* Check for `../', `./' or trailing `.' by itself. */
      if (path[i] == '.')
      {
	 /* Handle trailing `.' by itself. */
	 if (!path[i + 1])
	    goto zero_last;

	 /* Handle `./'. */
	 if (path[i + 1] == '/')
	 {
	    strcpy (path + i, path + i + 1);
	    i = (start < 0) ? 0 : start;
	    continue;
	 }

	 /* Handle `../' or trailing `..' by itself. */
	 if (path[i + 1] == '.' &&
	     (path[i + 2] == '/' || !path[i + 2]))
	 {
	    while (--start > -1 && path[start] != '/');
	    strcpy (path + start + 1, path + i + 2);
	    i = (start < 0) ? 0 : start;
	    continue;
	 }
      }	/* path == '.' */
   } /* while */

   if (!*path)
   {
      *path = stub_char;
      path[1] = '\0';
   }
}

/* "Touch" FILE, i.e. make its atime and mtime equal to the time
   specified with TM.  */
void
my_touch(char *file, time_t tm)
{
#ifdef HAVE_STRUCT_UTIMBUF
   struct utimbuf times;
   times.actime = times.modtime = tm;
#else
   time_t times[2];
   times[0] = times[1] = tm;
#endif
   
   if (utime(file, &times) == -1)
   {
      if (!opt.quiet)
	 fprintf(opt.lfile, "utime: %s\n", mystrerror(errno));
   }
}

/* Checks if a file is a symbolic link, and removes it if it is.  Does
   nothing under MS-Windows.  */
int
remove_link(const char *file)
{
   int err = 0;
   struct stat st;
   
#ifndef WINDOWS
   if (lstat(file, &st) == 0 && S_ISLNK(st.st_mode))
   {
#ifdef DEBUG
      if (opt.debug)
	 fprintf(opt.lfile, "Unlinking %s (symlink).\n", file);
#endif
      err = unlink(file);
      if (err != 0)
	 if (opt.verbose)
	    fprintf(opt.lfile, "Failed to unlink symlink `%s': %s\n",
		    file, mystrerror(errno));
   }
#endif /* WINDOWS */
   return err;
}

/* Does a file exist? This is quite a lousy implementation, since it
   supplies no error codes -- only a yes-or-no answer. Thus it will
   return that a file does not exist if, e.g., the directory is
   unreadable. I don't mind it too much currently, though.  The proper
   way should, of course, be to have a third, error state, other than
   true/false, but that would make the calling functions much more
   complex. */
int
exists(const char *filename)
{
   struct stat buf;
   return stat(filename, &buf) ? 0 : 1;
}

/* Returns 0 if the path is a directory, 1 otherwise. Returns 0 on
   error. */
int
isfile(const char *path)
{
   struct stat buf;
   if (stat(path, &buf) != 0)
      return 0;
   return S_ISDIR(buf.st_mode) ? 0 : 1;
}

/* The function that takes the dirname to be created, making sure that
   missing directories are made one by one. Its behaviour should be
   similar to mkdir -p on systems that support it. */
int
mymkdir(const char *d)
{
   int i, status, quit;
   char *dir;
   struct stat stbuf;

   /* Make a copy of dir, to be able to write to it. Otherwise, the
      function is unsafe if called with a read-only char *argument. */
   dir = nstrdup(d);
   /* If the first character of dir is '/', skip it (and thus enable
      creation of absolute-pathname directories. */
   quit = 0;
   for (i = (*dir == '/'); 1; ++i)
   {
      for (; dir[i] && dir[i] != '/'; i++)
	 ;
      if (!dir[i])
	 quit = 1;
      dir[i] = '\0';
      /* Check whether the directory already exists.  */
      status = stat(dir, &stbuf);
      if (status != 0)
      {
	 if (mkdir(dir, opt.dirmode) < 0)
	 {
	    free(dir);
	    return -1;
	 }
      }
      if (quit)
	 break;
      else
	 dir[i] = '/';
   } /* for */
   free(dir);
   return 0;
}

/* Determine whether a file is acceptable to be followed, according to
   lists of patterns to accept/reject. */
int
acceptable(const char *s)
{
   int l = strlen(s);

   while (l && s[l] != '/')
      --l;
   if (s[l] == '/')
      s += (l + 1);
   if (opt.accepts)
   {
      if (opt.rejects)
	 return (in_acclist((const char **)opt.accepts, s, 1)
		 && !in_acclist((const char **)opt.rejects, s, 1));
      else
	 return in_acclist((const char **)opt.accepts, s, 1);
   }
   else if (opt.rejects)
      return !in_acclist((const char **) opt.rejects, s, 1);
   return 1;
}

/* Returns whether a directory is acceptable for download, wrt
   include/exclude lists.

   If the argument flags is ALLABS, the leading '/' is ignored in
   paths; relative and absolute paths may be freely intermixed. */
int
accdir(const char *s, enum accd flags)
{
   char **x, *p;

   /* Remove starting '/'. */
   if (flags & ALLABS && *s == '/')
      ++s;
   if (opt.includes)
   {
      for (x = opt.includes; *x; x++)
      {
	 p = *x + (flags & ALLABS && **x == '/'); /* Remove '/' */
	 if (frontcmp(p, s))
	    break;
      }
      if (!*x)
	 return 0;
   }
   if (opt.excludes)
   {
      for (x = opt.excludes; *x; x++)
      {
	 p = *x + (flags & ALLABS && **x == '/'); /* Remove '/' */
	 if (frontcmp(p, s))
	    break;
      }
      if (*x)
	 return 0;
   }
   return 1;
}

/* Match a string against a pattern, backwards. E.g.:

   match_backwards("abc", "bc") -> 1
   match_backwards("abc", "ab") -> 0
   match_backwards("abc", "abc") -> 1 */
int
match_backwards(const char *string, const char *pattern)
{
   int i, j;
   
   for (i = strlen(string), j = strlen(pattern); i >= 0 && j >= 0; i--, j--)
      if (string[i] != pattern[j])
	 break;
   /* If the pattern was exhausted, the match was succesful. */
   if (j == -1)
      return 1;
   else
      return 0;
}

/* Does a URL match each element of a list. List elements are matched
   with fnmatch() or match_backwards(), according to whether the
   pattern (or suffix) contains globbing characters.

   If the argument backward is unset, don't do backward comparison --
   just compare them normally.  */
int
in_acclist(const char **accepts, const char *s, int backward)
{
   for (; *accepts; accepts++)
   {
      if (has_wildcards(*accepts))
      {
	 /* fnmatch returns 0 if the pattern *does* match the
	    string. */
	 if (fnmatch(*accepts, s, 0) == 0)
	    return 1;
      }
      else
      {
	 if (backward)
	 {
	    if (match_backwards(s, *accepts))
	       return 1;
	 }
	 else
	 {
	    if (!strcmp(s, *accepts))
	       return 1;
	 }
      }
   }
   return 0;
}

/* Return the malloc-ed suffix of a filename */
char *
suffix(const char *s)
{
   int i;
   
   for (i = strlen(s); i && s[i] != '/' && s[i] != '.'; i--);
   if (s[i++] == '.')
      return nstrdup(s + i);
   else
      return NULL;
}

/* The function reads a whole line. It reads the line realloc-ing the
   storage exponentially, doubling the storage after each overflow to
   minimize the number of calls to realloc().

   It is not an exemplary of correctness, since it kills off the
   newline (and no, there is no way to know if there was a newline at
   EOF). */
char *
read_whole_line(FILE *fp)
{
   char *line;
   int i, bufsize, c;

   i = 0;
   bufsize = DYNAMIC_LINE_BUFFER;
   line = nmalloc(bufsize);
   /* Construct the line. */
   while ((c = getc(fp)) != EOF && c != '\n')
   {
      if (i > bufsize - 1)
	 line = (char *)nrealloc(line, (bufsize <<= 1));
      line[i++] = c;
   }
   if (c == EOF && !i)
   {
      free(line);
      return NULL;
   }
   /* Check for overflow at zero-termination (no need to double the
      buffer in this case. */
   if (i == bufsize)
      line = (char *)nrealloc(line, i + 1);
   line[i] = '\0';
   return line;
}

/* Load file to memory, return the malloc-ed buffer, and the file
   size. The file is loaded in chunks, each one double the size of the
   previous one. The first chunk is FILE_BUFFER_SIZE bytes long. */
void
load_file(FILE *fp, char **buf, long *nread)
{
   long bufsize;

   bufsize = FILE_BUFFER_SIZE;
   *nread = 0;
   *buf = NULL;
   while (!feof(fp))
   {
      *buf = (char *)nrealloc(*buf, bufsize + *nread);
      *nread += fread(*buf + *nread, sizeof(char), bufsize, fp);
      bufsize <<= 1;
   }
}

/* Free the pointers in a NULL-terminated vector of pointers, then
   free the pointer itself. */
void
free_vec(char **vec)
{
   int i;

   if (!vec)
      return;
   for (i = 0; vec[i]; i++)
      free(vec[i]);
   free(vec);
}

/* Merge the two vectors (v1 will be placed before of v2).  The
   function effectively frees the vectors v1 and v2 (their contents
   must not be reused after the call).  If v1 is NULL, the function
   returns v2. */
char **
merge_vecs(char **v1, char **v2)
{
   int i, j;

   if (!v1)
      return v2;
   if (!v2)
      return v1;
   if (!*v2)
   {
      /* To avoid j == 0 */
      free(v2);
      return v1;
   }
   /* Count v1. */
   for (i = 0; v1[i]; i++);
   /* Count v2. */
   for (j = 0; v2[j]; j++);
   /* Reallocate v1. */
   v1 = (char **)nrealloc(v1, (i + j + 1) * sizeof(char **));
   memcpy(v1 + i, v2, (j + 1) * sizeof(char *));
   free(v2);
   return v1;
}

/* A set of simple-minded routines to store and search for strings in
   a linked list. You may add a string to the slist, and peek whether
   it's still in there at any time later. */

/* Add an element to the list.  If flags is NOSORT, the list will not
   be sorted.  */
slist *
add_slist(slist *l, const char *s, int flags)
{
   slist *t, *old, *beg;
   int cmp;

   if (flags & NOSORT)
   {
      if (!l)
      {
	 t = (slist *)nmalloc(sizeof(slist));
	 t->string = nstrdup(s);
	 t->next = NULL;
	 return t;
      }
      beg = l;
      /* Find the last element. */
      while (l->next)
	 l = l->next;
      t = (slist *)nmalloc(sizeof(slist));
      l->next = t;
      t->string = nstrdup(s);
      t->next = NULL;
      return beg;
   }
   /* Empty list or changing the first element. */
   if (!l || (cmp = strcmp(l->string, s)) > 0)
   {
      t = (slist *)nmalloc(sizeof(slist));
      t->string = nstrdup(s);
      t->next = l;
      return t;
   }

   beg = l;
   if (cmp == 0)
      return beg;

   /* Second two one-before-the-last element. */
   while (l->next)
   {
      old = l;
      l = l->next;
      cmp = strcmp(s, l->string);
      if (cmp == 0)             /* No repeating in the list. */
	 return beg;
      else if (cmp > 0)
	 continue;
      /* If the next list element is greater than s, put s between the
	 current and the next list element. */
      t = (slist *)nmalloc(sizeof(slist));
      old->next = t;
      t->next = l;
      t->string = nstrdup(s);
      return beg;
   }
   t = (slist *)nmalloc(sizeof(slist));
   t->string = nstrdup(s);
   /* Insert the new element after the last element. */
   l->next = t;
   t->next = NULL;
   return beg;
}   

/* Is there a specific entry in the list? */
int
in_slist(slist *l, const char *s)
{
   int cmp;

   while (l)
   {
      cmp = strcmp(l->string, s);
      if (cmp == 0)
	 return 1;
      else if (cmp > 0)         /* The list is ordered! */
	 return 0;
      l = l->next;
   }
   return 0;
}

/* Free the whole slist. */
void
free_slist(slist *l)
{
   slist *n;

   while (l)
   {
      n = l->next;
      free(l->string);
      free(l);
      l = n;
   }
}

/* Legible -- return a static pointer to the legibly printed long. */
char *
legible(long l)
{
   static char buf[20];
   char inbuf[20];
   int i, i1, mod;
   char *ptr, *in;

   /* Fill the buffer. */
   prnum(inbuf, l);
   /* Reset the pointers. */
   ptr = buf;
   in = inbuf;
   /* If the number is negative, shift the pointers. */
   if (*in == '-')
   {
      *ptr++ = '-';
      ++in;
   }
   /* How many digits before the first separator? */
   mod = strlen(in) % 3;
   /* Insert them. */
   for (i = 0; i < mod; i++)
      *ptr++ = in[i];
   /* Now insert the rest of them, putting separator before every
      third digit. */
   for (i1 = i, i = 0; in[i1]; i++, i1++)
   {
      if (i % 3 == 0 && i1 != 0)
	 *ptr++ = LEGIBLE_SEPARATOR;
      *ptr++ = in[i1];
   }
   /* Zero-terminate the string. */
   *ptr = '\0';
   return buf;
}

/* How many digits in a (long) integer? */
int
numdigit(long a)
{
   int res;

   for (res = 1; a /= 10; res++);
   return res;
}

/* Print a long integer to the string buffer.  The digits are first
   written in reverse order (the least significant digit first), and
   are then reversed.  */
void
prnum(char *where, long num)
{
   char *p;
   int i = 0, l;
   char c;

   if (num < 0)
   {
      *where++ = '-';
      num = -num;
   }
   p = where;
   /* Print the digits to the string. */
   do
   {
      *p++ = num % 10 + '0';
      num /= 10;
   } while (num);
   /* And reverse them. */
   l = p - where - 1;
   for (i = l/2; i >= 0; i--)
   {
      c = where[i];
      where[i] = where[l - i];
      where[l - i] = c;
   }
   where[l + 1] = '\0';
}

