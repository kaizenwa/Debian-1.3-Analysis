/*  Copyright (c) 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef VMS
# include <sys/types.h>
# include <sys/stat.h>
#else
# include "vms.h"
#endif

#include <slang.h>
#include "jdmacros.h"

#include "util.h"
#include "ttymsg.h"

char *slrn_skip_whitespace (char *b) /*{{{*/
{
   register char ch;
   
   while (((ch = *b) == ' ') || (ch == '\t') || (ch == '\n'))
     b++;
   return b;
}

/*}}}*/

/* returns a pointer to the end of the string */
char *slrn_trim_string (char *smin) /*{{{*/
{
   register char *s, ch;
   
   if (smin == NULL) return NULL;
   s = smin + strlen (smin);
   
   while (s > smin)
     {
	s--;
	ch = *s;
	if ((ch == ' ')
	    || (ch == '\n')
	    || (ch == '\t'))
	  {
	     *s = 0;
	     continue;
	  }
	
	s++;
	break;
     }
   return s;
}

/*}}}*/

char *slrn_strchr (char *s, char ch) /*{{{*/
{
   register char ch1;
   
   while (((ch1 = *s) != 0) && (ch != ch1)) s++;
   if (ch1 == 0) return NULL;
   return s;
}

/*}}}*/

/* Search for characters from list in string str.  If found, return a pointer
 * to the first occurrence.  If not found, return NULL. */
char *slrn_strbrk (char *str, char *list) /*{{{*/
{
   char ch, ch1, *p;
   
   while ((ch = *str) != 0)
     {
	p = list;
	while ((ch1 = *p) != 0)
	  {
	     if (ch == ch1) return str;
	     p++;
	  }
	str++;
     }
   return NULL;
}

/*}}}*/

char *slrn_simple_strtok (char *s, char *chp) /*{{{*/
{
   static char *s1;
   char ch = *chp;
   
   if (s == NULL)
     {
	if (s1 == NULL) return NULL;
	s = s1;
     }
   else s1 = s;
   
   while (*s1 && (*s1 != ch)) s1++;
   
   if (*s1 == 0)
     {
	s1 = NULL;
     }
   else *s1++ = 0;
   return s;
}

/*}}}*/


/* Note!!!  These routines assume a flat address space !! */

int slrn_case_strncmp (unsigned char *a, register unsigned char *b, register unsigned int n) /*{{{*/
{
   register unsigned char cha, chb, *bmax;
   register int diff = a - b;
   
   bmax = b + n;
   while (b < bmax)
     {
	cha = UPPER_CASE(b[diff]);
	chb = UPPER_CASE(*b);
	if (cha != chb)
	  {
	     return (int) cha - (int) chb;
	  }
	else if (chb == 0) return 0;
	b++;
     }
   return 0;
}

/*}}}*/

int slrn_case_strcmp (unsigned char *a, register unsigned char *b) /*{{{*/
{
   register unsigned char cha, chb;
   register int diff = a - b;
   
   while (1)
     {
	cha = UPPER_CASE(b[diff]);
	chb = UPPER_CASE(*b);
	if (cha != chb)
	  {
	     return (int) cha - (int) chb;
	  }
	else if (chb == 0) break;
	b++;
     }
   return 0;
}

/*}}}*/


static void fixup_path (char *path) /*{{{*/
{
   int len;
   
   len = strlen (path);
   if (len == 0) return;
   if (path[len - 1] == '/') return;
   path[len] = '/';
   path[len + 1] = 0;
}

/*}}}*/

/* dir and file could be the same in which case this performs a strcat. */
int slrn_dircat (char *dir, char *name, char *file)
{
   unsigned int len = 0;
   
   if (dir != NULL) len = strlen (dir);
   if (name != NULL) len += strlen (name);
   
   len += 2;			       /* for / and \0 */
   if (len > SLRN_MAX_PATH_LEN)
     {
	slrn_error ("File name too long.");
	return -1;
     }
   
   if (dir != NULL) 
     {
	if (dir != file) strcpy (file, dir);
	fixup_path (file);
     }
   else *file = 0;

   if (name != NULL) strcat (file, name);
   return 0;
}

/*{{{ Memory Allocation Routines */

static char *do_malloc_error (int do_error)
{
   if (do_error) slrn_error ("Memory allocation failure.");
   return NULL;
}

char *slrn_safe_strmalloc (char *s) /*{{{*/
{
   s = SLmake_string (s);
   if (s == NULL) slrn_exit_error ("Out of memory.");
   return s;
}

/*}}}*/

char *slrn_strmalloc (char *s, int do_error)
{
   s = SLmake_string (s);
   
   if (s == NULL)
     return do_malloc_error (do_error);
   
   return s;
}


char *slrn_malloc (unsigned int len, int do_memset, int do_error)
{   
   char *s;
   
   s = (char *) SLMALLOC (len);
   if (s == NULL)
     return do_malloc_error (do_error);

   if (do_memset)
     memset (s, 0, len);
   
   return s;
}

char *slrn_realloc (char *s, unsigned int len, int do_error)
{
   if (s == NULL)
     return slrn_malloc (len, 0, do_error);
   
   s = SLREALLOC (s, len);
   if (s == NULL)
     return do_malloc_error (do_error);
	
   return s;
}

char *slrn_safe_malloc (unsigned int len)
{
   char *s;
   
   s = slrn_malloc (len, 1, 0);

   if (s == NULL)
     slrn_exit_error ("Out of memory");
   
   return s;
}

void slrn_free (char *s)
{
   if (s != NULL) SLFREE (s);
}

/*}}}*/

char *slrn_fix_regexp (char *pat) /*{{{*/
{
   static char newpat[256];
   char *p, ch;
   
   p = newpat;
   *p++ = '^';
   while ((ch = *pat++) != 0)
     {
	if (ch == '.')
	  {
	     *p++ = '\\';
	  }
	else if (ch == '*')
	  {
	     *p++ = '.';
	  }
	*p++ = ch;
     }
   *p = 0;
   return newpat;
}

/*}}}*/

char *slrn_spool_dircat (char *root, char *name, int map_dots)
{
   char *spool_group, *p, ch;
   unsigned int len;

   len = strlen (root);

   spool_group = SLMALLOC(strlen(name) + len + 2);
   if (spool_group == NULL)
     {
	slrn_exit_error ("Out of memory.");
     }

   strcpy (spool_group, root);

   p = spool_group + len;
   if (len && (*(p - 1) != '/'))
     *p++ = '/';

   strcpy (p, name);

   if (map_dots) while ((ch = *p) != 0)
     {
	if (ch == '.') *p = '/';
	p++;
     }

   return spool_group;
}

int slrn_delete_file (char *f) /*{{{*/
{
#ifdef VMS
   return delete(f);
#else
   return unlink(f);
#endif
}

/*}}}*/

int slrn_fclose (FILE *fp) /*{{{*/
{
   if (0 == fclose (fp)) return 0;
   slrn_error ("Error closing file.  File system full? (errno = %d)", errno);
   return -1;
}

/*}}}*/


int slrn_file_exists (char *file) /*{{{*/
{
   struct stat st;
   int m;
   
#ifdef _S_IFDIR
# ifndef S_IFDIR
#  define S_IFDIR _S_IFDIR
# endif
#endif
   
#ifndef S_ISDIR
# ifdef S_IFDIR
#  define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
# else
#  define S_ISDIR(m) 0
# endif
#endif
   
   if (stat(file, &st) < 0) return 0;
   m = st.st_mode;
   
   if (S_ISDIR(m)) return (2);
   return 1;
}

/*}}}*/

char *slrn_basename (char *file)
{
   char *f;
#ifdef VMS
   f = slrn_strchr (file, ']');
   if (f != NULL) return f + 1;
   return file;
#else

   while (NULL != (f = slrn_strchr (file, '/')))
     file = f + 1;
   
   return file;
#endif
}
