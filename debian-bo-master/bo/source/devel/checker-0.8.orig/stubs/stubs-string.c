/* Checker stubs for functions defined in string.h
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

#ifdef HAVE_STRING_H
#include <string.h>
#include "checker_api.h"

#undef HAVE_memccpy
#undef HAVE_strxfrm
#undef HAVE_memmem
#undef HAVE_strsep
#undef HAVE_stpncpy
#undef HAVE_strfry
#undef HAVE_memfrob
#undef HAVE_swab

#if 0
#define HAVE_strrchr
#define HAVE_strcat
#define HAVE_memmove
#define HAVE_memcpy
#define HAVE_strncmp
#define HAVE_strncpy
#define HAVE_strcpy
#define HAVE_strlen
#define HAVE_rindex
#define HAVE_strcmp
#define HAVE_strchr
#define HAVE_strerror
#define HAVE_index
#define HAVE_bcopy
#define HAVE_bcmp
#define HAVE_bzero
#define HAVE_strstr
#define HAVE_memset
#define HAVE_memcmp
#define HAVE_strncat
#define HAVE_stpcpy
#define HAVE_strdup
#define HAVE_strcoll
#define HAVE_memchr
#define HAVE_strspn
#define HAVE_strpbrk
#define HAVE_strtok
#define HAVE_strcspn
#define HAVE_strncasecmp
#define HAVE_strcasecmp
#endif

size_t strnlen (const char *s, size_t n);

/* compiled from: . */
#ifdef HAVE_memcpy
void *
chkr$memcpy (void *dest, const void *src, size_t n)
{
  if (n > 0)
    {
      if (flag_weak_check_copy)
        {
          stubs_chkr_check_addr (dest, n, CHKR_TW, "dest");
          stubs_chkr_copy_bitmap (dest, (void *)src, n, "src->dest");
        }
      else
        {
          stubs_chkr_check_addr (src, n, CHKR_RO, "src");
          stubs_chkr_check_addr (dest, n, CHKR_WO, "dest");
        }
    }
  return memcpy (dest, src, n);
}
#endif /* HAVE_memcpy */

#ifdef HAVE_memmove
void *
chkr$memmove (void * dest, const void * src, size_t n)
{
  if (n > 0)
    {
      if (flag_weak_check_copy)
        {
          stubs_chkr_check_addr (dest, n, CHKR_TW, "dest");
          stubs_chkr_copy_bitmap (dest, (void *)src, n, "src->dest");
        }
      else
        {
          stubs_chkr_check_addr (src, n, CHKR_RO, "src");
          stubs_chkr_check_addr (dest, n, CHKR_WO, "dest");
        }
    }
#if USE_BI_JUMP
  __builtin_jump (memmove);
#else
  return memmove (dest, src, n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_memmove */

#ifdef HAVE_memccpy
void *
chkr$memccpy (void * arg0, const void * arg1, int arg2, size_t arg3)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (void), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (void), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (memccpy);
#else
  {
    void * res;
    res = memccpy (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_memccpy */

#ifdef HAVE_memset
void *
chkr$memset (void *buf, int c, size_t len)
{
  if (len > 0)
    stubs_chkr_check_addr (buf, len, CHKR_WO, "buf");
#if USE_BI_JUMP
  __builtin_jump (memset);
#else
  return memset (buf, c, len);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_memset */

#ifdef HAVE_memchr
void *
chkr$memchr (const void *buf, int c, size_t len)
{
  stubs_chkr_check_addr (buf, len, CHKR_RO, "buf");
#if USE_BI_JUMP
  __builtin_jump (memchr);
#else
  return memchr (buf, c, len);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_memchr */

#ifdef HAVE_strcpy
char *
chkr$strcpy (char *dest, const char *src)
{
  size_t len;
  len = strlen (src) + 1;
  stubs_chkr_check_addr (src, len, CHKR_RO, "src");
  stubs_chkr_check_addr (dest, len, CHKR_WO, "dest");
  return strcpy (dest, src);
}
#endif /* HAVE_strcpy */

#ifdef HAVE_strncpy
char *
chkr$strncpy (char *dest, const char *src, size_t n)
{
  size_t len = strnlen (src, n);
  if (len > 0)
    {
      stubs_chkr_check_addr (src, len, CHKR_RO, "src");
      stubs_chkr_check_addr (dest, len, CHKR_WO, "dest");
    }
  if (n != len)
    stubs_chkr_check_addr (dest + len, n - len, CHKR_MW, "dest");
  return strncpy (dest, src, n);
}
#endif /* HAVE_strncpy */

#ifdef HAVE_strcat
char *
chkr$strcat (char *dest, const char *src)
{
  size_t len_dest = strlen (dest);
  size_t len_src = strlen (src) + 1;
  if (len_src > 0)
    stubs_chkr_check_addr (src, len_src, CHKR_RO, "src");
  if (len_dest > 0)
    stubs_chkr_check_addr (dest, len_dest, CHKR_RO, "dest");
  if (len_src > 0)
    stubs_chkr_check_addr (dest + len_dest, len_src, CHKR_WO, "dest");
  return strcat (dest, src);
}

#endif /* HAVE_strcat */

#ifdef HAVE_strncat
char *
chkr$strncat (char *dest, const char *src, size_t n)
{
  int len_dest, len_src;
  char *res;
  
  len_dest = strlen (dest);
  stubs_chkr_check_addr (dest, len_dest + 1, CHKR_RO, "dest");
  len_src = strnlen (src, n);
  stubs_chkr_check_addr (src, len_src, CHKR_RO, "src");
  res = strncat (dest, src, n);
  if (res)
    stubs_chkr_set_right (dest + len_dest, len_src + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_strncat */

#ifdef HAVE_strcmp
int
chkr$strcmp (const char * s1, const char * s2)
{
  int len;
  const char *p1 = s1;
  const char *p2 = s2;
  
  /* Compute the length.  1 is for the 0. */
  for (len = 1; *p1 && *p2; p1++, p2++)
    len++;
  
  stubs_chkr_check_addr (s1, len, CHKR_RO, "s1");
  stubs_chkr_check_addr (s2, len, CHKR_RO, "s2");
#if USE_BI_JUMP
  __builtin_jump (strcmp);
#else
  return strcmp (s1, s2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strcmp */

#ifdef HAVE_strncmp
int
chkr$strncmp (const char *s1, const char *s2, size_t n)
{
  size_t len1 = strnlen (s1, n);
  size_t len2 = strnlen (s2, n);
  if (len1 > 0)
    stubs_chkr_check_addr (s1, len1, CHKR_RO, "s1");
  if (len2 > 0)
    stubs_chkr_check_addr (s2, len2, CHKR_RO, "s2");
  return strncmp (s1, s2, n);
}
#endif /* HAVE_strncmp */

#ifdef HAVE_strcoll
int
chkr$strcoll (const char *s1, const char *s2)
{
  stubs_chkr_check_str (s1, CHKR_RO, "s1");
  stubs_chkr_check_str (s2, CHKR_RO, "s2");
#if USE_BI_JUMP
  __builtin_jump (strcoll);
#else
  return strcoll (s1, s2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strcoll */

#ifdef HAVE_strxfrm
size_t
chkr$strxfrm (char * arg0, const char * arg1, size_t arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (strxfrm);
#else
  {
    size_t res;
    res = strxfrm (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strxfrm */

#ifdef HAVE_strdup
char *
chkr$strdup (const char *s)
{
  char *res;
  
  stubs_chkr_check_str (s, CHKR_RO, "s");
  res = strdup (s);
  stubs_chkr_set_right (res, strlen (s) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_strdup */

#ifdef HAVE_strchr
char *
chkr$strchr (const char *s, int c)
{
  stubs_chkr_check_str (s, CHKR_RO, "s");
#if USE_BI_JUMP
  __builtin_jump (strchr);
#else
  return strchr (s, c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strchr */

#ifdef HAVE_strrchr
char *
chkr$strrchr (const char *s, int c)
{
  stubs_chkr_check_str (s, CHKR_RO, "s");
#if USE_BI_JUMP
  __builtin_jump (strrchr);
#else
  return strrchr (s, c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strrchr */

#ifdef HAVE_strcspn
size_t
chkr$strcspn (const char *str1, const char *str2)
{
  stubs_chkr_check_str (str1, CHKR_RO, "str1");
  stubs_chkr_check_str (str2, CHKR_RO, "str2");
#if USE_BI_JUMP
  __builtin_jump (strcspn);
#else
  return strcspn (str1, str2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strcspn */

#ifdef HAVE_strspn
size_t
chkr$strspn (const char *str1, const char *str2)
{
  stubs_chkr_check_str (str1, CHKR_RO, "str1");
  stubs_chkr_check_str (str2, CHKR_RO, "str2");
#if USE_BI_JUMP
  __builtin_jump (strspn);
#else
  return strspn (str1, str2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strspn */

#ifdef HAVE_strpbrk
char *
chkr$strpbrk (const char *str1, const char *str2)
{
  stubs_chkr_check_str (str1, CHKR_RO, "str1");
  stubs_chkr_check_str (str2, CHKR_RO, "str2");
#if USE_BI_JUMP
  __builtin_jump (strpbrk);
#else
  return strpbrk (str1, str2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strpbrk */

#ifdef HAVE_strstr
char *
chkr$strstr (const char *str, const char *sub)
{
  stubs_chkr_check_str (str, CHKR_RO, "str");
  stubs_chkr_check_str (sub, CHKR_RO, "sub");
#if USE_BI_JUMP
  __builtin_jump (strstr);
#else
  return strstr (str, sub);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strstr */

#ifdef HAVE_strtok
char *
chkr$strtok (char *str, const char *delim)
{
  if (str)
    stubs_chkr_check_str (str, CHKR_RW, "str");
  stubs_chkr_check_str (delim, CHKR_RO, "delim");
#if USE_BI_JUMP
  __builtin_jump (strtok);
#else
  return strtok (str, delim);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strtok */

#ifdef HAVE_memmem
void *
chkr$memmem (const void * arg0, size_t arg1, const void * arg2, size_t arg3)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (void), CHKR_XX);
  stubs_chkr_check_addr (arg2, sizeof (void), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (memmem);
#else
  {
    void * res;
    res = memmem (arg0, arg1, arg2, arg3);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_memmem */

#ifdef HAVE_strlen
size_t
chkr$strlen (const char *s)
{
  /* FIXME */
  stubs_chkr_check_str (s, CHKR_RO, "s");
  return strlen (s);
}
#endif /* HAVE_strlen */

#ifdef HAVE_strerror
char *
chkr$strerror (int err)
{
  char *res;
  res = strerror (err);
  if (res)
    stubs_chkr_set_right (res, strlen (res) + 1, CHKR_RW);
  return res;
}
#endif /* HAVE_strerror */

#ifdef HAVE_index
char *
chkr$index (const char *s, int c)
{
  stubs_chkr_check_str (s, CHKR_RO, "s");
#if USE_BI_JUMP
  __builtin_jump (index);
#else
  return index (s, c);
#endif /* USE_BI_JUMP */
}
#endif /* HAVE_index */

#ifdef HAVE_rindex
char *
chkr$rindex (const char *s, int c)
{
  stubs_chkr_check_str (s, CHKR_RO, "s");
#if USE_BI_JUMP
  __builtin_jump (rindex);
#else
  return rindex (s, c);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_rindex */

#ifdef HAVE_bcopy
void
chkr$bcopy (const void *src, void *dest, int n)
{
  if (n > 0)
    {
      if (flag_weak_check_copy)
        {
          stubs_chkr_check_addr (dest, n, CHKR_TW, "dest");
          stubs_chkr_copy_bitmap (dest, (void *)src, n, "src->dest");
        }
      else
        {
          stubs_chkr_check_addr (src, n, CHKR_RO, "src");
          stubs_chkr_check_addr (dest, n, CHKR_WO, "dest");
        }
    }
#if USE_BI_JUMP
  __builtin_jump (bcopy);
#else
  bcopy (src, dest, n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_bcopy */

#ifdef HAVE_bzero
void
chkr$bzero (void *buf, int len)
{
  if (len > 0)
    stubs_chkr_check_addr (buf, len, CHKR_WO, "buf");
#if USE_BI_JUMP
  __builtin_jump (bzero);
#else
  bzero (buf, len);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_bzero */

#ifdef HAVE_bcmp
int
chkr$bcmp (const void *s1, const void *s2, int n)
{
  if (n > 0)
    {
      stubs_chkr_check_addr (s1, n, CHKR_RO, "s1");
      stubs_chkr_check_addr (s2, n, CHKR_RO, "s2");
    }
#if USE_BI_JUMP
  __builtin_jump (bcmp);
#else
  return bcmp (s1, s2, n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_bcmp */

#ifdef HAVE_ffs
int
chkr$ffs (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (ffs);
#else
  {
    int res;
    res = ffs (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ffs */

#ifdef HAVE_strcasecmp
int
chkr$strcasecmp (const char *s1, const char *s2)
{
  stubs_chkr_check_str (s1, CHKR_RO, "s1");
  stubs_chkr_check_str (s2, CHKR_RO, "s2");
#if USE_BI_JUMP
  __builtin_jump (strcmp);
#else
  return strcasecmp (s1, s2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strcasecmp */

#ifdef HAVE_strsep
char *
chkr$strsep (char ** arg0, const char * arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char *), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (strsep);
#else
  {
    char * res;
    res = strsep (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strsep */

#ifdef HAVE_strncasecmp
int
chkr$strncasecmp (const char *s1, const char *s2, size_t n)
{
  size_t len1 = strnlen (s1, n);
  size_t len2 = strnlen (s2, n);
  if (len1 > 0)
    stubs_chkr_check_addr (s1, len1, CHKR_RO, "s1");
  if (len2 > 0)
    stubs_chkr_check_addr (s2, len2, CHKR_RO, "s2");
  return strncasecmp (s1, s2, n);
}
#endif /* HAVE_strncasecmp */

#ifdef HAVE_strsignal
char *
chkr$strsignal (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (strsignal);
#else
  {
    char * res;
    res = strsignal (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strsignal */

#ifdef HAVE_stpcpy
char *
chkr$stpcpy (char *dest, const char *src)
{
  size_t len;
  len = strlen (src) + 1;
  stubs_chkr_check_addr (src, len, CHKR_RO, "src");
  stubs_chkr_check_addr (dest, len, CHKR_WO, "dest");
  return stpcpy (dest, src);
}
#endif /* HAVE_stpcpy */

#ifdef HAVE_stpncpy
char *
chkr$stpncpy (char * arg0, const char * arg1, size_t arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (stpncpy);
#else
  {
    char * res;
    res = stpncpy (arg0, arg1, arg2);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_stpncpy */

#ifdef HAVE_strfry
char *
chkr$strfry (char * arg0)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (char), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (strfry);
#else
  {
    char * res;
    res = strfry (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_strfry */

#ifdef HAVE_memfrob
void *
chkr$memfrob (void * arg0, size_t arg1)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (void), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (memfrob);
#else
  {
    void * res;
    res = memfrob (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_memfrob */

#ifdef HAVE_swab
void
chkr$swab (const void * arg0, void * arg1, size_t arg2)
{
  /* This function require a stub */
  stubs_chkr_check_addr (arg0, sizeof (void), CHKR_XX);
  stubs_chkr_check_addr (arg1, sizeof (void), CHKR_XX);
#if USE_BI_JUMP
  __builtin_jump (swab);
#else
  swab (arg0, arg1, arg2);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_swab */

#ifdef HAVE_memcmp
int
chkr$memcmp (const PTR s1, const PTR s2, size_t n)
{
  stubs_chkr_check_addr (s1, n, CHKR_RO, "s1");
  stubs_chkr_check_addr (s2, n, CHKR_RO, "s2");
#if USE_BI_JUMP
  __builtin_jump (strcmp);
#else
  return memcmp (s1, s2, n);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_memcmp */

#endif /* HAVE_STRING_H */
