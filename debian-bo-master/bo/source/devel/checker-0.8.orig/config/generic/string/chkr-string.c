#include <stddef.h>
#include "chkr-string.h"

void
bcopy (const char *src, char *dest, int len)
{
  if (dest < src)
    while (len--)
      *dest++ = *src++;
  else
    {
      char *lasts = (char *)src + (len-1);
      char *lastd = dest + (len-1);
      while (len--)
        *(char *)lastd-- = *(char *)lasts--;
    }
}

void *
memcpy (void *o, const void *i, size_t len)
{
  bcopy (i, o, len);
  return o;
}

void *
memmove (void *s1, const void *s2, size_t n)
{
  bcopy (s2, s1, n);
  return s1;
}

void *
memchr (const void *src_void, int c, size_t length)
{
  const unsigned char *src = (const unsigned char *)src_void;
  
  if (length == 0)
    return NULL;
    
  while (--length >= 0)
  {
    if (*src == c)
     return (void *)src;
    src++;
  }
  return NULL;
}

int
memcmp (const void *str1, const void *str2, size_t count)
{
  register unsigned char *s1 = (unsigned char*)str1;
  register unsigned char *s2 = (unsigned char*)str2;

  while (count-- > 0)
    {
      if (*s1++ != *s2++)
	  return s1[-1] < s2[-1] ? -1 : 1;
    }
  return 0;
}

void *
memset (void * dest, int val, size_t len)
{
  register unsigned char *ptr = (unsigned char*)dest;
  while (len-- > 0)
    *ptr++ = val;
  return dest;
}

char *
strcat (char *dest, const char *src)
{
  register char *s1 = dest;
  register const char *s2 = src;
  char c;

  /* Find the end of the string.  */
  do
    c = *s1++;
  while (c != '\0');

  /* Make S1 point before the next character, so we can increment
     it while memory is read (wins on pipelined cpus).  */
  s1 -= 2;

  do
    {
      c = *s2++;
      *++s1 = c;
    }
  while (c != '\0');

  return dest;
}

int
strcmp (const char *p1, const char *p2)
{
  register const unsigned char *s1 = (const unsigned char *) p1;
  register const unsigned char *s2 = (const unsigned char *) p2;
  unsigned char c1, c2;

  do
    {
      c1 = (unsigned char) *s1++;
      c2 = (unsigned char) *s2++;
      if (c1 == '\0')
	return c1 - c2;
    }
  while (c1 == c2);

  return c1 - c2;
}

/* Copy SRC to DEST.  */
char *
strcpy (char *dest, const char *src)
{
  char c;
  char *s = (char *) src;
  const ptrdiff_t off = dest - src - 1;

  do
    {
      c = *s++;
      s[off] = c;
    }
  while (c != '\0');

  return dest;
}

char *
strncpy (char *dest, const char *src, size_t n)
{
  char c;
  char *s = (char *) src;
  const ptrdiff_t off = dest - src - 1;
  
  if (n == 0)
    return dest;
    
  do
    {
      c = *s++;
      s[off] = c;
      if (--n == 0)
        break;
    }
  while (c != '\0');

  return dest;
}

char *
strchr (const char *s, int i)
{
  char c = (char) i;
  
  while (*s)
    if (*s == c)
      return (char *)s;
    else
      s++;
  return (char *)0;
}

int
strncmp (const char *s1, const char *s2, size_t n)
{
  unsigned char c1;
  unsigned char c2;
  
  if (n == 0)
    return 0;
    
  for (; n > 0; n--)
    {
      c1 = (unsigned char) *s1++;
      c2 = (unsigned char) *s2++;
      if (c1 == 0 || c1 != c2)
        return c1 - c2;
    }
    
  return c1 - c2;
}

size_t
strlen (const char *s)
{
  size_t len = 0;
  
  while (*s++)
    len++;
  return len;
}
