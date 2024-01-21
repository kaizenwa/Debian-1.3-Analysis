#ifndef _STRING_H_
#define _STRING_H_
#include <stddef.h>

#define bcopy(s,d,l) chkr_bcopy (s,d,l)
#define memcpy(o,i,l) chkr_memcpy (o,i,l)
#define memmove(s,d,n) chkr_memmove (s,d,n)
#define memchr(s,c,l) chkr_memchr (s,c,l)
#define memcmp(s,d,c) chkr_memcmp (s,d,c)
#define memset(d,v,l) chkr_memset (d,v,l)
#define strcat(d,s) chkr_strcat (d,s)
#define strcmp(a,b) chkr_strcmp (a,b)
#define strcpy(d,s) chkr_strcpy (d,s)
#define strncpy(d,s,n) chkr_strncpy (d,s,n)
#define strchr(s,i) chkr_strchr (s,i)
#define strncmp(a,b,n) chkr_strncmp (a,b,n)
#define strlen(s) chkr_strlen(s)

void bcopy (const char *src, char *dest, int len);
void *memcpy (void *o, const void *i, size_t len);
void *memmove (void *s1, const void *s2, size_t n);
void *memchr (const void *src_void, int c, size_t length);
int memcmp (const void *str1, const void *str2, size_t count);
void *memset (void * dest, int val, size_t len);
char *strcat (char *dest, const char *src);
int strcmp (const char *p1, const char *p2);
char *strcpy (char *dest, const char *src);
char *strncpy (char *dest, const char *src, size_t n);
char *strchr (const char *s, int i);
int strncmp (const char *s1, const char *s2, size_t n);
size_t strlen (const char *s);

#endif
