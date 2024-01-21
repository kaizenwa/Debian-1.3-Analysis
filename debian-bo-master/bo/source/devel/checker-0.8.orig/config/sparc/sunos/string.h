#ifndef _STRING_H_
#define _STRING_H_
#include <stddef.h>

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
