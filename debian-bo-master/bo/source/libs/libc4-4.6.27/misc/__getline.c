/* Copyright (C) 1993  Hongjiu Lu
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

#ifdef __linux__
#include <ansidecl.h>
#include <stdlib.h>
#include <gnu/types.h>

#if !defined(ssize_t) && !defined(_SSIZE_T)
#define _SSIZE_T
#define ssize_t __ssize_t
#endif
  
#endif
#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifndef MAX_CANON
#define MAX_CANON	256
#endif

/* Read up to (and including) a newline from STREAM into
 * *LINEPTR (and null-terminate it). *LINEPTR is a pointer
 * returned from malloc (or NULL), pointing to *N characters
 * of space.  It is realloc'd as necessary.  Returns the
 * number of characters read (not including the null
 * terminator), or -1 on error or EOF.
 */
#ifdef __linux__
ssize_t
DEFUN(__getline, (lineptr, n, stream),
      char **lineptr AND size_t *n AND FILE *stream)
#else       
__getline (char **lineptr, int *n, FILE *stream)
#endif
{
  char *ptr;	/* the latest section of the buffer */
  char *line;	/* Pointer to the buffer */
  int size;	/* Size of the buffer */
  int count;	/* # of chars in the buffer */

  if (lineptr == NULL || n == NULL)
  {
    errno = EINVAL;
    return -1;
  }

  if (ferror (stream))
    return -1;

  /* No buffer or too small, we allocate one. */
  if (*lineptr == NULL || *n < 2)
  {
    line = (*lineptr == NULL) ?
	malloc (MAX_CANON) : realloc (*lineptr, MAX_CANON);
    if (line == NULL) return -1;
    *lineptr = line;
    *n = MAX_CANON;
  }
  else
  {
    /* Assume the buffer size is `*n'. */
    line = *lineptr;
  }

  size = *n;
  count = 0;
  ptr = line;

  for (;;)
  {

    /* We call fgets () first. For the first time, our
     * buffer is *n. Next time, it is MAX_CANON chars
     * longer. Count in 1 char for the last '\0'. Our
     * additional buffer is MAX_CANON + 1 char long.
     */
    if (fgets (ptr, (count > 0 ? MAX_CANON + 1: size), stream)
	== NULL)
    {
      /* Check if we read in anything. */
      return (count) ? count : -1;
    }

    /* How many chars we have read in so far. */
    count += strlen (ptr);

    /* If the buffer is full and we still haven't seen the
     * newline, we need to expand the buffer and call
     * fgets () again. Caution: there may be no newline
     * at EOF. We will catch it when we call fgets () next
     * time.
     */
    if ((count >= size - 1) && line [count - 1] != '\n')
    {
      /* Expand the buffer by MAX_CANON chars. */
      size += MAX_CANON;
      line = realloc (line, size); 
      if (line == NULL) return -1;
      *lineptr = line;
      *n = size;

      /* ptr points to the newly expanded buffer. We
       * start at the previous '\0'.
       */
      ptr = &line [count];
    }
    else break;
  }

  return count;
}

#if 0
void
main ()
{
  char *buffer = NULL;
  int len, ret;

  while ((ret = __getline (&buffer, &len, stdin)) != -1)
    printf ("size: %d, len: %d: %s", len, ret, buffer);
}
#endif
