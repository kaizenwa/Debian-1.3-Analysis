/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/uio.h>

#pragma weak readv = __readv

/* Read data from file descriptor FD, and put the result in the
   buffers described by VECTOR, which is a vector of COUNT `struct iovec's.
   The buffers are filled in the order specified.
   Operates just like `read' (see <unistd.h>) except that data are
   put in VECTOR instead of a contiguous buffer.  */

int __readv( int, const struct iovec *, size_t);

int
DEFUN(__readv, (fd, vector, count),
      int fd AND CONST struct iovec *vector AND size_t count)
{
  char *buffer;
  size_t bytes;
  int bytes_read;
  register size_t i;

  /* We try the system call first. */
#define        min(a, b)       ((a) > (b) ? (b) : (a))
#ifdef MAX_IOVEC
  for (i = 0, bytes = 0; i < count; i += MAX_IOVEC, bytes += bytes_read) {
    bytes_read = __syscall_readv (fd, vector+i, min(count-i,MAX_IOVEC));
    if (bytes_read < 0) {
      if (errno != ENOSYS) return bytes_read;
      else goto nonsyscall; /* probably shouldn't happen */
    }
  }
  return bytes;
  nonsyscall:
#else /* this probably means syscall doesn't exist, but just in case... */
  bytes_read = __syscall_readv (fd, vector, count);
  if (bytes_read >= 0 || (bytes_read < 0 && errno != ENOSYS))
  {
    return bytes_read;
  }
#endif
  
  errno = 0;

  /* Find the total number of bytes to be read.  */
  bytes = 0;
  for (i = 0; i < count; ++i)
    bytes += vector[i].iov_len;

  /* Allocate a temporary buffer to hold the data.  */
  buffer = (char *) __alloca(bytes);

  /* Read the data.  */
  bytes_read = read(fd, buffer, bytes);
  if (bytes_read < 0)
    return -1;

  if (bytes_read == 0)
    return 0;

  /* Copy the data from BUFFER into the memory specified by VECTOR.  */
  bytes = bytes_read;
  for (i = 0; i < count; ++i)
    {
      size_t copy = min(vector[i].iov_len, bytes);

      (void) memcpy((PTR) vector[i].iov_base, (PTR) buffer, copy);

      buffer += copy;
      bytes -= copy;
      if (bytes == 0)
	break;
    }

  return bytes_read;
}
