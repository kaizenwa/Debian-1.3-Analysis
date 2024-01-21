#include <errno.h>

#pragma weak errno = _errno

#undef errno
extern int errno;
int _errno = 0;

#pragma weak __errno_location = __normal_errno_location

/* The one in libpthread will override __errno_location () */
int *__normal_errno_location ( void );

int *
__normal_errno_location ( void )
{
  return &errno;
}
