#include <netdb.h>

#pragma weak h_errno = _h_errno

#undef h_errno;
extern int h_errno;
int _h_errno = 0;

#pragma weak __h_errno_location = __normal_h_errno_location

/* The one in libpthread will override __h_errno_location () */
int *__normal_h_errno_location ( void );

int *
__normal_h_errno_location ( void )
{
  return &h_errno;
}
