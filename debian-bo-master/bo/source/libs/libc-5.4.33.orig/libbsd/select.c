/*   
 * select.c
 *   
 * Fix for programs which use a broken version of select()
 *   
 */  
     
#include <sys/time.h>
     
int select(int width, fd_set *readfds,
	fd_set *writefds, fd_set *exceptfds,
	struct timeval *timeout)
{    
  struct timeval local_timeout;
     
  if ( timeout )
  {
    local_timeout = *timeout;
    timeout = &local_timeout;
  }

  return __select(width,readfds,writefds,exceptfds,timeout);
}
