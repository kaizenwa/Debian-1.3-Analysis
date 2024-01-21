/*                              */
/* xemeraldia   -----  usleep.c */
/*                              */
 
#ifdef SYSV
#include <time.h>
#ifdef sgi
#include <sys/time.h>
#endif

void usleep (useconds)
     unsigned useconds;
{
    struct timeval timeout;

    timeout.tv_sec = 0;
    timeout.tv_usec = useconds;

    select (0, 0, 0, 0, &timeout);
}
#endif
