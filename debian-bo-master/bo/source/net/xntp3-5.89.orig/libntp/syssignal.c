#include <stdio.h>
#include <sys/types.h>
#include <signal.h>

#include "ntp_stdlib.h"

#ifdef HAVE_SIGACTION
#include <errno.h>

extern int errno;

void
signal_no_reset(sig, func)
     int sig;
     void (*func) P((int));
{
    int n;
    struct sigaction vec;

    vec.sa_handler = func;
    sigemptyset(&vec.sa_mask);
#ifdef SA_RESTART
    vec.sa_flags = SA_RESTART;
#else
    vec.sa_flags = 0;
#endif

    while (1) {
        n = sigaction(sig, &vec, NULL);
	if (n == -1 && errno == EINTR) continue;
	break;
    }
    if (n == -1) {
	perror("sigaction");
        exit(1);
    }
}

#else  /* not HAVE_SIGACTION */

RETSIGTYPE
signal_no_reset(sig, func)
int sig;
RETSIGTYPE (*func) P((int));
{
    signal(sig, func);
}
#endif /* not HAVE_SIGACTION */

