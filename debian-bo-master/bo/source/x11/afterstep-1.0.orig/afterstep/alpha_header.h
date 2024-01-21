/**************************************************************************/
/* If I do ALL this, I can compile OK with -Wall -Wstrict-prototypes on the
 * alpha's */
#include <sys/types.h>
#include <sys/time.h>


extern int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);

/* string manipulation */
#ifdef __GNUC__
extern size_t strlen(char *);
#endif

/* Commented out 08/22/96 -- Thanks to Pierre Wendling 
extern int bzero(char *, int);
extern int gethostname (char *, int);
*/
/**************************************************************************/

