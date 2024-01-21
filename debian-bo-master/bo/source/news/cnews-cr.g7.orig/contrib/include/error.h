#include <errno.h>
#include <netdb.h>

extern int errno;
extern int h_errno;

extern void warning(/* char *fmt, char *errstring */);
extern void error(/* char *fmt, char *errstring */);
extern int error_status(/* int saverrno, int savherrno */);
extern char *itos(/* char *fmt, int i */);

#define ERR_PERM	1
#define ERR_RETRY	2
