#ifndef PWDB_HEADERS_H
#define PWDB_HEADERS_H

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <features.h>
#include <ctype.h>
#ifndef __USE_BSD
#define __USE_BSD
#endif
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <utime.h>
#include <time.h>
#include <netinet/in.h>
#include <netdb.h>
#include <rpcsvc/ypclnt.h>
#include <rpc/rpc.h>

#ifdef MEMORY_DEBUG
#include "../libpam/include/security/pam_malloc.h"
#endif

#endif /* PWDB_HEADERS_H */
