#define _POSIX_SOURCE 1
#include <sys/types.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <sys/syslog.h>
#include <netdb.h>
#include <pwd.h>
#include <sys/stat.h>

#include <sys/termios.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <rpcsvc/ypclnt.h>
#include <linux/if_ether.h>

#include "pathnames.h"

#ifndef N_SLIP
#define N_SLIP SLIPDISC /* slip line discipline */
#endif


struct slipinfo
{
 int linespeed;
 int timeout;
 char sm_name[16];
 int sm_value;
 char *loginname;
 char loginfile[MAXPATHLEN];
 int uid;
 char raddr[16];
 char laddr[16];
 char unit[32];
 char mask[16];
 char option[3][16];
};

