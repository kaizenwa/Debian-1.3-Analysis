/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Wei Xu, Trusted Information Systems, Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/param.h>
#include "list.h"

#include <X11/Intrinsic.h>

#ifndef LLEV
#define LLEV    LOG_NOTICE
#endif
#ifndef LFAC
#define LFAC    LOG_DAEMON
#endif

#ifndef BUFSIZE
#define BUFSIZE 1024
#endif

#ifndef TRUE
#define TRUE   1
#endif
#ifndef FALSE
#define FALSE  0
#endif

#ifndef OK
#define OK     5
#endif
#ifndef CANCEL
#define CANCEL 0
#endif  

extern  int     uselog;
extern	int	errno;
extern	char   *sys_errlist[];


#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#ifdef  SYSV
#define getdtablesize() 64
#endif  SYSV


typedef struct	sws_t {
	char	*key;
	char	*arg;
} sws_t;

typedef struct	sws_l {
	int	cnt;
	sws_t	*sws;
} sws_l;


