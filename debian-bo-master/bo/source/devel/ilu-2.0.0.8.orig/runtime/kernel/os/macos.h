/*
 * macos.h -- MacOS specific macro definitions for library and system calls
 *
 * Antony A. Courtney, 17/7/93
 */

#ifndef MAC_OS_H
#define MAC_OS_H 1

#include "support.h"
#define OS_BCOPY		ilu_sup_bcopy
#define OS_GETPID		ilu_sup_getpid
#define OS_SIGVEC		SIGVEC_NOT_SUPPORTED

#ifndef ANSI_STRERROR
#define ANSI_STRERROR(a)	strerror(a)
#endif

