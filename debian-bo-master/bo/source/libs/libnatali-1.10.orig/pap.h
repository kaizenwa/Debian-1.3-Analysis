/*
** natali/pap.h
** Copyright 1995, 1996 Trinity College Computing Center.
** Written by David Chappell.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software and documentation are provided "as is" without
** express or implied warranty.
**
** Last revised 16 February 1996.
*/

#ifndef _NATALI_NBP
#ifdef _NATALI
#include "nbp.h"
#else
#include <at/nbp.h>
#endif
#endif

#ifndef _NATALI_PAP
#define _NATALI_PAP 1

/* pap_errno and its possible values */
extern int pap_errno;
#define PAPBADPARM 1
#define PAPSYSERR 2
#define PAPTOOMANY 3
#define PAPBUSY 4
#define PAPTIMEOUT 5
#define PAPHANGUP 6
#define PAPSIZERR 7
#define PAPBLOCKED 8
#define PAPDATARECD 9

/* possible pap_look() events */
#define PAP_CONNECT_RECVD 1
#define PAP_DATA_RECVD 2
#define PAP_WRITE_ENABLED 3
#define PAP_DISCONNECT 4

/* modes for pap_write() */
#define PAP_WAIT 1
#define PAP_NOWAIT 2

#ifdef __cplusplus
extern "C" {
#endif

/* Function prototypes. */
int pap_close(int fd);
int pap_abrupt_close(int fd);
int pap_open(at_nbptuple_t *tuple, u_short *quantum, unsigned char *status, short retry);
int pap_look(int fd);
int pap_read(int fd, char *data, int len, u_char *eof_flag);
int pap_write(int fd, const char *data, int len, u_char eof_flag, u_char mode);
int pap_sync(int fd);
int paps_status(int fd, char *status);
int paps_open(u_short quantum);
int paps_get_next_job(int fd, u_short *quantum, at_inet_t *src);

#ifdef __cplusplus
} ;
#endif

#endif

/* end of file */
