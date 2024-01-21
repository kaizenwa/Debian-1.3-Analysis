/*
** natali/appletalk.h
** Copyright 1995, 1996, Trinity College Computing Center.
** Written by David Chappell.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software and documentation are provided "as is" without
** express or implied warranty.
**
** This is an attempt to implement the AppleTalk Library Interface
** for Linux and Netatalk.
**
** This file was last modified 15 February 1996.
*/

#ifndef _NATALI_APPLETALK
#define _NATALI_APPLETALK 1

#ifdef __cplusplus
extern "C" {
#endif

/* define at_inet_t */
typedef u_short at_net;
typedef u_short at_node;
typedef u_short at_socket;
typedef struct at_inet_t
    {
    at_net net;
    at_node node;
    at_socket socket;
    } at_inet_t;

/* define at_retry_t */
typedef struct at_retry_t
    {
    short interval;
    short retries;
    } at_retry_t;

#ifdef __cplusplus
} ;
#endif

#endif

/* end of file */
