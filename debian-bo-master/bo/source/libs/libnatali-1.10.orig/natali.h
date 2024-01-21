/*
** natali/natali.h
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
** An ALI compatible library for Netatalk.
**
** This file contains internal definitions for the NATALI library.
** No caller of this library should use this include file.
**
** Last revised 1 February 1996.
*/

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <netatalk/endian.h>
#include <netatalk/at.h>
#include <netinet/in.h>
#include <atalk/atp.h>
#include <atalk/nbp.h>

/* This define is used to determine if we are actually
   compiling the library.  All library modules include
   this file.  No use of the library should include
   this file.
   */
#define _NATALI 1

/* If this is true, library functions will write to the file
   /tmp/natali.debug.
   */
/* #define DEBUG 1 */

#ifdef DEBUG
#define DODEBUG(a) natali_debug a
#else
#define DODEBUG(a)
#endif

/* Machine independent defines.
   */
#define BYTE unsigned char

/* True and False.
   */
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE !FALSE
#endif

/* The maximum number of file descriptors we expect the system
   to allow to one process.
   This should be the same limit as your C library recognizes.
   */
#define NATALI_MAXFDS 100

/* The maximum quantum.
   This is not a tunable.
   */
#define PAP_MAXQUANTUM 8

/* Interval between tickle packets and
   how often we must receive them if we 
   are not to conclude that the connexion 
   is dead.
   */
#define PAP_TICKLE_INTERVAL 60
#define PAP_TIMEOUT 120

/* Structure which describes a file descriptor which 
   is a PAP endpoint.
   */
#define SIGNITURE_PAP 1001	/* ID code */
struct PAP
	{
	int signiture;			/* structure type signiture */	
	ATP atp;			/* pointer to ATP endpoint structure */
	ATP newatp;			/* new server connexion */
	struct atpbuf *tickle_packet;	/* the tickle packet, preserved for re-sending */
	BYTE connid;			/* Connection id */
	BYTE nearquantum;		/* this end flow quantum */
	BYTE farquantum;		/* other end flow quantum */
	struct sockaddr_at sat;		/* destination socket address */
	int sent_sendcredit;		/* TRUE if we have a sent a send credit */
	int received_sendcredit;	/* TRUE if we have received a send credit */
	int write_call_blocked;		/* TRUE if last pap_write() returned pap_errno=PAPBLOCKED */
	BYTE port;			/* port of last SendData request */
	time_t last_send_time;		/* time last tickle sent */
	time_t last_recv_time;		/* time of last packet reception */
	int closed;			/* connection closed by other end */
	int my_seq;			/* last PAP sequence number I used */
	int his_seq;			/* last sequence number from other end */
	int data_received;		/* data has been received */
	int server;			/* is this a server endpoint? */
	BYTE status[256];		/* server status */
	int connexion_request;		/* connexion request received */
	} ;

/* The PAP commands. */
#define PAP_OpenConn 1
#define PAP_OpenConnReply 2
#define PAP_SendData 3
#define PAP_Data 4
#define PAP_Tickle 5
#define PAP_CloseConn 6
#define PAP_CloseConnReply 7
#define PAP_SendStatus 8
#define PAP_Status 9

/* Prototypes for internal functions.
   */
void natali_debug(const char string[], ...);
void natali_init(void);
struct PAP *natali_fd_to_pap(int fd);
int natali_pap_eventhandler(struct PAP *pap);
int natali_pap_sendcredit(struct PAP *pap);
int natali_pap_start_tickling(struct PAP *pap);

/* Definitions of internal global variables.
   */
extern void *natali_fds[];   
extern int natali_started;

/* end of file */
