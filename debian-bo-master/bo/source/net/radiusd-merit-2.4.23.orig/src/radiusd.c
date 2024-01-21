/* 
 * RADIUS -- Remote Authentication Dial In User Service
 * 
 * 
 * Livingston Enterprises, Inc. 6920 Koll Center Parkway Pleasanton, CA   94566
 * 
 * Copyright 1992 Livingston Enterprises, Inc.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose and without fee is hereby granted, provided that this copyright
 * and permission notice appear on all copies and supporting documentation,
 * the name of Livingston Enterprises, Inc. not be used in advertising or
 * publicity pertaining to distribution of the program without specific
 * prior permission, and notice be given in supporting documentation that
 * copying and distribution is by permission of Livingston Enterprises, Inc.
 * 
 * Livingston Enterprises, Inc. makes no representations about the suitability
 * of this software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 *
 * [C] The Regents of the University of Michigan and Merit Network, Inc. 1992,
 * 1993, 1994, 1995, 1996 All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice and this permission notice appear in all
 * copies of the software and derivative works or modified versions thereof,
 * and that both the copyright notice and this permission and disclaimer
 * notice appear in supporting documentation.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE REGENTS OF THE
 * UNIVERSITY OF MICHIGAN AND MERIT NETWORK, INC. DO NOT WARRANT THAT THE
 * FUNCTIONS CONTAINED IN THE SOFTWARE WILL MEET LICENSEE'S REQUIREMENTS OR
 * THAT OPERATION WILL BE UNINTERRUPTED OR ERROR FREE.  The Regents of the
 * University of Michigan and Merit Network, Inc. shall not be liable for any
 * special, indirect, incidental or consequential damages with respect to any
 * claim by Licensee or any third party arising from use of the software.
 *
 *
 * Public entry points in this file:
 *
 * build_acct_req
 * call_action
 * rad_2rad_recv
 * rad_recv
 * radius_send
 * start_fsm
 *
 */

/* N.B. don't look here for the version, run radiusd -v or look in version.c */

static char     sccsid[] =
		"@(#)radiusd.c 1.5 Copyright 1992 Livingston Enterprises Inc";

static char     rcsid[] =
		"$Id: radiusd.c,v 1.233 1996/06/14 19:43:29 web Exp $";

#include	<sys/types.h>
#include	<sys/stat.h>

#if !(defined(FD_SET) || defined(linux))
#include	<sys/select.h>
#endif	/* FD_SET */

#if defined(sys5)
#include	<sys/sysmacros.h>
#endif	/* sys5 */

#ifdef	SVR4
#include	<sys/systeminfo.h>
#endif	/* SVR4 */

#include	<sys/param.h>
#include	<sys/socket.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<sys/wait.h>
#include	<net/if.h>
#include	<netinet/in.h>

#include	<stdio.h>
#include	<stdlib.h>
#include	<netdb.h>
#include	<fcntl.h>
#include	<time.h>
#include	<errno.h>
#include	<signal.h>
#include	<syslog.h>

#include	"radius.h"

/*
 *	Static Declarations
 */

static u_char   acct_ident = 0;   /* sequence numbers on the accounting queue */
static u_char   auth_ident = 0;   /* sequence numbers on the authentication q */
static int      alarm_set = 0;    /* Flag indicates alarm is set */
static int      cache_users = 1;  /* Read users file to memory (default) */
static int      select_max = 0;   /* computed in main() */
static int      max_q_size = 0;   /* computed in is_dup_request() */
static int      cur_q_size = 0;   /* recorded in is_dup_request() */
static int      max_aq_size = 0;  /* computed in is_dup_request() */
static int      cur_aq_size = 0;  /* recorded in is_dup_request() */
static u_short  lport = 0;        /* UDP port number to receive requests on */
static u_short  llport = 0;       /* UDP port number to relay requests on */
static u_short  aaport = 0;       /* UDP port number to relay accounting on */
static char    *debug_file = RADIUS_DEBUG;
static int      zap_debugfile = 0; /* Empty debugfile first time referenced. */
static char    *radius_fsm = RADIUS_FSM;
static char    *cur_wrk_dir = (char *) NULL; /* Initially use our parent's. */
static AUTH_REQ *global_acct_q = (AUTH_REQ *) NULL; /* link acct. reqs. here */
static int      nfsm;             /* number of FSM entries */
static FSM_ENT **fsm;             /* pointer to FSM table */
static FSM_ENT **default_fsm;
static int      avalue;
static time_t   tofmaxdelay;
static time_t   select_cur = 0;   /* recorded in main() */
static char     astring[AUTH_STRING_LEN + 1];
static char     path[MAXPATHLEN];
static char     logversion[MAXPATHLEN];
static int      last_send_len = 0;    /* Length of last socket request sent */
static struct sockaddr_in last_send_sin; /* Socket info of last request sent */
static char     last_send_action[64]; /* Name of action sending last request */

/*
 *	Forward Declarations
 */

static int      ack_action PROTO((AUTH_REQ *, int, char *));
/* external     build_acct_req */
/* external     call_action */
static void     child_end PROTO((int));
static int      config_initialize PROTO((void));
static void     debug_bump PROTO((int));
static void     debug_off PROTO((int));
static int      fatal_log_action PROTO((AUTH_REQ *, int, char *));
static void     free_authreq PROTO((AUTH_REQ *));
static void     free_event PROTO((EVENT_ENT *));
static void     free_event_list PROTO((EVENT_ENT *));
static int      get_radrequest PROTO((AUTH_REQ **, struct sockaddr_in *, UINT4, u_int));
static u_char   get_state PROTO((VALUE_PAIR **));
static void     init_aatvs PROTO((void));
static AUTH_REQ *is_dup_request PROTO((AUTH_REQ *, int *));
static int      kill_action PROTO((AUTH_REQ *, int, char *));
static int      nak_action PROTO((AUTH_REQ *, int, char *));
static int      null_action PROTO((AUTH_REQ *, int, char *));
static int      pending_action PROTO((AUTH_REQ *, int, char *));
static int      protocol_check PROTO((AUTH_REQ *, VALUE_PAIR **));
static AUTH_REQ *proxy_forwarding PROTO((struct sockaddr_in *, UINT4, u_int));
/* external     rad_2rad_recv */
static void     rad_init PROTO((AATV *));
static void     rad_ipc_init PROTO((AATV *));
static AUTH_REQ *rad_ipc_recv PROTO((struct sockaddr_in *, UINT4, u_int, EV *));
/* external     rad_recv */
static int      rad_reply PROTO((AUTH_REQ *, int, char *));
static void     rad_reply_init PROTO((AATV *));
/* external     radius_send */
static void     reconfig PROTO((int));
static void     record_event PROTO((AUTH_REQ *, AATV *, int, int, char *));
static int      redo_action PROTO((AUTH_REQ *, int, char *));
static void     reply_timer PROTO((int));
static void     send_reply PROTO((char *, int, AUTH_REQ *, int));
static int      server_status PROTO((AUTH_REQ *, int, char *));
static void     set_debug PROTO ((int));
static void     sig_fatal PROTO ((int));
static void     sig_int PROTO ((int));
static void     sig_term PROTO ((int));
/* external     start_fsm */
static void     state_machine PROTO((EV, AUTH_REQ *));
static void     stat_files PROTO((int, int));
static int      timeout_action PROTO((AUTH_REQ *, int, char *));
static int      timer_action PROTO((AUTH_REQ *, int, char *));
static void     usage PROTO((void));
static int      xstring_clear_action PROTO((AUTH_REQ *, int, char *));
static int      wait_action PROTO((AUTH_REQ *, int, char *));

/*************************************************************************
 *
 *	Utility AATV structures
 *
 *************************************************************************/

static AATV     ack_aatv =
{ "ACK", -1, AA_DIRECT, NULL, NULL, ack_action, NULL, NULL, 0 };

static AATVPTR  rad_ack_aatv = & ack_aatv;

static AATV     any_aatv =
{ "*", -1, AA_DIRECT, NULL, NULL, null_action, NULL, NULL, 0 };

static AATVPTR  rad_any_aatv = & any_aatv;

static AATV     end_aatv =
{ "CLEANUP", -1, AA_DIRECT, NULL, NULL, NULL, NULL, NULL, 0 };

static AATVPTR  rad_end_aatv = & end_aatv;

static AATV     xstring_clear_aatv =
{ "CLRX", -1, AA_DIRECT, NULL, NULL, xstring_clear_action, NULL, NULL, 0 };

static AATVPTR  rad_xstring_clear_aatv = & xstring_clear_aatv;

static AATV     fatal_log_aatv =
{ "FATAL", -1, AA_DIRECT, NULL, NULL, fatal_log_action, NULL, NULL, 0 };

static AATVPTR  rad_fatal_log_aatv = & fatal_log_aatv;

static AATV     kill_aatv =
{ "KILL", -1, AA_DIRECT, NULL, NULL, kill_action, NULL, NULL, 0 };

static AATVPTR  rad_kill_aatv = & kill_aatv;

static AATV     nak_aatv =
{ "NAK", -1, AA_DIRECT, NULL, NULL, nak_action, NULL, NULL, 0 };

static AATVPTR  rad_nak_aatv = & nak_aatv;

static AATV     null_aatv =
{ "NULL", -1, AA_DIRECT, NULL, NULL, null_action, NULL, NULL, 0 };

static AATVPTR  rad_null_aatv = & null_aatv;

static AATV     pending_aatv =
{ "PENDING", -1, AA_DIRECT, NULL, NULL, pending_action, NULL, NULL, 0 };

static AATVPTR  rad_pending_aatv = & pending_aatv;

static AATV     redo_aatv =
{ "REDO", -1, AA_DIRECT, NULL, NULL, redo_action, NULL, NULL, 0 };

static AATVPTR  rad_redo_aatv = & redo_aatv;

static AATV     status_aatv =
{ "SRV_STATUS", -1, AA_DIRECT, NULL, NULL, server_status, NULL, NULL, 0 };

static AATVPTR  rad_status_aatv = & status_aatv;

static AATV     timeout_aatv =
{ "TIMEOUT", -1, AA_DIRECT, NULL, NULL, timeout_action, NULL, NULL, 0 };

static AATVPTR  rad_timeout_aatv = & timeout_aatv;

static AATV     timer_aatv =
{ "TIMER", -1, AA_DIRECT, NULL, NULL, timer_action, NULL, NULL, 0 };

static AATVPTR  rad_timer_aatv = & timer_aatv;

static AATV     wait_aatv =
{ "WAIT", -1, AA_DIRECT, NULL, NULL, wait_action, NULL, NULL, 0 };

static AATVPTR  rad_wait_aatv = & wait_aatv;

/* Refer to the Table of Attributes near the end of the RADIUS DRAFT RFC */

/* The Pruning flag values are: */
					/* The number of a/v pairs allowed: */
	/* 0 ==> neither */
	/* 1 ==> ack only			 0 implies RFC "0" */
	/* 2 ==> nak only			-1 implies RFC "0+" */
	/* 3 ==> both ................\    /.... 1 implies RFC "0-1" and "1" */

#ifndef ASCEND

static PRUN_RULE liv_attr[] = {	{   1, 0,  0 }, /* User-Name */
				{   2, 0,  0 }, /* User-Password */
				{   3, 0,  0 }, /* CHAP-Password */
				{   4, 0,  0 }, /* NAS-IP-Address */
				{   5, 0,  0 }, /* NAS-Port */
				{   6, 1,  1 }, /* Service-Type */
				{   7, 1,  1 }, /* Framed-Protocol */
				{   8, 1,  1 }, /* Framed-IP-Address */
				{   9, 1,  1 }, /* Framed-IP-Netmask */
				{  10, 1,  1 }, /* Framed-Routing */
				{  11, 1,  1 }, /* Filter-Id NB: non-standard */
				{  12, 1,  1 }, /* Framed-MTU */
				{  13, 1,  1 }, /* Framed-Compression non-RFC */
				{  14, 1,  1 }, /* Login-IP-Host non-standard */
				{  15, 1,  1 }, /* Login-Service */
				{  16, 1,  1 }, /* Login-Port */
				{  17, 0,  0 }, /* --- deprecated --- */
				{  18, 3, -1 }, /* Reply-Message */
				{  19, 1,  1 }, /* Login-Callback-Number */
				{  20, 1,  1 }, /* Framed-Callback-Id */
				{  21, 0,  0 }, /* --- deprecated --- */
				{  22, 1, -1 }, /* Framed-Route */
	/* non-standard */	{  23, 0,  0 }, /* Framed-IPX-Network */
				{  24, 1,  1 }, /* State */
				{  25, 1,  1 }, /* Class */
				{  26, 1, -1 }, /* Vendor-Specific */
				{  27, 1,  1 }, /* Session-Timeout */
				{  28, 1,  1 }, /* Idle-Timeout */
				{  29, 1,  1 }, /* Termination-Action */
				{  30, 0,  0 }, /* Client-Port-DNIS */
				{  31, 0,  0 }, /* Caller-Id */
				{  32, 0,  0 }, /* NAS-Identifier */
				{  33, 3, -1 }, /* Proxy-State */
	/* non-standard */	{  34, 0,  0 }, /* Login-LAT-Service */
	/* non-standard */	{  35, 0,  0 }, /* Login-LAT-Node */
	/* non-standard */	{  36, 0,  0 }, /* Login-LAT-Group */
	/* non-standard */	{  37, 0,  0 }, /* Framed-AppleTalk-Link */
	/* non-standard */	{  38, 0,  0 }, /* Framed-AppleTalk-Network */
	/* non-standard */	{  39, 0,  0 }, /* Framed-AppleTalk-Zone */
				{  40, 0,  0 }, /* Acct-Status-Type */
				{  41, 0,  0 }, /* Acct-Delay-Time */
				{  42, 0,  0 }, /* Acct-Input-Octets */
				{  43, 0,  0 }, /* Acct-Output-Octets */
				{  44, 0,  0 }, /* Acct-Session-Id */
				{  45, 0,  0 }, /* Acct-Authentic */
				{  46, 0,  0 }, /* Acct-Session-Time */
				{  47, 0,  0 }, /* Acct-Input-Packets */
				{  48, 0,  0 }, /* Acct-Output-Packets */
				{  49, 0,  0 }, /* Acct-Terminate-Cause */
				{  50, 0,  0 }, /* Acct-Multi-Session-Id */

				{  60, 0,  0 }, /* CHAP-Challenge */
				{  61, 0,  0 }, /* NAS-Port-Type */
				{  62, 1,  1 }, /* Port-Limit */
				{  63, 1,  1 }, /* Login-LAT-Port */

	/* temporary */		{ PW_LAS_START_TIME, 0,  0 }, /* Session-Start-Time */
	/* temporary */		{ PW_LAS_CODE, 0,  0 }, /* Session-Code */
	/* temporary */		{ PW_LAS_DURATION, 0,  0 }, /* Remote-Duration */
	/* temporary */		{ PW_LOCAL_DURATION, 0,  0 }, /* Local-Duration */
	/* temporary */		{ PW_SERVICE_CLASS, 0,  0 }, /* Service-Class */
	/* temporary */		{ PW_PORT_ENTRY, 0,  0 }, /* Port-Entry */
	/* experimental */	{ PW_PROXY_ACTION, 0,  0 }, /* Proxy-Action */
	/* experimental */	{ PW_TOKEN, 0,  0 }, /* Token */
	/* experimental */	{ PW_HUNTGROUP_NAME, 0,  0 }, /* Huntgroup-Name */
	/* experimental */	{ PW_USER_ID, 0,  0 }, /* User-Id */
	/* experimental */	{ PW_USER_REALM, 0,  0 }, /* User-Realm */
				{   0, 0,  0 } }; /* --- must be last --- */

static PRUN_LIST liv_list = { "Livingston", liv_attr, 0 }; /* should be file */

#else
/* This is a temporary fix to merge Ascend and Merit attiributes. */
/* NOTE: Some Merit attributes collide with Ascend attributes.    */
/*       In these cases the Merit attribute is preferred.         */

/* The Pruning flag values are: */
					/* The number of a/v pairs allowed: */
	/* 0 ==> neither */
	/* 1 ==> ack only			 0 implies RFC "0" */
	/* 2 ==> nak only			-1 implies RFC "0+" */
	/* 3 ==> both ................\    /.... 1 implies RFC "0-1" and "1" */

static PRUN_RULE mixup_attr[] = {
				{   1, 0,  0 }, /* User-Name */
				{   2, 0,  0 }, /* User-Password */
				{   3, 0,  0 }, /* CHAP-Password */
				{   4, 0,  0 }, /* NAS-IP-Address */
				{   5, 0,  0 }, /* NAS-Port */
				{   6, 1,  1 }, /* Service-Type */
				{   7, 1,  1 }, /* Framed-Protocol */
				{   8, 1,  1 }, /* Framed-IP-Address */
				{   9, 1,  1 }, /* Framed-IP-Netmask */
				{  10, 1,  1 }, /* Framed-Routing */
				{  11, 1,  1 }, /* Filter-Id NB: non-standard */
				{  12, 1,  1 }, /* Framed-MTU */
				{  13, 1,  1 }, /* Framed-Compression non-RFC */
				{  14, 1,  1 }, /* Login-IP-Host non-standard */
				{  15, 1,  1 }, /* Login-Service */
				{  16, 1,  1 }, /* Login-Port */
				{  17, 0,  0 }, /* --- deprecated --- */
				{  18, 3, -1 }, /* Reply-Message */
				{  19, 1,  1 }, /* Login-Callback-Number */
				{  20, 1,  1 }, /* Framed-Callback-Id */
				{  21, 0,  0 }, /* --- deprecated --- */
				{  22, 1, -1 }, /* Framed-Route */
	/* non-standard */	{  23, 0,  0 }, /* Framed-IPX-Network */
				{  24, 1,  1 }, /* State */
				{  25, 1,  1 }, /* Class */
				{  26, 1, -1 }, /* Vendor-Specific */
				{  27, 1,  1 }, /* Session-Timeout */
				{  28, 1,  1 }, /* Idle-Timeout */
				{  29, 1,  1 }, /* Termination-Action */
				{  30, 0,  0 }, /* Client-Port-DNIS */
				{  31, 0,  0 }, /* Caller-Id */
				{  32, 0,  0 }, /* NAS-Identifier */
				{  33, 3, -1 }, /* Proxy-State */
	/* non-standard */	{  34, 0,  0 }, /* Login-LAT-Service */
	/* non-standard */	{  35, 0,  0 }, /* Login-LAT-Node */
	/* non-standard */	{  36, 0,  0 }, /* Login-LAT-Group */
	/* non-standard */	{  37, 0,  0 }, /* Framed-AppleTalk-Link */
	/* non-standard */	{  38, 0,  0 }, /* Framed-AppleTalk-Network */
	/* non-standard */	{  39, 0,  0 }, /* Framed-AppleTalk-Zone */
				{  40, 0,  0 }, /* Acct-Status-Type */
				{  41, 0,  0 }, /* Acct-Delay-Time */
				{  42, 0,  0 }, /* Acct-Input-Octets */
				{  43, 0,  0 }, /* Acct-Output-Octets */
				{  44, 0,  0 }, /* Acct-Session-Id */
				{  45, 0,  0 }, /* Acct-Authentic */
				{  46, 0,  0 }, /* Acct-Session-Time */
				{  47, 0,  0 }, /* Acct-Input-Packets */
				{  48, 0,  0 }, /* Acct-Output-Packets */
				{  49, 0,  0 }, /* Acct-Terminate-Cause */
				{  50, 0,  0 }, /* Acct-Multi-Session-Id */

				{  60, 0,  0 }, /* CHAP-Challenge */
				{  61, 0,  0 }, /* NAS-Port-Type */
				{  62, 1,  1 }, /* Port-Limit */
				{  63, 1,  1 }, /* Login-LAT-Port */

	/* experimental-log */	{ PW_LAS_START_TIME, 0,  0 }, /* Session-Start-Time */
	/* experimental-log */	{ PW_LAS_CODE, 0,  0 }, /* Session-Code */
	/* experimental-log */	{ PW_LAS_DURATION, 0,  0 }, /* Remote-Duration */
	/* experimental-log */	{ PW_LOCAL_DURATION, 0,  0 }, /* Local-Duration */

	/* experimental */	{ PW_SERVICE_CLASS, 0,  0 }, /* Service-Class */
	/* experimental */	{ PW_PORT_ENTRY, 0,  0 }, /* Port-Entry */
	/* experimental */	{ PW_PROXY_ACTION, 0,  0 }, /* Proxy-Action */
	/* experimental */	{ PW_TOKEN, 0,  0 }, /* Token */
	/* experimental */	{ PW_HUNTGROUP_NAME, 0,  0 }, /* Huntgroup-Name */
	/* experimental */	{ PW_USER_ID, 0,  0 }, /* User-Id */
	/* experimental */	{ PW_USER_REALM, 0,  0 }, /* User-Realm */

	/* All Ascend attributes will be listed as 0+ */

	/* Ascend */		{ 172, 1, -1},	/* Ascend-Base-Channel-Count */
	/* Ascend */		{ 173, 1, -1},	/* Ascend-Minimum-Channels */
	/* Ascend */		{ 174, 1, -1},	/* Ascend-IPX-Route */
	/* Ascend */		{ 175, 1, -1},	/* Ascend-FT1-Caller */
	/* Ascend */		{ 176, 1, -1},	/* Ascend-Backup */
	/* Ascend */		{ 177, 1, -1},	/* Ascend-Call-Type */
	/* Ascend */		{ 178, 1, -1},	/* Ascend-Group */
	/* Ascend */		{ 179, 1, -1},	/* Ascend-FR-DLCI */
	/* Ascend */		{ 180, 1, -1},	/* Ascend-FR-Profile-Name */
	/* Ascend */		{ 181, 1, -1},	/* Ascend-Ara-PW */
	/* Ascend */		{ 182, 1, -1},	/* Ascend-IPX-Node-Addr	*/
	/* Ascend */		{ 183, 1, -1},	/* Ascend-Home-Agent-IP-Addr */
	/* Ascend */		{ 184, 1, -1},	/* Ascend-Home-Agent-Password */
	/* Ascend */		{ 185, 1, -1},	/* Ascend-Home-Network-Name */
	/* Ascend */		{ 186, 1, -1},	/* Ascend-Home-Agent-UDP-Port */
	/* Ascend */		{ 187, 1, -1},	/* Ascend-Multilink-ID */
	/* Ascend */		{ 188, 1, -1},	/* Ascend-Num-In-Multilink */
	/* Ascend */		{ 189, 1, -1},	/* Ascend-First-Dest */
	/* Ascend */		{ 190, 1, -1},	/* Ascend-Pre-Input-Octets */
	/* Ascend */		{ 191, 1, -1},	/* Ascend-Pre-Output-Octets */
	/* Ascend */		{ 192, 1, -1},	/* Ascend-Pre-Input-Packets */
	/* Ascend */		{ 193, 1, -1},	/* Ascend-Pre-Output-Packets */
	/* Ascend */		{ 194, 1, -1},	/* Ascend-Maximum-Time */
	/* Ascend */		{ 195, 1, -1},	/* Ascend-Disconnect-Cause */
	/* Ascend */		{ 196, 1, -1},	/* Ascend-Connect-Progress */
	/* Ascend */		{ 197, 1, -1},	/* Ascend-Data-Rate */
	/* Ascend */		{ 198, 1, -1},	/* Ascend-PreSession-Time */
	/* Ascend */		{ 199, 1, -1},	/* Ascend-Token-Idle */
	/* Ascend */		{ 200, 1, -1},	/* Ascend-Token-Immediate */
	/* Ascend */		{ 201, 1, -1},	/* Ascend-Require-Auth */
	/* Ascend */		{ 202, 1, -1},	/* Ascend-Number-Sessions */
	/* Ascend */		{ 203, 1, -1},	/* Ascend-Authen-Alias */
	/* Ascend */		{ 204, 1, -1},	/* Ascend-Token-Expiry */
	/* Ascend */		{ 205, 1, -1},	/* Ascend-Menu-Selector */
	/* Ascend */		{ 206, 1, -1},	/* Ascend-Menu-Item */
	/* Ascend */		{ 207, 1, -1},	/* Ascend-PW-Warntime */
	/* Ascend */		{ 208, 1, -1},	/* Ascend-PW-Lifetime */
	/* Ascend */		{ 209, 1, -1},	/* Ascend-IP-Direct */
	/* Ascend */		{ 210, 1, -1},	/* Ascend-PPP-VJ-Slot-Comp */

	/*
	 * Merit uses value 211, so Ascend-PPP-VJ-1172 is not supported.
	 */

	/* Ascend */		{ 212, 1, -1},	/* Ascend-PPP-Async-Map */

	/*
	 * Merit uses value 213, so Ascend-Third-Prompt is not supported.
	 */

	/* Ascend */		{ 214, 1, -1},	/* Ascend-Send-Secret */
	/* Ascend */		{ 215, 1, -1},	/* Ascend-Receive-Secret */
	/* Ascend */		{ 216, 1, -1},	/* Ascend-IPX-Peer-Mode */
	/* Ascend */		{ 217, 1, -1},	/* Ascend-IP-Pool-Definition */
	/* Ascend */		{ 218, 1, -1},	/* Ascend-Assign-IP-Pool */
	/* Ascend */		{ 219, 1, -1},	/* Ascend-FR-Direct */
	/* Ascend */		{ 220, 1, -1},	/* Ascend-FR-Direct-Profile */

	/*
	 * Merit uses values 221, 222, and 223 so Ascend-FR-Direct-DLCI,
	 * Ascend-Handle-IPX, and Ascend-Netware-Timeout are not supported.
	 */

	/* Ascend */		{ 224, 1, -1},	/* Ascend-IPX-Alias */
	/* Ascend */		{ 225, 1, -1},	/* Ascend-Metric */
	/* Ascend */		{ 226, 1, -1},	/* Ascend-PRI-Number-Type */
	/* Ascend */		{ 227, 1, -1},	/* Ascend-Dial-Number */
	/* Ascend */		{ 228, 1, -1},	/* Ascend-Route-IP */
	/* Ascend */		{ 229, 1, -1},	/* Ascend-Route-IPX */
	/* Ascend */		{ 230, 1, -1},	/* Ascend-Bridge */
	/* Ascend */		{ 231, 1, -1},	/* Ascend-Send-Auth */
	/* Ascend */		{ 232, 1, -1},	/* Ascend-Send-Passwd */
	/* Ascend */		{ 233, 1, -1},	/* Ascend-Link-Compression */
	/* Ascend */		{ 234, 1, -1},	/* Ascend-Target-Util */
	/* Ascend */		{ 235, 1, -1},	/* Ascend-Maximum-Channels */
	/* Ascend */		{ 236, 1, -1},	/* Ascend-Inc-Channel-Count */
	/* Ascend */		{ 237, 1, -1},	/* Ascend-Dec-Channel-Count */
	/* Ascend */		{ 238, 1, -1},	/* Ascend-Seconds-Of-History */
	/* Ascend */		{ 239, 1, -1},	/* Ascend-History-Weigh-Type */
	/* Ascend */		{ 240, 1, -1},	/* Ascend-Add-Seconds */
	/* Ascend */		{ 241, 1, -1},	/* Ascend-Remove-Seconds */
	/* Ascend */		{ 242, 1, -1},	/* Ascend-Data-Filter */
	/* Ascend */		{ 243, 1, -1},	/* Ascend-Call-Filter */
	/* Ascend */		{ 244, 1, -1},	/* Ascend-Idle-Limit */
	/* Ascend */		{ 245, 1, -1},	/* Ascend-Preempt-Limit */
	/* Ascend */		{ 246, 1, -1},	/* Ascend-Callback */
	/* Ascend */		{ 247, 1, -1},	/* Ascend-Data-Svc */
        /* Ascend */		{ 248, 1, -1},	/* Ascend-Force-56 */
	/* Ascend */		{ 249, 1, -1},	/* Ascend-Billing-Number */
	/* Ascend */		{ 250, 1, -1},	/* Ascend-Call-By-Call */
	/* Ascend */		{ 251, 1, -1},	/* Ascend-Transit-Number */
	/* Ascend */		{ 252, 1, -1},	/* Ascend-Host-Info */
	/* Ascend */		{ 253, 1, -1},	/* Ascend-PPP-Address */
	/* Ascend */		{ 254, 1, -1},	/* Ascend-MPP-Idle-Percent */
				{   0, 0,  0 } }; /* --- must be last --- */

static PRUN_LIST mixup_list = { "Livingston+some_Ascend", mixup_attr, 0 };
/* temporary for now */

#endif


/*
 *	Global Variables
 */

char            recv_buffer[4096];
char            send_buffer[4096];
char            ourhostname[MAXHOSTNAMELEN];
char            port_msg[AUTH_STRING_LEN]; /* Paired with ack_nak_flag below */
int             ack_nak_flag;       /* 0 => fail msg, 1 => success msg */
int             debug_flag = 0;     /* Values > 0 allow various debug output */
int             dumpcore = 0;       /* Normally 0, when > 0 causes core dump */
int             file_logging = 1;   /* 0 => syslog, 1 => logfile, 2 => stderr */
int             zap_logfile = 0;    /* Empty logfile first time referenced */
int             want_timer = 0;     /* When non-zero, call timer functions */
char           *progname;
char           *radius_dir;
char           *fsm_id = NULL;      /* From %FSMID keyword in FSM table */
FILE           *ddt = (FILE *) NULL;
FILE           *msgfd = (FILE *) NULL;

/* needed only by passchange.c */
UINT4           expiration_seconds;
UINT4           warning_seconds;

/* needed publicly only by rad.mnet.c */
struct sockaddr_in fromsin;       /* Remote socket info */

/* needed publicly only by rad.tacacs.c */
AUTH_REQ       *global_request_q = (AUTH_REQ *) NULL; /* auth. reqs. go here */

/* needed publicly only by authenticate.c, radfile.c and users.c */
AATV           *authtype_tv[PW_AUTH_MAX + 1]; /* AATV by authentication types */

/* needed publicly only by rad.accounting.c */
char           *radacct_dir;
u_short         inetd = 0;
u_short         aport = 0;

/* needed publicly only by funcs.c */
time_t          birthdate;

/* needed publicly only by users.c */
int             authfile_cnt = 0;
int             clients_cnt = 0;
int             users_cnt = 0;
/* the following variables are declared globally and initialized in users.c */
extern int      dnspid;            /* PID of DNS resolver process */
extern int      spawn_flag;        /* 0 => no spawning, 1 => spawning allowed */
extern int      rad_ipc_port;      /* Local port number of ipc socket */

static AATV     server_aatv =
{ "RADIUS", -1, AA_SOCKET, rad_init, NULL, NULL, rad_recv, NULL, -1 };

static AATVPTR  rad_server_aatv = & server_aatv;

static AATV     reply_aatv =
{ "REPLY", -1, AA_SOCKET, rad_reply_init, NULL, rad_reply, NULL, NULL, -1 };

static AATVPTR  rad_reply_aatv = & reply_aatv;

extern AATVPTR  rad_acct_aatv;
extern AATVPTR  rad_acct_switch_aatv;
extern AATVPTR  rad_accounting_aatv;
extern AATVPTR  rad_ipc_aatv;

static AATVPTR *aatv_ptrs[] =
{
	&rad_acct_aatv,
	&rad_acct_switch_aatv,
	&rad_accounting_aatv,
	&rad_server_aatv,
	&rad_reply_aatv,
	&rad_ipc_aatv,
	AATVS,	/* for all "engine external" AATVs (except ACCT) see radius.h */
	&rad_ack_aatv,
	&rad_any_aatv,
	&rad_end_aatv,
	&rad_fatal_log_aatv,
	&rad_kill_aatv,

#ifdef  MERIT_LAS
	&rad_log_aatv,
	&rad_log_all_aatv,
	&rad_log_brief_aatv,
	&rad_log_old_aatv,
	&rad_log_v1_0_aatv,
	&rad_log_v1_1_aatv,
	&rad_log_v2_0_aatv,
	&rad_log_v2_1_aatv,
#endif	/* MERIT_LAS */

	&rad_nak_aatv,
	&rad_null_aatv,
	&rad_pending_aatv,
	&rad_redo_aatv,
	&rad_status_aatv,
	&rad_timeout_aatv,
	&rad_timer_aatv,
	&rad_wait_aatv,
	&rad_xstring_clear_aatv
};

#define	MAX_AATV (sizeof (aatv_ptrs) / sizeof (aatv_ptrs[0]) )

static AATV    *sockfd_tv[MAX_AATV + 1];
static fd_set   select_mask;

static int    (*timer_funcs[MAX_AATV + 1]) PROTO((void));

/*************************************************************************
 *
 *	Function: Main RADIUS server code
 *
 *	Purpose: Handle dispatching of incoming RADIUS requests on well
 *		 known socket(s) to AATV recv() function(s).
 *
 *************************************************************************/

int
main (argc, argv)

int             argc;
char          **argv;

{
	int             result;
	int             len;
	int             selcnt;
	int             i;
	int             j;
	AATV	       *aatv;
	AUTH_REQ       *authreq;
	FILE           *fp;
	fd_set          readfds;
	struct timeval  timeout;
	struct timeval *selecttime = NULL;
	char           *ptr;
	struct servent *svp;
	int             dtablesize;
	int             authtype;
	EV              event;
	int             pid;
	int             cons;
	u_short         udp_port;
	sigset_t        signals;          /* Main signal mask */
	struct sigaction action;
	char            errmsg[256];
	char            oldpath[MAXPATHLEN];
	static char    *func = "main";

	fprintf (stderr, "Merit RADIUS 2.4.23C, licensed software\n");
	fprintf (stderr, "Copyright (c) 1992, 1993, 1994, 1995, 1996 by The\n");
	fprintf (stderr,
	     "Regents of the University of Michigan and Merit Network, Inc.\n");

#if ((defined OSF) || (defined SCO))
	set_auth_parameters (argc, argv);
#endif /* OSF */

	file_logging = 1;
	progname = *argv++;
	argc--;

	radius_dir = RADIUS_DIR;
	radacct_dir = RADACCT_DIR;
	timeout.tv_sec = 0;
	timeout.tv_usec = 0;

	action.sa_handler = sig_fatal;
	sigfillset (&action.sa_mask);
	action.sa_flags = 0;

	sigaction (SIGQUIT, &action, NULL);
	sigaction (SIGILL, &action, NULL);
	sigaction (SIGTRAP, &action, NULL);
	sigaction (SIGIOT, &action, NULL); /* Perhaps also known as SIGABRT ! */
	sigaction (SIGFPE, &action, NULL);

#ifdef	_SC_OPEN_MAX
	if ((dtablesize = sysconf (_SC_OPEN_MAX)) == -1)
	{
		perror ("_SC_OPEN_MAX");
		exit (-17);
	}
#else	/* Assume BSD */
	dtablesize = getdtablesize ();
#endif	/* _SC_OPEN_MAX */

	for (j = dtablesize; j >= 3; j--)
	{
		close (j);
	}

	/*
	 * Current options are:
	 *	
	 *	+  Print usage message.
	 *	h  Print usage message.
	 *	
	 *	a  Set RADIUS accounting directory
	 *	c  Set new current working directory
	 *	d  Set RADIUS database directory
	 *	f  Specify RADIUS FSM file.
	 *	g  Specify file, syslog or stderr logging.
	 *	p  Specify UDP port number for receiving RADIUS authentication.
	 *	pp Specify UDP port number for relaying RADIUS authentication.
	 *	q  Specify UDP port number for receiving RADIUS accounting.
	 *	qq Specify UDP port number for relaying RADIUS accounting.
	 *	s  Single-process (non-spawning) flag.
	 *	t  Specify inactivity timeout value.
	 *	u  Don't cache "users" files (DBM version only).
	 *	v  Print version info.
	 *	x  Add to the debug flag value.
	 *	z  Zap logfile and debug file first time they're referenced.
	 *	   (-z ignored if debugging '-x' isn't enabled)
	 *	
	 */

	while (argc) /* XXX - should use getopt here */
	{
		if (argv[0][0] != '-')
		{
			fprintf (stderr, "%s: Invalid argument, \"%s\"\n",
				progname, *argv);
			usage ();
		}

		switch (argv[0][1])
		{

		    case '+':
		    case 'h':
			usage ();
			break;

		    case 'v':
			strcpy (errmsg, version ());
			fprintf (stderr, "%s\n", errmsg);
			exit (-10);
			break;

		    case 'x':
			debug_flag++;
			break;

		    case 's':
			spawn_flag = 0;
			break;

		    case 'f':
			if (argc <= 1)
			{
				usage ();
			}
			radius_fsm = (++argv)[0];
			argc--;
			break;

		    case 'a':
			if (argc <= 1)
			{
				usage ();
			}
			radacct_dir = (++argv)[0];
			argc--;
			break;

		    case 'c':
			if (argc <= 1)
			{
				usage ();
			}
			cur_wrk_dir = (++argv)[0];
			argc--;
			break;

		    case 'd':
			if (argc <= 1)
			{
				usage ();
			}
			radius_dir = (++argv)[0];
			argc--;
			break;

		    case 'g':
			if (argc <= 1)
			{
				usage ();
			}
			if (strcasecmp (*(++argv), "logfile") == 0)
			{
				file_logging = 1;
				sprintf (path, "%s/%s", radius_dir, RADIUS_LOG);
				if ((msgfd = fopen (path, "a")) == (FILE *) NULL)
				{
					fprintf (stderr,
					   "%s: Couldn't open %s for logging\n",
						progname, path);
				}
			}
			else
			{
				if (strcasecmp (*argv, "stderr") == 0)
				{
					file_logging = 2;
					msgfd = stderr;
				}
				else
				{
					file_logging = 0;
#ifdef	LOG_CONS
					openlog ("radiusd", LOG_PID | LOG_CONS,
						LOG_DAEMON);
#else	/* LOG_CONS */
					openlog ("radiusd", LOG_PID);
#endif	/* LOG_CONS */
					msgfd = (FILE *) NULL;
				}
			}
			argc--;
			break;

		    case 'p':
			i = 0; /* use i as double-letter flag */
			if (argv[0][2] == 'p')
			{
				i = 1;
				if (argv[0][3])
				{
					ptr = &argv[0][3];
				}
				else
				{
					if (--argc == 0)
					{
						usage ();
					}
					ptr = (++argv)[0];
				}
			}
			else /* only one "p" and i == 0 */
			{
				if (argv[0][2])
				{
					ptr = &argv[0][2];
				}
				else
				{
					if (--argc == 0)
					{
						usage ();
					}
					ptr = (++argv)[0];
				}
			}

			if (sscanf (ptr, "%u", &result) != 1)
			{
				fprintf (stderr, "%s: Invalid port number\n",
					 progname);
				usage ();
			}

			if (i == 1) /* double "p" sets relay UDP port */
			{
				llport = result;
			}
			else /* only one "p" */
			{
				lport = result;
			}
			break;

		    case 'q':
			i = 0; /* use i as double-letter flag */
			if (argv[0][2] == 'q')
			{
				i = 1;
				if (argv[0][3])
				{
					ptr = &argv[0][3];
				}
				else
				{
					if (--argc == 0)
					{
						usage ();
					}
					ptr = (++argv)[0];
				}
			}
			else /* only one "q" and i == 0 */
			{
				if (argv[0][2])
				{
					ptr = &argv[0][2];
				}
				else
				{
					if (--argc == 0)
					{
						usage ();
					}
					ptr = (++argv)[0];
				}
			}

			if (sscanf (ptr, "%u", &result) != 1)
			{
				fprintf (stderr, "%s: Invalid port number\n",
					 progname);
				usage ();
			}

			if (i == 1) /* double "q" sets relay UDP port */
			{
				aaport = result;
			}
			else /* only one "q" */
			{
				aport = result;
			}
			break;

#if defined(USE_DBM) || defined(USE_NDBM)
		    case 'u':
			/* Don't read RADIUS_USERS file into data structure */
			cache_users = 0;
			break;
#endif	/* USE_DBM || USE_NDBM*/

		    case 't':

			/*
			 * Set inactivity timeout for select(2).  The program
			 * will exit upon timeout.  This is intended for those
			 * systems which will start the server from inetd(8).
			 */
			if (argv[0][2])
			{
				ptr = &argv[0][2];
			}
			else
			{
				if (argc > 1)
				{
					ptr = (++argv)[0];
					argc--;
				}
				else
				{
					ptr = NULL;
				}
			}
			if (ptr == NULL || sscanf (ptr, "%d", &i) != 1)
			{
				fprintf (stderr, "%s: Invalid timeout value\n",
					 progname);
				usage ();
			}
			timeout.tv_sec = 60 * i;
			selecttime = &timeout;
			break;

		    case 'z':	/* zap (empty) logfile & debug file, once */
			zap_logfile = 1;
			zap_debugfile = 1;
			break;

		    default:
			fprintf (stderr, "%s: Invalid option, \"%s\"\n",
				progname, *argv);
			usage ();
			break;
		}
		argc--;
		argv++;
	}

	if (zap_logfile > 0)
	{
		sprintf (path, "%s/%s", radius_dir, RADIUS_LOG);
		strcpy (oldpath, path);
		strcat (oldpath, ".old");
		unlink (oldpath);
		rename (path, oldpath);

		sprintf (path, "%s/%s", radius_dir, RADIUS_DEBUG);
		strcpy (oldpath, path);
		strcat (oldpath, ".old");
		unlink (oldpath);
		rename (path, oldpath);
	}

	logit (LOG_DAEMON, LOG_INFO, "Merit RADIUS 2.4.23C, licensed software");
	logit (LOG_DAEMON, LOG_INFO,
		"Copyright (c) 1992, 1993, 1994, 1995, 1996 by The");
	logit (LOG_DAEMON, LOG_INFO,
	       "Regents of the University of Michigan and Merit Network, Inc.");

	if ((file_logging == 1) && (msgfd != (FILE *) NULL))
	{
		fflush (msgfd);
	}

#ifdef	SVR4
	if (sysinfo (SI_HOSTNAME, ourhostname, sizeof (ourhostname)) < 0)
	{
		perror ("SI_HOSTNAME");
		exit (-16);
	}
#else	/* Assume BSD */
	if (gethostname (ourhostname, sizeof (ourhostname)) < 0)
	{
		perror ("gethostname");
		exit (-16);
	}
#endif	/* SVR4 */

	if (cur_wrk_dir != (char *) NULL)
	{
		if (chdir (cur_wrk_dir) < 0)
		{
			perror ("chdir");
			exit (-15);
		}
	}

	if (debug_flag > 0)
	{
		set_debug (1);
	}

	avalue = 0;
	astring[0] = '\0';

	/* Initialize the dictionary */
	if (dict_init () != 0)
	{
		if ((file_logging == 1) && (msgfd != (FILE *) NULL))
		{
			fflush (msgfd);
		}

		perror ("dict_init");
		exit (-1);
	}

	/* Initialize Configuration Values */
	if (config_initialize () != 0)
	{
		if ((file_logging == 1) && (msgfd != (FILE *) NULL))
		{
			fflush (msgfd);
		}

		perror ("config_initialize");
		exit (-2);
	}

	if (aport == 0 || aaport == 0) /* not given on command line */
	{
		svp = getservbyname ("radacct", "udp");
		if (svp == (struct servent *) NULL)
		{
			udp_port = PW_ACCT_UDP_PORT;
			fprintf (stderr,
	"%s: Cannot find service: radacct/udp\nUsing default UDP port %u for ",
				 progname, udp_port);
			if (aport == 0)
			{
				aport = udp_port;  /* Assume default */
				fprintf (stderr, "input ");
			}
			if (aaport == 0)
			{
				aaport = udp_port; /* Assume default */
				fprintf (stderr, "output ");
			}
			fprintf (stderr, "accounting port.\n");
		}
		else
		{
			if (aport == 0)
			{
				aport = ntohs(svp->s_port);
			}
			if (aaport == 0)
			{
				aaport = ntohs(svp->s_port);
			}
		}
	}

	if (lport == 0 || llport == 0) /* not given on command line */
	{
		svp = getservbyname ("radius", "udp");
		if (svp == (struct servent *) NULL)
		{
			udp_port = PW_AUTH_UDP_PORT;
			fprintf (stderr,
	"%s: Cannot find service: radius/udp\nUsing default UDP port %u for ",
				 progname, udp_port);
			if (lport == 0)
			{
				lport = udp_port;  /* Assume default */
				fprintf (stderr, "input ");
			}
			if (llport == 0)
			{
				llport = udp_port; /* Assume default */
				fprintf (stderr, "output ");
			}
			fprintf (stderr, "authentication port.\n");
		}
		else
		{
			if (lport == 0)
			{
				lport = ntohs(svp->s_port);
			}
			if (llport == 0)
			{
				llport = ntohs(svp->s_port);
			}
		}
	}

	fsm = (FSM_ENT **) NULL;
	default_fsm = (FSM_ENT **) NULL;
	nfsm = 0;

	for (i = 0; i <= PW_AUTH_MAX; i++)
	{
	        authtype_tv[i] = (AATV *) NULL; /* array for authentication */
	}

	for (i = j = 0; i < MAX_AATV; i++)
	{	
		if ((aatv = *aatv_ptrs[i]) == NULL)
		{
			continue;
		}
		if ((authtype = aatv->authen_type) != -1)
		{
			authtype_tv[authtype] = aatv;
		}
		if (aatv->timer != (int (*)() ) NULL)
		{
			timer_funcs[j++] = aatv->timer; /* array for timers */
		}
	}

	/* Read various config files to memory and init aatvs */
	reconfig (0);

	zap_logfile = 0;

	timer_funcs[j] = NULL;

	if (inetd > 0 && selecttime == NULL)
	{
		timeout.tv_sec = 60 * DEFAULT_INETD_TIMEOUT;
		selecttime = &timeout;
	}
	stat_files (1, cache_users); /* first arg == one => init stat values */

	/*
	 * Disconnect from session
	 */
	if (debug_flag == 0 && !inetd)
	{
		pid = (int) fork ();
		if (pid < 0)
		{
			sprintf (errmsg, "%s: Could not fork", progname);
			perror (errmsg);

			if ((file_logging == 1) && (msgfd != (FILE *) NULL))
			{
				fflush (msgfd);
			}

			exit (-5);
		}
		if (pid > 0) /* parent */
		{
			exit (0);
		}
	}

	setsid ();

	sigemptyset (&signals);		/* Init signal suspend mask */
	sigaddset (&signals, SIGALRM);
	sigaddset (&signals, SIGCHLD);
	sigaddset (&signals, SIGHUP);
	sigaddset (&signals, SIGINT);
	sigaddset (&signals, SIGTERM);
	sigaddset (&signals, SIGUSR1);
	sigaddset (&signals, SIGUSR2);

	memcpy ((char *) &action.sa_mask, (char *) &signals,
		sizeof (action.sa_mask));
	action.sa_flags = 0;

	action.sa_handler = reply_timer;	/* Set up to use alarm() */
	sigaction (SIGALRM, &action, NULL);
	action.sa_handler = child_end;	/* General AATV process end routine */
	sigaction (SIGCHLD, &action, NULL);
	action.sa_handler = reconfig;		/* Set up HUP signal handler */
	sigaction (SIGHUP, &action, NULL);
	action.sa_handler = sig_int;		/* Initialize all AATVs */
	sigaction (SIGINT, &action, NULL);
	action.sa_handler = sig_term;		/* Orderly shutdown */
	sigaction (SIGTERM, &action, NULL);
	action.sa_handler = debug_bump;		/* Increase debugging level */
	sigaction (SIGUSR1, &action, NULL);
	action.sa_handler = debug_off;		/* Disable debugging output */
	sigaction (SIGUSR2, &action, NULL);

	/*
	 * Disable signal processing until we're ready to go.  In particular
	 * we need to prevent update_clients() process from doing child_end()
	 * until we're fully initialized.
	 */
	sigprocmask (SIG_BLOCK, &signals, NULL);

	/* Position RCS revision number at beginning of logversion[] */

	ptr = strchr (RADIUS_VERSION, ' ');	/* Locate first space */
	strcpy (logversion, ptr + 1);	/* Move version number to front */
	strtok (logversion, " ");  /* Terminate string after version number */

	birthdate = time (0);
	if (debug_flag == 0)
	{
		sprintf (errmsg, "%s/%s", radius_dir, RADIUS_PID);
		if ((fp = fopen (errmsg, "w")) == (FILE *) NULL)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: Could not open %s for startup message",
				progname, errmsg);

			if ((file_logging == 1) && (msgfd != (FILE *) NULL))
			{
				fflush (msgfd);
			}

			exit (-6);
		}

		sprintf (errmsg, "%u PID %s version [%s]",
			getpid (), progname, logversion);

		if (inetd)
		{
			strcat (errmsg, " (x)inetd");
		}

		fprintf (fp, "%s %-24.24s\n", errmsg, ctime (&birthdate));
		fclose (fp);

		sprintf (oldpath, "%-24.24s", ctime (&birthdate));
		logit (LOG_DAEMON, LOG_INFO, "Started %s: %s", oldpath, errmsg);
#if !defined(M_UNIX)
		/*
		 * Open system console as stderr
		 */
		cons = open ("/dev/console", O_WRONLY | O_NOCTTY);
		if (cons != 2)
		{
			dup2 (cons, 2);
			close (cons);
		}
#endif	/* M_UNIX */
	}
	else
	{
		fprintf (ddt, "%-24.24s: Debugging turned ON, Level %d\n",
			ctime (&birthdate), debug_flag);
		fprintf (ddt, "%s\n", version ());
		fprintf (ddt, "Program = %s\n", progname);
	}

	if (selecttime != NULL && timeout.tv_sec == 0)
	{
		selecttime = NULL;	/* No time for this! */
	}
	tofmaxdelay = time (0); /* Initialize the time of maximum delay */

	/* Give DNS process a chance to finish before reading requests */
	if (spawn_flag)
	{
		FD_CLR(server_aatv.sockfd, &select_mask);
		alarm (1);
		alarm_set++;
	}

	/*
	 * Loop where we spend the rest of our life.
	 */
	result = 0;
	do
	{
		if (ddt != (FILE *) NULL && debug_flag == 0)
		{
			fprintf (ddt, "Debug turned OFF\n");
			(void) fclose (ddt);
			ddt = (FILE *) NULL;
		}

		if (file_logging == 1)
		{
			if (msgfd != (FILE *) NULL)
			{
				fflush (msgfd);

				/* if debugging, do NOT truncate logfile */
				if (debug_flag == 0)
				{
					trunc_logfile (&msgfd, RADIUS_LOG);
				}
			}
		}

		/* Enable signal processing now */
		sigprocmask (SIG_UNBLOCK, &signals, NULL);

		memcpy ((char *) &readfds, (char *) &select_mask,
							sizeof (readfds));
		selcnt = select (dtablesize, &readfds, NULL, NULL, selecttime);
		if (selcnt == 0)
		{
			logit (LOG_DAEMON, LOG_INFO,
			     "%s: terminated by inactivity timeout (%ld secs.)",
				progname, timeout.tv_sec);
			sig_term (0);
		}
		if (selcnt < 0)
		{
			if (errno == EINTR)
			{
				continue;
			}
			logit (LOG_DAEMON, LOG_ALERT, "select: %s",
				get_errmsg ());
			dumpcore = 1;
			abort ();
		}

		/*
		 * Suspend signal processing while we do our stuff.
		 * This is to prevent possible recursive calls to 
		 * nonreentrant functions.
		 */
		sigprocmask (SIG_BLOCK, &signals, NULL);

		for (i = 0; (aatv = sockfd_tv[i]) != NULL; i++)
		{
			time_t dummy_time;

			select_cur = time (&dummy_time);
			if (select_cur == (time_t) -1)
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: time() error, #%d", func, errno);
			}

			if (FD_ISSET(aatv->sockfd, &readfds))
			{
				len = sizeof (struct sockaddr_in);
				memset ((char *) &fromsin, '\0', len);
				result = recvfrom (aatv->sockfd,
						   (char *) recv_buffer,
						   (int) sizeof (recv_buffer),
						   (int) 0,
						   (struct sockaddr *) &fromsin,
						   & len);
				if (result < 0)
				{
					break;
				}

				if (result > 0) /* Handle received request */
				{
					/* For some event.value to be filled */
					/* in on return from AATV recv call */
					event.state = ST_ANY;
					event.a.aatv = aatv;
					event.isproxy = 0;
					event.xstring[0] = '\0';

					authreq = aatv->recv (& fromsin,
						ntohl (fromsin.sin_addr.s_addr),
						result, & event);
					if (authreq != (AUTH_REQ *) NULL)
					{
						authreq->fsm_aatv = aatv;
						state_machine (event, authreq);
					/* Set alarm if a request is pending */
						if (!alarm_set &&
							(global_acct_q ||
							 global_request_q))
						{
							alarm (1);
							alarm_set++;
						}
					}
				}
			}

			if ((time (0) - select_cur) > select_max)
			{
				tofmaxdelay = time (0);
				select_max = tofmaxdelay - select_cur;
			}
		}
	} while (result >= 0);

	logit (LOG_DAEMON, LOG_ALERT, "recv: %s", get_errmsg ());
	dumpcore = 1;
	abort ();
} /* end of main () */

/*************************************************************************
 *
 *	Function: ack_action
 *
 *	Purpose: Utility AATV which always responds positively.
 *
 *************************************************************************/

static int
ack_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	return EV_ACK;
} /* end of ack_action () */

/*************************************************************************
 *
 *	Function: build_acct_req
 *
 *	Purpose: Build an acct-request structure, fill out the header and
 *		 other values and attach attribute-value pairs as needed.
 *
 *	Returns: a pointer to an authreq data structure.
 *
 *************************************************************************/

AUTH_REQ *
build_acct_req (authreq, status, session_id, time, pairs)

AUTH_REQ       *authreq;		/* optional (may be NULL), so ignore */
int             status;			/* required! */
char           *session_id;		/* optional (may be NULL), so create */
int             time;			/* may be zero */
VALUE_PAIR     *pairs;			/* optional (may be NULL), so ignore */

{
	u_char          save_id;
	int             flag;
	AUTH_REQ       *acctreq;
	AUTH_REQ       *entry;
	VALUE_PAIR     *vp;
	VALUE_PAIR    **prev_ptr;
	FILE           *debugout = stdout;
	void            random_vector PROTO ((u_char *));
	char            str[AUTH_ID_LEN];
	u_char          vector[AUTH_VECTOR_LEN];
	static int      id = -1;
	static char    *func = "build_acct_req";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (ddt != (FILE *) NULL)
	{
		debugout = ddt;
	}

	/*
	 *	Allocate the new accounting data structure.
	 */

	if ((acctreq =
		(AUTH_REQ *) malloc (sizeof (AUTH_REQ))) == (AUTH_REQ *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory", func);
		abort ();
	}

	/*
	 *	Fill header fields
	 */

	acctreq->ipaddr = get_our_addr ();
	acctreq->udp_port = 0;		/* dummy values -- don't care */
	acctreq->id = 0;		/* dummy values -- don't care */
	acctreq->code = PW_ACCOUNTING_REQUEST;
	random_vector (vector);
	memcpy ((char *) acctreq->vector, (char *) vector,
		AUTH_VECTOR_LEN);
	acctreq->secret = "";
	acctreq->ttl = MAX_REQUEST_TIME;
	acctreq->timer = DEFAULT_TIMER_VALUE;
	acctreq->reply_id = 0;
	acctreq->retry_cnt = 0;
	acctreq->file_pfx = "";
	acctreq->realm_filter = "";
	if (authreq != (AUTH_REQ *) NULL) /* Pick up state from where we are. */
	{
		acctreq->state = authreq->state;
	}
	else
	{
		acctreq->state = ST_INIT;
	}
	acctreq->sws = 0;
	acctreq->result = EV_NAK;
	acctreq->cur_count = 0;
	acctreq->fsm_aatv = (AATV *) NULL;
	acctreq->direct_aatv = (AATV *) NULL;
	acctreq->event_q = (EVENT_ENT *) NULL;
	acctreq->next = (AUTH_REQ *) NULL;
	acctreq->request = (VALUE_PAIR *) NULL;
	acctreq->cur_request = (VALUE_PAIR *) NULL;
	acctreq->user_check = (VALUE_PAIR *) NULL;

	if (pairs != (VALUE_PAIR *) NULL)
	{
		dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: pairs list:", func));
		debug_list (debugout, pairs);
	}

	flag = 0;
	if (authreq != (AUTH_REQ *) NULL)
	{
		dprintf(2, (LOG_DAEMON, LOG_DEBUG,
			"%s: authreq->cur_request:", func));
		debug_list (debugout, authreq->cur_request);

		if (get_vp (authreq->cur_request, PW_ACCT_STATUS_TYPE)
							== (VALUE_PAIR *) NULL)
		{
			avpair_add (&acctreq->request, PW_ACCT_STATUS_TYPE,
				    &status, 0);
		}

		if (status == PW_STATUS_MODEM_STOP)
		{
			if (get_vp (authreq->cur_request, PW_ACCT_SESSION_TIME)
							== (VALUE_PAIR *) NULL)
			{
				avpair_add (&acctreq->request,
					    PW_ACCT_SESSION_TIME, &time, 0);
			}
		}

		if ((vp = get_vp (authreq->cur_request, PW_ACCT_SESSION_ID))
							== (VALUE_PAIR *) NULL)
		{
			flag = 1; /* Since no Acct-Session-Id can be present */
		}
	}
	else /* was NULL */
	{
		flag = 1;
		avpair_add (&acctreq->request, PW_ACCT_STATUS_TYPE, &status, 0);
		avpair_add (&acctreq->request, PW_ACCT_SESSION_TIME, &time, 0);
	}

	if (flag == 1) /* This ACCT packet needs an Acct-Session-Id */
	{
		if (session_id == (char *) NULL) /* create unique one */
		{
			sprintf (str, "%08X", ++id);
		}
		else /* use the one given by the caller */
		{
			memcpy (str, session_id, 8);
		}
		avpair_add (&acctreq->request, PW_ACCT_SESSION_ID, str, 8);
	}

	if (pairs != (VALUE_PAIR *) NULL)
	{
		list_copy (&acctreq->request, pairs);
	}

	if ((authreq != (AUTH_REQ *) NULL) &&
			(authreq->cur_request != (VALUE_PAIR *) NULL))
	{
		list_copy (&acctreq->request, authreq->cur_request);
	}

	/* Remove a/v pairs which should never be in ACCT packet (per RFC) */

	for (prev_ptr = &acctreq->request, vp = *prev_ptr;
		vp != (VALUE_PAIR *) NULL;
		vp = *prev_ptr)
	{
		switch (vp->attribute)
		{
		    case PW_PROXY_ACTION:
		    case PW_USER_PASSWORD:
		    case PW_CHAP_PASSWORD:
		    case PW_REPLY_MESSAGE:
		    case PW_STATE:
			/* Remove these from the request list. */
			*prev_ptr = vp->next;
			free (vp);
			break;

		    default:
			/* Leave all others in request list. */
			prev_ptr = &vp->next;
			break;
		}
	}

	list_copy (&acctreq->cur_request, acctreq->request);

	debug_list (debugout, acctreq->cur_request);

	/* Set unique reply-id in the new request */

	save_id = acct_ident;		/* use the one from is_dup_request() */
	do
	{
		entry = global_acct_q;
		acctreq->reply_id = ++acct_ident;
		if (acctreq->reply_id == save_id)	/* have looped */
		{
			break;		/* use ident again, depend on vector */
		}
		for (; entry != (AUTH_REQ *) NULL ; entry = entry->next)
		{
			if (entry->reply_id == acct_ident)
			{
				break;
			}
		}
	} while (entry != (AUTH_REQ *) NULL);

	/* Link new request on global accounting queue */
	acctreq->next = global_acct_q;
	global_acct_q = acctreq;

	return acctreq;
} /* end of build_acct_req () */

/*************************************************************************
 *
 *	Function: call_action
 *
 *	Purpose: Calls the action function specified by the AATV argument.
 *
 *	Returns: The result (return code or "event") of the action function.
 *
 *************************************************************************/

int
call_action (aatv, authreq, value, afpar)

AATV           *aatv;
AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	int             result;
	int             pid;
	static char    *func = "call_action";

	if (aatv == (AATV *) NULL)
	{
		dprintf(2, (LOG_DAEMON, LOG_DEBUG,
			"%s: configuration error: NULL AATV, value %d and '%s'",
			func, value, afpar));
		return EV_FATAL;
	}

	dprintf(2, (LOG_DAEMON, LOG_DEBUG,
		"%s: AATV '%s', type %d, value %d and '%s'", func, aatv->id,
		aatv->aatvfunc_type, value, afpar));

	if (authreq != (AUTH_REQ *) NULL)
	{
		if (aatv != authreq->fsm_aatv)
		{
			if (authreq->fsm_aatv != (AATV *) NULL)
			{
				authreq->direct_aatv = aatv;
			}
			else	/* Need to init fsm_aatv value */
			{
				authreq->fsm_aatv = aatv;
			}
		}
	}

	if (aatv->act_func == (int (*) ()) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: configuration error: NULL action function: %s",
			func, aatv->id);
		return EV_FATAL;
	}

	pid = 0;

	switch (aatv->aatvfunc_type)
	{
	    case AA_DIRECT:
	    case AA_SOCKET:
		result = aatv->act_func (authreq, value, afpar);
		break;

	    case AA_FORK:
	    case AA_FREPLY:
		/*
		 *	If -s option was specified,
		 *	then no child process is started.
		 *	So we wait.
		 */
		if (spawn_flag == 0)
		{
			result = aatv->act_func (authreq, value, afpar);
		}
		else
		{
			if (msgfd != (FILE *) NULL)
			{
				fflush (msgfd);
			}

			if ((pid = (int) fork ()) < 0)
			{
				logit (LOG_DAEMON, LOG_ALERT, "%s: fork: %s",
					func, get_errmsg ());
				result = EV_FATAL;
				break;
			}
			else
			{
				if (pid != 0) /* Parent proc returns */
				{
					result = EV_WAIT;
					break;
				}
			}
/* ---------- CHILD CODE STARTS ----------------------------- */
			/*
			 *	Child process continues with call to
			 *	service new request.
			 *
			 *	Child exits with return code from the
			 *	act_func for given authentication type.
			 */

			result = aatv->act_func (authreq, value, afpar);

			if (msgfd != (FILE *) NULL)
			{
				fflush (msgfd);
			}

			if (aatv->aatvfunc_type == AA_FREPLY)
			{
				/* Wait here to get our reply */

				/* pause (); */
				sleep (90);

				/*
				 *	Will be killed whether we make
				 *	it here or not.
				 */

				/***** NOTREACHED *****/

				_exit (EV_WAIT);
			}

			_exit (result);
/* ---------- CHILD CODE ENDS ----------------------------- */
		} /* end of else (spawn_flag) */
		break;
	} /* end of switch () */

	/* if there is an event pending ... */
	if ((aatv->aatvfunc_type != AA_DIRECT) && (result == EV_WAIT))
	{
		record_event (authreq, aatv, pid, value, afpar);
	}

	return result;
} /* end of call_action () */

/*************************************************************************
 *
 *	Function: child_end
 *
 *	Purpose: Invoked by SIGCHLD signal.  Some authentication requests
 *		 are handled by child processes in order to prevent blocking
 *		 the server process.  This routine handles the termination of
 *		 spawned processes.  A "EVENT_ENT" item corresponding to the
 *		 terminated process is found (which contains the PID of the
 *		 now deceased child).  The saved "state" (at the time of the
 *		 fork() call) along with the "event" (the child's return code)
 *		 are together presented to the state machine as a new "event".
 *		 The exit code of the child indicates the result (event) of
 *		 the action function (EV_NAK, EV_ACK, EV_ERROR, etc.).
 *
 *************************************************************************/

static void
child_end (signo)

int	signo;

{
	char            short_result;
	int             result;
	int             status;
	AUTH_REQ       *authreq;
	AUTH_REQ      **prev;
	EVENT_ENT      *event;
	EVENT_ENT     **prev_event;
	int             pid;
	EV              ev;
	static char    *func = "child_end";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	while ((pid = (int) waitpid ((pid_t) -1, &status, WNOHANG)) > 0)
	{
		result = 0;
		for (prev = &global_acct_q ; /* look on accounting queue */
			(authreq = *prev) != (AUTH_REQ *) NULL ;
			prev = &authreq->next)
		{
			for (prev_event = &authreq->event_q ;
				(event = *prev_event) ;
				prev_event = &event->next)
			{
				if (event->pid == (pid_t) pid)
				{
					result = 1;
					break;
				}
			}
			if (result == 1)
			{
				break;
			}
		}

		if (authreq == (AUTH_REQ *) NULL) /* look on global queue */
		{
			for (prev = &global_request_q ;
				(authreq = *prev) != (AUTH_REQ *) NULL ;
				prev = &authreq->next)
			{
				for (prev_event = &authreq->event_q ;
					(event = *prev_event) ;
					prev_event = &event->next)
				{
					if (event->pid == (pid_t) pid)
					{
						result = 1;
						break;
					}
				}
				if (result == 1)
				{
					break;
				}
			}
		}

		/* If the PID was found, the authreq will be non-NULL */
		if (authreq != (AUTH_REQ *) NULL)
		{
			if (WIFEXITED(status)) /* recover the event */
			{
				short_result = WEXITSTATUS(status);
				dprintf(2, (LOG_DAEMON, LOG_DEBUG,
					"%s: exit status: %X",
					func, short_result));
				if (short_result < 0)
				{
					ack_nak_flag = 0;
				}
				port_msg[0] = '\0';
				result = short_result;
			}
			else /* child died some other (non-normal) way */
			{
				if (authreq->ttl != 0) /* not a timeout */
				{
					logit (LOG_DAEMON, LOG_INFO,
		     "%s: Authtype %d child process terminated abnormally - %X",
			 	 		func,
						event->sub_aatv->authen_type,
						status);
					strcpy (port_msg,
						"RADIUS: Realm process error");
					ack_nak_flag = 0;
					result = EV_NAK;	
				}
				else		/* indicate timeout */
				{
					result = EV_ERROR;
				}
			}

			ev.state = event->state;
			ev.a.aatv = event->fsm_aatv;
			ev.isproxy = 0;
			ev.value = result;
			strcpy (ev.xstring, event->estring);

			/* unlink this EVENT_ENT and free the memory */

			*prev_event = event->next;

			state_machine (ev, authreq);
			free (event);
		}
		else if (pid == dnspid)
		{
			dnspid = 0;
		}
		else /* authreq was null */
		{
			dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				"%s: stray process end", func));
		}
	} /* end of while () */

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: leaving routine", func));

	return;
} /* end of child_end () */

/*************************************************************************
 *
 *	Function: config_initialize
 *
 *	Purpose: intializes configuration values:
 *
 *		 expiration_seconds - When updating a user password,
 *			the amount of time to add to the current time
 *			to set the time when the password will expire.
 *			This is stored as the VALUE Password-Expiration
 *			in the dictionary as number of days.
 *
 *		 warning_seconds - When acknowledging a user authentication,
 *			time remaining for valid password to notify user
 *			of password expiration.
 *
 *************************************************************************/

static int
config_initialize ()

{
	DICT_VALUE     *dval;

	if ((dval = dict_valfind ("Password-Expiration")) == (DICT_VALUE *) NULL)
	{
		expiration_seconds = (UINT4) 0;
	}
	else
	{
		expiration_seconds = dval->value * (UINT4) SECONDS_PER_DAY;
	}

	if ((dval = dict_valfind ("Password-Warning")) == (DICT_VALUE *) NULL)
	{
		warning_seconds = (UINT4) 0;
	}
	else
	{
		warning_seconds = dval->value * (UINT4) SECONDS_PER_DAY;
	}

	return (0);
} /* end of config_initialize () */

/*************************************************************************
 *
 *	Function: debug_bump
 *
 *	Purpose: Increase debugging level.
 *
 *************************************************************************/

static void
debug_bump (sig)

int             sig;

{
	time_t          now;

	if (debug_flag == 0)
	{
		debug_flag++;
		set_debug (1);
	}
	else
	{
		debug_flag++;
	}

	if (debug_flag > 0)
	{
		now = time (0);
		fprintf (ddt, "%-24.24s: Debugging ", ctime (&now));
		if (debug_flag == 1)
		{
			fprintf (ddt, "turned ON,");
		}
		else
		{
			fprintf (ddt, "increased to");
		}
		fprintf (ddt, " Level %d\n", debug_flag);
		fprintf (ddt, "%s\n", version ());
		fprintf (ddt, "Program = %s\n", progname);
	}

	return;
} /* end of debug_bump () */

/*************************************************************************
 *
 *	Function: debug_off
 *
 *	Purpose: Disable debugging output.
 *
 *************************************************************************/

static void
debug_off (sig)

int             sig;

{
	int             cons;

	set_debug (0);
	if (ddt != (FILE *) NULL)
	{
		fclose (ddt);
	}

	/*
	 *	Re-establish system console as stderr.
	 */
	cons = open ("/dev/console", O_WRONLY | O_NOCTTY);
	if (cons != 2)
	{
		dup2 (cons, 2);
		close (cons);
	}

	return;
} /* end of debug_off () */

/*************************************************************************
 *
 *	Function: fatal_log_action
 *
 *	Purpose: This AATV function logs fatal error conditions in the FSM.
 *
 *************************************************************************/

static int
fatal_log_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	static char    *func = "fatal_log_action";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (authreq != (AUTH_REQ *) NULL)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: fatal error detected in FSM: AATV '%s'  state %d",
			func, authreq->fsm_aatv->id, authreq->state);
		authreq->state = ST_END; /* indicates not to enter the FSM */
	}
	else
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: error detected in FSM", func);
	}
	return EV_NAK;
} /* end of fatal_log_action () */

/*************************************************************************
 *
 *	Function: free_authreq
 *
 *	Purpose: Free all chains hanging off an authreq structure, set
 *		 authreq structure to all zeroes and free it too.
 *
 *************************************************************************/

static void
free_authreq (authreq)

AUTH_REQ       *authreq;

{
	AUTH_REQ      **prev;
	AUTH_REQ       *current;
	static char    *func = "free_authreq";

	dprintf(3, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	/* Unlink from global request queue, if it's there */

	if (authreq->code == PW_ACCOUNTING_REQUEST)
	{
		prev = &global_acct_q;
	}
	else /* all other requests go onto the global request queue */
	{
		prev = &global_request_q;
	}

	for ( ; (current = *prev) != (AUTH_REQ *) NULL ; prev = &current->next)
	{
		if (current == authreq)
		{
			*prev = authreq->next;
			break;
		}
	}

	free_event_list (authreq->event_q);
	list_free (authreq->request);
	list_free (authreq->cur_request);
	list_free (authreq->user_check);
	memset ((char *) authreq, '\0', sizeof (AUTH_REQ));
	free (authreq);
	return;
} /* end of free_authreq () */

/*************************************************************************
 *
 *	Function: free_event
 *
 *	Purpose: Free an EVENT_ENT node and (perhaps) kill a child process.
 *
 *************************************************************************/

static void
free_event (event)

EVENT_ENT      *event;

{
	if (event != (EVENT_ENT *) NULL)
	{
		if (event->pid > 0)
		{
			kill (event->pid, SIGKILL);
		}
		if (event->packet != (u_char *) NULL)
		{
			free (event->packet);
		}
		memset ((char *) event, '\0', sizeof (EVENT_ENT));
		free (event);
	}
	return;
} /* end of free_event () */

/*************************************************************************
 *
 *	Function: free_event_list
 *
 *	Purpose: Free all EVENT_ENT nodes on event_q.
 *
 *************************************************************************/

static void
free_event_list (event)

EVENT_ENT      *event;

{
	EVENT_ENT      *next;
	static char    *func = "free_event_list";

	dprintf(3, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	while (event != (EVENT_ENT *) NULL)
	{
		next = event->next;
		free_event (event);
		event = next;
	}

	return;
} /* end of free_event_list () */

/*************************************************************************
 *
 *	Function: get_radrequest
 *
 *	Purpose: Receive UDP client requests, build an auth-request
 *		 structure, and attach attribute-value pairs contained in
 *		 the request to the new structure.
 *
 *	Returns: -1 if errors found,
 *		  0 if normal return.
 *
 *************************************************************************/

static int
get_radrequest (arptr, sin, host, length)

AUTH_REQ      **arptr;
struct sockaddr_in *sin;
UINT4           host;
u_int           length;

{
	u_short         udp_port;
	u_char         *buffer;
	AUTH_HDR       *auth;
	AUTH_REQ       *authreq;
	u_int           totallen;
	static char    *func = "get_radrequest";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	/*
	 * Watch out for malformed requests.
	 */

	if (length < AUTH_HDR_LEN)
	{
		return (-1);
	}

	buffer = (u_char *) recv_buffer;
	udp_port = ntohs (sin->sin_port);
	auth = (AUTH_HDR *) buffer;
	totallen = ntohs (auth->length);

	if (length < totallen ||
		(auth->code != PW_ACCOUNTING_REQUEST
		&& auth->code != PW_ACCESS_REQUEST
		&& auth->code != PW_FORWARDING
		&& auth->code != PW_PASSWORD_REQUEST
		&& auth->code != PW_STATUS_SERVER))
	{
		logit (LOG_DAEMON, LOG_INFO,
		     "%s: invalid request from %s[%u] - %s (type %u), len = %u",
			func, ip_hostname (host), udp_port,
			authtype_toa (auth->code), auth->code, totallen);
		return (-1);
	}

	if (auth->code == PW_FORWARDING)
	{
		*arptr = (AUTH_REQ *) NULL;
		return 0;
	}

	/*
	 * Pre-allocate the new request data structure
	 */

	if ((authreq =
		(AUTH_REQ *) malloc (sizeof (AUTH_REQ))) == (AUTH_REQ *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory", func);
		abort ();
	}

	dprintf(1, (LOG_DAEMON, LOG_DEBUG,
		"%s: Request from %lx (%s[%u]) %s (type %d), id = %d, len = %d",
		func, host, ip_hostname (host), udp_port,
		authtype_toa (auth->code), auth->code, auth->id, totallen));

	/*
	 * Fill header fields
	 */
	authreq->ipaddr = host;
	authreq->udp_port = udp_port;
	authreq->id = auth->id;
	authreq->code = auth->code;
	memcpy ((char *) authreq->vector, (char *) auth->vector,
		AUTH_VECTOR_LEN);
	authreq->secret = "";
	authreq->ttl = MAX_REQUEST_TIME;
	authreq->timer = 0;
	authreq->reply_id = 0;
	authreq->retry_cnt = 0;
	authreq->file_pfx = "";
	authreq->realm_filter = "";
	authreq->state = ST_END;
	authreq->sws = 0;
	authreq->result = EV_NAK;
	authreq->cur_count = 0;
	authreq->fsm_aatv = (AATV *) NULL;
	authreq->direct_aatv = (AATV *) NULL;
	authreq->event_q = (EVENT_ENT *) NULL;
	authreq->next = (AUTH_REQ *) NULL;
	authreq->request = gen_valpairs (auth);  /* put a/v pairs in request */
	authreq->cur_request = (VALUE_PAIR *) NULL;
	authreq->user_check = (VALUE_PAIR *) NULL;
	*arptr = authreq;

	/*
	 *	Verify that Access-Requests have
	 *		either a PW_USER_PASSWORD or a PW_CHAP_PASSWORD
	 *	**AND**
	 *		either a PW_NAS_IP_ADDRESS or a PW_NAS_IDENTIFIER.
	 */

	/* XXX conform with RADIUS DRAFT RFC
	if (missing either User-Password or missing CHAP-Password ||
		missing either NAS-IP-Address or missing NAS-Identifier ||
		missing User-Name ||
		missing NAS-Port ||
		NAS-IP-Address != source IP addr)
	{
		logit (LOG_DAEMON, LOG_ERR,
		      "%s: invalid packet from %s[%u] - %s (type %u), len = %u",
			func, ip_hostname (host), udp_port,
			authtype_toa (auth->code), auth->code, totallen);
		XXXfree_authreq (authreq);
		return (-1);
	}
	XXX fix the above to conform with RFC */

	return (0);
} /* end of get_radrequest () */

/*************************************************************************
 *
 *	Function: get_state
 *
 *	Purpose: Finds the last (nearest the end) PW_PROXY_STATE a/v pair
 *		 in the list of a/v pairs passed to it and if one is found,
 *		 removes it and returns the numeric value of that state.
 *
 *************************************************************************/

static u_char
get_state (vpq)

VALUE_PAIR    **vpq;

{
	long            num;
	u_char          result;
	VALUE_PAIR    **prev_ptr;
	VALUE_PAIR     *vp;
	VALUE_PAIR     *this_vp;
	static char    *func = "get_state";

	dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

	result = ST_END;
	this_vp = get_last_vp (*vpq, PW_PROXY_STATE); /* point to last one */

	if (this_vp != (VALUE_PAIR *) NULL) /* found at least one Proxy-State */
	{
		for (prev_ptr = vpq;
			(vp = *prev_ptr) != (VALUE_PAIR *) NULL;
			prev_ptr = &vp->next)
		{
			if (vp == this_vp) /* this is last Proxy-State */
			{
				num = atol (this_vp->strvalue);
				if (num >= 0 && num <= 255)  /* valid state */
				{
					result = (u_char) num;
				}
				*prev_ptr = this_vp->next; /* take off list */
				free (this_vp);
				break;
			}
		}
	}

	return result;
} /* end of get_state () */

/*************************************************************************
 *
 *	Function: init_aatvs
 *
 *	Purpose: Does the initialization of all AATVs which have
 *		 initialization functions.
 *
 *************************************************************************/

static void
init_aatvs ()

{
	int             i;
	int             j;
	int             n;
	AATV	       *this_aatv;
	AATV	       *seen_aatv;
	void		(*init_func) ();
	static char    *func = "init_aatvs";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	FD_ZERO(&select_mask);		/* clear the bits */
	for (i = j = 0; i < MAX_AATV; i++)
	{	
		if ((this_aatv = *aatv_ptrs[i]) == (AATV *) NULL)
		{
			continue;
		}
		for (n = 0; n < i; n++) /* AATV names must be unique. */
		{
			if ((seen_aatv = *aatv_ptrs[n]) == (AATV *) NULL)
			{
				continue;
			}
			if (strcasecmp ((char *) this_aatv->id,
					(char *) seen_aatv->id) == 0)
			{
				logit (LOG_DAEMON, LOG_ERR,
				"%s: configuration error: AATV names match: %s",
					func, this_aatv->id);
				abort ();
			}
		}
		init_func = this_aatv->init;
		if (init_func != (void (*) () ) NULL) 
		{
			init_func (this_aatv);

			if (msgfd != (FILE *) NULL)
			{
				fflush (msgfd);
			}

			if (((this_aatv->aatvfunc_type == AA_SOCKET) ||
				(this_aatv->aatvfunc_type == AA_FREPLY)) &&
				! FD_ISSET(this_aatv->sockfd, &select_mask))
			{
				FD_SET(this_aatv->sockfd, &select_mask);
				sockfd_tv[j++] = this_aatv;
			}
		}
	}

	sockfd_tv[j] = (AATV *) NULL;

	return;
} /* end of init_aatvs () */

/*************************************************************************
 *
 *	Function: is_dup_request
 *
 *	Purpose: Looks for duplicate client requests on global queues.
 *
 *	Returns: EV_DUP_REQ and found authreq, if request is a duplicate.
 *		 EV_NEW_A*** and NULL authreq, if request is not a duplicate.
 *		 EV_NAK and NULL authreq, if queue is full.
 *
 *************************************************************************/

static AUTH_REQ *
is_dup_request (authreq, event)

AUTH_REQ       *authreq;
int            *event;

{
	u_char          save_id;
	int             qcount = 0;
	UINT4           nas = 0;
	AUTH_REQ       *q;
	VALUE_PAIR     *avp;
	VALUE_PAIR     *vp;
	VALUE_PAIR     *vp2;
	char           *why_dup = "not a duplicate";
	char            pw[AUTH_PASS_LEN];
	char            pw2[AUTH_PASS_LEN];
	static char    *func = "is_dup_request";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	/*
	 * Look for duplicate request.
	 */

	if (authreq->code == PW_ACCOUNTING_REQUEST)
	{
		q = global_acct_q;
	}
	else /* all other requests go onto the global request queue */
	{
		q = global_request_q;
	}

	if ((vp = get_vp (authreq->request, PW_NAS_IP_ADDRESS))
							!= (VALUE_PAIR *) NULL)
	{
		nas = vp->lvalue;
	}

	*pw = 0;	/* Indicate we haven't decrypted new password yet */

	avp = authreq->request; /* Only interested in first a/v pair. */

	for (/* q set above */ ; q != (AUTH_REQ *) NULL ; q = q->next)
	{
		qcount++;

		if (authreq->ipaddr != q->ipaddr ||
			authreq->udp_port != q->udp_port)
		{
			continue;	/* Definitely is NOT a duplicate. */
		}

		/* Check first a/v pair as a quick way to skip some entries. */
		vp2 = q->request;
		if (avp->attribute == vp2->attribute) /* They match. */
		{
			if ((avp->attribute == PW_PROXY_ACTION) &&
				strcmp (avp->strvalue, vp2->strvalue) != 0)
			{
				continue; /* Not a match, not a duplicate. */
			}
		}
		else /* Could not possibly be a duplicate. */
		{
			continue;
		}

		if ((authreq->id == q->id) && /* Definitely IS a duplicate. */
			((nas != 0) && (nas != authreq->ipaddr)))
		{
			why_dup = "id and nas";
			break;		/* Cleanup and return at end of loop. */
		}

		/* Still may be a duplicate, check all a/v pairs to be sure. */
		for (vp = authreq->request; /* vp2 set above */
			vp != (VALUE_PAIR *) NULL && vp2 != (VALUE_PAIR *) NULL;
			vp = vp->next, vp2 = vp2->next)
		{
			if (vp->attribute != vp2->attribute)
			{
				why_dup = "not a dup (attr)";
				break;	/* Definitely is NOT a duplicate. */
			}
			if (vp->type == PW_TYPE_STRING)
			{
				if (strcmp (vp->strvalue, vp2->strvalue) == 0)
				{
					why_dup = "is a dup (string-match)";
					continue;
				}
				if (vp->attribute != PW_USER_PASSWORD ||
					get_passwd (q, pw2, (char *) NULL,
							   (char *) NULL) != 0)
				{
					why_dup = "not a dup (string-mismatch)";
					break; /* This is NOT a duplicate. */
				}
				if (*pw == 0) /* Only decrypt password once. */
				{
					get_passwd (authreq, pw, (char *) NULL,
						    (char *) NULL);
				}
				if (strcmp (pw, pw2) != 0)
				{
					break; /* This is NOT a duplicate. */
				}
				continue;
			}
			if (vp->lvalue != vp2->lvalue)
			{
				why_dup = "not a dup (numeric-mismatch)";
				break;	/* Definitely is NOT a duplicate. */
			}
		} /* end of for (each a/v pair) */

		/* These pointers are both NULL only if lists are identical. */
		if (vp == (VALUE_PAIR *) NULL && vp2 == (VALUE_PAIR *) NULL)
		{
			break; /* End of a/v pairs, therefore IS a duplicate */
		}
	}

	/*
	 * Free temporary memory used to compare User-Password attributes
	 */
	memset (pw, '\0', sizeof (pw));
	memset (pw2, '\0', sizeof (pw2));

	if (q != (AUTH_REQ *) NULL) /* Found a duplicate before reaching end. */
	{
		dprintf(1, (LOG_DAEMON, LOG_DEBUG,
			"RADIUS request %s (type %u) id %u is a duplicate (%s)",
			authtype_toa (authreq->code), authreq->code,
			authreq->id, why_dup));

		if (debug_flag >= 4)
		{
			FILE *debug_out = stderr;

			if (ddt)
			{
				debug_out = ddt;
			}

			/* Show given authreq... */
			if ((vp = get_vp (authreq->request, PW_NAS_IDENTIFIER))
							== (VALUE_PAIR *) NULL)
			{
				vp = get_vp (authreq->request,
						PW_NAS_IP_ADDRESS);
			}

			dprintf(4, (LOG_DAEMON, LOG_DEBUG, 
			  "%s: Incoming %s (type %u) id %u from %s via. %s[%d]",
				func, authtype_toa (authreq->code),
				authreq->code, authreq->id,
				(vp == (VALUE_PAIR *) NULL) ?
						"?" : avpair_vtoa (vp, 0),
				ip_hostname (authreq->ipaddr),
				authreq->udp_port));
			debug_list (debug_out, authreq->request);

			/* Show matching authreq... */
			if ((vp = get_vp (q->request, PW_NAS_IDENTIFIER))
							== (VALUE_PAIR *) NULL)
			{
				vp = get_vp (q->request, PW_NAS_IP_ADDRESS);
			}

			dprintf(4, (LOG_DAEMON, LOG_DEBUG, 
			  "%s: Matching %s (type %u) id %u from %s via. %s[%d]",
				func, authtype_toa (q->code), q->code, q->id,
				(vp == (VALUE_PAIR *) NULL) ?
						"?" : avpair_vtoa (vp, 0),
				ip_hostname (q->ipaddr), q->udp_port));
			debug_list (debug_out, q->request);
		}

		q->id = authreq->id;	/* Record original sequence number. */
		free_authreq (authreq);

		q->ttl = MAX_REQUEST_TIME;
		q->retry_cnt++;		/* Record the duplicate request. */

		*event = EV_DUP_REQ;
		return q;
	}

	/*
	 * Not a duplicate.  Enforce limit on maximum outstanding requests.
	 */

	if (qcount >= MAX_REQUESTS)
	{
		logit (LOG_DAEMON, LOG_INFO,
			"Request denied - too many replies (%u) in queue",
			qcount);
		strcpy (port_msg, "Can't process request now, try again later");
		ack_nak_flag = 0;
		*event = EV_NAK;
		return (AUTH_REQ *) NULL;	/* Rejected */
	}

	/* Set unique reply id in the new request */

	if (authreq->code == PW_ACCOUNTING_REQUEST)
	{
		save_id = acct_ident;
		do
		{
			authreq->reply_id = acct_ident++;
			if (qcount >= 255) /* size of u_char */
			{
				break;
			}

			for (q = global_acct_q ;
				q != (AUTH_REQ *) NULL ;
				q = q->next)
			{
				if (q->reply_id == authreq->reply_id)
				{
					break;
				}
			}
		} while (q != (AUTH_REQ *) NULL && save_id != acct_ident);

		/* Link new request on global accounting queue */
		authreq->next = global_acct_q;
		global_acct_q = authreq;
		*event = EV_NEW_ACCT;
		cur_aq_size = qcount++;
		max_aq_size = MAX(qcount, max_aq_size);
	}
	else /* all other requests look on the global request queue */
	{
		save_id = auth_ident;
		do
		{
			authreq->reply_id = auth_ident++;
			if (qcount >= 255) /* size of u_char */
			{
				break;
			}
			for (q = global_request_q ;
				q != (AUTH_REQ *) NULL ;
				q = q->next)
			{
				if (q->reply_id == authreq->reply_id)
				{
					break;
				}
			}
		} while (q != (AUTH_REQ *) NULL && save_id != auth_ident);

		/* Link new request on global request queue */
		authreq->next = global_request_q;
		global_request_q = authreq;
		*event = EV_NEW_AUTHEN;
		cur_q_size = qcount++;
		max_q_size = MAX(qcount, max_q_size);
	}

	return (AUTH_REQ *) NULL;	/* not a duplicate */
} /* end of is_dup_request () */

/*************************************************************************
 *
 *	Function: kill_action
 *
 *	Purpose: This AATV function unconditionally removes pending events
 *		 (the EVENT_ENT structures attached to the authreq).
 *
 *************************************************************************/

static int
kill_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	static char    *func = "kill_action";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (authreq != (AUTH_REQ *) NULL)
	{
		free_event_list (authreq->event_q);
		authreq->event_q = (EVENT_ENT *) NULL;
	}
	return EV_WAIT;
} /* end of kill_action () */

/*************************************************************************
 *
 *	Function: nak_action
 *
 *	Purpose: Utility AATV which always responds negatively.
 *
 *************************************************************************/

static int
nak_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	return EV_NAK;
} /* end of nak_action () */

/*************************************************************************
 *
 *	Function: null_action
 *
 *	Purpose: Place holder for no action in the FSM (which usually
 *		 results in a return to the engine's main select() loop).
 *
 *************************************************************************/

static int
null_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	return EV_WAIT;
} /* end of null_action () */

/*************************************************************************
 *
 *	Function: pending_action
 *
 *	Purpose: Checks on the event queue of this AUTH_REQ structure.
 *
 *************************************************************************/

static int
pending_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	static char    *func = "pending_action";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (authreq->event_q == (EVENT_ENT *) NULL)
	{
		return EV_ACK;
	}
	else /* something still in the event queue (pending) */
	{
		return EV_WAIT;
	}
} /* end of pending_action () */

/*************************************************************************
 *
 *	Function: protocol_check
 *
 *	Purpose: Perform old style authorization
 *
 *	Returns: EV_ACK or EV_NAK
 *
 *************************************************************************/

static int
protocol_check (authreq, protpair)

AUTH_REQ       *authreq;
VALUE_PAIR    **protpair;

{
	int             count = 0;
	int             result;
	VALUE_PAIR     *vp;
	VALUE_PAIR    **last;
	char           *ptr;
	int             fprot;
	static char    *fuser[] = {"dumbuser", "pppuser", "slipuser",
				"cblogin", "cbframed", "obuser", "admin",
				"execuser", "authonly"};
	static char    *func = "protocol_check";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	result = authreq->result;

	/*
	 * Special check to match protocol configuration returned to that
	 * being used by client.  Individual entries in users can override
	 * this check by explicitly providing a protocol configuration.
	 */
	if (result == EV_ACK)
	{
		fprot = -1;		/* Assume we don't know protocol */
		if ((vp = get_vp (authreq->request, PW_SERVICE_TYPE))
							== (VALUE_PAIR *) NULL)
		{
			fprot = 0; /* assume dumbuser */
		}
		else /* Service-Type provided in request */
		{
			fprot = vp->lvalue;
			if (fprot == PW_LOGIN)
			{
				fprot = 0;
			}
			else
			{
				if (fprot == PW_FRAMED)
				{
					if ((vp = get_vp (authreq->request,
						PW_FRAMED_PROTOCOL))
							!= (VALUE_PAIR *) NULL)
					{
						fprot = vp->lvalue;
					}
				}
			}
		}

		for (vp = authreq->cur_request ;
			vp != (VALUE_PAIR *) NULL ;
			vp = vp->next)
		{
			if (count == authreq->cur_count)
			{
				break; /* vp points to added stuff */
			}
			count++;
		}

		/*
		 *	Set "last" to point to the "next" pointer
		 *	of the actual last cur_request list item.
		 */

		for (last = &authreq->cur_request ;
			*last != (VALUE_PAIR *) NULL ;
			last = &((*last)->next))
		{
			continue;
		}

		if ((fprot >= 0) && ((vp == (VALUE_PAIR *) NULL) ||
			(get_vp (vp, PW_SERVICE_TYPE) == (VALUE_PAIR *) NULL)))
		{
			/*
			 * If appropriate config info isn't in cur_request,
			 * try to find "xxxuser" entry for this protocol.
			 * Forget it if funny entries aren't in "users"
			 * file because we might be a remote auth server
			 * and local server (closest to NAS) should fill
			 * in this info.
			 */

			user_find ((char *) authreq->file_pfx, fuser[fprot],
				   fprot, &vp, last, 0);

			list_free (vp); /* Free any dummy check items */
		}
	}

	*protpair = get_last_vp (authreq->cur_request, PW_FRAMED_PROTOCOL);

	/*
	 * Enforce protocol restrictions.  See if protocol specified in
	 * cur_request is in list of prohibited protocols in user_check.
	 */

	ptr = (char *) NULL;	/* Use as flag on loop exit */
	for (vp = authreq->user_check ;
		(result == EV_ACK) && (vp != (VALUE_PAIR *) NULL) ;
		vp = vp->next)
	{
		if ((vp = get_vp (vp, CI_PROHIBIT)) == (VALUE_PAIR *) NULL)
		{
			break;
		}

		if (*protpair == (VALUE_PAIR *) NULL)  /* was not FRAMED type */
		{
			if (vp->lvalue == PW_DUMB)
			{
				ptr = "Non-framed";
				result = EV_NAK;
			}
		}
		else
		{
			if (vp->lvalue == (*protpair)->lvalue)
			{
				ptr = dict_valget (vp->lvalue,
						   vp->name)->name;
				result = EV_NAK;
			}
		}
	}

	/* Attempt to use prohibited protocol; Tell user */
	if (ptr != (char *) NULL)
	{
		sprintf (port_msg, "%s access is prohibited", ptr);
		ack_nak_flag = 0;
	}

	return result;
} /* end of protocol_check () */

/*************************************************************************
 *
 *	Function: proxy_forwarding
 *
 *	Purpose: Process incoming proxy action forwarding packets.
 *
 *	Returns: authreq with a/v pairs for local case,
 *		 or NULL, if packet was forwarded.
 *
 *************************************************************************/

static AUTH_REQ *
proxy_forwarding (sin, host, len)

struct sockaddr_in *sin;
UINT4           host;
u_int           len;

{
	int           length;
	UINT4         ipaddr;
	u_char       *ptr;
	AUTH_HDR     *auth;
	AUTH_REQ     *areq;
	char         *agent;
	char         *cannonical;
	char         *filter;
	char         *pfx;		/* file prefix from clients file */
	char         *secret;
	int           type;
	struct sockaddr_in lcl_sin;
	char          realm[AUTH_ID_LEN + 1];
	static char  *func = "proxy_forwarding";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	auth = (AUTH_HDR *) recv_buffer;

	if (auth->code != PW_FORWARDING)
	{
		return (AUTH_REQ *) NULL;
	}

	ptr = auth->data;

	if (*ptr == PW_USER_REALM)
	{
		length = *++ptr;
		length = length - 2; /* correct for attribute type and length */
		memcpy (realm, (char *) ++ptr, length);
		realm[length] = '\0';

		/* Check that the second a/v pair is PW_PROXY_ACTION */
		if (*(ptr + length) != PW_PROXY_ACTION)
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: Badly formed packet for realm '%s'",
				func, realm);
			return (AUTH_REQ *) NULL;
		}

		/* Get the "agent" for this realm. */
		if (find_auth_type (realm, 0, "", &type, &agent, &cannonical,
					&filter) < 0)
		{
			logit (LOG_DAEMON, LOG_ERR, "%s: Can't find realm '%s'",
				func, realm);
			return (AUTH_REQ *) NULL;
		}

		/* See if this realm is of type RADIUS */
		if (type == AA_RAD)
		{
			/* See if the "agent" is known to us. */
			if (find_client_by_name (&ipaddr, agent, &secret, &pfx)
									== 0)
			{
				/* XXX: won't handle dual-homed machines */
				if (ipaddr == get_our_addr ()) /* for "us" */
				{
					/* Fake out get_radrequest() */
					auth->code = PW_ACCESS_REQUEST;
					if (get_radrequest (&areq, sin,
								host, len) == 0)
					{
						return areq; /* and a/v pairs */
					}
					else /* get_radrequest() logged error */
					{
						return (AUTH_REQ *) NULL;
					}
				}
				else /* was not for "us" */
				{
					lcl_sin.sin_family = AF_INET;
					lcl_sin.sin_addr.s_addr = htonl(ipaddr);
					lcl_sin.sin_port = htons(llport);
					length = (int) ntohs(auth->length);
					sendto (rad_2rad_aatv->sockfd,
						(char *) auth, length, (int) 0,
						(struct sockaddr *) & lcl_sin,
						sizeof (lcl_sin));
					return (AUTH_REQ *) NULL;
				}
			}
			else /* Could not find any client information. */
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: No client for '%s'", func, agent);
				return (AUTH_REQ *) NULL;
			}
		}
		else /* Realm was not of authentication type RADIUS */
		{
			/* Fake out get_radrequest() */
			auth->code = PW_ACCESS_REQUEST;
			if (get_radrequest (&areq, sin, host, len) == 0)
			{
				return areq; /* We now have a/v pairs */
			}
			else /* Error was logged in get_radrequest() */
			{
				return (AUTH_REQ *) NULL;
			}
		}
	}
	else /* first a/v pair was not PW_USER_REALM */
	{
		return (AUTH_REQ *) NULL; /* silently discard */
	}
} /* end of proxy_forwarding () */

/*************************************************************************
 *
 *	Function: rad_2rad_recv
 *
 *	Purpose: Retrieve RADIUS to RADIUS replies from global recv_buffer,
 *		 find matching local request structure, validate senders
 *		 credentials (using shared secret and reply digest), attach
 *		 reply-items from RADIUS packet to local request structure,
 *		 and return the (modified) identifed request structure and
 *		 the corresponding event type.
 *
 *************************************************************************/

AUTH_REQ *
rad_2rad_recv (sin, from_ipaddr, rcvlen, ev)

struct sockaddr_in *sin;
UINT4           from_ipaddr;
u_int           rcvlen;
EV             *ev;

{
	int             len;
	int             result;
	int             secretlen;
	u_char          code;
	u_char          rcv_id;
	u_char          state;
	char           *hostname;
	char           *pfx;
	char           *s;
	char           *secret;
	AUTH_HDR       *auth = (AUTH_HDR *) NULL;
	AUTH_REQ       *authreq;
	AUTH_REQ       *queue;
	VALUE_PAIR     *vp;
	VALUE_PAIR    **prev_ptr;
	VALUE_PAIR     *reply_items;
	EVENT_ENT      *event;
	EVENT_ENT     **prev_event;
	char            action[AUTH_ID_LEN + 1];
	u_char          md5buf[AUTH_VECTOR_LEN];
	u_char          reply_digest[AUTH_VECTOR_LEN];
	static char    *func = "rad_2rad_recv";
	
	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered, reply from %s[%u]",
		func, inet_ntoa (sin->sin_addr), ntohs (sin->sin_port)));

	ev->a.aatv = rad_any_aatv;
	ev->isproxy = 0;

	code = ((AUTH_HDR *) recv_buffer)->code;
	rcv_id = ((AUTH_HDR *) recv_buffer)->id;

	/*
	 *	Match up the received reply's reply_id with one of the
	 *	existing request structures on the global request queue.
	 */

	if ((code == PW_ACCESS_ACCEPT) || (code == PW_ACCESS_REJECT))
	{
		queue = global_request_q;
	}
	else
	{
		if (code == PW_ACCOUNTING_RESPONSE)
		{
			queue = global_acct_q;
		}
		else
		{
			logit (LOG_DAEMON, LOG_INFO,
			 "%s: Received strange reply to request %u from %s[%u]",
				func, rcv_id, inet_ntoa (sin->sin_addr),
				ntohs (sin->sin_port));
			return (AUTH_REQ *) NULL;
		}
	}

	/* Find sending server in the local database */
	if (find_client (from_ipaddr, &hostname, &secret, &pfx) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR, "%s: server \"%s\" not in %s/%s",
			func, ip_hostname (from_ipaddr), radius_dir,
			RADIUS_CLIENTS);
		return (AUTH_REQ *) NULL;
	}

	result = 0;	/* flag for detecting matching seuuence numbers */
	auth = (AUTH_HDR *) recv_buffer;
	secretlen = strlen (secret);

	for (authreq = queue;
		authreq != (AUTH_REQ *) NULL;
		authreq = authreq->next)
	{
		if (authreq->reply_id != rcv_id)
		{
			continue;
		}
		result++;	/* indicate having matched a sequence number */
		memcpy ((char *) md5buf, (char *) auth->vector,
			AUTH_VECTOR_LEN);
		memcpy ((char *) auth->vector, (char *) authreq->vector,
			AUTH_VECTOR_LEN);
		memcpy (recv_buffer + rcvlen, secret, secretlen);
		md5_calc (reply_digest, recv_buffer, rcvlen + secretlen);
		memset (recv_buffer + rcvlen, 0, secretlen);
		memcpy ((char *) auth->vector, (char *) md5buf,
			AUTH_VECTOR_LEN);
		if (memcmp ((char *) md5buf, (char *) reply_digest,
							AUTH_VECTOR_LEN) == 0)
		{
			break; /* the digests matched! */
		}
	}

	if (authreq == (AUTH_REQ *) NULL) /* nothing found on entire queue */
	{
		if (result >= 1) /* at least had one sequence number match */
		{
			logit (LOG_DAEMON, LOG_INFO,
	      "%s: Invalid reply %s (type %u) digest from %s id %u, %d matches",
				func, authtype_toa (code), code,
				ip_hostname (from_ipaddr), rcv_id, result);
		}
		else /* no sequence numbers matched */
		{
			logit (LOG_DAEMON, LOG_INFO,
	       "%s: Received unexpected reply %s (type %u) id %u from '%s[%u]'",
				func, authtype_toa (code), code, rcv_id,
				ip_hostname (from_ipaddr),ntohs (sin->sin_port));
		}

		/* Add some diagnostics. */
		if (debug_flag >= 4)
		{
			dumpit (LOG_DAEMON, LOG_DEBUG, recv_buffer, rcvlen, 0,
				"%s: got reply %s (type %u) id %u from %s[%u]",
				func, authtype_toa (code), code, rcv_id,
				ip_hostname (from_ipaddr), ntohs(sin->sin_port));
		}

		return authreq;
	}

	dprintf(1, (LOG_DAEMON, LOG_DEBUG,
		"%s: received reply %s (type %u) to RADIUS request %u/%u",
		func, authtype_toa (code), code, authreq->id, rcv_id));

	/*
	 *	Find the matching EVENT_ENT for this authreq which
	 *	recorded the sending of the request which resulted
	 *	in this reply being returned.  The PW_PROXY_STATE a/v
	 *	pair which was added by the request's originator is used
	 *	to locate the correct EVENT_ENT.  For FORK_REPLY types,
	 *	the PID value will be greater than zero.  For SOCKET
	 *	types, the PID will be zero.  For the latter case, we
	 *	need to kill the child having the same PID.  In either
	 *	case, we need the state recorded in the EVENT_ENT and
	 *	then we unlink the EVENT_ENT and free the memory.
	 */

	/* recover the reply items found in the received packet */
	reply_items = gen_valpairs (auth);

	state = get_state (&reply_items);  /* ST_END indicates no state found */

	if (state == ST_END) /* grandfather missing state */
	{
#ifdef MERIT_GRANDFATHER
		dprintf(1, (LOG_DAEMON, LOG_DEBUG,
			"%s: no state found, grab authen event", func));
		for (prev_event = &authreq->event_q ;
			(event = *prev_event) != (EVENT_ENT *) NULL ;
			prev_event = &event->next)
		{
			if (event->fsm_aatv == rad_authen_aatv)
			{
				dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				  "%s: event [%d '%s' '%s'  PID = %d  %d '%s']",
					func, event->state, event->fsm_aatv->id,
					event->sub_aatv->id, event->pid,
					event->evalue, event->estring));
				ev->state = event->state;
				ev->a.aatv = event->fsm_aatv;
				ev->isproxy = 0;
				strcpy (ev->xstring, event->estring);
				*prev_event = event->next;
				free_event (event);

				/* Remove caller's last Proxy-State */
				(void) get_state (&authreq->cur_request);

				break;
			}
		}
#else	/* MERIT_GRANDFATHER */
		logit (LOG_DAEMON, LOG_ERR,
			"%s: Missing reply state from %s[%u]", func,
			ip_hostname (from_ipaddr), ntohs (sin->sin_port));
		list_free (reply_items);
		return (AUTH_REQ *) NULL;
#endif  /* MERIT_GRANDFATHER */
	}
	else /* find the correct EVENT_ENT by matching the state we now have */
	{
		if ((vp = get_vp (reply_items, PW_PROXY_ACTION))
							== (VALUE_PAIR *) NULL)
		{
			dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				"%s: missing Proxy-Action, using first match",
				func));
			strcpy (action, (char *) rad_authen_aatv->id);
		}
		else
		{
			len = MIN(strlen (vp->strvalue), AUTH_ID_LEN);
			strncpy (action, vp->strvalue, len);
			action[len] = '\0';
		}

		for (prev_event = &authreq->event_q ;
			(event = *prev_event) != (EVENT_ENT *) NULL ;
			prev_event = &event->next)
		{
			if ((event->action == (char *) NULL) ||
				(event->action[0] == '\0'))
			{
				s = (char *) event->fsm_aatv->id;
			}
			else
			{
				s = event->action;
			}

			if ((event->state == state) && strcmp (s, action) == 0)
			{
				dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				  "%s: event [%d '%s' '%s'  PID = %d  %d '%s']",
					func, event->state, event->action,
					event->sub_aatv->id, event->pid,
					event->evalue, event->estring));
				ev->state = event->state;
				ev->a.aatv = event->fsm_aatv;
				ev->isproxy = 0;
				strcpy (ev->xstring, event->estring);
				*prev_event = event->next;
				free_event (event);
				break;
			}
		}
	}

	if (event == (EVENT_ENT *) NULL)
	{
		logit (LOG_DAEMON, LOG_INFO,
       "%s: Received unexpected reply %s (type %u) to request %u from '%s[%u]'",
			func, authtype_toa (code), code, rcv_id,
			ip_hostname (from_ipaddr), ntohs (sin->sin_port));
		list_free (reply_items);
		return (AUTH_REQ *) NULL;
	}

	for (prev_ptr = &reply_items, vp = *prev_ptr;
		vp != (VALUE_PAIR *) NULL;
		vp = *prev_ptr)
	{
		switch (vp->attribute)
		{
		    case PW_USER_PASSWORD:
			/* Remove any Password items in the reply_items list */
			*prev_ptr = vp->next;
			free (vp);
			break;

		    default:
			/* Leave all other items in reply_items list */
			prev_ptr = &vp->next;
		}
	}

	if (reply_items != (VALUE_PAIR *) NULL)
	{
		/*
		 * Replace current set of cur_request value_pairs
		 * with those contained in the RADIUS reply packet.
		 */
		if (state != ST_END) /* free items if reply has state */
		{
			list_free (authreq->cur_request);
			authreq->cur_request = reply_items;
		}
		else /* for older version compatibility, just add 'em on */
		{
			list_cat (&authreq->cur_request, reply_items);
		}
	}

	switch (code)
	{
	    case PW_ACCESS_ACCEPT:
	    case PW_ACCOUNTING_RESPONSE:
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: success", func));

		result = EV_ACK;
		break;

	    case PW_ACCESS_REJECT:
		dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: failure", func));

	    /**** FALLTHROUGH ****/

	    default:
		result = EV_NAK;
	}

	port_msg[0] = '\0';

	ev->value = result;
	return authreq;
} /* end of rad_2rad_recv () */

/*************************************************************************
 *
 *	Function: rad_init
 *
 *	Purpose: Open a socket for sending to and receiving from NAS's.
 *		 Check if there's a socket on file descriptor zero.  If so,
 *		 check the socket port number.  If it is not ours, get and
 *		 bind the client request socket ourself.  Otherwise, use
 *		 this file descriptor for the client request socket.  If
 *		 file descriptor zero is not a socket, get and bind the
 *		 client request socket ourself.
 *
 *************************************************************************/

static void
rad_init (aatv)

AATV   *aatv;

{
	struct sockaddr_in lclsin;
	int             lcllen;
	int             sockfd = 0; /* Where listen for RADIUS requests */
	static char    *func = "rad_init";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (aatv->sockfd == -1)
	{
		lcllen = sizeof (lclsin);
		memset ((char *) &lclsin, '\0', lcllen);
		if ((getsockname (sockfd, (struct sockaddr *) &lclsin,
								&lcllen) < 0) ||
			lclsin.sin_port != htons(lport))
		{
			sockfd = setupsock (&lclsin, lport);
		}
		else
		{
			inetd++;	/* indicate automatic invocation */
		}
		aatv->sockfd = sockfd;
	}
	return;
} /* end of rad_init () */

static AATV	radipc_aatv =
		{
			"RADDNS",
			-1,
			AA_SOCKET,
			rad_ipc_init,
			NULL,
			NULL,
			rad_ipc_recv,
			NULL,
			-1
		};

AATVPTR         rad_ipc_aatv = & radipc_aatv;

/*************************************************************************
 *
 *      Function: rad_ipc_init
 *
 *      Purpose: Initialize the IPC socket used for resolving DNS names.
 *
 *************************************************************************/

static void
rad_ipc_init (aatv)

AATV           *aatv;

{
        struct sockaddr_in lclsin;
        static char    *func = "rad_ipc_init";

        dprintf(2, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));

        if (aatv->sockfd == -1)
        {
                aatv->sockfd = setupsock (&lclsin, 0);
		rad_ipc_port = ntohs(lclsin.sin_port);
        }
        return;
} /* end of rad_ipc_init () */

/*************************************************************************
 *
 *      Function: rad_ipc_recv
 *
 *      Purpose: Process received DNS updates for clients database.
 *
 *************************************************************************/

static AUTH_REQ *
rad_ipc_recv (sin, from_ipaddr, rcvlen, ev)

struct sockaddr_in *sin;
UINT4           from_ipaddr;
u_int           rcvlen;
EV             *ev;

{
	static char     *func = "rad_ipc_recv";

	dprintf(4, (LOG_AUTH, LOG_DEBUG, "%s: entered", func));
	 
	switch (recv_buffer[0])
	{

	    case (0):	/* DNS reply (check that sender is us) */
		dns_recv (sin, from_ipaddr, rcvlen);	
		break;

#if defined(MERIT_NASMAN) 
	    case (1):	/* NAS Manager reply */
		nas_man_recv (sin, from_ipaddr, rcvlen);	
		break;
#endif	/* MERIT_NASMAN */

	    default:
		logit (LOG_DAEMON, LOG_INFO, "%s: from %s - Invalid code",
			func, ip_hostname (from_ipaddr));
		break;
	}
	return (AUTH_REQ *) NULL;

} /* end of rad_ipc_recv () */

/*************************************************************************
 *
 *	Function: rad_recv
 *
 *	Purpose: Receive RADIUS client UDP requests, build an auth-request
 *		 structure, attach attribute-value pairs contained in the
 *		 request to the new structure, determine if this request
 *		 is a duplicate, if not, assign a unique identifier code,
 *		 and generate the appropriate EV_NEW_*** type event, and
 *		 otherwise generate an EV_DUP_REQ event.
 *
 *	Valid incoming requests are:
 *
 *	PW_ACCESS_REQUEST - Authentication request from a network
 *				client (a NAS, RADIUS server or a program).
 *
 *	PW_STATUS_SERVER - Management Poll to see if we're (still) alive.
 *
 *	PW_FORWARDING - Forward (possibly) this information to someone.
 *
 *	PW_PASSWORD_REQUEST - User request to change a password.
 *
 *	PW_ACCOUNTING_REQUEST - Accounting packets from client.
 *
 *************************************************************************/

AUTH_REQ *
rad_recv (sin, host, length, ev)

struct sockaddr_in *sin;
UINT4           host;
u_int           length;
EV             *ev;

{
	int           result;
	AUTH_REQ     *authreq;
	AUTH_REQ     *candidate;
	char         *file_pfx; /* Source specific file prefix: user/authfile */
	char         *hostname;
	char         *nasname;
	char         *secret;
	VALUE_PAIR   *vp;
	static char  *func = "rad_recv";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	port_msg[0] = '\0';

	/* collect the received packet */

	if (get_radrequest (&authreq, sin, host, length) < 0)
	{
		return (AUTH_REQ *) NULL;
	}

	if (authreq == (AUTH_REQ *) NULL) /* was PW_FORWARDING */
	{
		if ((authreq = proxy_forwarding (sin, host, length))
							== (AUTH_REQ *) NULL)
		{
			return authreq;
		}
		else /* We have a real authreq of type PW_ACCESS_REQUEST now. */
		{
			authreq->state = ST_INIT; /* set initial state */

			/*
			 * Strip off first a/v pair (PW_USER_REALM) leaving
			 * Proxy-Action as first a/v pair on request list.
			 */

			vp = authreq->request;
			authreq->request = vp->next;
			vp->next = (VALUE_PAIR *) NULL;
			list_free (vp);

			list_copy (&authreq->cur_request, authreq->request);

			/*
			 * Grab Proxy-Action and hang off event.a.proxy.
			 */

			vp = authreq->request;
			ev->a.proxy = vp->strvalue;
			ev->isproxy = 1;
			ev->value = EV_ACK;
			return authreq;
		}
	}

	authreq->state = ST_INIT; /* new requests get initial state */

#ifdef MERIT_GRANDFATHER
	/* XXX: grandfather old style PW_AUTHENTICATION_ONLY to new style */

	if ((authreq->code == PW_ACCESS_REQUEST) &&
		((vp = get_vp (authreq->request, PW_SERVICE_TYPE))
						!= (VALUE_PAIR *) NULL &&
		vp->lvalue == PW_OUTBOUND_USER))
	{
		vp->lvalue = PW_AUTHENTICATE_ONLY;
	}

	/* XXX: grandfather old style MANAGEMENT_POLL to new STATUS SERVER */

	if ((authreq->code == PW_ACCESS_REQUEST) &&
		((vp = get_vp (authreq->request, PW_SERVICE_TYPE))
						!= (VALUE_PAIR *) NULL &&
		vp->lvalue == PW_ADMINISTRATIVE_USER)) /* PW_MANAGEMENT_POLL */
	{
		authreq->code = PW_STATUS_SERVER;
	}
#endif	/* MERIT_GRANDFATHER */

	if (authreq->code == PW_STATUS_SERVER) /* Use a well known "secret" */
	{
		authreq->secret = MGMT_POLL_SECRET;
		authreq->file_pfx = "";
	}
	else /* Get shared secret and prefix of this client from clients file */
	{
		if (find_client (authreq->ipaddr, &hostname, &secret,
								&file_pfx) != 0)
		{
			logit (LOG_DAEMON, LOG_INFO,
				"%s: Request from unknown client %s",
				func, ip_hostname (authreq->ipaddr));
			free_authreq (authreq);
			return (AUTH_REQ *) NULL;
		}
		authreq->secret = secret;
		authreq->file_pfx = file_pfx;
	}

	/* See if this request is a duplicate.  The possible result values */
	/* are one of: EV_NEW_AUTHEN, EV_NEW_ACCT, EV_NAK or EV_DUP_REQ. */

	candidate = is_dup_request (authreq, &result);

	if (candidate != (AUTH_REQ *) NULL) /* non-NULL => duplicate found */
	{
		authreq = candidate; /* Just use the old request (duplicate). */
	}
	else /* Copy the request a/v pairs on cur_request for non-duplicate. */
	{
		list_copy (&authreq->cur_request, authreq->request);

		if (authreq->code != PW_STATUS_SERVER)
		{
			if ((vp = get_vp (authreq->request, PW_USER_NAME))
							== (VALUE_PAIR *) NULL)
			{
				vp = get_vp (authreq->request,
							PW_NAS_IP_ADDRESS);
				nasname = (vp == (VALUE_PAIR *) NULL) ? NULL
						: ip_hostname (vp->lvalue);
				missing_attribute (authreq, func,
						   PW_USER_NAME, NULL);
				if (authreq->code != PW_ACCOUNTING_REQUEST)
				{
					free_authreq (authreq);
					return (AUTH_REQ *) NULL;
				}
			}
			else /* Copy into PW_USER_ID a/v pair for later. */
			{
				if (get_vp (authreq->cur_request, PW_USER_ID)
							== (VALUE_PAIR *) NULL)
				{
					avpair_add (&authreq->cur_request,
						    PW_USER_ID, vp->strvalue,
						    AUTH_ID_LEN);
				}
			}

			if (get_vp (authreq->cur_request, PW_NAS_IDENTIFIER)
						== (VALUE_PAIR *) NULL &&
			   (vp = get_vp (authreq->request, PW_NAS_IP_ADDRESS))
						!= (VALUE_PAIR *) NULL)
			{
				avpair_add (&authreq->cur_request,
					    PW_NAS_IDENTIFIER,
					    ip_hostname (vp->lvalue), 0);
			}

		}

#ifdef STATE_IN_REQUEST	/* Access-Challenge not supported for ASCEND NASes. */
		/*
		 * Flag as EV_RE_ACCESS if this new request contains a PW_STATE
		 * attribute-value pair, since it used to be Access-Challenge.
		 */
		if ((vp = get_vp (authreq->request, PW_STATE))
							!= (VALUE_PAIR *) NULL)
		{
			result = EV_RE_ACCESS;
		}
#endif	/* STATE_IN_REQUEST */

		/* Count the a/v pairs on cur_request. */

		for (vp = authreq->cur_request;
			vp != (VALUE_PAIR *) NULL;
			vp = vp->next)
		{
			authreq->cur_count++;
		}
	}

	ev->value = result;
	ev->isproxy = 0;

	/* skip "if" statement for EV_NAK or EV_DUP_REQ events */
	if (result != EV_DUP_REQ && result != EV_NAK)
	{
		ev->isproxy = 1;
		ev->value = EV_ACK;
		switch (authreq->code)
		{
		    case PW_PASSWORD_REQUEST:
			ev->a.proxy = EN_NEW_PASSWD;
			break;

		    case PW_ACCESS_REQUEST:
			if (((vp = get_vp (authreq->request, PW_SERVICE_TYPE))
						!= (VALUE_PAIR *) NULL) &&
				vp->lvalue == PW_AUTHENTICATE_ONLY)
			{
				ev->a.proxy = EN_AUTH_ONLY;
				break;
			}
#if defined(MERIT_HUNTGROUP) || defined(MERIT_LAS) || defined(MERIT_OAS)
			else
			{
				if ((vp = get_vp (authreq->request,
						  PW_PROXY_ACTION))
							!= (VALUE_PAIR *) NULL)
				{
					SAR_FROM_PROXY(authreq);
					ev->a.proxy = vp->strvalue;
					break;
				}
			}
#endif	/* MERIT_HUNTGROUP */
			ev->a.proxy = EN_NEW_AUTHEN;
			break;

		    case PW_ACCOUNTING_REQUEST:
#if defined(MERIT_HUNTGROUP) || defined(MERIT_LAS) || defined(MERIT_OAS)
			if ((vp = get_vp (authreq->request, PW_PROXY_ACTION))
							!= (VALUE_PAIR *) NULL)
			{
				SAR_FROM_PROXY(authreq);
				ev->a.proxy = vp->strvalue;
				break;
			}
#endif	/* MERIT_HUNTGROUP */
			ev->a.proxy = EN_NEW_ACCT;
			break;

		    case PW_STATUS_SERVER:

			/*
			 *	Allow management poll from anywhere.
			 *	Check to see if we're (still) alive.
			 */

			/*
			 *	Side effect -- update files:
			 *	(first arg == zero) => do not initialize
			 */
			stat_files (0, cache_users);

			ev->a.proxy = EN_MGT_POLL;
			break;

		    default: /* Don't know what this is! */
			logit (LOG_DAEMON, LOG_ERR,
				"%s: FATAL, unknown code from [%s], %d", func,
				ip_hostname (authreq->ipaddr), authreq->code);
			ev->a.proxy = EN_FATAL;
			ev->value = EV_FATAL;
			break;

		} /* end of switch */
	} /* end of if */

	return authreq;
} /* end of rad_recv () */

/*************************************************************************
 *
 *	Function: rad_reply
 *
 *	Purpose: Generates and sends reply back to client based on
 *		 result code.  A reply is not sent to client if a
 *		 timeout occurred on our request to a remote server.
 *
 *************************************************************************/

static int
rad_reply (authreq, value, af_param)

AUTH_REQ       *authreq;
int             value;
char           *af_param;

{
	u_short         portnum;
	int             result = 0;
	int             sockfd;
	VALUE_PAIR     *vp;
	VALUE_PAIR     *save_cur = (VALUE_PAIR *) NULL;
	VALUE_PAIR     *namepair;
	VALUE_PAIR     *protpair;
	VALUE_PAIR     *check_item;
	char           *get;
	char           *put;
	char            astring[64];
	char           *packet;
	char           *type;
	char            failed_message[2048]; /* room for > sixteen messages */
	static char    *reply[] = {"FAILED", "OK", "NOREPLY", "OTHER"};
	static char    *func = "rad_reply";

	if (authreq == (AUTH_REQ *) NULL)
	{
		return EV_NAK;
	}

	free_event_list (authreq->event_q); /* free up any remaining events */
	authreq->event_q = (EVENT_ENT *) NULL;
	authreq->timer = 0;

	sockfd = rad_server_aatv->sockfd;
	if (authreq->code == PW_ACCESS_REQUEST)
	{
		if ((vp = get_vp (authreq->request, PW_PROXY_ACTION))
							== (VALUE_PAIR *) NULL)
		{
			packet = "Authentication";
		}
		else
		{
			packet = vp->strvalue;
		}
		result = protocol_check (authreq, &protpair);
	}
	else
	{
		result = authreq->result;
		protpair = (VALUE_PAIR *) NULL;
		if (authreq->code == PW_ACCOUNTING_REQUEST)
		{
			if ((vp = get_vp (authreq->request,
				  PW_ACCT_STATUS_TYPE)) != (VALUE_PAIR *) NULL)
			{
				switch (vp->lvalue)
				{
				    case PW_STATUS_START:
					type = "Start";
					break;
				    case PW_STATUS_STOP:
					type = "Stop";
					break;
				    case PW_STATUS_ALIVE:
					type = "Alive";
					break;
				    case PW_STATUS_MODEM_START:
					type = "Modem-Start";
					break;
				    case PW_STATUS_MODEM_STOP:
					type = "Modem-Stop";
					break;
				    case PW_STATUS_CANCEL:
					type = "Cancel";
					break;
				    default:
					type = "";
				}
			}
			else
			{
				type = "";
			}
			if ((vp = get_vp (authreq->request, PW_PROXY_ACTION))
							== (VALUE_PAIR *) NULL)
			{
				 packet = "Accounting";
			}
			else
			{
				packet = vp->strvalue;
			}
			sockfd = rad_acct_aatv->sockfd;
		}
		else
		{
			if (authreq->code == PW_PASSWORD_REQUEST)
			{
				packet = "Passwd";
			}
			else /* was PW_STATUS_SERVER */
			{
				packet = "Status Server";
			}
		}
	}

	if ((result == EV_ACK) || (authreq->code == PW_ACCOUNTING_REQUEST))
	{	/* It's a go! */
		if (authreq->realm_filter[0]) /* filter specified in authfile */
		{
			if ((check_item = get_last_vp (authreq->cur_request,
					          PW_FILTER_ID))
							== (VALUE_PAIR *) NULL)
			{ /* Generate filter value-pair */
				astring[0] = '\0';
				if ((check_item =
					avpair_add (&authreq->cur_request,
						    PW_FILTER_ID, astring, 0))
							== (VALUE_PAIR *) NULL)
				{
					logit (LOG_DAEMON, LOG_ERR,
						"%s: problem adding filter",
						func);
					dumpcore = 1;
					abort ();
				}
			}
			strcpy (check_item->strvalue,
				(char *) authreq->realm_filter);
		}
		if (authreq->request != ((VALUE_PAIR *) NULL) &&
			authreq->request->attribute != PW_PROXY_ACTION)
		{ /* was NAS */
			save_cur = authreq->cur_request;
			authreq->cur_request = (VALUE_PAIR *) NULL;
			list_copy (&authreq->cur_request, save_cur);

#ifdef ASCEND
			prune_pairs (authreq, &mixup_list, result);
#else
			prune_pairs (authreq, &liv_list, result);
#endif	/* ASCEND */

		}
		send_reply (packet, result, authreq, sockfd);
		if (save_cur != (VALUE_PAIR *) NULL)
		{
			list_free (authreq->cur_request);
			authreq->cur_request = save_cur;
		}
	}
	else /* was failure, challenge or error */
	{
		/* On failure, log protocol from original user request */
		protpair = get_vp (authreq->request, PW_FRAMED_PROTOCOL);

		/*
		 *	A postive result indicates a password failure
		 *	which may be due to a lost packet.  The client
		 *	should retry -- don't issue a reject yet.
		 */
		if (result == EV_NAK || result == EV_ACC_CHAL)
		{
			if (authreq->request != ((VALUE_PAIR *) NULL) &&
				authreq->request->attribute != PW_PROXY_ACTION)
			{ /* was NAS */
				save_cur = authreq->cur_request;
				authreq->cur_request = (VALUE_PAIR *) NULL;
				list_copy (&authreq->cur_request, save_cur);

#ifdef ASCEND
				prune_pairs (authreq, &mixup_list, result);
#else
				prune_pairs (authreq, &liv_list, result);
#endif	/* ASCEND */

			}
			send_reply (packet, result, authreq, sockfd);
			if (save_cur != (VALUE_PAIR *) NULL)
			{
				list_free (authreq->cur_request);
				authreq->cur_request = save_cur;
			}
		}
	}

	/* retrieve user name again */
	namepair = get_vp (authreq->request, PW_USER_NAME);

	/*
	 * On success, log protocol from authreq->cur_request
	 * ("protpair" was already set to protocol found in reply items)
	 */

	/* Log out the results of this request */

	if ((vp = get_vp (authreq->request, PW_NAS_PORT))
							== (VALUE_PAIR *) NULL)
	{
		portnum = 0;
	}
	else
	{
		portnum = (u_short) vp->lvalue;
	}

	if (result > EV_ERROR)
	{
		result = 3;
	}
	else
	{
		result++;
	}

	if ((authreq->code == PW_ACCESS_REQUEST) ||
		(authreq->code == PW_ACCOUNTING_REQUEST) ||
		(authreq->code == PW_PASSWORD_REQUEST))
	{
		astring[0] = '\0';
		if (authreq->retry_cnt > 0)
		{
			sprintf (astring, " (%d retries)",
				authreq->retry_cnt);
		}

		if (result == 0) /* indicates failure, now */
		{
			put = failed_message;
			for (vp = authreq->cur_request;
				vp != (VALUE_PAIR *) NULL;
				vp = vp->next)
			{
				if (vp->attribute == PW_REPLY_MESSAGE)
				{
					for (get = vp->strvalue; *get; get++)
					{
						if (put == failed_message)
						{
							*put++ = ' ';
						}

						/* Make ctrl chars printable. */
						if (*get < ' ')
						{
							*put++ = '^';
							*put++ = *get + 'A' - 1;
						}
						else
						{
							*put++ = *get;
						}
					}
				}
			}

			if ((port_msg != NULL) && (strlen (port_msg) > 0))
			{
				for (get = port_msg; *get; get++)
				{
					if (put == failed_message)
					{
						*put++ = ' ';
					}

					/* Make ctrl chars printable. */
					if (*get < ' ')
					{
						*put++ = '^';
						*put++ = *get + 'A' - 1;
					}
					else
					{
						*put++ = *get;
					}
				}
			}

			*put = '\0';  /* Terminate the string. */
		}
		else /* was not failure */
		{
			failed_message[0] = '\0';
		}

		vp = get_vp (authreq->cur_request, PW_NAS_IDENTIFIER);

		/* Not an error, but a way to log it */
		if (!TAR_NO_LOG(authreq))
		{
			logit (LOG_DAEMON, LOG_INFO,
			    "%s: %u/%u '%s' via %s from %s port %u %s - %s%s%s",
				packet, authreq->id, authreq->reply_id,
				namepair ? namepair->strvalue : "?reboot?",
				ip_hostname (authreq->ipaddr),
				(vp == (VALUE_PAIR *) NULL) ?
					"?" : vp->strvalue,
				portnum,
				(authreq->code == PW_ACCOUNTING_REQUEST) ?
					type : type_string (authreq, protpair),
				reply[result], astring, failed_message);
		}
	}

	if ((authreq->ttl = CLEANUP_DELAY) == 0)
	{
		authreq->ttl = 1; /* must have non-zero timeout value */
	}

	SAR_NO_LOG(authreq);	/* no logging of duplicates in hold state */
	last_send_len = 0;	/* forces redo_action() to use act_func() */

	return EV_WAIT;
} /* end of rad_reply () */

/*************************************************************************
 *
 *	Function: rad_reply_init
 *
 *	Purpose: Perform RADIUS Reply initialization.
 *
 *************************************************************************/

static void
rad_reply_init (aatv)

AATV   *aatv;

{
	static char    *func = "rad_reply_init";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	aatv->sockfd = rad_server_aatv->sockfd;

	return;
} /* end of rad_reply_init () */

/*************************************************************************
 *
 *	Function: radius_send
 *
 *	Purpose: Send a RADIUS request packet to a remote RADIUS server.
 *
 *	Returns: length of packet sent, if successful,
 *		 -1, if error detected.
 *
 *************************************************************************/

int 
radius_send (cmd, code, dns_name, authreq, socket)

char           *cmd;
u_int           code;
char           *dns_name;
AUTH_REQ       *authreq;
int             socket;

{
	UINT4              remote_ipaddr;
	UINT4              lvalue;
	int                i;
	int                len;
	int                secretlen;
	int                total_length;
	u_short            svc_port;
	u_char            *ptr;
	char              *pfx;
	char              *secret;
	AUTH_HDR          *auth;
	DICT_ATTR         *attr;
	VALUE_PAIR        *vp;
	VALUE_PAIR        *pw_vp;
	char               passwd[AUTH_PASS_LEN + 1];
	u_char             passbuf[AUTH_PASS_LEN];
	u_char             md5buf[AUTH_PASS_LEN + AUTH_VECTOR_LEN];
	char               buffer[256];
	static char       *func = "radius_send";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	/* Find remote RADIUS server in the database */
	if (find_client_by_name (&remote_ipaddr, dns_name, &secret, &pfx) != 0)
	{
		logit (LOG_DAEMON, LOG_ERR,
			"%s: server '%s' not in %s/%s",
			func, dns_name, radius_dir, RADIUS_CLIENTS);
		return (-1);
	}

	/* See if relaying this request might start a looping situation */
	if ((debug_flag == 0) &&
	    (authreq->ipaddr == remote_ipaddr)) /* then why ask your asker? */
	{
		logit (LOG_DAEMON, LOG_ERR,
		   "%s: remote server '%s (%s)' matches requestor's address %s",
			 func, dns_name, ip_hostname (remote_ipaddr),
			 ip_hostname (authreq->ipaddr));
		return (-1);
	}

#if defined(MERIT_HUNTGROUP) || defined(MERIT_LAS) || defined(MERIT_OAS)
	if ((cmd != (char *) NULL) && (*cmd != '\0'))
	{
		/*
		 *	When relaying a request to a remote server, attach
		 *	the "cmd" (usually the action name from the FSM table)
		 *	as first attribute-value pair of type PW_PROXY_ACTION.
		 */

		if ((vp = (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							== (VALUE_PAIR *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT,
				"%s: FATAL out of memory", func);
			abort ();
		}
		attr = dict_attrget (PW_PROXY_ACTION);
		strcpy (vp->name, attr->name);
		vp->attribute = attr->value;
		vp->type = attr->type;
		strcpy (vp->strvalue, cmd);
		vp->next = authreq->cur_request;
		authreq->cur_request = vp;  /* place at start of cur_request */
		strcpy (last_send_action, cmd); /* cmd name for reply match */
	}
	else	/* No command */
	{
		last_send_action[0] = '\0';
	}
#else
	last_send_action[0] = '\0';
#endif	/* MERIT_HUNTGROUP */

	/*
	 *	Add the PW_PROXY_STATE attribute-value pair on the end
	 *	of the request as the last a/v pair in the packet.
	 *	This allows the receiver to add reply items after the
	 *	state, there-by delimiting the request from the reply.
	 *	Also, the state is needed upon receipt to help match
	 *	up incoming replies with existing request structures.
	 */

	if ((vp = (VALUE_PAIR *) malloc (sizeof (VALUE_PAIR)))
							== (VALUE_PAIR *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory", func);
		abort ();
	}
	attr = dict_attrget (PW_PROXY_STATE);
	strcpy (vp->name, attr->name);
	vp->attribute = attr->value;
	vp->type = attr->type;
	vp->next = (VALUE_PAIR *) NULL;
	sprintf (buffer, "%d", authreq->state);
	strcpy (vp->strvalue, buffer);
	insert_vp (&authreq->cur_request, (VALUE_PAIR *) NULL, vp); /* at end */

	/* Get the port to use to send RADIUS packets to remote RADIUS host */
	if (code == PW_ACCOUNTING_REQUEST)
	{
		svc_port = aaport;
	}
	else /* was Access-Request */
	{
		svc_port = llport;
	}

	/* Start to build the RADIUS protocol packet */
	auth = (AUTH_HDR *) send_buffer;
	auth->code = (u_char) code;
	auth->id = authreq->reply_id;
	memcpy ((char *) auth->vector, (char *) authreq->vector,
		AUTH_VECTOR_LEN);

	dprintf(1, (LOG_DAEMON, LOG_DEBUG,
	"Relaying RADIUS request of id %d now %d from %lx (%s) to %s (type %u)",
		authreq->id, authreq->reply_id, authreq->ipaddr,
		ip_hostname (authreq->ipaddr), dns_name, authreq->code));

	/*
	 * Decrypt pw, if it's there - Pass along CHAP too.  If PASSWORD v/p
	 * is present, routine will decrypt it in passwd.  CHAP can just be
	 * passed along since we copy the vector from the request which is
	 * the CHAP challenge.
	 */

	/* Get the requesting client's secret from the authreq a/v pair */
	get_passwd (authreq, passwd, (char *) NULL, (char *) NULL);

	total_length = AUTH_HDR_LEN;

	if (get_vp (authreq->cur_request, PW_USER_PASSWORD)
							== (VALUE_PAIR *) NULL)
	{
		if ((pw_vp = get_vp (authreq->request, PW_USER_PASSWORD))
							!= (VALUE_PAIR *) NULL)
		{
			if (avpair_add (&authreq->cur_request, PW_USER_PASSWORD,
					pw_vp->strvalue, AUTH_PASS_LEN)
							== (VALUE_PAIR *) NULL)
			{
				logit (LOG_DAEMON, LOG_ERR,
		   			"%s: cannot add user password", func);
				return -1;
			}
		}
	}

	/* Load up the configuration values for the user */
	ptr = auth->data;
	for ( vp = authreq->cur_request ;
		vp != (VALUE_PAIR *) NULL ;
		vp = vp->next)
	{
		if ((vp->attribute == PW_USER_PASSWORD) &&
			(authreq->fsm_aatv != rad_authen_aatv))
		{
			continue; /* send passwords to LAS only */
		}

		debug_pair (stdout, vp);
		*ptr++ = vp->attribute;

		switch (vp->type)
		{
		    case PW_TYPE_STRING:
		    case PW_TYPE_OCTETS:
		    case PW_TYPE_VENDOR:
			if (vp->attribute == PW_USER_PASSWORD)
			{
				/*
				 *	Re-encrypt the password using the
				 *	secret shared by us and the remote
				 *	RADIUS system.  The variable "passwd"
				 *	contains an already decrypted password.
				 */
				*ptr++ = AUTH_PASS_LEN + 2;
				len = strlen (passwd);
				if (len > AUTH_PASS_LEN)
				{
					len = AUTH_PASS_LEN;
				}
				memset ((char *) passbuf, 0, AUTH_PASS_LEN);
				memcpy ((char *) passbuf, passwd, len);

				/* Calculate the MD5 Digest */

				secretlen = strlen (secret);
				strncpy ((char *) md5buf, secret, secretlen);
				memcpy ((char *) md5buf + secretlen,
						auth->vector, AUTH_VECTOR_LEN);
				md5_calc (ptr, md5buf,
						  secretlen + AUTH_VECTOR_LEN);

				/* Xor password into the MD5 digest */

				for (i = 0; i < AUTH_PASS_LEN; i++)
				{
					*ptr++ ^= passbuf[i];
				}
				memset ((char *) md5buf, 0, sizeof (md5buf));
				total_length += AUTH_PASS_LEN + 2;
			}
			else
			{
				/* len = (vp->lvalue > 0) ?
					  vp->lvalue : strlen (vp->strvalue); */
				len = strlen (vp->strvalue);
				*ptr++ = len + 2;
				memcpy ((char *) ptr, vp->strvalue, len);
				ptr[len] = '\0';
				ptr += len;
				total_length += len + 2;
			}
			break;

		    case PW_TYPE_INTEGER:
		    case PW_TYPE_IPADDR:
			*ptr++ = sizeof (UINT4) + 2;
			lvalue = htonl (vp->lvalue);
			memcpy ((char *) ptr, (char *) &lvalue, sizeof (UINT4));
			ptr += sizeof (UINT4);
			total_length += sizeof (UINT4) + 2;
			break;

		    default:
			break;
		}
	}

	memset (passwd, 0, sizeof (passwd));  /* Don't keep password around */
	auth->length = htons (total_length);

	memset ((char *) &last_send_sin, '\0', sizeof (last_send_sin));
	last_send_sin.sin_family = AF_INET;
	last_send_sin.sin_addr.s_addr = htonl(remote_ipaddr);
	last_send_sin.sin_port = htons(svc_port);

	last_send_len = total_length;

	return sendto (socket, (char *) auth, (int) total_length, (int) 0,
		(struct sockaddr *) & last_send_sin, sizeof (last_send_sin));
} /* end of radius_send () */

/*************************************************************************
 *
 *	Function: reconfig
 *
 *	Purpose: Invoked by HUP signal.  You may issue a HUP signal to
 *		 the server process after updating any of the configuration
 *		 files.  Calls config_files() to set up memory resident info
 *		 from these files.  Closes and re-opens the logfile.
 *
 *************************************************************************/

static void
reconfig (sig)

int             sig;

{
	int             n;
	static char    *func = "reconfig";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (sig > 0) /* then it really is a signal... */
	{
		logit (LOG_DAEMON, LOG_INFO, "%s: HUP signal received", func);
		select_max = 0;		/* Reset the select() time recorder */
		max_q_size = 0;		/* and the max queue size variables. */
		max_aq_size = 0;

		if (msgfd != NULL)
		{
			fflush (msgfd);
			if (msgfd != stderr)
			{
				sprintf (path, "%s/%s", radius_dir, RADIUS_LOG);
				freopen (path, "a", msgfd);
			}
		}
	}

	/* Can't reread FSM if we're already in it! */
	if (sig != -1)
	{
		/* Read the FSM config files into data structures */
		if ((n = init_fsm (nfsm, aatv_ptrs, MAX_AATV, radius_fsm,
						&fsm, &default_fsm)) <= 0)
		{
			if ((file_logging == 1) && (msgfd != (FILE *) NULL))
			{
				fflush (msgfd);
			}

			exit (-3);
		}
		nfsm = n;
	}

	config_init ();

	n = config_files (cache_users, 1, 1);

	if (n == 0) /* Initialize all aatv modules */
	{
		init_aatvs (); /* may increase value of the inetd variable */
	}
	else
	{
		if ((file_logging == 1) && (msgfd != (FILE *) NULL))
		{
			fflush (msgfd);
		}

		exit (-4);
	}

	config_fini ();

	return;
} /* end of reconfig () */

/*************************************************************************
 *
 *	Function: record_event
 *
 *	Purpose: Adds EVENT_ENT to authreq if resultant event is EV_WAIT.
 *		 Records current state, FSM action, direct action, PID,
 *		 integer and string in EVENT_ENT for later re-sending.
 *
 *************************************************************************/

static void
record_event (authreq, aatv, pid, value, string)

AUTH_REQ       *authreq;
AATV           *aatv;
int             pid;
int             value;
char           *string;

{
	EVENT_ENT      *event;
	static char    *func = "record_event";

	if ((event =
		(EVENT_ENT *) malloc (sizeof (EVENT_ENT))) == (EVENT_ENT *) NULL)
	{
		logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory", func);
		abort ();
	}

	event->auth_head = authreq; /* all EVENT_ENT nodes point to authreq */

	event->state = authreq->state;
	event->fsm_aatv = authreq->fsm_aatv;
	event->sub_aatv = aatv;

	if ((aatv->aatvfunc_type == AA_SOCKET) && (last_send_len > 0))
	{
		if ((event->packet = (u_char *) malloc (last_send_len))
							== (u_char *) NULL)
		{
			logit (LOG_DAEMON, LOG_ALERT, "%s: FATAL out of memory",
				func);
			abort ();
		}
		memcpy ((char *) &event->sin, (char *) &last_send_sin,
			sizeof (last_send_sin));
		event->len = last_send_len;
		memcpy ((char *) event->packet, send_buffer, last_send_len);
		strcpy (event->action, last_send_action);
	}
	else
	{
		event->len = 0;
		event->packet = (u_char *) NULL;
		event->action[0] = '\0';
	}

	last_send_len = 0;
	event->evalue = value;
	strcpy (event->estring, string);
	event->pid = (pid_t) pid;

	event->next = authreq->event_q; /* link this node onto event_q */
	authreq->event_q = event;

	dprintf(2, (LOG_DAEMON, LOG_DEBUG,
		"%s: event [%d '%s' '%s'  PID = %d  %d '%s']", func,
		event->state, event->fsm_aatv->id, event->sub_aatv->id,
		event->pid, event->evalue, event->estring));

	return;
} /* end of record_event () */

/*************************************************************************
 *
 *	Function: redo_action
 *
 *	Purpose: For pending events of aatvfunc_type == AA_SOCKET this
 *		 AATV function re-issues the socket AATV action function.
 *
 *************************************************************************/

static int
redo_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	int             result;
	u_char          save_state;
	EVENT_ENT      *event_list;
	EVENT_ENT      *event;
	EVENT_ENT     **prev_event;
	static char    *func = "redo_action";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (authreq == (AUTH_REQ *) NULL)
	{
		return EV_WAIT;
	}

	event_list = (EVENT_ENT *) NULL;

	for (prev_event = &authreq->event_q ;
		(event = *prev_event) != (EVENT_ENT *) NULL ; )
	{
		dprintf(2, (LOG_DAEMON, LOG_DEBUG,
			"%s: event [%d '%s' '%s'  PID = %d  %d '%s']", func,
			event->state, event->fsm_aatv->id, event->sub_aatv->id,
			event->pid, event->evalue, event->estring));
		if (event->sub_aatv->aatvfunc_type == AA_SOCKET)
		{
			*prev_event = event->next;
			event->next = event_list;
			event_list = event;
			dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				"%s: event found: AATV '%s'", func, 
				event->sub_aatv->id));
			continue;
		}
		prev_event = &event->next;
	}

	/* redo each event by calling its AATV redo() function */

	while (event_list != (EVENT_ENT *) NULL)
	{
		event = event_list;
		event_list = event->next;

		if (event->packet == (u_char *) NULL)
		{
			save_state = authreq->state;
			authreq->state = event->state;
			authreq->fsm_aatv = event->fsm_aatv;
			dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				"%s: invoking AATV '%s'", func,
				event->sub_aatv->id));
			result = call_action (event->sub_aatv, authreq,
						event->evalue, event->estring);
			authreq->state = save_state;
		}
		else
		{
			dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				"%s: resending packet for %s", func,
				event->sub_aatv->id));
			sendto (event->sub_aatv->sockfd, (char *) event->packet,
				event->len, (int) 0,
				(struct sockaddr *) &event->sin,
				sizeof (event->sin));
			result = EV_ACK;	/* prevent freeing of event */
		}

		if (result == EV_WAIT)
		{
			free_event (event);
		}
		else /* return to authreq structure */
		{
			event->next = authreq->event_q;
			authreq->event_q = event;
		}
	}
	return EV_WAIT;
} /* end of redo_action () */

/*************************************************************************
 *
 *	Function: reply_timer
 *
 *	Purpose: Invoked by ALARM signal.  Performs timeout of outstanding
 *		 RADIUS-to-RADIUS requests and MinosAuth requests.  Requests
 *		 stay around on the global queue with a state of ST_HOLD
 *		 until they timeout -- when they are removed from the queue.
 *
 *************************************************************************/

static void
reply_timer (signo)

int	signo;

{
	int             i;
	int             reset = 0;
	AUTH_REQ       *authreq;
	AUTH_REQ      **prev;
	int           (*timer_func) ();
	EV              ev;
	static char    *func = "reply_timer";

	dprintf(3, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if ((file_logging == 1) && (msgfd != (FILE *) NULL))
	{
		fflush (msgfd);
	}

	/* Really only need to do this the first time through here... */
	FD_SET(server_aatv.sockfd, &select_mask);

	ev.state = ST_ANY;
	ev.a.aatv = rad_any_aatv;
	ev.isproxy = 0;
	ev.xstring[0] = '\0';

	/* Check all entries for process timeout (request ttl) and timer */
	for (prev = &global_acct_q ; (authreq = *prev) ; )
	{
		if ((authreq->timer != 0) && (--authreq->timer == 0))
		{
			ev.value = EV_TIMER;
			state_machine (ev, authreq);
			if (*prev != authreq)
			{
				continue;
			}
		}

		/* Ignore requests that have a ttl of zero */
		if ((authreq->ttl != 0) && (--authreq->ttl == 0))
		{
			ev.value = EV_TIMEOUT;
			state_machine (ev, authreq);
			if (*prev != authreq)
			{
				continue;
			}
		}

		prev = &authreq->next;
	}

	for (prev = &global_request_q ; (authreq = *prev) ; )
	{
		if ((authreq->timer != 0) && (--authreq->timer == 0))
		{
			ev.value = EV_TIMER;
			state_machine (ev, authreq);
			if (*prev != authreq)
			{
				continue;
			}
		}

		/* Ignore requests that have a ttl of zero */
		if ((authreq->ttl != 0) && (--authreq->ttl == 0))
		{
			ev.value = EV_TIMEOUT;
			state_machine (ev, authreq);
			if (*prev != authreq)
			{
				continue;
			}
		}

		prev = &authreq->next;
	}

	for (i = 0 ; (timer_func = timer_funcs[i]) ; i++) 
	{
		if (timer_func ())
		{
			reset++;
		}
	}

  	if ((global_request_q == (AUTH_REQ *) NULL) &&
  		(global_acct_q == (AUTH_REQ *) NULL) &&
		(want_timer == 0) &&
		(reset == 0))
	{
  		alarm_set = 0;		/* Indicate no alarm set now */
  	}
	else /* still awaiting some replies */
	{
    		alarm (1);		/* Reset timer */
 	}

	return;
} /* end of reply_timer () */

/*************************************************************************
 *
 *	Function: send_reply
 *
 *	Purpose: Reply to the original request with an ACKNOWLEDGE,
 *		 CHALLENGE or REJECT.
 *
 *		 Attach reply attribute value pairs and user message,
 *		 if any, if ACKNOWLEDGE or CHALLENGE.
 *
 *		 If ack_nak_flag == 1, issue REJECT without a user
 *		 message, otherwise issue REJECT with a user message.
 *
 *************************************************************************/

static void
send_reply (packet, result, authreq, sockfd)

char           *packet;
int             result;
AUTH_REQ       *authreq;
int             sockfd;

{
	AUTH_HDR       *auth;
	VALUE_PAIR     *reply;
	u_char         *ptr;
	FILE           *debugout = stdout;
	int             len;
	int             secretlen;
	UINT4           lvalue;
	u_short         total_length;
	u_char          digest[AUTH_VECTOR_LEN];
	char            type[20];
	static char    *func = "send_reply";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered: result = %d",
		func, result));

	auth = (AUTH_HDR *) send_buffer;

	/* Build standard response header */
	if (result == EV_NAK)
	{
		strcpy (type, "Reject");
		if (authreq->code == PW_PASSWORD_REQUEST)
		{
			auth->code = PW_PASSWORD_REJECT;
		}
		else if (authreq->code == PW_ACCESS_REQUEST)
		{
			auth->code = PW_ACCESS_REJECT;
		}
		else /* was PW_STATUS_SERVER */
		{
			auth->code = PW_ACCESS_REJECT;
		}
	}
	else
	{
		if (result == EV_ACK)
		{
			strcpy (type, "Ack");
			if (authreq->code == PW_PASSWORD_REQUEST)
			{
				auth->code = PW_PASSWORD_ACK;
			}
			else if (authreq->code == PW_ACCESS_REQUEST)
			{
				auth->code = PW_ACCESS_ACCEPT;
			}
			else if (authreq->code == PW_ACCOUNTING_REQUEST)
			{
				auth->code = PW_ACCOUNTING_RESPONSE;
			}
			else /* was PW_STATUS_SERVER */
			{
				auth->code = PW_ACCESS_ACCEPT;
			}
		}
		else
		{
			if (result == EV_ACC_CHAL)
			{
				auth->code = PW_ACCESS_CHALLENGE;
				strcpy (type, "Challenge");
			}
			else
			{
				dprintf(1, (LOG_DAEMON, LOG_DEBUG,
					"%s: event %d not supported",
					func, result));
				return;
			}

		}
	}

	auth->id = authreq->id;
	memcpy ((char *) auth->vector, (char *) authreq->vector,
		AUTH_VECTOR_LEN);

	total_length = AUTH_HDR_LEN;

	ptr = auth->data;

	/* Load up the configuration values for the user (if any) */
	reply = authreq->cur_request;
	while (reply != (VALUE_PAIR *) NULL)
	{
		debug_pair (stdout, reply);
		*ptr++ = reply->attribute;

		switch (reply->type)
		{
		    case PW_TYPE_STRING:
			/* len = (reply->lvalue > 0) ?
				   reply->lvalue : strlen (reply->strvalue); */
			len = strlen (reply->strvalue);
			*ptr++ = len + 2;
			memcpy ((char *) ptr, reply->strvalue, len);
			ptr += len;
			total_length += len + 2;
			break;

		    case PW_TYPE_OCTETS:
		    case PW_TYPE_VENDOR:
			len = reply->lvalue;
			*ptr++ = len + 2;
			memcpy ((char *) ptr, reply->strvalue, len);
			ptr += len;
			total_length += len + 2;
			break;

		    case PW_TYPE_IPADDR:
			/* Re-resolve all DNS names */
			if (*reply->strvalue != '\0')
			{
				if (find_host_by_name (&reply->lvalue,
							reply->strvalue) != 0)
				{
					logit (LOG_DAEMON, LOG_ERR,
		"%s: Reply not sent - Unresolved or bad DNS name '%s' present",
						func, reply->strvalue);
					/* Ugly but effective to just ... */
					return;
				}
			}

		    case PW_TYPE_INTEGER:
			*ptr++ = sizeof (UINT4) + 2;
			lvalue = htonl (reply->lvalue);
			memcpy ((char *) ptr, (char *) &lvalue, sizeof (UINT4));
			ptr += sizeof (UINT4);
			total_length += sizeof (UINT4) + 2;
			break;

		    default:
			break;
		}

		reply = reply->next;
	}

	if (result == EV_NAK)
	{
		if (strlen (port_msg) > 0 && ack_nak_flag == 0)
		{
			; /* have a NAK Reply-Message in port_msg */
		}
		else /* have empty port_msg, look for any Reply-Messages */
		{
			if (get_vp (authreq->cur_request, PW_REPLY_MESSAGE)
							== (VALUE_PAIR *) NULL)
			{
				sprintf (port_msg, "Authentication failure");
				ack_nak_flag = 0;
			}
		}
	}

	/* May not want to send back advisory message */
	if ((ack_nak_flag > 0) && (result == EV_NAK))
	{
		;
	}
	else /* Append the user message (if any) */
	{
		if ((port_msg != (char *) NULL) && (strlen (port_msg) > 0))
		{
			strcat (port_msg, "\r\n");
			len = strlen (port_msg);
			if (len > 0 && len < AUTH_STRING_LEN)
			{
				*ptr++ = PW_REPLY_MESSAGE;
				*ptr++ = len + 2;
				memcpy ((char *) ptr, port_msg, len);
				ptr += len;
				total_length += len + 2;
			}
			if (debug_flag)
			{
				if (ddt != (FILE *) NULL)
				{
					debugout = ddt;
				}

				fputs ("    ", debugout);
				fprintf (debugout, "Reply-Message = \"%s\"",
					port_msg);
				fputs ("\n", debugout);
			}
		}
	}

	/* Set total length in the header */
	auth->length = htons (total_length);

	/* Append the secret and calculate the response digest */
	secretlen = strlen ((char *) authreq->secret);
	memcpy (send_buffer + total_length, (char *) authreq->secret,
		secretlen);
	md5_calc (digest, (char *) auth, total_length + secretlen);
	memcpy ((char *) auth->vector, (char *) digest, AUTH_VECTOR_LEN);
	memset (send_buffer + total_length, '\0', secretlen);

	memset ((char *) &last_send_sin, '\0', sizeof (last_send_sin));
	last_send_sin.sin_family = AF_INET;
	last_send_sin.sin_addr.s_addr = htonl(authreq->ipaddr);
	last_send_sin.sin_port = htons(authreq->udp_port);

	dprintf(1, (LOG_AUTH, LOG_DEBUG,
		"%s: %s %s for id %d of %s (type %u) to %lx (%s)", func, packet,
		type, authreq->id, authtype_toa (authreq->code), authreq->code,
		(UINT4) authreq->ipaddr, ip_hostname (authreq->ipaddr)));

	last_send_len = total_length;

	/* Send it to the user */
	sendto (sockfd, (char *) auth, (int) total_length, (int) 0,
		(struct sockaddr *) & last_send_sin, sizeof (last_send_sin));

	return;
} /* end of send_reply () */

/*************************************************************************
 *
 *	Function: server_status
 *
 *	Purpose: Allow management poll (poll to see if we're alive)
 *		 from anywhere.  Returns some server statistics.
 *
 *************************************************************************/

static int
server_status (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	char           *nas_name;
	VALUE_PAIR     *vp;
	int             pool_cnt = -1;
	long            hours;
	long            minutes;
	long            left;
	UINT4           ip_addr;
	char            buf[AUTH_STRING_LEN];
	char            etc[32];	/* For "dbg:" and birthdate. */
	static char    *func = "server_status";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

#ifdef MERIT_TIMELEFT
	vp = get_vp (authreq->cur_request, PW_PROXY_ACTION);
	if (vp != (VALUE_PAIR *) NULL && strcmp (vp->strvalue, "TIMELEFT") == 0)
	{
		vp = get_vp (authreq->cur_request, PW_NAS_IDENTIFIER);
		if (vp != (VALUE_PAIR *) NULL)
		{
			nas_name = vp->strvalue;
		}
		else
		{
			nas_name = "unknown";
		}

		vp = get_vp (authreq->request, PW_FRAMED_IP_ADDRESS);
		if (vp != (VALUE_PAIR *) NULL)
		{
			ip_addr = vp->lvalue;
		}
		else
		{
			ip_addr = 0L;
		}

		if ((left = find_time_remaining (nas_name, ip_addr)) > 0)
		{
			hours = left / 3600;
			minutes = (left - (hours * 3660)) / 60;
			sprintf (port_msg,
				"\r\n%ld hour%s and %ld minute%s left.",
				hours, hours == 1L ? "" : "s",
				minutes, minutes == 1L ? "" : "s");
		}
		else /* a negative number of seconds left was returned */
		{
			if (left == -3L)
			{
				sprintf (port_msg,
					"\r\nUnable to identify NAS.");
			}
			if (left == -2L)
			{
				sprintf (port_msg,
					"\r\nUnlimited time remains.");
			}
			if (left == -1L)
			{
				sprintf (port_msg,
					"\r\nUnable to identify user.");
			}
			if ((left == -3L) || (left == -1L))
			{
				strcat (port_msg, "  Retry in a few minutes.");
			}
		}

		ack_nak_flag = 1;

		return EV_ACK;
	}
#endif	/* MERIT_TIMELEFT */

#ifdef MERIT_LAS
	pool_cnt = query_pool (authreq); /* tokenpool counts returned here */
	sprintf (buf, "number of pools: %d", pool_cnt);
	avpair_add (&authreq->cur_request, PW_REPLY_MESSAGE, buf, 0);
#endif	/* MERIT_LAS */

	sprintf (buf,
		"auth queue: %d/%d, acct queue: %d/%d, maxtime: %d (%-24.24s)",
		max_q_size, cur_q_size, max_aq_size, cur_aq_size, select_max,
		ctime (&tofmaxdelay));
	avpair_add (&authreq->cur_request, PW_REPLY_MESSAGE, buf, 0);

	sprintf (buf, "authfile: %d, clients: %d, users: %d, ", authfile_cnt,
		 clients_cnt, users_cnt);

	/* Show the FSMID from the %FSMID keyword in FSM table. */
	if (fsm_id != NULL)
	{
	    strcat (buf, "fsmid: ");
	    strcat (buf, fsm_id);
	    strcat (buf, ", ");
	}
	else
	{
	    sprintf (etc, " fsm: %d, ", nfsm);
	}

	/* Show what our debugging level is remotely. */
	if (debug_flag > 0)
	{
	    sprintf (etc, "dbg: %d, ", debug_flag);
	    strcat (buf, etc);
	}

	/* Put in the birthdate of when we started running. */
	sprintf (etc, "%-24.24s", ctime (&birthdate));
	strcat (buf, etc);

	avpair_add (&authreq->cur_request, PW_REPLY_MESSAGE, buf, 0);

	sprintf (port_msg, "%s", version ());
	ack_nak_flag = 1;

	return EV_ACK;
} /* end of server_status () */

/*************************************************************************
 *
 *	Function: set_debug
 *
 *	Purpose: Turn on (code == 1) or off (code == 0) debuging by opening
 *		 or closing the debug file.
 *
 *************************************************************************/

static void
set_debug (code)

int code;

{
	int n;
	time_t          now;

	if (code)
	{
		sprintf (path, "%s/%s", radius_dir, debug_file);
		ddt = freopen (path, "w+", stderr);
		if (ddt == (FILE *) NULL)
		{
			debug_flag = 0;
		}
		else
		{
#if defined(HAVE_SETVBUF)
			setvbuf (ddt, NULL, _IOLBF, BUFSIZ);
#else
			setlinebuf (ddt);
#endif
			if (zap_debugfile == 0)
			{
				if ((n = fcntl (fileno (ddt), F_GETFL, 0)) < 0)
				{
					syslog (LOG_WARNING,
						"fcntl(ddt, F_GETFL): %m");
				}
				else
				{
					(void) fcntl (fileno(ddt),
							F_SETFL, n|O_APPEND);
				}
			}
			else
			{
				zap_debugfile = 0;
			}
		}
	}
	else	/* delay closing ddt, we might interrupt someone */
	{
		if (ddt != (FILE *) NULL)
		{
			now = time (0);
			fprintf (ddt, "%-24.24s: Debugging turned off\n",
				ctime (&now));
			fflush (ddt);
			debug_flag = 0;
		}
	}
} /* end of set_debug () */

/*************************************************************************
 *
 *	Function: sig_fatal
 *
 *	Purpose: Log error message when undefined (fatal) signals occur.
 *
 *************************************************************************/

static void
sig_fatal (sig)

int             sig;

{
	struct sigaction action;
	static char    *func = "sig_fatal";

	dprintf(1, (LOG_DAEMON, LOG_DEBUG, "%s: signal %d", func, sig));

	switch (sig)
	{
	    case SIGABRT:
		if (dumpcore == 0) /* don't call abort(3) here */
		{
			logit (LOG_DAEMON, LOG_ERR,
				"%s: exit on signal (%d)", func, sig);
			exit (-7);
		}
	}

	logit (LOG_DAEMON, LOG_ERR, "%s: ABORT on signal (%d)", func, sig);

	dumpcore = 1;

	action.sa_handler = SIG_DFL;
	sigemptyset (&action.sa_mask);
	action.sa_flags = 0;

	sigaction (SIGABRT, &action, NULL);
	abort ();

	/*** NOTREACHED ***/

} /* end of sig_fatal () */

/*************************************************************************
 *
 *	Function: sig_int
 *
 *	Purpose: Another way to initialize all AATVs which have
 *		 initialization functions.
 *
 *************************************************************************/

static void
sig_int (sig)

int             sig;

{
	static char    *func = "sig_int";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	logit (LOG_DAEMON, LOG_INFO, "%s: INT signal received", func);

	init_aatvs ();

	return;
} /* end of sig_int () */

/*************************************************************************
 *
 *	Function: sig_term
 *
 *	Purpose: Orderly shutdown of RADIUS engine/server.
 *
 *************************************************************************/

static void
sig_term (sig)

int             sig;

{
	int             i;
	AATV           *aatv;
	void          (*close_func) PROTO((void));
	static char    *func = "sig_term";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	if (sig == 0)
	{
		logit (LOG_DAEMON, LOG_INFO, "%s: select timed out", func);
	}
	else
	{
		logit (LOG_DAEMON, LOG_INFO, "%s: TERM signal received", func);
	}

	/* It would be nice to purge the authreq queue(s) here, but it's hard */

	for (i = 0; i < MAX_AATV; i++)
	{	
		if ((aatv = *aatv_ptrs[i]) == NULL)
		{
			continue;
		}

		close_func = aatv->cleanup;
		if (close_func != (void (*) () ) NULL) 
		{
			close_func ();
		}
	}

	if (file_logging == 0)
	{
		closelog ();
	}
	else /* may be file logging */
	{
		if (file_logging == 1)
		{
			(void) fclose (msgfd);
		}
	}

	if (ddt != (FILE *) NULL)
	{
		(void) fclose (ddt);
	}

	exit (0);
} /* end of sig_term () */

/*************************************************************************
 *
 *	Function: start_fsm
 *
 *	Purpose: Enters the FSM with the given event, AATV and authreq.
 *
 *************************************************************************/

void
start_fsm (authreq, event, action, xstring)

AUTH_REQ       *authreq;
int             event;
char           *action;		/* optional (may be NULL), so ignore */
char           *xstring;        /* pointer to optional string FSM parameter */

{
	EV              ev;
	static char    *func = "start_fsm";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	ev.state = authreq->state; /* record the old (current) state */
	authreq->state = ST_INIT; /* always enter the FSM at the start state */

	ev.isproxy = 0; /* assume action is not a proxy string */
	if (action == (char *) NULL)
	{
		ev.a.aatv = rad_any_aatv;
	}
	else /* action is either a proxy string or an AATV name */
	{
		if (action[0] == '+') /* action is a proxy string */
		{
			ev.a.proxy = add_string (&action[1], ASIS);
			ev.isproxy = 1;
		}
		else /* action was an AATV name */
		{
			ev.a.aatv = find_aatv (action);
		}
	}

	ev.value = event;

	if (xstring != (char *) NULL)
	{
		strcpy (ev.xstring, xstring);
	}
	else
	{
		ev.xstring[0] = '\0';
	}

	state_machine (ev, authreq);

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: exiting", func));

	return;
} /* end of start_fsm () */

/*************************************************************************
 *
 *	Function: state_machine
 *
 *	Purpose: Given an event from a previous action and a current state,
 *		 determine (from a pre-configured FSM table): 1) the action
 *		 to take as a result of this event having occurred and,
 *		 2) the value of the next state to go to in the FSM table.
 *
 *************************************************************************/

static void
state_machine (given, authreq)

EV              given;
AUTH_REQ       *authreq;

{
	int             result;
	int             avalue;
	u_char          fsm_state;
	u_char          cur_state;
	u_char          next_state;
	u_char          default_match;
	FSM_ENT        *fsm_ent;
	AATV           *fsm_action;
	static char    *func = "state_machine";

	/*
	 *	Loop forever in the FSM until either:
	 *
	 *	1) a state is encountered which has no action (NULL action)
	 *
	 *	   or
	 *
	 *	2) the end of the FSM table is reached
	 *
	 */

	result = EV_ACK;

	if (authreq == (AUTH_REQ *) NULL)
	{
		cur_state = ST_ANY;
	}
	else
	{
		cur_state = authreq->state;
	}

	dprintf(2, (LOG_DAEMON, LOG_DEBUG,
		"%s: entered: current state = %d  event = [%d %s %d]", func,
		cur_state, given.state, (given.isproxy == 1) ? given.a.proxy :
			(char *) given.a.aatv->id, given.value));

	while (cur_state != ST_END)
	{
		if (cur_state == ST_ANY)
		{
			fsm_ent = (FSM_ENT *) NULL; /* to force default table */
		}
		else
		{
			if (cur_state >= nfsm)
			{
				logit (LOG_DAEMON, LOG_ERR,
					"%s: invalid state machine", func);
				dumpcore = 1;
				abort ();
			}

			fsm_ent = fsm[cur_state];
			do /* find explicit match */
			{
				if ((fsm_ent->event.value == given.value) &&
					(fsm_ent->event.state == ST_ANY ||
					 fsm_ent->event.state == given.state) &&
					((fsm_ent->event.isproxy == 0 &&
						given.isproxy == 0 &&
					(fsm_ent->event.a.aatv == &any_aatv ||
					fsm_ent->event.a.aatv == given.a.aatv))
					||
					(fsm_ent->event.isproxy == 1 &&
						given.isproxy == 1 &&
					strcmp (fsm_ent->event.a.proxy,
						given.a.proxy) == 0)))
				{
					fsm_state = fsm_ent->next_state;
					fsm_action = fsm_ent->action;
					avalue = fsm_ent->xvalue;
					if (fsm_ent->xstring != (char *) NULL)
					{
						strcpy (given.xstring,
							fsm_ent->xstring);
					}
					break; /* found explicit match */
				}
				fsm_ent = fsm_ent->next;
			}
			while (fsm_ent != (FSM_ENT *) NULL);
			default_match = 0;
		}

		if (fsm_ent == (FSM_ENT *) NULL) /* else, try default table */
		{
			fsm_ent = default_fsm[0];
			do /* find implicit match */
			{
				if (fsm_ent->event.value == given.value)
				{
					fsm_state = fsm_ent->next_state;
					fsm_action = fsm_ent->action;
					avalue = fsm_ent->xvalue;
					if (fsm_ent->xstring != (char *) NULL)
					{
						strcpy (given.xstring,
							fsm_ent->xstring);
					}
					break; /* found implicit match */
				}
				fsm_ent = fsm_ent->next;
			}
			while (fsm_ent != (FSM_ENT *) NULL);

			if (fsm_ent == (FSM_ENT *) NULL)
			{
				/*
				 *	If there are no explicit events which
				 *	match and no default event table
				 *	entries which match, then the default
				 *	action is to ignore event.
				 */

				if (given.isproxy == 1)
				{
					logit (LOG_DAEMON, LOG_INFO,
		    "%s: proxy action %s not found, %s (type %u) id %u from %s",
						func, given.a.proxy,
						authtype_toa (authreq->code),
						authreq->code, authreq->id,
						ip_hostname (authreq->ipaddr));
				}

				fsm_state = ST_SAME;
				fsm_action = rad_null_aatv;
			}
			default_match = 1;
		}

		if ((next_state = fsm_state) == ST_SAME)
		{
			/* We're to stay in same state */
			next_state = cur_state;
		}

		dprintf(2, (LOG_DAEMON, LOG_DEBUG,
			"%s: decision: action = %s  next_state = %d", func,
			fsm_action->id, next_state));

		if (authreq != (AUTH_REQ *) NULL)
		{
			authreq->fsm_aatv = fsm_action;
			if (given.value == EV_ACK ||
				given.value == EV_NAK ||
				given.value == EV_ERROR ||
				given.value == EV_ACC_CHAL)
			{
				authreq->result = given.value;
			}
		}

		if (fsm_action == rad_end_aatv)
		{
			break;
		}

		result = call_action (fsm_action, authreq, avalue,
								given.xstring);
		given.state = cur_state;
		given.a.aatv = fsm_action;
		given.isproxy = 0;
		given.value = result;
		dprintf(2, (LOG_DAEMON, LOG_DEBUG,
			"%s: after action: event = [%d %s %d %s]", func,
			given.state, (given.isproxy == 1) ? given.a.proxy :
				(char *) given.a.aatv->id, given.value,
			given.xstring));

		if (authreq != (AUTH_REQ *) NULL)
		{
			authreq->state = next_state;
		}

		if (cur_state == next_state && default_match)
		{
		/* Exit if default table entry used and staying in same state */
			dprintf(2, (LOG_DAEMON, LOG_DEBUG,
				"%s: return from FSM -- nothing to do", func));
			return;	
		}
		cur_state = next_state;

	} /* end of while (1) */

	free_authreq (authreq);

	dprintf(2, (LOG_DAEMON, LOG_DEBUG,
		"%s: return from FSM -- finished with FSM table", func));
	return; /* this return means: finished with the FSM */
} /* end of state_machine () */

/*************************************************************************
 *
 *	Function: stat_files
 *
 *	Purpose: Reads configuration files into memory if their timestamps
 *		 differ from the recorded values.
 *
 *************************************************************************/

static void
stat_files (init_flag, cache_flag)

int             init_flag;  /* controls initialization (1 => init) */
int             cache_flag; /* copy of radiusd cache_users (-u) option */

{
	int             doit;           /* one implies call config_files() */
	static time_t   save_atime = 0; /* timestamp of authfile */
	static time_t   save_ctime = 0; /* timestamp of clients file */
	static time_t   save_utime = 0; /* timestamp of users file */
#ifdef MERIT_HUNTGROUP
	static time_t   save_hgtime = 0; /* timestamp of huntgroups file */
#endif	/* MERIT_HUNTGROUP */
	struct stat     stbuf;
	static char    *func = "stat_files";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: entered", func));

	doit = 0;
	sprintf (path, "%s/%s", radius_dir, RADIUS_AUTH);
	if (stat (path, &stbuf))
	{
		save_atime = (time_t) 0;
	}
	else /* stat () returned zero (success) */
	{
		doit = MAX(doit, (stbuf.st_mtime > save_atime) ? 1 : 0);
		save_atime = stbuf.st_mtime;
	}

	sprintf (path, "%s/%s", radius_dir, RADIUS_CLIENTS);
	if (stat (path, &stbuf))
	{
		save_ctime = (time_t) 0;
	}
	else /* stat () returned zero (success) */
	{
		doit = MAX(doit, (stbuf.st_mtime > save_ctime) ? 1 : 0);
		save_ctime = stbuf.st_mtime;
	}

	if (cache_flag)
	{
		sprintf (path, "%s/%s", radius_dir, RADIUS_USERS);
		if (stat (path, &stbuf))
		{
			save_utime = (time_t) 0;
		}
		else /* stat () returned zero (success) */
		{
			doit = MAX(doit, (stbuf.st_mtime > save_utime) ? 1 : 0);
			save_utime = stbuf.st_mtime;
		}
	}

#ifdef MERIT_HUNTGROUP
	sprintf (path, "%s/%s", radius_dir, RADIUS_HUNTGROUPS);
        if (stat (path, &stbuf))
	{
		save_hgtime = (time_t) 0;
	}
	else /* stat () returned zero (success) */
	{
		doit = MAX(doit, (stbuf.st_mtime > save_hgtime) ? 1 : 0);
		save_hgtime = stbuf.st_mtime;
	}
#endif	/* MERIT_HUNTGROUP */

	if ( (!init_flag) && doit )
	{
		reconfig (-1);
	}

	return;
} /* end of stat_files () */

/*************************************************************************
 *
 *	Function: timeout_action
 *
 *	Purpose: Performs the logging required when a timeout event occurs.
 *
 *************************************************************************/

static int
timeout_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	VALUE_PAIR     *acct;
	VALUE_PAIR     *nas = (VALUE_PAIR *) NULL;
	VALUE_PAIR     *port = (VALUE_PAIR *) NULL;
	VALUE_PAIR     *proxy = (VALUE_PAIR *) NULL;
	VALUE_PAIR     *user = (VALUE_PAIR *) NULL;
	char            astring[100];
	static char    *func = "timeout_action";

	if (TAR_NO_LOG(authreq))
	{
		return EV_WAIT;
	}

	astring[0] = '\0';
	if (authreq->retry_cnt > 0)
	{
		sprintf (astring, " (%d retries)", authreq->retry_cnt);
	}

	user = get_vp (authreq->cur_request, PW_USER_NAME);
	nas = get_vp (authreq->cur_request, PW_NAS_IDENTIFIER);
	port = get_vp (authreq->cur_request, PW_NAS_PORT);
	if (authreq->cur_request->attribute == PW_PROXY_ACTION)
	{
		proxy = authreq->cur_request;
	}

	if (authreq->code == PW_ACCOUNTING_REQUEST)
	{
		acct = get_vp (authreq->cur_request, PW_ACCT_STATUS_TYPE);
		if (acct != (VALUE_PAIR *) NULL)
		{
			strcat (astring, " ");
			strcat (astring, acct->name);
			strcat (astring, " = ");
			strcat (astring, avpair_vtoa (acct, 0));
		}
		else
		{
			strcat (astring, " unknown-acct");
		}
	}

	logit (LOG_DAEMON, LOG_INFO,
"%s:%s request %s (type %u) %u/%u for %s (%s port %d) from %s timed out from state %u%s",
		func,
		(proxy == (VALUE_PAIR *) NULL) ? "" : proxy->strvalue,
		authtype_toa (authreq->code), authreq->code, authreq->id,
		authreq->reply_id,
		(user == (VALUE_PAIR *) NULL) ? "?" : user->strvalue,
		(nas == (VALUE_PAIR *) NULL) ? "?" : nas->strvalue,
		(port == (VALUE_PAIR *) NULL) ? -1 : port->lvalue,
		ip_hostname (authreq->ipaddr), authreq->state, astring);
	return EV_WAIT;
} /* end of timeout_action () */

/*************************************************************************
 *
 *	Function: timer_action
 *
 *	Purpose: Initializes the request timer value.
 *
 *************************************************************************/

static int
timer_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	static char    *func = "timer_action";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG,
		"%s: setting request timer value to %d", func, value));

	authreq->timer = value;

	return EV_WAIT;
} /* end of timer_action () */

/*************************************************************************
 *
 *	Function: usage
 *
 *	Purpose: Display the syntax for starting this program.
 *
 *************************************************************************/

static void
usage ()

{
	fprintf (stderr,
		"Usage: %s [ -d raddb_dir ] [ -a acct_dir ] [ -c cwd ]\n",
		progname);
	fprintf (stderr, "\t\t [ -p auth_port ] [ -q acct_port ] [ -f fsm ]\n");
	fprintf (stderr, "\t\t [ -pp auth_relay ] [ -qq acct_relay ]\n");
	fprintf (stderr,
		"\t\t [ -g 'logfile' | 'syslog' | 'stderr' ] [ -h ]\n");
	fprintf (stderr, "\t\t [ -t timeout ] [ -s ] [ -x ] [ -v ] [ -z ]");

#if defined(USE_DBM) || defined(USE_NDBM)
	fprintf (stderr, " [ -u ]");
#endif	/* USE_DBM || USE_NDBM*/

	fprintf (stderr, "\n");
	fprintf (stderr, "\t -d\tdirectory: of users, clients, and authfile\n");
	fprintf (stderr, "\t -a\tdirectory: where to put accounting records\n");
	fprintf (stderr, "\t -c\tdirectory: new current working directory\n");
	fprintf (stderr, "\t -p\tport number to listen for auth requests on\n");
	fprintf (stderr, "\t -q\tport number to listen for acct requests on\n");
	fprintf (stderr, "\t -f\tfile name of FSM configuration table\n");
	fprintf (stderr, "\t -pp\tport number to relay auth requests on\n");
	fprintf (stderr, "\t -qq\tport number to relay acct requests on\n");
	fprintf (stderr, "\t -g\tselect logfile, syslog or stderr logging\n");
	fprintf (stderr, "\t -t\tinactivity timeout value (minutes)\n");
	fprintf (stderr, "\t -s\tsingle process (non-spawning) mode\n");
	fprintf (stderr, "\t -x\tadd to debug flag value\n");
	fprintf (stderr, "\t -v\tdisplays RADIUS version\n");
	fprintf (stderr, "\t -z\tempty (zap) the logfile & debug file if -x\n");
	fprintf (stderr, "\t -h\tdisplays this help syntax\n");

#if defined(USE_DBM) || defined(USE_NDBM)
	fprintf (stderr, "\t -u\tdon't cache users file in memory\n");
#endif	/* USE_DBM || USE_NDBM*/

	exit (-8);
} /* end of usage () */

/*************************************************************************
 *
 *	Function: wait_action
 *
 *	Purpose: Utility AATV which always responds tenatively...
 *		 until the counter runs to zero.
 *
 *************************************************************************/

static int
wait_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{      
	static char    *func = "wait_action";

	if (value > 0)
	{
	  	if (authreq->retry_cnt >= value)
		{
			  return EV_ACK;
		}
	}

	return EV_WAIT;
} /* end of wait_action () */

/*************************************************************************
 *
 *	Function: xstring_clear_action
 *
 *	Purpose: Utility AATV which clears out the xstring value.
 *
 *************************************************************************/

static int
xstring_clear_action (authreq, value, afpar)

AUTH_REQ       *authreq;
int             value;
char           *afpar;

{
	EVENT_ENT       *ev;
	static char     *func = "xstring_clear_action";

	dprintf(2, (LOG_DAEMON, LOG_DEBUG,
		"%s: entered (authreq=0x%p, %d, afpar=0x%p='%s'",
		func, authreq, value, afpar, afpar));

	dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: aatvs: fsm=%s, direct=%s",
		func, authreq->fsm_aatv->id, authreq->direct_aatv->id));

	for (ev = authreq->event_q; ev; ev = ev->next)
	{
		dprintf(2, (LOG_DAEMON, LOG_DEBUG,
	       "%s: event at 0x%p, aatvs fsm=%s, sub_fsm=%s, estring=0x%p='%s'",
			func, ev, ev->fsm_aatv->id, ev->sub_aatv->id,
			(char *) (ev->estring), ev->estring));
 
		if (ev->fsm_aatv == authreq->fsm_aatv)
		{
			dprintf(2, (LOG_DAEMON, LOG_DEBUG, "%s: zap '%s'",
				func, ev->estring));
			ev->estring[0] = '\0';
		}
	}

	return EV_ACK;

} /* end of xstring_clear_action () */
