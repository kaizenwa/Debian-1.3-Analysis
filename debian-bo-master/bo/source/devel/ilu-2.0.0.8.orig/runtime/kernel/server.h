/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: server.h,v 1.8 1995/08/22 01:59:06 janssen Exp $ */
/* Last tweaked by Mike Spreitzer November 24, 1993 7:28 am PST */

#define server_lock(s)		((s)->sr_lock)
#define server_is_true(s)	((s)->sr_true != FALSE)
#define server_id(s)		((s)->sr_id)
#define server_is_closed(s)	((s)->sr_closed)
#define server_is_connected(s)	((s)->sr_connected)
#define server_connections(s)	((s)->sr_connHead.next)
#define server_ports(s)		((s)->sr_ports)
#define server_local_port(s)	((s)->sr_local_port)
#define server_objs(s)		((s)->sr_objs)
#define server_singles(s)	((s)->sr_singles)
#define server_objtab(s)	((s)->sr_objtab)
#define server_default_port(s)	((s)->sr_default_port)
#define server_true_language(s)	((s)->sr_true_language)
