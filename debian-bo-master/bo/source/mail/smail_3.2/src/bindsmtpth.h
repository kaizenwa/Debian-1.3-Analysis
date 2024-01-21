/* @(#) bindsmtpth.h,v 1.4 1992/07/11 11:48:01 tron Exp */

/*****************************************************************************
 * File Name:	 bindsmtpth.h
 * Description:	 Transport hint structure specific to BIND/SMTP
 * Author:	 Simon Leinen (simon@liasun6)
 * Date Created:  9-Jul-91
 ****************************************************************************/

#ifndef _bindsmtp_h_
#define _bindsmtp_h_

struct mx_transport_hint {
    int preference;			/* preference of exchanger */
    char *exchanger;			/* hostname of exchanger */
    int implicit;			/* hint generated because no MX found */
    struct ipaddr_hint *ipaddrs;	/* chain of IP addrs */
};

struct ipaddr_hint {
    struct ipaddr_hint *succ;
    char *hostname;
    struct in_addr addr;
};

#endif /* not _bindsmtp_h_ */
