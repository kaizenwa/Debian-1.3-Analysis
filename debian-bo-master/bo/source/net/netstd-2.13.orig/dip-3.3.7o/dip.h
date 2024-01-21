/*
 * dip		A program for handling dialup IP connecions.
 *
 * Version:	@(#)dip.h	3.3.7-uri	05/24/94
 * Version:	@(#)dip.h	3.3.7		12/13/93
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 *              Uri Blumenthal <uri@watson.ibm.com>
 *              (C) 1994
 *
 *		Paul Cadach <paul@paul.east.alma-ata.su>
 *		(C) 1994
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#define __KERNEL__
#include <sys/types.h>
#undef __KERNEL__
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <string.h>
#include <syslog.h>
#include <fcntl.h>
#include <termios.h>
#include <signal.h>
#include <dirent.h>
#include <limits.h>
#include <ctype.h>
#include <pwd.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <netinet/in.h>
#include <netdb.h>
#include <resolv.h>
#include <net/if_arp.h>

#include <linux/autoconf.h>

#include "pathnames.h"



/* DIP definitions. */
#define DIP_SFX		".dip"
#define DIP_SLIP	1		/* we support the SLIP protocol	 */
#define DIP_CSLIP	1		/* we support the CSLIP protocol */
#define DIP_PPP		1		/* we support the PPP  protocol	 */
#define DIP_TERM        1		/* we support the TERM protocol  */



/* SLIP/PPP/ASK initial protocol configuration. */
#define DEF_MTU		296
#define DEF_PROT	"SLIP"
#define DEF_MODEM	"HAYES"
#define DEF_SPEED	"38400"

/* For configuring interface/routing */
#define IFC_CONFIG	1
#define IFC_ROUTE	2
#define IFC_ARP  	3
#define IFC_PRE		1
#define IFC_UP		2
#define IFC_DOWN	3
#define IFC_POST	4

struct dip {
  /* Login info. */
  char			name[16];		/* login name of host	*/
  char			passwd[16];		/* any external passwd	*/
  char			home[1024];		/* user home directory	*/

  /* Misc protocol info. */
  char			comment[128];		/* any comments		*/
  char			protocol[16];		/* protocol to use	*/
  int			protonr;		/* decoded protocol #	*/
  int			mtu;			/* MTU to use for conn.	*/

  /* Connection info. */
  char			ifname[32];		/* SLIP device name     */
  char			hom_host[128];		/* real local host name */
  struct in_addr	hom_ip;			/* home IP address  	*/
  char			local[128];		/* local host name	*/
  struct in_addr	loc_ip;			/* mapped IP address	*/
  char			remote[128];		/* remote host name	*/
  struct in_addr	rmt_ip;			/* mapped IP address	*/
  char			gateway[128];		/* remote gateway name	*/
  struct in_addr	rmt_gw;			/* mapped IP address	*/
  char			netmask[32];		/* netmask of route	*/
  char			route[128];		/* name of rmt network	*/
  struct in_addr	rmt_rt;			/* mapped IP address	*/
  int			rtdefault;		/* are we the dflt gw?	*/
  int			proxyarp;		/* set proxy arp?       */
  int			timeout;		/* line inactivity timer*/
};


/* DIP protocol switcher definitions. */
struct protosw {
  char	*name;
  int	type;
  void	(*func)(struct dip *);
};

/* Modem chat responses list */
struct response {
  struct response *next;
  char            *string;
  int              code;
};

/* IFCONFIG/ROUTE parameters list */
struct ifcfg {
  struct ifcfg *next;
  struct ifcfg *prev;
  int           type;			/* Type of program: IFC_CONFIG */
					/*               or IFC_ROUTE  */
  int           when;                   /* When start it	       */
  char         *param;			/* Argument list	       */
};


/* DIP global variables. */
extern int		opt_v;		/* flag: be verbose!		*/
extern struct dip	mydip;		/* global DIP entry		*/
extern struct protosw	protosw[];	/* table with protocol info	*/
extern struct response	*chat;		/* list of modem's responses	*/
extern struct ifcfg	*cfg;		/* manually cfg. for interface	*/

/* Global functions. */
extern void     setproctitle(char *fmt, ...);
extern void	do_command(FILE *file);
extern void	do_login(char *user);
extern void	do_terminal(void);
extern int	get_prot(char *name);
extern int	dip_daemon(struct dip *dip);
extern int	dip_setup(struct dip *dip);
extern int	dip_login_setup(struct dip *dip);

extern void 	dip_addchat(struct response **seq, char *string, int code);

extern int	mdm_init(char *text);
extern int	mdm_chatseq(char *send, struct response *expect, int out);
extern int	mdm_dial(char *number, int out);
extern int	mdm_modem(char *modem);
extern int	mdm_reset(void);
extern int	mdm_hangup(void);

extern int	attach(struct dip *dip);
extern void	detach(struct dip *dip);

extern void	cleanup(void);
extern void	log_cleanup(void);

extern int	tty_getc(void);
extern int	tty_putc(int c);
extern int 	tty_puts(char *s);
extern int	tty_askfd(void);
extern int	tty_get_disc(int *disc);
extern int	tty_set_disc(int disc);
extern int	tty_get_encap(int *encap);
extern int	tty_set_encap(int encap);
extern int	tty_get_name(char *name);
extern int	tty_databits(char *bits);
extern int	tty_stopbits(char *bits);
extern int	tty_parity(char *type);
extern int	tty_speed(char *speed);
extern int	hanguptty(void);
extern int	tty_notlocal(void);
extern int	tty_login(void);
extern int	tty_flush(void);
extern int	tty_close(void);
extern int	tty_login_close(void);
extern int	tty_open(char *name);
extern int	tty_nomesg(void);
extern void	tty_setmode(int mode);
extern int	tty_getmode(void);
extern void     tty_sendbreak(void);

#ifdef SKEY
/* S/Key authentication routines in libskey.a */

#define	SKEY_FAILURE	-1
#define	SKEY_SUCCESS	0
#define SKEY_NOT_FOUND	1

extern int	skey_haskey(char *name);
extern int	skey_authenticate(char *name);
extern int	keycrunch(char *result, char *seed, char *passwd);
extern char 	*btoe(char *engout, char *c);
extern char 	*readpass(char *buf, int n);
extern void	f(char *x);

/* token used in the passwd entry of diphosts to indicate
   that this user has S/Key authentication */
#define	SKEY_TOKEN	"s/key"

/* challenge string to look for */
#define SKEY_CHALLENGE	"[s/key"

#endif	/* SKEY */

/* End of DIP.H */
