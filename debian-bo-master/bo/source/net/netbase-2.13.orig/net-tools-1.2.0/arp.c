
/*
 * arp		This file contains an implementation of the command
 *		that maintains the kernel's ARP cache.  It is derived
 *		from Berkeley UNIX arp(8), but cleaner and with sup-
 *		port for devices other than Ethernet.
 *
 * Usage:	arp [-vn] [-p proto] [-t type] -a [hostname]
 *		arp [-v] [-p proto] -d hostname ...
 *		arp [-v] [-p proto] [-t type] -s hostname hw_addr
 *						[temp|pub|rarp|trail]
 *		arp [-vn] -f filename
 *
 * Version:	@(#)arp.c	1.50	01/20/94
 *
 * Author: 	Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
 *
 * Modified for NET3 by Alan Cox.
 * Modified for proxy arp netmasks by Andrew Tridgell
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include "support.h"
#include "pathnames.h"
#include "version.h"
#include "config.h"
#include "net-locale.h"

#define DFLT_AF	"inet"
#define DFLT_HW	"ether"


char *Release = RELEASE,
     *Version = "@(#) arp 1.50 (01/20/94)";


int opt_n = 0;				/* do not resolve addresses	*/
int opt_v = 0;				/* debugging output flag	*/
struct aftype *ap;			/* current address family	*/
struct hwtype *hw;			/* current hardware type	*/
int skfd;				/* active /proc  descriptor	*/
int sockfd;				/* active socket descriptor     */


static void usage(void);


/* Delete an entry from the ARP cache. */
static int
arp_del(char **args)
{
  char host[128];
  struct arpreq req;
  struct sockaddr sa;

  /* Resolve the host name. */
  if (*args == NULL) {
	fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_hostname, "arp: need host name\n"));
	return(-1);
  }
  strcpy(host, *args);
  if (ap->input(host, &sa) < 0) {
	ap->herror(host);
	return(-1);
  }

  /* If a host has more than one address, use the correct one! */
  memcpy((char *) &req.arp_pa, (char *) &sa, sizeof(struct sockaddr));

  /* Call the kernel. */
  if (ioctl(sockfd, SIOCDARP, &req) < 0) {
	if (errno != ENXIO) {
		perror("SIOCDARP");
		return(-1);
	} else printf(NLS_CATGETS(catfd, arpSet, arp_no_arp, "No ARP entry for %s\n"), host);
  }

  return(0);
}


/* Set an entry in the ARP cache. */
static int
arp_set(char **args)
{
  char host[128];
  struct arpreq req;
  struct sockaddr sa;
  int flags;

  /* Resolve the host name. */
  if (*args == NULL) {
	fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_hostname, "arp: need host name\n"));
	return(-1);
  }
  strcpy(host, *args++);
  if (ap->input(host, &sa) < 0) {
	ap->herror(host);
	return(-1);
  }

  /* If a host has more than one address, use the correct one! */
  memcpy((char *) &req.arp_pa, (char *) &sa, sizeof(struct sockaddr));

  /* Fetch the hardware address. */
  if (*args == NULL) {
	fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_need_hw, "arp: need hardware address\n"));
	return(-1);
  }
  if (hw->input(*args++, &req.arp_ha) < 0) {
	fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_invalidhw, "arp: invalid hardware address\n"));
	return(-1);
  }

  /* Check out any modifiers. */
  flags = ATF_PERM;
  while (*args != NULL) {
	if (! strcmp(*args, "temp")) flags &= ~ATF_PERM;
	if (! strcmp(*args, "pub")) flags |= ATF_PUBL;
/*	if (! strcmp(*args, "rarp")) flags |= ATF_RARP;*/
	if (! strcmp(*args, "trail")) flags |= ATF_USETRAILERS;
	if (! strcmp(*args, "netmask")) 
	  {
	    if (*++args == NULL) usage();
	    if (strcmp(*args,"255.255.255.255") != 0)
	      {
		strcpy(host, *args);
		if (ap->input(host, &sa) < 0) {
		  ap->herror(host);
		  return(-1);
		}
		memcpy((char *) &req.arp_netmask, (char *) &sa,
		       sizeof(struct sockaddr));
		flags |= ATF_NETMASK;
	      }
	  }
	args++;
  }

  if ((flags & ATF_NETMASK) && !(flags & ATF_PUBL))
    usage();

  /* Fill in the remainder of the request. */
  req.arp_flags = flags;

  /* Call the kernel. */
  if (ioctl(sockfd, SIOCSARP, &req) < 0) {
	perror("SIOCSARP");
	return(-1);
  }
  return(0);
}


/* Process an EtherFile */
static int
arp_file(char *name)
{
  char buff[1024];
  char *sp, *args[32];
  int linenr, argc;
  FILE *fp;

  if ((fp = fopen(name, "r")) == NULL) {
	fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_cant_open, "arp: cannot open etherfile %s !\n"), name);
	return(-1);
  }

  /* Read the lines in the file. */
  linenr = 0;
  while (fgets(buff, sizeof(buff), fp) != (char *)NULL) {
	linenr++;
	if (opt_v == 1) fprintf(stderr, ">> %s", buff);
	if ((sp = strchr(buff, '\n')) != (char *)NULL) *sp = '\0';
	if (buff[0] == '#' || buff[0] == '\0') continue;

	argc = getargs(buff, args);
	if (argc < 2) {
		fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_formaterr, 
					    "arp: format error on line %u of etherfile %s !\n"),
			linenr, name);
		continue;
	}

	if (arp_set(args) != 0) {
		fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_cant_set,
					    "arp: cannot set entry on line %u of etherfile %s !\n"),
			linenr, name);
	}
  }

  (void) fclose(fp);
  return(0);
}


/* Print the contents of an ARP request block. */
static void
arp_disp(struct arpreq *req)
{
  static int title = 0;
  struct hwtype *xhw;
  struct aftype *xap;
  char *sp, flags[6];

  /* Fetch the hardware type, which was given by the kernel. */
  if(req->arp_ha.sa_family==0)
  	req->arp_ha.sa_family=ARPHRD_ETHER;
  xhw = get_hwntype(req->arp_ha.sa_family);
  if (xhw == NULL) xhw = get_hwtype("ether");
  xap = get_afntype(req->arp_pa.sa_family);
  if (xap == NULL) xap = get_aftype("inet");
  
  if (title++ == 0) printf(NLS_CATGETS(catfd, arpSet, arp_address1,
				       "Address\t\t\tHW type\t\tHW address\t\tFlags\n"));

  /* Setup the flags. */
  flags[0] = '\0';
  if (req->arp_flags & ATF_COM) strcat(flags, "C");
  if (req->arp_flags & ATF_PERM) strcat(flags, "M");
  if (req->arp_flags & ATF_PUBL) strcat(flags, "P");
/*  if (req->arp_flags & ATF_RARP) strcat(flags, "R");*/
  if (req->arp_flags & ATF_USETRAILERS) strcat(flags, "T");

  sp = xap->print(req->arp_pa.sa_data/*, opt_n*/);
  printf("%-23.23s\t%-16.16s", sp, xhw->title);
  printf("%-21.21s\t%-6s\n", xhw->sprint(&req->arp_ha), flags);
}


/* Print the contents of an ARP request block. */
static void
arp_disp_2(char *ip,int type,int arp_flags,char *hw,char *mask)
{
  static int title = 0;
  struct hwtype *xhw;
  struct aftype *xap;
/*  char *sp; */ 
  char flags[6];

  xhw = get_hwntype(type);
  if (xhw == NULL) 
    xhw = get_hwtype("ether");
/*
 * xap = get_afntype(req->arp_pa.sa_family);
 * if (xap == NULL) 
 */
  xap = get_aftype("inet");
  
  if (title++ == 0) 
    printf(NLS_CATGETS(catfd, arpSet, arp_address2,
		       "Address\t\t\tHW type\t\tHW address\t\tFlags\tMask\n"));

  /* Setup the flags. */
  flags[0] = '\0';
  if (arp_flags & ATF_COM) strcat(flags, "C");
  if (arp_flags & ATF_PERM) strcat(flags, "M");
  if (arp_flags & ATF_PUBL) strcat(flags, "P");
/*  if (arp_flags & ATF_RARP) strcat(flags, "R");*/
  if (arp_flags & ATF_USETRAILERS) strcat(flags, "T");

#ifdef NOTDEF
  sp = xap->print(req->arp_pa.sa_data/*, opt_n*/);
#endif
  printf("%-23.23s\t%-16.16s", ip /* sp */, xhw->title);
  printf("%-21.21s\t%-6s\t%s\n", hw /* xhw->sprint(&req->arp_ha) */, flags,mask);
}


/* Display the contents of the ARP cache in the kernel. */
static int
pr_arps(void)
{
  char ip[100];
  char hw[100];
  char mask[100];
  char line[200];
  int type,flags;
  FILE *fp;
  int num;

  /* Open the PROCps kernel table. */
  if ((fp = fopen(_PATH_PROCNET_ARP, "r")) == NULL) {
	perror(_PATH_PROCNET_ARP);
	return(-1);
  }

  /* Bypass header -- read until newline */
  if (fgets(line, sizeof(line), fp) != (char *)NULL)
    {
      /* check to see if this kernel has an arp netmask */
      int has_mask = (strstr(line,"Mask") != NULL);

      /* Read the ARP cache entries. */
      for(;;)
	{
	  *mask = 0;
	  if (has_mask)
	    num=fscanf(fp,"%s 0x%x 0x%x %s %s",ip,&type,&flags,hw,mask);
	  else
	    num=fscanf(fp,"%s 0x%x 0x%x %s",ip,&type,&flags,hw);
	  if(num<4)
	    break;

	  arp_disp_2(ip,type,flags,hw,mask);
	}
    }

  (void) fclose(fp);
  return(0);
}


/* Show one or more entries from the ARP cache. */
static int
arp_show(char *name)
{
  char host[128];
  struct sockaddr sa;
  struct arpreq req;

  /* Do we have to show the whole table? */
  if (name == NULL) return(pr_arps());

  /* Nope.  Resolve the host name. */
  strcpy(host, name);
  if (ap->input(host, &sa) < 0) {
	fprintf(stderr, "arp: %s: %s\n", host, strerror(errno));
	return(-1);
  }

  /* If a host has more than one address, use the correct one! */
  memcpy((char *) &req.arp_pa, &sa, sizeof(struct sockaddr));
  req.arp_ha.sa_family = AF_UNSPEC;	/* any type */

  /* Call the kernel. */
  if (ioctl(sockfd, SIOCGARP, &req) < 0) {
	if (errno != ENXIO) perror("SIOCGARP");
	  else printf(NLS_CATGETS(catfd, arpSet, arp_no_arp, "No ARP entry for %s\n"), host);
        return(-1);
  }
 
  /* We found it.  Display the results. */
  arp_disp(&req);

  return(0);
}


static void
usage(void)
{
  fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_usage1,
	"Usage: arp [-vn] [-p proto] [-t type] -a [hostname]\n"));
  fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_usage2,
	"       arp [-v] [-p proto] -d hostname ...\n"));
  fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_usage3,
	"       arp [-v] [-p proto] [-t type] -s hostname hw_addr [netmask aa.bb.cc.dd] [temp|pub|trail]\n"));
  fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_usage4,
	"       arp [-vn] -f filename\n"));
  fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_usage5,
	"       Note: netmask may only be used with the pub option\n"));
  NLS_CATCLOSE(catfd)
  exit(-1);
}


int
main(int argc, char **argv)
{
  int c, what;

#if NLS
  setlocale (LC_MESSAGES, "");
  catfd = catopen ("nettools", MCLoadBySet);
#endif

  /* Initialize variables... */
  if ((hw = get_hwtype(DFLT_HW)) == NULL) {
	fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_hw_not_supp,
				    "%s: hardware type not supported!\n"), DFLT_HW);
	NLS_CATCLOSE(catfd)
	return(-1);
  }
  if ((ap = get_aftype(DFLT_AF)) == NULL) {
	fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_fam_not_supp,
				    "%s: address family not supported!\n"), DFLT_AF);
	NLS_CATCLOSE(catfd)
	return(-1);
  }
  what = -1;

  /* Fetch the command-line arguments. */
  opterr = 0;
  while ((c = getopt(argc, argv, "adfp:nst:v")) != EOF) switch(c) {
	case 'a':
		what = 1;
		break;

	case 'd':
		what = 3;
		break;

	case 'f':
		what = 2;
		break;

	case 'n':
		opt_n = 1;
		break;

	case 'p':
		ap = get_aftype(optarg);
		if (ap == NULL) {
			fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_unkn_addr,
						    "arp: %s: unknown address family.\n"),
				optarg);
			NLS_CATCLOSE(catfd)
			exit(-1);
		}
		break;

	case 's':
		what = 4;
		break;

	case 't':
		hw = get_hwtype(optarg);
		if (hw == NULL) {
			fprintf(stderr, NLS_CATGETS(catfd, arpSet, arp_unkn_hw,
						    "arp: %s: unknown hardware type.\n"),
				optarg);
			NLS_CATCLOSE(catfd)
			exit(-1);
		}
		break;

	case 'v':
		opt_v = 1;
		break;

	default:
		usage();
  }

  /* Create an entry point into the ARP protocol. */
  if ((skfd = open(_PATH_PROCNET_ARP, O_RDONLY)) < 0) {
	perror(_PATH_PROCNET_ARP);
	NLS_CATCLOSE(catfd)
	exit(-1);
  }
  if ((sockfd = socket(AF_INET,SOCK_DGRAM,0)) <0)
  {
  	perror("socket");
	NLS_CATCLOSE(catfd)
  	exit(-1);
  }

  /* Now see what we have to do here... */
  switch(what) {
	case 1:		/* show an ARP entry in the cache */
		what = arp_show(argv[optind]);
		break;

	case 2:		/* process an EtherFile */
		what = arp_file(argv[optind]);
		break;

	case 3:		/* delete an ARP entry from the cache */
		what = arp_del(&argv[optind]);
		break;

	case 4:		/* set an ARP entry in the cache */
		what = arp_set(&argv[optind]);
		break;

	default:
		usage();
  }

  (void) close(skfd);
  NLS_CATCLOSE(catfd)
  exit(what);
}

