/* 
 * Network accounting
 * capture-linux.c - capture raw packets - linux version
 * (C) 1994, 1995 Ulrich Callmeier
 */

#define IGNORE_UNENC 1

#include <sys/time.h>
#include <sys/wait.h>
#include "netacct.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>
#include <strings.h>
#include <signal.h>
#include <fcntl.h>
#include <utmp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netinet/protocols.h>
#include <linux/if.h>
#include <linux/if_ether.h>
#include <linux/ip.h>
#include <linux/tcp.h>

char *rcs_revision_capture_c = "$Revision: 1.3 $";

void handle_ip(unsigned char buf[], char *devname, char *user);

static int capture_sd = -1;

void init_capture()
/*
 * 1) Open our capture socket
 * 2) Set all the promisc devices to promiscous mode 
 */
{
    struct ifreq ifr;
    struct promisc_device *p;

    if ((capture_sd = socket (AF_INET, SOCK_PACKET, htons (ETH_P_ALL))) < 0)
	{
	    syslog(LOG_ERR, "can't get socket: %m\n");
	    daemon_stop(0);
	}

    p = cfg -> promisc;
    
    while(p!=NULL)
	{
	    strcpy (p -> oldifr.ifr_name, p -> name);
	    
	    if (ioctl (capture_sd, SIOCGIFFLAGS, &(p -> oldifr)) < 0)
		{
		    syslog(LOG_ERR, "can't get flags: %m\n");
		    daemon_stop(0);
		}
	    
	    p -> reset = 1;
	    
	    ifr = p -> oldifr;
	    ifr.ifr_flags |= IFF_PROMISC;
	    
	    if (ioctl (capture_sd, SIOCSIFFLAGS, &ifr) < 0)
		{
		    syslog(LOG_ERR, "can't set flags: %m\n");
		    daemon_stop(0);
		}

	    DEBUG(DBG_MISC, sprintf(dbg, "%s set to promiscous mode\n", p -> name));
	    
	    p = p -> next;
	}
}

void exit_capture(void)
{
    struct promisc_device *p;

    /* do we have to check (capture_sd >= 0) ? */

    p = cfg -> promisc;
    
    while(p != NULL)
	{
	    if (ioctl (capture_sd, SIOCSIFFLAGS, &(p -> oldifr)) < 0)
		{
		    syslog(LOG_ERR, "can't reset flags: %m\n");
		}
	    
	    p = p -> next;
	}
    
    close (capture_sd);
}

inline int onnet(unsigned long int addr, struct ipnetwork *net)
{
  return ((addr & net -> netmask) == net -> netnumber);
}

int onnetlist(unsigned long int addr, struct ipnetwork *netlist)
{
    while(netlist!=NULL)
	{
	    if(onnet(addr, netlist))
		{
		    return 1;
		}
	    netlist = netlist -> next;
	}
    return 0;
}

struct dynadat *dynadat = NULL;

char *check_user_dev2line(char *devname)
/*
 * Find username corresponding to devname
 */
{
    struct dev2line *d2l;
    char *line;
    struct dynadat *dd;
    int found;

    d2l = dev2line;
    line = NULL;

    while(d2l!=NULL)
	{
	    if(strcmp(d2l -> netinterface, devname)==0)
		{
		    line = d2l -> line;
		    break;
		}
	    
	    d2l = d2l -> next;
	}
    
    if(line == NULL)
	return NULL;
    
    dd = dynadat;

    found = 0;
    while(dd != NULL)
	{
	    if(strcmp(dd -> netinterface, devname) == 0)
		{
		    found = 1;
		    break;
		}
	    dd = dd -> next;
	}
	    
    if(!found)
	{
	    /* We don't have an entry for this device yet. Add one */

	    dd = malloc(sizeof(struct dynadat));
	    
	    dd -> netinterface = strdup(devname);
	    dd -> last_stat = 0; /* force reading of utmp */
	    dd -> mtime = 0;
	    dd -> user = NULL;
	    dd -> next = dynadat;
	    dynadat = dd;
	}

    /* dd now points to the right dynadat entry */
    /* maybe this is out of date, so we check */
	    
    if ((now - dd->last_stat) > FORCE_STAT_TIME )
	{
	    struct stat statbuf;
	    
	    /* it could be invalid, lets stat utmp */
	    
	    if(stat(_PATH_UTMP, &statbuf)==0)
	      {
		DEBUG(DBG_UTMP, sprintf(dbg, "%d: did a stat of %s\n",(int) now,_PATH_UTMP));
		
		dd -> last_stat = now;
		
		if((statbuf.st_mtime - dd->mtime) > 0)
		  {
		    struct utmp *ut_rec; /* utmp record */
		    
		    /* we have to wade through utmp */
		    DEBUG(DBG_UTMP, sprintf(dbg, "%d: wading through utmp %s\n",(int) now, _PATH_UTMP));
		    
		    dd -> mtime = statbuf.st_mtime;
		    
		    while ((ut_rec = getutent()))
		      {
			if ((ut_rec->ut_type == USER_PROCESS) &&
			    (ut_rec->ut_name[0] != '\000') &&
			    (strcmp(ut_rec->ut_line,line)==0))
			  {
			    if(dd -> user) free(dd -> user);
			    dd -> user = malloc(10);
			    strncpy(dd -> user, ut_rec->ut_user, 8);
			    dd->user[8] = 0;
	
			    DEBUG(DBG_DYNAMIC, sprintf(dbg, "found %s for %s\n",dd->user, line));

			    break;
			  }
		      }
		    endutent();
		  }
	      }
	    else
	      {
		syslog(LOG_ERR,"couldn't stat %s: %m\n",_PATH_UTMP);
		return NULL;
	      }
	  }
    
    return dd -> user;
}

char *check_user_dynamicip(__u32 addr)
/*
 * Find username corresponding to addr
 */
{
    struct dynadat *dd;
    int found;

    DEBUG(DBG_ANNOYING, sprintf(dbg, "check_user_dynamicip(%s)\n", intoa(addr)));

    dd = dynadat;

    found = 0;
    while(dd != NULL)
      {
	if(dd -> addr == addr)
	  {
	    found = 1;
	    break;
	  }
	dd = dd -> next;
      }
	    
    if(!found)
	{
	    /* We don't have an entry for this addr yet. Add one */

	    dd = malloc(sizeof(struct dynadat));
	    
	    dd -> addr = addr;
	    dd -> last_stat = 0; /* force reading of dir */
	    dd -> mtime = 0;
	    dd -> user = NULL;
	    dd -> next = dynadat;
	    dynadat = dd;

	    DEBUG(DBG_DYNAMIC, sprintf(dbg, "added entry for %s to dynadat list\n", intoa(addr)));
	    
	}

    /* dd now points to the right dynadat entry */
    /* maybe this is out of date, so we check */
	    
    if ((now - dd->last_stat) > FORCE_STAT_TIME )
      {
	struct stat statbuf;
	
	/* it could be invalid, lets stat dir */
	    
	if(stat(cfg -> dynamicip, &statbuf)==0)
	  {
	    DEBUG(DBG_DYNAMIC, sprintf(dbg, "%d: did a stat of %s, last_stat was %d, mtime was %d\n",(int) now, cfg->dynamicip, (int) dd->last_stat, (int) dd -> mtime));
	    
	    dd -> last_stat = now;
	    
	    if((statbuf.st_mtime - dd->mtime) > 0)
	      {
		FILE *f;
		char *s;
		/* we have to read dir */
		
		s = malloc(strlen(cfg->dynamicip) + 1 + 15 + 1 );
		
		strcpy(s, cfg->dynamicip);
		strcat(s, "/");
		strcat(s, intoa(addr));
		
		DEBUG(DBG_DYNAMIC, sprintf(dbg, "%d: reading %s\n",(int) now, s));
		
		if(dd -> user) free(dd -> user);
		dd -> user = NULL;
		dd -> mtime = statbuf.st_mtime;
		
		f = fopen(s, "r");
		
		if(f == NULL)
		  {
		    /* syslog(LOG_ERR,"couldn't fopen %s: %m\n",s); */
		    DEBUG(DBG_DYNAMIC, sprintf(dbg, "%d: couldn't fopen %s: %s\n",(int) now, s, strerror(errno)));
		    return NULL;
		  }
		
		dd -> user = malloc(BUFSIZ);
		
		fgets(dd -> user, BUFSIZ, f);
		if(dd->user[strlen(dd->user)-1]=='\n') dd->user[strlen(dd->user)-1]='\0';
		
		fclose(f);
		
		DEBUG(DBG_DYNAMIC, sprintf(dbg, "found %s for %s\n",dd->user, intoa(addr)));
		
	      }
	  }
	else
	  {
	    syslog(LOG_ERR,"couldn't stat %s: %m\n",cfg->dynamicip);
	    return NULL;
	  }
      }
    
    return dd -> user;
}

void packet_loop()
{
  struct sockaddr saddr;
  int sizeaddr;
  
  unsigned char buff[1600];
  unsigned char *buf;

  int hardheader;
  int length;
  static struct iphdr *tmp_iphdr;
  int type;

  int dynamicstyle;
  int do_user;
  __u32 dynamicaddr, otheraddr;
  
  char *user;
  
  struct promisc_device *p;

  dynamicstyle = (dev2line != NULL) ? 1 : ((cfg->dynamicip != NULL) ? 2 : 0);

  buf = &buff[20];

  while (running)
    {
      sizeaddr = 14;
      length = recvfrom (capture_sd, buf, 127, 0, &saddr, &sizeaddr);
      if (length == -1)
	{
	  if(errno != EINTR)
	    DEBUG(DBG_SYSCALL, sprintf(dbg, "recvfrom: %s\n", strerror(errno)));
	  continue;
	}
      
      do_user = 0;
      
      p = cfg -> notdev;
    
      while(p!=NULL)
	{
		if (!strcmp(p -> name, saddr.sa_data)) {
			packets->notdev++;
			break;
		}
		p = p -> next;
	}
	if (p)
		continue;

      if((strncmp(saddr.sa_data,"eth",3)==0) || strncmp(saddr.sa_data,"lo",2)==0)
	/* THIS NEEDS FIXING !! LIST IS INCOMPLETE */
	{
	  hardheader = 14;
	  type = (buf[12] * 256 + buf[13]);
		
	  if(type != ETH_P_IP)
	    {
	      /* ETH_P_ARP, ETH_P_RARP, ETH_P_IPX, etc. */
	      packets->ignored++;
	      continue;
	    }
	}
      else
	{
	  /* ASSUMES: interface - line just with ppp and slip etc. */
	  if(dynamicstyle == 1) do_user = 1;
	  
	  hardheader = 0;
	  type = 0;
	  packets->unenc++;
#ifdef IGNORE_UNENC
	  continue; /* ignore ppp/slip */
#endif
	}

      tmp_iphdr = (void *) (buf+hardheader);
      if((tmp_iphdr->saddr & cfg->ignoremask) == (tmp_iphdr->daddr & cfg->ignoremask))
	{
	  packets->local++;
	  continue;
	}
      else
	{
	  if(onnetlist(tmp_iphdr->saddr,cfg->ignorenet) || onnetlist(tmp_iphdr->daddr, cfg->ignorenet))
	    {
	      if(!(onnetlist(tmp_iphdr->saddr,cfg->dontignore) || onnetlist(tmp_iphdr->daddr, cfg->dontignore)))
		{
		  if(debug_level & DBG_IGNORE)
		    {
		      char tmp[18];
		      strcpy(tmp, intoa(tmp_iphdr->saddr));
		      DEBUG(DBG_IGNORE, sprintf(dbg, "netignored: %s -> %s\n",
						tmp,intoa(tmp_iphdr->daddr)));
		    }
		  packets->netignored++;
		  continue;
		}
	    }
	  packets->ip++;
	  user = NULL;
	 
	  switch(dynamicstyle)
	      {
	      case 1:
		if(do_user) user = check_user_dev2line(saddr.sa_data);
		break;
	      case 2:
		dynamicaddr = otheraddr = 0;
		if(onnet(tmp_iphdr->saddr, &cfg->dynamicnet)) 
		  {
		    dynamicaddr = tmp_iphdr->saddr;
		    if (onnet(tmp_iphdr->daddr, &cfg->dynamicnet)) otheraddr = tmp_iphdr->daddr;
		    DEBUG(DBG_ANNOYING, sprintf(dbg, "source %s is on dynamicnet\n", intoa(dynamicaddr)));

		  }
		else if (onnet(tmp_iphdr->daddr, &cfg->dynamicnet))
		  {
		    dynamicaddr = tmp_iphdr->daddr;
		    if(onnet(tmp_iphdr->saddr, &cfg->dynamicnet)) otheraddr = tmp_iphdr->saddr;
		    DEBUG(DBG_ANNOYING, sprintf(dbg, "destination %s is on dynamicnet\n", intoa(dynamicaddr)));
		  }
		
		if(dynamicaddr != 0)
		  {
		    if(onnetlist(dynamicaddr, cfg->excludenamelookup))
		      {
			DEBUG(DBG_ANNOYING, sprintf(dbg, "BUT: %s is excluded from name lookup\n", intoa(dynamicaddr)));

			dynamicaddr = otheraddr;
			otheraddr = 0;
			if(onnetlist(dynamicaddr, cfg->excludenamelookup))
			  {
			    DEBUG(DBG_ANNOYING, sprintf(dbg, "prev. bug: %s is excluded from name lookup, too\n", intoa(dynamicaddr)));
			    dynamicaddr = 0;
			  }
		      }
		    if(dynamicaddr != 0)
		      {
			user = check_user_dynamicip(dynamicaddr); 
			if((user == NULL) && (otheraddr != 0))
			  {
			    user = check_user_dynamicip(dynamicaddr); 
			  }
		      }
		  }
		break;
	      }
	    handle_ip(buf+hardheader, saddr.sa_data, user);
	  }
      }
}

void handle_ip(unsigned char buf[], char *devname, char *user)
{
    static struct iphdr *tmp_iphdr;
    static struct tcphdr *tmp_tcphdr;
    unsigned short srcport, dstport;
    tmp_iphdr = (void *) &buf[0];
    tmp_tcphdr = (void *) &buf[tmp_iphdr->ihl*4];
    /* relevant headers of udphdr and tcphdr are identical */

    switch(tmp_iphdr->protocol)
	{
	case IP_UDP:
	    packets->ip_udp++;
	    
	    srcport = ntohs(tmp_tcphdr->source);
	    dstport = ntohs(tmp_tcphdr->dest);

	    break;
	case IP_TCP:
	    packets->ip_tcp++;

	    srcport = ntohs(tmp_tcphdr->source);
	    dstport = ntohs(tmp_tcphdr->dest);

	    break;
	case IP_ICMP:
	    packets->ip_icmp++;
	    srcport = dstport = 0;
	    break;
	default:
	    packets->ip_other++;
	    srcport = dstport = 0;
	    break;
	}
    
    register_packet(tmp_iphdr->saddr,tmp_iphdr->daddr,tmp_iphdr->protocol, srcport, dstport, ntohs(tmp_iphdr->tot_len), devname, user);
}
