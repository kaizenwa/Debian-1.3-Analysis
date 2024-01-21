/*
 * Show ICMP packets incoming (and detect bombs)
 *   modified from ping
 *   1994 - Laurent Demailly - <dl@hplyot.obspm.fr>
 */

/* note : the original bsd code was *very* buggy !!!
          it should be ok, now */

/*
 * Print out the packet, if it came from us.  This logic is necessary
 * because ALL readers of the ICMP socket get a copy of ALL ICMP packets
 * which arrive ('tis only fair).  This permits multiple copies of this
 * program to be run without having intermingled output (or statistics!).
 */

#include	"defs.h"

#ifndef ANSI_OFFSETOF
#ifndef offsetof
#        define offsetof(t,m)  (int)((&((t *)0L)->m))
#endif
#endif

char to_hex(a)
  int a;
{
  return ((char)(a <= 9 ? a + '0' : (a -10) + 'A'));
}

int pr_pack(buf, cc, from)
char			*buf;	/* ptr to start of IP header */
int			cc;	/* total size of received packet */
struct sockaddr_in	*from;	/* address of sender */
{
  int			iphdrlen,doipdecoding=1;
  struct ip		*ip;	/* ptr to IP header */
  register struct icmp	*icp;	/* ptr to ICMP header */
  struct tcphdr 	*tp;    /* ptr to TCP header */
  time_t			t;
  char			*pr_type(),*pr_subtype(),*strtime;
  struct hostent	*hostent=NULL;
  struct servent	*servent=NULL;
  static char prbuf[1024];	/* provide enough room for even the longest hosts*/
	
  /*
   * We have to look at the IP header, to get its length.
   * We also verify that what follows the IP header contains at
   * least an ICMP header (8 bytes minimum).
   */
  ip = (struct ip *) buf;
  iphdrlen = ip->ip_hl << 2;	/* convert # 16-bit words to #bytes */
  if (cc < iphdrlen + ICMP_MINLEN) {
    sprintf(prbuf,"packet too short (%d bytes) from %s", cc,
	    inet_ntoa(from->sin_addr));
    if (syslogdoutput) {
      syslog(LOG_WARNING,"%s",prbuf);
      } else {
	puts(prbuf);
	fflush(stdout);
      }
    return -1;
  }
  cc -= iphdrlen;

  icp = (struct icmp *)(buf + iphdrlen);
  switch (icp->icmp_type) 
    {
    case ICMP_ECHO :
    case ICMP_ECHOREPLY :
      doipdecoding=0;
      if (verbose<2) break;
    case ICMP_SOURCEQUENCH :
    case ICMP_TIMXCEED : 
    case ICMP_REDIRECT :
      if (!verbose) break;
    default :
      if (!nonamequery) {
	hostent=gethostbyaddr((char*)&(from->sin_addr.s_addr),
			      sizeof (struct in_addr),
			      AF_INET);
      }
      if (!syslogdoutput) {
	t=time((time_t *)NULL); strtime=ctime(&t);
	strtime+=4;		/* skip day name */
	strtime[15]=0;		/* keep MMM DD HH:MM:SS */
        printf("%s ",strtime);
      }
      sprintf(prbuf,hostent?"ICMP_%s%s < %s [%s]":"ICMP_%s%s < %s",
	      pr_type(icp->icmp_type),
	      icp->icmp_type==ICMP_UNREACH?pr_subtype(icp->icmp_code):"",
	      inet_ntoa(from->sin_addr),
	      hostent?hostent->h_name:NULL
	      );
      if ( doipdecoding && 
           ( cc >= offsetof(struct icmp,icmp_ip.ip_src)+sizeof(icp->icmp_ip.ip_dst) ) )
	{
	  if (showsrcip) 
	    { /*  icp->icmp_ip.ip_src.s_addr == local host, show it
		  only if requested (might be usefull for host with several
		  interfaces */
	      if (!nonamequery) {
		hostent=gethostbyaddr((char*)&(icp->icmp_ip.ip_src.s_addr),
				      sizeof (struct in_addr),
				      AF_INET);
	      }
	      sprintf(prbuf+strlen(prbuf),hostent?" - %s [%s]":" - %s",
		      inet_ntoa(icp->icmp_ip.ip_src),
		      hostent?hostent->h_name:NULL);
	    }
	  if (cc>=offsetof(struct icmp,icmp_ip.ip_dst)+sizeof(icp->icmp_ip.ip_dst))
	    {
	      if (!nonamequery) {
		hostent=gethostbyaddr((char*)&(icp->icmp_ip.ip_dst.s_addr),
				      sizeof (struct in_addr),
				      AF_INET);
	      }
	      sprintf(prbuf+strlen(prbuf),hostent?" > %s [%s]":" > %s",
		      inet_ntoa(icp->icmp_ip.ip_dst),
		      hostent?hostent->h_name:NULL);
	      tp = (struct tcphdr *)((char *)&(icp->icmp_dun)+sizeof(struct ip)) ;
	      if (cc>=offsetof(struct icmp,icmp_dun)+sizeof(struct ip)+offsetof(struct tcphdr,th_seq)+sizeof(tp->th_seq))
		{
		  if (noportquery) {
		      sprintf(prbuf+strlen(prbuf)," sp=%d dp=%d seq=0x%8.8x",
			  ntohs(tp->th_sport),ntohs(tp->th_dport),
                          ntohl(tp->th_seq));
		  } else {
		    if ((servent=getservbyport(ntohs(tp->th_sport),NULL))) 
		      sprintf(prbuf+strlen(prbuf)," sp=%d [%s]",
			      ntohs(tp->th_sport),servent->s_name);
		    else
		      sprintf(prbuf+strlen(prbuf)," sp=%d",tp->th_sport);
		    if ((servent=getservbyport(ntohs(tp->th_dport),NULL))) 
		      sprintf(prbuf+strlen(prbuf)," dp=%d [%s] seq=0x%8.8x",
			      ntohs(tp->th_dport),servent->s_name,
			      ntohl(tp->th_seq));
		    else
		      sprintf(prbuf+strlen(prbuf)," dp=%d seq=0x%8.8x",
			      ntohs(tp->th_dport),ntohl(tp->th_seq));
		  }
		}
	    }
	}
      sprintf(prbuf+strlen(prbuf)," sz=%d(+%d)",cc,iphdrlen);
      if (syslogdoutput) {
	  syslog(LOG_NOTICE,"%s",prbuf);
	} else {
	  puts(prbuf);
	  fflush(stdout);
	  if (verbose>2) {	/* hexa dump adapted from a file dump by dl (me!) */
	    /* certainly not the smartest around, but it works !*/
	    static char	h[] = "                                          ";
	    static char	a[] = "                ";
	    int	i,j,b,n, flagNEof;
	    unsigned char	*pbuf=(unsigned char *)buf;
	
	    n = 0;
	    flagNEof = 1;
	    while (flagNEof) {
	      i = j = 0;
	      while (i < 16 && (flagNEof = cc--)) {
		b= (int)(*(pbuf++));
		h[j++] = to_hex(b >> 4);
		h[j++] = to_hex(b & 0x0F);
		j += i % 2 + ((i == 7) << 1);
		a[i++] = (b > 31 && b < 127) ? b : '.';
	      }
	      if (i==0) break;
	      while (i < 16) {
		h[j++] = ' ';
		h[j++] = ' ';
		j += i % 2 + ((i == 7) << 1);
		a[i++] = ' ';
	      }
	      printf("%04X :  %s   %s\n", n, h, a);
	      n += 16;
	    }
	  }
	}
    }
  return 0;
}

/*
 * Convert an ICMP "type" field to a printable string.
 * This is called for ICMP packets that are received that are not
 * ICMP_ECHOREPLY packets.
 */

char *
pr_type(t)
register int t;
{
	static char	*ttab[] = {
		"Echo_Reply",
		"1",
		"2",
		"Dest_Unreachable",
		"Source_Quench",
		"Redirect",
		"6",
		"7",
		"Echo",
		"RouterAdvert",
		"Router_Solicit",
		"Time_Exceeded",
		"Parameter_Problem",
		"Timestamp",
		"Timestamp_Reply",
		"Info_Request",
		"Info_Reply",
		"Mask_Request",
		"Mask_Reply"
	};

	if (t < 0 || t > 18)
		return("OUT_OF_RANGE");
	return(ttab[t]);
}

/*
 * Convert an ICMP UNREACH sub-"type" field to a printable string.
 */

char *
pr_subtype(t)
register int t;
{
	static char	*ttab[] = {
	  "Net",
	  "Host",
	  "Protocol",
	  "Port",
	  "Frag",
	  "Source",
	  "DestNet",
	  "DestHost",
	  "Isolated",
	  "AuthNet",
	  "AuthHost",
	  "NetSvc",
	  "HostSvc"
	  };
	static char buf[80];
	
	if (t < 0 || t > 12)
		return("[--Sub-Type-OUT-OF-RANGE--]");
	sprintf(buf,"[%s]",ttab[t]);
	return(buf);
}

