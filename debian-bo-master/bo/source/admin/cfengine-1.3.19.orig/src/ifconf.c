/* cfengine for GNU
 
        Copyright (C) 1995
        Free Software Foundation, Inc.
 
   This file is part of GNU cfengine - written and maintained 
   by Mark Burgess, Dept of Computing and Engineering, Oslo College,
   Dept. of Theoretical physics, University of Oslo
 
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/
 

/*******************************************************************/
/*                                                                 */
/*  INET checking for cfengine                                     */
/*                                                                 */
/*  This is based on the action of "ifconfig" for IP protocols     */
/*  It assumes that we are on the internet and uses ioctl to get   */
/*  the necessary info from the device. Sanity checking is done... */
/*                                                                 */
/* Sockets are very poorly documented. The basic socket adress     */
/* struct sockaddr is a generic type. Specific socket addresses    */
/* must be specified depending on the family or protocol being     */
/* used. e.g. if you're using the internet inet protocol, then     */
/* the fmaily is AF_INT and the socket address type is sockadr_in  */
/* Although it is not obvious, the documentation assures us that   */
/* we can cast a pointer of one type into a pointer of the other:  */
/*                                                                 */
/* Here's an example                                               */
/*                                                                 */
/*   #include <netinet/in.h>                                       */
/*                                                                 */
/*        struct in_addr adr;                                      */
/* e.g.   adr.s_addr = inet_addr("129.240.22.34");                 */
/*        printf("addr is %s\n",inet_ntoa(adr));                   */
/*                                                                 */
/*                                                                 */
/* We have to do the following in order to convert                 */
/* a sockaddr struct into a sockaddr_in struct required by the     */
/* ifreq struct!! These calls have no right to work, but somehow   */
/* they do!                                                        */
/*                                                                 */
/* struct sockaddr_in sin;                                         */
/* sin.sin_addr.s_addr = inet_addr("129.240.22.34");               */
/*                                                                 */
/* IFR.ifr_addr = *((struct sockaddr *) &sin);                     */
/*                                                                 */
/* sin = *(struct sockaddr_in *) &IFR.ifr_addr;                    */
/*                                                                 */
/* printf("IP address: %s\n",inet_ntoa(sin.sin_addr));             */
/*                                                                 */
/*******************************************************************/

#define INET

/* IRIX makes the routing stuff obsolete unless we do this */
#undef sgi

#include "cf.defs.h"
#include "cf.extern.h"

struct ifreq IFR;

#define cfproto 0

#ifndef IPPROTO_IP     /* Old boxes, hpux 7 etc */
# define IPPROTO_IP 0
#endif

#ifndef SIOCSIFBRDADDR
# define SIOCSIFBRDADDR  SIOCGIFBRDADDR
#endif

/*******************************************************************/

IfConf (class)

enum classes class;

{ int sk, flags, metric, isnotsane = false;

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can configure the net interface.\n");
   return;
   }

if (VNETMASK[0] == '\0')
   {
   FatalError("Program does not define a subnetmask");
   }

if (VBROADCAST[0] == '\0')
   {
   FatalError("Program does not define a broadcast mode for this host");
   }


strcpy(IFR.ifr_name,VIFDEV[class]);
IFR.ifr_addr.sa_family = AF_INET;

Verbose("Interface name : %s\n",VIFDEV[class]);

if ((sk = socket(AF_INET,SOCK_DGRAM,IPPROTO_IP)) == -1)
   {
   perror("cfengine: socket");
   FatalError("Error in IfConfig()");
   }

if (ioctl(sk,SIOCGIFFLAGS, (caddr_t) &IFR) == -1)   /* Get the device status flags */
   {
   perror ("ioctl #1:");
   FatalError("Software error: no such ethernet device");
   }

flags = IFR.ifr_flags;
strcpy(IFR.ifr_name,VIFDEV[class]);                   /* copy this each time */
 
if (ioctl(sk,SIOCGIFMETRIC, (caddr_t) &IFR) == -1)   /* Get the routing priority */
   {
   perror ("ioctl #2:");
   FatalError("Software error: error getting metric");
   }

metric = IFR.ifr_metric;

isnotsane = GetIfStatus(sk,class);

if (! DONTDO && isnotsane)
   {
   SetIfStatus(sk,class);
   GetIfStatus(sk,class);
   }

close(sk);
}


/*******************************************************************/

GetIfStatus(sk,class)

int sk;
enum classes class;

{ struct sockaddr_in *sin;
  struct sockaddr_in netmask;
  int insane = false;
  struct hostent *hp;
  struct in_addr inaddr;

if ((hp = gethostbyname(VSYSNAME.nodename)) == NULL)
   {
   perror("GetIfStatus(): gethostbyname: ");
   return false;
   }
else
   {
   bcopy(hp->h_addr,&inaddr, hp->h_length);
   Verbose("Address given by nameserver: %s\n",inet_ntoa(inaddr));
   }

strcpy(IFR.ifr_name,VIFDEV[class]);

if (ioctl(sk,SIOCGIFADDR, (caddr_t) &IFR) == -1)   /* Get the device status flags */
   {
   return false;
   }

sin = (struct sockaddr_in *) &IFR.ifr_addr;

Verbose("IP address on %s: %s\n",VIFDEV[class],inet_ntoa(sin->sin_addr));  /* man inet */

if (strcmp((char *)inet_ntoa(*(struct in_addr *)(hp->h_addr)),(char *)inet_ntoa(sin->sin_addr)) != 0)
   {
   printf("cfengine: This machine is configured with an address which differs from\n");
   printf("          the nameserver's information! (Insane!)\n");
   printf("          Don't quite know what to do...\n");
   insane = true;
   }

if (ioctl(sk,SIOCGIFNETMASK, (caddr_t) &IFR) == -1) 
   {
   return;
   }

netmask.sin_addr = ((struct sockaddr_in *) &IFR.ifr_addr)->sin_addr;

Verbose("Netmask: %s\n",inet_ntoa(netmask.sin_addr));

strcpy(VBUFF,inet_ntoa(netmask.sin_addr));

if (strcmp(VBUFF,VNETMASK))
   {
   printf("cfengine : The netmask is incorrectly configured, resetting...\n");
   insane = true;
   }

if (ioctl(sk,SIOCGIFBRDADDR, (caddr_t) &IFR) == -1) 
   {
   return false;
   }

sin = (struct sockaddr_in *) &IFR.ifr_addr;
strcpy(VBUFF,inet_ntoa(sin->sin_addr));

Verbose("Broadcast address: %s\n",inet_ntoa(sin->sin_addr));

GetBroadcastAddr(inet_ntoa(inaddr));

if (strcmp(VBUFF,VBROADCAST) != 0)
   {
   printf("cfengine: broadcast address was %s (should be %s)\n",VBUFF,VBROADCAST);
   insane = true;
   }

return(insane);
}

/*******************************************************************/

SetIfStatus(sk,class)

int sk;
enum classes class;

{ struct sockaddr_in *sin;
  struct sockaddr_in netmask, broadcast;

   /*********************************

   Don't try to set the address yet...

    if (ioctl(sk,SIOCSIFADDR, (caddr_t) &IFR) == -1) 
      {
      perror ("Can't set IP address");
      return;
      } 

   **********************************/

/* set netmask */

strcpy(IFR.ifr_name,VIFDEV[class]);
netmask.sin_addr.s_addr = inet_network(VNETMASK);
IFR.ifr_addr = *((struct sockaddr *) &netmask);
sin = (struct sockaddr_in *) &IFR.ifr_addr;

if (ioctl(sk,SIOCSIFNETMASK, (caddr_t) &IFR)) 
   {
   printf("cfengine: Permission to reconfigure netmask denied.\n");
   perror("ioctl");
   }
else
   {
   printf("cfengine: Set Netmask to: %s\n",inet_ntoa(netmask.sin_addr));
   }

/* broadcast addr */

strcpy(IFR.ifr_name,VIFDEV[class]);
broadcast.sin_addr.s_addr = inet_addr(VBROADCAST);
IFR.ifr_addr = *((struct sockaddr *) &broadcast);
sin = (struct sockaddr_in *) &IFR.ifr_addr;

if (ioctl(sk,SIOCSIFBRDADDR, (caddr_t) &IFR) == -1) 
   {
   printf("cfengine: Permission to reconfigure broadcast denied.\n");
   perror("ioctl");
   return;
   } 

if ((char *) sin->sin_addr.s_addr == NULL)
   {
   printf("cfengine : No broadcast address on socket after configuration!!\n");
   }
else
   {
   printf("cfengine : Set Broadcast address to: %s\n",inet_ntoa(sin->sin_addr));
   }
}

/*****************************************************/

GetBroadcastAddr(ipaddr)

char *ipaddr;

{ unsigned int na,nb,nc,nd;
  unsigned int ia,ib,ic,id;
  unsigned int ba,bb,bc,bd;
  unsigned netmask,ip,broadcast;

sscanf(VNETMASK,"%u.%u.%u.%u",&na,&nb,&nc,&nd);

netmask = nd + 256*nc + 256*256*nb + 256*256*256*na;

sscanf(ipaddr,"%u.%u.%u.%u",&ia,&ib,&ic,&id);

ip = id + 256*ic + 256*256*ib + 256*256*256*ia;

if (strcmp(VBROADCAST,"zero") == 0)
   {
   broadcast = ip & netmask;
   }
else if (strcmp(VBROADCAST,"one") == 0)
   {
   broadcast = ip | (~netmask);
   }
else
   {
   return;
   }

ba = broadcast / (256 * 256 * 256);
bb = (broadcast / (256 * 256)) % 256;
bc = broadcast / (256) % 256;
bd = broadcast % 256;
sprintf(VBROADCAST,"%u.%u.%u.%u",ba,bb,bc,bd);
}

/****************************************************************/
/*                                                              */
/* Routing Tables:                                              */
/*                                                              */
/* To check that we have at least one static route entry to     */
/* the nearest gateway -- i.e. the wildcard entry for "default" */
/* we need some way of accessing the routing tables. There is   */
/* no elegant way of doing this, alas.                          */
/*                                                              */
/****************************************************************/

SetDefaultRoute()

{ int sk, defaultokay = 1;
  struct sockaddr_in sindst,singw;

#ifdef HAVE_ORTENTRY
  struct ortentry route;
#else
  struct rtentry route;
#endif

  FILE *pp;

if (getuid() != 0)                            
   {
   printf("cfengine: Only root can set a default route.\n");
   return;
   }

if (VDEFAULTROUTE[0] == '\0')
   {
   if (VERBOSE || DEBUG)
      {
      printf("cfengine: No default route is defined. Ignoring the routing tables.\n");
      }
   return;
   }

if ((pp = popen(VNETSTAT[VSYSTEMHARDCLASS],"r")) == NULL)
   {
   printf("cfengine: failed to open pipe from %s\n",VNETSTAT[VSYSTEMHARDCLASS]);
   return;
   }

while (!feof(pp))
   {
   fgets(VBUFF,bufsize,pp);
   if (strncmp(VBUFF,"default",7) == 0)
      {
      if (strstr(VBUFF,VDEFAULTROUTE))
         {
         if (VERBOSE || DEBUG)
            {
            printf("cfengine: default route is already set to %s\n",VDEFAULTROUTE);
            }
         defaultokay = 1;
         break;
         }
      else
         {
         printf("cfengine: the default packet-route is incorrectly set\n");
         printf("          Please correct this manually using route(1).\n");
         break;
         }
      }
   defaultokay = 0;
   }

pclose(pp);

if (defaultokay)
   {
   return;
   }

if ((sk = socket(AF_INET,SOCK_RAW,cfproto)) == -1)
   {
   if ( VSYSTEMHARDCLASS == linuxx )
      {
      Debug ("No raw socket protocol for linux\n");
      return;
      }
   
   printf("System class %s\n",CLASSTEXT[VSYSTEMHARDCLASS]);
   perror("cfengine: socket");
   FatalError("Error in SetDefaultRoute()");
   }

sindst.sin_family = AF_INET;
singw.sin_family = AF_INET;

sindst.sin_addr.s_addr = INADDR_ANY;
singw.sin_addr.s_addr = inet_addr(VDEFAULTROUTE);

route.rt_dst = *(struct sockaddr *)&sindst;      /* This disgusting method is necessary */
route.rt_gateway = *(struct sockaddr *)&singw;
route.rt_flags = RTF_GATEWAY;

if (! DONTDO)
   {
   if (ioctl(sk,SIOCADDRT, (caddr_t) &route) == -1)   /* Get the device status flags */
      {
      perror ("ioctl SIOCADDRT:");
      FatalError("Software error: set default route");
      }

   printf("cfengine: The routing table did not contain a default route.\n");
   printf("          I'm setting it to %s\n",VDEFAULTROUTE);
   }
}
