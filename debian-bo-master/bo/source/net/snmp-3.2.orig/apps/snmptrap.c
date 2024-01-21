/*
 * snmptrap.c - send snmp traps to a network entity.
 *
 */
/***********************************************************
	Copyright 1989 by Carnegie Mellon University

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of CMU not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#ifndef linux
# include <nlist.h>
#else
#include <unistd.h>
#include <arpa/inet.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>
#endif

#include "snmp.h"
#include "asn1.h"
#include "snmp_impl.h"
#include "snmp_api.h"
#include "snmp_client.h"

extern int  errno;
int	snmp_dump_packet = 0;

#define NUM_NETWORKS	16   /* max number of interfaces to check */

oid objid_enterprise[] = {1, 3, 6, 1, 4, 1, 3, 1, 1};
oid objid_sysdescr[] = {1, 3, 6, 1, 2, 1, 1, 1, 0};

#ifndef linux
struct nlist nl[] = {
    { "_boottime" },
    { "" }
};
#endif

int snmp_input(){
  return 0;
}

#ifndef IFF_LOOPBACK
#define IFF_LOOPBACK 0
#endif
#define LOOPBACK    0x7f000001
u_long
get_myaddr(){
    int sd;
    struct ifconf ifc;
    struct ifreq conf[NUM_NETWORKS], *ifrp, ifreq;
    struct sockaddr_in *in_addr;
    int count;
    int interfaces;		/* number of interfaces returned by ioctl */

    if ((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
	return 0;
    ifc.ifc_len = sizeof(conf);
    ifc.ifc_buf = (caddr_t)conf;
    if (ioctl(sd, SIOCGIFCONF, (char *)&ifc) < 0){
	close(sd);
	return 0;
    }
    ifrp = ifc.ifc_req;
    interfaces = ifc.ifc_len / sizeof(struct ifreq);
    for(count = 0; count < interfaces; count++, ifrp++){
	ifreq = *ifrp;
	if (ioctl(sd, SIOCGIFFLAGS, (char *)&ifreq) < 0)
	    continue;
	in_addr = (struct sockaddr_in *)&ifrp->ifr_addr;
	if ((ifreq.ifr_flags & IFF_UP)
	    && (ifreq.ifr_flags & IFF_RUNNING)
	    && !(ifreq.ifr_flags & IFF_LOOPBACK)
	    && in_addr->sin_addr.s_addr != LOOPBACK){
		close(sd);
		return in_addr->sin_addr.s_addr;
	    }
    }
    close(sd);
    return 0;
}

/*
 * Returns uptime in centiseconds(!).
 */
long uptime(){
#ifndef linux
    struct timeval boottime, now, diff;
    int kmem;

    if ((kmem = open("/dev/kmem", 0)) < 0)
	return 0;
    nlist("/vmunix", nl);
    if (nl[0].n_type == 0){
	close(kmem);
	return 0;
    }
    
    lseek(kmem, (long)nl[0].n_value, L_SET);
    read(kmem, &boottime, sizeof(boottime));
    close(kmem);

    gettimeofday(&now, 0);
    now.tv_sec--;
    now.tv_usec += 1000000L;
    diff.tv_sec = now.tv_sec - boottime.tv_sec;
    diff.tv_usec = now.tv_usec - boottime.tv_usec;
    if (diff.tv_usec > 1000000L){
	diff.tv_usec -= 1000000L;
	diff.tv_sec++;
    }
    return ((diff.tv_sec * 100) + (diff.tv_usec / 10000));
#else /* linux */
    FILE *in = fopen ("/proc/uptime", "r");
    long uptim = 0, a, b;
    if (in)
      {
	  if (2 == fscanf (in, "%ld.%ld", &a, &b))
	    uptim = a * 100 + b;
	  fclose (in);
      }
    return uptim;
#endif /* linux */
}

u_long parse_address(address)
    char *address;
{
    u_long addr;
    struct sockaddr_in saddr;
    struct hostent *hp;

    if ((addr = inet_addr(address)) != -1)
	return addr;
    hp = gethostbyname(address);
    if (hp == NULL){
	fprintf(stderr, "unknown host: %s\n", address);
	return 0;
    } else {
	bcopy((char *)hp->h_addr, (char *)&saddr.sin_addr, hp->h_length);
	return saddr.sin_addr.s_addr;
    }

}

int
main(argc, argv)
    int	    argc;
    char    *argv[];
{
    struct snmp_session session, *ss;
    struct snmp_pdu *pdu;
    struct variable_list *vars;
    int	arg;
    char *gateway = NULL;
    char *community = NULL;
    char *trap = NULL, *specific = NULL, *description = NULL, *agent = NULL;


    /*
     * usage: snmptrap gateway-name community-name trap-type specific-type device-description [ -a agent-addr ]
     */
    for(arg = 1; arg < argc; arg++){
	if (argv[arg][0] == '-'){
	    switch(argv[arg][1]){
		case 'a':
		    agent = argv[++arg];
		    break;
		case 'd':
		    snmp_dump_packet++;
		    break;
		default:
		    printf("invalid option: -%c\n", argv[arg][1]);
		    break;
	    }
	    continue;
	}
	if (gateway == NULL){
	    gateway = argv[arg];
	} else if (community == NULL){
	    community = argv[arg]; 
	} else if (trap == NULL){
	    trap = argv[arg];
	} else if (specific == NULL){
	    specific = argv[arg];
	} else {
	    description = argv[arg];
	}
    }

    if (!(gateway && community && trap && specific && description)){
	printf("usage: snmptrap host community trap-type specific-type device-description [ -a agent-addr ]\n");
	exit(1);
    }

    bzero((char *)&session, sizeof(struct snmp_session));
    session.peername = gateway;
    session.community = (u_char *)community;
    session.community_len = strlen((char *)community);
    session.retries = SNMP_DEFAULT_RETRIES;
    session.timeout = SNMP_DEFAULT_TIMEOUT;
    session.authenticator = NULL;
    session.callback = snmp_input;
    session.callback_magic = NULL;
    session.remote_port = SNMP_TRAP_PORT;
    ss = snmp_open(&session);
    if (ss == NULL){
	printf("Couldn't open snmp\n");
	exit(-1);
    }

    pdu = snmp_pdu_create(TRP_REQ_MSG);
    pdu->enterprise = (oid *)malloc(sizeof(objid_enterprise));
    bcopy((char *)objid_enterprise, (char *)pdu->enterprise, sizeof(objid_enterprise));
    pdu->enterprise_length = sizeof(objid_enterprise) / sizeof(oid);
    if (agent != NULL)
	pdu->agent_addr.sin_addr.s_addr = parse_address(agent);
    else
	pdu->agent_addr.sin_addr.s_addr = get_myaddr();
    pdu->trap_type = atoi(trap);
    pdu->specific_type = atoi(specific);
    pdu->time = uptime();

    pdu->variables = vars = (struct variable_list *)malloc(sizeof(struct variable_list));
    vars->next_variable = NULL;
    vars->name = (oid *)malloc(sizeof(objid_sysdescr));
    bcopy((char *)objid_sysdescr, (char *)vars->name, sizeof(objid_sysdescr));
    vars->name_length = sizeof(objid_sysdescr) / sizeof(oid);
    vars->type = ASN_OCTET_STR;
    vars->val.string = (u_char *)malloc(strlen(description) + 1);
    strcpy((char *)vars->val.string, description);
    vars->val_len = strlen(description);

    if (snmp_send(ss, pdu)== 0){
	printf("error\n");
    }
    snmp_close(ss);

    return 0;
}

