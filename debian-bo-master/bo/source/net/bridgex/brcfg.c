/*
 * This is a bridge configuration tool for Linux Kernels 2.1.X
 * Taken from a free floating sourcecode and revised/improved/hacked upon
 * by Christoph Lameter <clameter@debian.org>
 * 12 September 1996 Revised syntax, expanded the tool to use all options
 *	that the bridge offers. Added protocol filtering support.
 */

#include <asm/byteorder.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <linux/skbuff.h>
/* #include <net/br.h> */
/* #include "/usr/src/linux/include/net/br.h" */
#include "br.h"


struct br_stat br_stats;
int fd;

struct prot {
 int nr;
 char *name;
} protocols[] =
{ 0x0060,"LOOP",
  0x0200,"ECHO",
  0x0400,"PUP",
  0x0800,"IP",
  0x0805,"X25",  
  0x0806,"ARP", 
  0x08FF,"BPQ",
  0x6000,"DEC",
  0x6001,"DNA_DL",
  0x6002,"DNA_RC",
  0x6003,"DNA_RT",
  0x6004,"LAT",
  0x6005,"DIAG",
  0x6006,"CUST",
  0x6007,"SCA",
  0x8035,"RARP",
  0x809B,"ATALK",
  0x80F3,"AARP",
  0x8137,"IPX",
  0x86DD,"IPV6",
  0x0001,"802_3",
  0x0002,"AX25",
  0x0004,"802_2",
  0x0005,"SNAP",
  0x0006,"DDCMP",
  0x0011,"TR_802_2",
  0,NULL
};

void disp_protocol(int protocol)
{
	struct prot *p;
	for (p=protocols;p->nr;p++) {
		if (protocol==p->nr) { printf(p->name);return; }
	}
	printf("0x%04X",protocol);
};

int
disp_id(id)
bridge_id_t *id;
{
	printf("0x%04x %02x:%02x:%02x:%02x:%02x:%02x",
		id->BRIDGE_PRIORITY,
		id->BRIDGE_ID_ULA[0],
		id->BRIDGE_ID_ULA[1],
		id->BRIDGE_ID_ULA[2],
		id->BRIDGE_ID_ULA[3],
		id->BRIDGE_ID_ULA[4],
		id->BRIDGE_ID_ULA[5]);
	return(0);
};

int disp_ports(ports, n)
Port_data *ports;
int n;
{
	int i;
	int flags = 0;

	printf("--- port stats ---\n");
	for (i=0; i<n; i++) {
		if (ports[i].state == Disabled)
			continue;
		printf("port %i\t",i);
		printf("port id 0x%04x\t",ports[i].port_id);
		printf("port state	");
		switch(ports[i].state) {
			case Disabled:
				printf("DISABLED (0x%x)\n",ports[i].state);
				break;
			case Listening:
				printf("LISTENING (0x%x)\n",ports[i].state);
				break;
			case Learning:
				printf("LEARNING (0x%x)\n",ports[i].state);
				break;
			case Forwarding:
				printf("FORWARDING (0x%x)\n",ports[i].state);
				break;
			case Blocking:
				printf("BLOCKING (0x%x)\n",ports[i].state);
				break;
		}	
		printf("designated root		");
		disp_id(&ports[i].designated_root);
		printf("\n");
		printf("designated bridge	");
		disp_id(&ports[i].designated_bridge);
		printf("\n");
		printf("path cost		%i\t",ports[i].path_cost);
		printf("designated cost		%i\n",ports[i].designated_cost);
		printf("designated port		%i\t",ports[i].designated_port);
		printf("flags			");
		if (ports[i].top_change_ack) {
			printf("TOPOLOGY_CHANGE_ACK ");
			flags++;
		}
		if (ports[i].config_pending) {
			printf("CONFIG_PENDING");
			flags++;
		}
		if (!flags)
			printf("NONE");
		printf("\n");
	}
	return(0);
};

/*
 * Display current bridge status
 */
 
char *policies[2] = { "Reject all protocols","Accept all protocols"};

int disp_bridge(Bridge_data *bridge)
{
	int flags = 0;

	printf("bridge id		");
	disp_id(&bridge->bridge_id);
	printf("\n");
	printf("designated root		");
	disp_id(&bridge->designated_root);
	printf("\n");
	printf("bridge max age		%i\t",bridge->bridge_max_age);
	printf("max age			%i\n",bridge->max_age);
	printf("bridge hello time	%i\t",bridge->bridge_hello_time);
	printf("hello time		%i\n",bridge->hello_time);
	printf("bridge forward delay	%i\t",bridge->bridge_forward_delay);
	printf("forward delay		%i\n",bridge->forward_delay);
	printf("root path cost		%i\t",bridge->root_path_cost);
	printf("root port		%i\n",bridge->root_port);
	printf("flags			");
	if (bridge->top_change) {
		printf("TOPOLOGY_CHANGE ");
		flags++;
	}
	if (bridge->top_change_detected) {
		printf("TOPOLOGY_CHANGE_DETECTED");
		flags++;
	}
	if (!flags)
		printf("NONE");
	printf("\n");
	return(0);
}

void bridge_status(void)
{	int i;
	if (ioctl(fd, SIOCGIFBR, &br_stats) < 0) {
		printf("Kernel Bridge Module missing.\n");
		exit(1);
	}
	printf("bridging is %sABLED \tdebugging is %sABLED\tprot-stats are %sABLED\n", 
		br_stats.flags & BR_UP?"EN":"DIS",
		br_stats.flags & BR_DEBUG?"EN":"DIS",
		br_stats.flags & BR_PROT_STATS?"EN":"DIS");
	disp_bridge(&br_stats.bridge_data);
	disp_ports(&br_stats.port_data, 8);
	printf("Policy                  %s\n",policies[br_stats.policy]);
	printf("exempt protocols        %i ",br_stats.exempt_protocols);
	if (br_stats.exempt_protocols)
	{ putchar('(');
	  for(i=0;i<BR_MAX_PROTOCOLS;i++)
	  if (br_stats.protocols[i])
	  { disp_protocol(br_stats.protocols[i]);putchar(' ');
	  }
	  putchar(')');
	}
	printf("\n");
}

void cmd(int cmd,int arg1,int arg2)
{ struct br_cf bcf;

	bcf.cmd = cmd;
	bcf.arg1 = arg1;
	bcf.arg2 = arg2;
	if (ioctl(fd, SIOCSIFBR, &bcf) < 0) {
		perror("ioctl(SIOCSIFBR) failed");
		exit(1);
	}
}

void help(void)
{ printf("brcfg - Bridge Configuration tool v0.2\n"
"--------------------------------------\n"
"brcfg sta[rt]				Start Bridge\n"
"brcfg sto[p]				Stop Bridge\n"
"brcfg p[ort] x e[nable]			Enable a port\n"
"brcfg p[ort] x d[isable]		Disable a port\n"
"brcfg p[ort] x p[riority] y		Set the priority of a port\n"
"brcfg pr[iority] y			Set bridge priority\n"
"brcfg pa[thcost] y			Set the pathcosts\n"
"brcfg d[ebug] on			Switch debugging on\n"
"brcfg d[ebug] off			Switch debugging off\n"
"brcfg pol[icy] r[eject]/a[ccept]	Switch the policy/flush protocol list\n"
"brcfg e[xempt] <protocol> ..		Set list of exempt protocols\n"
"brcfg l[ist]				List available protocols\n"
"brcfg stat[s] z[ero]			Reset Statistics counters"
"brcfg stat[s] d[isable]		Switch protocol statistics off"
"brcfg stat[s] e[nable]			Switch keeping protocol statistics on"
"brcfg stat[s] s[how]			Show protocol statistics"
"brcfg statu[s]				Show bridge status\n"
"\n"
"Examples:\n"
"---------\n"
"brcfg start exempt atalk aarp		Bridge start dont do LocalTalk bridging\n"
"\n"
"brcfg stop				Bridge stop\n");
}

void debug(char *option)
{ if (strcasecmp(option,"on")==0)
  { cmd(BRCMD_ENABLE_DEBUG,0,0);
    printf("Debug on.\n");
  } else
  if (strcasecmp(option,"off")==0)
  { cmd(BRCMD_DISABLE_DEBUG,0,0);
    printf("Debug off.\n");
  } else
  { printf("Debug mode can only be on or off!");
    exit(1);
  }
}

void exempt(char *protocol)
{ /* identify protocol */

  struct prot *p;

  for(p=protocols;p->nr && strcasecmp(protocol,p->name)!=0;p++) ;
  if (p->nr)
  { cmd(BRCMD_EXEMPT_PROTOCOL,p->nr,0);
    printf("Exempt protocol ");disp_protocol(p->nr);printf(".\n");
    return;
  }
  printf("Protocol to be exempted %s not in protocol list\n(See /usr/include/net/if_ether.h)\n",protocol);
  exit(1);
}

void listprots(void)
{ struct prot *p;
  printf("Protocol Listing:\n------------------\n");
  for (p=protocols;p->nr;p++) printf("0x%04X %s\n",p->nr,p->name);
}

void policy(char *kind)
{ if (kind[0]=='a')
  { cmd(BRCMD_SET_POLICY,1,0);
    printf("Policy accept all protocols.\n");
  } else
  if (kind[0]=='r')
  { cmd(BRCMD_SET_POLICY,0,0);
    printf("Policy reject all protocols.\n");
  } else
  { printf("Policy must be either accept or deny.\n");
    exit(1);
  }
}

void pathcost(char *port,char *cost)
{ int p=atoi(port),c=atoi(cost);
  cmd(BRCMD_SET_PATH_COST,p,c);
  printf("Pathcost for port %d set to %d",p,c);
}

void port(char *no,char *mode)
{ int port=atoi(no);
  if (mode[0]=='e')
  { cmd(BRCMD_PORT_ENABLE,port,0);
    printf("Enable Port %d\n",port);
  } else
  if (mode[0]=='d')
  { cmd(BRCMD_PORT_DISABLE,port,0);
    printf("Disable Port %d\n",port);
  } else
  { printf("Port option can only be enable or disable");
  }
}

void portprior(char *port,char *prio)
{ int po=atoi(port),pr=atoi(prio);
  cmd(BRCMD_SET_PORT_PRIORITY,po,pr);
  printf("Port Priority for port %d set to %d\n",po,pr);
}

void priority(char *prio)
{ int p=atoi(prio);
  cmd(BRCMD_SET_BRIDGE_PRIORITY,p,0);
  printf("Bridge priority set to %d\n",p);
}

void bridge_stat(char *a)
{	int x;
	switch (a[0]) {
		case 'e' : 
			cmd(BRCMD_ENABLE_PROT_STATS,0,0);
			printf("Protocol Statistics enabled\n");
			break;
		case 'd' : 
			cmd(BRCMD_DISABLE_PROT_STATS,0,0);
			printf("Protocol Statistics disabled\n");
			break;
		case 'z' : 
			cmd(BRCMD_ZERO_PROT_STATS,0,0);
			printf("Protocol Statistics counters reset.\n");
			break;
		case 's' :
			if (ioctl(fd, SIOCGIFBR, &br_stats) < 0) {
				printf("Kernel Bridge Module missing.\n");
				exit(1);
			}
			printf("Protocol statistics are %s\n",(br_stats.flags & BR_PROT_STATS) ? "ENABLED" : "DISABLED");
			if (br_stats.prot_id[0])
			{
				printf("Protocol Number of Packets encountered\n");
				printf("--------------------------------------\n");
				for(x=0;x<BR_MAX_PROT_STATS && br_stats.prot_id[x]!=0;x++) {
					disp_protocol(br_stats.prot_id[x]);printf(" %d\n",br_stats.prot_counter[x]);
				}
				if (br_stats.prot_id[BR_MAX_PROT_STATS-1]) {
					printf("Warning: Protocol table too short. Possibly not all protocols listed!n");
				}
			} else {
				printf("No protocol data has been gathered by the kernel\n");
			}
			break;
		default  : printf("stat argument must be enable,disable,zero or show\n");
			exit(1);
			break;
	}
}

int main(int argc, char **argv)
{
	fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd <= 0) {
		perror("socket failed: Probably bridge not compiled into kernel");
                return(-1);
        };
	if (argc==1) bridge_status();
	else
	for (argv++;*argv;argv++)
	{	char *p;
		p=*argv; while (*p=='-' || *p=='/') p++;
	  	switch (*p) {
			case 'h' : help();break;
			case 'd' : debug(*++argv);break;
			case 'e' : for(argv++;*argv;argv++) exempt(*argv);
			        argv--;
				break;
			case 'p' : if (p[2]=='l') { policy(*++argv);break; }
				else if (p[2]=='a') { pathcost(*++argv,*++argv);break; }
				else if (p[1]=='r') { priority(*++argv);break; }
				else
				if (argv[2][0]=='p')
				{ portprior(argv[1],argv[3]);argv+=3;
				  break;
				}
				else
				{  port(*++argv,*++argv);break;
				}
			case 's' : if (p[3]=='r') {
					cmd(BRCMD_BRIDGE_ENABLE,0,0);
					printf("Bridge started.\n");
					break;
				}
				else if (p[2]=='o') {
					cmd(BRCMD_BRIDGE_DISABLE,0,0);
					printf("Bridge stopped.\n");
					break;
				}
				else if (p[3]=='t') {
				    if (argv[1]==NULL) bridge_stat("show");
				     else bridge_stat(*++argv);
				} else
				bridge_status();
				break;
			case 'l' : listprots();break;
			default : printf("Unknown option %s\n",p);exit(1);
		}
	}

	close(fd);
	return 0;
}		
