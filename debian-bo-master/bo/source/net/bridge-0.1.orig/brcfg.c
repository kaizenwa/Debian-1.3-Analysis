
#include <sys/types.h>
#include <sys/socket.h>
#include <skbuff.h>

#include "br.h"


struct br_stat br_stats;
int cmd = 0;
int disable = 0;
int enable = 0;
int port = 0;
int debug = 0;

int
main(argc, argv)
int argc;
char **argv;
{
	int fd;
	struct br_cf *bcf;

	argc--, *argv++;
	while (argc--) {
		if (strncmp(*argv,"-dis",4) == 0) {
			disable = 1;
			*argv++;
			continue;
		}
		if (strncmp(*argv,"-ena",4) == 0) {
			enable = 1;
			*argv++;
			continue;
		}
		if (strncmp(*argv,"-deb",4) == 0) {
			debug = 1;
			*argv++;
			continue;
		}
		if (strncmp(*argv,"-por",4) == 0) {
			*argv++;
			argc--;
			port = atoi(*argv);
			*argv++;
			continue;
		}
	}
	fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd < 0) {
		perror("socket failed");
		return(-1);
	}
	if (port) {	/* port specific commands... */
		if (enable)
			cmd = BRCMD_PORT_ENABLE;
		if (disable)
			cmd = BRCMD_PORT_DISABLE;
	} else if (debug) {	/* debugging */
		if (enable)
			cmd = BRCMD_ENABLE_DEBUG;
		if (disable)
			cmd = BRCMD_DISABLE_DEBUG;
	} else {	/* bridge specific commands */
		if (enable)
			cmd = BRCMD_BRIDGE_ENABLE;
		if (disable)
			cmd = BRCMD_BRIDGE_DISABLE;
	}
	if (cmd) {
		bcf = (struct br_cf *)&br_stats;
		bcf->cmd = cmd;
		bcf->arg1 = port;
		if (ioctl(fd, SIOCSIFBR, &br_stats) < 0) {
			perror("ioctl(SIOCSIFBR) failed");
			return(-1);
		}
	}
	if (ioctl(fd, SIOCGIFBR, &br_stats) < 0) {
		perror("ioctl(SIOCGIFBR) failed");
		return(-1);
	}
	if (cmd) {

	}
	printf("bridging is %sABLED \tdebugging is %sABLED\n", 
		br_stats.flags & BR_UP?"EN":"DIS",
		br_stats.flags & BR_DEBUG?"EN":"DIS");
	disp_bridge(&br_stats.bridge_data);
	disp_ports(&br_stats.port_data, 8);
	close(fd);
}		

int
disp_bridge(bridge)
Bridge_data *bridge;
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
}

int
disp_ports(ports, n)
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
}
