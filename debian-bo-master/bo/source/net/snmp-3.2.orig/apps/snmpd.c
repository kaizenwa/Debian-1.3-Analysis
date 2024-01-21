/*
 * snmpd.c - rrespond to SNMP queries from management stations
 *
 */
/***********************************************************
	Copyright 1988, 1989 by Carnegie Mellon University

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

/*
 * additions, fixes and enhancements for Linux by 
 * Erik Schoenfelder <schoenfr@ibr.cs.tu-bs.de> and
 * Juergen Schoenwaelder <schoenw@cs.utwente.nl> 
 * June 1996.
 *
 * Please refer to ../README.linux for a summary of the changes and 
 * enhancements.
 */

/*
 * Small Linux specific patch by Patrick Weemeeuw
 * (patrick.weemeeuw@kulnet.kuleuven.ac.be)
 * to keep track of HrProcessorLoad more accurately.
 * -- 8 July 1996
 */


#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <sys/socket.h>
#include <errno.h>
#ifdef linux
#include <sys/time.h>
#include <arpa/inet.h>
#include <malloc.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <wait.h>
#include <ctype.h>
#endif

#include "snmp.h"
#include "snmp_impl.h"
#include "asn1.h"
#include "snmp_groupvars.h"

#ifdef linux
#include "hr_processor_load.h"
#endif

/*
 * -f option: fork a background demon: 
 */
int fork_daemon = 0;


extern int  errno;
#if 0
int	snmp_dump_packet = 0;
#else
extern int snmp_dump_packet;
#endif
int log_addresses = 0;

/* be somewhat verbose: */
extern int verbose;


/* from snmp_vars.c: */
extern char version_descr [256];

/* from snmplib/snmp.c: */
extern char trap_sink [256];
extern char trap_community [256];

/* fwd: */
static int receive();
static int agent_read();
/* from snmp_agent.c: */
extern int init_agent_auth();
/* from snmp_config.c: */
extern int read_config();
/* from snmp_agent.c: */
extern int snmp_agent_parse();
/* from snmp.c: */
extern void read_main_config_file();


/*
 * send a trap via snmptrap(1). ignore silently, if not avail.
 */
static void
send_trap (host, comm, dev, trap)
char *host, *comm, *dev;
int trap;
{
    char *cmd = "snmptrap";
    char trapt [20];
    int pid;

    sprintf (trapt, "%d", trap);

    if (! (pid = fork ())) {
	int fd = open ("/dev/null", O_RDWR);
	if (fd >= 0) {
	    dup2 (0, fd);
	    dup2 (1, fd);
	    dup2 (2, fd);
	}
	setsid();
	execlp (cmd, cmd, host, comm, trapt, "0", dev, (char *) 0);
	/* dont care   perror ("execlp"); */
	_exit (0);
    }
    else if (pid > 0)
      waitpid (pid, (int *) 0, 0);		/* simple wait should work */
}

void
send_easy_trap (trap)
int trap;
{
    /* from agent/snmp_vars.c: */
    extern int snmp_enableauthentraps;		/* default: 2 == disabled */

    if (snmp_enableauthentraps != 2)
      send_trap (trap_sink, trap_community, version_descr, trap);
}


/*
 * snmp port to use:
 */
static unsigned short snmp_port = SNMP_PORT;

/*
 * path to agentinfo and configfile: 
 */
char *snmp_agentinfo = "/etc/snmpd.agentinfo";
char *snmp_configfile = "/etc/snmpd.conf";

static void
usage ()
{
    printf ("\nuse: snmpd [-v] [-d] [-p <port>] [-f] [-a <agentinfo>] [-c <configfile>\n\n");
    exit (1);
}
    

int
main(argc, argv)
    int	    argc;
    char    *argv[];
{
    int	arg;
    int sd;
    struct sockaddr_in	me;

    /*
     * usage: snmpd
     */
    for(arg = 1; arg < argc; arg++){
	if (argv[arg][0] == '-'){
	    switch(argv[arg][1]){
		case 'd':
		    snmp_dump_packet++;
		    break;

		  case 'a':
		    if (arg + 1 >= argc)
		      usage ();
                    snmp_agentinfo = argv[++arg];
		    break;

		  case 'c':
		    if (arg + 1 >= argc)
		      usage ();
                    snmp_configfile = argv[++arg];
		    break;

		  case 'p':
		    if (arg + 1 >= argc)
		      usage ();
                    snmp_port = atoi(argv[++arg]);
                    break;

		case 'v':
		    verbose++;
		    break;

		case 'f':
		    fork_daemon = 1;
		    break;

		default:
		    printf("invalid option: -%c\n", argv[arg][1]);
		    usage ();
		    break;
		}
	    continue;
	} else {
	    usage ();
	}
    }
    /* Set up connections */
    sd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sd < 0){
	perror("socket");
	return 0;
    }
    me.sin_family = AF_INET;
    me.sin_addr.s_addr = INADDR_ANY;
    me.sin_port = htons(snmp_port);
    if (bind(sd, (struct sockaddr *)&me, sizeof(me)) != 0){
	perror("bind");
	return 0;
    }

    if (verbose) {
	printf ("listening on port %d\n", snmp_port);
    }

    /* read configs, before it's too late... */
    read_main_config_file ();
    
    /*
     * okey dokey: ready to fork and to serve:
     */
    if (fork_daemon) {
	int fd, pid;
	
	switch (pid = fork ()) {
	  case -1:
	    perror ("cannot fork - ignored; reason");
	    break;
	  case 0:
	    
	    /* to whom you would tell... */
	    verbose = 0;
	    log_addresses = 0;
	    snmp_dump_packet = 0;
	    
	    fd = open ("/dev/null", O_RDWR);
	    if (verbose) {
		fprintf (stderr, "forking background demon...\n");
		fflush (stderr);
	    }
	    if (fd > 0) {
		dup2(fd, 0);
		dup2(fd, 1);
		dup2(fd, 2);
	    }

	    /* detach from controling terminal ... */
            setsid ();

	    /* continue.... */
	    break;
	  default:
	    exit (0);
	}
    }

#ifdef linux
    init_HrProcessorLoad();	/* starts signal handler to keep track of
				   cpu load */
#endif

    init_snmp();
    if( init_agent_auth() < 0 ) exit( 1 );
    /*init_users();*/
    if( read_config() < 0 ) exit( 2 );

    /* set private community string, if read from config file: */
    {
	extern char *communities [];		/* agent/snmp_agent.c */
	extern char *public_community;		/* snmplib/snmp.c */
	extern char *private_community;		/* snmplib/snmp.c */
	if (public_community)
	  communities [0] = public_community;
	if (private_community)
	  communities [1] = private_community;
    }

    /* send coldstart trap via snmptrap(1) if possible */
    send_easy_trap (0);

    /* process incoming requests: */
    receive(sd);

    return 0;
}

static int
receive(sd)
    int sd;
{
    int numfds;
    fd_set fdset;
    int count;

    while(1){
	numfds = 0;
	FD_ZERO(&fdset);
	numfds = sd + 1;
	FD_SET(sd, &fdset);
	count = select(numfds, &fdset, 0, 0, 0);
	if (count > 0){
	    if(FD_ISSET(sd, &fdset))
		agent_read(sd);
	} else switch(count){
	    case 0:
		break;
	    case -1:
		if (errno == EINTR){
		    continue;
		} else {
		    perror("select");
		}
		return -1;
	    default:
		printf("select returned %d\n", count);
		return -1;
	}
    }
}


static int
agent_read(sd)
    int sd;
{
    struct sockaddr_in	from;
    int length, out_length, fromlength;
    u_char  packet[SNMP_MAX_LEN], outpacket[SNMP_MAX_LEN];

    fromlength = sizeof from;
    length = recvfrom(sd, packet, SNMP_MAX_LEN, 0, (struct sockaddr *)&from, &fromlength);
    if (length == -1) {
#ifdef linux
	if (errno != ECONNREFUSED)
#endif
	  perror("recvfrom");
    } else {
      snmp_inpkts++;
    }

    if (snmp_dump_packet){
	int count;

	printf("recieved %d bytes from %s:\n", length, inet_ntoa(from.sin_addr));
	for(count = 0; count < length; count++){
	    printf("%02X ", packet[count]);
	    if ((count % 16) == 15)
		printf("\n");
	}
	printf("\n\n");
    }
    out_length = SNMP_MAX_LEN;
    if (snmp_agent_parse(packet, length, outpacket, &out_length, from.sin_addr)){
	if (snmp_dump_packet){
	    int count;

	    printf("sent %d bytes to %s:\n", out_length, inet_ntoa(from.sin_addr));
	    for(count = 0; count < out_length; count++){
		printf("%02X ", outpacket[count]);
		if ((count % 16) == 15)
		    printf("\n");
	    }
	    printf("\n\n");
	}
	if (sendto(sd, (char *) outpacket, out_length, 0, 
		   (struct sockaddr *)&from, sizeof(from)) < 0){
#ifdef linux
	    if (errno != EINVAL) 
#endif
	      perror("sendto");
	    return 0;
	} else
	  snmp_outpkts++;

    }
    return 0;
}




/* 
 * added for the extensions of the linux port:
 */

/* tell something... */
int verbose = 0;

/*
 * store descriptions of interfaces:
 */

conf_if_list *if_list = 0;

static void 
add_interface_description (val)
char *val;
{
  char *name, *type = 0, *speed = 0;
  conf_if_list *nnew;

  name = strtok (val, " \t");
  if (name) type = strtok ((char *) 0, " \t");
  if (type) speed = strtok ((char *) 0, " \t");
  if (! type)
    {
      fprintf (stderr, 
   "warning: reading config: missing type for interface `%s'\n",
	       name ? name : "<unknown>");
      return;
    }

  if (! (nnew = (conf_if_list *) malloc (sizeof (conf_if_list)))
      || ! (nnew->name = strdup (name)))
    {
      fprintf (stderr, "error: reading config: out of memory...aborting.\n");
      exit (1);
    }
  
  nnew->type = atoi (type);
  nnew->speed = speed ? atoi (speed) : 0;

  if (verbose)
    printf ("added from config:  name: %s  type: %d  speed: %d\n", 
	    nnew->name, nnew->type, nnew->speed);

  nnew->next = if_list;
  if_list = nnew;
}







/*
 * read the main-config file (if avail) and initialize the interface 
 * parameter, contact, ...
 */

static int main_config_read = 0;		/* still read ? */


/* exported to ../agent/snmp_vars.c: */
char sysContact[256] = "Unknown";
char sysLocation[256] = "Unknown";
char sysName[256] = "";
char *public_community = 0;
char *private_community = 0;

/* port read from the configfile: */
int conf_snmp_port = -1;

/* trapsink host and community; setable by configfile: */
char trap_sink [256] = "localhost";
char trap_community [256] = "public";
int conf_authentraps = 0;

void
read_main_config_file ()
{
  FILE *in;
  char line [1024];

  if (main_config_read)
    return;

  /* only do this once: */
  main_config_read = 1;

  /* init path's: */
  { char *pfx = getenv ("SNMPCONFIGFILE");
    if (pfx && (pfx = strdup (pfx)))
      snmp_configfile = pfx;
  }
  
  if (verbose)
    printf ("reading main config file: %s\n", snmp_configfile);

  if (! (in = fopen (snmp_configfile, "r"))) {
      fprintf (stderr, "warning: cannot open %s - using default paths.\n",
	       snmp_configfile);
      return;
  }

  while (fgets (line, sizeof (line), in)) {
      char *key, *val;
      
      if (! *line || *line == '\n' || *line == '#')
	continue;
      if (line [strlen (line) - 1] == '\n')
	line [strlen (line) - 1] = 0;

      if (! (val = strchr (line, ':'))) {

	  if (strncmp (line, "view", 4) 
	      && strncmp (line, "user", 4)
	      && strncmp (line, "community", 9)) {
	      fprintf (stderr, 
		       "warning: reading config: don't know what to do "
		       " with this line:\n\t%s\n", line);
	  }
	  continue;
      }

      key = line;

      for (*val++ = 0; *val == ' ' || *val == '\t'; val++)
	continue;

      /* okey dokey; now we have a key and a value: */

      /** printf ("got key `%s' and val `%s'\n", key, val); **/
      
      if (! strcmp (key, "interface"))
	add_interface_description (val);
      else if (! strcmp (key, "system contact")) {
	  int len = sizeof (sysContact);
	  if (strlen (val) < len)
	    strcpy (sysContact, val);
	  else {
	      strncpy (sysContact, val, len - 1);
	      sysContact [len - 1] = 0;
	  }
	  
	  if (verbose)
	    printf ("added from config: system contact is %s\n", sysContact);

      } else if (! strcmp (key, "system location")) {
	  
	  int len = sizeof (sysLocation);
	  if (strlen (val) < len)
	    strcpy (sysLocation, val);
	  else {
	      strncpy (sysLocation, val, len - 1);
	      sysLocation [len - 1] = 0;
	  }
	  
	  if (verbose)
	    printf ("added from config: system location is %s\n", 
		    sysLocation);

      } else if (! strcmp (key, "system name")) {

	  int len = sizeof (sysName);
	  if (strlen (val) < len)
	    strcpy (sysName, val);
	  else {
	      strncpy (sysName, val, len - 1);
	      sysName [len - 1] = 0;
	  }
	  
	  if (verbose)
	    printf ("added from config: system name is %s\n", 
		    sysName);
      } else if (! strcmp (key, "public") || ! strcmp (key, "private")) {
	  static char comm [256];
	  char *s = 0;
	  int err = 0;
	  
	  if (! (s = strdup (val)))
	    {
		fprintf (stderr, "snmpd: out of mem - over and out.\n");
		exit (1);
	    }
	  
	  while (isspace (comm [strlen (s) - 1]))
	    s [strlen (s) - 1] = 0;
	  while (*s && isspace (*s))
	    s++;
	  if (! *s)
	    {
		fprintf (stderr,
			 "snmpd: empty %s community string.\n", key);
		err = 1;
	    }
	  
	  if (! err) {
	      if (! strcmp (key, "private"))
		private_community = s;
	      else
		public_community = s;
	  }
	  else
	    fprintf (stderr, 
		     "snmpd: default %s community will be used.\n", key);
      } else if (! strcmp (key, "port")) {
	  unsigned short p = atoi (val);
	  if (! val)
	    fprintf (stderr, "snmpd: no valid port found.\n");
	  else {
	      conf_snmp_port = p;
	      if (verbose)
		printf ("port set to %d\n", conf_snmp_port);
	  }
      } else if (! strcmp (key, "trap sink")) {
	  int len = sizeof (trap_sink);
	  if (strlen (val) < len)
	    strcpy (trap_sink, val);
	  else {
	      strncpy (trap_sink, val, len - 1);
	      trap_sink [len - 1] = 0;
	  }
	  
	  if (verbose)
	    printf ("added from config: trap sink addess is %s\n", 
		    trap_sink);
      }
      else if (! strcmp (key, "trap community"))
	{
	    int len = sizeof (trap_community);
	    if (strlen (val) < len)
	      strcpy (trap_community, val);
	    else {
		strncpy (trap_community, val, len - 1);
		trap_community [len - 1] = 0;
	    }
	    
	    if (verbose)
	      printf ("added from config: trap community string is %s\n", 
		      trap_community);
      } else if (! strcmp (key, "authentraps")) {
	  if (! strcmp (val, "yes"))
	    conf_authentraps = 1;
	  else if (! strcmp (val, "no"))
	    conf_authentraps = 2;
	  else
	    fprintf (stderr, 
		     "warning: reading config: unknown val for %s\n", key);
	  
	  if (verbose)
	    printf ("added from config: authentraps set to %s\n", val);
      } else {
	  fprintf (stderr, 
		   "warning: reading config: unknown key `%s'\n", key);
      }
    }
  fclose (in);
}

