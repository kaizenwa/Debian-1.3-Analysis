/* Command line test of simple binding name service */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ilu_simpbind.h"

extern ilu_simpbind_Server ilu_simpbind_create_server (ilu_Server,	/* server */
						       ilu_string,	/* ih */
						       char *,		/* backing file name */
						       ilu_boolean);	/* protected */

static void usage(char *pname)
{
  fprintf (stderr, "Usage:  %s [-s] [-p PORT] [-h HOSTNAME-OR-IP-ADDR] [-r REALM-NAME] [-f BACKING-FILE]\n", pname);
  exit(1);
}

int main (int ac, char **av)
{
  ilu_simpbind_Server theSB;
  ilu_Server theServer;
  int stop;
  char *filename = ILU_NIL;
  char *pinfo = "sunrpc";
  char *tinfo[3] = { "sunrpcrm", ILU_NIL, ILU_NIL };
  char realm_name[1024];
  char hostname[1024];
  char tcpinfo[1100];
  char sid[1100];
  unsigned long rawport;
  ilu_shortcardinal port;
  ilu_boolean protected = ilu_FALSE;
  int i = 1;

  struct hostent *he;
  struct in_addr *hea;

  ilu_GetSBServiceParms(realm_name, hostname, &port);
  while (i < ac) {
    if (strcmp(av[i], "-r") == 0) {
      if (i++ < ac)
	strcpy(realm_name, av[i++]), i++;
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-p") == 0) {
      if (i++ < ac) {
	(rawport = strtoul(av[i], ILU_NIL, 0)), i++;
	if (rawport >= 0 && rawport <= 0xFFFF)
	  port = rawport;
	else
	  usage(av[0]);
      }
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-h") == 0) {
      if (i++ < ac)
	strcpy (hostname, av[i]), i++;
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-s") == 0) {
      protected = ilu_TRUE;
      i++;
    } else if (strcmp(av[i], "-f") == 0) {
      if (i++ < ac)
	filename = av[i++];
      else
	usage(av[0]);
    } else usage(av[0]);
  }    

  /* initialize simpbind interface */
  ilu_simpbind__InitializeServer();

  sprintf (sid, "ILUSimpleBindingService.%s", realm_name);
  sprintf (tcpinfo, "tcp_%s_%d", hostname, port);
  tinfo[1] = tcpinfo;

  /* make kernel server */
  theServer = ILU_C_InitializeServer(sid,
				     NULL, /* use std object table */
				     pinfo,
				     tinfo,
				     ILU_NIL, /* no identity at present */
				     ilu_TRUE /* establish as default server */
				     );
  
  if (filename == ILU_NIL)
    {
      sprintf (hostname, "/tmp/%s", sid);
      filename = hostname;
    }

  theSB = ilu_simpbind_create_server (theServer, "Server", filename, protected);
  if (theSB == ILU_NIL)
    {
      fprintf (stderr, "Error creating server object.\n");
      return 1;
    }
  else
    {
      printf ("Simple binding server ready; SBH is \"%s\".\n",
	      ILU_C_SBHOfObject(theSB));
      ilu_RunMainLoop (&stop);
    }
  return 0;
}



