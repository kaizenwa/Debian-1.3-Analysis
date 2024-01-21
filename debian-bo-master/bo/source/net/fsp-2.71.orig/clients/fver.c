    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "tweak.h"
#include "client_def.h"
#include "c_extern.h"
#include "version.h"
#include <stdio.h>

int main PROTO3(int, argc, char **, argv, char **, envp)
{
  UBUF *ub;
  unsigned int len, tput = 0, len2;
  char *v1, *v2;
  
  if(argc == 1)	{ /* no arg supplied, get version string of server */
    env_client();
    ub = client_interact(CC_VERSION,0L, 0, (unsigned char *)NULLP, 0,
			 (unsigned char *)NULLP);
    len = BB_READ2(ub->bb_len);
    len2 = BB_READ4(ub->bb_pos);
    v1 = ub->buf; v2 = ub->buf+len;
    printf("FSP version: %s\n",v1);
    /* we have a new type of server */
    if(len2) {
      if(*v2 & VER_LOG)
	printf("\tLogging of all server transactions is ENABLED.\n");
      else
	printf("\tLogging of all server transactions is DISABLED.\n");
      if(*v2 & VER_READONLY)
	printf("\tRemote server is run in READONLY mode.\n");
      else
	printf("\tRemote server is run in READ/WRITE mode.\n");
      if(*v2 & VER_REVNAME)
	printf("\tServer REQUIRES connections to reverse name.\n");
      else
	printf("\tServer DOESN'T REQUIRE connections to reverse.\n");
      if(*v2 & VER_PRIVMODE)
	printf("\tRemote server is run in PRIVATE mode.\n");
      else
	printf("\tRemote server is run in PUBLIC mode.\n");
      if(*v2 & VER_THRUPUT) {
	printf("\tRemote server throughput control is ENABLED.");
	tput |= (((unsigned)*(++v2) << 24) & 0xff000000);
	tput |= (((unsigned)*(++v2) << 16) & 0x00ff0000);
	tput |= (((unsigned)*(++v2) << 8) & 0x0000ff00);
	tput |= ((unsigned)*(++v2) & 0x000000ff);
	printf(" (max %d bytes/sec)\n", tput);
      } else
	printf("\tRemote server throughput control is DISABLED.\n");
    }
    client_done();
  } else
    printf("Local FSP version: %s\n",VERSION_STR);
  exit(0);
}
