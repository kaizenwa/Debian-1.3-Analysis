/*
 * This is my implementation of the domainname command.
 *    usage: domainname [nisdomain]
 * without argument: prints the current domainname
 * with argument: sets the domainname (you must be root to do this)
 * --Swen (swen@uni-paderborn.de)
 */

/*
 * $Log: domainname.c,v $
 * Revision 2.2  1995/01/24 12:24:45  swen
 * Added RCS keywords.
 *
 */

static char rcsid[] = "$Id: domainname.c,v 2.2 1995/01/24 12:24:45 swen Exp $" ;
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define DOMAINNAMELEN 64+1

void
usage(void)
{
  puts("Usage: domainname [nisdomain]") ;
  exit(1) ;
}

int
main(int argc, char *argv[])
{
  char *name, *tmp ;
  
  if (argc > 2 || (argc == 2 && argv[1][0] == '-'))
      usage();

  name = (char *) malloc(DOMAINNAMELEN) ;
  if (name == NULL)
    {
      perror("malloc") ;
      exit(1) ;
    }
  if (argc < 2)
    {
      if (getdomainname(name, DOMAINNAMELEN) < 0) 
	{
	  perror("getdomainname");
	  free(name);
	  exit(1);
	}
      else
	puts(name);
    }
  else
    {
      name = strcpy(name, argv[1]) ;
      tmp = strchr(name, '\n') ;
      if (tmp != NULL)
	*tmp = '\0' ;
      if (setdomainname(name, strlen(name)) < 0)
        {
  	  perror("setdomainname");
	  free(name);
	  exit(1);
        }
     }
  free(name);
  return(0);
}

