/*
 * This combines all term clients into one binary to save 
 * disk space for those with very small disk space quotas.
 *
 */

#define I_STRING
#include "includes.h"

int trdate(int,char **);
int tdownload(int,char **);
int tupload(int,char **);
int tudpredir(int,char **);
int tredir(int,char **);
int tmon(int,char **);
int tshutdown(int,char **);
int trsh(int,char **);
int txconn(int,char **);

int main(int argc, char *argv[])
{
  char *p,*q=NULL;

  if(argv[0]) {

    for(p = argv[0];p;p = strchr(p,'/')) q=p++;
  
    while(*q == '-' || *q == '/') ++q;
  
    if (!strcmp(q,"trdate")) 
      exit(trdate(argc,argv));
    else if (!strcmp(q,"trdated")) 
      exit(trdate(argc,argv));
    else if (!strcmp(q,"tdownload")) 
      exit(tdownload(argc,argv));
    else if (!strcmp(q,"tupload")) 
      exit(tupload(argc,argv));
    else if (!strcmp(q,"tudpredir")) 
      exit(tudpredir(argc,argv));
    else if (!strcmp(q,"tredir")) 
      exit(tredir(argc,argv));
    else if (!strcmp(q,"tmon")) 
      exit(tmon(argc,argv));
    else if (!strcmp(q,"tshutdown")) 
      exit(tshutdown(argc,argv));
    else if (!strcmp(q,"trsh")) 
      exit(trsh(argc,argv));
    else if (!strcmp(q,"txconn")) 
      exit(txconn(argc,argv));
  }
  fprintf(stderr,"Unrecognized program name.\n");
  exit(1);
}
  
