#include <stdio.h>
#include <stdlib.h>
#ifndef vms
#include <malloc.h>
#endif
#include <time.h>
#include <string.h>

#define MAX_CONFIGS 32

void usage(void)
{
  fprintf(stderr,"Usage: random_effect <config_database>\n");
  exit(1);
}

int main(int argc, char *argv[])
{
  time_t now;
  FILE *fin;
  char xb_cmdline[1024];
  char *names[MAX_CONFIGS];
  char line[256];
  int cnt=0;
  int i;

  if(argc!=2)
    usage();

  now = time(NULL);
  srand((unsigned int)now);	/* randomize */
  
  fin=fopen(argv[1],"r");
  if(fin==NULL)
  {
    fprintf(stderr,"Could not open the xbanner configuration-files list\n");
    exit(1);
  }

  fgets(xb_cmdline,1023,fin);	/* get the cmd line to run xbanner */
  xb_cmdline[strlen(xb_cmdline)-1]='\0';	/* trim CR */
  strcat(xb_cmdline," -file ");	/* add the -file option */

  while(fgets(line,255,fin)!=NULL)
  {
    names[cnt]=(char*)malloc(strlen(line)+1);
    if(names[cnt]==NULL)
    {
      fprintf(stderr,"Memory allocation error\n");
      exit(1);
    }
    strcpy(names[cnt],line);
    cnt++;
  }
  i=rand()%cnt;
  strcat(xb_cmdline,names[i]);
  fprintf(stderr,"XBanner Random Effect executing: %s\n",xb_cmdline);
  system(xb_cmdline);
  return 0;
}
