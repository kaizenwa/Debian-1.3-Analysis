
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "../src/config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>

int main(int argc,char *argv[])
{
  long j;
  int i,r;
  char ch;
  
  if(argc>2) {
    fprintf(stderr, "%s: Usage %s [number]\n", argv[0], argv[0]);
    exit(1);
  }
  
  r=atoi(argv[1]);

  if(fseek(stdin, 0L, SEEK_END))
    perror(argv[0]);
  for(j=ftell(stdin)/r-1; j>=0; j--) {
    if(fseek(stdin, j*r, SEEK_SET))
      perror(argv[0]);
    for(i=0; i<r; i++) {
      ch=fgetc(stdin);
      fputc(ch, stdout);
    }
  }
  
  return(0);
}
