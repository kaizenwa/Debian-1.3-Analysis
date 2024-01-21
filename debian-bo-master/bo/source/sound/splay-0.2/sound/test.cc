// Tester for MPEG/WAVE Sound library

#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <unistd.h>

#include "mpegsound.h"

int main(int argc,char *argv[])
{
  Fileplayer *player=NULL;

  if(argc<2)
  {
    printf("Args Error\n");
    exit(1);
  }

  if(strstr(argv[1],".mp"))player=new Mpegfileplayer;
  else player=new Wavefileplayer;

  if(argv[1][0]=='-')player->openfile(NULL);
  else player->openfile(argv[1]);
  //  player->setforcetomono(true);
  ((Mpegfileplayer *)player)->playingwiththread(100);
}
