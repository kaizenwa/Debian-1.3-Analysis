/* Splay Version 0.2

   Copyright (C) 1997 by Jung woo-jae */

// It's an example of using MPEG/WAVE Sound library

// Anyone can use MPEG/WAVE Sound library under GPL

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <mpegsound.h>

static char *Sounderrors[SOUND_ERROR_UNKNOWN]=
{ "Failed to open sound device.",
  "Sound device is busy.",
  "Buffersize of sound device is wrong.",
  "Sound device control error.",

  "Failed to open file for reading.",
  "Failed to read file.",

  "Memory is not enough.",
  "Unexpected EOF.",
  "Bad sound file format.",

  "Cannot make thread.",

  "Unknown error.",
};

static char *progname;
static bool forcetomonoflag=false;
static int threadstore=0;

inline void error(int n)
{
  fprintf(stderr,"%s: %s",progname,Sounderrors[n-1]);
  return;
}

void playingthread(Mpegfileplayer *player)
{
  if(player->geterrorcode()>0)error(player->geterrorcode());
  else
  {
    player->setforcetomono(forcetomonoflag);
    player->playingwiththread(threadstore);
    if(player->geterrorcode()>0)error(player->geterrorcode());
  }
}

void playing(Fileplayer *player)
{
  if(player->geterrorcode()>0)error(player->geterrorcode());
  else
  {
    player->setforcetomono(forcetomonoflag);
    player->playing();
    if(player->geterrorcode()>0)error(player->geterrorcode());
  }
}

void play(char *filename)
{
  Fileplayer *player;

  fprintf(stderr,"%s: \n",filename);
  if(strstr(filename,".mp"))
  {
    player=new Mpegfileplayer;
    player->openfile(filename);
#ifdef PTHREADEDMPEG
    playingthread((Mpegfileplayer *)player);
#else
    playing(player);
#endif
  }
  else
  {
    player=new Wavefileplayer;
    player->openfile(filename);
    playing(player);
  }
  delete player;
}

int main(int argc,char *argv[])
{
  bool shuffle=false;
  int c;

  progname=argv[0];
  if(argc<2)
  {
    printf("Soundplayer version 0.2\n"
	   "Usage : splay [-mMWs] [-t number] files ...\n"
	   "\n"
	   "\tif name of files contain \".mp\",  MPEG player\n"
	   "\tothers, Wave player\n\n"
	   " -m : force to mono\n"
	   " -M : playing mpeg as standard input\n"
	   " -W : playing wave as standard input\n"
	   " -s : shuffle play\n\n"
#ifdef PTHREADEDMPEG
	   " -t number : Making thread and\n"
	   "             store a number of frames before playing\n"
	   "             If number is less than 20, no thread\n\n"
#endif
	   );
    return 0;
  }

  while((c=getopt(argc,argv,"mMWt:s"))>=0)
  {
    switch(c)
    {
      case 'm':forcetomonoflag=true;break;
      case 'M':
	{
	  Mpegfileplayer player;

	  player.openfile(NULL);
	  playing(&player);
	}break;
      case 'W':
	{
	  Wavefileplayer player;

	  player.openfile(NULL);
	  playing(&player);
	}break;
      case 't':
	sscanf(optarg,"%d",&threadstore);
	break;
      case 's':shuffle=true;break;
      default:fprintf(stderr,"Bad argument.\n");
    }
  }

  if(shuffle && optind<argc)
  {
    int *list;
    int listsize=argc-optind;

    if((list=(int *)malloc(listsize*sizeof(int))))
    {
      int i,j=0;
      time_t t;

      printf("Shuffle play\n");

      for(i=optind;i<argc;i++,j++)list[j]=i;
      srandom(time(&t));

      while(listsize)
      {
	i=random()%listsize;
	play(argv[list[i]]);
	listsize--;
	list[i]=list[listsize];
      }

      return false;
    }
  }

  for(int i=optind;i<argc;i++)
    play(argv[i]);

  return 0;
}
