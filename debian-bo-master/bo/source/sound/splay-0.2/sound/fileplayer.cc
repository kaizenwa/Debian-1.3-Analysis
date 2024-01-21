/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Fileplayer.cc
// It's an example for how to use MPEG/WAVE Sound library

#include <fcntl.h>
#include "mpegsound.h"

// Wave file player
bool Wavefileplayer::openfile(char *filename)
{
// Player
  if((player=(Soundplayer *)new Rawplayer)==NULL)
    return error(SOUND_ERROR_MEMORYNOTENOUGH);
  if(!player->initialize())return error(player->geterrorcode());

// Loader
  if(filename)
  {
    if((filehandle=open(filename,O_RDONLY,0))==-1)
      return error(SOUND_ERROR_FILEOPENFAIL);
  }
  else filehandle=0;

  if((loader=new Soundinputstreamfromfile(filehandle))==NULL)
    return error(SOUND_ERROR_MEMORYNOTENOUGH);

// Server
  if((server=new Wavetoraw(loader,player))==NULL)
    return error(SOUND_ERROR_MEMORYNOTENOUGH);
  return server->initialize();
}

bool Wavefileplayer::playing(void)
{
  if(!server->run())return false; // Read first time
  while(server->run());           // Playing

  errorcode=server->geterrorcode();
  if(errorcode==SOUND_ERROR_FINISH)return true;
  return false;
}




// Mpegfileplayer
bool Mpegfileplayer::openfile(char *filename)
{
// Player
  if((player=(Soundplayer *)new Rawplayer)==NULL)
    return error(SOUND_ERROR_MEMORYNOTENOUGH);
  if(!player->initialize())return error(player->geterrorcode());

// Loader
  if((loader=new Soundinputbitstreamfromfile)==NULL)
    return error(SOUND_ERROR_MEMORYNOTENOUGH);
  if(!loader->open(filename))return error(loader->geterrorcode());

// Server
  if((server=new Mpegtoraw(loader,player))==NULL)
    return error(SOUND_ERROR_MEMORYNOTENOUGH);

// Initialize server
  server->initialize();
  return true;
}


bool Mpegfileplayer::playing(void)
{
  if(!server->run(-1))return false;       // Initialize MPEG Layer 3
  while(server->run(100));                // Playing

  errorcode=server->geterrorcode();
  if(errorcode==SOUND_ERROR_FINISH)return true;
  return false;
}

#ifdef PTHREADEDMPEG
bool Mpegfileplayer::playingwiththread(int framenumbers)
{
  if(framenumbers<20)return playing();

  server->makethreadedplayer(framenumbers);

  if(!server->run(-1))return false;       // Initialize MPEG Layer 3
  while(server->run(100));                // Playing
  
  server->freethreadedplayer();
  
  errorcode=server->geterrorcode();
  if(errorcode==SOUND_ERROR_FINISH)return true;
  return false;
}
#endif

