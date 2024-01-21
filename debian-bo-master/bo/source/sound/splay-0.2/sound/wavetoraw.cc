/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Wavetoraw.cc
// Server which strips wave header.

#include <malloc.h>

#include "mpegsound.h"

// Convert wave format to raw format class
bool Wavetoraw::initialize(void)
{
  if(!buffer)
  {
    buffersize=player->getblocksize();
    if((buffer=(char *)malloc(buffersize))==NULL)
    {
      errorcode=SOUND_ERROR_MEMORYNOTENOUGH;
      return false;
    }
  }
  return true;
}

bool Wavetoraw::run(void)
{
  int c;

  if(initialized)
  {
    c=loader->getblock(buffer,buffersize);
    if(c<0)
    {
      errorcode=SOUND_ERROR_FILEREADFAIL;
      return false;
    }

    count-=c;
    if(player->putblock(buffer,buffersize)==false)return false;

    if(count<=0)
    {
      errorcode=SOUND_ERROR_FINISH;
      return false;
    }
  }
  else
  {
    c=loader->getblock(buffer,sizeof(WAVEHEADER));
    if(c<0)
    {
      errorcode=SOUND_ERROR_FILEREADFAIL;
      return false;
    }

    if(!testwave(buffer))return false;
    if(player->setsoundtype(stereo,samplesize,speed)==false)return false;
    initialized=true;
  }
  return true;
}

bool Wavetoraw::testwave(char *buffer)
{
  WAVEHEADER *tmp=(WAVEHEADER *)buffer;

  if(tmp->main_chunk==RIFF && tmp->chunk_type==WAVE &&
     tmp->sub_chunk==FMT && tmp->data_chunk==DATA)
    if(tmp->format==PCM_CODE && tmp->modus<=2)
    {
      stereo=(tmp->modus==WAVE_STEREO) ? 1 : 0;
      samplesize=(int)(tmp->bit_p_spl);
      speed=(int)(tmp->sample_fq);
      count=(int)(tmp->data_length);

      return true;
    }

  errorcode=SOUND_ERROR_BAD;
  return false;
}
