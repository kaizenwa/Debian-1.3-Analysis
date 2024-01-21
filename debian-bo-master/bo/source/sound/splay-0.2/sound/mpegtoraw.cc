/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Mpegtoraw.cc
// Server which get mpeg format and put raw format.

#include "mpegsound.h"

inline void Mpegtoraw::flushrawdata(void)
#ifdef PTHREADEDMPEG
{
  if(threadflag)
  {
    if(((queue_tail+1)%queue_framenumber)==queue_head)
    {
      playlocked=false;
      while(((queue_tail+1)%queue_framenumber)==queue_head)sleep(1);
    }
    memcpy(rawqueue+(queue_tail*RAWDATASIZE),rawdata,
	   RAWDATASIZE*sizeof(short int));
    queue_sizes[queue_tail]=(rawdataoffset<<1);
    
    if(queue_tail>=queue_frametail)queue_tail=0; else queue_tail++;
  }
  else player->putblock(rawdata,rawdataoffset<<1);
  rawdataoffset=0;
}
#else
{
  player->putblock((char *)rawdata,rawdataoffset<<1);
  rawdataoffset=0;
};
#endif


// Convert mpeg to raw
// Mpeg headder class
void Mpegtoraw::initialize(void)
{
  register int i;
  register REAL *s1,*s2;
  REAL *s3,*s4;

  scalefactor=SCALE;
  calcbufferoffset=15;
  currentcalcbuffer=0;

  s1=calcbufferL[0];s2=calcbufferR[0];
  s3=calcbufferL[1];s4=calcbufferR[1];
  for(i=CALCBUFFERSIZE;i;i--)
    *s1++=*s2++=*s3++=*s4++=0.0;

  layer3initialize();

#ifdef PTHREADEDMPEG
  threadflag=false;
  rawqueue=NULL;
  queue_sizes=NULL;
#endif
};

bool Mpegtoraw::loadheader(void)
{
  register int c;
  bool flag;

  loader->sync();

// Synchronize
  flag=false;
  do
  {
    if((c=loader->getbytedirect())<0)break;

    if(c==0xff)
      while(!flag)
      {
	if((c=loader->getbytedirect())<0)
	{
	  flag=true;
	  break;
	}
	if((c&0xf0)==0xf0)
	{
	  flag=true;
	  break;
	}
	else if(c!=0xff)break;
      }
  }while(!flag);

  if(c<0)return error(SOUND_ERROR_FINISH);

// Analyzing
  c&=0xf;
  protection=c&1;
  layer=4-((c>>1)&3);

  c=((loader->getbytedirect()))>>1;
  padding=(c&1);             c>>=1;
  frequency=(_frequency)(c&2); c>>=2;
  bitrateindex=(int)c;
  if(bitrateindex==15)return error(SOUND_ERROR_BAD);

  c=((unsigned int)(loader->getbytedirect()))>>4;
  extendedmode=c&3;
  mode=(_mode)(c>>2);


// Making information
  inputstereo= (mode==single)?0:1;
  if(forcetomonoflag)outputstereo=0; else outputstereo=inputstereo;

  /*  if(layer==2)
    if((bitrateindex>=1 && bitrateindex<=3) || (bitrateindex==5)) {
      if(inputstereo)return error(SOUND_ERROR_BAD); }
    else if(bitrateindex==11 && mode==single)return error(SOUND_ERROR_BAD); */

  channelbitrate=bitrateindex;
  if(inputstereo)
    if(channelbitrate==4)channelbitrate=1;
    else channelbitrate-=4;

  if(channelbitrate==1 || channelbitrate==2)tableindex=0; else tableindex=1;

  if(layer==1)subbandnumber=MAXSUBBAND;
  else
  {
    if(!tableindex)
      if(frequency==frequency32000)subbandnumber=12; else subbandnumber=8;
    else if(frequency==frequency48000||
	    (channelbitrate>=3 && channelbitrate<=5))
      subbandnumber=27;
    else subbandnumber=30;
  }

  if(mode==single)stereobound=0;
  else if(mode==joint)stereobound=(extendedmode+1)<<2;
  else stereobound=subbandnumber;

  if(stereobound>subbandnumber)stereobound=subbandnumber;

  // framesize & slots
  if(layer==1)
  {
    framesize=(12000*bitrate[0][bitrateindex])/frequencies[frequency];
    if(frequency==frequency44100 && padding)framesize++;
    framesize<<=2;
  }
  else
  {
    framesize=(144000*bitrate[layer-1][bitrateindex])/frequencies[frequency];
    if(padding)framesize++;
    if(layer==3)
      layer3slots=framesize-((mode==single)?17:32)
	                   -(protection?0:2)
	                   -4;
  }

  if(!loader->readbuffer(framesize-4))error(SOUND_ERROR_FILEREADFAIL);

  if(!protection)
  {
    loader->getbyte();                      // CRC, Not check!!
    loader->getbyte();
  }


  if(loader->eof())return error(SOUND_ERROR_FINISH);

  return true;
}

#ifdef PTHREADEDMPEG
void Mpegtoraw::threadedplayer(void)
{
  while(playlocked)sleep(1);

  for(;;)
    if(queue_head!=queue_tail)
    {
      player->putblock(rawqueue+queue_head*RAWDATASIZE,
		       queue_sizes[queue_head]);
      if(queue_head==queue_frametail)queue_head=0; else queue_head++;
    }
    else
    {
      if(!queue_framenumber)break;
      sleep(1);
    }
  threadflag=false;
}

static void *threadlinker(void *arg)
{
  ((Mpegtoraw *)arg)->threadedplayer();

  return NULL;
}

bool Mpegtoraw::makethreadedplayer(int framenumbers)
{
  rawqueue=(short int *)malloc(sizeof(short int)*RAWDATASIZE*framenumbers);
  if(rawqueue==NULL)error(SOUND_ERROR_MEMORYNOTENOUGH);
  queue_sizes=(int *)malloc(sizeof(int)*framenumbers);
  if(queue_sizes==NULL)error(SOUND_ERROR_MEMORYNOTENOUGH);

  playlocked=true;
  queue_framenumber=framenumbers;
  queue_frametail=framenumbers-1;
  queue_head=queue_tail=0;

  if(pthread_create(&thread,0,threadlinker,this))error(SOUND_ERROR_THREADFAIL);
  threadflag=true;

  return true;
}

void Mpegtoraw::freethreadedplayer(void)
{
  if(threadflag)queue_framenumber=0;   // Terminate thread
  while(threadflag)sleep(1);           // Wait
  if(rawqueue)free(rawqueue);
  if(queue_sizes)free(queue_sizes);
}
#endif

// Convert mpeg to raw
bool Mpegtoraw::run(int frames)
{
  clearrawdata();

  for(;frames;frames--)
  {
    if(loader->eof())
    {
      errorcode=SOUND_ERROR_FINISH;
      break;
    }
    if(loadheader()==false)break;

    if(frequency!=lastfrequency)
    {
      if(lastfrequency>=0)error(SOUND_ERROR_BAD);
      lastfrequency=frequency;
    }
    if(frames<0)
    {
      frames=-frames;
      player->setsoundtype(outputstereo,16,frequencies[frequency]);
    }

    if     (layer==3)extractlayer3();
    else if(layer==2)extractlayer2();
    else if(layer==1)extractlayer1();

    flushrawdata();
    if(player->geterrorcode())error(geterrorcode());
  }
  return (errorcode==0);
}
