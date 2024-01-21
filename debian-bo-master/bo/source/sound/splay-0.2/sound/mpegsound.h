/* MPEG/WAVE Sound library
     Version 0.2

   (C) 1997 by Jung woo-jae */

// Mpegsound.h
//   This is typeset for functions in MPEG/WAVE Sound library.
//   Now, it's for only linux-pc-?86

/************************************/
/* Inlcude default library packages */
/************************************/
#include <stdio.h>
#include <malloc.h>
#include <unistd.h>

#ifdef PTHREADEDMPEG
#include <pthread.h>
#endif

/************************************************************/
/* BBitstream is my own library for MPEG/WAVE sound library */
/************************************************************/
#include <bbitstream.h>

#ifndef _L__SOUND__

#define _L__SOUND__

/****************/
/* Sound Errors */
/****************/
// General error
#define SOUND_ERROR_OK               0
#define SOUND_ERROR_FINISH          -1

// Device error (for player)
#define SOUND_ERROR_DEVOPENFAIL      1
#define SOUND_ERROR_DEVBUSY          2
#define SOUND_ERROR_DEVBADBUFFERSIZE 3
#define SOUND_ERROR_DEVCTRLERROR     4

// Sound file (for reader)
#define SOUND_ERROR_FILEOPENFAIL     5
#define SOUND_ERROR_FILEREADFAIL     6

// Miscellneous (for translater)
#define SOUND_ERROR_MEMORYNOTENOUGH  7
#define SOUND_ERROR_EOF              8
#define SOUND_ERROR_BAD              9

#define SOUND_ERROR_THREADFAIL       10

#define SOUND_ERROR_UNKNOWN          11


/**************************/
/* Define values for MPEG */
/**************************/
#define SCALEBLOCK     12
#define CALCBUFFERSIZE 512
#define MAXSUBBAND     32
#define MAXCHANNEL     2
#define MAXTABLE       2
#define SCALE          32768
#define MAXSCALE       (SCALE-1)
#define MINSCALE       (-SCALE)
#define RAWDATASIZE    (2*2*32*SSLIMIT)

#define LS 0
#define RS 1

#define SSLIMIT      18
#define SBLIMIT      32

#define WINDOWSIZE    4096

// Huffmancode
#define HTN 34


/*******************************************/
/* Define values for Microsoft WAVE format */
/*******************************************/
#define RIFF		0x46464952
#define WAVE		0x45564157
#define FMT		0x20746D66
#define DATA		0x61746164
#define PCM_CODE	1
#define WAVE_MONO	1
#define WAVE_STEREO	2

#define MODE_MONO   0
#define MODE_STEREO 1

/********************/
/* Type definitions */
/********************/
typedef float REAL;
typedef struct _waveheader {
  unsigned long	 main_chunk;  // 'RIFF'
  unsigned long	 length;      // filelen
  unsigned long	 chunk_type;  // 'WAVE'

  unsigned long  sub_chunk;   // 'fmt '
  unsigned long  sc_len;      // length of sub_chunk, =16
  unsigned short format;      // should be 1 for PCM-code
  unsigned short modus;	      // 1 Mono, 2 Stereo
  unsigned long	 sample_fq;   // frequence of sample
  unsigned long  byte_p_sec;
  unsigned short byte_p_spl;  // samplesize; 1 or 2 bytes
  unsigned short bit_p_spl;   // 8, 12 or 16 bit

  unsigned long  data_chunk;  // 'data'
  unsigned long  data_length; // samplecount
}WAVEHEADER;

typedef struct
{
  bool         generalflag;
  unsigned int part2_3_length;
  unsigned int big_values;
  unsigned int global_gain;
  unsigned int scalefac_compress;
  unsigned int window_switching_flag;
  unsigned int block_type;
  unsigned int mixed_block_flag;
  unsigned int table_select[3];
  unsigned int subblock_gain[3];
  unsigned int region0_count;
  unsigned int region1_count;
  unsigned int preflag;
  unsigned int scalefac_scale;
  unsigned int count1table_select;
}layer3grinfo;

typedef struct
{
  unsigned main_data_begin;
  unsigned private_bits;
  struct
  {
    unsigned scfsi[4];
    layer3grinfo gr[2];
  }ch[2];
}layer3sideinfo;

typedef struct
{
  int l[23];            /* [cb] */
  int s[3][13];         /* [window][cb] */
}layer3scalefactor;     /* [ch] */

typedef struct
{
  int tablename;
  unsigned int xlen,ylen;
  unsigned int linbits;
  unsigned int treelen;
  const unsigned char (*val)[2];
}HUFFMANCODETABLE;


/*********************************/
/* Sound input interface classes */
/*********************************/
// Superclass for inputstream
class Soundinputstream
{
public:
  Soundinputstream()     {errorcode=SOUND_ERROR_OK;};
  int geterrorcode(void) {return errorcode;};

  virtual int getblock(char *buffer,int size)=0;

protected:
  int errorcode;
};

// Stream class for opened file
class Soundinputstreamfromfile : public Soundinputstream
{
public:
  Soundinputstreamfromfile(int handle) {filehandle=handle;};
  int getblock(char *buffer,int size)
  {
    int i=read(filehandle,buffer,size);
    if(i<0)errorcode=SOUND_ERROR_FILEREADFAIL;
    return i;
  };

private:
  int filehandle;
};

// Superclass for inputbitstream
class Soundinputbitstream 
{
public:
  Soundinputbitstream()          {errorcode=SOUND_ERROR_OK;};
  virtual ~Soundinputbitstream() {};
  virtual int  geterrorcode(void)=0;

  virtual bool open(char *filename)      =0;
  virtual int  getbytedirect(void)       =0;
  virtual bool readbuffer(int size)      =0;
  virtual int  getbyte(void)             =0;
  virtual int  getbit(void)              =0;
  virtual int  getbits(int bits)         =0;
  virtual void sync(void)                =0;
  virtual bool eof(void)                 =0;

protected:
  int errorcode;
};

// Inputbitstream class for file
class Soundinputbitstreamfromfile : public Soundinputbitstream
{
public:
  Soundinputbitstreamfromfile()  {in=NULL;};
  ~Soundinputbitstreamfromfile() {if(in)delete in;};

  int geterrorcode(void)
  {
    switch(in->geterrorcode())
    {
      case BBITSTREAM_ERROR_OK:return errorcode;
      case BBITSTREAM_ERROR_OPENFAIL:return SOUND_ERROR_FILEOPENFAIL;
      case BBITSTREAM_ERROR_READFAIL:return SOUND_ERROR_FILEREADFAIL;
    }
    return SOUND_ERROR_UNKNOWN;
  };
  bool open(char *filename)
  {
    bool flag;

    if((in=new Bufferinputbitstream)==NULL)
    {
      errorcode=SOUND_ERROR_MEMORYNOTENOUGH;
      return false;
    };
    flag=in->openfile(filename);
    if(!flag)errorcode=SOUND_ERROR_FILEOPENFAIL;
    return flag;
  };
  int  getbytedirect(void)     {return in->getbytedirect();};
  bool readbuffer(int size)    {return in->readbuffer(size);};
  int  getbyte(void)           {return in->getbyte();};
  int  getbit(void)            {return in->getbit();};
  int  getbits(int bits)       {return in->getbits(bits);};
  bool eof(void)               {return in->eof();};
  void sync(void)              {in->sync();};

private:
  Bufferinputbitstream *in;
};



/**********************************/
/* Sound player interface classes */
/**********************************/
// Superclass for player
class Soundplayer
{
public:
  Soundplayer()          {errorcode=SOUND_ERROR_OK;};
  virtual ~Soundplayer() {};

  int geterrorcode(void) {return errorcode;};

  virtual bool initialize(void)                                 =0;
  virtual bool setsoundtype(int stereo,int samplesize,int speed)=0;
  virtual bool putblock(void *buffer,int size)                  =0;

  virtual int  getblocksize(void) {return 1024;};

protected:
  int errorcode;
  bool error(int errorno) {errorcode=errorno; return false;};
};


// Class for playing raw data
class Rawplayer : public Soundplayer
{
public:
  ~Rawplayer();

  bool initialize(void);
  bool setsoundtype(int stereo,int samplesize,int speed);
  bool putblock(void *buffer,int size);
  int  getblocksize(void) {return audiobuffersize;};

private:
  short int rawbuffer[RAWDATASIZE];
  int  rawbuffersize;
  int  audiohandle,audiobuffersize;
  int  rawstereo,rawsamplesize,rawspeed;
  bool forcetomono,forceto8;
};



/*********************************/
/* Data format converter classes */
/*********************************/
// Class for converting wave format to raw format
class Wavetoraw
{
public:
  Wavetoraw(Soundinputstream *loader,Soundplayer *player)
  {
    errorcode=SOUND_ERROR_OK;
    initialized=false;buffer=NULL;
    this->loader=loader;this->player=player;
  };
  ~Wavetoraw(){if(buffer)free(buffer);};

  int  geterrorcode(void){return errorcode;};


  bool initialize(void);
  void setforcetomono(bool flag){forcetomonoflag=flag;};
  bool run(void);

private:
  bool forcetomonoflag;
  int errorcode;

  Soundinputstream *loader;
  Soundplayer *player;

  bool initialized;
  char *buffer;
  int  buffersize;
  int  samplesize,speed,stereo,count;

  bool testwave(char *buffer);
};


// Class for Mpeg layer3
class Mpegbitwindow
{
public:
  Mpegbitwindow(){bitindex=point=0;};

  void initialize(void)  {bitindex=point=0;};
  int  gettotalbit(void) {return bitindex;};
  void putbyte(int c)    {buffer[point&(WINDOWSIZE-1)]=c;point++;};
  void rewind(int bits)  {bitindex-=bits;};
  void forward(int bits) {bitindex+=bits;};
  int  getbits(int bits);
  int  getbit(void)
  {
    int r=(buffer[(bitindex>>3)&(WINDOWSIZE-1)]>>(7-(bitindex&7)))&1;
    bitindex++;return r;
  };

private:
  int  point,bitindex;
  char buffer[WINDOWSIZE];
};

// Class for converting mpeg format to raw format
class Mpegtoraw
{
public:
  int  geterrorcode(void) {return errorcode;};
  void initialize(void);
  bool loadheader(void);

private:
  int errorcode;

  //
  // Variables
  //
  // Interface
  Soundinputbitstream *loader;
  Soundplayer *player;
  unsigned int getbits(int num)       {return loader->getbits(num);};
  unsigned int getbit()               {return loader->getbit();};

  // Global variables
  int lastfrequency,laststereo;

  // Mpeg Header variables
  int layer,protection,bitrateindex,padding,extendedmode;
  enum _mode      {fullstereo,joint,dual,single}                 mode;
  enum _frequency {frequency44100,frequency48000,frequency32000} frequency;

  // Informatinons made by header variables
  int tableindex,channelbitrate;
  int stereobound,subbandnumber,inputstereo,outputstereo;
  REAL scalefactor;
  int framesize;

  // for Layer3
  int layer3slots,layer3framestart,layer3part2start;
  REAL prevblck[2][SBLIMIT][SSLIMIT];
  layer3sideinfo sideinfo;
  layer3scalefactor scalefactors[2];
  Mpegbitwindow bitwindow;
  int wgetbits(int num){return bitwindow.getbits(num);};
  int wgetbit(void){return bitwindow.getbit();};

  // Variables for synthesis
  REAL calcbufferL[2][CALCBUFFERSIZE],calcbufferR[2][CALCBUFFERSIZE];
  int  currentcalcbuffer,calcbufferoffset;

  // raw output
  bool forcetomonoflag;
  short int rawdata[RAWDATASIZE];
  int  rawdataoffset;
  void clearrawdata(void){rawdataoffset=0;};
  void putraw(short int pcm){rawdata[rawdataoffset++]=pcm;};
  void flushrawdata(void);

#ifdef PTHREADEDMPEG
public:
  bool makethreadedplayer(int framenumbers);
  void freethreadedplayer(void);
  void threadedplayer(void);

private:
  bool threadflag;
  pthread_t thread;
  short int *rawqueue;
  int *queue_sizes;
  int queue_framenumber,queue_frametail;
  int queue_head,queue_tail;
  bool playlocked;
#endif

  //
  // Const tables
  //

  // Tables for layer 1,2 and 3
  static const int bitrate[3][15],frequencies[3];

  // Tables for layer 1 and 2
  static const int bitalloclengthtable[MAXTABLE][MAXSUBBAND];
  static const REAL factortable[15],offsettable[15];
  static const REAL scalefactorstable[64];
  static const REAL group5bits[27*3],group7bits[125*3],group10bits[729*3];
  static const REAL *grouptableA[16],*grouptableB1[16],*grouptableB234[16];
  static const int codelengthtableA [16],
                   codelengthtableB1[16],codelengthtableB2[16],
                   codelengthtableB3[ 8],codelengthtableB4[ 4];
  static const REAL factortableA [16],
                    factortableB1[16],factortableB2[16],
                    factortableB3[ 8],factortableB4[ 4];
  static const REAL ctableA [16],
                    ctableB1[16],ctableB2[16],
                    ctableB3[ 8],ctableB4[ 4];
  static const REAL dtableA [16],
                    dtableB1[16],dtableB2[16],
                    dtableB3[ 8],dtableB4[ 4];

  // Tables for layer 3
  static const HUFFMANCODETABLE ht[HTN];

  // Tables for subbandsynthesis
  static const REAL filter[512];

public:
  Mpegtoraw(Soundinputbitstream *loader,Soundplayer *player)
  {
    forcetomonoflag=false;
    errorcode=SOUND_ERROR_OK;
    this->loader=loader;
    this->player=player;

  };
  void setforcetomono(bool flag){forcetomonoflag=flag;};
  bool run(int frames);

private:
  bool error(int errorno){errorcode=errorno;return false;};

  // Subbandsynthesis
  void computebuffer(REAL *fraction,REAL buffer[2][CALCBUFFERSIZE]);
  void generatesingle(void);
  void generate(void);
  void subbandsynthesis(REAL *fractionL,REAL *fractionR);

  // Extarctor
  void extractlayer1(void);
  void extractlayer2(void);
  void extractlayer3(void);

  // Functions for layer III
  void layer3initialize(void);
  bool layer3getsideinfo(void);
  void layer3getscalefactors(int ch,int gr);
  void layer3huffmandecode(int ch,int gr,int out[SBLIMIT][SSLIMIT]);
  REAL layer3twopow2(int scale,int preflag,int pretab_offset,int l);
  REAL layer3twopow2_1(int a,int b,int c);
  void layer3dequantizesample(int ch,int gr,int   in[SBLIMIT][SSLIMIT],
			                    REAL out[SBLIMIT][SSLIMIT]);
  void layer3fixtostereo(int gr,REAL  in[2][SBLIMIT][SSLIMIT],
			        REAL out[2][SBLIMIT][SSLIMIT]);
  void layer3reorder(int ch,int gr,REAL  in[SBLIMIT][SSLIMIT],
		                   REAL out[SBLIMIT][SSLIMIT]);
  void layer3antialias(int ch,int gr,REAL  in[SBLIMIT][SSLIMIT],
		                     REAL out[SBLIMIT][SSLIMIT]);
  void layer3reorderandantialias(int ch,int gr,REAL  in[SBLIMIT][SSLIMIT],
				               REAL out[SBLIMIT][SSLIMIT]);

  void layer3inv_mdct(REAL *in,REAL *out,int block_type);
  void layer3hybrid(int ch,int gr,int sb,REAL in[SSLIMIT],
		                         REAL out[SSLIMIT][SBLIMIT]);
  
  int  huffmandecoder(const HUFFMANCODETABLE *h,int *x,int *y,int *v,int *w);
};


/***********************/
/* File player classes */
/***********************/
// Superclass for playing file
class Fileplayer
{
public:
  Fileplayer()           {errorcode=SOUND_ERROR_OK;player=NULL;};
  virtual ~Fileplayer()  {delete player;};

  int geterrorcode(void) {return errorcode;};

  virtual bool openfile(char *filename) =0;
  virtual void setforcetomono(bool flag)=0;
  virtual bool playing(void)            =0;

protected:
  bool error(int errorno){errorcode=errorno;return false;};

  int errorcode;
  Soundplayer *player;
};


// Class for playing wave file
class Wavefileplayer : public Fileplayer
{
public:
  Wavefileplayer()  {loader=NULL;server=NULL;filehandle=0;};
  ~Wavefileplayer() {close(filehandle);delete loader;delete server;};

  bool openfile(char *filename);
  void setforcetomono(bool flag){server->setforcetomono(flag);};
  bool playing(void);
  
private:
  int filehandle;
  Soundinputstream *loader;
  Wavetoraw *server;
};


// Class for playing MPEG file
class Mpegfileplayer : public Fileplayer
{
public:
  Mpegfileplayer()  {} ;
  ~Mpegfileplayer() {delete loader; delete server;};

  bool openfile(char *filename);
  void setforcetomono(bool flag){server->setforcetomono(flag);};
  bool playing(void);
#if PTHREADEDMPEG
  bool playingwiththread(int framenumbers);
#endif

private:
  Soundinputbitstream *loader;
  Mpegtoraw *server;
};

#endif
