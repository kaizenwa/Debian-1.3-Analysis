// Bitstream.H

#include <stdio.h>

#ifndef _L__BITSTREAM__

#define _L__BITSTREAM__

#define BBITSTREAM_ERROR_OK       0
#define BBITSTREAM_ERROR_OPENFAIL 1
#define BBITSTREAM_ERROR_READFAIL 2

class Bufferinputbitstream
{
public:
  int geterrorcode(void){return errorcode;};

  Bufferinputbitstream()
    {fp=NULL;errorcode=BBITSTREAM_ERROR_OK;point=buffersize=0;};
  ~Bufferinputbitstream() {if(fp)fclose(fp);};

  bool openfile(char *filename);
  bool readbuffer(int bytes);
  int getbytedirect(void);
  int getbyte(void);
  int getbits(int bits);
  int getbit(void)
  {
    int c;

    if(!bitindex)
    {
      if((c=getbyte())==-1)return -1;
      store[0]=c;
      bitindex=7;
    }
    else bitindex--;
    c=(store[0]&128)>>7;
    store[0]<<=1;

    return c;
  };

  bool eof(void) {return (fp==NULL);};
  void sync(void) {bitindex&=8;};

private:
  int errorcode;

  FILE *fp;
  int bitindex;
  unsigned char store[5];

  int buffersize,point;
  char buffer[4096];
};

class Bufferoutputbitstream
{
public:
  Bufferoutputbitstream() {fp=NULL;};
  ~Bufferoutputbitstream() {if(fp)fclose(fp);};
  bool open(char *filename);
  bool putbits(int c,int bits);
  bool putbit(int c)
  {
    if(bitindex==8)
    {
      if(putbyte(store[4])==EOF)return false;
      bitindex=1;store[4]=c;
    }
    else {bitindex++;store[4]=(store[4]<<1)|(c&1);}
    return true;
  };
  bool sync(void);

private:
  FILE *fp;
  int bitindex;
  unsigned char store[5];

  int putbyte(int c){return putc(c,fp);};
};

#endif
