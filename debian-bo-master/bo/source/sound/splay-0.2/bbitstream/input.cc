// Input.CC

#include "bbitstream.h"

/************************/
/* Input bitstrem class */
/************************/
bool Bufferinputbitstream::openfile(char *filename)
{
  bitindex=point=buffersize=0;

  if(filename==NULL)fp=stdin;
  else if((fp=fopen(filename,"r"))<0)
  {
    errorcode=BBITSTREAM_ERROR_OPENFAIL;
    return false;
  }

  return true;
}

int Bufferinputbitstream::getbytedirect(void)
{
  int c;

  if((c=getc(fp))<0)
  {
    errorcode=BBITSTREAM_ERROR_READFAIL;
    fclose(fp);fp=NULL;

    return -1;
  }

  return c;
}

bool Bufferinputbitstream::readbuffer(int size)
{
  if(fread(buffer,size,1,fp)!=1)
  {
    errorcode=BBITSTREAM_ERROR_READFAIL;
    fclose(fp);fp=NULL;
    return false;
  }
  buffersize=size;
  point=0;
  return true;
}

int Bufferinputbitstream::getbyte(void)
{
  if(point<buffersize)
    return (unsigned char)buffer[point++];
  errorcode=BBITSTREAM_ERROR_READFAIL;
  return -1;
}

int Bufferinputbitstream::getbits(int bits)
{
  int *current,c;

  current=(int *)(store);
  *((int *)(store+1))=0;

  while(bits)
  {
    if(!bitindex)                    // Charge
    {
      if((c=getbyte())==-1)return -1;
      store[0]=c;
      bitindex=8;
    }

    if(bits>=bitindex)
    {
      (*current)<<=bitindex;
      bits-=bitindex;
      bitindex=0;
    }
    else
    {
      (*current)<<=bits;
      bitindex-=bits;
      bits=0;
    }
  }

  return *((int *)(store+1));
}
