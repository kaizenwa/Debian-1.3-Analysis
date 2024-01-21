// Output.CC

#include <stdio.h>

#include "bbitstream.h"


/**************************/
/* Output bitstream class */
/**************************/
bool Bufferoutputbitstream::open(char *filename)
{
  bitindex=0;

  return (fp=fopen(filename,"wb"));
}

bool Bufferoutputbitstream::sync(void)
{
  if(bitindex)
  {
    if(bitindex<8)store[4]<<=8-bitindex;

    if(putbyte(store[4])==EOF)return false;
    bitindex=0;
  }

  return true;
}


bool Bufferoutputbitstream::putbits(int c,int bits)
{
  int *current;
  int chargebit;

  c<<=32-bits;
  *((int *)store)=c;

  current=(int *)(store+1);

  while(bits)
  {
    if(bitindex==8)                    // Charge
    {
      if(putbyte(store[4])==EOF)return false;
      bitindex=0;
    }
    chargebit=8-bitindex;

    if(bits>=chargebit)
    {
      (*current)<<=chargebit;
      bits-=chargebit;
      bitindex=8;
    }
    else
    {
      (*current)<<=bits;
      bitindex+=bits;
      bits=0;
    }
  }

  return true;
}



