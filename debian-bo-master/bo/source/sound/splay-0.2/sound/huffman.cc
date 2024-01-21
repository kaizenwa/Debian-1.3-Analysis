/* MPEG/WAVE Sound library

   (C) 1997 by Jung woo-jae */

// Huffman.cc
// It's for MPEG layer 3
// I've removed function of reading HUFFDEC file and initializing.
// Then I've include initialized huffman code table into huffmantable.cc

#include "mpegsound.h"

typedef unsigned int HUFFBITS;
#define MXOFF   250

/* do the huffman-decoding 						*/
/* note! for counta,countb -the 4 bit value is returned in y, discard x */
int Mpegtoraw::huffmandecoder(const HUFFMANCODETABLE *h,
			      int *x,int *y,int *v,int *w)
{  
  HUFFBITS level=(1<<(sizeof(HUFFBITS)*8-1));
  int point=0;
  int error=1;
  register int xx,yy;
  int vv,ww;

  //  fprintf(stderr,"P%dP",h);
  //  fprintf(stderr,"N%dN",h->tablename);

  if(h->val==NULL)return 2;

  /* table 0 needs no bits */
  if(h->treelen==0)
  {
    *x=*y=0;
    return 0;
  }

  /* Lookup in Huffman table. */
  do
  {
    if(h->val[point][0]==0)
    {   /*end of tree*/
      error=0;
      break;
    } 
    if(wgetbit())
    {
      while(h->val[point][1]>=MXOFF)point+=h->val[point][1]; 
      point+=h->val[point][1];
    }
    else
    {
      while(h->val[point][0]>=MXOFF)point+=h->val[point][0]; 
      point+=h->val[point][0];
    }
    level>>=1;
  }while(level || ((unsigned)point<ht->treelen));
  
  /* Check for error. */
  
  if(error)
  { /* set x and y to a medium value as a simple concealment */
    //    printf("Illegal Huffman code in data.\n");
    xx=(h->xlen-1<<1);
    yy=(h->ylen-1<<1);
  }
  else
  {
    xx=h->val[point][1]>>4;
    yy=h->val[point][1]&0xf;
  }


  //  fprintf(stderr,"<%d>",xx);

  /* Process sign encodings for quadruples tables. */
  if(h->tablename>=32)
  {
    vv=(yy>>3)&1;
    ww=(yy>>2)&1;
    xx=(yy>>1)&1;
    yy=yy     &1;

     /* v, w, x and y are reversed in the bitstream. 
        switch them around to make test bistream work. */

    if(vv)if(wgetbit())vv=-vv;
    if(ww)if(wgetbit())ww=-ww;
    if(xx)if(wgetbit())xx=-xx;
    if(yy)if(wgetbit())yy=-yy;

    *v=vv;*w=ww;
    //    fprintf(stderr,"(%d)",xx);
  }
  else /* Process sign and escape encodings for dual tables. */
  {
    /* x and y are reversed in the test bitstream.
       Reverse x and y here to make test bitstream work.
       removed 11/11/92 -ag */

    //    fprintf(stderr,"{%d %d}",xx,h->linbits);
    if(h->linbits)if((h->xlen-1)==(unsigned)xx)xx+=wgetbits(h->linbits);
    if(xx)if(wgetbit())xx=-xx;
    //    fprintf(stderr,"{%d}",xx);
    if(h->linbits)if((h->ylen-1)==(unsigned)yy)yy+=wgetbits(h->linbits);
    if(yy)if(wgetbit())yy=-yy;
    *v=0;*w=0;
  }

  *x=xx;*y=yy;
  //  fprintf(stderr,"[%d %d]",*x,xx);
	  
  return error;  
}
