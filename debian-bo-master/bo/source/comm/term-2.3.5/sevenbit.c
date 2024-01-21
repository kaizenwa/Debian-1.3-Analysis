
#include "includes.h"
#include "debug.h"
/* gets size bits from starting from bit 'curr' in the bit stream data */

int s_2_e_buff(un_char *data, un_char *out, int len) {
    int     byte,
            bit,
            j;
    byte = 0;
    bit = 0;
    j = 0;
    do {
      out[j] = ((unsigned)(data[byte++] & 127) >> bit);
      out[j] |= (data[byte] & 127) << (7 - bit);
      if (++bit == 7) {
	bit = 0;
	++byte;
      }
      ++j;
    } while (byte + 1< len);
    return j;
}

int e_2_s_put(un_char *out, un_char data, int key) {
  int bit = 0;
  bit = key&7;
  key>>=3;
  if (bit ==7) {
    out[++key]=0;
    bit = 0;
  }
  out[key++] |= (data<<bit) & 127;
  out[key] = data >> (7 - bit);
  ++bit;
  return key * 8 + bit;
}


/* This is a new set of routines for converting to and from 7 bits and
 * maybe eventually other 6, 5, and 4 bits as well.  Unlike the old 
 * routine, the extra bits are added to the end.  This way the data
 * is still compressible by vis.42, if none of the data was 8 bit
 * we can return the original data, and if we can use the same
 * buffer for input and output.  
 */


int convert_to_8(int nbits,int in_len,un_char *in_buffer,
    un_char *out_buffer) {

  if (nbits > 7) {
    memcpy(out_buffer,in_buffer,in_len);
    return in_len;
  }

  if (nbits > 3) {
    int i,cost,out_len;
    int nmask = 255, mmask = 255, mbits = 8;

    cost = nbits/(mbits -= nbits);
    nmask >>= nbits;
    mmask >>= mbits;

    if ((i=out_len=in_len - (in_len + cost)/(cost+1)) > 0) {
      int j,k = in_len;

      j = ((--i)%cost)+1;
      while(i >= 0) {
        un_char extra = in_buffer[--k];
        for(;j > 0;--j,--i) {
          out_buffer[i] = (in_buffer[i] & mmask) | ((extra & nmask)<<nbits);
          extra >>= mbits;
        }
        j = cost;
      }
    }
    return out_len;
  }

  return -1;

}



int convert_from_8(int nbits,int in_len,un_char *in_buffer,
    un_char *out_buffer){

  if (nbits > 7) {
    if (out_buffer != in_buffer) memcpy(out_buffer,in_buffer,in_len);
    return -in_len;
  }

  if (nbits > 3) {
    int i;
    int mmask = 255, nmask = 255, cost, mbits = 8;
    int out_len = in_len, needed = 0;

    if (out_buffer != in_buffer)
      memcpy(out_buffer,in_buffer,in_len);

    cost = nbits/(mbits -= nbits);
    nmask >>= nbits;
    mmask >>= mbits;
    for(i=0,out_len=in_len;i<in_len;) {
      int j;
      un_char *extra;

      extra = &out_buffer[out_len++];
      for(*extra=0, j=0;j < cost && i < in_len;++j) {
        *extra = ((out_buffer[i] >> nbits) & nmask) | (*extra << mbits);
        out_buffer[i++] &= mmask;
      }
      needed |= *extra;
    }
    return (needed) ? out_len : -in_len;
  }

  return -1;

}



