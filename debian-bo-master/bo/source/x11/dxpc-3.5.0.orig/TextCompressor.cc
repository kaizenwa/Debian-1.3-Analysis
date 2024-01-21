#include <iostream.h>
#include "TextCompressor.h"
#include "EncodeBuffer.h"
#include "DecodeBuffer.h"
#include "util.h"



void
TextCompressor::encodeChar(unsigned char ch, EncodeBuffer& encodeBuffer)
{
  // encode each successive character of text using
  // a predictive model where the last 1.625 characters
  // (low order 7 bits of the previous character, plus the
  // low order 4 bits of the character before that, plus
  // the low order  bits of the character before that)
  // are used to find the right cache...
  encodeBuffer.encodeCachedValue((unsigned int)ch, 8, cache_[key_]);
  key_ = (((key_ & 0x0f) << 7) | ((key_ & 0x180) << 4) | (ch & 0x7f));
}


unsigned char
TextCompressor::decodeChar(DecodeBuffer& decodeBuffer)
{
  unsigned char nextChar;
  decodeBuffer.decodeCachedValue(nextChar, 8, cache_[key_]);
  key_ = (((key_ & 0x0f) << 7) | ((key_ & 0x180) << 4) | (nextChar & 0x7f));
  return nextChar;
}
