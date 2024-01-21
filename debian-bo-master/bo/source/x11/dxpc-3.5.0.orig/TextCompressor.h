#ifndef TextCompressor_H
#define TextCompressor_H

#include "CharCache.h"

class EncodeBuffer;
class DecodeBuffer;

class TextCompressor {
 public:
  TextCompressor(CharCache* cache): cache_(cache), key_(0) {}

  void encodeChar(unsigned char ch, EncodeBuffer&);
  unsigned char decodeChar(DecodeBuffer&);
  void reset() { key_ = 0; }

 private:
  CharCache* cache_;
  unsigned int key_;
};

#endif /* TextCompressor_H */
