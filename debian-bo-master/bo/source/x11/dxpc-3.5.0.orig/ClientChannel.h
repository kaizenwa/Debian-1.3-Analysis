#ifndef ClientChannel_H
#define ClientChannel_H

#include "Channel.h"
#include "ClientReadBuffer.h"
#include "WriteBuffer.h"
#include "ClientCache.h"
#include "ServerCache.h"
#include "SequenceNumQueue.h"
#include "Stats.h"

class DecodeBuffer;

class ClientChannel: public Channel {
 public:
  ClientChannel(int xClientFD, unsigned int statisticsLevel);
  virtual ~ClientChannel();

  virtual int doRead(EncodeBuffer&);
  virtual int doWrite(const unsigned char* message, unsigned int length);

  void setBigEndian(int flag);

 protected:
  void decodeCharInfo_(DecodeBuffer&, unsigned char*);

  ClientReadBuffer readBuffer_;
  int fd_;
  WriteBuffer writeBuffer_;
  int firstRequest_;
  int firstReply_;

  ClientCache clientCache_;
  ServerCache serverCache_;
  SequenceNumQueue sequenceNumQueue_;

  int bigEndian_;
  unsigned int imageByteOrder_;
  unsigned int bitmapBitOrder_;
  unsigned int scanlineUnit_;
  unsigned int scanlinePad_;

  unsigned int statisticsLevel_;
  Stats stats_;
};

#endif /* ClientChannel_H */
