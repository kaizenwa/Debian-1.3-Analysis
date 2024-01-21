#ifndef ServerChannel_H
#define ServerChannel_H

#include "Channel.h"
#include "ServerReadBuffer.h"
#include "WriteBuffer.h"
#include "ClientCache.h"
#include "ServerCache.h"
#include "SequenceNumQueue.h"
#include "Stats.h"


class ServerChannel: public Channel {
 public:
  ServerChannel(int xServerFD, unsigned int statisticsLevel);
  virtual ~ServerChannel();

  virtual int doRead(EncodeBuffer&);
  virtual int doWrite(const unsigned char* message, unsigned int length);

  void setBigEndian(int flag);

 protected:
  void encodeCharInfo_(const unsigned char* nextSrc, EncodeBuffer&);

  ServerReadBuffer readBuffer_;
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
  Stats replyStats_;
};

#endif /* ServerChannel_H */
