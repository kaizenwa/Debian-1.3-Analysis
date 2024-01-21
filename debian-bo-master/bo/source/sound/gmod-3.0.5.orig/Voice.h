// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef VOICE_H
#define VOICE_H

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#define VOL_CHANGED 0x01
#define NOTE_CHANGED 0x02
#define PAN_CHANGED 0x04
#define KEY_CHANGED 0x10

class Sample;

class Voice
{
public:
  Voice() : channel_(0), sample_(0), volume_(0), mainVolume_(255),
    envelopeVol_(0), tremoloVol_(0), tremorVol_(1), volumeEnvelopePos_(-1),
    panEnvelopePos_(-1), globalVolSlide_(0), volType_(0),
    whatChanged_(VOL_CHANGED | PAN_CHANGED),
    noteDelayed_(0), envelopePan_(0), pan_(0), panSlide_(0), panFactor_(100),
    tremor_(0), tremolo_(0), tremoloOld_(0), tremoloDepth_(0),
    tremoloPosition_(0), tremoloWave_(0) {};

  void channel(int chan) { channel_ = chan; };

  void envelopeVol(int vol);
  int envelopeVol() const { return envelopeVol_; };

  void note(int, int);
  void sample(const Sample *s) { sample_ = s; };
  void volume(int);
  int volume() const { return volume_; };
  void mainVolume(int);
  void tremor(int);
  void tremolo(int);
  void tremoloWave(int);
  void envelopePan(int);
  int envelopePan() const { return envelopePan_; };
  void pan(int);
  void panSlide(int amt) { panSlide_ = amt; };
  void globalVolSlide(int amt) { globalVolSlide_ = amt; };
  void setEnvelopePos(int pos);
  void panFactor(int factor) { panFactor_ = factor; };

  void volType(int type) { volType_ = type; };

  void resetEffects();
  void doTick(int);
  void doUpdates(int);

  void noteDelay(int x) { noteDelayed_ = x; };
  void keyOff();

private:
  void startNote(int, int) const;

  unsigned char channel_;
  unsigned char note_;
  const Sample *sample_;

  unsigned char volume_; // 0..255
  unsigned char mainVolume_; // 0..255
  unsigned char envelopeVol_; // 0..64
  signed char tremoloVol_; // -128..127 (adjustment)
  unsigned char tremorVol_; // 0..1

  int volumeEnvelopePos_;
  char inVolumeSustain_;
  unsigned short fadeVol_;

  int panEnvelopePos_;
  char inPanSustain_;
  
  signed char globalVolSlide_;
  unsigned char volType_;

  unsigned char whatChanged_;
  unsigned char noteDelayed_;

  unsigned char envelopePan_; // 0..64
  unsigned char pan_;  // 0..255
  signed char panSlide_;
  signed char panFactor_;

  unsigned char tremor_;
  unsigned char tremTotal_;

  unsigned char tremolo_;
  unsigned char tremoloOld_;
  unsigned char tremoloDepth_;
  unsigned char tremoloPosition_;
  unsigned char tremoloWave_;
};

inline void
Voice::envelopeVol(int vol)
{
  if (envelopeVol_ != vol)
    {
      envelopeVol_ = vol;
      whatChanged_ |= VOL_CHANGED;
    }
}

inline void
Voice::envelopePan(int pan)
{
  if (envelopePan_ != pan)
    {
      envelopePan_ = pan;
      whatChanged_ |= PAN_CHANGED;
    }
}

inline void
Voice::volume(int vol)
{
  if (volume_ != vol)
    {
      volume_ = vol;
      whatChanged_ |= VOL_CHANGED;
    }
};

inline void
Voice::pan(int panVal)
{
  if (pan_ != panVal)
    {
      pan_ = panVal;
      whatChanged_ |= PAN_CHANGED;
    }
}

inline void
Voice::mainVolume(int vol)
{
  if (mainVolume_ != vol)
    {
      mainVolume_ = vol;
      whatChanged_ |= VOL_CHANGED;
    }
}

inline void
Voice::tremoloWave(int parm)
{
  if ((parm & 0x03) == 0x03)
    parm -= 1;

  tremoloWave_ = parm;
}

#endif
