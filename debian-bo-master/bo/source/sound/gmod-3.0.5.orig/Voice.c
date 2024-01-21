// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <stdlib.h>
#include <sys/ultrasound.h>

#include "defines.h"
#include "Sample.h"
#include "Voice.h"

void sync_time();

void
Voice::startNote(int dev, int note) const
{
  SEQ_DECLAREBUF();
  int vol;

  vol = ((((volume_ * envelopeVol_) / 64) + tremoloVol_) * tremorVol_ *
	 mainVolume_) / 255;
  
  if (vol > 255)
    vol = 255;
  else if (vol < 0)
    vol = 0;

  if (volType_ == VOL_LOG)
    {
      int new_volume;
      unsigned char bits;

      bits = (vol & 0xf0) >> 4;
      new_volume = 1 << bits;
      vol &= 0x0f;
      if (bits >= 4)
	{
	  new_volume |= (vol << (bits - 4));
	  for (bits -= 4; bits > 0; bits--)
	    new_volume |= (1 << (bits - 1));
	}
      else
	new_volume |= (vol >> (4 - bits));

      vol = (unsigned char)(new_volume / 256);
    }
  else
    vol /= 2;

  SEQ_START_NOTE(dev, channel_, note, (unsigned char)vol);
}

void
Voice::keyOff()
{
  if (volumeEnvelopePos_ == -1)
    whatChanged_ |= KEY_CHANGED;
  else
    inVolumeSustain_ = MY_FALSE;

  if (panEnvelopePos_ == -1)
    inPanSustain_ = MY_FALSE;
}

void
Voice::note(int note, int vol)
{
  whatChanged_ |= NOTE_CHANGED;
  note_ = note;
  volume_ = vol;

  if (tremoloWave_ <= 3)
    tremoloPosition_ = 0;

  if (sample_ && sample_->hasVolumeEnvelope())
    {
      int envVol;

      volumeEnvelopePos_ = 0;
      inVolumeSustain_ = MY_TRUE;
      fadeVol_ = 65535;
      envVol =  sample_->volumeEnvelopeY(volumeEnvelopePos_, fadeVol_,
					 MY_TRUE);
      
      if (envVol != -1)
	{
	  envelopeVol(envVol);
	  volumeEnvelopePos_ = 0;
	}
    }
  else
    {
      volumeEnvelopePos_ = -1;
      envelopeVol(64);
    }
	  
  if (sample_ && sample_->hasPanEnvelope())
    {
      unsigned short dummy = 65535;
      int envPan;

      panEnvelopePos_ = 0;
      inPanSustain_ = MY_TRUE;
      envPan = sample_->panEnvelopeY(panEnvelopePos_, dummy, MY_TRUE);

      if (envPan != -1)
	{
	  envelopePan(envPan);
	  panEnvelopePos_ = 0;
	}
    }
  else
    {
      panEnvelopePos_ = -1;
      envelopePan(32);
    }
};

void
Voice::setEnvelopePos(int pos)
{
  if (sample_ && sample_->hasVolumeEnvelope())
    volumeEnvelopePos_ = pos;

  if (sample_ && sample_->hasPanEnvelope())
    panEnvelopePos_ = pos;
}

void
Voice::tremor(int amount)
{
  tremor_ = (amount >> 4) & 0x0f;
  tremTotal_ = tremor_ + (amount & 0x0f);

  if (!tremTotal_)
    tremor_ = 0;
}

void
Voice::tremolo(int amount)
{
  tremolo_ = (amount >> 4) & 0x0f;
  
  if (!tremolo_)
    tremolo_ = tremoloOld_;
  else
    tremoloOld_ = tremolo_;

  if (amount &= 0x0f)
    tremoloDepth_ = amount;
}

void
Voice::resetEffects()
{
  if (tremor_)
    {
      tremor_ = 0;

      if (!tremorVol_)
	{
	  tremorVol_ = 1;
	  whatChanged_ = VOL_CHANGED;
	}
    }

  if (tremolo_)
    {
      tremolo_ = 0;
      tremoloVol_ = 0;
      whatChanged_ = VOL_CHANGED;
    }
}

void
Voice::doTick(int tickNo)
{
  extern short vibra_table[][NUM_VIBRA];
  int scratch;

  if ((volumeEnvelopePos_ != -1) && !noteDelayed_ && sample_)
    {
      scratch = sample_->volumeEnvelopeY(volumeEnvelopePos_, fadeVol_,
					 inVolumeSustain_);

      if (scratch == -1)
	volumeEnvelopePos_ = -1;
      else
	envelopeVol(scratch);
    }

  if ((panEnvelopePos_ != -1) && !noteDelayed_ && sample_)
    {
      unsigned short dummy = 65535;

      scratch = sample_->panEnvelopeY(panEnvelopePos_, dummy, inPanSustain_);

      if (scratch == -1)
	panEnvelopePos_ = -1;  // stop panning
      else
	envelopePan(scratch);
    }
      
  if (tremor_)
    {
      scratch = tickNo % tremTotal_;

      if (scratch == tremor_)
	{
	  tremorVol_ = 0;
	  whatChanged_ |= VOL_CHANGED;
	}
      else if (!scratch)
	{
	  tremorVol_ = 1;
	  whatChanged_ |= VOL_CHANGED;
	}
    }
  
  if (tremolo_ && tickNo)
    {
      unsigned char vol;

      vol = vibra_table[tremoloWave_ & 0x03][tremoloPosition_] *
	tremoloDepth_ * VOL_SLIDE_RATE / 128;
      tremoloPosition_ += tremolo_;
      tremoloPosition_ %= NUM_VIBRA;

      if (vol != tremoloVol_)
	{
	  tremoloVol_ = vol;
	  whatChanged_ |= VOL_CHANGED;
	}
    }

  if (panSlide_ && tickNo)
    {
      scratch = pan_ + panSlide_;

      if (scratch < 0)
	{
	  scratch = 0;
	  panSlide_ = 0;
	}
      else if (scratch > 255)
	{
	  scratch = 255;
	  panSlide_ = 0;
	}

      pan(scratch);
    }

  if (globalVolSlide_ && tickNo)
    {
      scratch = mainVolume_ + globalVolSlide_;

      if (scratch < 0)
	{
	  scratch = 0;
	  globalVolSlide_ = 0;
	}
      else if (scratch > 255)
	{
	  scratch = 255;
	  globalVolSlide_ = 0;
	}
      
      mainVolume(scratch);
    }
}

void
Voice::doUpdates(int dev)
{
  SEQ_DECLAREBUF();

  if (!noteDelayed_)
    {
      if (whatChanged_)
	sync_time();

      if (whatChanged_ & PAN_CHANGED)
	{
	  int panVal;
	  
	  panVal = pan_ + (envelopePan_ - 32) * (128 - abs(128 - pan_)) / 32;
	  
	  if (panVal > 255)
	    panVal = 255;
	  else if (panVal < 0)
	    panVal = 0;
	  
	  if (panFactor_ != 100)
	    {
	      panVal -= 128;
	      panVal = (panVal * panFactor_) / 100;
	      panVal += 128;
	    }

	  SEQ_CONTROL(dev, channel_, CTL_PAN, panVal); 

	  if (!(whatChanged_ & NOTE_CHANGED))
	    {
	      panVal = ((panVal * 2) - 240) / 32 + 7;
	      GUS_VOICEBALA(dev, channel_, panVal);
	    }
	}

      if (whatChanged_ & NOTE_CHANGED)
	startNote(dev, note_);
      else if (whatChanged_ & VOL_CHANGED)
	startNote(dev, 255);

      if (whatChanged_ & KEY_CHANGED)
	  GUS_VOICEOFF(dev, channel_);
      
      whatChanged_ = 0;
    }
}
