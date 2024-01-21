// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include <sys/ultrasound.h>

#include "commands.h"
#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Sample.h"
#include "Sequencer.h"

static unsigned char s3m_extend_tab[] =
{CMD_INFO, CMD_GLISSANDO, CMD_FINETUNE, CMD_VIBRA_WAVE,	/* S0 S1 S2 S3 */
 CMD_TREMOLO_WAVE, CMD_INFO, CMD_INFO, CMD_INFO,	/* S4 S5 S6 S7 */
 CMD_SET_PAN, CMD_INFO, CMD_INFO, CMD_PATTERN_LOOP,	/* S8 S9 SA SB */
 CMD_CUT_NOTE, CMD_DELAY_NOTE, CMD_DELAY_PAT, CMD_INFO};	/* SC SD SE SF */

void
cvt_xm_play (int channel, unsigned char *effect, unsigned char *parm)
{
  int tmp;

  switch (*effect)
    {
    case CMD_SLIDEUP:
    case CMD_SLIDEDOWN:
    case CMD_SLIDETO:
    case CMD_VIBRATO:
    case CMD_PORTANDVOL:
    case CMD_VIBRAANDVOL:
    case CMD_TREMOLO:
    case CMD_VOLSLIDE:
    case CMD_FINEVOLUP:
    case CMD_FINEVOLDOWN:
    case CMD_PANSLIDE:
    case CMD_RETRIGVOL:
    case CMD_GLOBALVOL_SLIDE:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;
      break;
    case CMD_GLOBAL_VOL:
      if (*parm)
	voices[channel].last_info = *parm;

      if (*parm)
	*parm = (*parm * 4) - 1;
      
      if (*parm > 255)
	*parm = 255;
      break;
    case CMD_XFINEPORTUP:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;

      *effect = CMD_FINEPORTUP;
      *parm &= 0x0f;
      tmp = (int) (*parm) + voices[channel].fineslide_adj;
      if (tmp >= 0)
	{
	  voices[channel].fineslide_adj = tmp % 4;
	  *parm = tmp / 4;
	}
      else
	{
	  voices[channel].fineslide_adj = (signed char) (tmp);
	  *parm = 0;
	}
      break;
    case CMD_XFINEPORTDOWN:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;

      *effect = CMD_FINEPORTDOWN;
      *parm &= 0x0f;
      tmp = (int) (*parm) - voices[channel].fineslide_adj;
      if (tmp >= 0)
	{
	  voices[channel].fineslide_adj = -(tmp % 4);
	  *parm = tmp / 4;
	}
      else
	{
	  voices[channel].fineslide_adj = (signed char) (-tmp);
	  *parm = 0;
	}
      break;
    default:
      if (*parm != 0)
	voices[channel].last_info = *parm;
      if (*effect == CMD_INFO)
	{
	  *effect = 0;
	  *parm = 0;
	}
    }
}

void
cvt_s3m_play (int channel, unsigned char *effect, unsigned char *parm)
{
  int tmp;

  switch (*effect)
    {
    case 0:
      break;
    case CMD_VOLSLIDE:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;

      if ((*parm > 0xf0) && (*parm < 0xff))
	{
	  *effect = CMD_FINEVOLDOWN;
	  *parm &= 0x0f;
	}
      else if (((*parm & 0x0f) == 0x0f) && ((*parm & 0xf0) > 0))
	{
	  *effect = CMD_FINEVOLUP;
	  *parm = (*parm >> 4) & 0x0f;
	}
      else if (((*parm & 0x0f) != 0) && ((*parm & 0xf0) != 0))
	*parm &= 0x0f;
      break;
    case CMD_SLIDEUP:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;

      if (*parm >= 0xf0)
	{
	  *effect = CMD_FINEPORTUP;
	  *parm &= 0x0f;
	}
      else if (*parm >= 0xe0)
	{			/* extra-fine slide, not really supported */
	  *effect = CMD_FINEPORTUP;
	  *parm &= 0x0f;
	  tmp = (int) (*parm) + voices[channel].fineslide_adj;
	  if (tmp >= 0)
	    {
	      voices[channel].fineslide_adj = tmp % 4;
	      *parm = tmp / 4;
	    }
	  else
	    {
	      voices[channel].fineslide_adj = (signed char) (tmp);
	      *parm = 0;
	    }
	}
      break;
    case CMD_SLIDEDOWN:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;

      if (*parm >= 0xf0)
	{
	  *effect = CMD_FINEPORTDOWN;
	  *parm &= 0x0f;
	}
      else if (*parm >= 0xe0)
	{			/* extra-fine slide, not really supported */
	  *effect = CMD_FINEPORTDOWN;
	  *parm &= 0x0f;
	  tmp = (int) (*parm) - voices[channel].fineslide_adj;
	  if (tmp >= 0)
	    {
	      voices[channel].fineslide_adj = -(tmp % 4);
	      *parm = tmp / 4;
	    }
	  else
	    {
	      voices[channel].fineslide_adj = (signed char) (-tmp);
	      *parm = 0;
	    }
	}
      break;
    case CMD_ARPEG2:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;

      *effect = CMD_ARPEG;
      break;
    case CMD_TREMOR:
    case CMD_VIBRAANDVOL:
    case CMD_PORTANDVOL:
    case CMD_SLIDETO:
    case CMD_VIBRATO:
    case CMD_RETRIGVOL:
      if (*parm == 0)
	*parm = voices[channel].last_info;
      else
	voices[channel].last_info = *parm;
      break;
    case CMD_BREAK:
      voices[channel].last_info = *parm;
      *parm = ((*parm >> 4) & 0x0f) * 10 + (*parm & 0x0f);
      break;
    case CMD_FINETUNE:
      voices[channel].last_info = *parm;
      *parm &= 0x0f;
      if (*parm >= 8)
	*parm -= 8;
      else
	*parm += 8;
      break;
    case CMD_GLOBAL_VOL:
      voices[channel].last_info = *parm;
      if (*parm > 0)
	*parm = (*parm * 4) - 1;
      break;
    case CMD_EXTENDED:
      voices[channel].last_info = *parm;
      *effect = s3m_extend_tab[(*parm & 0xf0) >> 4];
      *parm &= 0x0f;

      if (*effect == CMD_SET_PAN)
	*parm *= 17;

      break;
    default:
      voices[channel].last_info = *parm;
    }

  if (*effect == CMD_INFO)
    {
      *effect = 0;
      *parm = 0;
    }
}

int
do_command (int channel, int command, int parm, int parm2, int note, int position, int pattern,
	    struct song_info *song_char, struct effect_info *effects)
{
  extern Sequencer *seq;
  int jump = 0;

  switch (command)
    {

    case CMD_NOP:;
      break;

    case CMD_ARPEG:
      if (parm)
	set_arpeg (channel, parm);
      break;

    case CMD_JUMP:
      jump = MOVE_JUMP;
      effects->position = parm;
      effects->pattern = 0;
      break;

    case CMD_BREAK:
      jump = MOVE_BREAK;
      if (((position + 1) < song_char->songlength) &&
	  (parm < pattern_len[tune[position + 1]]))
	effects->pattern = parm;
      else
	effects->pattern = 0;
      break;

    case CMD_SET_TICKS:
      if (parm)
	TICKS_PER_DIVISION (parm);
      else
	{
#ifdef USE_X
	  sync_time ();
	  SEQ_ECHO_BACK (ECHO_SPEED0);
#else
	  jump = MOVE_EXIT;
#endif
	}
      break;

    case CMD_SET_BPM:
      TEMPO (parm, song_char->clock_speed);
      break;

    case CMD_SLIDEUP:
      if ((parm > 0) && (voices[channel].sample < song_char->nr_samples))
	{
	  set_slideto(channel, parm, song_char->highest_note, SLIDE_UP,
		      song_char->slide_type);
	}
      break;

    case CMD_SLIDEDOWN:
      if ((parm > 0) && (voices[channel].sample < song_char->nr_samples))
	{
	  set_slideto(channel, parm, song_char->lowest_note, SLIDE_DOWN,
		      song_char->slide_type);
	}
      break;

    case CMD_SLIDETO:
      if (voices[channel].sample < song_char->nr_samples)
	set_slideto(channel, parm, note, SLIDE_PORT, song_char->slide_type);
      break;

    case CMD_SETOFFSET:
    case CMD_SETOFFSET_1024:
    case CMD_SETOFFSET_FINE:
      sync_time ();
      /* if there is no note, restart current note to prevent click */
      if (!note)
	{
	  seq->note(channel, voices[channel].note, seq->volume(channel)),

	  // the next doUpdates can be removed once all commands are in
	  // the Voice class
	  seq->doUpdates(channel);
	}

      if (command == CMD_SETOFFSET)
	{
	  GUS_VOICE_POS (gus_dev, channel, parm * 256);
	}
      else if (command == CMD_SETOFFSET_1024)
	{
	  GUS_VOICE_POS (gus_dev, channel, parm * 1024);
	}
      else
	{
	  GUS_VOICE_POS (gus_dev, channel, ((parm * 256) + parm2) * 4);
	}
      break;

    case CMD_VIBRA_WAVE:
      if ((parm & 0x03) == 0x03)
	parm -= 1;
      voices[channel].vibra_wave = parm;
      break;

    case CMD_TREMOLO_WAVE:
      seq->tremoloWave(channel, parm);
      break;

    case CMD_GLISSANDO:
      voices[channel].glissando = parm;
      break;

    case CMD_DELAY_PAT:
      effects->delay_notes = parm;
      break;

    case CMD_CUT_NOTE:
      voices[channel].cut_count = parm;
      break;

    case CMD_DELAY_NOTE:
      if (parm == 0)
	parm = 1;
      voices[channel].delay_count = parm;
      break;

    case CMD_PATTERN_LOOP:
      if (parm == 0)
	voices[channel].pattern = pattern;
      else
	{
	  effects->loop_chan = channel;
	  if (voices[channel].loop_times == 0)
	    {
	      voices[channel].loop_times = parm;
	      effects->pattern = voices[channel].pattern;
	      jump = MOVE_LOOP;
	    }
	  else if (--voices[channel].loop_times > 0)
	    {
	      effects->pattern = voices[channel].pattern;
	      jump = MOVE_LOOP;
	    }
	}
      break;

    case CMD_RETRIGGER:
      voices[channel].retrigger = parm;
      voices[channel].retrig_vol = 0;
      break;

    case CMD_RETRIGVOL:
      voices[channel].retrigger = parm & 0x0f;
      voices[channel].retrig_vol = (parm >> 4) & 0x0f;
      break;

    case CMD_FINEVOLUP:
      voices[channel].finevol = MY_TRUE;
      voices[channel].volslide = parm * VOL_SLIDE_RATE;
      break;

    case CMD_FINEVOLDOWN:
      voices[channel].finevol = MY_TRUE;
      voices[channel].volslide = -parm * VOL_SLIDE_RATE;
      break;

    case CMD_FINEPORTUP:
      if ((parm > 0) && (voices[channel].sample < song_char->nr_samples))
	set_slideto(channel, parm, song_char->highest_note, SLIDE_ONCE,
		    song_char->slide_type);
      break;

    case CMD_FINEPORTDOWN:
      if ((parm > 0) && (voices[channel].sample < song_char->nr_samples))
	set_slideto(channel, parm, song_char->lowest_note, SLIDE_ONCE,
		    song_char->slide_type);
      break;

    case CMD_VOLSLIDE:
      set_volslide (channel, parm, song_char->type);
      break;

    case CMD_PORTANDVOL:
      set_volslide (channel, parm, song_char->type);
      voices[channel].slide_pitch = 1;
      break;

    case CMD_VIBRATO:
      set_vibrato (channel, parm);
      break;

    case CMD_VIBRAANDVOL:
      set_vibrato (channel, 0);
      set_volslide (channel, parm, song_char->type);
      break;

    case CMD_VIBRASPEED:
      voices[channel].vibra_old_rate = parm;
      break;

    case CMD_TREMOLO:
      seq->tremolo(channel, parm);
      break;

    case CMD_TREMOR:
      seq->tremor(channel, parm);
      break;

    case CMD_PANSLIDE:
      if (parm & 0xf0)
	seq->panSlide(channel, (parm >> 4) & 0x0f);
      else
	seq->panSlide(channel, -(parm & 0x0f));
      break;

    case CMD_GLOBALVOL_SLIDE:
      if ((parm >> 4) & 0x0f)
	seq->globalVolSlide(VOL_SLIDE_RATE * ((parm >> 4) & 0x0f));
      else
	seq->globalVolSlide(-VOL_SLIDE_RATE * (parm & 0x0f));
      break;

    case CMD_SETENV_POS:
      seq->setEnvelopePos(channel, parm);
      break;

    default:
      /* printf ("Command %x %02x\n", pat->command, pat->parm1); */
      break;
    }

  return jump;
}

/* do_pre_command:  Commands executed before note is started */

void
do_pre_command (int channel, int command, int parm,
		struct song_info *song_char, struct options_info *options)
{
  extern Sample *samples[];
  extern Sequencer *seq;

  switch (command)
    {
    case 0:
    case CMD_NOP:
      break;

    case CMD_FINETUNE:
      parm &= 0x0f;
      if ((voices[channel].sample < song_char->nr_samples) &&
	  samples[voices[channel].sample]->ok())
	{
	  if (parm <= 7)
	    samples[voices[channel].sample]->finetune((short)(12.5 * parm));
	  else
	    samples[voices[channel].sample]->finetune((short)(12.5 * (parm - 16)));
	}
      break;

    case CMD_SET_PAN:
      seq->pan(channel, parm);
      break;

    case CMD_VOLUME:
      seq->volume(channel, parm);
      break;
    
    case CMD_GLOBAL_VOL:
      seq->mainVolume(options->main_volume * parm / 255);
      break;
    }
}

int
play_note (int channel, int position, int pattern, struct note_info *pat,
	   struct song_info *song_char, struct effect_info *effects,
	   struct options_info *options)
{
  extern Sample *samples[];
  extern Sequencer *seq;

  int jump = 0;
  int sample = pat->sample, note = pat->note, vol, tmp_int;
  int old_sample, old_arpeg, old_vibra;
  unsigned char command = pat->command[1], param = pat->parm1[1];

  if (song_char->type == MODULE_S3M)
    cvt_s3m_play (channel, &command, &param);
  else if (song_char->type == MODULE_XM)
    cvt_xm_play (channel, &command, &param);

  old_arpeg = voices[channel].arpeg_num;
  old_vibra = voices[channel].vibra_rate;

  if (voices[channel].slide_pitch == SLIDE_OFF)
    {
      sync_time ();
      GUS_VOICEOFF (gus_dev, channel);
      voices[channel].slide_pitch = 0;
    }

  if ((command != CMD_NOP) || (pat->note != 0))
    {
      voices[channel].arpeg_num = 0;
      voices[channel].vibra_rate = 0;
      voices[channel].slide_pitch = 0;
      voices[channel].volslide = 0;
      voices[channel].retrigger = 0;
      voices[channel].cut_count = 0;
      voices[channel].delay_count = 0;
      seq->resetEffects(channel);
    }
  else
    {
      if (voices[channel].slide_pitch == SLIDE_ONCE)
        voices[channel].slide_pitch = 0;

      if (voices[channel].finevol == MY_TRUE)
      {
        voices[channel].finevol = MY_FALSE;
        voices[channel].volslide = 0;
      }
    }

  old_sample = voices[channel].sample;

  /* A sample with no note resets the volume to its default, but should not */
  /* retrigger the note.  This really isn't possible with the current */
  /* drivers if the sample changes, so the note is retriggered on a sample */
  /* change */

  if (sample)
    {
      voices[channel].sample = --sample;

      if (old_sample != sample)
	{
	  if ((sample < song_char->nr_samples) && (samples[sample]->ok))
	    {
	      sync_time ();
	      SEQ_SET_PATCH (gus_dev, channel, sample);
	      seq->sample(channel, samples[sample]);

	      if (samples[sample]->pan() > 0)
		seq->pan(channel, samples[sample]->pan());
	    }
	  else
	    seq->sample(channel, 0);
	}

      if (!note || (pat->command[0] == CMD_SLIDETO) ||
	  (command == CMD_SLIDETO))
	{
	  if ((sample < song_char->nr_samples) && samples[sample]->ok())
	    vol = samples[sample]->volume();
	  else
	    vol = 0;

	  if (old_sample != sample)
	      seq->note(channel, voices[channel].note, 0);
	  
	  if ((seq->volume(channel) != vol) &&
	      (pat->command[0] != CMD_VOLUME) &&
	      (command != CMD_VOLUME))
	    seq->volume(channel, vol);
	}
      else
	{
	  if ((sample < song_char->nr_samples) && samples[sample]->ok())
	    seq->volume(channel, samples[sample]->volume());
	  else
	    seq->volume(channel, 0);
	}
    }
  else
    sample = voices[channel].sample;

  if (note == NOTE_STOP)
    {
      seq->keyOff(channel);
      note = 0;
    }

  do_pre_command(channel, pat->command[0], pat->parm1[0], song_char, options);
  do_pre_command(channel, command, param, song_char, options);

  if (note && (pat->command[0] != CMD_SLIDETO) &&
      (command != CMD_SLIDETO))
    {
      sync_time ();

      if ((sample < song_char->nr_samples) && samples[sample]->ok())
	{
	  if (voices[channel].pitchbender || old_arpeg || old_vibra ||
	      (samples[sample]->finetune() != voices[channel].finetune))
	    {
	      /* correct for rounding by the driver */
	      tmp_int = samples[sample]->finetune();
	      if (tmp_int >= 0)
		tmp_int++;
	      else
		tmp_int--;

	      if ((pat->command[0] != CMD_DELAY_NOTE) &&
		  (command != CMD_DELAY_NOTE))
		SEQ_PITCHBEND (gus_dev, channel, tmp_int);
		
	      voices[channel].pitchbender = 0;
	      voices[channel].finetune = samples[sample]->finetune();
	      old_arpeg = 0;
	      old_vibra = 0;
	    }

	  if ((pat->command[0] != CMD_DELAY_NOTE) &&
	      (command != CMD_DELAY_NOTE))
	    seq->note(channel, note, seq->volume(channel));
	  else
	    seq->noteDelay(channel, 1);

	  voices[channel].note = note;
	  voices[channel].fineslide_adj = 0;
	  
	  if (song_char->slide_type == SLIDE_NOTE_LIN)
	    voices[channel].slide_period = note * 100;
	  else
	    voices[channel].slide_period = period_table[note - NOTE_BASE] * 256;

	  if (voices[channel].vibra_wave <= 3)
	    voices[channel].vibra_position = 0;
	}
      else
	GUS_VOICEOFF (gus_dev, channel);
    }

  // Do an update here.  This will not be needed once all the commands
  // are implemented in the Voice class
  seq->doUpdates(channel);

  jump = do_command (channel, pat->command[0], pat->parm1[0], pat->parm2[0], pat->note,
		     position, pattern, song_char, effects);
  jump |= do_command (channel, command, param, pat->parm2[1], pat->note,
		      position, pattern, song_char, effects);

  if (((old_arpeg && !voices[channel].arpeg_num) ||
       (old_vibra && !voices[channel].vibra_rate)) &&
      !pat->note)
    {
      /* correct for rounding by the driver */
      tmp_int = voices[channel].pitchbender + voices[channel].finetune;
      if (tmp_int >= 0)
	tmp_int++;
      else
	tmp_int--;

      sync_time ();
      SEQ_PITCHBEND (gus_dev, channel, tmp_int);
    }

  return jump;
}
