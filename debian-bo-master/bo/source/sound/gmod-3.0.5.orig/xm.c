// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <errno.h>
#include <string.h>

#include "xm.h"
#include "Sequencer.h"

void catchup(FILE *, int, int *);
void remove_noprint(char *);

int
XM_sample::load(Sequencer &seq, FILE * mod_fd, int sample_no, int cutFactor,
		void *pt, void *fp)
{
  u_char header[214];
  u_long header_size, skip_length;
  u_short num_samples;
  int bytes_read = 0;
  unsigned short *period_table = (unsigned short *)pt;
  int *file_pos = (int *)fp;
  
  _ok = 0;
  fread(header, 1, 29, mod_fd);
  *file_pos += 29;

  strncpy(_name, (char *)(header + 4), 22);
  _name[22] = '\0';
  remove_noprint(_name);

  header_size = INTEL_LONG(header);
  num_samples = INTEL_SHORT(header + 27);
      
  if (num_samples > 0)
    {
      fread(header, 1, 214, mod_fd);
      *file_pos += 214;

      catchup(mod_fd, *file_pos + (header_size - 243), file_pos);
      header_size = INTEL_LONG(header);

      volumeEnvelope.load(int(header[196]), int(header[198]), int(header[199]),
			  int(header[200]), int(header[204]),
			  (unsigned short)(INTEL_SHORT(header + 210)),
			  header + 100);

      panEnvelope.load(int(header[197]), int(header[201]), int(header[202]),
		       int(header[203]), int(header[205]), 0, header + 148);

      fread(header, 1, 40, mod_fd);
      *file_pos += 40;

      catchup(mod_fd, *file_pos + (header_size - 40), file_pos);

      skip_length = 0;

      /* skip any remaining sample headers */

      while(num_samples > 1)
	{
	  fread(header + 40, 1, 40, mod_fd);
	  skip_length += INTEL_LONG(header + 40);
	  *file_pos += 40;
	  catchup(mod_fd, *file_pos + (header_size - 40), file_pos);
	  num_samples--;
	}
      
      _length = INTEL_LONG(header);
      loop_start = INTEL_LONG(header + 4);
      loop_end = loop_start + INTEL_LONG(header + 8);

      if ((BYTE(header + 14) & 0x03) == 1)
	_mode = WAVE_LOOPING;
      else if ((BYTE(header + 14) & 0x03) == 2)
	_mode = WAVE_LOOPING | WAVE_BIDIR_LOOP;
      else
	_mode = 0;

      if ((BYTE(header + 14) & 0x10) > 0)
	_mode |= WAVE_16_BITS;

      panning_ = header[15];
      // should check range of relative note!!

      if (((signed char)(header[16])) != 0)
	_base_note = 111978496 / period_table[-((signed char)(header[16])) + 60 - NOTE_BASE];
      else
	_base_note = C2FREQ;

     _base_freq = NTSC_RATE;
	  
      if (_length > 0)
	{
	  int rc;

	  rc = seq.patch_load(mod_fd, sample_no, *this, bytes_read,
			      cutFactor);

	  if (!rc)
	    _ok = 1;
	  
	  *file_pos += bytes_read;

	  if (rc == -ENOSPC)
	    bytes_read = -bytes_read;
	}

      if (BYTE(header + 12) > 0)
	_volume = (BYTE(header + 12) * 4) - 1;
      else
	_volume = 0;

      _finetune = (signed char)(BYTE(header + 13)) * 100 / 128;

      /* skip any remaining samples for this instrument */

      catchup(mod_fd, *file_pos + skip_length, file_pos);
    }
  else
    catchup(mod_fd, *file_pos + (header_size - 29), file_pos);

  return bytes_read;
}

void
XM_sample::decode(char *data) const
{
  signed short old_val, new_val;
  int i;

  old_val = 0;

  if (_mode & WAVE_16_BITS)
    {
      for (i = 0; i < _length; i+=2)
	{
	  new_val = (signed short)(INTEL_SHORT(data + i)) + old_val;
	  data[i] = new_val & 0x00ff;
	  data[i+1] = (new_val & 0xff00) >> 8;
	  old_val = new_val;
	}
    }
  else
    {
      for (i = 0; i < _length; i++)
	{
	  new_val = (signed short)(data[i]) + old_val;
	  data[i] = new_val;
	  old_val = new_val;
	}
    }
}
