// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <stdlib.h>
#include <string.h>

#ifdef USE_LOCAL
#include "soundcard.h"
#else
#include <sys/soundcard.h>
#endif

#include "Sample.h"
#include "Sequencer.h"

int
Sequencer::patch_load (FILE * mod_fd, int sample_num,
		       const Sample &samp, int &bytes_read,
		       int cutFactor) const
{
  int x_bytes_read;
  int len;
  unsigned short loop_flags;
  int loop_start;
  int loop_end;
  int i, rc;
  struct patch_info tpatch;
  struct patch_info *patch;

  memset(&tpatch, 0, sizeof(patch_info));
  samp.get_patch_info(tpatch);
  patch = (struct patch_info *)malloc(sizeof(struct patch_info) +
				      tpatch.len + 2);

  memcpy(patch, &tpatch, sizeof(struct patch_info));
  patch->key = GUS_PATCH;
  patch->low_note = 0;
  patch->high_note = 0x7fffffff;
  patch->volume = 0;
  patch->panning = -112;
  patch->device_no = gus_dev;
  patch->instr_no = sample_num;

  len = patch->len;

  /* fix loop_end if necessary */

  if (patch->loop_end > len)
    patch->loop_end = len;

  loop_start = patch->loop_start;
  loop_end = patch->loop_end;

  if (loop_start >= loop_end)
    patch->mode &= ~WAVE_LOOPING;

  loop_flags = patch->mode;

  if ((bytes_read = fread(patch->data, 1, len, mod_fd)) != len)
    {
      if (((loop_flags & WAVE_16_BITS) && (bytes_read > 1)) ||
	  (!(loop_flags & WAVE_16_BITS) && (bytes_read > 0)))
	{
	  /* Warning: Short sample */

	  x_bytes_read = bytes_read;

	  if ((loop_flags & WAVE_16_BITS) && ((bytes_read % 2) != 0))
	    x_bytes_read--;

	  while (x_bytes_read < len)
	    {
	      if (loop_flags & WAVE_16_BITS)
		{
		  patch->data[x_bytes_read] = patch->data[x_bytes_read - 2];
		  x_bytes_read++;
		  patch->data[x_bytes_read] = patch->data[x_bytes_read - 2];
		}
	      else
		patch->data[x_bytes_read] = patch->data[x_bytes_read - 1];
	      x_bytes_read++;
	    }
	}
      else
	{
	  free(patch);
	  return -ENODATA;
	}
    }

  /* "decode" sample if necessary */
  samp.decode(patch->data);

  /* convert 16 bits to 8 if necessary */

  if ((loop_flags & WAVE_16_BITS) && ((len > 256*1024) || (cutFactor > 1)))
    {
      for (i = 0; i < len - 1; i+= 2)
	patch->data[i / 2] = patch->data[i + 1];
      
      len = (patch->len /= 2);
      loop_start = (patch->loop_start /= 2);
      loop_end = (patch->loop_end /= 2);
      loop_flags &= ~WAVE_16_BITS;
      patch->base_freq /= 2;
    }

  /* extend looped samples, since length might equal loop end */

  if (loop_flags & WAVE_LOOPING)
    {
      if (loop_flags & WAVE_16_BITS)
	{
	  if (len >= 2)
	    {
	      patch->data[len] = patch->data[len - 2];
	      patch->data[len + 1] = patch->data[len - 1];
	      len += 2;
	      patch->len += 2;
	    }
	}
      else if (len >= 1)
	{
	  patch->data[len] = patch->data[len - 1];
	  len++;
	  (patch->len)++;
	}
    }

  /* Shorten samples to fit in memory.  Do not shorten a looped sample if */
  /* the start and end are not evenly divisible by cutFactor; otherwise, */
  /* the sample may go out of tune. */

  if ((cutFactor == 2) && (loop_flags & WAVE_LOOPING) && (loop_start % 2) &&
      (loop_end % 2))
    {
      loop_start = (patch->loop_start -= 1);
      loop_end = (patch->loop_end -= 1);
    }
      
  if ((cutFactor > 1) && !((loop_flags & WAVE_LOOPING) &&
			   ((loop_start % cutFactor) ||
			    (loop_end % cutFactor))))
   {
      for (i = 0; i < len / cutFactor; i++)
	patch->data[i] = patch->data[i * cutFactor];

      len = (patch->len /= cutFactor);
      loop_start = (patch->loop_start /= cutFactor);
      loop_end = (patch->loop_end /= cutFactor);
      patch->base_freq /= cutFactor;
    }

  /* try to remove loop clicking */

  if ((loop_flags & WAVE_LOOPING) && (loop_end >= 2) &&
      !(loop_flags & WAVE_16_BITS))
    {
      patch->data[loop_end] = patch->data[loop_start];

      if (loop_flags & WAVE_UNSIGNED)
	patch->data[loop_end - 1] =
	  ((unsigned char)(patch->data[loop_end - 2]) +
	   (unsigned char)(patch->data[loop_end])) / 2;
      else
	patch->data[loop_end - 1] =
	  ((signed char)(patch->data[loop_end - 2]) +
	   (signed char)(patch->data[loop_end])) / 2;
    }

  if (memory() < len)
    rc = -ENOSPC;
  else
    {
#define write ::write
      rc = SEQ_WRPATCH2(patch, sizeof(struct patch_info) + len);
#undef write
    }

  free(patch);
  return rc;
}
