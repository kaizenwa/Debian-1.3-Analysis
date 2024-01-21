/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "../config.h"
#include "volume.h"
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/soundcard.h>
#include <sys/ioctl.h>
#include <sys/errno.h>

static int mixer = -1;
extern char *mixer_name;
extern const unsigned char *sources[];
extern char *myname;

int mix_open(int request) {
  if (mixer<0) {
    int devmask;
    mixer = open(mixer_name, O_RDONLY);
    if (mixer<0) return -1;

    if (ioctl(mixer, SOUND_MIXER_READ_DEVMASK, &devmask)<0) {
      fprintf(stderr, _("%s: failed to read mixer capabilities.\n"), myname);
      goto fail;
    }

    if (!(devmask & (1<<request))) {
      fprintf(stderr, _("%s: mixer id `%s' not supported by your system.\n"),
	      myname, sources[request]);
      errno=ENODEV;
      goto fail;
    }

  }
  return mixer;

fail:
  close(mixer);
  return -1;
}

void mix_close() {
  if (mixer>=0) {
    close(mixer);
    mixer=-1;
  }
}

int mix_set_volume(int req, int *left, int *right) {
  int res,n;

  if (mixer>=0) {
    if ((*left>=0) && (*right>=0)) {
      n = (*left & 0x7F) + ((*right & 0x7F)<<8);
      res=ioctl(mixer, MIXER_WRITE(req), &n);
    } else {
      res=0;
    }
    if (res>=0) {
      res=ioctl(mixer,MIXER_READ(req), &n);
      *left = n&0x7F;
      *right = (n>>8)&0x7F;
    }
  } else
    res=-1;
  if (res<0) *left = *right = 0;
  return res;
}

int mix_get_volume(int req, int *left, int *right) {
  int res,n;

  if (mixer>=0) {
    res=ioctl(mixer, MIXER_READ(req), &n);
    *left = n&0x7F;
    *right = (n>>8)&0x7F;
  } else
    res=-1;
  if (res<0) *left = *right = 0;
  return res;
}
