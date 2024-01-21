
/*
   crc.c : Multiple Devices tools for Linux
           Copyright (C) 1994-96 Marc ZYNGIER
	   <zyngier@ufr-info-p7.ibp.fr> or
	   <maz@gloups.fdn.fr>

   A lot of this file comes from the modules pakage.
   Please flame ME if something is wrong here, NOT the original authors.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   You should have received a copy of the GNU General Public License
   (for example /usr/src/linux/COPYING); if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  
*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/md.h>

int open_or_die (char *file)
{
  int fd;
  
  if ((fd=open (file, O_RDONLY))==-1)
  {
    perror (file);
    exit (EXIT_FAILURE);
  }

  return fd;
}

/* stollen from modules tools... */
static unsigned long crctab32[] =
{
#include "crc32.tab"
};

static unsigned long updcrc32(register unsigned char *s)
{
  static unsigned long crcreg;
  register unsigned long c, *t;
  
  if (s == 0)
    c = 0xffffffffU;
  else
  {
    c = crcreg;
    t = crctab32;
    while (*s)
      c = t[((int)c ^ (*s++)) & 0xff] ^ (c >> 8);
  }
  crcreg = c;
  return c ^ 0xffffffffU;
}

unsigned long calc_checksum (char *entry[], int pers)
{
  int i, fd, sz=0;
  unsigned long cap;
  char text[1024];

  updcrc32 (NULL);
  
  for (i=1; entry[i]; i++)
  {    
    fd=open_or_die (entry[i]);

    if (ioctl (fd, BLKGETSIZE, &cap))
    {
      perror (entry[i]);
      exit (EXIT_FAILURE);
    }

    sz += sprintf (text + sz, "%08lx%s%d", cap, entry[i], i);
  }

  sz += sprintf (text + sz, "%08x", pers);

  return updcrc32 (text);
}
