/* "tape on a disk" emulator subroutines for MS-DOS and OS/2.
   Copyright (C) 1989 Kai Uwe Rommel */

/* DMA problems with 64k borders are not handled here but by OS/2 itself
   and by the DOS DISKACC-API simulation routines in DISKACC\DISKAPI.C */


#include <stdio.h>
#include <stdlib.h>

#include "diskacc.h"


static USHORT	handle, drive, sides, tracks, sectors;
static ULONG	current, size;


int dsk_open (char *path, int oflag, int mode)
{
  drive = *path - '0';

  handle = DskOpen(drive, &sides, &tracks, &sectors);

  if ( handle == -1 )
    return -1;

  size = 512L * sides * tracks * sectors;
  current = 0L;

  return handle;
}


int dsk_close(int fildes)
{
  if ( fildes == handle)
    DskClose(handle);

  return 0;
}


int dsk_read(int fildes, char *buf, unsigned nbyte)
{
  unsigned side, track, sector, nsects, ok, chunk;

  if ( fildes != handle )
    return -1;

  if ( nbyte % 512 != 0 )
    return -1;

  sector = (unsigned) (current / 512L);

  track  = sector / sectors;
  sector -= track * sectors;

  side   = track % sides;
  track  /= sides;

  nsects = nbyte / 512;
  ok = 0;

  while ( nsects != 0 )
  {
    if ( track >= tracks )
      break;

    chunk = min(sectors - sector, nsects);

    if ( DskRead(handle, side, track, sector + 1, chunk, buf) != 0 )
      break;

    ok += chunk;
    nsects -= chunk;
    sector = 0;

    if ( ++side >= sides )
    {
      side = 0;
      track++;
    }

    current += 512L * chunk;
    buf += 512 * chunk;
  }

  return ok * 512;
}


int dsk_write(int fildes, char *buf, unsigned nbyte)
{
  unsigned side, track, sector, nsects, ok, chunk;

  if ( fildes != handle )
    return -1;

  if ( nbyte % 512 != 0 )
    return -1;

  sector = (unsigned) (current / 512L);

  track  = sector / sectors;
  sector -= track * sectors;

  side   = track % sides;
  track  /= sides;

  nsects = nbyte / 512;
  ok = 0;

  while ( nsects != 0 )
  {
    if ( track >= tracks )
      break;

    chunk = min(sectors - sector, nsects);

    if ( DskWrite(handle, side, track, sector + 1, chunk, buf) != 0 )
      break;

    ok += chunk;
    nsects -= chunk;
    sector = 0;

    if ( ++side >= sides )
    {
      side = 0;
      track++;
    }

    current += 512L * chunk;
    buf += 512 * chunk;
  }

  return ok * 512;
}


long dsk_lseek(int fildes, long offset, int whence)
{
  if ( fildes != handle )
    return -1;

  if ( offset % 512L != 0L )
    return -1;

  switch ( whence )
  {
  case SEEK_SET:
    current = offset;
    break;
  case SEEK_CUR:
    current += offset;
    break;
  case SEEK_END:
    current = size + offset;
    break;
  }

  return current;
}
