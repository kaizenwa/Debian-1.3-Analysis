/* DISKAPI.C
 *
 * Autor:    Kai Uwe Rommel
 * Datum:    Thu 28-Dec-1989
 * Stand:    Sun 14-Jan-1990
 *
 * Compiler: MS C ab 5.00
 * System:   PC/MS-DOS ab 3.20
 *
 */

#include <dos.h>

#include "diskacc.h"

#ifndef NULL
#  define NULL	(void *)0
#endif

#define DSTEP(x)       (* (char far *) (0x00400090L + (x)))
#define INC(ptr, x)    (ptr = (PVOID) ((PBYTE) ptr + x * 512))

extern int diskint(int service, int drive, int head, int track,
                   int sector, int nsects, void far *buffer);


static int test_sector(int drive, int side, int track, int sector)
{
  return (diskint(4, drive, side, track, sector, 1, NULL) == 1);
}


INT APIENTRY DskOpen(USHORT drive, PUSHORT sides,
                     PUSHORT tracks, PUSHORT sectors)
{
  int cnt, erg;

  if ( drive > 1 )
    return -1;

  diskint(0, drive, 0, 0, 0, 0, NULL);

  for ( cnt = 0; cnt < 3; cnt++ )
    if ( erg = test_sector(drive, 0, 0, 1) )
      break;

  if ( ! erg )
    return -1;

  for ( cnt = 8; cnt <= 10; cnt ++ )
    if ( test_sector(drive, 0, 0, cnt) )
      *sectors = cnt;
    else
      break;

  if ( *sectors == 10 )
    for ( cnt = 15; cnt <= 20; cnt ++ )
      if ( test_sector(drive, 0, 0, cnt) )
        *sectors = cnt;
      else
        break;

  *sides = test_sector(drive, 1, 0, 1) ? 2 : 1;

  if ( DSTEP(drive) & 0x20 )
    if ( !test_sector(drive, 0, 1, 1) )
    {
       DSTEP(drive) &= ~0x20;

       if ( !test_sector(drive, 0, 1, 1) )
          return -1;
    }

  *tracks = (DSTEP(drive) & 0x20) ? 40 : 80;

  return drive + 1;
}


VOID APIENTRY DskClose(USHORT handle)
{
  if ( (handle < 1) || (2 < handle) )
    return;

  diskint(0, handle - 1, 0, 0, 0, 0, NULL);
}


static int dmaborder(PVOID buf, USHORT nsects)
{
  unsigned long lo, hi;

  lo = ((long) FP_SEG(buf) << 4) + (long) FP_OFF(buf);
  hi = lo + nsects * 512L - 1;

  if ( (lo & 0xFFFF0000) == (hi & 0xFFFF0000) )
    return -1;

  return (int) (((hi & 0xFFFF0000) - lo) / 512L);
}


INT APIENTRY DskRead(USHORT handle, USHORT side, USHORT  track,
                     USHORT sector, USHORT nsects, PVOID buf)
{
  char buffer[1024], *ptr;
  int dma, cnt;

  if ( (handle < 1) || (2 < handle) )
    return -1;

  if ( (dma = dmaborder(buf, nsects)) == -1 )
  {
    if ( diskint(2, handle - 1, side, track, sector, nsects, buf) != nsects )
      return -1;
  }
  else
  {
    ptr = (dmaborder(buffer, 1) == -1) ? buffer : buffer + 512;

    if ( dma > 0 )
    {
      if ( diskint(2, handle - 1, side, track, sector, dma, buf) != dma )
        return -1;

      INC(buf, dma);
      sector += dma;
      nsects -= dma;
    }

    if ( diskint(2, handle - 1, side, track, sector, 1, ptr) != 1 )
      return -1;

    for ( cnt = 0; cnt < 512; cnt++ )
      ((PBYTE) buf)[cnt] = ptr[cnt];

    INC(buf, 1);
    sector++;
    nsects--;

    if ( diskint(2, handle - 1, side, track, sector, nsects, buf) != nsects )
      return -1;
  }

  return 0;
}

 
INT APIENTRY DskWrite(USHORT handle, USHORT side, USHORT  track,
                      USHORT sector, USHORT nsects, PVOID buf)
{
  char buffer[1024], *ptr;
  int dma, cnt;

  if ( (handle < 1) || (2 < handle) )
    return -1;

  if ( (dma = dmaborder(buf, nsects)) == -1 )
  {
    if ( diskint(3, handle - 1, side, track, sector, nsects, buf) != nsects )
      return -1;
  }
  else
  {
    ptr = (dmaborder(buffer, 1) == -1) ? buffer : buffer + 512;

    if ( dma > 0 )
    {
      if ( diskint(3, handle - 1, side, track, sector, dma, buf) != dma )
        return -1;

      INC(buf, dma);
      sector += dma;
      nsects -= dma;
    }

    for ( cnt = 0; cnt < 512; cnt++ )
      ptr[cnt] = ((PBYTE) buf)[cnt];

    if ( diskint(3, handle - 1, side, track, sector, 1, ptr) != 1 )
      return -1;

    INC(buf, 1);
    sector++;
    nsects--;

    if ( diskint(3, handle - 1, side, track, sector, nsects, buf) != nsects )
      return -1;
  }

  return 0;
}

 
/* Ende DISKAPI.C */
