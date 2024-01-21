/* DISKACC.C
 *
 * Autor:    Kai Uwe Rommel
 * Datum:    Thu 28-Dec-1989
 * Stand:    Thu 04-Jan-1990
 *
 * Compiler: MS C ab 5.00
 * System:   OS/2 ab 1.1
 *
 */


#define INCL_DOSDEVICES
#define INCL_DOSDEVIOCTL
#define INCL_NOPM
#include <os2.h>

#include "diskacc.h" 

int _acrtused = 0;

typedef struct
{
  BYTE   bCommand;
  USHORT usHead;
  USHORT usCylinder;
  USHORT usFirstSector;
  USHORT cSectors;
  struct
  {
    USHORT usSectorNumber;
    USHORT usSectorSize;
  }
  TrackTable[20];
}
TRACK;


static int test_sector(int handle, int side, int track, int sector)
{
  char buffer[1024];
  TRACK trk;

  trk.bCommand      = 0;
  trk.usHead        = side;
  trk.usCylinder    = track;
  trk.usFirstSector = 0;
  trk.cSectors      = 1;

  trk.TrackTable[0].usSectorNumber = sector;
  trk.TrackTable[0].usSectorSize   = 512;

  return (DosDevIOCtl(buffer, &trk, DSK_READTRACK, IOCTL_DISK, handle) == 0);
}


INT APIENTRY DskOpen(USHORT drive, PUSHORT sides,
                     PUSHORT tracks, PUSHORT sectors)
{
  BIOSPARAMETERBLOCK bpb;
  HFILE handle;
  USHORT action;
  BYTE cmd = 0;
  char name[3];

  name[0] = (char) drive + 'A';
  name[1] = ':';
  name[2] = 0;

  if ( DosOpen(name, &handle, &action, 0L, FILE_NORMAL, FILE_OPEN,
               OPEN_FLAGS_DASD | OPEN_FLAGS_FAIL_ON_ERROR |
               OPEN_ACCESS_READWRITE | OPEN_SHARE_DENYREADWRITE, 0L) != 0 )
    return -1;

  if ( DosDevIOCtl(0L, &cmd, DSK_LOCKDRIVE, IOCTL_DISK, handle) != 0 )
  {
    DosClose(handle);
    return -1;
  }

  if ( DosDevIOCtl(&bpb, &cmd, DSK_GETDEVICEPARAMS, IOCTL_DISK, handle) != 0 )
  {
    DosDevIOCtl(0L, &cmd, DSK_UNLOCKDRIVE, IOCTL_DISK, handle);
    DosClose(handle);
    return -1;
  }

  *sectors = bpb.usSectorsPerTrack;
  *tracks  = bpb.cCylinders;
  *sides   = bpb.cHeads;

  if ( *sectors >= 15 )
    if ( !test_sector(handle, 0, 0, 15) )
    {
      if ( *sectors == 15 )
        *tracks = 40;

      *sectors = 9;
    }

  return handle;
}


VOID APIENTRY DskClose(USHORT handle)
{
  BYTE cmd = 0;

  DosDevIOCtl(0L, &cmd, DSK_UNLOCKDRIVE, IOCTL_DISK, handle);
  DosClose(handle);
}


INT APIENTRY DskRead(USHORT handle, USHORT side, USHORT  track,
                     USHORT sector, USHORT nsects, PVOID buf)
{
  TRACK trk;
  int cnt;

  trk.bCommand      = 0;
  trk.usHead        = side;
  trk.usCylinder    = track;
  trk.usFirstSector = 0;
  trk.cSectors      = nsects;

  for ( cnt = 0; cnt < nsects; cnt++ )
  {
    trk.TrackTable[cnt].usSectorNumber = sector + cnt;
    trk.TrackTable[cnt].usSectorSize   = 512;
  }

  return DosDevIOCtl(buf, &trk, DSK_READTRACK, IOCTL_DISK, handle);
}

 
INT APIENTRY DskWrite(USHORT handle, USHORT side, USHORT  track,
                      USHORT sector, USHORT nsects, PVOID buf)
{
  TRACK trk;
  int cnt;

  trk.bCommand      = 0;
  trk.usHead        = side;
  trk.usCylinder    = track;
  trk.usFirstSector = 0;
  trk.cSectors      = nsects;

  for ( cnt = 0; cnt < nsects; cnt++ )
  {
    trk.TrackTable[cnt].usSectorNumber = sector + cnt;
    trk.TrackTable[cnt].usSectorSize   = 512;
  }

  return DosDevIOCtl(buf, &trk, DSK_WRITETRACK, IOCTL_DISK, handle);
}

 
/* Ende DISKACC.C */
