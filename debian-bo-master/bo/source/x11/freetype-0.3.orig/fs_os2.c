/****************************************************************************}
{*                                                                          *}
{*  fs_os2 : FullScreen OS/2 Graphics Modes Support.                        *}
{*                                                                          *}
{*  This utility component is in charge of managing graphics modes used     *}
{*  by the full-screen FreeType viewer. It also provides a very simple      *}
{*  bitmap/pixmap blitter, as well as a VRAM pointer that may be used       *}
{*  by the raster library component for debugging purpose.                  *}
{*                                                                          *}
{*  Tested with gcc-2.6.3                                                   *}
{*                                                                          *}
{*                                                                          *}
{****************************************************************************/

#include "fullscr.h"

#define INCL_SUB

#include <os2.h>
#ifdef __EMX__
#include <os2thunk.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

  char*  Vio;

  VIOMODEINFO    OrgMode;
  long           VioBufOfs;
  unsigned short Status;

  int            Vio_ScanLineWidth;

  /* BIOS Video Mode 640x480x16 */

  VIOMODEINFO VioMode_640x480x16 =
  {
    sizeof(VIOMODEINFO),
    VGMT_OTHER + VGMT_GRAPHICS,
    COLORS_16,
    80,
    35,
    640,
    480
  };

  VIOMODEINFO VioMode_320x200x256 =
  {
    sizeof(VIOMODEINFO),
    VGMT_OTHER + VGMT_GRAPHICS,
    8,
    40,
    25,
    320,
    200
  };


  VIOPHYSBUF VioBuf =
  {
    (void*)0xA0000L,
    64*1024
  };

  /* Restores Screen to its original state */

  void RestoreScreen()
  {
    VioSetMode( &OrgMode, 0 );
  }

  /* Sets graphics mode */

  void SetGraphScreen( int mode )
  {
    int rc;

    OrgMode.cb = sizeof(VIOMODEINFO);
    VioGetMode( &OrgMode, 0 );

    switch (mode)
    {
      case FS_Graphics_Mono :  
        rc = VioSetMode( &VioMode_640x480x16, 0 );
        Vio_ScanLineWidth = 80;
        break;

      case FS_Graphics_Gray :  
        rc = VioSetMode( &VioMode_320x200x256, 0 );
        Vio_ScanLineWidth = 320;
        break;

      default: rc = -1;
    }

    if ( rc )
    {
      fprintf( stderr, "ERROR : Could not set fullscreen graphics mode\n" );
      exit(5);
    }

    if ( VioGetPhysBuf( &VioBuf, 0 ) )
    {
      fprintf( stderr, "ERROR : Could not access VRAM\n" );
      exit(7);
    }

    VioBufOfs = (long)MAKEP( VioBuf.asel[0], 0 );

    memset( (void*)VioBufOfs, 0, 64*1024 );
    Vio = (char*)VioBufOfs;
  };

  /* Display a bit/pixmap */

  void  Display_Bitmap_On_Screen( char* buffer, int line, int col )
  {
    int    y;
    char*  target;

    target = Vio + ( line-1 )*Vio_ScanLineWidth;

    for ( y = 0; y < line; y++ )
    {
      memcpy( target, buffer, col );
      target -= Vio_ScanLineWidth;
      buffer += col;
    }
  };

