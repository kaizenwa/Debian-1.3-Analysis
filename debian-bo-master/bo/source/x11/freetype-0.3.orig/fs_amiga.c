/*************************************************************************}
{*                                                                       *}
{*  fs_amiga : FullScreen Amiga Graphics Modes Support.                  *}
{*                                                                       *}
{*  This utility component is in charge of managing graphics modes used  *}
{*  by the full-screen FreeType viewer. It also provides a very simple   *}
{*  bitmap/pixmap blitter, as well as a VRAM pointer that may be used    *}
{*  by the raster library component for debugging purpose.               *}
{*                                                                       *}
{*  This implementation is AmigaOS dependent.                            *}
{*  Compiled using GCC 2.7.2.1 & ixemul.library 45.0                     *}
{*                                                                       *}
{*  29/11/96 : Miguel A. Pérez-Valdenebro - map@medusa.es                *}
{*                                                                       *}
{*************************************************************************/

/*
 *  Standard includes
 */ 
#include <stdlib.h>

/*
 *  AmigaOS includes
 */  
#include <exec/types.h>
#include <intuition/intuition.h>
#include <intuition/screens.h>

#ifdef __GNUC__
#include <inline/exec.h>
#include <inline/intuition.h>
#include <inline/dos.h>
#else
#include <clib/exec_protos.h>
#include <clib/intuition_protos.h>
#include <clib/dos_protos.h>
#endif

/*
 *  Freetype includes
 */  
#include "fullscr.h"

/*
 *  Some screen definitions
 */  
#define SCREEN_WIDTH  640
#define SCREEN_HEIGHT 512
#define SCREEN_DEPTH    1

/*
 *  External variables
 */  
extern struct Library *SysBase;
extern struct Library *DOSBase;

/*
 *  Global variables
 */  
struct Library * IntuitionBase = NULL;

int Vio_ScanLineWidth;

/*
 *  Local variables
 */  
static struct Screen * fts = NULL;
static struct Window * ftw = NULL;
static struct NewWindow nw;

static char *  Vio;

/*
 * Exit gracefully
 */
void AmigaCleanUp( void )
{
  if( ftw )
    CloseWindow( ftw );

  if( fts )
    CloseScreen( fts );

  if( IntuitionBase )
    CloseLibrary( IntuitionBase );
}

/*
 * Restores Screen to its original state
 */
void RestoreScreen( void )
{
/* Do nothing */
}

/*
 * Open libraries & custom screen
 */
static int AmigaInit( void )
{
  /*
   * CleanUp at exit
   */
  if( atexit( AmigaCleanUp ) )
  {
    PutStr("atexit() failed\n");
    return -1;
  }

  /*
   * Open intuition library
   */
  IntuitionBase = (struct Library *)OpenLibrary( "intuition.library", 37L );
  if( IntuitionBase == NULL )
  {
    PutStr("Could not open intuition library\n");
    return -1;
  }

  /*
   * Open custom screen
   */
  fts = (struct Screen *)OpenScreenTags( NULL,
                            SA_DisplayID,(PAL_MONITOR_ID | HIRESLACE_KEY),
                            SA_Width,SCREEN_WIDTH,
                            SA_Height,SCREEN_HEIGHT,
                            SA_Depth,SCREEN_DEPTH,
                            TAG_DONE );

  if( fts == NULL )
  {
    PutStr("Could not open custom screen\n");
    return -1;
  }

  /*
   * Initialise Intuition window (needed to get events in its port) 
   */
  nw.LeftEdge = nw.TopEdge = 0;
  nw.Width = fts->Width;
  nw.Height = fts->Height;
  nw.FirstGadget = NULL;
  nw.Title = NULL;
  nw.IDCMPFlags= IDCMP_VANILLAKEY | IDCMP_MOUSEBUTTONS;
  nw.Flags = WFLG_BACKDROP | WFLG_BORDERLESS | WFLG_RMBTRAP | WFLG_ACTIVATE;
  nw.Type = CUSTOMSCREEN;
  nw.Screen = fts;

  /*
   * Open Intuition window
   */
  ftw = OpenWindow(&nw);
  if( ftw == NULL )
  {
    PutStr("Could not open intuition window\n");
    return -1;
  }

  Vio = (char *) fts->BitMap.Planes[0];
  Vio_ScanLineWidth = fts->BitMap.BytesPerRow;

  return 0;
}

/*
 *  Get events in the window
 */
char GetEvent( void )
{
  struct IntuiMessage * msg;
  ULONG class;
  USHORT code;

  WaitPort( ftw->UserPort );

  while( ( msg = (struct IntuiMessage *)GetMsg( ftw->UserPort ) ) )
  {
    class = msg->Class;
    code = msg->Code;

    ReplyMsg( (struct Message *)msg );

    switch( class )
    {
      case IDCMP_MOUSEBUTTONS:
        return (char)27;

      case IDCMP_VANILLAKEY:
        return (char)code;
    }
  }

  return '\0';
}

/*
 * Sets Amiga graphics mode
 */
void SetGraphScreen( int mode )
{
  if( mode == FS_Graphics_Gray )
  {
    PutStr("Gray mode not supported yet\n");
    exit(20);
  }

  if( AmigaInit() == -1 )
    exit(20);
}

/*
 *  Display Bitmap
 */  
void  Display_Bitmap_On_Screen( char * buffer, int line, int col )
{
    int    y;
    char*  target;

    target = Vio + ( line - 1 ) * Vio_ScanLineWidth;

    for ( y = 0; y < line; y++ )
    {
      CopyMem( buffer, target, col );
      target -= Vio_ScanLineWidth;
      buffer += col;
    }
}
