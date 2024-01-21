/****************************************************************************}
{*                                                                          *}
{*  win_x11 : X11R6 Windowed Display support.                               *}
{*                                                                          *}
{*  This utility component is in charge of managing the display window      *}
{*  used by the X11 FreeType viewer. Note that the Vio pointer is not       *}
{*  in VRAM anymore. It should thus be used to debug the rasterizer.        *}
{*                                                                          *}
{*  body in file : win_x11.c                                                *}
{*                                                                          *}
{****************************************************************************/

#ifndef WIN_X11_H
#define WIN_X11_H

#include "lib/tttypes.h"

#define FS_Graphics_Mono  1
#define FS_Graphics_Gray  2

  extern char* Vio;
  /* Pointer to VRAM buffer */

  extern int    Vio_ScanLineWidth;
  /* Scan Line width in bytes */

  void  SetGraphScreen( int mode );
  /* Set a Graphics Mode, chosen from the FS_Graphics_xxx list */


  void  RestoreScreen();
  /* Restore previous ( or text ) video mode */

  void  Display_Bitmap_On_Screen( PByte buffer, Int line, Int col );
  /* Display a bitmap of 'line' lines, and 'col' columns ( each */
  /* column made of 8 bits )                                    */

#endif /* WIN_X11_H */
