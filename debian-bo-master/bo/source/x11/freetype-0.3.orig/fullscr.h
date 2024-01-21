/****************************************************************************}
{*                                                                          *}
{*  fullscr.h : FullScreen Graphics Modes Support.                          *}
{*                                                                          *}
{*  This utility component is in charge of managing graphics modes used     *}
{*  by the full-screen FreeType viewer. It also provides a very simple      *}
{*  bitmap/pixmap blitter, as well as a VRAM pointer that may be used       *}
{*  by the raster library component for debugging purpose.                  *}
{*                                                                          *}
{*  bodies in files : fs_os2.c and fs_dos.c                                 *}
{*                                                                          *}
{****************************************************************************/

#ifndef FULLSCR_H
#define FULLSCR_H

#define FS_Graphics_Mono  1
#define FS_Graphics_Gray  2

  extern char*  Vio;
  /* Pointer to VRAM buffer */

  extern int    Vio_ScanLineWidth;
  /* Scan Line width in bytes */

  void  SetGraphScreen( int  mode );
  /* Set a Graphics Mode, chosen from the FS_Graphics_xxx list */

  void  RestoreScreen();
  /* Restore previous ( or text ) video mode */

  void  Display_Bitmap_On_Screen( char* buffer, int line, int col );
  /* Display a bitmap of 'line' lines, and 'col' columns ( each */
  /* column made of 8 bits )                                    */

#ifdef __amigaos__
  char GetEvent( void );
  /* Get events in the window port */
#endif
#endif /* FULLSCR_H */
