/****************************************************************************}
{*                                                                          *}
{*  fs_dos : FullScreen DOS Graphics Modes Support.                         *}
{*                                                                          *}
{*  This utility component is in charge of managing graphics modes used     *}
{*  by the full-screen FreeType viewer. It also provides a very simple      *}
{*  bitmap/pixmap blitter, as well as a VRAM pointer that may be used       *}
{*  by the raster library component for debugging purpose.                  *}
{*                                                                          *}
{*  Tested with Borland C++ 3.1 and emx-gcc.                                *}
{*                                                                          *}
{*  23-11-96 : Discarded the Borland inline assembly code  - David          *}
{*                                                                          *}
{****************************************************************************/

#include "fullscr.h"
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

/* The following #ifdef are used to define the following macros :          */
/*                                                                         */
/*  - int86  : function to call an interrupt                               */
/*  - reg_ax : the 'ax' register as stored in the REGS struct              */
/*                                                                         */

/* ---- Borland C 3.1 dos support ---------------------------------------- */

#ifdef __BORLANDC__

#include <dos.h>                     /* Includes the declaration of int86  */
#define  reg_ax  regs.x.ax
#endif

/* ---- EMX/Dos compiler support ----------------------------------------- */

#ifdef __EMX__                   
#include <sys/hw.h>  
#define  int86   _int86
#define  reg_ax  regs.x.ax
#endif

/* ---- WATCOM Dos/16 & Dos/32 support ----------------------------------- */

#ifdef __WATCOMC__               
#include <i86.h>
#define  reg_ax  regs.w.ax

#ifdef __386__
#define  int86   int386
#endif

#endif


  char* Vio;
  int   Vio_ScanLineWidth;

  /* Set Graphics Mode */

  void SetGraphScreen( int mode )
  {
    union REGS regs;

    switch (mode)
    {
      case FS_Graphics_Mono:  /* Standard VGA 640x480x16 mode */

        reg_ax = 0x12;
        int86( 0x10, &regs, &regs );

        Vio_ScanLineWidth = 80;
        break;

      case FS_Graphics_Gray:  /* Standard VGA 320x200x256 mode */

        reg_ax = 0x13;
        int86( 0x10, &regs, &regs );

        Vio_ScanLineWidth = 320;
        break;

      default:
        fprintf( stderr, "ERROR : Could not set fullscreen graphics mode\n" );
        exit(5);
    }

/* Unfortunately, EMX does not support the MK_FP macro/function */
/* Maybe we could use MAKEP ??                                  */

#ifdef __EMX__
    Vio = _memaccess( 0xA0000, 0xAFFFF, 1 );
#else
    Vio = (char*)MK_FP( 0xA000, 0 );
#endif
  }

  /* Revert to text mode */

  void RestoreScreen()
  {
    union REGS regs;

    reg_ax = 0x3;
    int86( 0x10, &regs, &regs );
  }

  /* Display bit/pixmap */

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

