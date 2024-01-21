/* $Id: zoom.c,v 1.2 1996/11/19 12:47:20 robert Exp $ */
/****************************************************************************}
{*                                                                          *}
{*  The FreeType project - a Free and Portable Quality TrueType Renderer.   *}
{*                                                                          *}
{*  Copyright 1996  D. Turner, R.Wilhelm, W. Lemberg                        *}
{*                                                                          *}
{*  ZOOM : A Simple Glyph Viewer. Now supports graylevels rendering         *}
{*         with the '-g' option.                                            *}
{*                                                                          *}
{*                                                                          *}
{*  Keys :                                                                  *}
{*                                                                          *}
{*  x :   fine counter_clockwise rotation                                   *}
{*  c :   fine clockwise rotation                                           *}
{*                                                                          *}
{*  v :   fast counter_clockwise rotation                                   *}
{*  b :   fast clockwise rotation                                           *}
{*                                                                          *}
{*  + :   fast scale up                                                     *}
{*  - :   fast scale down                                                   *}
{*  u :   fine scale down                                                   *}
{*  j :   fine scale up                                                     *}
{*                                                                          *}
{*  l :   go to next glyph                                                  *}
{*  k :   go to previous glyph                                              *}
{*                                                                          *}
{*  o :   go to tenth next glyph                                            *}
{*  i :   go to tenth previous glyph                                        *}
{*                                                                          *}
{*  0 :   go to hundredth next glyph                                        *}
{*  9 :   go to hundredth previous glyph                                    *}
{*                                                                          *}
{*  ESC :   exit                                                            *}
{*                                                                          *}
{*                                                                          *}
{*  NOTE : This is just a test program that is used to show off and         *}
{*         debug the current engine; which is still in alpha. In no         *}
{*         way does it shows the final high-level interface that            *}
{*         client applications will use. Wait for at least a beta for       *}
{*         this.                                                            *}
{*                                                                          *}
{****************************************************************************/

#include "tttypes.h"
#include "ttcalc.h"
#include "tttables.h"
#include "ttmemory.h"
#include "ttraster.h"
#include "fullscr.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>    /* libc ANSI */

#if defined(__OS2__) || defined(__MSDOS__)
#include <conio.h>
#endif

#define Pi  3.1415926535

#define  Precis   64
#define  Precis2  32

#define  PrecisAux  1024

#define  Centre_X   320
#define  Centre_Y   225

#define  Profile_Buff_Size  64000      /* Size of the render pool   */
                                       /* Minimum is around 4 Kb    */
                                       /* experiment to see how its */
                                       /* size impacts on raster    */
                                       /* performance..             */
#define  Font_Buff_Size  128000        /* this buffer holds all     */
                                       /* font specific data.       */

#ifdef X11
  extern char  WindowName[];
#endif

  PStorage    Font_Buffer;

  PGlyph          Gl;
  PGlyphContours  curGlyphContours;

  int   num_pts;
  int   num_ctr;

  int   glyfArray;

  PShort  epts_ctr;

  PStorage   x_coord;
  PStorage   y_coord;
  PByte      Flag;

  Long   ymin, ymax, xmax, xmin, xsize;
  Int    res, resB;

  float  resR, resX, resY;

  Int  numPoints, numContours;

  TRasterBlock  Bit;

  Int           Rotation;
  Int           Fail;
  Int           Num;

  char          GrayLines[1024];
  int           gray_render;


  void  Init_Engine()
  {
    PByte  p;

    if ( gray_render )
    {
      Bit.rows  = 200;
      Bit.cols  = 320;
      Bit.width = 320*4;
      Bit.flow  = TT_Flow_Down;
      Bit.size  = 320*200;
    }
    else
    {
      Bit.rows  = 450;
      Bit.cols  = 80;
      Bit.width = 640;
      Bit.flow  = TT_Flow_Down;
      Bit.size  = 80*450;
    }

    Bit.bitmap = (void*)malloc( Bit.size );
    if ( !Bit.bitmap )
    {
      fprintf( stderr, "ERROR: Not enough memory to allocate bitmap!!\n" );
      exit( 1 );
    }

    /* XXX Note that the render pool should be allocated within the   */
    /*     Font Pool. For various reasons, and because we're still in */
    /*     pre-alpha :) we still malloc it...                         */

    p = (PByte)malloc( Profile_Buff_Size );
    if ( !p )
    {
      fprintf( stderr,
               "ERROR: Not enough memory to allocate render pool!\n" );
      exit( 1 );
    }

    if ( gray_render )
      InitRasterizer( &Bit, (long*)p, Profile_Buff_Size, GrayLines, 512 );
    else
      InitRasterizer( &Bit, (long*)p, Profile_Buff_Size, NULL, 0 );

    memset( Bit.bitmap, 0, Bit.size );
  }


  void ClearData()
  {
    memset( Bit.bitmap, 0, Bit.size );

    if ( x_coord )
      free( x_coord );
    if ( y_coord )
      free( y_coord );
    if ( Flag )
      free( Flag );

#ifdef X11
    sprintf( WindowName, "zoom: glyph index = %d", Num );
#endif
  }


  Bool  LoadTrueTypeChar( Int idx )
  {
    Long    off;
    float   x, y;
    UShort  j;
    UShort  EM;
    float   Cr, Sr;

    x_coord = NULL;
    y_coord = NULL;
    Flag    = NULL;

    if ( idx < 0 || idx > num_glyphs ) return FAILURE;

    if ( ( Gl = Load_TrueType_Glyph( idx ) ) == NULL )
      return FAILURE;

    numPoints        = Gl->numberOfPoints;
    numContours      = Gl->numberOfContours;
    curGlyphContours = Gl->contours;

    x_coord = (PStorage)malloc( sizeof(TT_Fixed) * numPoints );
    y_coord = (PStorage)malloc( sizeof(TT_Fixed) * numPoints );
    Flag    = (PByte)malloc( numPoints );
    if ( x_coord == NULL || y_coord == NULL || Flag == NULL )
      return FAILURE;

    xmin = Gl->xMin;
    xmax = Gl->xMax;
    ymin = Gl->yMin;
    ymax = Gl->yMax;

    EM = font_header->units_per_EM;

    xmax -= xmin;
    ymax -= ymin;

    res--;
    resR = (float)res / EM / 2;

    xmax = xmax*resR + 0.5;
    ymax = ymax*resR + 0.5;

    Cr = cos( Rotation*Pi / 512 );
    Sr = sin( Rotation*Pi / 512 );

    for ( j = 0; j < numPoints; j++ )
    {
      x = Gl->points[j].x * (float)res/EM;
      y = Gl->points[j].y * (float)res/EM;

      off = Precis * ( Cr*(x-xmax) + Sr*(y-ymax) );
      x_coord[j] = Precis*Centre_X + off;

      off = Precis * ( -Sr*(x-xmax) + Cr*(y-ymax) );
      y_coord[j] = Precis*Centre_Y + off;

      Flag[j] = Gl->points[j].flag & 1;
    }

    xsize = ( xmax+7 ) / 8;
    res++;

    return SUCCESS;
  }


  Bool  ConvertRaster()
  {
    short  B[64];
    Int    i;

    TGlyphRecord  G;

    for ( i = 0; i < numContours; i++ )
      B[i] = curGlyphContours[i].finish;

    G.outlines  = numContours;
    G.outStarts = B;
    G.points    = numPoints;
    G.xCoord    = x_coord;
    G.yCoord    = y_coord;
    G.flag      = Flag;

    /* Note : for gray-levels, we use the default palette */
    /*        make up your own if necessary               */

    if ( gray_render )
      return Render_Gray_Glyph( &G, NULL );
    else
      return Render_Glyph( &G );

    Release_TrueType_Glyph ( Gl );
  }


  int Process_Key( char c )
  {
    switch (c)
    {
    case (char)27:
      return 1;

    case 'x':
      Rotation = ( Rotation-1 ) & 1023;
      break;

    case 'c':
      Rotation = ( Rotation+1 ) & 1023;
      break;

    case 'v':
      Rotation = ( Rotation-16 ) & 1023;
      break;

    case 'b':
      Rotation = ( Rotation+16 ) & 1023;
      break;

    case '+':
      if ( res < 1040 ) res += 10; else res = 1050;
      break;

    case '-':
      if ( res > 11 ) res -= 10; else res = 1;
      break;

    case '9':
      if ( Num > 100 ) Num-= 100; else Num = 0;
      break;

    case '0':
      if ( Num < num_glyphs-101 ) Num += 100; else Num = num_glyphs-1;
      break;

    case 'i':
      if ( Num > 10 ) Num-= 10; else Num = 0;
      break;

    case 'o':
      if ( Num < num_glyphs-11 ) Num += 10; else Num = num_glyphs-1;
      break;

    case 'k':
      if ( Num > 0 ) Num--;
      break;

    case 'l':
      if (Num < num_glyphs-1) Num++;
      break;

    case 'u':
      if ( res > 0 ) res--;
      break;

    case 'j':
      if ( res < 450 ) res++;
      break;
    }
    return 0;
  }

#if defined(X11)

  /* This function is called from x11_disp.c X11_Events() function */

  int Process_Input( char c )
  {
    if ( Process_Key( c ) ) return 1;

    ClearData();

    if ( LoadTrueTypeChar( Num ) )
    {
      if (ConvertRaster())
      {
        if ( gray_render )
          Display_Bitmap_On_Screen( (char*)Bit.bitmap, 200,320 );
        else
          Display_Bitmap_On_Screen( (char*)Bit.bitmap, 450, 80 );
      }
      else Fail++;
    }
    else
      Fail++;
    return 0;
  }

#endif


  int main( int argc, char** argv )
  {

#if defined(X11)
    void X11_events();
#endif

    int     i;
    char    filename[128+4];
    char*   execname;

    Font_Buffer = (PStorage)malloc( Font_Buff_Size );
    if ( !Font_Buffer )
    {
      fprintf( stderr, "Error: Could not even allocate font pool!!\n" );
      exit( 1 );
    }

    Init_FontPool( Font_Buffer, Font_Buff_Size );

    curGlyphContours = NULL;

    num_pts = 0;
    num_ctr = 0;

    execname    = argv[0];
    gray_render = 0;

    if ( argc > 1 && !strcmp( argv[1], "-g" ) )
    {
      argc--;
      argv++;
      gray_render = 1;
    }

    if ( argc != 2 )
    {
      fprintf( stderr, "Zoom: simple TrueType glyph viewer - part of the FreeType project\n" );
      fprintf( stderr, "-----------------------------------------------------------------\n\n");
      fprintf( stderr, "Usage: %s [-g] fontname[.ttf]\n\n", execname );
      fprintf( stderr, "  where '-g' asks for gray-levels rendering\n\n" );
      exit(1);
    }

    i = strlen( argv[1] );
    while ( i > 0 && argv[1][i] != '\\' )
    {
      if ( argv[1][i] == '.' )
        i = 0;
      i--;
    }

    filename[128] = 0;
    strncpy( filename, argv[1], 128 );
    if ( i >= 0 )
      strncpy( filename + strlen(filename), ".ttf", 4 );

    if ( !Open_TrueType_File( filename ) )
    {
      fprintf( stderr, "Error, could not find/open %s\n\n", filename );
      exit( 1 );
    }

    res  = 450;
    resB = (res+7) / 8;

    Load_TrueType_Tables();

    Load_TrueType_MaxProfile();

    Init_Engine();

    if ( gray_render )
      SetGraphScreen( FS_Graphics_Gray );
    else
      SetGraphScreen( FS_Graphics_Mono );

    Num      = 0;
    Fail     = 0;
    Rotation = 0;

#if defined(X11)

    if ( LoadTrueTypeChar(Num) )
    {
     if (ConvertRaster())
     {
       if ( gray_render )
         Display_Bitmap_On_Screen( (char*)Bit.bitmap, 200,320 );
       else
         Display_Bitmap_On_Screen( (char*)Bit.bitmap, 450, 80 );
     }
     else Fail++;

     X11_events();
    }
#else   /* X11 */

    for ( ;; )
    {
      if ( LoadTrueTypeChar( Num ) )
      {
       if (ConvertRaster())

         if ( gray_render )
           Display_Bitmap_On_Screen( (char*)Bit.bitmap, 200,320 );
         else
           Display_Bitmap_On_Screen( (char*)Bit.bitmap, 450, 80 );
       
       else Fail++;
      }
      else
        Fail++;

#ifdef __amigaos__
      if ( Process_Key( GetEvent()) ) goto Fin;
#else
      if ( Process_Key(getch()) ) goto Fin;
#endif

      ClearData();
    }

  Fin:

#endif  /* X11 */

    RestoreScreen();
    Close_TrueType_File();
    return 0;
  }
