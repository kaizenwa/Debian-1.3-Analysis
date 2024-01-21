/*******************************************************************
 *
 *  TTRaster.h                                                 1.1
 *
 *    The FreeType rasterizer (specification).
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  NOTES:
 *
 *   The rasterizer is able to work with a block of memory given
 *   by a client application called the 'Render Pool'.            
 *
 *   Now supports gray-level rendering.
 *
 ******************************************************************/

#ifndef TTRASTER_H
#define TTRASTER_H

#define TT_Flow_Down   -1  /* down-flowing bitmap and profile  */
#define TT_Flow_Up      1  /* up-flowing bitmap and profile    */
#define TT_Flow_Error   0  /* an error occured while rendering */

#define Raster_Err_None               0
#define Raster_Err_Not_Ini            1  /* Raster was not initialized      */
#define Raster_Err_Overflow           2  /* Profile table overflow          */
#define Raster_Err_Neg_Height         3  /* Negative height encountered !!  */
#define Raster_Err_Invalid            4  /* Invalid value encountered       */
#define Raster_Err_Gray_Unsupported   5  /* Graylevel rendering unsupported */

#define TT_DropOut_Control_None     0  /* No Drop-out control               */
#define TT_DropOut_Control_Simple   1  /* Simple dropout control (rule #3)  */
#define TT_DropOut_Control_Complex  2  /* Complex dropout control (rule #4) */

  struct _TRasterBlock
  {
    unsigned long    rows;    /* Number of rows             */
    unsigned long    cols;    /* Number of 8-pixels columns */
    unsigned long    width;   /* Number of pixels per line  */
    int              flow;    /* Bitmap orientation         */

    void*            bitmap;  /* bit/pixmap buffer          */
    unsigned long    size;    /* bit/pixmap size in bytes   */
  };
  typedef struct _TRasterBlock  TRasterBlock;
  typedef TRasterBlock         *PRasterBlock;
  /* A structure used to describe the target bitmap or pixmap to the  */
  /* renderer. Not that there is nothing in this structure that gives */
  /* the nature of the buffer.                                        */

  struct _TGlyphRecord
  {
    unsigned long    outlines;    /* number of contours in glyph          */
    short*           outStarts;   /* points to an array of each contour's */
                                  /* start point index                    */
    unsigned long    points;      /* number of points in the glyph        */
    long*            xCoord;      /* table of x coordinates               */
    long*            yCoord;      /* table of y coordinates               */
    char*            flag;        /* table of flags                       */
  };
  typedef struct _TGlyphRecord  TGlyphRecord;
  typedef TGlyphRecord         *PGlyphRecord;
  /* A structure used to describe the source glyph to the renderer */

  extern int  Raster_Error;
  /* Global Raster Error variable */

  int  Render_Glyph( TGlyphRecord* aGlyph );
  /* Render one glyph in the target bitmap */

  int  Render_Gray_Glyph( TGlyphRecord*  aGlyph,
                          char*          palette );
  /* Render one gray-level glyph in the target pixmap     */
  /* palette points to an array of 5 colors used for the rendering */
  /* use nil to reuse the last palette. Default is VGA graylevels  */

  int  InitRasterizer( TRasterBlock*  rasterBlock,
                       long*          profBuffer,
                       long           profSize,
                       char*          grayBuffer,
                       int            grayLength );
  /* Initializes the rasterizer, specifying a target bit/pixmap, */
  /* the render pool as well as the gray lines buffer ( put NULL */
  /* if gray-level rasterization is not desired )                */

  /* NOTE : This interface is due to change in the future */

#endif /* RASTER_H */
