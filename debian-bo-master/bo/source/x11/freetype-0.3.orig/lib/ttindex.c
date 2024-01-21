/*******************************************************************
 *
 *  TTIndex.c
 *
 *    TrueType index and cmap handling (body).
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute 
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 ******************************************************************/

#include "ttindex.h"
#include "tttables.h"

  Int  code_to_index0( UShort  charCode, PCMap0Table  cmap0 );
  Int  code_to_index2( UShort  charCode, PCMap2Table  cmap2 );
  Int  code_to_index4( UShort  charCode, PCMap4Table  cmap4 );
  Int  code_to_index6( UShort  charCode, PCMap6Table  cmap6 );


/*******************************************************************
 *
 *  Function    : TT_select_CMap
 *
 *  Description : Select the CMap to use.
 *
 *  Input  :  UShort  platformID
 *            UShort  platformEncodingID
 *
 *  Output :  Array index into cmap_tables. -1 on failure.
 *
 ******************************************************************/

  Int  TT_select_CMap( UShort  platformID,
                       UShort  platformEncodingID )
  {
    Int  i;

    for ( i = 0; i < num_CME; i++ )
    {
      if ( ( cmap_dir_entries[i].platformID == platformID ) &&
           ( cmap_dir_entries[i].platformEncodingID == platformEncodingID ) )

        return i;
    }

    return -1;
  }

/*******************************************************************
 *
 *  Function    : TT_code_to_index
 *
 *  Description : Convert the character code into a glyph index.
 *                Uses the CMap index returned from TT_select_CMap().
 *
 *  Input  :  UShort  charCode
 *            Int     cmapIndex
 *
 *  Output :  Glyph index into the glyphs array.
 *            0 if the glyph does not exist (`missing character glyph')
 *            -1 on failure.
 *
 ******************************************************************/

  Int  TT_code_to_index( UShort charCode,
                         Int    cmapIndex )
  {
    switch ( cmap_tables[cmapIndex].format )
    {
    case 0:
      return code_to_index0( charCode,
                             cmap_tables[cmapIndex].cmap.cmap0 );
    case 2:
      return code_to_index2( charCode,
                             cmap_tables[cmapIndex].cmap.cmap2 );
    case 4:
      return code_to_index4( charCode,
                             cmap_tables[cmapIndex].cmap.cmap4 );
    case 6:
      return code_to_index6( charCode,
                             cmap_tables[cmapIndex].cmap.cmap6 );
    default:
      return -1;
    }
  }

/*******************************************************************
 *
 *  Function    : code_to_index0
 *
 *  Description : Convert the character code into a glyph index.
 *                Uses format 0.
 *                charCode will be masked to get a value in the range
 *                0x00-0xFF.
 *
 *  Input  :  UShort       charCode
 *            PCMap0Table  cmap0
 *
 *  Output :  Glyph index into the glyphs array.
 *            0 if the glyph does not exist.
 *
 ******************************************************************/

  Int  code_to_index0( UShort       charCode,
                       PCMap0Table  cmap0 )
  {
    return cmap0->glyphIdArray[charCode & 0xFF];
  }

/*******************************************************************
 *
 *  Function    : code_to_index2
 *
 *  Description : Convert the character code into a glyph index.
 *                Uses format 2.
 *
 *  Input  :  UShort       charCode
 *            PCMap2Table  cmap2
 *
 *  Output :  Glyph index into the glyphs array.
 *            0 if the glyph does not exist.
 *
 ******************************************************************/

  Int  code_to_index2( UShort       charCode,
                       PCMap2Table  cmap2 )
  {
    UShort           index1;
    TCMap2SubHeader  sh2;

    index1 = cmap2->header.subHeaderKeys[charCode <= 0xFF ?
                                         charCode : (charCode >> 8)];

    if ( index1 == 0 )
      return cmap2->glyphIdArray[charCode];     /* 8bit character code */

    else                                        /* 16bit character code */
    {
      sh2 = cmap2->subHeaders[index1];

      if ( (charCode & 0xFF) < sh2.firstCode )
        return 0;

      if ( (charCode & 0xFF) >= (sh2.firstCode + sh2.entryCount) )
        return 0;

      return cmap2->glyphIdArray[sh2.idRangeOffset / 2 + (charCode & 0xFF) -
                                 sh2.firstCode] + sh2.idDelta;
    }
  }

/*******************************************************************
 *
 *  Function    : code_to_index4
 *
 *  Description : Convert the character code into a glyph index.
 *                Uses format 4.
 *
 *  Input  :  UShort       charCode
 *            PCMap4Table  cmap4
 *
 *  Output :  Glyph index into the glyphs array.
 *            0 if the glyph does not exist.
 *
 ******************************************************************/

  Int  code_to_index4( UShort       charCode,
                       PCMap4Table  cmap4 )
  {
    UShort         index1, segCount;
    Int            i;
    TCMap4Segment  seg4;

    segCount = cmap4->header.segCountX2 / 2;

    for (i = 0; i < segCount; i++ )
      if ( charCode <= cmap4->segments[i].endCount )
        break;

    seg4 = cmap4->segments[i];

    if ( charCode < seg4.startCount )
      return 0;

    if (seg4.idRangeOffset == 0 )
      return charCode + seg4.idDelta;

    else
    {
      index1 = seg4.idRangeOffset / 2 + (charCode - seg4.startCount) -
               (segCount - i);

      if ( cmap4->glyphIdArray[index1] == 0 )
        return 0;
      else
        return cmap4->glyphIdArray[index1] + seg4.idDelta;
    }
  }

/*******************************************************************
 *
 *  Function    : code_to_index6
 *
 *  Description : Convert the character code into a glyph index.
 *                Uses format 6.
 *
 *  Input  :  UShort       charCode
 *            PCMap6Table  cmap6
 *
 *  Output :  Glyph index into the glyphs array.
 *            0 if the glyph does not exist (`missing character glyph')
 *
 ******************************************************************/

  Int  code_to_index6( UShort       charCode,
                       PCMap6Table  cmap6 )
  {
    UShort firstCode;

    firstCode = cmap6->header.firstCode;

    if ( charCode < firstCode )
      return 0;

    if ( charCode >= (firstCode + cmap6->header.entryCount) )
      return 0;

    return cmap6->glyphIdArray[charCode - firstCode];
  }
