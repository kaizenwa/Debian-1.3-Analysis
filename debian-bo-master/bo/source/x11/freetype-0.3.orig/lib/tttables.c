/*******************************************************************
 *
 *  TTTables.c                                                1.0
 *
 *    TrueType Tables structures and handling (body).
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

#include "tttables.h"
#include "tttags.h"
#include "ttfile.h"
#include "ttcalc.h"
#include "ttmemory.h"
#include <stdlib.h>     /* for malloc() and free() */
#include <string.h>

  TGraphicsState  Gs;
  PShort          Cvt;
  Int             CvtSize;

  Long  Scale1;
  Long  Scale2;

  TT_F26Dot6  PointSize;

  TTableDir           table_dir;
  PTableDirEntries    table_dir_entries = (PTableDirEntries)0;
  Int                 num_TDE = 0;

  TCMapDir            cmap_dir;
  PCMapDirEntries     cmap_dir_entries = (PCMapDirEntries)0;
  PCMapTables         cmap_tables = (PCMapTables)0;
  Int                 num_CME = 0;
 
  TMaxProfile         maxProfile;
 
  THeader            *font_header = (THeader*)0;
 
  TLoca              *glyph_locations = (TLoca*)0;
 
  Int                 num_glyphs = 0;

/*******************************************************************
 *
 *  Function    : Load_TrueType_Tables
 *
 *  Description : Load the TrueType table directory.
 *
 *  Input  :  None
 *
 *  Output :  True on success. False on failure.
 *
 ******************************************************************/

  Bool Load_TrueType_Tables()
  {
    Int t, l;
#ifdef DEBUG
    char buf[5];
#endif

    if ( !READ_ULong ( table_dir.version       ) ||
         !READ_UShort( table_dir.numTables     ) ||
         !READ_UShort( table_dir.searchRange   ) ||
         !READ_UShort( table_dir.entrySelector ) ||
         !READ_UShort( table_dir.rangeShift    ) )
         
      return FAILURE;

#ifdef DEBUG
    fprintf( stderr, "Table directory version : %ld\n", 
                     table_dir.version );

    fprintf( stderr, "Number of tables        : %d\n",
                     table_dir.numTables );
#endif

    num_TDE = table_dir.numTables;

    l = sizeof(TTableDirEntry) * num_TDE;

    if ( !ALLOC( l, table_dir_entries ) ) return FAILURE;

    l = 0;

    for ( t = 0; t < num_TDE; t++ )
    {
      table_dir_entries[t].checksum = 0;

      if ( !READ_ULong( table_dir_entries[t].tag      ) ||
           !READ_ULong( table_dir_entries[t].checksum ) ||
           !READ_ULong( table_dir_entries[t].offset   ) ||
           !READ_ULong( table_dir_entries[t].length   ) )

        return FAILURE;

      table_dir_entries[t].checksum = 0;

#ifdef DEBUG
      fprintf( stderr, "%s : %lx : %ld\n", 
                       Tags_to_String( table_dir_entries[t].tag, buf ),
                       table_dir_entries[t].offset, 
                       table_dir_entries[t].length );
#endif
    }

    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    :  LookUp_TrueType_Table
 *
 *  Description :  Looks for a table by its tag.
 *
 *  Input  :  Long tag    MacOS ressource fork like four-letters
 *                        table tag (padded with space characters if
 *                        necessary)
 *
 *  Output :  table index on success, -1 on failure.
 *
 ******************************************************************/

  Int LookUp_TrueType_Table( Long tag )
  {
    Int first, last, mid, diff;
#ifdef DEBUG
    char buf[5];
#endif

    if ( table_dir_entries )
    {
      first = 0;
      last = num_TDE;

      while (first <= last)
      {
        mid = (last - first) / 2 + first;
        diff = table_dir_entries[mid].tag - tag;

        if ( diff == 0 )
        {
#ifdef DEBUG
          fprintf( stderr, "'%s' found\n", Tags_to_String( tag, buf ));
#endif
          return mid;

        } else if ( diff < 0 )
          first = mid + 1;
        else
          last = mid - 1;
        }
    }

#ifdef DEBUG
    fprintf( stderr, "'%s' unavailable\n", Tags_to_String( tag, buf ));
#endif
    return -1;
  }

/*******************************************************************
 *
 *  Function    : Load_TrueType_Header
 *
 *  Description : Loads the Font Header into memory.
 *
 *  Input  :  None
 *
 *  Output :  True on success. False on failure.
 *
 ******************************************************************/

  Bool Load_TrueType_Header()
  {
    Int i;

    i = LookUp_TrueType_Table(TTAG_head);
    if ( i < 0 ) return FAILURE;

    if ( ALLOC( sizeof(THeader), font_header ) )
    {
      if ( !Seek_Font_File( table_dir_entries[i].offset ) )
        return FAILURE;

      if ( !READ_ULong(  font_header->table_version ) ||
           !READ_ULong(  font_header->font_revision ) ||
 
           !READ_Long(   font_header->cheksum_adjust ) ||
           !READ_Long(   font_header->magic_number   ) ||
 
           !READ_UShort( font_header->flags        ) ||
           !READ_UShort( font_header->units_per_EM ) ||

           !READ_Long(   font_header->created.l1  ) ||
           !READ_Long(   font_header->created.l2  ) ||
           !READ_Long(   font_header->modified.l1 ) ||
           !READ_Long(   font_header->modified.l2 ) ||

           !READ_Short(  font_header->xMin ) ||
           !READ_Short(  font_header->yMin ) ||
           !READ_Short(  font_header->xMax ) ||
           !READ_Short(  font_header->yMax ) ||

           !READ_UShort( font_header->mac_style       ) ||
           !READ_UShort( font_header->lowest_rec_PPEM ) ||
 
           !READ_Short(  font_header->font_direction      ) ||
           !READ_Short(  font_header->index_to_loc_format ) ||
           !READ_Short(  font_header->glyph_data_format   ) )

        return FAILURE;

      return SUCCESS;
    }

    return FAILURE;
  }

/*******************************************************************
 *
 *  Function    : Load_TrueType_Locations
 *
 *  Description : Load the glyph locations table.
 *
 *  Input  :  None
 *
 *  Output :  True on success. False on failure.
 *
 ******************************************************************/

  Bool Load_TrueType_Locations()
  {
    Int       i, t, longOffsets;
    Long      sz;
    PStorage  locs;
    Short*    locs2;

    TMarkRecord  mrk;

    longOffsets = 0;

    if ( !font_header )
      if ( !Load_TrueType_Header() ) return FAILURE;

    longOffsets = font_header->index_to_loc_format;

    /* default offset format is Short */

    t = LookUp_TrueType_Table(TTAG_loca);
    if ( t < 0 ) return FAILURE;

    if ( !ALLOC( sizeof(TLoca), glyph_locations ) ) 
      return FAILURE;

    if ( !Seek_Font_File( table_dir_entries[t].offset ) )
      return FAILURE;

    if ( longOffsets )   /* 32 bits offsets */
    {

#ifdef DEBUG
      fprintf( stderr, "long location offsets\n" );
#endif
      sz = table_dir_entries[t].length / 4;
      glyph_locations->size = sz;

      if ( !ALLOC( 4*sz, locs ) ) return FAILURE;

      glyph_locations->table = locs;

      for ( t = 0; t < sz; t++ )
        if ( !READ_ULong( locs[t] ) ) return FAILURE;
    }
    else   /* 16 bits offsets */
    {

#ifdef DEBUG
      fprintf( stderr, "short location offsets\n" );
#endif
      sz = table_dir_entries[t].length / 2;
      glyph_locations->size = sz;

      if ( !ALLOC( 4*sz, locs )) return FAILURE;
      Mark( &mrk );
      if ( !ALLOC( 2*sz, locs2 )) return FAILURE;

      glyph_locations->table = locs;

      for ( t = 0; t < sz; t++ )
        if ( !READ_UShort( locs2[t] ) ) return FAILURE;

      for ( i = 0; i < sz; i++ ) locs[i] = 2*(Long)locs2[i];

      if ( !Release(&mrk) ) return FAILURE;
    }

    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    :  Load_TrueType_CVT
 *
 *  Description :  Loads the Control Value Table.
 *
 *  Input  :  None
 *
 *  Output :  True on success. False on failure.
 *
 ******************************************************************/

  Bool Load_TrueType_CVT()
  {
    Int m;

    PTableDirEntries p;

    m = LookUp_TrueType_Table(TTAG_cvt);
    if ( m < 0 ) return FAILURE;

    p = table_dir_entries + m;

    if ( !ALLOC( p->length, Cvt ) ) return FAILURE;

    CvtSize = p->length / sizeof(Short);

    if ( !Seek_Font_File( p->offset ) ) return FAILURE;

    for ( m = 0; m < CvtSize; m++ )
      if ( !READ_Short( Cvt[m] ) ) return FAILURE;

    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    :  Load_TrueType_CMap
 *
 *  Description :  Load character mapping tables.
 *
 *  Input  :  None
 *
 *  Output :  True on success. False on failure.
 *
 ******************************************************************/

  Bool Load_TrueType_CMap()
  {
    Int  i, l, m, t;

    Int  num_SH;
    Int  num_SEG;

    UShort  dummy;

    PTableDirEntries  p;

    PCMap0Table  c0;
    PCMap2Table  c2;
    PCMap4Table  c4;
    PCMap6Table  c6;

    PCMap2SubHeader  sh2;
    PCMap4Segment    seg4;

    m = LookUp_TrueType_Table( TTAG_cmap );
    if ( m < 0 ) return FAILURE;

    p = table_dir_entries + m;

    /* Reading the CMap directory */

    if ( !Seek_Font_File( p->offset ) ) return FAILURE;

    if ( !READ_UShort( cmap_dir.tableVersionNumber ) ||
         !READ_UShort( cmap_dir.numCMaps ) )

      return FAILURE;

    num_CME = cmap_dir.numCMaps;
    l       = sizeof( TCMapDirEntry ) * num_CME;

    if ( !ALLOC( l, cmap_dir_entries ) )
      return FAILURE;

    for ( m = 0; m < num_CME; m++ )
    {
      if ( !READ_UShort( cmap_dir_entries[m].platformID         ) ||
           !READ_UShort( cmap_dir_entries[m].platformEncodingID ) ||
           !READ_Long  ( cmap_dir_entries[m].offset             ) )

        return FAILURE;
    }

    l = sizeof( TCMapTable ) * num_CME;

    if ( !ALLOC( l, cmap_tables ) ) return FAILURE;

    for ( t = 0; t < num_CME; t++ )
    {
      if ( !Seek_Font_File( cmap_dir_entries[t].offset ) )
        return FAILURE;

      if ( !READ_UShort( cmap_tables[t].format  ) ||
           !READ_UShort( cmap_tables[t].length  ) ||
           !READ_UShort( cmap_tables[t].version ) )

        return FAILURE;

      switch ( cmap_tables[t].format )
      {

      case 0:
        if ( !ALLOC( sizeof( TCMap0Table ), c0 ) )
          return FAILURE;

        cmap_tables[t].cmap.cmap0 = c0;

        if ( !ALLOC( 256, c0->glyphIdArray ) )
          return FAILURE;

        if ( !Read_Font_File( (void*)(c0->glyphIdArray), 256 ) )
          return FAILURE;
        break;


      case 2:
        if ( !ALLOC( sizeof( TCMap2Table ), c2 ) )
          return FAILURE;

        cmap_tables[t].cmap.cmap2 = c2;

        num_SH = 0;     /* in multiples of 8 = sizeof( TCMap2SubHeader ) */

        for ( i = 0; i < 256; i++ )
        {
          if ( !READ_UShort( c2->header.subHeaderKeys[i] ) )
            return FAILURE;

          if ( num_SH < c2->header.subHeaderKeys[i] )
            num_SH = c2->header.subHeaderKeys[i];
        }

        /* in multiples of 2 = sizeof( UShort ) */
        l = cmap_tables[t].length - (256 + 3) * sizeof( UShort ) - num_SH;

        if ( !ALLOC( num_SH, c2->subHeaders ) )
          return FAILURE;

        for ( i = 0; i < num_SH / 8; i++ )
        {
          sh2 = c2->subHeaders + i;

          if ( !READ_UShort( sh2->firstCode     ) ||
               !READ_UShort( sh2->entryCount    ) ||
               !READ_Short ( sh2->idDelta       ) ||
               !READ_UShort( sh2->idRangeOffset ) )

            return FAILURE;

          /* correction for actual location */
          sh2->idRangeOffset -= num_SH - i * 8 + 2;
        }

        if ( !ALLOC( l, c2->glyphIdArray ) )
          return FAILURE;

        for ( i = 0; i < l / 2; i++ )
          if ( !READ_UShort( c2->glyphIdArray[i] ) ) return FAILURE;
        break;


      case 4:
        if ( !ALLOC( sizeof( TCMap4Table ), c4 ) )
          return FAILURE;

        cmap_tables[t].cmap.cmap4 = c4;

        if ( !READ_UShort( c4->header.segCountX2    ) ||
             !READ_UShort( c4->header.searchRange   ) ||
             !READ_UShort( c4->header.entrySelector ) ||
             !READ_UShort( c4->header.rangeShift    ) )

          return FAILURE;

        num_SEG = c4->header.segCountX2 / 2;

        if ( !ALLOC( num_SEG * sizeof( TCMap4Segment ), c4->segments ) )
          return FAILURE;

        for ( i = 0; i < num_SEG; i++ )
        {
          seg4 = c4->segments + i;

          if ( !READ_UShort( seg4->endCount ) )
            return FAILURE;
        }

        if ( !READ_UShort( dummy ) )
          return FAILURE;

        for ( i = 0; i < num_SEG; i++ )
        {
          seg4 = c4->segments + i;

          if ( !READ_UShort( seg4->startCount ) )
            return FAILURE;
        }

        for ( i = 0; i < num_SEG; i++ )
        {
          seg4 = c4->segments + i;

          if ( !READ_UShort( seg4->idDelta ) )
            return FAILURE;
        }

        for ( i = 0; i < num_SEG; i++ )
        {
          seg4 = c4->segments + i;

          if ( !READ_UShort( seg4->idRangeOffset ) )
            return FAILURE;
        }

        /* in multiples of 2 = sizeof( UShort ) */
        l = cmap_tables[t].length - (16 + 4 * c4->header.segCountX2);

        if ( !ALLOC( l, c4->glyphIdArray ) )
          return FAILURE;

        for ( i = 0; i < l / 2; i++ )
          if ( !READ_UShort( c4->glyphIdArray[i] ) ) return FAILURE;
        break;


      case 6:
        if ( !ALLOC( sizeof( TCMap6Table ), c6 ) )
          return FAILURE;

        cmap_tables[t].cmap.cmap6 = c6;

        if ( !READ_UShort( c6->header.firstCode  ) ||
             !READ_UShort( c6->header.entryCount ) )

          return FAILURE;

        l = c6->header.entryCount;

        if ( !ALLOC( l * sizeof( UShort ), c6->glyphIdArray ) )
          return FAILURE;

        for ( i = 0; i < l; i++)
          if ( !READ_UShort( c6->glyphIdArray[i] ) ) return FAILURE;
        break;


      default:
        return FAILURE;                 /* corrupt CMap table */
      }
    }

    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    :  Load_TrueType_MaxProfile
 *
 *  Description :  Load the MaxProfile table.
 *
 *  Input  :  None
 *
 *  Output :  True on success. False on failure.
 *
 ******************************************************************/

  Bool Load_TrueType_MaxProfile()
  {
    Int m;

    m = LookUp_TrueType_Table(TTAG_maxp);
    if ( m < 0 ) return FAILURE;

    if ( !Seek_Font_File( table_dir_entries[m].offset ) )
      return FAILURE;

    if ( !READ_ULong ( maxProfile.version     ) ||
         !READ_UShort( maxProfile.numGlyphs   ) ||
         !READ_UShort( maxProfile.maxPoints   ) ||
         !READ_UShort( maxProfile.maxContours ) ||

         !READ_UShort( maxProfile.maxCompositePoints   ) ||
         !READ_UShort( maxProfile.maxCompositeContours ) ||

         !READ_UShort( maxProfile.maxZones           ) ||
         !READ_UShort( maxProfile.maxTwilightPoints  ) ||
         !READ_UShort( maxProfile.maxStorage         ) ||
         !READ_UShort( maxProfile.maxFunctionDefs    ) ||
         !READ_UShort( maxProfile.maxInstructionDefs ) ||
         !READ_UShort( maxProfile.maxStackElements      ) ||
         !READ_UShort( maxProfile.maxSizeOfInstructions ) ||
         !READ_UShort( maxProfile.maxComponentElements ) ||
         !READ_UShort( maxProfile.maxComponentDepth ) )

      return FAILURE;

#ifdef DEBUG
    fprintf( stderr, "MAXP loaded\n" );
#endif

    num_glyphs = maxProfile.numGlyphs;

    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    :  Load_TrueType_Glyph
 *
 *  Description :  Load a glyph from the TT file. The necesssary memory
 *                 will be currently allocated with malloc until the new
 *                 memory management is implemented.
 *
 *  Input  :  Int  glyphIndex
 *
 *  Output :  a pointer to a TGlyph structure on success. NULL on error.
 *
 *  NOTE :
 *
 *    This function will be replaced soon with a more sophisticated one!!
 *
 ******************************************************************/

  PGlyph  Load_TrueType_Glyph( Int  glyphIndex )
  {
    Int    sz, i;
    Short  szc, szp;
    UShort k;
    Byte   b, c, cnt;
 
    Short  x, y;

    Long      offset;
    PStorage  locs;

    PGlyph          Gl;
    PGlyphContours  con = (PGlyphContours)0;
    PPoints         pts = (PPoints)0;

    i = LookUp_TrueType_Table(TTAG_glyf);
    if ( i < 0 ) return NULL;

    offset = table_dir_entries[i].offset;

    if ( !glyph_locations )
      if ( !Load_TrueType_Locations() ) return NULL;

    locs = glyph_locations->table;
    sz   = glyph_locations->size;

    if ( ( Gl = malloc( sizeof( TGlyph ) ) ) == NULL )
      return NULL;

#ifdef DEBUG
    fprintf( stderr, "---------------------------\n" );
    fprintf( stderr, "glyph number = %d\n", glyphIndex );
#endif
    if ( !Seek_Font_File( offset + locs[glyphIndex] ) )
      goto Fin;

    if ( !READ_UShort( Gl->numberOfContours ) ||
         !READ_UShort( Gl->xMin ) ||
         !READ_UShort( Gl->yMin ) ||
         !READ_UShort( Gl->xMax ) ||
         !READ_UShort( Gl->yMax ) )

      goto Fin;;

#ifdef DEBUG
    fprintf( stderr, "n contours : %d\n", Gl->numberOfContours );
    fprintf( stderr, "x minimum  : %d\n", Gl->xMin );
    fprintf( stderr, "y minimum  : %d\n", Gl->yMin );
    fprintf( stderr, "x maximum  : %d\n", Gl->xMax );
    fprintf( stderr, "y maximum  : %d\n", Gl->yMax );
#endif

    szc = Gl->numberOfContours;
    if ( szc < 0 || szc > maxProfile.maxContours ) goto Fin;

    if ( ( con = malloc( szc * sizeof( TGlyphContour ) ) ) == NULL )
      goto Fin;

    Gl->contours = con;
    szp          = 0;

    for ( k = 0; k < szc; k++ )
    {
      con[k].start = szp;
      if ( !READ_UShort( szp ) ) goto Fin;
      con[k].finish = szp++;
    }

    Gl->numberOfPoints = szp;

#ifdef DEBUG
    fprintf( stderr, "[%d points]\n", szp );
#endif

    /* For now, we skip instructions */

    if ( !READ_UShort( k ) ) goto Fin;

#ifdef DEBUG
    fprintf( stderr, "[%d instructions]\n", k );
#endif

    Skip_Font_File( k );

    if ( ( pts = malloc ( szp * sizeof( TPointRec ) ) ) == NULL )
    {
#ifdef DEBUG
      fprintf( stderr, "could not malloc points\n" );
#endif
      goto Fin;
    }

    Gl->points = pts;

    /* Reading the flags */

    k = 0;
    while ( k < szp )
    {
      READ_Byte( c );
      pts[k].flag = c;
      k++;

      if ( c & 8 )
      {
        READ_Byte( b );
        cnt = b;
        while ( cnt > 0 )
        {
          pts[k++].flag = c;
          cnt--;
        }
      }
    }

    /* Reading the Xs */

    for ( k = 0; k < szp; k++ )
    {
      if ( pts[k].flag & 2 )
      {
        READ_Byte( c );
        if ( pts[k].flag & 16 ) x = c; else x = -c;
      }
      else
        if ( pts[k].flag & 16 ) x = 0; else READ_Short( x );

      pts[k].x = x;
    }

    /* Reading the Ys */

    for ( k = 0; k < szp; k++ )
    {
      if ( pts[k].flag & 4 )
      {
        READ_Byte( c );
        if ( pts[k].flag & 32 ) y = c; else y = -c;
      }
      else
        if ( pts[k].flag & 32 ) y = 0; else READ_Short( y );

      pts[k].y = y;
    }

    /* Convert relative coords to absolute ones */

    for ( k = 1; k < szp; k++ )
    {
      pts[k].x += pts[k-1].x;
      pts[k].y += pts[k-1].y;
    }

    return Gl;


  Fin:
    if ( pts )
      free( pts );
    if ( con )
      free( con );
    free( Gl );

    return NULL;
  }

/*******************************************************************
 *
 *  Function    :  Release_TrueType_Glyph
 *
 *  Description :  Releases a glyph from memory. Will be done with
 *                 free() currently until the new memory management is
 *                 implemented.
 *
 *  Input  :  PGlyph  Gl
 *
 *  Output :  none.
 *
 *  NOTE :
 *
 *    This function will be replaced soon with a more sophisticated one!!
 *
 ******************************************************************/

  void  Release_TrueType_Glyph( PGlyph  Gl )
  {
    if ( Gl )
    {
      if ( Gl->points )
        free( Gl->points );
      if ( Gl->contours )
        free( Gl->contours );
      free( Gl );
    }
  }

/*******************************************************************
 *
 *  Function    :  Open_TrueType_File
 *
 *  Description :  Open a TrueType font file.
 *
 *  Input  :  char* name   Name of the file to open
 *
 *  Output :  True on success. False on failure.
 *
 ******************************************************************/

  Bool Open_TrueType_File( char* name )
  {
    return Open_Font_File( name );
  }

/*******************************************************************
 *
 *  Function    : Close_TrueType_File
 *
 *  Description : Guess what .. ;-)
 *
 *  Input  :  None
 *
 *  Output :  True (always)
 *
 ******************************************************************/

  void Close_TrueType_File()
  {
    Close_Font_File();
  }

/*******************************************************************
 *
 *  Function    : Tags_to_String
 *
 *  Description : returns ASCIIZ representation of the input tag
 *                it does not check the input parameters, but this
 *                should be OK in this case.
 *
 *  Input  :  Long tag : the tag to convert
 *            char *str: where to place the result
 *
 *  Output :  str
 *
 ******************************************************************/

  char *Tags_to_String( Long tag, char * str )
  {
    str[0] = (tag >> 24) & 0xff;
    str[1] = (tag >> 16) & 0xff;
    str[2] = (tag >>  8) & 0xff;
    str[3] = (tag >>  0) & 0xff;
    str[4] = 0;
    return str;
  }
