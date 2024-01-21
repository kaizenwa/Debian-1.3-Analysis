/*******************************************************************
 *
 *  TTTables.h                                                1.0
 *
 *    TrueType Tables structures and handling (specification).
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

#ifndef TTTABLES_H
#define TTTABLES_H

#include "tttypes.h"

  typedef TPoint          TTZone[64];
  typedef TPoint         *PTZone;

  struct _TCVTRecord
  {
    int       n;  /* size of 32bit elements */
    PStorage  a;  /* table base address     */
  };
  typedef struct _TCVTRecord TCVTRecord;

  /*****************************************************)
  (*                                                   *)
  (*              TrueType tables types                *)
  (*                                                   *)
  (*****************************************************/

  /* Graphics State                            *)
  (*                                           *)
  (* The Graphics State (GS) is managed by the *)
  (* instruction field, but does not come from *)
  (* the font file. Thus, we can use 'int's    *)
  (* where needed.                             *)
  (*                                           */

  struct _TGraphicsState
  {
    Bool              auto_flip;
    TT_F26Dot6        control_value_cutin;
    Int               delta_base;
    Int               delta_shift;

    TT_UnitVector     dualVector,
                      projVector,
                      freeVector;

    Int               gep0,
                      gep1,
                      gep2;

    char              instruct_control;
    long              loop;

    TT_F26Dot6        minimum_distance;
    Int               round_state;

    Int               rp0,
                      rp1,
                      rp2;

    Bool              scan_control;
    TT_F26Dot6        single_width_cutin;
    TT_F26Dot6        single_width_value;
  };
  typedef struct _TGraphicsState TGraphicsState;

  /* TrueType table directory type */

  struct _TTableDir
  {
    TT_Fixed   version;          /* should be 0x10000 */
    UShort     numTables;        /* number of tables  */

    UShort     searchRange;     /* These parameters are only used */
    UShort     entrySelector;   /* for a binary search in the     */
    UShort     rangeShift;      /* directory. We ignore them      */
  };
  typedef struct _TTableDir TTableDir;

  /* The 'TableDir' is followed by 'numTables' TableDirEntries */

  struct _TTableDirEntry
  {
    Long     tag;      /* table type        */
    Long     checksum; /* table checksum    */
    Long     offset;   /* table file offset */
    Long     length;   /* table length      */
  };
  typedef struct _TTableDirEntry TTableDirEntry;
  typedef TTableDirEntry         TTableDirEntries[32];
  typedef TTableDirEntry        *PTableDirEntries;

  /* 'cmap' tables */

  struct _TCMapDir
  {
    UShort  tableVersionNumber; /* Should be 0       */
    UShort  numCMaps;           /* number of entries */
  };
  typedef struct _TCMapDir  TCMapDir;

  /* The 'cmap' is followed by cMapNum TCMapDirEntries */
  struct _TCMapDirEntry
  {
    UShort   platformID;          /* Windows = 3        */
    UShort   platformEncodingID;  /* Microsoft UGL = 1  */
    Long     offset;              /* subtable offset    */
  };
  typedef struct _TCMapDirEntry  TCMapDirEntry;
  typedef TCMapDirEntry          TCMapDirEntries[];
  typedef TCMapDirEntry*         PCMapDirEntries;

  /* NOTE : The following types are not defined by the TrueType */
  /*        spec. However, they represent the layout of the     */
  /*        character mapping tables in memory. This could      */
  /*        easily change in future versions of the library.    */

  /* Format 2 is used for mixed 8/16bit encodings (usually CJK fonts). */
  struct _TCMap2
  {
    UShort  subHeaderKeys[256]; /* high byte mapping table     */
                                /* value = subHeader index * 8 */
  };
  typedef struct _TCMap2  TCMap2;

  /* The format 2 table contains a variable-length array of subHeaders    */
  /* (at most 256 entries) whose size must be determined algorithmically. */
  struct _TCMap2SubHeader
  {
    UShort  firstCode,      /* first valid low byte                  */
            entryCount;     /* number of valid low bytes             */
    Short   idDelta;        /* delta value to get glyphIndex         */
    UShort  idRangeOffset;  /* offset from here to pos. of firstCode */
  };
  typedef struct _TCMap2SubHeader   TCMap2SubHeader;
  typedef struct _TCMap2SubHeader*  PCMap2SubHeader;

  /* Microsoft standard character to glyph index mapping table. */
  struct _TCMap4
  {
    UShort  segCountX2,     /* segments number * 2          */
            searchRange,    /* these parameters can be used */
            entrySelector,  /* for a binary search          */
            rangeShift;
  };
  typedef struct _TCMap4  TCMap4;

  /* The format 4 table contains segCount segments. */
  struct _TCMap4Segment
  {
    UShort  endCount,
            startCount,
            idDelta,
            idRangeOffset;
  };
  typedef struct _TCMap4Segment   TCMap4Segment;
  typedef struct _TCMap4Segment*  PCMap4Segment;

  /* Trimmed table mapping (for representing one subrange). */
  struct _TCMap6
  {
    UShort  firstCode,      /* first character code of subrange      */
            entryCount;     /* number of character codes in subrange */
  };
  typedef struct _TCMap6  TCMap6;

  /* The following structs define the layout of the cmap tables in memory */
  /* and should be used to access the data.                               */

  struct _TCMap0Table
  {
    PUShort  glyphIdArray;
  };
  typedef struct _TCMap0Table   TCMap0Table;
  typedef struct _TCMap0Table*  PCMap0Table;

  struct _TCMap2Table
  {
    TCMap2           header;
    PCMap2SubHeader  subHeaders;
    PUShort          glyphIdArray;
  };
  typedef struct _TCMap2Table   TCMap2Table;
  typedef struct _TCMap2Table*  PCMap2Table;

  struct _TCMap4Table
  {
    TCMap4         header;
    PCMap4Segment  segments;
    PUShort        glyphIdArray;
  };
  typedef struct _TCMap4Table   TCMap4Table;
  typedef struct _TCMap4Table*  PCMap4Table;

  struct _TCMap6Table
  {
    TCMap6   header;
    PUShort  glyphIdArray;
  };
  typedef struct _TCMap6Table   TCMap6Table;
  typedef struct _TCMap6Table*  PCMap6Table;

  struct _TCMapTable
  {
    UShort  format,         /* must be 0, 2, 4, or 6       */
            length,         /* size in bytes               */
            version;        /* version number. starts at 0 */
    union
    {
      PCMap0Table  cmap0;
      PCMap2Table  cmap2;
      PCMap4Table  cmap4;
      PCMap6Table  cmap6;
    } cmap;
  };
  typedef struct _TCMapTable   TCMapTable;
  typedef TCMapTable           TCMapTables[];
  typedef TCMapTable*          PCMapTables;

  /* 'maxp' Maximum Profiles table */

  struct _TMaxProfile
  {
    TT_Fixed  version;
    UShort    numGlyphs,
              maxPoints,
              maxContours,
              maxCompositePoints,
              maxCompositeContours,
              maxZones,
              maxTwilightPoints,
              maxStorage,
              maxFunctionDefs,
              maxInstructionDefs,
              maxStackElements,
              maxSizeOfInstructions,
              maxComponentElements,
              maxComponentDepth;
  };
  typedef struct _TMaxProfile TMaxProfile;

#define TMaxProfile_Size  32

  /* 'head' table type */

  struct _TLongDateTime
  {
    Long l1, l2;
  };
  typedef struct _TLongDateTime TLongDateTime;

  struct _THeader
  {
    TT_Fixed       table_version;
    TT_Fixed       font_revision;

    Long           cheksum_adjust;
    Long           magic_number;

    UShort         flags;
    UShort         units_per_EM;

    TLongDateTime  created;
    TLongDateTime  modified;

    Short          xMin;
    Short          yMin;
    Short          xMax;
    Short          yMax;

    UShort         mac_style;
    UShort         lowest_rec_PPEM;

    Short          font_direction;
    Short          index_to_loc_format;
    Short          glyph_data_format;
  };
  typedef struct _THeader THeader;

  /* 'loca' location table type */

  struct _TLoca
  {
    int        size;
    PStorage   table;
  };
  typedef struct _TLoca  TLoca;

  struct _TGlyphContour
  {
    UShort start;
    UShort finish;
  };
  typedef struct _TGlyphContour  TGlyphContour;
  typedef TGlyphContour         *PGlyphContour;

  typedef TGlyphContour          TGlyphContours[1024];
  typedef TGlyphContour         *PGlyphContours;

  struct _TGlyph
  {
    Short    numberOfContours;
    Short    xMin;
    Short    yMin;
    Short    xMax;
    Short    yMax;
    Short    numberOfPoints;

    PGlyphContours  contours;
    PPoints         points;
  };
  typedef struct _TGlyph  TGlyph;
  typedef TGlyph         *PGlyph;

  typedef TGlyph          TGlyphs[1000];
  typedef TGlyph         *PGlyphs;


  extern TTableDir           table_dir;
  extern PTableDirEntries    table_dir_entries;
  extern Int                 num_TDE;

  extern TCMapDir            cmap_dir;
  extern PCMapDirEntries     cmap_dir_entries;
  extern PCMapTables         cmap_tables;
  extern Int                 num_CME;

  extern TMaxProfile         maxProfile;

  extern THeader            *font_header;

  extern TLoca              *glyph_locations;

  extern Int                 num_glyphs;

  extern  TGraphicsState  Gs;
  extern  PShort          Cvt;
  extern  Int             CvtSize;

  extern  Long  Scale1;
  extern  Long  Scale2;

  extern  TT_F26Dot6  PointSize;


  Bool Open_TrueType_File( char* name );
  void Close_TrueType_File();

  Bool Load_TrueType_Tables();
  Int  LookUp_TrueType_Table( Long tag );
  Bool Load_TrueType_Header();
  Bool Load_TrueType_CMap();
  Bool Load_TrueType_MaxProfile();
  Bool Load_TrueType_CVT();
  Bool Load_TrueType_Locations();

  PGlyph  Load_TrueType_Glyph( Int  glyphIndex );
  void    Release_TrueType_Glyph( PGlyph  Gl );

  char *Tags_to_String( Long tag, char *str );

#endif
