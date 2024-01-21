/*******************************************************************
 *
 *  TTIndex.h
 *
 *    TrueType index and cmap handling (specification).
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

#ifndef TTINDEX_H
#define TTINDEX_H

#include "tttypes.h"


  Int  TT_select_CMap( UShort  platformID,
                       UShort  platformEncodingID );
  Int  TT_code_to_index( UShort charCode,
                         Int    cmapIndex );

#endif
