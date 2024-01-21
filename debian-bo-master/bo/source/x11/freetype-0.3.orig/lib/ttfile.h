/*******************************************************************
 *
 *  TTFile.h                                                  1.0
 *
 *    File I/O Component (specification).
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

#ifndef TTFILE_H
#define TTFILE_H

#include "tttypes.h"

  Bool Open_Font_File( char* name );
  Bool Read_Font_File( void* buff, long count );

#ifdef ONE_COMPLEMENT
#error  One complement processors not supported
#endif

#define READ_Char(c)    Read_8 ( (unsigned char*) &(c) )
#define READ_Byte(b)    Read_8 ( (unsigned char*) &(b) )
#define READ_Short(s)   Read_16( (unsigned short*) &(s) )
#define READ_UShort(s)  Read_16( (unsigned short*) &(s) )

#define READ_Long(l)    Read_32( (unsigned long*) &(l) )
#define READ_ULong(l)   Read_32( (unsigned long*) &(l) )

  /* Please use the above macros to read words from the file */

  Bool Read_8 ( unsigned char*  c );
  Bool Read_16( unsigned short* s );
  Bool Read_32( unsigned long*  l );

  Bool Seek_Font_File( long pos  );
  Bool Skip_Font_File( long dist );

  Bool Read_At_Font_File( long pos, void* buff, long count );

  Bool Close_Font_File();

#endif /* TTFILE_H */
