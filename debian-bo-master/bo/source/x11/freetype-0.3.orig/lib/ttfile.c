/*******************************************************************
 *
 *  TTFile.c                                                  1.0
 *
 *    File I/O Component (body).
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

#include "ttfile.h"
#include "tterror.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

  /* IMPORTANT NOTE :                                              */
  /*                                                               */
  /*   This version of the 'File' component will load the whole    */
  /*   file into memory before allowing to parse it. This is very  */
  /*   useful for debugging, though it limits 16bit systems        */
  /*   to work on font files smaller than 64kByte.                 */
  /*                                                               */
  /*   It is the equivalent of 'TTFileM.Pas' found in the 'PAS/'   */
  /*   source directory. Change it to reflect 'TTFile.Pas' to      */
  /*   use any file, on any system.                                */

  FILE* font_file;

  unsigned char* font_buff;
  long           font_size;
  long           font_pos;

/*******************************************************************
 *
 *  Function    :  Open_Font_File
 *
 *  Description :  opens the font file and reads it into a memory
 *                 buffer (to ease development and speed debug).
 *
 *  Input  :  name   pathname of the file to open
 *
 *  Output :  True on sucess.
 *
 ******************************************************************/

  Bool Open_Font_File( char* name )
  {
    font_file = fopen( name, "rb" );
    if (!font_file)
    {
      Error = TT_Err_File_Error;
      return FAILURE;
    }

    fseek( font_file, 0, SEEK_END );
    font_size = ftell( font_file );

#ifdef DEBUG
    fprintf( stderr, "(Open_Font_File) file size is = %ld\n", font_size );
#endif

    fseek( font_file, 0, SEEK_SET );

    font_pos  = 0;

    font_buff = (unsigned char*)malloc( font_size );
    if (!font_buff)
    {
      Error = TT_Err_File_Error;
      return FAILURE;
    }
    fread( font_buff, 1, font_size, font_file );

    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    : Close_Font_File
 *
 *  Description : Closes the font file and releases memory buffer.
 *
 *  Input  :  None
 *
 *  Output :  True (always)
 *
 ******************************************************************/

  Bool Close_Font_File()
  {
    fclose( font_file );

    if ( font_buff ) free( font_buff );
    font_buff = NULL;

    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    : Seek_Font_File
 *
 *  Description : Seek the file cursor to a different position.
 *
 *  Input  :  long pos     new position on file
 *
 *  Output :  True on success. False if out of range.
 *
 *  Notes  :  Does not set the error variable.
 *
 ******************************************************************/

  Bool Seek_Font_File( long pos )
  {
    if ( pos < 0 || pos >= font_size )
      return FAILURE;

    font_pos = pos;
    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    : Skip_Font_File
 *
 *  Description : Skip forward the file cursor.
 *
 *  Input  :  long distance    number of bytes to skip
 *
 *  Output :  see Seek_Font_File.
 *
 ******************************************************************/

  Bool Skip_Font_File( long dist )
  {
    return Seek_Font_File( font_pos + dist );
  }

/*******************************************************************
 *
 *  Function    : Read_Font_File
 *
 *  Description : Reads a chunk of the file and copy it to memory.
 *
 *  Input  :  void*  buff     target pointer
 *            long   count    length in bytes to read 
 *
 *  Output :  True if success. False if out of range.
 *
 *  Notes  :  Current version prints an error message even if the
 *            debug state is off.
 *
 ******************************************************************/

  Bool Read_Font_File( void* buff, long count )
  {
    if ( font_pos + count > font_size )
    {
      fprintf( stderr,
               "tried to read bytes %ld-%ld\n", font_pos, font_pos+count );
      return FAILURE;
    }

    memcpy( buff, font_buff + font_pos, count );
    font_pos += count;
    return SUCCESS;
  }

/*******************************************************************
 *
 *  Function    : Read_At_Font_File
 *
 *  Description : Read file at a specified position.
 *
 *  Input  :  long   pos      position to seek to before read
 *            void*  buff     target buffer
 *            long   count    number of bytes to read
 *
 *  Output :  True on success. False if error.
 *
 *  Notes  :  prints an error message if seek failed.
 *
 ******************************************************************/

  Bool Read_At_Font_File( long pos, void* buff, long count )
  {
    if ( !Seek_Font_File(pos) )
    {
      fprintf( stderr, "could not position file cursor at %ld\n", pos );
      return FAILURE;
    }
    return Read_Font_File(buff,count);
  }

/*******************************************************************
 *
 *  Function    : Read_8
 *
 *  Description : Read a byte from the file.
 *
 *  Input  : unsigned char* c   address of target byte
 *
 *  Output : True on success. False on overflow.
 *
 *  Notes  : Use the READ_xx macros instead of this function.
 *
 ******************************************************************/

  Bool Read_8( unsigned char* c )
  {
    if ( font_pos < font_size )
    {
      *c = font_buff[font_pos++];
      return SUCCESS;
    }
    else
      return FAILURE;
  }

/*******************************************************************
 *
 *  Function    : Read_16
 *
 *  Description : Read a 2byte word from the file.
 *
 *  Input  :  unsigned short* s  address of target short
 *
 *  Output :  True on success. False on overflow.
 *
 *  Notes  :  Use the READ_xx macros instead of this function.
 *
 ******************************************************************/

  Bool Read_16( unsigned short* s )
  {
    if ( font_pos + 1 < font_size )
    {

#ifdef LOOSE_ACCESS
      *s = *((unsigned short*)( font_buff + font_pos ));
#else
      *s = (font_buff[font_pos] << 8 ) | font_buff[font_pos+1];
#endif /* LOOSE */

      font_pos += 2;
      return SUCCESS;
    };
    return FAILURE;
  }

/*******************************************************************
 *
 *  Function    : Read_32
 *
 *  Description : Read a 4byte word from the file.
 *
 *  Input  : unsigned long* l  address of target long
 *
 *  Output : True on success. False if overflow.
 *
 *  Notes  : Use the READ_xx macros instead of this function.
 *
 ******************************************************************/

  Bool  Read_32( unsigned long* l )
  {
    if ( font_pos + 3 < font_size )
    {

#ifdef LOOSE_ACCESS
      *l = *((unsigned long *)( font_pos + font_buff ));
#else
      *l = (font_buff[ font_pos ] << 24) | (font_buff[font_pos+1] << 16) |
           (font_buff[font_pos+2] << 8) |  (font_buff[font_pos+3]);
#endif /* LOOSE */

      font_pos += 4;
      return SUCCESS;
    };
    return FAILURE;
  }
