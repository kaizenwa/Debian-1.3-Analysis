/*******************************************************************
 *
 *  TTError.h
 *
 *    Error number declaration and handling (specification).
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  NOTE:
 *
 *    This component's body only contains the global variable Error
 *    which will most probably move to the Font Pool in the final
 *    release.
 *
 ******************************************************************/

#ifndef TTERROR_H
#define TTERROR_H

#define TT_Err_Ok                     0
#define TT_Err_Invalid_Opcode         1
#define TT_Err_Too_Few_Arguments      2
#define TT_Err_Stack_Overflow         3
#define TT_Err_Code_Overflow          4
#define TT_Err_Bad_Argument           5
#define TT_Err_Divide_By_Zero         6
#define TT_Err_Storage_Overflow       7
#define TT_Err_Cvt_Overflow           8
#define TT_Err_Invalid_Reference      9
#define TT_Err_Invalid_Distance      10
#define TT_Err_Interpolate_Twilight  11
#define TT_Err_Debug_OpCode          12
#define TT_Err_ENDF_In_Exec_Stream   13
#define TT_Err_Out_Of_CodeRanges     14
#define TT_Err_Nested_DEFS           15
#define TT_Err_File_Error            16

  extern int Error;

#endif /* TTERROR_H */
