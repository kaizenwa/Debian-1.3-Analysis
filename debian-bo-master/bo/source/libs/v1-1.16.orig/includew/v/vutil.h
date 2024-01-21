//===============================================================
// vutil.h - V utility funcitons
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VUTIL_H
#define VUTIL_H
    extern void LongToStr(long intg, char* str);	// prototype
    extern void IntToStr(int intg, char* str);
    extern int vLblLen(char *str);
    extern int vTextLen(char *str, int& numLines);
    extern void vGetLocalTime(char* tm);
    extern void vGetLocalDate(char* tm);
    extern void ByteToStr(unsigned char byteval, char* str);
    extern long StrToLong(char* str);
#endif
