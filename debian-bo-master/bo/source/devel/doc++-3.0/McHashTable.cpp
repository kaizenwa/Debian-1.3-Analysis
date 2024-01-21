/////////////////////////////////////////////////////////////////
//
// $Id: McHashTable.cpp,v 3.0 1997/02/04 17:48:57 bzfzoeck Exp $
//
// $Log: McHashTable.cpp,v $
// Revision 3.0  1997/02/04 17:48:57  bzfzoeck
// released Version 3.0
//
// Revision 1.2  1996/12/16 10:44:50  bzfzoeck
// now really
//
// Revision 1.1  1996/10/12  20:43:27  bzfzoeck
// Ist auch im mclib repository drin !
//
// Revision 1.1  1996/07/17  15:27:06  bzfstall
// Hash table added.
//
//
/////////////////////////////////////////////////////////////////
#include "McHashTable.h"

// One word summary of a string. This is taken from Tcl.

int hash(const char* str)
{
    int result = 0;
    while (1) {
	char c = *str++;
	if (c == 0) break;
	result += (result<<3) + c;
    }
    return result;
}

int hash(int i)
{
    return i;
}

int compare(const char* str1, const char* str2)
{
    return strcmp(str1,str2);
}

int compare(int i1, int i2)
{
    return i1 - i2;
}

