// $Id: VoidArray.h,v 1.1.1.1 1995/05/01 15:48:04 zeller Exp $ -*- C++ -*-
// void * Array

#ifndef _DDD_VoidArray_h
#define _DDD_VoidArray_h

#ifdef __GNUG__
#pragma interface
#endif

#include "VarArray.h"
#include "DynArray.h"

typedef VarArray<void *> VoidArray;

#endif // _DDD_VoidArray_h
// DON'T ADD ANYTHING BEHIND THIS #endif
