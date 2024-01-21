/* Interface for Objective-C Tcl interpreter object
   Copyright (C) 1993,1994  R. Andrew McCallum

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   This file is part of the Tcl/Objective-C interface library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/ 

#ifndef _Tcl_h
#define _Tcl_h

#include <objc/Object.h>
#include <tcl.h>
#include <tclObjc.h>

@interface Tcl : Object
{
  @public
  Tcl_Interp *interp;
  int code;
  id namesToObjects;
  id objectsToNames;
  BOOL evalDebugPrint;
}

+ initialize;
+ firstTcl;
+ tclAtIndex: (unsigned)index;
+ (unsigned) tclCount;

- initWithArgc: (int)argc argv: (char**)argv;
- (char *) preInitWithArgc: (int)argc argv: (char**)argv;
- init;
- free;

- eval: (char *)fmt, ...;
- globalEval: (char *)fmt, ...;
- evalFile: (const char *)filename;

- promptAndEval;

- (BOOL) variableExists: (const char *)varName;
- (const char *) variableValue: (const char *)varName;
- (BOOL) globalVariableExists: (const char *)varName;
- (const char *) globalVariableValue: (const char *)varName;
- (int) code;
- (const char *) result;
- (Tcl_Interp*) interp;

- registerObject: (id)anObject withName: (const char *)aName;
- unregisterObject: (id)anObject;
- unregisterObjectNamed:(const char *)aName;
- (const char *) nameForObject:anObject;
- objectNamed:(const char *)aName;
- (BOOL) objectIsRegistered: anObject;
- (BOOL) nameIsRegistered: (const char *)aName;

- setEvalDebugPrint: (BOOL) value;

@end

#endif /* _Tcl_h */
