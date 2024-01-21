// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: IntermediateFunction.h,v 1.2 1996/01/08 03:03:38 geppetto Exp $
#ifndef _IntermediateFunction_h
#define _IntermediateFunction_h
#pragma interface

#include <deque>
#include "JavaMethodSignature.h"
#include "JavaAccessFlags.h"
#include "JavaFieldSignature.h"
#include "unicode_string.h"
class CCompiler;
class CIntermediateClass;
class CCompoundStatement;

//
//  Class name : CIntermediateFunction
//  Description : This class is used during compilation as a half-baked
//    representation of a java function.  Since Java is not an L-attributed
//    language (you can use some names before they are declared), I have to
//    resort to this sort of messiness.
//    It is basically just a protected structure used by CCompiler.
//
class CIntermediateFunction {
  friend class CIntermediateClass;
  friend class CCompiler;
protected:
  CIntermediateFunction(const CJavaMethodSignature& signature,
			const CJavaAccessFlags& modifiers,
			deque<CJavaFieldSignature>* adoptParameters,
			deque<unicode_string>* adoptThrows);
  ~CIntermediateFunction();
  
private:
  CJavaMethodSignature fSignature;
  CJavaAccessFlags fAccessFlags;
  deque<CJavaFieldSignature> fParameters;
  deque<unicode_string> fThrows;
  unsigned long fMaxLocalVariables;
  CCompoundStatement* fBlock;
};

#endif
