// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: IntermediateFunction.C,v 1.2 1996/01/08 03:03:38 geppetto Exp $
#pragma implementation
#include "IntermediateFunction.h"
#include "Statement.h"

//
//  Method name : CIntermediateFunction
//  Description : Constructs an intermediate function out of its parts.
//
CIntermediateFunction::CIntermediateFunction(
			const CJavaMethodSignature& signature,
			const CJavaAccessFlags& modifiers,
			deque<CJavaFieldSignature>* adoptParameters,
			deque<unicode_string>* adoptThrows)
  : fSignature(signature),
    fMaxLocalVariables(0),
    fBlock(0),
    fAccessFlags(modifiers)
{
  if (adoptParameters != 0) {
    fParameters = *adoptParameters;
    delete adoptParameters;
  }
  if (adoptThrows != 0) {
    fThrows = *adoptThrows;
    delete adoptThrows;
  }
}

//
//  Method name : ~CIntermediateFunction
//  Description : Destructor
//
CIntermediateFunction::~CIntermediateFunction()
{
  delete fBlock;
}
